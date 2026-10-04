import type { WatcherLocalKupmiosNativeObservation } from "../l1/local-kupmios-native-observation.js";
import type { WatcherMultiProviderConsistency } from "../l1/multi-provider-consistency.js";
import {
  admitWatcherNativeRollForwardBlock,
  type WatcherNativeBlockAdmission,
} from "../l1/native-block-admission.js";
import type {
  WatcherNativeChainSyncEvent,
  WatcherNativeChainSyncPoint,
} from "../l1/native-chain-sync.js";
import { indexPersistedObservations } from "../l1/rollback-engine/state.verify-persisted-consistency-evidence.js";
import type { WatcherBlockProgressStore } from "../storage/block-progress-store.js";
import type { WatcherDurableRuntime } from "../storage/durable-runtime.js";
import { WatcherDurableAuthorityConflict } from "../storage/durable-runtime.load-published-authority.js";
import type { WatcherDurableStore } from "../storage/durable-store.js";
import type { WatcherBlockRelevance } from "./block-relevance.js";
import type { WatcherCoordinatorHoldReason } from "./chain-coordinator.integrity-hold.js";

export const WATCHER_CHAIN_COORDINATOR_SCHEMA_VERSION =
  "midgard-watcher-production-chain-coordinator-v1" as const;

export type WatcherChainCoordinator = Readonly<{
  schemaVersion: typeof WATCHER_CHAIN_COORDINATOR_SCHEMA_VERSION;
  handle(event: WatcherNativeChainSyncEvent): Promise<void>;
  resume(): Promise<void>;
  waitForDelivery(): Promise<void>;
  stop(): Promise<void>;
  status(): Readonly<{
    rollbackPoint: WatcherNativeChainSyncPoint | null;
    quarantined: boolean;
    integrityHold: WatcherCoordinatorHoldReason | null;
    deliveryHeld: boolean;
    bufferedBlockCount: number;
    processedThrough: WatcherProcessedHead | null;
  }>;
}>;

export type WatcherProcessedHead = Readonly<{
  blockHash: string;
  blockNo: string;
  slot: string;
}>;

/**
 * Quiet blocks are still forced into the durable finality authority at this
 * spacing so its finalized point, and the retained progress ring that links
 * it to the next touched block, stay bounded.
 */
export const WATCHER_AUTHORITY_CHECKPOINT_INTERVAL_BLOCKS = 2_160n;

export type WatcherChainCoordinatorDependencies = Readonly<{
  /**
   * Cheap local answer to "did this block touch anything the watcher tracks".
   * Quiet blocks retain authenticated recovery evidence without advancing
   * the sparse durable finality checkpoint.
   */
  relevance?: (block: WatcherNativeBlockAdmission) => WatcherBlockRelevance;
  /** "Processed through block N", written once every finalized block. */
  progress?: WatcherBlockProgressStore;
}>;

export type WatcherChainCoordinatorHooks = Readonly<{
  /** Canonical inclusion is reversible and never advances durable finality. */
  onIncluded?(
    input: Readonly<{
      nativeBlock: WatcherNativeBlockAdmission;
      localObservation: WatcherLocalKupmiosNativeObservation | null;
      relevance: WatcherBlockRelevance;
    }>,
  ): Promise<void>;
  /** Immediate volatile fence, including while a previous delivery is awaiting. */
  onRollbackArrived?(point: WatcherNativeChainSyncPoint): void;
  /** Must revoke actuation authority synchronously before its first await. */
  onRollback(point: WatcherNativeChainSyncPoint): Promise<void>;
  /**
   * Runs once for every exact release-final block, including authenticated
   * restart replay that the durable finality snapshot has already passed.
   */
  onFinalized(
    input: Readonly<{
      nativeBlock: WatcherNativeBlockAdmission;
      /** Absent when a quiet block's observation is retained only for recovery. */
      localObservation: WatcherLocalKupmiosNativeObservation | null;
      relevance: WatcherBlockRelevance;
    }>,
  ): Promise<void>;
}>;

export type AdmitRollForward = (
  event: Extract<
    WatcherNativeChainSyncEvent,
    { readonly kind: "roll_forward" }
  >,
) => WatcherNativeBlockAdmission;

export const depthAtTip = (
  block: WatcherNativeBlockAdmission,
  event: Extract<
    WatcherNativeChainSyncEvent,
    { readonly kind: "roll_forward" }
  >,
): string => {
  if (event.tip.kind !== "point") {
    throw new Error("native roll-forward cannot have Origin as its tip");
  }
  const depth = BigInt(event.tip.blockNo) - BigInt(block.blockNo) + 1n;
  // A cold replay can be arbitrarily far behind the live tip. Recovery bounds
  // constrain replacement paths, not a canonical block's confirmation depth.
  if (depth <= 0n) {
    throw new Error("native block is ahead of its advertised tip");
  }
  return depth.toString();
};

export const pointKey = (blockHash: string, slot: string): string =>
  `${blockHash}@${slot}`;

export const canonicalPathFromHistory = (input: {
  readonly history: readonly WatcherMultiProviderConsistency[];
  readonly store?: WatcherDurableStore;
  readonly ancestor: Extract<
    WatcherNativeChainSyncPoint,
    { readonly kind: "point" }
  >;
  readonly terminal: Readonly<{
    blockHash: string;
    blockNo: string;
    lastSeenConsistencyDigest: string;
  }>;
}): readonly WatcherMultiProviderConsistency[] | null => {
  const terminal = input.history.find(
    ({ consistencyDigest }) =>
      consistencyDigest === input.terminal.lastSeenConsistencyDigest,
  );
  const terminalAgreement = terminal?.agreement;
  if (
    terminal === undefined ||
    terminalAgreement === null ||
    terminalAgreement === undefined ||
    terminalAgreement.blockHash !== input.terminal.blockHash ||
    terminalAgreement.blockNo !== input.terminal.blockNo
  ) {
    return null;
  }
  if (input.store !== undefined) {
    const index = indexPersistedObservations(input.store);
    const predecessors = new Map<string, WatcherMultiProviderConsistency>();
    for (const candidate of input.history) {
      const agreement = candidate.agreement;
      if (
        candidate.status !== "agreed" ||
        candidate.protocolDecision !== "allowed" ||
        agreement === null
      )
        continue;
      const prior = predecessors.get(agreement.blockHash);
      if (
        prior === undefined ||
        BigInt(prior.agreement!.minimumDepth) < BigInt(agreement.minimumDepth)
      )
        predecessors.set(agreement.blockHash, candidate);
    }
    const reversed: WatcherMultiProviderConsistency[] = [];
    let current = terminal;
    for (let remaining = 2_161; remaining > 0; remaining -= 1) {
      const agreement = current.agreement!;
      reversed.push(current);
      if (
        agreement.blockHash === input.ancestor.blockHash &&
        agreement.slot === input.ancestor.slot
      )
        return reversed.length >= 2 ? Object.freeze(reversed.reverse()) : null;
      const observationId =
        current.chainAuthorityObservationDigest ??
        current.observationEvidenceDigests[0];
      const parentHash =
        observationId === undefined
          ? undefined
          : index.get(observationId)?.observation.chainPoint.parentBlockHash;
      if (parentHash === undefined || parentHash === null) return null;
      const predecessor = predecessors.get(parentHash);
      if (
        predecessor?.agreement === null ||
        predecessor === undefined ||
        BigInt(predecessor.agreement.blockNo) + 1n !==
          BigInt(agreement.blockNo) ||
        BigInt(predecessor.agreement.slot) >= BigInt(agreement.slot)
      )
        return null;
      current = predecessor;
    }
    return null;
  }
  const candidates = new Map<string, WatcherMultiProviderConsistency>();
  for (const consistency of input.history) {
    const agreement = consistency.agreement;
    if (
      consistency.status !== "agreed" ||
      consistency.protocolDecision !== "allowed" ||
      agreement === null ||
      BigInt(agreement.blockNo) > BigInt(input.terminal.blockNo)
    ) {
      continue;
    }
    const existing = candidates.get(agreement.blockNo);
    if (
      existing === undefined ||
      BigInt(existing.agreement!.minimumDepth) < BigInt(agreement.minimumDepth)
    ) {
      candidates.set(agreement.blockNo, consistency);
    }
  }
  candidates.set(input.terminal.blockNo, terminal);
  const ancestor = [...candidates.values()].find(
    ({ agreement }) =>
      agreement?.blockHash === input.ancestor.blockHash &&
      agreement.slot === input.ancestor.slot,
  );
  if (ancestor?.agreement === null || ancestor === undefined) return null;
  const path: WatcherMultiProviderConsistency[] = [];
  for (
    let blockNo = BigInt(ancestor.agreement.blockNo);
    blockNo <= BigInt(input.terminal.blockNo);
    blockNo += 1n
  ) {
    const consistency = candidates.get(blockNo.toString());
    if (consistency === undefined) return null;
    path.push(consistency);
  }
  return path.length >= 2 ? Object.freeze(path) : null;
};

export const recoverWatcherCoordinatorAfterRestart = async (input: {
  readonly durable: WatcherDurableRuntime;
  readonly restartIntersection?: WatcherNativeChainSyncPoint;
  readonly assertCurrent?: () => void;
}): Promise<boolean> => {
  let quarantined = input.durable.readFinality().phase === "quarantined";
  if (!quarantined) return false;
  const state = input.durable.read();
  const ancestor = input.restartIntersection;
  const finalized = state.currentFinalityState.finalized;
  const triggerDigest =
    state.currentFinalityState.incident?.triggerConsistencyDigest;
  if (
    ancestor?.kind !== "point" ||
    finalized === null ||
    triggerDigest === null ||
    triggerDigest === undefined
  ) {
    return true;
  }
  const previousPath = canonicalPathFromHistory({
    history: state.authenticatedConsistencyHistory,
    store: state.currentStore,
    ancestor,
    terminal: finalized,
  });
  const trigger = state.authenticatedConsistencyHistory.find(
    ({ consistencyDigest }) => consistencyDigest === triggerDigest,
  );
  const ancestorConsistency = previousPath?.[0];
  if (
    previousPath === null ||
    trigger === undefined ||
    ancestorConsistency === undefined
  ) {
    return true;
  }
  const recovery = await input.durable.persistPostFinalityRecovery({
    assertCurrent: input.assertCurrent,
    previousCanonicalPath: previousPath,
    replacementCanonicalPath: Object.freeze([ancestorConsistency, trigger]),
    transportAttestations: Object.freeze([]),
  });
  if (recovery.persistence === "conflict") {
    throw new WatcherDurableAuthorityConflict(
      "watcher restart recovery persistence conflicted",
    );
  }
  quarantined = recovery.result.protocolDecision !== "resume_replay";
  return quarantined;
};

/** Only a semantic consumer waiting for authority may hold delivery. */
export class WatcherConsumerDeliveryHeld extends Error {
  constructor() {
    super("Watcher consumer delivery is held until history authority recovers");
  }
}

export const nextBufferedChild = (
  blocks: ReadonlyMap<string, WatcherNativeBlockAdmission>,
  parentHash: string | null,
  parentBlockNo: string | null,
): WatcherNativeBlockAdmission | null => {
  const candidates = [...blocks.values()].filter((block) => {
    if (parentHash === null || parentBlockNo === null) return true;
    return (
      block.prevHash === parentHash &&
      BigInt(block.blockNo) === BigInt(parentBlockNo) + 1n
    );
  });
  candidates.sort((left, right) => {
    const byBlockNo = BigInt(left.blockNo) - BigInt(right.blockNo);
    return byBlockNo < 0n
      ? -1
      : byBlockNo > 0n
        ? 1
        : left.blockHash.localeCompare(right.blockHash);
  });
  if (parentHash === null && candidates.length > 1) {
    const minimum = candidates[0]!.blockNo;
    if (candidates.filter(({ blockNo }) => blockNo === minimum).length !== 1) {
      throw new Error("native buffer contains competing unanchored children");
    }
  }
  return candidates[0] ?? null;
};

export const productionDependencies = Object.freeze({
  admitRollForward: admitWatcherNativeRollForwardBlock as AdmitRollForward,
});
