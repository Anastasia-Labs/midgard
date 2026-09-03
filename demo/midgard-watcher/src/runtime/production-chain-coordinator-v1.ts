import {
  evaluateWatcherFinality,
  type WatcherFinalityPolicy,
} from "../l1/finality-engine.js";
import type { WatcherLocalKupmiosNativeObservationRuntime } from "../l1/local-kupmios-native-observation-v1.js";
import type { WatcherLocalKupmiosNativeObservation } from "../l1/local-kupmios-native-observation-v1.js";
import type { WatcherMultiProviderConsistency } from "../l1/multi-provider-consistency.js";
import {
  admitWatcherNativeRollForwardBlock,
  type WatcherNativeBlockAdmission,
} from "../l1/native-block-admission-v1.js";
import type {
  WatcherNativeChainSyncEvent,
  WatcherNativeChainSyncPoint,
} from "../l1/native-chain-sync-v1.js";
import type { WatcherDurableRuntime } from "../storage/production-durable-runtime-v1.js";

export const WATCHER_CHAIN_COORDINATOR_SCHEMA_VERSION =
  "midgard-watcher-production-chain-coordinator-v1" as const;

export type WatcherChainCoordinator = Readonly<{
  schemaVersion: typeof WATCHER_CHAIN_COORDINATOR_SCHEMA_VERSION;
  handle(event: WatcherNativeChainSyncEvent): Promise<void>;
  status(): Readonly<{
    rollbackPoint: WatcherNativeChainSyncPoint | null;
    quarantined: boolean;
    bufferedBlockCount: number;
  }>;
}>;

export type WatcherChainCoordinatorHooks = Readonly<{
  /** Must revoke actuation authority synchronously before its first await. */
  onRollback(point: WatcherNativeChainSyncPoint): Promise<void>;
  /**
   * Runs once for every exact release-final block, including authenticated
   * restart replay that the durable finality snapshot has already passed.
   */
  onFinalized(
    input: Readonly<{
      nativeBlock: WatcherNativeBlockAdmission;
      localObservation: WatcherLocalKupmiosNativeObservation;
    }>,
  ): Promise<void>;
}>;

type AdmitRollForward = (
  event: Extract<
    WatcherNativeChainSyncEvent,
    { readonly kind: "roll_forward" }
  >,
) => WatcherNativeBlockAdmission;

const depthAtTip = (
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
  if (depth <= 0n || depth > 2_160n) {
    throw new Error("native block depth is outside the release recovery bound");
  }
  return depth.toString();
};

const pointKey = (blockHash: string, slot: string): string =>
  `${blockHash}@${slot}`;

const samePoint = (
  point: WatcherNativeChainSyncPoint,
  block: WatcherNativeBlockAdmission,
): boolean =>
  point.kind === "point" &&
  point.blockHash === block.blockHash &&
  point.slot === block.slot;

const canonicalPathFromHistory = (input: {
  readonly history: readonly WatcherMultiProviderConsistency[];
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

const nextBufferedChild = (
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

const productionDependencies = Object.freeze({
  admitRollForward: admitWatcherNativeRollForwardBlock as AdmitRollForward,
});

const createCoordinator = (input: {
  readonly policy: WatcherFinalityPolicy;
  readonly durable: WatcherDurableRuntime;
  readonly observation: WatcherLocalKupmiosNativeObservationRuntime;
  readonly restartIntersection?: WatcherNativeChainSyncPoint;
  readonly hooks: WatcherChainCoordinatorHooks;
  readonly dependencies: Readonly<{ admitRollForward: AdmitRollForward }>;
}): WatcherChainCoordinator => {
  const buffered = new Map<string, WatcherNativeBlockAdmission>();
  let rollbackPoint: WatcherNativeChainSyncPoint | null = null;
  let quarantined =
    input.durable.read().currentFinalityState.phase === "quarantined";
  const releaseFinalizedHooked = new Map<
    string,
    Readonly<{ blockHash: string; blockNo: string; slot: string }>
  >();

  const restartRecovery = (async () => {
    if (!quarantined) return;
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
      return;
    }
    const previousPath = canonicalPathFromHistory({
      history: state.authenticatedConsistencyHistory,
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
      return;
    }
    const recovery = await input.durable.persistPostFinalityRecovery({
      previousCanonicalPath: previousPath,
      replacementCanonicalPath: Object.freeze([ancestorConsistency, trigger]),
      transportAttestations: Object.freeze([]),
    });
    if (recovery.persistence === "conflict") {
      throw new Error("watcher restart recovery persistence conflicted");
    }
    quarantined = recovery.result.protocolDecision !== "resume_replay";
  })();

  const observe = async (
    block: WatcherNativeBlockAdmission,
    event: Extract<
      WatcherNativeChainSyncEvent,
      { readonly kind: "roll_forward" }
    >,
  ) =>
    await input.observation.observe({ block, depth: depthAtTip(block, event) });

  const processRollbackReplacement = async (
    block: WatcherNativeBlockAdmission,
    event: Extract<
      WatcherNativeChainSyncEvent,
      { readonly kind: "roll_forward" }
    >,
  ): Promise<void> => {
    const target = rollbackPoint;
    if (target === null) return;
    if (target.kind === "point" && block.prevHash !== target.blockHash) {
      throw new Error(
        "replacement block is not the child of the native rollback point",
      );
    }
    const before = input.durable.read();
    const previousFinalityState = before.currentFinalityState;
    const observed = await observe(block, event);
    await input.durable.persistObservation(observed);
    const finalityResult = evaluateWatcherFinality(
      input.policy,
      previousFinalityState,
      observed.consistency,
    );
    const rollback = await input.durable.persistRollback({
      previousFinalityState,
      consistency: observed.consistency,
      finalityResult,
      transportAttestations: observed.transportAttestations,
    });
    if (rollback.persistence === "conflict") {
      throw new Error("watcher rollback persistence conflicted");
    }
    if (rollback.result.action === "reject") {
      throw new Error(
        "authenticated native rollback was rejected by durable recovery",
      );
    }
    quarantined = rollback.result.protocolDecision === "quarantined";
    if (
      quarantined &&
      target.kind === "point" &&
      previousFinalityState.phase === "finalized" &&
      previousFinalityState.finalized !== null
    ) {
      const previousPath = canonicalPathFromHistory({
        history: before.authenticatedConsistencyHistory,
        ancestor: target,
        terminal: previousFinalityState.finalized,
      });
      const ancestorConsistency = previousPath?.[0];
      if (previousPath !== null && ancestorConsistency !== undefined) {
        const recovery = await input.durable.persistPostFinalityRecovery({
          previousCanonicalPath: previousPath,
          replacementCanonicalPath: Object.freeze([
            ancestorConsistency,
            observed.consistency,
          ]),
          transportAttestations: observed.transportAttestations,
        });
        if (recovery.persistence === "conflict") {
          throw new Error(
            "watcher post-finality recovery persistence conflicted",
          );
        }
        quarantined = recovery.result.protocolDecision !== "resume_replay";
      }
    }
    rollbackPoint = null;
  };

  const advanceCanonical = async (
    event: Extract<
      WatcherNativeChainSyncEvent,
      { readonly kind: "roll_forward" }
    >,
  ): Promise<void> => {
    for (let iteration = 0; iteration <= buffered.size; iteration += 1) {
      const state = input.durable.read().currentFinalityState;
      if (state.phase === "quarantined") {
        quarantined = true;
        return;
      }
      const target =
        state.phase === "pending" && state.pending !== null
          ? ([...buffered.values()].find(
              (block) =>
                block.blockHash === state.pending!.blockHash &&
                block.slot === state.pending!.slot &&
                block.blockNo === state.pending!.blockNo,
            ) ?? null)
          : nextBufferedChild(
              buffered,
              state.phase === "finalized"
                ? (state.finalized?.blockHash ?? null)
                : null,
              state.phase === "finalized"
                ? (state.finalized?.blockNo ?? null)
                : null,
            );
      if (target === null) return;
      const observed = await observe(target, event);
      const progress = await input.durable.persistCanonicalProgress(observed);
      if (progress.persistence === "conflict") {
        throw new Error("watcher canonical progress persistence conflicted");
      }
      if (progress.finalityResult.action !== "finalize") return;
      await input.hooks.onFinalized({
        nativeBlock: target,
        localObservation: observed,
      });
      releaseFinalizedHooked.set(
        pointKey(target.blockHash, target.slot),
        Object.freeze({
          blockHash: target.blockHash,
          blockNo: target.blockNo,
          slot: target.slot,
        }),
      );
      buffered.delete(pointKey(target.blockHash, target.slot));
    }
    throw new Error("watcher canonical buffer did not converge");
  };

  return Object.freeze({
    schemaVersion: WATCHER_CHAIN_COORDINATOR_SCHEMA_VERSION,
    handle: async (event) => {
      if (event.kind === "roll_backward") {
        // The production hook invalidates the in-memory actuation generation
        // before awaiting its durable cache rollback.
        await input.hooks.onRollback(event.point);
        for (const [key, hooked] of releaseFinalizedHooked) {
          if (
            event.point.kind === "origin" ||
            BigInt(hooked.slot) > BigInt(event.point.slot) ||
            (hooked.slot === event.point.slot &&
              hooked.blockHash !== event.point.blockHash)
          ) {
            releaseFinalizedHooked.delete(key);
          }
        }
      }
      await restartRecovery;
      if (quarantined) {
        throw new Error(
          "watcher is quarantined pending authenticated post-finality recovery",
        );
      }
      if (event.kind === "roll_backward") {
        rollbackPoint = event.point;
        for (const [key, block] of buffered) {
          if (
            event.point.kind === "origin" ||
            BigInt(block.slot) > BigInt(event.point.slot) ||
            samePoint(event.point, block)
          ) {
            buffered.delete(key);
          }
        }
        return;
      }
      const block = input.dependencies.admitRollForward(event);
      const key = pointKey(block.blockHash, block.slot);
      const existing = buffered.get(key);
      if (
        existing !== undefined &&
        JSON.stringify(existing) !== JSON.stringify(block)
      ) {
        throw new Error("native buffered block identity was substituted");
      }
      buffered.set(key, block);
      if (rollbackPoint !== null) {
        await processRollbackReplacement(block, event);
        if (quarantined) return;
      }
      await advanceCanonical(event);
      // The SQLite durable snapshot and sparse queue cache are intentionally
      // separate records. A crash can commit finality immediately before the
      // queue hook appends its authenticated sparse cursor. On restart native
      // replay begins at the queue cursor, while the durable finality state is
      // already ahead and therefore has no canonical-progress child to select.
      // Reauthenticate and deliver every such release-final replay block once;
      // exact digest append semantics make a repeated crash idempotent.
      if (
        BigInt(depthAtTip(block, event)) >=
          BigInt(input.policy.confirmationDepth) &&
        !releaseFinalizedHooked.has(key)
      ) {
        const observed = await observe(block, event);
        await input.hooks.onFinalized({
          nativeBlock: block,
          localObservation: observed,
        });
        releaseFinalizedHooked.set(
          key,
          Object.freeze({
            blockHash: block.blockHash,
            blockNo: block.blockNo,
            slot: block.slot,
          }),
        );
      }
      const minimumBlockNo = BigInt(block.blockNo) - 2_160n;
      for (const [bufferedKey, candidate] of buffered) {
        if (BigInt(candidate.blockNo) < minimumBlockNo)
          buffered.delete(bufferedKey);
      }
      for (const [hookedKey, hooked] of releaseFinalizedHooked) {
        if (BigInt(hooked.blockNo) < minimumBlockNo) {
          releaseFinalizedHooked.delete(hookedKey);
        }
      }
    },
    status: () =>
      Object.freeze({
        rollbackPoint,
        quarantined,
        bufferedBlockCount: buffered.size,
      }),
  });
};

export const createWatcherChainCoordinator = (input: {
  readonly policy: WatcherFinalityPolicy;
  readonly durable: WatcherDurableRuntime;
  readonly observation: WatcherLocalKupmiosNativeObservationRuntime;
  readonly restartIntersection?: WatcherNativeChainSyncPoint;
  readonly hooks: WatcherChainCoordinatorHooks;
}): WatcherChainCoordinator =>
  createCoordinator({ ...input, dependencies: productionDependencies });

/** Test-only seam for independently exercising ordering and rollback states. */
export const unsafeCreateWatcherChainCoordinatorForTest = (
  input: {
    readonly policy: WatcherFinalityPolicy;
    readonly durable: WatcherDurableRuntime;
    readonly observation: WatcherLocalKupmiosNativeObservationRuntime;
    readonly restartIntersection?: WatcherNativeChainSyncPoint;
    readonly hooks?: WatcherChainCoordinatorHooks;
  },
  dependencies: Readonly<{ admitRollForward: AdmitRollForward }>,
): WatcherChainCoordinator =>
  createCoordinator({
    ...input,
    hooks:
      input.hooks ??
      Object.freeze({
        onRollback: async () => undefined,
        onFinalized: async () => undefined,
      }),
    dependencies,
  });
