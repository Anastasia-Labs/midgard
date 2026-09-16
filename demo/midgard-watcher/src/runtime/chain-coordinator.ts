import {
  evaluateWatcherFinality,
  makeWatcherFinalityBootstrapState,
  type WatcherFinalityPolicy,
} from "../l1/finality-engine.js";
import type { WatcherLocalKupmiosNativeObservationRuntime } from "../l1/local-kupmios-native-observation.js";
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
import type { WatcherBlockProgressStore } from "../storage/block-progress-store.js";
import type { WatcherDurableRuntime } from "../storage/durable-runtime.js";
import type { WatcherBlockRelevance } from "./block-relevance.js";

export const WATCHER_CHAIN_COORDINATOR_SCHEMA_VERSION =
  "midgard-watcher-production-chain-coordinator-v1" as const;

export type WatcherChainCoordinator = Readonly<{
  schemaVersion: typeof WATCHER_CHAIN_COORDINATOR_SCHEMA_VERSION;
  handle(event: WatcherNativeChainSyncEvent): Promise<void>;
  status(): Readonly<{
    rollbackPoint: WatcherNativeChainSyncPoint | null;
    quarantined: boolean;
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
   * Quiet blocks skip the network observation and the durable authority.
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
  /** Must revoke actuation authority synchronously before its first await. */
  onRollback(point: WatcherNativeChainSyncPoint): Promise<void>;
  /**
   * Runs once for every exact release-final block, including authenticated
   * restart replay that the durable finality snapshot has already passed.
   */
  onFinalized(
    input: Readonly<{
      nativeBlock: WatcherNativeBlockAdmission;
      /** Absent for quiet blocks, which are never observed over the network. */
      localObservation: WatcherLocalKupmiosNativeObservation | null;
      relevance: WatcherBlockRelevance;
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
  // A cold replay can be arbitrarily far behind the live tip. Recovery bounds
  // constrain replacement paths, not a canonical block's confirmation depth.
  if (depth <= 0n) {
    throw new Error("native block is ahead of its advertised tip");
  }
  return depth.toString();
};

const pointKey = (blockHash: string, slot: string): string =>
  `${blockHash}@${slot}`;

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
  readonly dependencies: Readonly<{ admitRollForward: AdmitRollForward }> &
    WatcherChainCoordinatorDependencies;
}): WatcherChainCoordinator => {
  const buffered = new Map<string, WatcherNativeBlockAdmission>();
  const relevances = new Map<string, WatcherBlockRelevance>();
  const relevanceOf = (
    block: WatcherNativeBlockAdmission,
  ): WatcherBlockRelevance => {
    const key = pointKey(block.blockHash, block.slot);
    const known = relevances.get(key);
    if (known !== undefined) return known;
    const relevance = input.dependencies.relevance?.(block) ?? "touched";
    relevances.set(key, relevance);
    return relevance;
  };
  const forget = (key: string): void => {
    buffered.delete(key);
    captured.delete(key);
    relevances.delete(key);
  };
  const captured = new Map<
    string,
    {
      first: WatcherLocalKupmiosNativeObservation;
      firstDepth: string;
      latest: WatcherLocalKupmiosNativeObservation;
      latestDepth: string;
      /** Depth of the newest observation the durable authority has seen. */
      persistedDepth: string | null;
    }
  >();
  const retainedFinality = input.durable.readFinality();
  // A sparse queue cursor may lag the finality snapshot after a crash. Only
  // this retained prefix can be replayed without advancing durable finality.
  // A pending successor does not itself belong to the finalized prefix.
  let replayBoundary =
    retainedFinality.phase === "finalized" &&
    retainedFinality.finalized !== null
      ? { point: retainedFinality.finalized, inclusive: true }
      : retainedFinality.phase === "pending" &&
          retainedFinality.pending !== null
        ? { point: retainedFinality.pending, inclusive: false }
        : null;
  const retainedHistory =
    retainedFinality.phase === "pending"
      ? (input.durable.read().authenticatedConsistencyHistory ?? [])
      : [];
  const progress = input.dependencies.progress ?? null;
  let progressHead = progress?.readHead() ?? null;
  const authorityFinalizedHead = (): WatcherProcessedHead | null => {
    const state = input.durable.readFinality();
    return state.phase === "finalized" && state.finalized !== null
      ? Object.freeze({
          blockHash: state.finalized.blockHash,
          blockNo: state.finalized.blockNo,
          slot: state.finalized.slot,
        })
      : null;
  };
  const headOf = (
    record: Readonly<{ blockHash: string; blockNo: string; slot: string }>,
  ): WatcherProcessedHead =>
    Object.freeze({
      blockHash: record.blockHash,
      blockNo: record.blockNo,
      slot: record.slot,
    });
  // "Processed through": the progress ring first, then the durable finality
  // authority. Everything at or below it is recorded fact until a rollback.
  const recomputeEffectiveHead = (): WatcherProcessedHead | null =>
    progressHead !== null ? headOf(progressHead) : authorityFinalizedHead();
  let effectiveHead = recomputeEffectiveHead();
  if (
    replayBoundary !== null &&
    replayBoundary.inclusive &&
    progress !== null &&
    progressHead !== null &&
    input.restartIntersection?.kind === "point" &&
    BigInt(progressHead.blockNo) >= BigInt(replayBoundary.point.blockNo)
  ) {
    // The progress ring attests every block from durable finality to the
    // restart point; there is no retained prefix left to replay.
    const intersection = input.restartIntersection;
    const attested = progress
      .readRange({
        afterBlockNo: (BigInt(replayBoundary.point.blockNo) - 1n).toString(),
        throughBlockNo: progressHead.blockNo,
      })
      .some(
        (row) =>
          row.blockHash === intersection.blockHash &&
          row.slot === intersection.slot,
      );
    if (attested) replayBoundary = null;
  }
  let retainedReplay: readonly WatcherNativeBlockAdmission[] | null = null;
  let retainedReplacement: Readonly<{
    block: WatcherNativeBlockAdmission;
    parent: Extract<WatcherNativeChainSyncPoint, { readonly kind: "point" }>;
  }> | null = null;
  if (
    replayBoundary !== null &&
    input.restartIntersection?.kind === "point" &&
    input.restartIntersection.blockHash === replayBoundary.point.blockHash &&
    input.restartIntersection.slot === replayBoundary.point.slot
  ) {
    replayBoundary = null;
  }
  let rollbackPoint: WatcherNativeChainSyncPoint | null = null;
  let firstNativeEvent = true;
  let quarantined = input.durable.readFinality().phase === "quarantined";
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
  ): Promise<WatcherLocalKupmiosNativeObservation> => {
    const key = pointKey(block.blockHash, block.slot);
    const depth = depthAtTip(block, event);
    const prior = captured.get(key);
    if (prior?.latestDepth === depth) return prior.latest;
    const observation = await input.observation.observe({ block, depth });
    if (prior === undefined) {
      captured.set(key, {
        first: observation,
        firstDepth: depth,
        latest: observation,
        latestDepth: depth,
        persistedDepth: null,
      });
    } else {
      prior.latest = observation;
      prior.latestDepth = depth;
    }
    return observation;
  };

  const recordProgress = (
    block: WatcherNativeBlockAdmission,
    relevance: WatcherBlockRelevance,
  ): void => {
    if (progress !== null) {
      if (
        progressHead !== null &&
        BigInt(block.blockNo) <= BigInt(progressHead.blockNo)
      ) {
        if (
          progressHead.blockNo === block.blockNo &&
          progressHead.blockHash !== block.blockHash
        ) {
          throw new Error(
            "watcher block progress head conflicts with the finalized block",
          );
        }
      } else {
        if (
          progressHead !== null &&
          progressHead.blockHash !== block.prevHash
        ) {
          // Only a contiguous ring attests ancestry. After a rewind to a point
          // the ring never recorded, the durable authority alone anchors it.
          progress.rollbackTo({ kind: "origin" });
          progressHead = null;
        }
        const record = Object.freeze({
          blockHash: block.blockHash,
          parentBlockHash: block.prevHash,
          blockNo: block.blockNo,
          slot: block.slot,
          relevance,
        });
        const finalized = authorityFinalizedHead();
        progress.record(
          record,
          finalized === null
            ? undefined
            : { retainFromBlockNo: finalized.blockNo },
        );
        progressHead = record;
      }
    }
    if (
      effectiveHead === null ||
      BigInt(block.blockNo) > BigInt(effectiveHead.blockNo)
    ) {
      effectiveHead = headOf(block);
    }
  };

  const deliverFinalized = async (
    block: WatcherNativeBlockAdmission,
    observation: WatcherLocalKupmiosNativeObservation | null,
    relevance: WatcherBlockRelevance,
  ): Promise<void> => {
    const key = pointKey(block.blockHash, block.slot);
    if (releaseFinalizedHooked.has(key)) return;
    await input.hooks.onFinalized({
      nativeBlock: block,
      localObservation: observation,
      relevance,
    });
    releaseFinalizedHooked.set(
      key,
      Object.freeze({
        blockHash: block.blockHash,
        blockNo: block.blockNo,
        slot: block.slot,
      }),
    );
    // Written after the block's own transactions: a crash between them costs
    // one idempotent replay of this block on restart, never lost work.
    recordProgress(block, relevance);
  };

  const ancestryFromFinalized = (
    finalized: WatcherProcessedHead,
    target: WatcherNativeBlockAdmission,
  ) => {
    if (BigInt(target.blockNo) === BigInt(finalized.blockNo) + 1n) return [];
    if (progress === null) return [];
    return progress
      .readRange({
        afterBlockNo: finalized.blockNo,
        throughBlockNo: (BigInt(target.blockNo) - 1n).toString(),
      })
      .map((row) =>
        Object.freeze({
          blockHash: row.blockHash,
          parentBlockHash: row.parentBlockHash,
          blockNo: row.blockNo,
          slot: row.slot,
        }),
      );
  };

  const replayRetainedPrefix = async (
    event: Extract<
      WatcherNativeChainSyncEvent,
      { readonly kind: "roll_forward" }
    >,
  ): Promise<boolean> => {
    if (replayBoundary === null) return true;
    const boundary = replayBoundary;
    if (retainedReplay === null) {
      const intersection = input.restartIntersection;
      if (intersection?.kind !== "point") {
        throw new Error(
          "retained finality replay requires an exact restart intersection",
        );
      }
      if (BigInt(event.blockNo) < BigInt(boundary.point.blockNo)) return false;
      const candidates = [...buffered.values()]
        .filter(
          (block) => BigInt(block.blockNo) <= BigInt(boundary.point.blockNo),
        )
        .sort((left, right) =>
          BigInt(left.blockNo) < BigInt(right.blockNo) ? -1 : 1,
        );
      const last = candidates.at(-1);
      if (
        last === undefined ||
        last.blockNo !== boundary.point.blockNo ||
        (boundary.inclusive &&
          (last.blockHash !== boundary.point.blockHash ||
            last.slot !== boundary.point.slot)) ||
        (last.blockHash === boundary.point.blockHash &&
          last.slot !== boundary.point.slot)
      ) {
        throw new Error(
          "native replay differs from the retained finality boundary",
        );
      }
      let parentHash = intersection.blockHash;
      let parentSlot = intersection.slot;
      let parentBlockNo: string | null = null;
      for (const block of candidates) {
        if (
          block.prevHash !== parentHash ||
          BigInt(block.slot) <= BigInt(parentSlot) ||
          (parentBlockNo !== null &&
            BigInt(block.blockNo) !== BigInt(parentBlockNo) + 1n)
        ) {
          throw new Error(
            "native replay is not a contiguous retained finality prefix",
          );
        }
        parentHash = block.blockHash;
        parentSlot = block.slot;
        parentBlockNo = block.blockNo;
      }
      const prefix = boundary.inclusive ? candidates : candidates.slice(0, -1);
      if (!boundary.inclusive && prefix.length > 0) {
        // Pending alone is not finality authority. Re-establish its exact
        // predecessor's finalized state from the retained authenticated
        // observations, then bind that point to the native ancestry above.
        const predecessor = prefix.at(-1)!;
        const history = retainedHistory
          .filter(
            ({ agreement }) =>
              agreement?.blockHash === predecessor.blockHash &&
              agreement.blockNo === predecessor.blockNo &&
              agreement.slot === predecessor.slot,
          )
          .sort((left, right) =>
            BigInt(left.agreement!.minimumDepth) <
            BigInt(right.agreement!.minimumDepth)
              ? -1
              : 1,
          );
        let state = makeWatcherFinalityBootstrapState(input.policy);
        for (const consistency of history) {
          if (state === null) break;
          const result = evaluateWatcherFinality(
            input.policy,
            state,
            consistency,
          );
          if (result.action === "reject" || result.state === null) {
            state = null;
            break;
          }
          state = result.state;
          if (state.phase === "finalized") break;
        }
        if (
          state?.phase !== "finalized" ||
          state.finalized?.blockHash !== predecessor.blockHash ||
          state.finalized.blockNo !== predecessor.blockNo ||
          state.finalized.slot !== predecessor.slot
        ) {
          throw new Error(
            "pending replay prefix lacks retained predecessor finality",
          );
        }
      }
      retainedReplay = prefix;
      if (!boundary.inclusive && last.blockHash !== boundary.point.blockHash) {
        const parent = candidates.at(-2);
        retainedReplacement = {
          block: last,
          parent:
            parent === undefined
              ? intersection
              : {
                  kind: "point",
                  blockHash: parent.blockHash,
                  slot: parent.slot,
                },
        };
      }
    }
    while (retainedReplay.length > 0) {
      const block = retainedReplay[0]!;
      if (
        BigInt(depthAtTip(block, event)) <
        BigInt(input.policy.confirmationDepth)
      ) {
        return false;
      }
      await deliverFinalized(
        block,
        await observe(block, event),
        relevanceOf(block),
      );
      forget(pointKey(block.blockHash, block.slot));
      retainedReplay = retainedReplay.slice(1);
    }
    replayBoundary = null;
    retainedReplay = null;
    if (retainedReplacement !== null) {
      // FindIntersect can select an ancestor of an orphaned pending point.
      // Once its replacement and ancestry are authenticated, use the normal
      // pre-finality rewind path. A finalized boundary never takes this path.
      const replacement = retainedReplacement;
      retainedReplacement = null;
      await input.hooks.onRollback(replacement.parent);
      rollbackPoint = replacement.parent;
      await processRollbackReplacement(replacement.block, event);
      if (quarantined) return false;
    }
    return true;
  };

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
    progressHead = progress?.readHead() ?? null;
    effectiveHead = recomputeEffectiveHead();
  };

  const advanceCanonical = async (
    event: Extract<
      WatcherNativeChainSyncEvent,
      { readonly kind: "roll_forward" }
    >,
  ): Promise<void> => {
    const confirmationDepth = BigInt(input.policy.confirmationDepth);
    const maximumIterations = buffered.size + 1;
    for (let iteration = 0; iteration < maximumIterations; iteration += 1) {
      const state = input.durable.readFinality();
      if (state.phase === "quarantined") {
        quarantined = true;
        return;
      }
      if (state.phase === "pending" && state.pending !== null) {
        const pending = state.pending;
        const target =
          [...buffered.values()].find(
            (block) =>
              block.blockHash === pending.blockHash &&
              block.slot === pending.slot &&
              block.blockNo === pending.blockNo,
          ) ?? null;
        if (target === null) return;
        const depth = depthAtTip(target, event);
        // Finality needs a second observation at confirmation depth. Every
        // shallower arrival would only persist another pending snapshot.
        if (BigInt(depth) < confirmationDepth) return;
        const key = pointKey(target.blockHash, target.slot);
        if (captured.get(key)?.persistedDepth === depth) return;
        const observed = await observe(target, event);
        const progressed =
          await input.durable.persistCanonicalProgress(observed);
        if (progressed.persistence === "conflict") {
          throw new Error("watcher canonical progress persistence conflicted");
        }
        captured.get(key)!.persistedDepth = depth;
        if (progressed.finalityResult.action !== "finalize") return;
        await deliverFinalized(target, observed, relevanceOf(target));
        forget(key);
        continue;
      }
      const finalized = authorityFinalizedHead();
      const head = effectiveHead;
      const target = nextBufferedChild(
        buffered,
        head?.blockHash ?? null,
        head?.blockNo ?? null,
      );
      if (target === null) return;
      const key = pointKey(target.blockHash, target.slot);
      const relevance = relevanceOf(target);
      const atFinalized =
        finalized !== null &&
        finalized.blockHash === target.blockHash &&
        finalized.slot === target.slot;
      if (
        finalized !== null &&
        !atFinalized &&
        BigInt(target.blockNo) <= BigInt(finalized.blockNo)
      ) {
        throw new Error(
          "watcher processed head trails durable finality by more than one block",
        );
      }
      if (atFinalized) {
        // The authority committed this block but its progress row is absent:
        // the process stopped between them. Re-run its idempotent hooks.
        const observed =
          relevance === "touched"
            ? (captured.get(key)?.latest ?? (await observe(target, event)))
            : null;
        await deliverFinalized(target, observed, relevance);
        forget(key);
        continue;
      }
      const forcedCheckpoint =
        finalized !== null &&
        BigInt(target.blockNo) - BigInt(finalized.blockNo) >=
          WATCHER_AUTHORITY_CHECKPOINT_INTERVAL_BLOCKS;
      if (relevance === "quiet" && !forcedCheckpoint) {
        // A quiet block is final once it is deep enough; nothing else about
        // it is ever consulted.
        if (BigInt(depthAtTip(target, event)) < confirmationDepth) return;
        await deliverFinalized(target, null, "quiet");
        forget(key);
        continue;
      }
      if (!captured.has(key)) await observe(target, event);
      const arrival = captured.get(key)!;
      let observed = arrival.first;
      const ancestry =
        finalized === null ? [] : ancestryFromFinalized(finalized, target);
      let progressed = await input.durable.persistCanonicalProgress({
        ...observed,
        ancestry,
      });
      if (progressed.persistence === "conflict") {
        throw new Error("watcher canonical progress persistence conflicted");
      }
      arrival.persistedDepth = arrival.firstDepth;
      if (
        progressed.finalityResult.action !== "finalize" &&
        BigInt(depthAtTip(target, event)) > BigInt(arrival.firstDepth)
      ) {
        observed = await observe(target, event);
        progressed = await input.durable.persistCanonicalProgress(observed);
        if (progressed.persistence === "conflict") {
          throw new Error("watcher canonical progress persistence conflicted");
        }
        arrival.persistedDepth = depthAtTip(target, event);
      }
      if (progressed.finalityResult.action !== "finalize") return;
      await deliverFinalized(target, observed, relevance);
      forget(key);
    }
    throw new Error("watcher canonical buffer did not converge");
  };

  const includedHooked = new Set<string>();

  return Object.freeze({
    schemaVersion: WATCHER_CHAIN_COORDINATOR_SCHEMA_VERSION,
    handle: async (event) => {
      const selected = input.restartIntersection;
      const initialAcknowledgement =
        firstNativeEvent &&
        event.kind === "roll_backward" &&
        selected !== undefined &&
        (selected.kind === "origin"
          ? event.point.kind === "origin"
          : event.point.kind === "point" &&
            event.point.blockHash === selected.blockHash &&
            event.point.slot === selected.slot);
      firstNativeEvent = false;
      if (initialAcknowledgement) {
        // Native FindIntersect acknowledges the selected point with a backward
        // frame. This first exact acknowledgement is not a rewind unless the
        // node intersected below the recorded processed head.
        await restartRecovery;
        if (!quarantined) {
          // With a progress ring the node was offered the recorded head
          // first, so a lower selection means it lacks that head: a rewind.
          // Without one, a lower selection is the lagging queue cursor and
          // the retained prefix replays below.
          const head = effectiveHead;
          if (
            progress === null ||
            head === null ||
            (event.point.kind === "point" &&
              event.point.blockHash === head.blockHash &&
              event.point.slot === head.slot)
          )
            return;
        }
      }
      if (event.kind === "roll_backward") {
        // The production hook invalidates the in-memory actuation generation
        // before awaiting its durable cache rollback.
        includedHooked.clear();
        await input.hooks.onRollback(event.point);
        replayBoundary = null;
        retainedReplay = null;
        retainedReplacement = null;
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
        const point = event.point;
        for (const [key, block] of buffered) {
          if (
            point.kind === "origin" ||
            BigInt(block.slot) > BigInt(point.slot) ||
            (block.slot === point.slot && block.blockHash !== point.blockHash)
          ) {
            forget(key);
          }
        }
        progress?.rollbackTo(point);
        progressHead = progress?.readHead() ?? null;
        const finality = input.durable.readFinality();
        const frontier =
          finality.phase === "pending"
            ? finality.pending
            : finality.phase === "finalized"
              ? finality.finalized
              : null;
        const belowAuthority =
          frontier === null ||
          frontier === undefined ||
          point.kind === "origin" ||
          BigInt(point.slot) < BigInt(frontier.slot) ||
          (point.slot === frontier.slot &&
            point.blockHash !== frontier.blockHash);
        if (belowAuthority) {
          // The durable authority itself is contradicted (or has not been
          // established yet): its dedicated rewind path evaluates the
          // replacement block.
          rollbackPoint = point;
          return;
        }
        // Only quiet, ring-attested history above the authority is affected.
        effectiveHead = recomputeEffectiveHead();
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
      // Touched blocks are observed at first visibility; quiet blocks cost
      // nothing beyond the local classification until they finalize.
      if (relevanceOf(block) === "touched") await observe(block, event);
      if (!(await replayRetainedPrefix(event))) return;
      await advanceCanonical(event);
      if (input.hooks.onIncluded !== undefined) {
        // Replay the volatile suffix in native order. Finalized processing keeps
        // its own cursor and may still be waiting on its oldest pending block.
        let includedParent = effectiveHead;
        for (const candidate of [...buffered.values()].sort((left, right) =>
          BigInt(left.blockNo) < BigInt(right.blockNo) ? -1 : 1,
        )) {
          if (
            includedParent !== null &&
            (candidate.prevHash !== includedParent.blockHash ||
              BigInt(candidate.blockNo) !==
                BigInt(includedParent.blockNo) + 1n ||
              BigInt(candidate.slot) <= BigInt(includedParent.slot))
          )
            throw new Error(
              "included native suffix is not contiguous with canonical progress",
            );
          includedParent = headOf(candidate);
          const candidateKey = pointKey(candidate.blockHash, candidate.slot);
          if (includedHooked.has(candidateKey)) continue;
          const relevance = relevanceOf(candidate);
          await input.hooks.onIncluded({
            nativeBlock: candidate,
            relevance,
            localObservation:
              relevance === "touched" ? await observe(candidate, event) : null,
          });
          includedHooked.add(candidateKey);
        }
        for (const includedKey of includedHooked)
          if (!buffered.has(includedKey)) includedHooked.delete(includedKey);
      }
      const minimumBlockNo = BigInt(block.blockNo) - 2_160n;
      for (const [bufferedKey, candidate] of buffered) {
        if (BigInt(candidate.blockNo) < minimumBlockNo) forget(bufferedKey);
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
        processedThrough: effectiveHead,
      }),
  });
};

export const createWatcherChainCoordinator = (input: {
  readonly policy: WatcherFinalityPolicy;
  readonly durable: WatcherDurableRuntime;
  readonly observation: WatcherLocalKupmiosNativeObservationRuntime;
  readonly restartIntersection?: WatcherNativeChainSyncPoint;
  readonly hooks: WatcherChainCoordinatorHooks;
  readonly relevance?: WatcherChainCoordinatorDependencies["relevance"];
  readonly progress?: WatcherChainCoordinatorDependencies["progress"];
}): WatcherChainCoordinator =>
  createCoordinator({
    ...input,
    dependencies: {
      ...productionDependencies,
      relevance: input.relevance,
      progress: input.progress,
    },
  });

/** Test-only seam for independently exercising ordering and rollback states. */
export const unsafeCreateWatcherChainCoordinatorForTest = (
  input: {
    readonly policy: WatcherFinalityPolicy;
    readonly durable: WatcherDurableRuntime;
    readonly observation: WatcherLocalKupmiosNativeObservationRuntime;
    readonly restartIntersection?: WatcherNativeChainSyncPoint;
    readonly hooks?: WatcherChainCoordinatorHooks;
  },
  dependencies: Readonly<{ admitRollForward: AdmitRollForward }> &
    WatcherChainCoordinatorDependencies,
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
