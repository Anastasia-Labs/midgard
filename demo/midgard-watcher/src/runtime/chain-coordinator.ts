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
import type { WatcherDurableRuntime } from "../storage/durable-runtime.js";

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
  // A cold replay can be arbitrarily far behind the live tip. Recovery bounds
  // constrain replacement paths, not a canonical block's confirmation depth.
  if (depth <= 0n) {
    throw new Error("native block is ahead of its advertised tip");
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
  const captured = new Map<
    string,
    {
      first: WatcherLocalKupmiosNativeObservation;
      firstDepth: string;
      latest: WatcherLocalKupmiosNativeObservation;
      latestDepth: string;
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
      });
    } else {
      prior.latest = observation;
      prior.latestDepth = depth;
    }
    return observation;
  };

  const deliverFinalized = async (
    block: WatcherNativeBlockAdmission,
    observation: WatcherLocalKupmiosNativeObservation,
  ): Promise<void> => {
    const key = pointKey(block.blockHash, block.slot);
    if (releaseFinalizedHooked.has(key)) return;
    await input.hooks.onFinalized({
      nativeBlock: block,
      localObservation: observation,
    });
    releaseFinalizedHooked.set(
      key,
      Object.freeze({
        blockHash: block.blockHash,
        blockNo: block.blockNo,
        slot: block.slot,
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
      await deliverFinalized(block, await observe(block, event));
      const key = pointKey(block.blockHash, block.slot);
      buffered.delete(key);
      captured.delete(key);
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
  };

  const advanceCanonical = async (
    event: Extract<
      WatcherNativeChainSyncEvent,
      { readonly kind: "roll_forward" }
    >,
  ): Promise<void> => {
    const maximumIterations = buffered.size + 1;
    for (let iteration = 0; iteration < maximumIterations; iteration += 1) {
      const state = input.durable.readFinality();
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
      const key = pointKey(target.blockHash, target.slot);
      const arrival = captured.get(key);
      if (arrival === undefined) {
        throw new Error(
          "native buffered block has no authenticated first observation",
        );
      }
      // A child may have waited behind the pending head. Preserve the actual
      // earlier observation instead of making its first visibility the later
      // tip at which it becomes the canonical child.
      let observed =
        state.phase === "pending"
          ? await observe(target, event)
          : arrival.first;
      let progress = await input.durable.persistCanonicalProgress(observed);
      if (progress.persistence === "conflict") {
        throw new Error("watcher canonical progress persistence conflicted");
      }
      if (
        progress.finalityResult.action !== "finalize" &&
        state.phase !== "pending" &&
        BigInt(depthAtTip(target, event)) > BigInt(arrival.firstDepth)
      ) {
        observed = await observe(target, event);
        progress = await input.durable.persistCanonicalProgress(observed);
        if (progress.persistence === "conflict") {
          throw new Error("watcher canonical progress persistence conflicted");
        }
      }
      if (progress.finalityResult.action !== "finalize") return;
      await deliverFinalized(target, observed);
      buffered.delete(key);
      captured.delete(key);
    }
    throw new Error("watcher canonical buffer did not converge");
  };

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
        // frame. This first exact acknowledgement is not a rewind, including
        // when the sparse queue cursor trails retained durable finality.
        await restartRecovery;
        if (!quarantined) return;
      }
      if (event.kind === "roll_backward") {
        // The production hook invalidates the in-memory actuation generation
        // before awaiting its durable cache rollback.
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
        rollbackPoint = event.point;
        for (const [key, block] of buffered) {
          if (
            event.point.kind === "origin" ||
            BigInt(block.slot) > BigInt(event.point.slot) ||
            samePoint(event.point, block)
          ) {
            buffered.delete(key);
            captured.delete(key);
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
      await observe(block, event);
      if (!(await replayRetainedPrefix(event))) return;
      await advanceCanonical(event);
      const minimumBlockNo = BigInt(block.blockNo) - 2_160n;
      for (const [bufferedKey, candidate] of buffered) {
        if (BigInt(candidate.blockNo) < minimumBlockNo) {
          buffered.delete(bufferedKey);
          captured.delete(bufferedKey);
        }
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
