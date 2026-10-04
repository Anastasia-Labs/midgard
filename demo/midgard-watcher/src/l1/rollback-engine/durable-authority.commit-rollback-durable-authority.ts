import { readWatcherLocalUserEventValidation } from "../../indexers/user-event-indexer.js";
import {
  compareAndSwapWatcherDurableAtomicSnapshot,
  makeWatcherDurablePayload,
  makeWatcherDurableStore,
  type WatcherDurableStore,
  watcherSameCanonicalJson,
} from "../../storage/durable-store.js";
import {
  assertWatcherUserEventCheckpointSuccessor,
  parseWatcherUserEventCheckpoint,
  readWatcherUserEventCheckpointPayload,
  type WatcherUserEventArchive,
  type WatcherUserEventCheckpoint,
  type WatcherUserEventCheckpointExpectation,
  watcherUserEventCheckpointExpectationMatches,
  type WatcherUserEventValidation,
} from "../../storage/user-event-checkpoint.js";
import {
  parseWatcherFinalityState,
  type WatcherFinalityState,
} from ".././finality-engine.js";
import {
  encodeWatcherNormalizedL1Block,
  isWatcherL1BlockAttestedBy,
  type WatcherL1TransportAttestationContext,
  type WatcherNormalizedL1Block,
} from ".././l1-adapter.js";
import { type WatcherMultiProviderConsistency } from ".././multi-provider-consistency.js";
import {
  makeRollbackDurableAuthorityHandle,
  makeRollbackDurableAuthoritySnapshot,
  WATCHER_ROLLBACK_CONSISTENCY_HISTORY_BOUND,
} from "./durable-authority.decode-rollback-durable-authority-snapshot.js";
import {
  makeRollbackDurableTrustedHead,
  runtimeForRollbackDurableAuthority,
} from "./durable-authority.parse-rollback-durable-trusted-head.js";
import { sameStrings } from "./records.js";
import { sorted } from "./state.js";
import {
  WATCHER_ROLLBACK_BOUNDS,
  type WatcherRollbackDurableAuthority,
  type WatcherRollbackDurableAuthorityRuntime,
  type WatcherRollbackDurableObservationResult,
  type WatcherRollbackDurableTrustedHead,
  type WatcherRollbackState,
} from "./types.js";

/**
 * Publishes structural checkpoint bytes without evaluating a user-event
 * transition. The upper owner remains responsible for semantic admission and
 * its declared evidence closure. A committed head still needs independent
 * publication/read-back before any protected receipt is issued.
 */
export const persistWatcherRollbackDurableUserEventCheckpoint = async (
  input: WatcherUserEventCheckpointExpectation &
    Readonly<{
      authority: WatcherRollbackDurableAuthority;
      archive: WatcherUserEventArchive;
      nextCheckpoint: unknown;
      validationCandidate?: unknown;
    }>,
): Promise<WatcherRollbackDurableObservationResult> => {
  const runtime = runtimeForRollbackDurableAuthority(input.authority);
  const current = runtime.snapshot.userEventCheckpoint;
  const next = parseWatcherUserEventCheckpoint(input.nextCheckpoint, {
    deploymentMarker: runtime.policy.deploymentMarker,
    network: runtime.policy.network,
    blueprintHash: runtime.policy.blueprintHash,
    finalityPolicyDigest: runtime.policy.policyDigest,
  });
  if (!watcherUserEventCheckpointExpectationMatches(current, input)) {
    return Object.freeze({ persistence: "conflict" });
  }
  if (current?.checkpointDigest !== next.checkpointDigest) {
    assertWatcherUserEventCheckpointSuccessor(current, next);
  }
  await readWatcherUserEventCheckpointPayload(next, input.archive);
  const validation =
    input.validationCandidate === undefined
      ? current?.checkpointDigest === next.checkpointDigest
        ? runtime.snapshot.userEventValidation
        : null
      : readWatcherLocalUserEventValidation(input.validationCandidate, next);
  if (
    current?.checkpointDigest === next.checkpointDigest &&
    watcherSameCanonicalJson(validation, runtime.snapshot.userEventValidation)
  ) {
    return Object.freeze({
      persistence: "unchanged",
      authority: input.authority,
      trustedHead: makeRollbackDurableTrustedHead(
        runtime.policy,
        runtime.snapshot,
        runtime.snapshotSha256,
        runtime.authenticationKey,
      ),
    });
  }
  const committed = await commitRollbackDurableAuthority(
    input.authority,
    runtime.snapshot.currentStore,
    runtime.snapshot.rollbackState,
    runtime.snapshot.rollbackBootstrapState,
    runtime.snapshot.trustedCheckpointStateDigest,
    runtime.snapshot.consistencyHistory,
    next,
    validation,
  );
  return committed === null
    ? Object.freeze({ persistence: "conflict" })
    : Object.freeze({ persistence: "committed", ...committed });
};

// This private commit accepts only results from the validated transition
// builders below (or the parsed checkpoint publisher above). Their source
// capability already carries durable validation. Validate new dependencies
// before calling here, then atomically persist that result and its binding;
// replaying the unchanged prefix here would discard the benefit of validation.
export const commitRollbackDurableAuthority = async (
  authority: WatcherRollbackDurableAuthority,
  currentStore: WatcherDurableStore,
  rollbackState: WatcherRollbackState,
  rollbackBootstrapState: WatcherRollbackState,
  trustedCheckpointStateDigest: string,
  consistencyHistory?: readonly WatcherMultiProviderConsistency[],
  userEventCheckpoint?: WatcherUserEventCheckpoint,
  userEventValidation?: WatcherUserEventValidation | null,
): Promise<Readonly<{
  authority: WatcherRollbackDurableAuthority;
  trustedHead: WatcherRollbackDurableTrustedHead;
}> | null> => {
  const runtime = runtimeForRollbackDurableAuthority(authority);
  const observationIds = new Set(
    currentStore.l1Observations.map(({ observationId }) => observationId),
  );
  const retainedConsistencyHistory = Object.freeze(
    (consistencyHistory ?? runtime.snapshot.consistencyHistory).filter(
      ({ observationEvidenceDigests }) =>
        observationEvidenceDigests.every((digest) =>
          observationIds.has(digest),
        ),
    ),
  );
  const { snapshot, encoded } = makeRollbackDurableAuthoritySnapshot(
    runtime.policy,
    (BigInt(runtime.snapshot.revision) + 1n).toString(),
    runtime.snapshotSha256,
    currentStore,
    retainedConsistencyHistory,
    rollbackState,
    rollbackBootstrapState,
    trustedCheckpointStateDigest,
    userEventCheckpoint ?? runtime.snapshot.userEventCheckpoint,
    runtime.authenticationKey,
    userEventValidation === undefined
      ? runtime.snapshot.userEventValidation
      : userEventValidation,
  );
  const commit = await compareAndSwapWatcherDurableAtomicSnapshot({
    backend: runtime.backend,
    expectedSha256: runtime.snapshotSha256,
    next: encoded,
    canonicalValue: snapshot,
  });
  if (!commit.committed) {
    return null;
  }
  const nextAuthority = makeRollbackDurableAuthorityHandle(
    Object.freeze({
      backend: runtime.backend,
      policy: runtime.policy,
      snapshot,
      encoded,
      snapshotSha256: commit.sha256,
      authenticationKey: runtime.authenticationKey,
    }),
  );
  return Object.freeze({
    authority: nextAuthority,
    trustedHead: makeRollbackDurableTrustedHead(
      runtime.policy,
      snapshot,
      commit.sha256,
      runtime.authenticationKey,
    ),
  });
};

export const currentRollbackFinalityState = (
  runtime: WatcherRollbackDurableAuthorityRuntime,
): WatcherFinalityState => {
  const input =
    runtime.snapshot.rollbackState.transitions.at(-1)?.finalityResult.state ??
    runtime.snapshot.rollbackState.bootstrapFinalityState;
  const parsed = parseWatcherFinalityState(input, runtime.policy);
  if (parsed === null) {
    throw new Error("watcher rollback durable current finality state invalid");
  }
  return parsed;
};

export const storeWithAuthenticatedObservations = (
  source: WatcherDurableStore,
  blocks: readonly WatcherNormalizedL1Block[],
): WatcherDurableStore => {
  const chainPoints = blocks.map((block) =>
    Object.freeze({
      chainPointId: block.chainPoint.chainPointId,
      providerId: block.provider.providerId,
      blockHash: block.chainPoint.blockHash,
      slot: block.chainPoint.slot,
      blockNo: block.chainPoint.blockNo,
      depth: block.chainPoint.depth,
    }),
  );
  const observations = blocks.map((block) =>
    Object.freeze({
      observationId: block.observationDigest,
      providerId: block.provider.providerId,
      chainPointId: block.chainPoint.chainPointId,
      payload: makeWatcherDurablePayload(
        encodeWatcherNormalizedL1Block(block).toString("hex"),
      ),
    }),
  );
  const observationIds = new Set(
    observations.map(({ observationId }) => observationId),
  );
  const chainPointIds = new Set(
    chainPoints.map(({ chainPointId }) => chainPointId),
  );
  return makeWatcherDurableStore({
    deploymentMarker: source.deploymentMarker,
    revision: (BigInt(source.revision) + 1n).toString(),
    records: {
      l1Observations: [
        ...source.l1Observations.filter(
          ({ observationId }) => !observationIds.has(observationId),
        ),
        ...observations,
      ],
      chainPoints: [
        ...source.chainPoints.filter(
          ({ chainPointId }) => !chainPointIds.has(chainPointId),
        ),
        ...chainPoints,
      ],
      protocolUtxos: source.protocolUtxos,
      spentProtocolUtxos: source.spentProtocolUtxos,
      daProofInputs: source.daProofInputs,
      reconstructedStates: source.reconstructedStates,
      decisions: source.decisions,
      faults: source.faults,
      submissions: source.submissions,
      confirmations: source.confirmations,
      retries: source.retries,
      deadlines: source.deadlines,
      correctionResults: source.correctionResults,
    },
  });
};

const frontierBlockNo = (state: WatcherFinalityState): bigint | null => {
  const frontier =
    state.phase === "pending"
      ? (state.finalized ?? state.pending)
      : state.phase === "finalized"
        ? state.finalized
        : null;
  return frontier === null ? null : BigInt(frontier.blockNo);
};

/**
 * Appends one authenticated canonical observation to the durable evidence and
 * retires the evidence that has fallen out of the recovery horizon.
 *
 * Every reader of this evidence looks at most
 * `WATCHER_ROLLBACK_BOUNDS.postFinalityRecoveryDepth` blocks below released
 * finality (or the pending frontier before the first release): a rewind removes only points at or above its
 * replacement, a post-finality recovery path is at most that many blocks
 * long, and a pending restart replays only the pending block's predecessor.
 * A newer pending successor cannot prune the released predecessor's recovery
 * ancestry. Surviving dependent records
 * retain their chain points even when their depth snapshots are compacted.
 *
 * The new input, every retained history entry's observations, and every chain
 * point another record still references are never retired. Below the
 * horizon, while any observation of a block hash is kept, every other
 * observation of that block stays with it, so provider agreement on that block
 * remains checkable; above it, an observation retires with its compacted depth
 * snapshot.
 */
export const nextAuthenticatedEvidenceWithinRecoveryHorizon = (input: {
  readonly source: WatcherDurableStore;
  readonly history: readonly WatcherMultiProviderConsistency[];
  readonly observations: readonly WatcherNormalizedL1Block[];
  readonly consistency: WatcherMultiProviderConsistency;
  readonly frontier: WatcherFinalityState;
}): Readonly<{
  store: WatcherDurableStore;
  history: readonly WatcherMultiProviderConsistency[];
}> => {
  const appended = storeWithAuthenticatedObservations(
    input.source,
    input.observations,
  );
  const appendedHistory = [
    ...input.history.filter(
      ({ consistencyDigest }) =>
        consistencyDigest !== input.consistency.consistencyDigest,
    ),
    input.consistency,
  ];
  const anchor = frontierBlockNo(input.frontier);
  const horizon =
    anchor === null
      ? null
      : anchor - WATCHER_ROLLBACK_BOUNDS.postFinalityRecoveryDepth;
  // Depth changes do not change a block's point/content. Retain the newest
  // evidence for that identity plus the exact snapshots finality references.
  // Reconnects can supply arbitrarily many distinct depths at one height.
  const protectedDigests = new Set([
    input.frontier.pending?.firstSeenConsistencyDigest,
    input.frontier.pending?.lastSeenConsistencyDigest,
    input.frontier.finalized?.firstSeenConsistencyDigest,
    input.frontier.finalized?.lastSeenConsistencyDigest,
    input.frontier.incident?.triggerConsistencyDigest,
    input.consistency.consistencyDigest,
  ]);
  const latestByPoint = new Map<string, WatcherMultiProviderConsistency>();
  for (const consistency of appendedHistory) {
    const agreement = consistency.agreement;
    if (agreement === null) continue;
    const identity = `${agreement.pointDigest}:${agreement.blockContentDigest}`;
    const latest = latestByPoint.get(identity);
    if (
      latest === undefined ||
      BigInt(agreement.minimumDepth) >= BigInt(latest.agreement!.minimumDepth)
    )
      latestByPoint.set(identity, consistency);
  }
  const latest = new Set(latestByPoint.values());
  const retained = appendedHistory.filter(
    (consistency) =>
      protectedDigests.has(consistency.consistencyDigest) ||
      ((horizon === null ||
        horizon <= 0n ||
        consistency.agreement === null ||
        BigInt(consistency.agreement.blockNo) >= horizon) &&
        (consistency.agreement === null || latest.has(consistency))),
  );
  let store = appended;
  if (retained.length !== appendedHistory.length) {
    const kept = new Set([
      ...input.observations.map(({ observationDigest }) => observationDigest),
      ...retained.flatMap(
        ({ observationEvidenceDigests }) => observationEvidenceDigests,
      ),
    ]);
    const retainedEntries = new Set(retained);
    const retired = new Set(
      appendedHistory
        .filter((consistency) => !retainedEntries.has(consistency))
        .flatMap(({ observationEvidenceDigests }) => observationEvidenceDigests)
        .filter((digest) => !kept.has(digest)),
    );
    const points = new Map(
      appended.chainPoints.map((point) => [point.chainPointId, point]),
    );
    const belowHorizon = (chainPointId: string): boolean => {
      const point = points.get(chainPointId);
      return (
        horizon !== null &&
        point !== undefined &&
        BigInt(point.blockNo) < horizon
      );
    };
    // Above the horizon an observation retires with its compacted depth
    // snapshot. Below it, an observation leaves only with every other
    // observation of its block.
    const retirable = new Set(
      appended.l1Observations.filter(({ observationId }) =>
        retired.has(observationId),
      ),
    );
    const belowHorizonRetirable = new Set(
      [...retirable].filter(({ chainPointId }) => belowHorizon(chainPointId)),
    );
    const blockHash = (chainPointId: string) =>
      points.get(chainPointId)?.blockHash;
    const retainedBlockHashes = new Set(
      appended.l1Observations
        .filter((observation) => !retirable.has(observation))
        .map(({ chainPointId }) => blockHash(chainPointId)),
    );
    const retiredObservations = new Set(
      [...retirable].filter(
        (observation) =>
          !belowHorizonRetirable.has(observation) ||
          !retainedBlockHashes.has(blockHash(observation.chainPointId)),
      ),
    );
    const l1Observations = appended.l1Observations.filter(
      (observation) => !retiredObservations.has(observation),
    );
    const referenced = new Set([
      ...l1Observations.map(({ chainPointId }) => chainPointId),
      ...appended.protocolUtxos.map(({ chainPointId }) => chainPointId),
      ...appended.spentProtocolUtxos.flatMap(
        ({ chainPointId, spentAtChainPointId }) => [
          chainPointId,
          spentAtChainPointId,
        ],
      ),
      ...appended.reconstructedStates.map(({ chainPointId }) => chainPointId),
      ...appended.confirmations.map(({ chainPointId }) => chainPointId),
    ]);
    const retiredPoints = new Set(
      [...retiredObservations]
        .map(({ chainPointId }) => chainPointId)
        .filter((chainPointId) => !referenced.has(chainPointId)),
    );
    store = makeWatcherDurableStore({
      deploymentMarker: appended.deploymentMarker,
      revision: appended.revision,
      records: {
        ...appended,
        l1Observations,
        chainPoints: appended.chainPoints.filter(
          ({ chainPointId }) => !retiredPoints.has(chainPointId),
        ),
      },
    });
  }
  // The horizon spans 2,161 heights; equivalent depth snapshots are compacted.
  // Competing block identities retain their evidence and fail closed at this cap.
  if (retained.length > WATCHER_ROLLBACK_CONSISTENCY_HISTORY_BOUND) {
    throw new Error(
      "watcher authenticated consistency history exceeds its bound",
    );
  }
  return Object.freeze({ store, history: Object.freeze(retained) });
};

export const authenticatesCanonicalBlock = (input: {
  readonly block: WatcherNormalizedL1Block;
  readonly observations: readonly WatcherNormalizedL1Block[];
  readonly consistency: WatcherMultiProviderConsistency;
  readonly transportAttestations: readonly WatcherL1TransportAttestationContext[];
}): boolean =>
  input.observations.length === input.consistency.observationCount &&
  input.observations.some(
    ({ observationDigest }) =>
      observationDigest === input.block.observationDigest,
  ) &&
  input.observations.every((observation) =>
    input.transportAttestations.some((context) =>
      isWatcherL1BlockAttestedBy(observation, context),
    ),
  ) &&
  sameStrings(
    sorted(
      input.observations.map(({ observationDigest }) => observationDigest),
    ),
    input.consistency.observationEvidenceDigests,
  ) &&
  input.consistency.status === "agreed" &&
  input.consistency.protocolDecision === "allowed" &&
  input.consistency.chainAuthorityObservationDigest ===
    input.block.observationDigest &&
  input.consistency.agreement?.pointDigest ===
    input.block.chainPoint.pointDigest &&
  input.consistency.agreement.blockContentDigest ===
    input.block.blockContentDigest;
