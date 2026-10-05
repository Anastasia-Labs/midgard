import {
  type WatcherDurableStore,
  watcherSameCanonicalJson,
} from "../../storage/durable-store.js";
import { type WatcherFinalityPolicy } from ".././finality-engine.js";
import {
  encodeWatcherNormalizedL1Block,
  type WatcherL1TransportAttestationContext,
  type WatcherNormalizedL1Block,
} from ".././l1-adapter.js";
import { type WatcherMultiProviderConsistency } from ".././multi-provider-consistency.js";
import {
  authenticatesCanonicalBlock,
  commitRollbackDurableAuthority,
  currentRollbackFinalityState,
  nextAuthenticatedEvidenceWithinRecoveryHorizon,
} from "./durable-authority.commit-rollback-durable-authority.js";
import {
  makeRollbackDurableTrustedHead,
  runtimeForRollbackDurableAuthority,
} from "./durable-authority.parse-rollback-durable-trusted-head.js";
import { freezeRollbackSnapshotJson } from "./durable-authority.rollback-authority-canonical.js";
import {
  makeEpochBootstrapState,
  type PersistedObservationIndexEntry,
  verifyPersistedConsistencyEvidence,
} from "./state.js";
import {
  type WatcherRollbackDurableAuthority,
  type WatcherRollbackDurableObservationResult,
} from "./types.js";

/** Verify the newly appended evidence without decoding unchanged history.
 * The source is privately owned; the store builder only changes these two
 * record collections. An existing identity must retain its exact content. */
export const assertCanonicalProgressEvidence = (
  policy: WatcherFinalityPolicy,
  source: WatcherDurableStore,
  next: WatcherDurableStore,
  input: {
    readonly observations: readonly WatcherNormalizedL1Block[];
    readonly consistency: WatcherMultiProviderConsistency;
    readonly transportAttestations: readonly WatcherL1TransportAttestationContext[];
  },
): void => {
  const index = new Map<string, PersistedObservationIndexEntry>();
  for (const observation of input.observations) {
    const durable = next.l1Observations.find(
      ({ observationId }) => observationId === observation.observationDigest,
    );
    const point = next.chainPoints.find(
      ({ chainPointId }) =>
        chainPointId === observation.chainPoint.chainPointId,
    );
    const priorObservation = source.l1Observations.find(
      ({ observationId }) => observationId === observation.observationDigest,
    );
    const priorPoint = source.chainPoints.find(
      ({ chainPointId }) =>
        chainPointId === observation.chainPoint.chainPointId,
    );
    if (
      durable === undefined ||
      point === undefined ||
      index.has(observation.observationDigest) ||
      (priorObservation !== undefined &&
        !watcherSameCanonicalJson(priorObservation, durable)) ||
      (priorPoint !== undefined && !watcherSameCanonicalJson(priorPoint, point))
    ) {
      throw new Error("watcher canonical progress changed retained evidence");
    }
    index.set(observation.observationDigest, { durable, point, observation });
  }
  if (
    verifyPersistedConsistencyEvidence(
      policy,
      next,
      input.consistency,
      input.transportAttestations,
      index,
    ) === null
  ) {
    throw new Error(
      "watcher canonical progress evidence failed live verification",
    );
  }
};

/** One authenticated canonical block and the evidence that admitted it. */
export type WatcherRollbackDurableObservationEntry = Readonly<{
  block: WatcherNormalizedL1Block;
  observations: readonly WatcherNormalizedL1Block[];
  consistency: WatcherMultiProviderConsistency;
  transportAttestations: readonly WatcherL1TransportAttestationContext[];
}>;

/**
 * Journals authenticated replacement evidence before a rewind/incident is
 * evaluated. This operation changes no finality or rollback decision and its
 * emitted head still requires external CAS publication before use.
 */
export const persistWatcherRollbackDurableObservation = async (
  input: WatcherRollbackDurableObservationEntry &
    Readonly<{ authority: WatcherRollbackDurableAuthority }>,
): Promise<WatcherRollbackDurableObservationResult> =>
  await persistWatcherRollbackDurableObservations({
    authority: input.authority,
    entries: [input],
  });

/**
 * Journals several authenticated blocks in one durable revision. Every entry
 * is authenticated and verified exactly as a single observation would be; the
 * retained-store encode, MAC and CAS run once, so catching up a run of quiet
 * blocks costs one commit of the retained store rather than one per block.
 */
export const persistWatcherRollbackDurableObservations = async (input: {
  readonly authority: WatcherRollbackDurableAuthority;
  readonly entries: readonly WatcherRollbackDurableObservationEntry[];
}): Promise<WatcherRollbackDurableObservationResult> => {
  const runtime = runtimeForRollbackDurableAuthority(input.authority);
  if (
    input.entries.length === 0 ||
    !input.entries.every((entry) => authenticatesCanonicalBlock(entry))
  ) {
    throw new Error(
      "watcher durable observation lacks authenticated local-node agreement",
    );
  }
  // A run journals distinct blocks; one block's successive depths would each
  // stay protected here although block-by-block journaling compacts them.
  if (
    new Set(input.entries.map(({ block }) => block.chainPoint.pointDigest))
      .size !== input.entries.length
  ) {
    throw new Error("watcher durable observation run repeats a block");
  }
  const observations = input.entries.flatMap(
    ({ observations: entryObservations }) => entryObservations,
  );
  const existingById = new Map(
    runtime.snapshot.currentStore.l1Observations.map((observation) => [
      observation.observationId,
      observation,
    ]),
  );
  const storedAs = (observation: WatcherNormalizedL1Block) => {
    const existing = existingById.get(observation.observationDigest);
    return existing === undefined
      ? "absent"
      : existing.providerId === observation.provider.providerId &&
          existing.chainPointId === observation.chainPoint.chainPointId &&
          existing.payload.cborHex ===
            encodeWatcherNormalizedL1Block(observation).toString("hex")
        ? "same"
        : "substituted";
  };
  const retainedDigests = new Set(
    runtime.snapshot.consistencyHistory.map(
      ({ consistencyDigest }) => consistencyDigest,
    ),
  );
  if (
    observations.every((observation) => storedAs(observation) === "same") &&
    input.entries.every(({ consistency }) =>
      retainedDigests.has(consistency.consistencyDigest),
    )
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
  if (
    observations.some((observation) => storedAs(observation) === "substituted")
  ) {
    throw new Error("watcher durable observation identity was substituted");
  }
  if (
    runtime.snapshot.rollbackState.incident !== null ||
    currentRollbackFinalityState(runtime).phase === "quarantined"
  ) {
    throw new Error(
      "watcher quarantined observation requires post-finality recovery",
    );
  }
  const { store: nextStore, history: nextHistory } =
    nextAuthenticatedEvidenceWithinRecoveryHorizon({
      source: runtime.snapshot.currentStore,
      history: runtime.snapshot.consistencyHistory,
      observations,
      consistencies: input.entries.map(({ consistency }) => consistency),
      frontier: currentRollbackFinalityState(runtime),
    });
  for (const entry of input.entries)
    assertCanonicalProgressEvidence(
      runtime.policy,
      runtime.snapshot.currentStore,
      nextStore,
      entry,
    );
  freezeRollbackSnapshotJson(nextStore);
  // Persist the new evidence and its store binding in the same revision.
  // Finality and the authenticated prior transition lineage stay unchanged.
  const checkpoint = makeEpochBootstrapState(
    runtime.policy,
    runtime.snapshot.rollbackState,
    nextStore,
    currentRollbackFinalityState(runtime),
    null,
    "observation",
  );
  const committed = await commitRollbackDurableAuthority(
    input.authority,
    nextStore,
    checkpoint,
    checkpoint,
    checkpoint.stateDigest,
    nextHistory,
  );
  return committed === null
    ? Object.freeze({ persistence: "conflict" })
    : Object.freeze({
        persistence: "committed",
        authority: committed.authority,
        trustedHead: committed.trustedHead,
      });
};

/**
 * Atomically journals the exact native chain-authority observation before any
 * finality/rollback decision is acted upon. The observation must be the one
 * admitted by the live chain-sync transport and selected by the independently
 * reconciled local Kupo/Ogmios consistency result.
 */
/**
 * A processed-but-unrecorded stretch of blocks between the finalized
 * authority block and a new canonical block. Quiet blocks are not persisted
 * into the authority; the caller attests them from its block-progress store.
 */
export type WatcherRollbackCanonicalAncestryLink = Readonly<{
  blockHash: string;
  parentBlockHash: string;
  blockNo: string;
  slot: string;
}>;

export const ancestryLinksFinalizedToBlock = (
  finalized: Readonly<{ blockHash: string; blockNo: string; slot: string }>,
  block: WatcherNormalizedL1Block,
  ancestry: readonly WatcherRollbackCanonicalAncestryLink[],
): boolean => {
  let previous = finalized;
  for (const link of ancestry) {
    if (
      link.parentBlockHash !== previous.blockHash ||
      BigInt(link.blockNo) !== BigInt(previous.blockNo) + 1n ||
      BigInt(link.slot) <= BigInt(previous.slot)
    )
      return false;
    previous = link;
  }
  return (
    block.chainPoint.parentBlockHash === previous.blockHash &&
    BigInt(block.chainPoint.blockNo) === BigInt(previous.blockNo) + 1n &&
    BigInt(block.chainPoint.slot) > BigInt(previous.slot)
  );
};

/** Pure ancestry-link test seam; it grants no durable authority. */
export const unsafeWatcherCanonicalAncestryLinksForTest =
  ancestryLinksFinalizedToBlock;
