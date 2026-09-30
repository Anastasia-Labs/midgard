import {
  readWatcherProtectedUserEventCheckpoint,
  readWatcherProtectedUserEventCheckpointReceipt,
  type WatcherDurableRuntime,
  type WatcherProtectedUserEventCheckpoint,
} from "../../storage/durable-runtime.js";
import {
  assertWatcherStateQueueHeaderObservation,
  type WatcherStateQueueHeaderObservation,
} from ".././authenticated-state-queue-observation.js";
import {
  localArchiveObject,
  localCoverageOnHead,
  localHistories,
  localOwner,
  type LocalPair,
  type LocalPreparedRead,
  localRefuse,
  localTransitions,
  localUnavailableErrors,
  type WatcherLocalUserEventHistory,
  type WatcherLocalUserEventTransition,
} from "./local-history.local-history-owner.js";
import { prepareLocalUserEventTransition } from "./local-history.prepare-local-user-event-transition.js";
import { localLivePair } from "./local-history.restore-watcher-local-user-event-coverage.js";
import { same, sha256Bytes } from "./policy.js";

export const prepareWatcherLocalUserEventTransition = (
  input: LocalPair &
    Readonly<{
      history: WatcherLocalUserEventHistory;
      publication: WatcherProtectedUserEventCheckpoint;
    }>,
): WatcherLocalUserEventTransition => {
  const {
    history,
    publication: receipt,
    finality,
    observation,
    referenceAuthority,
  } = input;
  const owner = localOwner(history);
  if (owner.semanticReplay)
    return localRefuse("semantic replay has not been published");
  const publication = readWatcherProtectedUserEventCheckpointReceipt(receipt);
  if (!same(publication.checkpoint, owner.checkpoint))
    return localRefuse(
      "protected predecessor differs; semantic reconciliation required",
    );
  const transition = prepareLocalUserEventTransition(
    history,
    Object.freeze({ finality, observation, referenceAuthority }),
  );
  readWatcherProtectedUserEventCheckpointReceipt(receipt);
  return transition;
};

export const readWatcherLocalUserEventTransition = (
  transition: WatcherLocalUserEventTransition,
): LocalPreparedRead => {
  const prepared =
    localTransitions.get(transition) ??
    localRefuse("transition is not privately admitted");
  const owner = localOwner(prepared.history);
  if (prepared.accepted) {
    if (
      owner.lastAccepted !== transition ||
      owner.checkpoint?.checkpointDigest !==
        prepared.value.nextCheckpoint.checkpointDigest
    )
      return localRefuse("accepted transition is no longer the head");
  } else if (
    owner.generation !== prepared.generation ||
    owner.candidate !== transition
  )
    return localRefuse("transition is no longer pending");
  const live = localLivePair(owner, prepared.pair);
  if (
    live.witness.first !== prepared.witness.first ||
    live.witness.current !== prepared.witness.current ||
    live.referenceEvidence !== prepared.referenceEvidence
  )
    return localRefuse("prepared evidence differs");
  return prepared.value;
};

/** Exact protected publication advances the private cursor once, never preparation. */
export const acceptWatcherLocalUserEventPublication = (
  input: Readonly<{
    history: WatcherLocalUserEventHistory;
    transition: WatcherLocalUserEventTransition;
    publication: WatcherProtectedUserEventCheckpoint;
  }>,
) => {
  const { history, transition, publication: publicationReceipt } = input;
  const owner = localOwner(history);
  const prepared =
    localTransitions.get(transition) ??
    localRefuse("transition is not privately admitted");
  const publication =
    readWatcherProtectedUserEventCheckpointReceipt(publicationReceipt);
  if (
    prepared.history !== history ||
    !same(publication.checkpoint, prepared.value.nextCheckpoint) ||
    publication.payload === null ||
    sha256Bytes(publication.payload) !==
      prepared.value.nextCheckpoint.payloadDigest
  )
    return localRefuse(
      "publication does not match the exact prepared frame and payload",
    );
  if (prepared.accepted) {
    if (
      owner.checkpoint?.checkpointDigest !==
      prepared.value.nextCheckpoint.checkpointDigest
    )
      return localRefuse("accepted publication is no longer the head");
    return Object.freeze({
      entryDigest: prepared.value.entry.entryDigest,
      cursor: prepared.value.entry.cursor,
    });
  }
  if (owner.semanticReplay)
    return localRefuse("semantic replay requires readmission publication");
  return commitLocalUserEventTransition(history, transition);
};

export const commitLocalUserEventTransition = (
  history: WatcherLocalUserEventHistory,
  transition: WatcherLocalUserEventTransition,
) => {
  const owner = localOwner(history);
  const prepared =
    localTransitions.get(transition) ??
    localRefuse("transition is not privately admitted");
  if (prepared.history !== history || prepared.accepted)
    return localRefuse("transition is not a pending step of this owner");
  readWatcherLocalUserEventTransition(transition);
  owner.store = prepared.value.nextStore;
  owner.snapshot = prepared.value.snapshot;
  owner.entries = Object.freeze([...owner.entries, prepared.value.entry]);
  owner.coverage = localCoverageOnHead(prepared.value.entry);
  owner.acceptedEvidence = Object.freeze([
    ...owner.acceptedEvidence,
    Object.freeze({
      entry: prepared.value.entry,
      entryArchiveDigest: localArchiveObject({
        entry: prepared.value.entry,
        observation: prepared.value.observation,
      }).digest,
      rawBlockCbor:
        prepared.witness.current.observation.capture.nativeBlock.rawBlockCbor,
      pointDigest:
        prepared.witness.current.observation.native.chainPoint.pointDigest,
      chainPointId:
        prepared.witness.current.observation.native.chainPoint.chainPointId,
    }),
  ]);
  owner.lastAccepted = transition;
  owner.archiveObjects = prepared.value.archiveObjects;
  owner.checkpoint = prepared.value.nextCheckpoint;
  owner.generation += 1;
  owner.acceptedAtMonotonicMs = performance.now();
  owner.candidate = null;
  prepared.accepted = true;
  return Object.freeze({
    entryDigest: prepared.value.entry.entryDigest,
    cursor: prepared.value.entry.cursor,
  });
};

/** Closing an owner revokes every transition and event capability it issued. */
export const closeWatcherLocalUserEventHistory = (
  history: WatcherLocalUserEventHistory,
): void => {
  const owner =
    localHistories.get(history) ??
    localRefuse("history is not privately admitted");
  owner.closed = true;
};

/** Retire every issued capability synchronously before rollback recovery awaits. */
export const suspendWatcherLocalUserEventHistory = (
  history: WatcherLocalUserEventHistory,
): void => {
  const owner = localHistories.get(history) ?? localRefuse("unknown history");
  if (owner.closed) return localRefuse("history is closed");
  owner.generation += 1;
  owner.suspendedAt = performance.now();
};

/** Same-process recovery preserves the accepted fold only when new native W12
 * evidence and a genuine protected read still identify that exact publication. */
export const resumeWatcherLocalUserEventHistory = (
  input: LocalPair &
    Readonly<{
      history: WatcherLocalUserEventHistory;
      publication: WatcherProtectedUserEventCheckpoint;
    }>,
): void => {
  const owner =
    localHistories.get(input.history) ?? localRefuse("unknown history");
  const head = owner.entries.at(-1);
  const accepted = owner.acceptedEvidence.at(-1);
  const protectedHead = readWatcherProtectedUserEventCheckpointReceipt(
    input.publication,
  );
  if (
    owner.closed ||
    owner.suspendedAt === null ||
    owner.semanticReplay ||
    owner.candidate !== null ||
    owner.anchorCandidate !== null ||
    head === undefined ||
    accepted === undefined ||
    owner.checkpoint === null ||
    owner.snapshot.quarantined ||
    !same(protectedHead.checkpoint, owner.checkpoint) ||
    protectedHead.payload === null ||
    sha256Bytes(protectedHead.payload) !== owner.checkpoint.payloadDigest
  )
    return localRefuse("suspended history requires restart reconciliation");
  const { witness } = localLivePair(owner, input);
  if (
    witness.first.observation.capture.startedAtMonotonicMs <
      owner.suspendedAt ||
    !same(witness.current.observation.capture.point, head.cursor) ||
    witness.current.observation.capture.nativeBlock.rawBlockCbor !==
      accepted.rawBlockCbor
  )
    return localRefuse(
      "rollback recovery does not freshly corroborate the accepted head",
    );
  readWatcherProtectedUserEventCheckpointReceipt(input.publication);
  owner.generation += 1;
  owner.acceptedAtMonotonicMs = performance.now();
  owner.suspendedAt = null;
};

export const localAuthorityUnavailable = (reason: string): never => {
  const error = new Error(`Local user-event authority unavailable: ${reason}`);
  localUnavailableErrors.add(error);
  throw error;
};

/** Only an ordinary candidate's unavailable event/header membership is recoverable.
 * Callers must still freshly fence protected-head, native lease and generation. */
export const isWatcherLocalUserEventAuthorityUnavailable = (
  error: unknown,
): error is Error =>
  error instanceof Error && localUnavailableErrors.has(error);

/** Refresh the protected checkpoint without requiring any particular event. */
export const assertWatcherLocalUserEventHeadCurrent = async (
  input: LocalPair &
    Readonly<{
      history: WatcherLocalUserEventHistory;
      runtime: WatcherDurableRuntime;
    }>,
): Promise<void> => {
  const owner = localOwner(input.history);
  const generation = owner.generation;
  const checkpoint = owner.checkpoint;
  const assertCurrent = () => {
    const head = owner.entries.at(-1);
    const accepted = owner.acceptedEvidence.at(-1);
    const finality = input.runtime.read().currentFinalityState;
    if (
      localOwner(input.history) !== owner ||
      owner.generation !== generation ||
      owner.checkpoint !== checkpoint ||
      checkpoint === null ||
      owner.semanticReplay ||
      owner.candidate !== null ||
      owner.anchorCandidate !== null ||
      owner.snapshot.quarantined ||
      owner.acceptedAtMonotonicMs === null ||
      head === undefined ||
      accepted === undefined ||
      finality.phase === "quarantined" ||
      finality.incident !== null
    )
      return localRefuse("user-event protected head is no longer current");
    const { witness } = localLivePair(owner, input);
    if (
      witness.first.observation.capture.startedAtMonotonicMs <
        owner.acceptedAtMonotonicMs ||
      !same(witness.current.observation.capture.point, head.cursor) ||
      witness.current.observation.capture.nativeBlock.rawBlockCbor !==
        accepted.rawBlockCbor
    )
      return localRefuse(
        "user-event head lease is not fresh exact W12 evidence",
      );
  };
  assertCurrent();
  const receipt = await readWatcherProtectedUserEventCheckpoint(input.runtime);
  assertCurrent();
  const protectedHead = readWatcherProtectedUserEventCheckpointReceipt(receipt);
  if (
    checkpoint === null ||
    !same(protectedHead.checkpoint, checkpoint) ||
    protectedHead.payload === null ||
    sha256Bytes(protectedHead.payload) !== checkpoint.payloadDigest
  )
    return localRefuse(
      "user-event protected head differs after candidate refusal",
    );
};

export const localHeaderFields = (
  header: WatcherStateQueueHeaderObservation,
) => {
  assertWatcherStateQueueHeaderObservation(header);
  return Object.freeze({
    headerHash: header.headerHash,
    headerCborHex: header.headerCborHex,
    queueOutRef: header.queueOutRef,
    observedTransactionHash: header.observedTransactionHash,
    observedBlockHash: header.observedBlockHash,
    observedSlot: header.observedSlot,
    observedBlockNo: header.observedBlockNo,
  });
};
