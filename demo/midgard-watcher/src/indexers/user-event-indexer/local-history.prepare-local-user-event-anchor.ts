import {
  readWatcherProtectedUserEventCheckpointReceipt,
  type WatcherProtectedUserEventCheckpoint,
} from "../../storage/durable-runtime.js";
import {
  makeWatcherUserEventCheckpoint,
  type WatcherUserEventArchive,
} from "../../storage/user-event-checkpoint.js";
import { makeWatcherUserEventArchiveIndex } from ".././user-event-history-archive.js";
import {
  localAnchorBrand,
  localArchiveObject,
  localOwner,
  type LocalPair,
  localRefuse,
  localRetainedEvidence,
  type WatcherLocalUserEventAnchor,
  type WatcherLocalUserEventHistory,
} from "./local-history.local-history-owner.js";
import {
  localAnchors,
  localArchiveClosure,
  localMaterializedStore,
} from "./local-history.local-stable-snapshot.js";
import { localLivePair } from "./local-history.restore-watcher-local-user-event-coverage.js";
import { same } from "./policy.js";
import { topologyMatches } from "./snapshot.js";

export const prepareLocalUserEventAnchor = async (
  history: WatcherLocalUserEventHistory,
  pair: LocalPair,
  archive: WatcherUserEventArchive,
  publication?: WatcherProtectedUserEventCheckpoint,
): Promise<WatcherLocalUserEventAnchor> => {
  const owner = localOwner(history);
  const generation = owner.generation;
  const sourceStore = owner.store;
  const sourceCheckpoint = owner.checkpoint;
  const sourceObjects = owner.archiveObjects;
  const priorIndex = owner.archiveIndex;
  const snapshot = owner.snapshot;
  if (
    owner.candidate !== null ||
    owner.anchorCandidate !== null ||
    sourceCheckpoint === null ||
    owner.entries.length <= 64
  )
    return localRefuse(
      "anchor requires a published head with more than 64 retained blocks and no pending operation",
    );
  const current = localLivePair(owner, pair);
  const head = owner.entries.at(-1)!;
  if (
    !same(current.witness.current.observation.capture.point, head.cursor) ||
    current.witness.current.observation.capture.nativeBlock.rawBlockCbor !==
      owner.acceptedEvidence.at(-1)!.rawBlockCbor
  )
    return localRefuse("anchor capture differs from the exact semantic head");
  const retainedEntries = Object.freeze(owner.entries.slice(-64));
  const retainedEvidence = Object.freeze(owner.acceptedEvidence.slice(-64));
  const retainedIds = new Set(
    retainedEntries.map(({ entryDigest }) => entryDigest),
  );
  const eventPoints = new Set(
    [...snapshot.activeEvents, ...snapshot.terminalEvents].flatMap((event) =>
      "terminalPointDigest" in event
        ? [event.originPointDigest, event.terminalPointDigest]
        : [event.originPointDigest],
    ),
  );
  const allEvidence = localRetainedEvidence(owner);
  if (
    [...eventPoints].some(
      (point) => !allEvidence.some(({ pointDigest }) => pointDigest === point),
    )
  )
    return localRefuse("anchor event provenance is absent");
  const pinnedEvidence = Object.freeze(
    allEvidence.filter(
      ({ entry, pointDigest }) =>
        !retainedIds.has(entry.entryDigest) && eventPoints.has(pointDigest),
    ),
  );
  const nextStore = localMaterializedStore(sourceStore, [
    ...pinnedEvidence,
    ...retainedEvidence,
  ]);
  if (!topologyMatches(nextStore, snapshot))
    return localRefuse("anchor changes event topology");
  const storeArchive = localArchiveObject(nextStore);
  const index = await makeWatcherUserEventArchiveIndex(archive, {
    previous: priorIndex,
    firstEntrySequence:
      priorIndex === null
        ? "0"
        : (BigInt(priorIndex.index.lastEntrySequence) + 1n).toString(),
    lastEntrySequence: head.sequence,
    sourcePayloadDigest: sourceCheckpoint.payloadDigest,
    sourceArchiveDigests: sourceCheckpoint.requiredArchiveDigests,
    materializedStoreDigest: storeArchive.digest,
    retainedEntryDigests: retainedEntries.map(({ entryDigest }) => entryDigest),
  });
  const indexArchive = localArchiveObject(index);
  const archiveIndex = Object.freeze({ digest: indexArchive.digest, index });
  const payload = localArchiveObject({
    schemaVersion: "midgard-watcher-local-user-event-checkpoint-payload-v1",
    originArchiveDigest: owner.originArchive.digest,
    originDigest: owner.originDigest,
    policy: owner.policy,
    anchor: {
      kind: "materialized_history",
      indexDigest: archiveIndex.digest,
      indexSequence: index.indexSequence,
      retainedSuffixEntries: "64",
    },
    head,
    storeArchiveDigest: storeArchive.digest,
    snapshot,
    retainedEntries,
    requiredSemanticResume:
      "authenticated_origin_replay_or_semantic_publication_receipt",
  });
  const requiredEvidence = new Set(
    [...pinnedEvidence, ...retainedEvidence].flatMap((record) => [
      record.entry.evidenceDigest,
      record.entryArchiveDigest,
    ]),
  );
  const evidenceObjects = sourceObjects.filter(({ digest }) =>
    requiredEvidence.has(digest),
  );
  if (evidenceObjects.length !== requiredEvidence.size)
    return localRefuse("anchor retained archive dependency is absent");
  const archiveObjects = localArchiveClosure([
    owner.originArchive,
    ...evidenceObjects,
    storeArchive,
    indexArchive,
    payload,
  ]);
  const nextCheckpoint = makeWatcherUserEventCheckpoint({
    ...sourceCheckpoint,
    checkpointSequence: (
      BigInt(sourceCheckpoint.checkpointSequence) + 1n
    ).toString(),
    predecessorCheckpointDigest: sourceCheckpoint.checkpointDigest,
    payloadDigest: payload.digest,
    requiredArchiveDigests: archiveObjects.map(({ digest }) => digest).sort(),
  });
  if (
    owner.generation !== generation ||
    owner.store !== sourceStore ||
    owner.checkpoint !== sourceCheckpoint ||
    owner.archiveObjects !== sourceObjects ||
    owner.candidate !== null ||
    owner.anchorCandidate !== null
  )
    return localRefuse("history changed during anchor preparation");
  if (publication !== undefined)
    readWatcherProtectedUserEventCheckpointReceipt(publication);
  localLivePair(owner, pair);
  const receipt = Object.freeze({ [localAnchorBrand]: true as const });
  const value = Object.freeze({
    sourceStore,
    nextStore,
    archiveIndex,
    archiveObjects,
    retainedEntries,
    retainedEvidence,
    pinnedEvidence,
    nextCheckpoint,
    expectedCheckpointDigest: sourceCheckpoint.checkpointDigest,
    expectedCheckpointSequence: sourceCheckpoint.checkpointSequence,
  });
  localAnchors.set(receipt, {
    history,
    pair,
    generation,
    value,
    accepted: false,
  });
  owner.anchorCandidate = receipt;
  return receipt;
};

export const readWatcherLocalUserEventAnchor = (
  receipt: WatcherLocalUserEventAnchor,
) => {
  const anchor =
    localAnchors.get(receipt) ??
    localRefuse("anchor is not privately admitted");
  const owner = localOwner(anchor.history);
  if (
    anchor.accepted ||
    owner.anchorCandidate !== receipt ||
    owner.generation !== anchor.generation ||
    owner.store !== anchor.value.sourceStore ||
    owner.candidate !== null
  )
    return localRefuse("anchor is no longer pending");
  const current = localLivePair(owner, anchor.pair);
  if (
    !same(
      current.witness.current.observation.capture.point,
      owner.entries.at(-1)!.cursor,
    ) ||
    current.witness.current.observation.capture.nativeBlock.rawBlockCbor !==
      owner.acceptedEvidence.at(-1)!.rawBlockCbor
  )
    return localRefuse("anchor head is no longer current");
  return Object.freeze({
    archiveObjects: anchor.value.archiveObjects,
    nextCheckpoint: anchor.value.nextCheckpoint,
    expectedCheckpointDigest: anchor.value.expectedCheckpointDigest,
    expectedCheckpointSequence: anchor.value.expectedCheckpointSequence,
  });
};

export const commitLocalUserEventAnchor = (
  receipt: WatcherLocalUserEventAnchor,
): WatcherLocalUserEventHistory => {
  readWatcherLocalUserEventAnchor(receipt);
  const anchor = localAnchors.get(receipt)!;
  const owner = localOwner(anchor.history);
  owner.store = anchor.value.nextStore;
  owner.entries = anchor.value.retainedEntries;
  owner.acceptedEvidence = anchor.value.retainedEvidence;
  owner.pinnedEvidence = anchor.value.pinnedEvidence;
  owner.archiveIndex = anchor.value.archiveIndex;
  owner.archiveObjects = anchor.value.archiveObjects;
  owner.checkpoint = anchor.value.nextCheckpoint;
  owner.anchorCandidate = null;
  owner.lastAccepted = null;
  owner.generation += 1;
  owner.acceptedAtMonotonicMs = performance.now();
  anchor.accepted = true;
  return anchor.history;
};
