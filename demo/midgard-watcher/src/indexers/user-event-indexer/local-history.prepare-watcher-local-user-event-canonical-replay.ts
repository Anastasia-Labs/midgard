import {
  readWatcherProtectedUserEventCheckpoint,
  readWatcherProtectedUserEventCheckpointReceipt,
  type WatcherProtectedUserEventCheckpoint,
} from "../../storage/durable-runtime.js";
import {
  makeWatcherUserEventCheckpoint,
  type WatcherUserEventArchive,
} from "../../storage/user-event-checkpoint.js";
import { type WatcherUserEventOriginFacts } from ".././user-event-origin.js";
import { type WatcherLocalUserEventReplaySource } from "./local-history.admit-watcher-local-user-event-authority.js";
import {
  closeWatcherLocalUserEventHistory,
  commitLocalUserEventTransition,
} from "./local-history.assert-watcher-local-user-event-head-current.js";
import {
  createLocalUserEventHistory,
  localAnchorDue,
} from "./local-history.create-local-user-event-history.js";
import { localArchivedEntry } from "./local-history.local-entry-at-block.js";
import {
  localArchiveEvidence,
  localArchiveObject,
  localCoverageOnHead,
  localOwner,
  type LocalPair,
  localReadmissionBrand,
  localReadmissions,
  localRefuse,
  type WatcherLocalUserEventAnchor,
  type WatcherLocalUserEventHistory,
  type WatcherLocalUserEventReadmission,
} from "./local-history.local-history-owner.js";
import {
  localArchiveClosure,
  localStableOrigin,
} from "./local-history.local-stable-snapshot.js";
import {
  commitLocalUserEventAnchor,
  prepareLocalUserEventAnchor,
} from "./local-history.prepare-local-user-event-anchor.js";
import { prepareLocalUserEventTransition } from "./local-history.prepare-local-user-event-transition.js";
import { prepareWatcherLocalUserEventReadmission } from "./local-history.prepare-watcher-local-user-event-readmission.js";
import { localLivePair } from "./local-history.restore-watcher-local-user-event-coverage.js";
import { isHex32, same, sha256Bytes } from "./policy.js";
import { type PlainRecord } from "./types.js";

/** Rebuild a replacement branch from fresh native evidence. Unlike archived
 * readmission, this requires a positive conflicting block at the saved head's
 * height; an unavailable Order, missing block, or transport error proves nothing. */
export const prepareWatcherLocalUserEventCanonicalReplay = async (
  input: Omit<
    Parameters<typeof prepareWatcherLocalUserEventReadmission>[0],
    "replayBlock"
  > &
    Readonly<{
      replayCanonical: (
        savedHead: WatcherUserEventOriginFacts["parentPoint"],
      ) => AsyncIterable<
        Awaited<ReturnType<WatcherLocalUserEventReplaySource>>
      >;
    }>,
): Promise<WatcherLocalUserEventReadmission> => {
  const publication = await readWatcherProtectedUserEventCheckpoint(
    input.runtime,
  );
  const published = readWatcherProtectedUserEventCheckpointReceipt(publication);
  const previous = published.checkpoint;
  if (
    previous === null ||
    published.payload === null ||
    published.validation?.checkpointDigest !== previous.checkpointDigest ||
    published.validation.payloadDigest !== previous.payloadDigest
  )
    return localRefuse(
      "canonical replay requires a validated protected checkpoint",
    );
  const payload = objectForLocalRestart(published.payload);
  const savedHead = localArchivedEntry(payload.head);
  const history = createLocalUserEventHistory({
    ...input,
    publication,
    semanticReplay: true,
  });
  const owner = localOwner(history);
  let retained: Awaited<ReturnType<WatcherLocalUserEventReplaySource>> | null =
    null;
  try {
    if (
      !same(payload.policy, owner.policy) ||
      previous.userEventPolicyDigest !== owner.policy.policyDigest ||
      previous.finalityPolicyDigest !== owner.finalityPolicy.policyDigest ||
      !isHex32(payload.originArchiveDigest)
    )
      return localRefuse("canonical replay deployment or policy differs");
    const oldOriginBytes = await input.archive.read(
      payload.originArchiveDigest,
    );
    if (
      oldOriginBytes === null ||
      sha256Bytes(oldOriginBytes) !== payload.originArchiveDigest
    )
      return localRefuse("canonical replay original provenance is absent");
    const oldOrigin = objectForLocalRestart(oldOriginBytes);
    if (
      !same(
        localStableOrigin(oldOrigin.facts),
        localArchiveEvidence(localStableOrigin(owner.origin)),
      )
    )
      return localRefuse("canonical replay activation differs");
    const firstPair = {
      finality: input.finality,
      observation: input.observation,
      referenceAuthority: input.referenceAuthority,
    };
    commitLocalUserEventTransition(
      history,
      prepareLocalUserEventTransition(history, firstPair),
    );
    let replacement: WatcherUserEventOriginFacts["parentPoint"] | null = null;
    const writeObjects = async () => {
      for (const object of owner.archiveObjects) {
        if (
          (await input.archive.put(Buffer.from(object.bytesHex, "hex"))) !==
          object.digest
        )
          return localRefuse("canonical replay archive write differs");
      }
    };
    for await (const pair of input.replayCanonical(savedHead.cursor)) {
      try {
        const current = localLivePair(owner, pair).witness.current.observation
          .capture.point;
        if (BigInt(current.blockNo) > BigInt(savedHead.cursor.blockNo))
          return localRefuse("canonical replay skipped the saved head height");
        commitLocalUserEventTransition(
          history,
          prepareLocalUserEventTransition(history, pair),
        );
        await retained?.close();
        retained = pair;
        if (localAnchorDue(owner)) {
          await writeObjects();
          const anchor = await prepareLocalUserEventAnchor(
            history,
            pair,
            input.archive,
          );
          commitLocalUserEventAnchor(anchor);
        }
        if (current.blockNo === savedHead.cursor.blockNo) {
          if (current.blockHash === savedHead.cursor.blockHash)
            return localRefuse(
              "canonical replay has no conflicting head block",
            );
          replacement = current;
          break;
        }
      } finally {
        if (retained !== pair) await pair.close();
      }
    }
    if (replacement === null || retained === null)
      return localRefuse(
        "canonical replacement has not reached the saved head height",
      );
    // Historical predecessor evidence remains inspectable, but only this fresh
    // contiguous fold and its live final head authorize the replacement.
    const provenance = localArchiveObject({
      kind: "canonical_branch_replacement",
      previousCheckpoint: previous,
      previousHead: savedHead,
      replacementHead: replacement,
    });
    const archiveObjects = localArchiveClosure([
      ...owner.archiveObjects,
      provenance,
    ]);
    const nextCheckpoint = makeWatcherUserEventCheckpoint({
      ...previous,
      checkpointSequence: (BigInt(previous.checkpointSequence) + 1n).toString(),
      predecessorCheckpointDigest: previous.checkpointDigest,
      rollbackGeneration: (BigInt(previous.rollbackGeneration) + 1n).toString(),
      payloadDigest: owner.checkpoint!.payloadDigest,
      requiredArchiveDigests: archiveObjects.map(({ digest }) => digest).sort(),
    });
    const refreshed = readWatcherProtectedUserEventCheckpointReceipt(
      await readWatcherProtectedUserEventCheckpoint(input.runtime),
    );
    if (!same(refreshed.checkpoint, previous))
      return localRefuse("protected head changed during canonical replay");
    const receipt = Object.freeze({ [localReadmissionBrand]: true as const });
    const headPair = retained;
    localReadmissions.set(receipt, {
      runtime: input.runtime,
      history,
      pair: headPair,
      generation: owner.generation,
      previousCheckpoint: previous,
      nextCheckpoint,
      archiveObjects,
      release: () => headPair.close(),
      accepted: false,
    });
    readWatcherLocalUserEventReadmission(receipt);
    retained = null;
    return receipt;
  } catch (error) {
    closeWatcherLocalUserEventHistory(history);
    throw error;
  } finally {
    await retained?.close();
  }
};

export const readWatcherLocalUserEventReadmission = (
  receipt: WatcherLocalUserEventReadmission,
) => {
  const readmission =
    localReadmissions.get(receipt) ??
    localRefuse("readmission is not privately admitted");
  const owner = localOwner(readmission.history);
  if (
    readmission.accepted ||
    !owner.semanticReplay ||
    owner.generation !== readmission.generation ||
    owner.candidate !== null
  )
    return localRefuse("readmission is no longer pending");
  const live = localLivePair(owner, readmission.pair);
  const finality = readmission.runtime.read().currentFinalityState;
  if (
    finality.phase === "quarantined" ||
    finality.incident !== null ||
    !same(
      live.witness.current.observation.capture.point,
      owner.entries.at(-1)!.cursor,
    ) ||
    live.witness.current.observation.capture.nativeBlock.rawBlockCbor !==
      owner.acceptedEvidence.at(-1)!.rawBlockCbor
  )
    return localRefuse("semantic readmission head is no longer current");
  return Object.freeze({
    archiveObjects: readmission.archiveObjects,
    nextCheckpoint: readmission.nextCheckpoint,
    expectedCheckpointDigest: readmission.previousCheckpoint.checkpointDigest,
    expectedCheckpointSequence:
      readmission.previousCheckpoint.checkpointSequence,
  });
};

export const acceptWatcherLocalUserEventReadmission = (
  receipt: WatcherLocalUserEventReadmission,
  publication: WatcherProtectedUserEventCheckpoint,
): WatcherLocalUserEventHistory => {
  const prepared = readWatcherLocalUserEventReadmission(receipt);
  const observed = readWatcherProtectedUserEventCheckpointReceipt(publication);
  if (
    !same(observed.checkpoint, prepared.nextCheckpoint) ||
    observed.payload === null ||
    sha256Bytes(observed.payload) !== prepared.nextCheckpoint.payloadDigest
  )
    return localRefuse("semantic readmission publication differs");
  const readmission = localReadmissions.get(receipt)!;
  const owner = localOwner(readmission.history);
  owner.checkpoint = readmission.nextCheckpoint;
  owner.archiveObjects = readmission.archiveObjects;
  owner.lastAccepted = null;
  owner.semanticReplay = false;
  owner.coverage = localCoverageOnHead(owner.entries.at(-1)!);
  owner.generation += 1;
  owner.acceptedAtMonotonicMs = performance.now();
  readmission.accepted = true;
  return readmission.history;
};

export const closeWatcherLocalUserEventReadmission = async (
  receipt: WatcherLocalUserEventReadmission,
): Promise<void> => {
  const readmission =
    localReadmissions.get(receipt) ??
    localRefuse("readmission is not privately admitted");
  if (!readmission.accepted)
    closeWatcherLocalUserEventHistory(readmission.history);
  await readmission.release();
};

export const prepareWatcherLocalUserEventAnchor = async (
  input: LocalPair &
    Readonly<{
      history: WatcherLocalUserEventHistory;
      archive: WatcherUserEventArchive;
      publication: WatcherProtectedUserEventCheckpoint;
    }>,
): Promise<WatcherLocalUserEventAnchor> => {
  const {
    history,
    archive,
    publication,
    finality,
    observation,
    referenceAuthority,
  } = input;
  const owner = localOwner(history);
  if (
    owner.semanticReplay ||
    !same(
      readWatcherProtectedUserEventCheckpointReceipt(publication).checkpoint,
      owner.checkpoint,
    )
  )
    return localRefuse("anchor protected predecessor differs");
  return await prepareLocalUserEventAnchor(
    history,
    Object.freeze({ finality, observation, referenceAuthority }),
    archive,
    publication,
  );
};

export const objectForLocalRestart = (bytes: Uint8Array): PlainRecord => {
  const value: unknown = JSON.parse(
    new TextDecoder("utf-8", { fatal: true }).decode(bytes),
  );
  if (typeof value !== "object" || value === null || Array.isArray(value))
    return localRefuse("saved semantic state is not an object");
  return value as PlainRecord;
};
