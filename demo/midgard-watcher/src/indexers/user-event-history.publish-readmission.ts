import type { WatcherLocalBackfillFinalityReceipt } from "../l1/finality-engine.js";
import type { WatcherLocalBackfillObservationReceipt } from "../l1/l1-adapter.js";
import type {
  VerifiedWatcherDeploymentIdentity,
  WatcherUserEventScriptBinding,
} from "../runtime/deployment-identity.js";
import {
  persistWatcherUserEventCheckpoint,
  readWatcherProtectedUserEventCheckpoint,
  readWatcherProtectedUserEventCheckpointReceipt,
  type WatcherDurableRuntime,
} from "../storage/durable-runtime.js";
import {
  type WatcherUserEventArchive,
  watcherUserEventArchiveDigest,
} from "../storage/user-event-checkpoint.js";
import { makeLocalUserEventPublisher } from "./user-event-history.make-local-user-event-publisher.js";
import {
  acceptWatcherLocalUserEventReadmission,
  closeWatcherLocalUserEventReadmission,
  createWatcherLocalUserEventHistory,
  prepareWatcherLocalUserEventCanonicalReplay,
  prepareWatcherLocalUserEventReadmission,
  readWatcherLocalUserEventReadmission,
  restoreWatcherLocalUserEventHistory,
} from "./user-event-indexer.js";
import type { WatcherUserEventOriginReceipt } from "./user-event-origin.js";

/** Dependencies are fixed for the lifetime of this publisher. The archive and
 * runtime must refer to the same deployment; lower publication verifies closure.
 * The returned descriptive state does not confer indexing or dispatch authority.
 */
export const createWatcherLocalUserEventPublisher = async (
  input: Readonly<{
    origin: WatcherUserEventOriginReceipt;
    deploymentIdentity: VerifiedWatcherDeploymentIdentity;
    scriptBinding: WatcherUserEventScriptBinding;
    finality: WatcherLocalBackfillFinalityReceipt;
    observation: WatcherLocalBackfillObservationReceipt;
    runtime: WatcherDurableRuntime;
    archive: WatcherUserEventArchive;
  }>,
) => {
  const {
    runtime,
    archive,
    origin,
    deploymentIdentity,
    scriptBinding,
    finality,
    observation,
  } = input;
  const publication = await readWatcherProtectedUserEventCheckpoint(runtime);
  const history = createWatcherLocalUserEventHistory({
    origin,
    deploymentIdentity,
    scriptBinding,
    finality,
    observation,
    publication,
  });
  return makeLocalUserEventPublisher(history, runtime, archive);
};

/** Restarts over a nonempty checkpoint by replaying authenticated whole blocks
 * from activation. Its new evidence is explicitly published as readmission;
 * original archived provenance remains unchanged and does not become authority.
 */
export const recoverWatcherLocalUserEventPublisher = async (
  input: Parameters<typeof prepareWatcherLocalUserEventReadmission>[0],
): Promise<ReturnType<typeof makeLocalUserEventPublisher>> => {
  const { runtime, archive, ...originAndSource } = input;
  const readmission = await prepareWatcherLocalUserEventReadmission({
    ...originAndSource,
    runtime,
    archive,
  });
  return publishReadmission(readmission, runtime, archive);
};

export const replaceWatcherLocalUserEventPublisher = async (
  input: Parameters<typeof prepareWatcherLocalUserEventCanonicalReplay>[0],
): Promise<ReturnType<typeof makeLocalUserEventPublisher>> =>
  publishReadmission(
    await prepareWatcherLocalUserEventCanonicalReplay(input),
    input.runtime,
    input.archive,
  );

const publishReadmission = async (
  readmission: Awaited<
    ReturnType<typeof prepareWatcherLocalUserEventReadmission>
  >,
  runtime: WatcherDurableRuntime,
  archive: WatcherUserEventArchive,
): Promise<ReturnType<typeof makeLocalUserEventPublisher>> => {
  try {
    const prepared = readWatcherLocalUserEventReadmission(readmission);
    for (const object of prepared.archiveObjects) {
      const bytes = Buffer.from(object.bytesHex, "hex");
      if (
        watcherUserEventArchiveDigest(bytes) !== object.digest ||
        (await archive.put(bytes)) !== object.digest
      )
        throw new Error(
          "Local semantic readmission archive write differs from its digest",
        );
      readWatcherLocalUserEventReadmission(readmission);
    }
    const current = readWatcherProtectedUserEventCheckpointReceipt(
      await readWatcherProtectedUserEventCheckpoint(runtime),
    );
    readWatcherLocalUserEventReadmission(readmission);
    const finality = runtime.readFinality();
    if (
      finality.phase === "quarantined" ||
      finality.incident !== null ||
      current.checkpoint?.checkpointDigest !==
        prepared.expectedCheckpointDigest ||
      current.checkpoint.checkpointSequence !==
        prepared.expectedCheckpointSequence
    )
      throw new Error(
        "Local semantic readmission protected predecessor is no longer current",
      );
    const published = await persistWatcherUserEventCheckpoint(runtime, {
      expectedCheckpointDigest: prepared.expectedCheckpointDigest,
      expectedCheckpointSequence: prepared.expectedCheckpointSequence,
      nextCheckpoint: prepared.nextCheckpoint,
      validationCandidate: readmission,
    });
    const history = acceptWatcherLocalUserEventReadmission(
      readmission,
      published.protectedCheckpoint,
    );
    return makeLocalUserEventPublisher(history, runtime, archive);
  } finally {
    await closeWatcherLocalUserEventReadmission(readmission);
  }
};

/** Ordinary restart restores saved semantic validation and corroborates its
 * exact current head. It does not publish a replacement checkpoint or replay. */
export const resumeWatcherLocalUserEventPublisher = async (
  input: Parameters<typeof restoreWatcherLocalUserEventHistory>[0],
) =>
  makeLocalUserEventPublisher(
    await restoreWatcherLocalUserEventHistory(input),
    input.runtime,
    input.archive,
  );
