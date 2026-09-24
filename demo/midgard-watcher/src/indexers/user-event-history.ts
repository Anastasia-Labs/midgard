/** Bounded local semantic history with archive-before-CAS publication.
 * A nonempty checkpoint requires semantic restart admission; it cannot bootstrap here.
 */
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
import { watcherSameCanonicalJson } from "../storage/durable-store.js";
import {
  type WatcherUserEventArchive,
  watcherUserEventArchiveDigest,
} from "../storage/user-event-checkpoint.js";
import type { WatcherStateQueueHeaderObservation } from "./authenticated-state-queue-observation.js";
import {
  acceptWatcherLocalUserEventAnchor,
  acceptWatcherLocalUserEventPublication,
  acceptWatcherLocalUserEventReadmission,
  admitWatcherLocalUserEventAuthority,
  advanceWatcherLocalUserEventCoverage,
  assertWatcherLocalUserEventHeadCurrent,
  assertWatcherLocalUserEventPointCovered,
  closeWatcherLocalUserEventHistory,
  closeWatcherLocalUserEventReadmission,
  createWatcherLocalUserEventHistory,
  prepareWatcherLocalUserEventAnchor,
  prepareWatcherLocalUserEventCanonicalReplay,
  prepareWatcherLocalUserEventReadmission,
  prepareWatcherLocalUserEventTransition,
  readWatcherLocalUserEventAnchor,
  readWatcherLocalUserEventCoverage,
  readWatcherLocalUserEventHistory,
  readWatcherLocalUserEventReadmission,
  readWatcherLocalUserEventTransition,
  restoreWatcherLocalUserEventCoverage,
  restoreWatcherLocalUserEventHistory,
  resumeWatcherLocalUserEventHistory,
  rewindWatcherLocalUserEventCoverage,
  suspendWatcherLocalUserEventHistory,
  type WatcherLocalUserEventAnchor,
  type WatcherLocalUserEventAuthority,
  type WatcherLocalUserEventHistory,
  type WatcherLocalUserEventTransition,
  type WatcherUserEventKind,
} from "./user-event-indexer.js";
import type { WatcherUserEventOriginReceipt } from "./user-event-origin.js";
import type { WatcherUserEventReferenceAuthority } from "./user-event-reference-authority.js";

type LocalPublicationInput = Readonly<{
  finality: WatcherLocalBackfillFinalityReceipt;
  observation: WatcherLocalBackfillObservationReceipt;
  referenceAuthority: WatcherUserEventReferenceAuthority;
}>;

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

const makeLocalUserEventPublisher = (
  history: WatcherLocalUserEventHistory,
  runtime: WatcherDurableRuntime,
  archive: WatcherUserEventArchive,
) => {
  let pending: Readonly<{
    transition: WatcherLocalUserEventTransition;
    pair: LocalPublicationInput;
  }> | null = null;
  let pendingAnchor: Readonly<{
    pair: LocalPublicationInput;
    receipt: WatcherLocalUserEventAnchor;
  }> | null = null;
  let inFlight = false;
  let closed = false;
  let suspended = false;
  const assertOpen = () => {
    if (closed) throw new Error("Local user-event publisher is closed");
    if (suspended) throw new Error("Local user-event publisher is suspended");
    const finality = runtime.readFinality();
    if (finality.phase === "quarantined" || finality.incident !== null)
      throw new Error("Local user-event publisher runtime is quarantined");
  };
  const publish = async (request: LocalPublicationInput) => {
    assertOpen();
    if (pendingAnchor !== null)
      throw new Error("Local anchor publication is unresolved");
    const pair = Object.freeze({
      finality: request.finality,
      observation: request.observation,
      referenceAuthority: request.referenceAuthority,
    });
    if (
      pending !== null &&
      (pending.pair.finality !== pair.finality ||
        pending.pair.observation !== pair.observation ||
        pending.pair.referenceAuthority !== pair.referenceAuthority)
    ) {
      throw new Error(
        "Local user-event publication is unresolved; reconcile the original candidate first",
      );
    }
    const fresh = await readWatcherProtectedUserEventCheckpoint(runtime);
    assertOpen();
    if (pending === null)
      pending = Object.freeze({
        pair,
        transition: prepareWatcherLocalUserEventTransition({
          history,
          ...pair,
          publication: fresh,
        }),
      });
    const transition = pending.transition;
    const prepared = readWatcherLocalUserEventTransition(transition);
    const reconcile = (receipt: typeof fresh) => {
      const observed = readWatcherProtectedUserEventCheckpointReceipt(receipt);
      if (
        watcherSameCanonicalJson(observed.checkpoint, prepared.nextCheckpoint)
      ) {
        const accepted = acceptWatcherLocalUserEventPublication({
          history,
          transition,
          publication: receipt,
        });
        pending = null;
        return accepted;
      }
      if (
        (observed.checkpoint?.checkpointDigest ?? null) !==
          prepared.expectedCheckpointDigest ||
        (observed.checkpoint?.checkpointSequence ?? null) !==
          prepared.expectedCheckpointSequence
      ) {
        throw new Error(
          "Local user-event protected head changed; semantic reconciliation required",
        );
      }
      return null;
    };
    const alreadyPublished = reconcile(fresh);
    if (alreadyPublished !== null) return alreadyPublished;
    const protectedDigests = new Set(
      readWatcherProtectedUserEventCheckpointReceipt(fresh).checkpoint
        ?.requiredArchiveDigests ?? [],
    );
    for (const object of prepared.archiveObjects) {
      // The fresh protected read checked these bytes. Keep the full closure in
      // the candidate and revalidate it after writes; only avoid redundant puts.
      if (protectedDigests.has(object.digest)) continue;
      const bytes = Buffer.from(object.bytesHex, "hex");
      if (
        watcherUserEventArchiveDigest(bytes) !== object.digest ||
        (await archive.put(bytes)) !== object.digest
      ) {
        throw new Error(
          "Local user-event archive write differs from its content digest",
        );
      }
      assertOpen();
      readWatcherLocalUserEventTransition(transition);
    }
    // A synchronous receipt does not observe another process's future writes.
    // Refresh after the archive writes, then recheck the private candidate before CAS.
    const refreshed = await readWatcherProtectedUserEventCheckpoint(runtime);
    assertOpen();
    readWatcherLocalUserEventTransition(transition);
    const concurrentlyPublished = reconcile(refreshed);
    if (concurrentlyPublished !== null) return concurrentlyPublished;
    const result = await persistWatcherUserEventCheckpoint(runtime, {
      expectedCheckpointDigest: prepared.expectedCheckpointDigest,
      expectedCheckpointSequence: prepared.expectedCheckpointSequence,
      nextCheckpoint: prepared.nextCheckpoint,
      validationCandidate: transition,
    });
    assertOpen();
    const accepted = acceptWatcherLocalUserEventPublication({
      history,
      transition,
      publication: result.protectedCheckpoint,
    });
    pending = null;
    return accepted;
  };
  const rotate = async (request: LocalPublicationInput) => {
    assertOpen();
    if (pending !== null)
      throw new Error("Local user-event publication is unresolved");
    const pair = Object.freeze({
      finality: request.finality,
      observation: request.observation,
      referenceAuthority: request.referenceAuthority,
    });
    if (
      pendingAnchor !== null &&
      (pendingAnchor.pair.finality !== pair.finality ||
        pendingAnchor.pair.observation !== pair.observation ||
        pendingAnchor.pair.referenceAuthority !== pair.referenceAuthority)
    )
      throw new Error(
        "Local anchor publication is unresolved; reconcile the original pair",
      );
    const current = await readWatcherProtectedUserEventCheckpoint(runtime);
    assertOpen();
    if (pendingAnchor === null)
      pendingAnchor = Object.freeze({
        pair,
        receipt: await prepareWatcherLocalUserEventAnchor({
          history,
          archive,
          publication: current,
          ...pair,
        }),
      });
    assertOpen();
    const receipt = pendingAnchor.receipt;
    const prepared = readWatcherLocalUserEventAnchor(receipt);
    const reconcile = (publication: typeof current): boolean => {
      const observed =
        readWatcherProtectedUserEventCheckpointReceipt(publication);
      if (
        watcherSameCanonicalJson(observed.checkpoint, prepared.nextCheckpoint)
      ) {
        acceptWatcherLocalUserEventAnchor(receipt, publication);
        pendingAnchor = null;
        return true;
      }
      if (
        observed.checkpoint?.checkpointDigest !==
          prepared.expectedCheckpointDigest ||
        observed.checkpoint.checkpointSequence !==
          prepared.expectedCheckpointSequence
      )
        throw new Error("Local anchor protected predecessor changed");
      return false;
    };
    if (reconcile(current)) return readWatcherLocalUserEventHistory(history);
    for (const object of prepared.archiveObjects) {
      const bytes = Buffer.from(object.bytesHex, "hex");
      if (
        watcherUserEventArchiveDigest(bytes) !== object.digest ||
        (await archive.put(bytes)) !== object.digest
      )
        throw new Error("Local anchor archive write differs from its digest");
      assertOpen();
      readWatcherLocalUserEventAnchor(receipt);
    }
    const refreshed = await readWatcherProtectedUserEventCheckpoint(runtime);
    assertOpen();
    readWatcherLocalUserEventAnchor(receipt);
    if (reconcile(refreshed)) return readWatcherLocalUserEventHistory(history);
    const published = await persistWatcherUserEventCheckpoint(runtime, {
      expectedCheckpointDigest: prepared.expectedCheckpointDigest,
      expectedCheckpointSequence: prepared.expectedCheckpointSequence,
      nextCheckpoint: prepared.nextCheckpoint,
      validationCandidate: receipt,
    });
    assertOpen();
    acceptWatcherLocalUserEventAnchor(receipt, published.protectedCheckpoint);
    pendingAnchor = null;
    return readWatcherLocalUserEventHistory(history);
  };
  return Object.freeze({
    read: () => {
      assertOpen();
      return readWatcherLocalUserEventHistory(history);
    },
    publish: async (request: LocalPublicationInput) => {
      assertOpen();
      if (inFlight)
        throw new Error("Local user-event publication is already in flight");
      inFlight = true;
      try {
        const pair = Object.freeze({
          finality: request.finality,
          observation: request.observation,
          referenceAuthority: request.referenceAuthority,
        });
        return await publish(pair);
      } finally {
        inFlight = false;
      }
    },
    rotate: async (request: LocalPublicationInput) => {
      assertOpen();
      if (inFlight)
        throw new Error("Local user-event publication is already in flight");
      inFlight = true;
      try {
        return await rotate(request);
      } finally {
        inFlight = false;
      }
    },
    eventAuthority: async (
      request: LocalPublicationInput &
        Readonly<{
          eventId: string;
          kind: WatcherUserEventKind;
          throughHeader?: WatcherStateQueueHeaderObservation;
        }>,
    ): Promise<WatcherLocalUserEventAuthority> => {
      assertOpen();
      if (inFlight)
        throw new Error("Local user-event publication is already in flight");
      return await admitWatcherLocalUserEventAuthority({
        history,
        runtime,
        archive,
        ...(request.throughHeader === undefined
          ? {}
          : { throughHeader: request.throughHeader }),
        eventId: request.eventId,
        kind: request.kind,
        finality: request.finality,
        observation: request.observation,
        referenceAuthority: request.referenceAuthority,
      });
    },
    assertHeadCurrent: async (pair: LocalPublicationInput) => {
      assertOpen();
      if (inFlight)
        throw new Error("Local user-event publication is already in flight");
      await assertWatcherLocalUserEventHeadCurrent({
        ...pair,
        history,
        runtime,
      });
      assertOpen();
    },
    assertPointCovered: async (
      point: Parameters<
        typeof assertWatcherLocalUserEventPointCovered
      >[0]["point"],
    ) => {
      assertOpen();
      const coverage = await assertWatcherLocalUserEventPointCovered({
        history,
        runtime,
        archive,
        point,
      });
      assertOpen();
      return coverage;
    },
    /** The moving coverage checkpoint over the quiet stretch above the head. */
    readCoverage: () => readWatcherLocalUserEventCoverage(history),
    /** Admits one quiet native block above the covered head; no request. */
    advanceCoverage: (
      header: Parameters<
        typeof advanceWatcherLocalUserEventCoverage
      >[0]["header"],
    ) => {
      assertOpen();
      if (inFlight || pending !== null || pendingAnchor !== null)
        throw new Error("Local user-event publication is already in flight");
      return advanceWatcherLocalUserEventCoverage({ history, header });
    },
    /** Moves coverage back to a point at or above the head after a native
     * rollback whose fork lies inside the quiet stretch. */
    rewindCoverage: (
      point: Parameters<typeof rewindWatcherLocalUserEventCoverage>[0]["point"],
    ) => {
      assertOpen();
      if (inFlight || pending !== null || pendingAnchor !== null)
        throw new Error("Local user-event publication is already in flight");
      return rewindWatcherLocalUserEventCoverage({ history, point });
    },
    /** Restores the saved coverage record over the restored head. */
    restoreCoverage: (
      saved: Parameters<
        typeof restoreWatcherLocalUserEventCoverage
      >[0]["saved"],
    ) => {
      assertOpen();
      if (inFlight || pending !== null || pendingAnchor !== null)
        throw new Error("Local user-event publication is already in flight");
      return restoreWatcherLocalUserEventCoverage({ history, saved });
    },
    suspend: () => {
      if (closed) throw new Error("Local user-event publisher is closed");
      suspended = true;
      suspendWatcherLocalUserEventHistory(history);
    },
    resume: async (pair: LocalPublicationInput) => {
      if (
        closed ||
        !suspended ||
        inFlight ||
        pending !== null ||
        pendingAnchor !== null
      )
        throw new Error(
          "Local user-event rollback requires restart reconciliation",
        );
      const publication =
        await readWatcherProtectedUserEventCheckpoint(runtime);
      const finality = runtime.readFinality();
      if (
        closed ||
        !suspended ||
        inFlight ||
        finality.phase === "quarantined" ||
        finality.incident !== null
      )
        throw new Error(
          "Local user-event rollback recovery is no longer current",
        );
      resumeWatcherLocalUserEventHistory({ history, publication, ...pair });
      suspended = false;
    },
    close: () => {
      closed = true;
      closeWatcherLocalUserEventHistory(history);
    },
  });
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
