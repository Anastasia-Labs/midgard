import type {
  WatcherAuthenticatedStateQueueObservation,
  WatcherReleasedHeaderProof,
  WatcherStateQueueHeaderObservation,
} from "../indexers/authenticated-state-queue-observation.js";
import { verificationRecorder } from "./fault-decision-bridge.classification-record.js";
import {
  authenticatedHeaderObservation,
  type BridgeDependencies,
} from "./fault-decision-bridge.selected-target.js";

/**
 * A header that left the L1 queue at the watcher's release depth, merged
 * into confirmed state or removed, before this watcher classified it. No
 * fault proof can target it, and its public DA payload may already be
 * pruned, since retention drops a payload once its header leaves the queue.
 * Classification skips it before any DA read; the operations sink records
 * `unverified_merged` or `unverified_removed`, and the operator is warned,
 * once per header and L1 transaction. No decision is journaled and nothing
 * is deleted.
 */
export const unverifiedMergedRecorder = (
  dependencies: Pick<
    BridgeDependencies,
    "observationDigest" | "operationsSink" | "nowMs" | "monotonicNowMs" | "warn"
  >,
) => {
  const recorded = new Set<string>();
  const record = async (
    observation: WatcherAuthenticatedStateQueueObservation,
    header: WatcherStateQueueHeaderObservation,
    proof: WatcherReleasedHeaderProof,
  ): Promise<void> => {
    const removal = "removalTransactionHash" in proof ? proof : null;
    const transactionHash =
      "removalTransactionHash" in proof
        ? proof.removalTransactionHash
        : proof.mergeTransactionHash;
    const key = `${header.headerHash}:${transactionHash}`;
    if (proof.headerHash !== header.headerHash)
      throw new Error("release proof names another state-queue header");
    if (recorded.has(key)) return;
    const subjectDigest = await dependencies.observationDigest(
      authenticatedHeaderObservation(observation, header),
    );
    const verification = verificationRecorder(dependencies, header.headerHash);
    if (removal === null)
      verification.recordMerged(subjectDigest, transactionHash);
    else
      verification.recordRemoved(
        subjectDigest,
        transactionHash,
        removal.removalKind,
      );
    const lock = observation.finalizedCorrectionLock?.datum ?? "Idle";
    dependencies.warn?.(
      Object.freeze({
        event: removal === null ? "unverified_merged" : "unverified_removed",
        headerHash: header.headerHash,
        transactionHash,
        ...(removal === null ? {} : { removalKind: removal.removalKind }),
        // A merged Locked target leaves no runnable correction target.
        lockedCorrectionTarget:
          lock !== "Idle" &&
          lock.Locked.target_header_hash === header.headerHash,
      }),
    );
    recorded.add(key);
  };
  return Object.freeze({
    /** Records every released header of `observation`, in queue order. */
    recordAll: async (
      observation: WatcherAuthenticatedStateQueueObservation,
      released: ReadonlyMap<string, WatcherReleasedHeaderProof>,
    ): Promise<void> => {
      const queued = new Set(
        observation.finalizedHeaders.map(({ headerHash }) => headerHash),
      );
      if ([...released.keys()].some((headerHash) => !queued.has(headerHash)))
        throw new Error("release proof names a header outside the observation");
      for (const header of observation.finalizedHeaders) {
        const proof = released.get(header.headerHash);
        if (proof !== undefined) await record(observation, header, proof);
      }
    },
    /** A rollback may return a header to the queue; it is recorded afresh. */
    reset: (): void => recorded.clear(),
  });
};
