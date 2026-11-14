import { type WatcherFaultProofSupervisorStatus } from "../fault-proofs/fault-proof-supervisor.js";

/**
 * Whether the decision driver may retire replay transcripts at a
 * release-final observation: the supervisor recovered, accepts work, and
 * holds no unfinished objective and no queued, active or blocked job. Any
 * of those may still need a transcript, so the driver resets the
 * retirement witnesses instead.
 */
export const watcherRetirementReady = (
  status: Pick<
    WatcherFaultProofSupervisorStatus,
    | "recovered"
    | "phase"
    | "unfinishedObjectiveCount"
    | "queuedJobCount"
    | "activeJob"
    | "blockedJob"
  >,
): boolean =>
  status.recovered &&
  status.phase === "accepting" &&
  status.unfinishedObjectiveCount === 0 &&
  status.queuedJobCount === 0 &&
  status.activeJob === null &&
  status.blockedJob === null;
