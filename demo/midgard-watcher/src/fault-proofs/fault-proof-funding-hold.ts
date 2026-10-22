import { isWorkflowFundingPauseReason } from "@al-ft/midgard-fault-proofs";

import { WatcherProverFundingUnavailableError } from "../funding/prover-funding-reservation.js";
import type { WatcherProofObjective } from "./fault-proof-objective-journal.js";

/**
 * An objective paused because the prover wallet cannot fund its next
 * transaction. It resumes on the next observation; the hold names the pause
 * on readiness until a later run meets something else, as one does once the
 * wallet is topped up.
 */
export const WATCHER_PROVER_FUNDING_UNAVAILABLE = "prover_funding_unavailable";

/** The funding reason of a pending run's reason or of a run's failure. */
const fundingReason = (cause: unknown): string | undefined => {
  if (cause instanceof WatcherProverFundingUnavailableError)
    return cause.message;
  if (typeof cause === "string" && isWorkflowFundingPauseReason(cause))
    return cause;
  return undefined;
};

/**
 * Holds the objective under `prover_funding_unavailable` when `cause` (a
 * pending run's reason, or the error a run failed with) says the prover
 * wallet cannot fund it.
 */
export const holdWatcherFaultProofOnFunding = (
  holds: Map<string, Readonly<{ reason: string; detail: string }>>,
  job: WatcherProofObjective,
  cause: unknown,
): Readonly<{ kind: "pending"; reason: string }> | undefined => {
  const reason = fundingReason(cause);
  if (reason === undefined) return undefined;
  const objective = `${job.category}/${job.headerHash}`;
  holds.set(objective, {
    reason: WATCHER_PROVER_FUNDING_UNAVAILABLE,
    detail: `${objective}: ${reason}`,
  });
  return { kind: "pending", reason };
};
