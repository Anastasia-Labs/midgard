import { Effect } from "effect";

import type { Globals } from "../services/globals.js";
import {
  clearLivenessIncident,
  raiseLivenessIncident,
} from "../services/liveness-halt.js";
import type { WorkerOutput } from "../workers/utils/commit-block-header.js";

export const COMMIT_WORKER_SOURCE = "commit_worker";
export const COMMIT_WORKER_FAILED = "commit_worker_failed";
/** The liveness source `commit_base_pending` is raised under. */
export const COMMIT_BASE_SOURCE = "commit_base";
/**
 * The commit waits for its base: a foreign tail landed-block processing has
 * not applied to the working ledger yet. It clears on the worker's next
 * output that is not this wait.
 */
export const COMMIT_BASE_PENDING = "commit_base_pending";

/** Reports worker failure without holding the commit fibers: they must retry
 * through their ordinary journal and lease gates. Only actual worker success
 * or an independent empty-work preflight clears this reason. A heartbeat,
 * scheduler deferral or worker no-op does not prove outstanding work recovered.
 *
 * A commit waiting for its base raises `commit_base_pending` under
 * `commit_base`; any other output clears it. */
export const applyCommitWorkerReadiness = (
  globals: Pick<Globals, "LIVENESS_REASONS">,
  output: WorkerOutput,
): Effect.Effect<void> =>
  output.type === "AwaitingCommitBaseOutput"
    ? raiseLivenessIncident(
        globals,
        COMMIT_BASE_SOURCE,
        COMMIT_BASE_PENDING,
        `commit base ${output.baseHeaderHash}: ${output.detail}`,
      )
    : Effect.zipRight(
        clearLivenessIncident(globals, COMMIT_BASE_SOURCE),
        workerOutcomeReadiness(globals, output),
      );

const workerOutcomeReadiness = (
  globals: Pick<Globals, "LIVENESS_REASONS">,
  output: Exclude<WorkerOutput, { type: "AwaitingCommitBaseOutput" }>,
): Effect.Effect<void> => {
  switch (output.type) {
    case "FailureOutput":
      return raiseLivenessIncident(
        globals,
        COMMIT_WORKER_SOURCE,
        COMMIT_WORKER_FAILED,
        output.error,
      );
    case "SuccessfulSubmissionOutput":
    case "SubmittedAwaitingConfirmationOutput":
    case "SuccessfulLocalFinalizationRecoveryOutput":
      return clearCommitWorkerFailure(globals);
    case "NothingToCommitOutput":
    case "SkippedSubmissionOutput":
    case "RegisteredDueWorkOutput":
    case "SubmittedAwaitingLocalFinalizationOutput":
      return Effect.void;
  }
};

export const clearCommitWorkerFailure = (
  globals: Pick<Globals, "LIVENESS_REASONS">,
): Effect.Effect<void> =>
  Effect.all(
    [
      clearLivenessIncident(globals, COMMIT_WORKER_SOURCE),
      clearLivenessIncident(globals, COMMIT_BASE_SOURCE),
    ],
    { discard: true },
  );
