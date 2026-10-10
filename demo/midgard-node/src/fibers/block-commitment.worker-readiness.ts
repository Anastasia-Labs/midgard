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
/** The liveness source `commit_window_pending` is raised under. */
export const COMMIT_WINDOW_SOURCE = "commit_window";
/**
 * The commit waits for the next scheduler window: an abandoned journal whose
 * commit was signed holds its header hash. It clears on the worker's next
 * output that is not this wait.
 */
export const COMMIT_WINDOW_PENDING = "commit_window_pending";

/** Reports worker failure without holding the commit fibers: they must retry
 * through their ordinary journal and lease gates. Only actual worker success
 * or an independent empty-work preflight clears this reason. A heartbeat,
 * scheduler deferral or worker no-op does not prove outstanding work recovered.
 *
 * A commit waiting for its base raises `commit_base_pending` under
 * `commit_base`, and one waiting for the next window raises
 * `commit_window_pending` under `commit_window`; any other output clears
 * each. */
export const applyCommitWorkerReadiness = (
  globals: Pick<Globals, "LIVENESS_REASONS">,
  output: WorkerOutput,
): Effect.Effect<void> => {
  if (output.type === "AwaitingCommitBaseOutput")
    return Effect.zipRight(
      clearLivenessIncident(globals, COMMIT_WINDOW_SOURCE),
      raiseLivenessIncident(
        globals,
        COMMIT_BASE_SOURCE,
        COMMIT_BASE_PENDING,
        `commit base ${output.baseHeaderHash}: ${output.detail}`,
      ),
    );
  if (output.type === "AwaitingNextCommitWindowOutput")
    return Effect.zipRight(
      clearLivenessIncident(globals, COMMIT_BASE_SOURCE),
      raiseLivenessIncident(
        globals,
        COMMIT_WINDOW_SOURCE,
        COMMIT_WINDOW_PENDING,
        `header ${output.heldHeaderHash}: ${output.detail}`,
      ),
    );
  return Effect.all(
    [
      clearLivenessIncident(globals, COMMIT_BASE_SOURCE),
      clearLivenessIncident(globals, COMMIT_WINDOW_SOURCE),
      workerOutcomeReadiness(globals, output),
    ],
    { discard: true },
  );
};

const workerOutcomeReadiness = (
  globals: Pick<Globals, "LIVENESS_REASONS">,
  output: Exclude<
    WorkerOutput,
    { type: "AwaitingCommitBaseOutput" | "AwaitingNextCommitWindowOutput" }
  >,
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
      clearLivenessIncident(globals, COMMIT_WINDOW_SOURCE),
    ],
    { discard: true },
  );
