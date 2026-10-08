import { Effect } from "effect";

import type { Globals } from "../services/globals.js";
import {
  clearLivenessIncident,
  raiseLivenessIncident,
} from "../services/liveness-halt.js";
import {
  COMMIT_STAGE_BATCH_UNDECIDED,
  type WorkerOutput,
} from "../workers/utils/commit-block-header.js";

export { COMMIT_STAGE_BATCH_UNDECIDED };
export const COMMIT_WORKER_SOURCE = "commit_worker";
export const COMMIT_WORKER_FAILED = "commit_worker_failed";
/** The liveness source `commit_stage_batch_undecided` is raised under. */
export const COMMIT_STAGE_BATCH_SOURCE = "commit_stage_batch";

/** Reports worker failure without holding the commit fibers: they must retry
 * through their ordinary journal and lease gates. Only actual worker success
 * or an independent empty-work preflight clears this reason. A heartbeat,
 * scheduler deferral or worker no-op does not prove outstanding work recovered.
 *
 * A failure tagged `commit_stage_batch_undecided` (the commit stage's
 * rejection closure met an acceptance receipt with an undecided member) is
 * raised under `commit_stage_batch` instead of `commit_worker_failed`. The
 * worker retries on its ordinary cadence; the member decides once its block
 * lands or is reopened, and the reason clears by the same rule. */
export const applyCommitWorkerReadiness = (
  globals: Pick<Globals, "LIVENESS_REASONS">,
  output: WorkerOutput,
): Effect.Effect<void> => {
  switch (output.type) {
    case "FailureOutput":
      return output.reason === COMMIT_STAGE_BATCH_UNDECIDED
        ? Effect.zipRight(
            clearLivenessIncident(globals, COMMIT_WORKER_SOURCE),
            raiseLivenessIncident(
              globals,
              COMMIT_STAGE_BATCH_SOURCE,
              COMMIT_STAGE_BATCH_UNDECIDED,
              output.error,
            ),
          )
        : raiseLivenessIncident(
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
    case "AwaitingForeignDaOutput":
    case "SubmittedAwaitingLocalFinalizationOutput":
      return Effect.void;
  }
};

export const clearCommitWorkerFailure = (
  globals: Pick<Globals, "LIVENESS_REASONS">,
): Effect.Effect<void> =>
  Effect.zipRight(
    clearLivenessIncident(globals, COMMIT_WORKER_SOURCE),
    clearLivenessIncident(globals, COMMIT_STAGE_BATCH_SOURCE),
  );
