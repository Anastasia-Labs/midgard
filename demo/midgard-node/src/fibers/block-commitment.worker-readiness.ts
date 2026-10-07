import { Effect } from "effect";

import type { Globals } from "../services/globals.js";
import {
  clearLivenessIncident,
  raiseLivenessIncident,
} from "../services/liveness-halt.js";
import type { WorkerOutput } from "../workers/utils/commit-block-header.js";

export const COMMIT_WORKER_SOURCE = "commit_worker";
export const COMMIT_WORKER_FAILED = "commit_worker_failed";

/** Reports worker failure without holding the commit fibers: they must retry
 * through their ordinary journal and lease gates. Only actual worker success
 * or an independent empty-work preflight clears this reason. A heartbeat,
 * scheduler deferral or worker no-op does not prove outstanding work recovered. */
export const applyCommitWorkerReadiness = (
  globals: Pick<Globals, "LIVENESS_REASONS">,
  output: WorkerOutput,
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
    case "AwaitingForeignDaOutput":
    case "SubmittedAwaitingLocalFinalizationOutput":
      return Effect.void;
  }
};

export const clearCommitWorkerFailure = (
  globals: Pick<Globals, "LIVENESS_REASONS">,
): Effect.Effect<void> => clearLivenessIncident(globals, COMMIT_WORKER_SOURCE);
