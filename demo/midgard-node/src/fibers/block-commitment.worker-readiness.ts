import { Effect, Ref } from "effect";

import type { Globals } from "../services/globals.js";
import { LOCAL_FAULT_RUN_LIMIT } from "../services/l1-follower.coalesced-runner.js";
import {
  clearLivenessIncident,
  raiseLivenessIncident,
} from "../services/liveness-halt.js";
import { signalTransientExhausted } from "../services/transient-exhaustion.js";
import type { WorkerOutput } from "../workers/utils/commit-block-header.js";

export const COMMIT_WORKER_SOURCE = "commit_worker";
export const COMMIT_WORKER_FAILED = "commit_worker_failed";
/** How long `commit_worker_failed` stays raised before it is escalated: ten
 * minutes (thirty nominal L1 blocks). The worker retries on every commit
 * tick, so a failure that outlasts this is a stalled commit pipeline, not a
 * passing one. */
export const COMMIT_WORKER_FAILED_ESCALATION_MS = 10 * 60_000;
/**
 * The reason the node exits under (source `commit_worker`) once
 * `LOCAL_FAULT_RUN_LIMIT` commit-worker attempts since the last successful
 * commit failed on this process's Ed25519 verifier fault.
 */
export const COMMIT_WORKER_VERIFIER_UNAVAILABLE =
  "commit_worker_verifier_unavailable";
/**
 * The reason the node exits under (source `commit_worker`) once
 * `LOCAL_FAULT_RUN_LIMIT` commit-worker attempts since the last successful
 * commit failed on a local fault, the last of them on a due withdrawal row
 * this node cannot classify from its bytes. The detail names the row.
 */
export const COMMIT_WORKER_WITHDRAWAL_UNCLASSIFIABLE =
  "commit_worker_withdrawal_unclassifiable";
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
 * through their ordinary journal and lease gates. A failure is escalated
 * after `COMMIT_WORKER_FAILED_ESCALATION_MS`. Only actual worker success
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
        { escalateAfterMs: COMMIT_WORKER_FAILED_ESCALATION_MS },
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

/** The outputs that prove a commit went through: the same set that clears
 * `commit_worker_failed`. */
const COMMIT_SUCCEEDED: ReadonlySet<WorkerOutput["type"]> = new Set([
  "SuccessfulSubmissionOutput",
  "SubmittedAwaitingConfirmationOutput",
  "SuccessfulLocalFinalizationRecoveryOutput",
]);

/**
 * Bounds commit-worker attempts that fail on a local fault
 * (`FailureOutput.localFault`): this process's Ed25519 verifier fault, or a
 * due withdrawal row it cannot classify from its bytes, named in
 * `localFaultSubject`. This is how the coalesced runner bounds a local fault
 * on a landed block: each such failure counts, and once `limit` of them
 * accrue without a successful commit (`COMMIT_SUCCEEDED`) between them the
 * node exits non-zero (`transient-exhaustion.ts`) for its supervisor to
 * restart it, under the reason of the fault that reached the bound. A
 * successful commit resets the count, and a failure on a different row than
 * the last one counted starts it afresh at one, since that row's failure
 * shows the earlier row was classified. A wait (for the commit base or
 * window), a no-op, a skip, or a failure that is not a local fault leaves it
 * as it is, so a persistent fault interleaved with them still reaches the
 * bound. The count is this process's, so a restarted node meeting the same
 * fault retries within the same bound and exits again. Failures that are not
 * local faults are never counted and stay `commit_worker_failed` as before.
 */
export const boundCommitWorkerLocalFaults = (
  globals: Pick<
    Globals,
    "COMMIT_WORKER_LOCAL_FAULT_RUNS" | "TRANSIENT_EXHAUSTION"
  >,
  output: WorkerOutput,
  limit: number = LOCAL_FAULT_RUN_LIMIT,
): Effect.Effect<void> => {
  const fault =
    output.type === "FailureOutput" && output.localFault !== undefined
      ? output
      : undefined;
  const row = fault?.localFaultSubject;
  return Ref.updateAndGet(globals.COMMIT_WORKER_LOCAL_FAULT_RUNS, (count) =>
    fault === undefined
      ? COMMIT_SUCCEEDED.has(output.type)
        ? { runs: 0, row: undefined }
        : count
      : row !== undefined && count.row !== undefined && count.row !== row
        ? { runs: 1, row }
        : { runs: count.runs + 1, row: row ?? count.row },
  ).pipe(
    Effect.flatMap((count) =>
      fault !== undefined && count.runs >= limit
        ? Effect.sync(() =>
            signalTransientExhausted(
              globals.TRANSIENT_EXHAUSTION,
              row === undefined
                ? {
                    source: COMMIT_WORKER_SOURCE,
                    reason: COMMIT_WORKER_VERIFIER_UNAVAILABLE,
                    detail:
                      count.row === undefined
                        ? `${count.runs} commit attempts since the last successful commit failed on the Ed25519 verifier: ${fault.error}`
                        : `${count.runs} commit attempts since the last successful commit failed on a local fault (withdrawal ${count.row} could not be classified), the last on the Ed25519 verifier: ${fault.error}`,
                  }
                : {
                    source: COMMIT_WORKER_SOURCE,
                    reason: COMMIT_WORKER_WITHDRAWAL_UNCLASSIFIABLE,
                    detail: `${count.runs} commit attempts since the last successful commit failed on a local fault, the last on withdrawal ${row}, which this node cannot classify: ${fault.error}`,
                  },
            ),
          )
        : Effect.void,
    ),
  );
};
