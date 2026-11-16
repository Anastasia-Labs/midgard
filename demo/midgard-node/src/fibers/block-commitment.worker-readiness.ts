import { LOCAL_FAULT_RUN_LIMIT } from "@al-ft/midgard-fault-proofs";
import { Effect, Ref } from "effect";

import { countLedgerDivergence } from "../mpf/local-ledger-divergence.js";
import type { Globals } from "../services/globals.js";
import {
  type ActiveLivenessReason,
  clearLivenessIncident,
  raiseLivenessIncident,
} from "../services/liveness-halt.js";
import { signalTransientExhausted } from "../services/transient-exhaustion.js";
import type {
  WithdrawalUnclassifiableCase,
  WorkerOutput,
} from "../workers/utils/commit-block-header.js";

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
 * commit failed on a local fault, the last of them on this process's Ed25519
 * verifier fault.
 */
export const COMMIT_WORKER_VERIFIER_UNAVAILABLE =
  "commit_worker_verifier_unavailable";
/**
 * The reason the node exits under (source `commit_worker`) once
 * `LOCAL_FAULT_RUN_LIMIT` commit-worker attempts since the last successful
 * commit failed on a local fault, the last of them on a due withdrawal row
 * this node cannot classify. The detail names the row and its case: the row
 * or the ledger output it names does not decode, or the row decodes and the
 * classifier failed on this node.
 */
export const COMMIT_WORKER_WITHDRAWAL_UNCLASSIFIABLE =
  "commit_worker_withdrawal_unclassifiable";
/** The liveness source `commit_base_pending` is raised under. */
export const COMMIT_BASE_SOURCE = "commit_base";
/**
 * The commit waits for its base: a tail landed-block processing has not
 * applied to the working ledger yet, or this node's own block in flight,
 * which is never a base until it lands and is applied. It clears on the
 * worker's next output that is not this wait.
 */
export const COMMIT_BASE_PENDING = "commit_base_pending";
/**
 * The commit is refused: its base's entries do not derive the root this
 * node stores for that base, named `commit_base_ledger_divergence:<check>`
 * by the check that found it (`COMMIT_BASE_DIVERGENCE_CHECKS`). Raised under
 * `commit_base` in place of `commit_base_pending`, and counted under the
 * check in `midgard_local_ledger_divergence_total`; it clears on the
 * worker's next output that is not this refusal.
 */
export const COMMIT_BASE_LEDGER_DIVERGENCE = "commit_base_ledger_divergence";
/**
 * The commit waits for the main thread's verification of `confirmed_ledger`
 * (`confirmed-ledger-verification.ts`): its base is a landed tail, built
 * from the landed frontier, and no verification has passed at that
 * frontier yet: none since startup, none since a fold moved the frontier,
 * or the last one found a divergence. Raised under `commit_base` in place
 * of `commit_base_pending`; it clears on the worker's next output that is
 * not this wait, and never on a timeout.
 */
export const COMMIT_BASE_LEDGER_UNVERIFIED = "commit_base_ledger_unverified";
/**
 * The last run of the main thread's verification of `confirmed_ledger`
 * failed before it could pass or find a divergence: its snapshot could
 * not be read, or its worker thread failed (out of its heap, for one).
 * Raised under `confirmed_ledger_verification`; commits on a landed tail
 * keep waiting as `commit_base_ledger_unverified` while no run passes at
 * the frontier. It clears on the next run that completes.
 */
export const CONFIRMED_LEDGER_VERIFICATION_FAILED =
  "confirmed_ledger_verification_failed";
/** The liveness source `commit_window_pending` is raised under. */
export const COMMIT_WINDOW_SOURCE = "commit_window";
/**
 * The commit waits for the next scheduler window: a dead journal whose
 * commit was signed holds its header hash. It clears on the worker's next
 * output that is not this wait.
 */
export const COMMIT_WINDOW_PENDING = "commit_window_pending";

/** The liveness source `commit_submit_refused` is raised under. */
export const COMMIT_SUBMIT_SOURCE = "commit_submit";
/**
 * The commit's L1 submission was refused before anything was broadcast,
 * named `commit_submit_refused:<code>` by the refusal's bounded code
 * (`COMMIT_SUBMIT_REFUSAL_CODES`). The attempt is abandoned and retried with
 * its payload, so the refusal is a detail until its unbroken run outlasts
 * `COMMIT_WORKER_FAILED_ESCALATION_MS`, then a reason. It clears with
 * `commit_worker_failed` (`clearCommitWorkerFailure`).
 */
export const COMMIT_SUBMIT_REFUSED = "commit_submit_refused";
/**
 * Why a commit's submission was refused before broadcast: the commit anchor
 * left the follower's chain (the pre-broadcast gate), the intent journal
 * refused the bytes (it also holds them under its own reason), signing
 * failed, or another refusal.
 */
export const COMMIT_SUBMIT_REFUSAL_CODES = [
  "COMMIT_ANCHOR_NOT_CANONICAL",
  "INTENT_JOURNAL_REFUSED",
  "SIGNING_FAILED",
  "UNCLASSIFIED",
] as const;
export type CommitSubmitRefusalCode =
  (typeof COMMIT_SUBMIT_REFUSAL_CODES)[number];

/** A raised `commit_submit_refused` that is still a readiness detail: its
 * run has not outlasted its bound. */
const commitSubmitRefusalIsDetail = ({
  source,
  escalated,
}: ActiveLivenessReason): boolean =>
  source === COMMIT_SUBMIT_SOURCE && !escalated;

/** The raised liveness reasons as `/readyz` names them: each in `reasons`,
 * but a `commit_submit_refused` still a detail in `details`. */
export const livenessReadiness = (
  active: readonly ActiveLivenessReason[],
): {
  readonly reasons: readonly ActiveLivenessReason[];
  readonly details: readonly string[];
} => ({
  reasons: active.filter((each) => !commitSubmitRefusalIsDetail(each)),
  details: active
    .filter(commitSubmitRefusalIsDetail)
    .map(({ reason }) => reason),
});

/** The liveness source `commit_scheduler_alignment_failed` is raised under. */
export const COMMIT_SCHEDULER_SOURCE = "commit_scheduler";
/**
 * Scheduler alignment before the commit worker failed while commit work is
 * pending, so the node cannot take its turn. It holds no fiber: the tick is
 * skipped and retried. It escalates after `COMMIT_WORKER_FAILED_ESCALATION_MS`
 * like a failing worker, and clears on the next alignment that succeeds or
 * waits, or the next tick with no work.
 */
export const COMMIT_SCHEDULER_ALIGNMENT_FAILED =
  "commit_scheduler_alignment_failed";

/** Reports worker failure without holding the commit fibers: they must retry
 * through their ordinary journal and lease gates. A failure is escalated
 * after `COMMIT_WORKER_FAILED_ESCALATION_MS`. Only actual worker success
 * or an independent empty-work preflight clears this reason. A heartbeat,
 * scheduler deferral or worker no-op does not prove outstanding work recovered.
 *
 * A submission refused before broadcast raises `commit_submit_refused`
 * under `commit_submit`, cleared with this reason.
 *
 * A commit waiting for its base raises `commit_base_pending` under
 * `commit_base`, `commit_base_ledger_unverified` where it waits for the
 * verification of the frontier it builds from, or
 * `commit_base_ledger_divergence:<check>` where the base's entries do not
 * derive its stored root, and one waiting for the next window raises
 * `commit_window_pending` under `commit_window`; any other output clears
 * each. */
export const applyCommitWorkerReadiness = (
  globals: Pick<Globals, "LIVENESS_REASONS">,
  output: WorkerOutput,
): Effect.Effect<void> => {
  if (output.type === "AwaitingCommitBaseOutput")
    return Effect.all(
      [
        clearLivenessIncident(globals, COMMIT_WINDOW_SOURCE),
        raiseLivenessIncident(
          globals,
          COMMIT_BASE_SOURCE,
          output.divergence !== undefined
            ? `${COMMIT_BASE_LEDGER_DIVERGENCE}:${output.divergence}`
            : output.unverified === true
              ? COMMIT_BASE_LEDGER_UNVERIFIED
              : COMMIT_BASE_PENDING,
          `commit base ${output.baseHeaderHash}: ${output.detail}`,
        ),
        // The worker's check is counted here, in the main thread whose
        // metrics the exporter reads, once per refused attempt.
        output.divergence === undefined
          ? Effect.void
          : countLedgerDivergence({ check: output.divergence }),
      ],
      { discard: true },
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
      return clearCommitWorkerFailure(globals);
    case "SkippedSubmissionOutput":
      return output.submitRefusal === undefined
        ? Effect.void
        : raiseLivenessIncident(
            globals,
            COMMIT_SUBMIT_SOURCE,
            `${COMMIT_SUBMIT_REFUSED}:${output.submitRefusal.code}`,
            output.submitRefusal.detail,
            { escalateAfterMs: COMMIT_WORKER_FAILED_ESCALATION_MS },
          );
    case "NothingToCommitOutput":
    case "RegisteredDueWorkOutput":
      return Effect.void;
  }
};

export const clearCommitWorkerFailure = (
  globals: Pick<Globals, "LIVENESS_REASONS">,
): Effect.Effect<void> =>
  Effect.all(
    [
      clearLivenessIncident(globals, COMMIT_WORKER_SOURCE),
      clearLivenessIncident(globals, COMMIT_SUBMIT_SOURCE),
      clearLivenessIncident(globals, COMMIT_BASE_SOURCE),
      clearLivenessIncident(globals, COMMIT_WINDOW_SOURCE),
      clearLivenessIncident(globals, COMMIT_SCHEDULER_SOURCE),
    ],
    { discard: true },
  );

/** What the exit's detail says of each case of a withdrawal row this node
 * cannot classify. */
const UNCLASSIFIABLE_CASE_TEXT: Readonly<
  Record<WithdrawalUnclassifiableCase, string>
> = {
  undecodable: "the row or the ledger output it names does not decode",
  classifier_failed: "the row decodes, and the classifier failed on this node",
};

/** The outputs that prove a commit went through: the same set that clears
 * `commit_worker_failed`. */
const COMMIT_SUCCEEDED: ReadonlySet<WorkerOutput["type"]> = new Set([
  "SuccessfulSubmissionOutput",
  "SubmittedAwaitingConfirmationOutput",
]);

/**
 * Bounds commit-worker attempts that fail on a local fault
 * (`FailureOutput.localFault`): this process's Ed25519 verifier fault, or a
 * due withdrawal row it cannot classify, named in `localFaultSubject`. This
 * is how the coalesced runner bounds a local fault on a landed block: each
 * such failure counts, whichever row it names, and once `limit` of them
 * accrue without a successful commit (`COMMIT_SUCCEEDED`) between them the
 * node exits non-zero (`transient-exhaustion.ts`) for its supervisor to
 * restart it, under the reason of the fault that reached the bound. Only a
 * successful commit resets the count. The latest row is kept for the exit's
 * detail alone, which names it and its case. A wait (for the commit base or
 * window), a no-op, a skip, or a failure that is not a local fault leaves the
 * count as it is, so a persistent fault interleaved with them still reaches
 * the bound. The count is this process's, so a restarted node meeting the
 * same fault retries within the same bound and exits again. Failures that are
 * not local faults are never counted and stay `commit_worker_failed` as
 * before.
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
                    detail: `${count.runs} commit attempts since the last successful commit failed on a local fault, the last on withdrawal ${row}, which this node cannot classify (${UNCLASSIFIABLE_CASE_TEXT[fault.localFaultCase]}): ${fault.error}`,
                  },
            ),
          )
        : Effect.void,
    ),
  );
};
