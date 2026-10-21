/**
 * The import checker's evaluator: one script run of an imported
 * transaction, under a kill. A crash or any other evaluator failure is a
 * `fault`, a run killed at its kill or whose worker failed before the run
 * was sent to it is a `timeout`, and a run not started in time is
 * `cancelled`; none is a verdict. The kill is wall-clock time, which
 * machine load stretches, and a worker that cannot start says nothing
 * about the run, so a `timeout` says nothing about the transaction. The evaluator in use is the validation pool's verdict-mode
 * CEK evaluator (`poolImportCheckEvaluator`); another one only implements
 * `ImportCheckEvaluator`.
 */
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import {
  type LocalScriptEvalResult,
  type MidgardCekVerdictRequest,
} from "@al-ft/midgard-validation";
import { Effect } from "effect";

import {
  type ImportCheckLane,
  ValidationJobTimeoutError,
  type ValidationPoolService,
} from "../services/validation-pool.js";

/**
 * - `evaluated`: the evaluator's result, compared with the declared budget
 *   by the rebuilt replay as in any Phase B run;
 * - `fault`: crashed, or failed to evaluate;
 * - `timeout`: killed at its kill, or its worker failed (or its start timed
 *   out) before the run was sent to it;
 * - `cancelled`: not started by its `startBy`.
 */
export type ImportCheckOutcome =
  | Readonly<{ kind: "evaluated"; result: LocalScriptEvalResult }>
  | Readonly<{ kind: "fault"; detail: string }>
  | Readonly<{ kind: "timeout"; detail: string }>
  | Readonly<{ kind: "cancelled" }>;

export type ImportCheckEvaluateOptions = Readonly<{
  killMs: number;
  fresh: boolean;
  lane: ImportCheckLane;
  /** Cancelled when not started by this time (epoch ms). */
  startBy?: number;
}>;

export type ImportCheckEvaluator = Readonly<{
  /**
   * Runs `run` in `lane`, killed `killMs` after it starts; with `fresh`, on
   * a newly started evaluator instance. Never fails: a kill is a `timeout`,
   * any other failure a `fault`.
   */
  evaluate: (
    run: MidgardCekVerdictRequest,
    options: ImportCheckEvaluateOptions,
  ) => Effect.Effect<ImportCheckOutcome>;
  /** The workers reserved for the backlog lane; 0 when none is. */
  backlogShare: number;
}>;

/**
 * The validation pool's evaluator, below admission; undefined when the pool
 * is disabled (no run can be killed on the main thread).
 */
export const poolImportCheckEvaluator = (
  pool: Pick<
    ValidationPoolService,
    "evaluateImportCheck" | "importCheckBacklogShare"
  >,
): ImportCheckEvaluator | undefined => {
  const evaluateImportCheck = pool.evaluateImportCheck;
  if (evaluateImportCheck === undefined) return undefined;
  return {
    backlogShare: pool.importCheckBacklogShare ?? 0,
    evaluate: (run, options) =>
      evaluateImportCheck(run, options).pipe(
        Effect.map((result): ImportCheckOutcome => {
          if (result.kind !== "accepted" && result.kind !== "script_invalid")
            return {
              kind: "fault",
              detail: `unknown evaluation result ${String((result as { kind: unknown }).kind)}`,
            };
          return { kind: "evaluated", result };
        }),
        Effect.catchAll((error) => {
          if (error.notStarted === true)
            return Effect.succeed<ImportCheckOutcome>({ kind: "cancelled" });
          const detail =
            error.cause === undefined
              ? error.message
              : `${error.message}: ${formatUnknownError(error.cause)}`;
          return Effect.succeed<ImportCheckOutcome>(
            error.workerUnavailable === true ||
              error.cause instanceof ValidationJobTimeoutError
              ? { kind: "timeout", detail }
              : { kind: "fault", detail },
          );
        }),
      ),
  };
};
