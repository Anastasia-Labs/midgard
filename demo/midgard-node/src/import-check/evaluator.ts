/**
 * The import checker's evaluator: one script run of an imported
 * transaction, under a kill. A kill, a crash or any evaluator failure is a
 * `fault`, never a verdict. The evaluator in use is the validation pool's
 * verdict-mode CEK evaluator (`poolImportCheckEvaluator`); another one only
 * implements `ImportCheckEvaluator`.
 */
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { type MidgardCekVerdictRequest } from "@al-ft/midgard-validation";
import { Effect } from "effect";

import { type ImportCheckLoad } from "../database/importChecks.js";
import { type ImportCheckConfig } from "../services/config.import-check.js";
import {
  type ImportCheckLane,
  type ValidationPoolService,
} from "../services/validation-pool.js";

export type ImportCheckOutcome =
  | Readonly<{ kind: "accepted" }>
  | Readonly<{ kind: "rejected"; detail: string }>
  | Readonly<{ kind: "fault"; detail: string }>;

export type ImportCheckEvaluator = Readonly<{
  /**
   * Runs `run` in `lane`, killed `killMs` after it starts; with `fresh`, on
   * a newly started evaluator instance. Never fails: a failure is a
   * `fault`.
   */
  evaluate: (
    run: MidgardCekVerdictRequest,
    options: Readonly<{
      killMs: number;
      fresh: boolean;
      lane: ImportCheckLane;
    }>,
  ) => Effect.Effect<ImportCheckOutcome>;
}>;

/** The bytes a run decodes: its script, its context and its sidecar. */
export const importCheckRunBytes = (run: MidgardCekVerdictRequest): number =>
  run.scriptBytes.byteLength +
  run.contextCbor.byteLength +
  (run.programMaterialSidecarCbor?.byteLength ?? 0);

/** The kill of one run: `floor + a * declared cpu + b * bytes`. */
export const importCheckKillMs = (
  run: MidgardCekVerdictRequest,
  config: ImportCheckConfig,
): number =>
  Math.ceil(
    config.killFloorMs +
      (Number(run.executionBudget?.cpu ?? 0n) * config.killMsPerGigaCpu) / 1e9 +
      (importCheckRunBytes(run) * config.killUsPerKib) / 1024 / 1000,
  );

/** An upper bound on the kills of an entry's runs, from its load. */
export const importCheckLoadKillMs = (
  load: ImportCheckLoad,
  config: ImportCheckConfig,
): number =>
  load.runs * (config.killFloorMs + 1) +
  (load.cpu * config.killMsPerGigaCpu) / 1e9 +
  (load.bytes * config.killUsPerKib) / 1024 / 1000;

/**
 * The validation pool's evaluator, below admission; undefined when the pool
 * is disabled (no run can be killed on the main thread).
 */
export const poolImportCheckEvaluator = (
  pool: Pick<ValidationPoolService, "evaluateImportCheck">,
): ImportCheckEvaluator | undefined => {
  const evaluateImportCheck = pool.evaluateImportCheck;
  if (evaluateImportCheck === undefined) return undefined;
  return {
    evaluate: (run, options) =>
      evaluateImportCheck(run, options).pipe(
        Effect.map((result): ImportCheckOutcome => {
          if (result.kind === "script_invalid")
            return { kind: "rejected", detail: result.detail };
          if (result.kind !== "accepted")
            return {
              kind: "fault",
              detail: `unknown evaluation result ${String((result as { kind: unknown }).kind)}`,
            };
          const declared = run.executionBudget;
          return declared !== undefined &&
            (result.budget.cpu > declared.cpu ||
              result.budget.memory > declared.memory)
            ? {
                kind: "rejected",
                detail: `budget exceeded (spent mem=${result.budget.memory} cpu=${result.budget.cpu}, declared mem=${declared.memory} cpu=${declared.cpu})`,
              }
            : { kind: "accepted" };
        }),
        Effect.catchAll((error) =>
          Effect.succeed<ImportCheckOutcome>({
            kind: "fault",
            detail:
              error.cause === undefined
                ? error.message
                : `${error.message}: ${formatUnknownError(error.cause)}`,
          }),
        ),
      ),
  };
};
