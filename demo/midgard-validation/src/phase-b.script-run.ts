import { type MidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import { Effect } from "effect";

import {
  type MidgardCekExecutionGraph,
  type MidgardCekStructuralExecution,
} from "./cek-executor.js";
import {
  evaluateMidgardCekScript,
  type MidgardCekScriptRun,
} from "./phase-b.evaluate-cek-script.js";
import { type RequiredScriptExecution } from "./phase-b.resolve-reference-inputs.js";
import { encodeMidgardCekPlutusData } from "./plutus-data-iterative.encode.js";
import {
  buildMidgardScriptContext,
  buildPlutusV3ScriptContext,
  type ScriptContextView,
} from "./script-context.js";
import { type LocalScriptEvalResult, type PhaseBConfig } from "./types.js";

/** The canonical script context bytes one non-native execution runs over. */
export const scriptExecutionContextCbor = (
  view: ScriptContextView,
  execution: RequiredScriptExecution,
  redeemer: Parameters<typeof buildMidgardScriptContext>[2],
): Uint8Array =>
  encodeMidgardCekPlutusData(
    execution.resolved.version === "MidgardV1"
      ? buildMidgardScriptContext(view, execution.purpose, redeemer)
      : buildPlutusV3ScriptContext(view, execution.purpose, redeemer),
  );

/**
 * Runs one script the way Phase B does: through the proof hook when one is
 * set, through the out-of-process verdict hook in verdict mode, and
 * otherwise on the in-process structural executor.
 */
export const runScriptExecution = (
  run: MidgardCekScriptRun,
  programMaterialSidecarCbor: Uint8Array | null,
  material: MidgardCekProgramMaterialSidecar,
  config: Pick<
    PhaseBConfig,
    "evaluateProofScript" | "evaluateCekVerdict" | "cekExecutionMode"
  >,
): Effect.Effect<
  {
    readonly result: LocalScriptEvalResult;
    readonly graph: MidgardCekExecutionGraph | null;
    readonly execution: MidgardCekStructuralExecution | null;
  },
  Error
> =>
  Effect.gen(function* () {
    const mode = config.cekExecutionMode ?? "verdict";
    if (config.evaluateProofScript !== undefined) {
      const result = yield* config.evaluateProofScript(
        run.scriptBytes,
        run.contextCbor,
        run.executionBudget,
        run.executionIndex,
      );
      return { result, graph: null, execution: null };
    }
    if (mode === "verdict" && config.evaluateCekVerdict !== undefined) {
      const result = yield* config.evaluateCekVerdict({
        ...run,
        programMaterialSidecarCbor,
      });
      return { result, graph: null, execution: null };
    }
    return evaluateMidgardCekScript(run, material, mode);
  });
