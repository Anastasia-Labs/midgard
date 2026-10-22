import {
  buildMidgardCekExecutionGraph,
  executeMidgardCekStructuralProgram,
  type MidgardCekExecutionGraph,
  type MidgardCekExecutionMode,
  type MidgardCekStructuralExecution,
} from "./cek-executor.js";
import { buildMidgardCanonicalCekProgram } from "./cek-program.js";
import { MidgardUplcCapabilityError } from "./plutus-script-evaluator.capability-error.js";
import {
  evaluateStoredMidgardPlutusScript,
  storedMidgardPlutusScriptPhase2Class,
} from "./plutus-script-evaluator.js";
import type { ScriptDecodePhase2Class } from "./script-decode.step-types.js";
import { scriptDecodeRecordOfScript } from "./script-decode.step-walk.js";
import { type LocalScriptEvalResult } from "./types.js";

/** One Phase B script execution: its program, context and limits. */
export type MidgardCekScriptRun = {
  readonly scriptBytes: Uint8Array;
  readonly scriptLanguage: "PlutusV3" | "MidgardV1";
  readonly contextCbor: Uint8Array;
  readonly executionBudget?: {
    readonly cpu: bigint;
    readonly memory: bigint;
  };
  readonly executionIndex: bigint;
  readonly maxSteps: number;
};

/**
 * A transaction whose script run has no verdict because the evaluator
 * itself failed: a throw, one of its own bounds (the step bound, an internal
 * payload bound) or a proof-mode self-check. The script's bytes were checked
 * before the run, so the failure is never the transaction's verdict: no
 * caller rejects the transaction for it. `cause` is the evaluator's own
 * failure, kept so a caller can tell a node fault (memory, the runtime) apart.
 */
export class ScriptEvaluationIndeterminateError extends Error {
  readonly _tag = "ScriptEvaluationIndeterminate";
  constructor(
    readonly txId: Buffer,
    detail: string,
    options?: { readonly cause?: unknown },
  ) {
    super(
      `transaction ${txId.toString("hex")} has no script verdict: ${detail}`,
      options?.cause === undefined ? undefined : { cause: options.cause },
    );
    this.name = "ScriptEvaluationIndeterminateError";
  }
}

/** Exact worker request; the carried script is sufficient for evaluation. */
export type MidgardCekVerdictRequest = MidgardCekScriptRun;

const indeterminate = (cause: unknown): LocalScriptEvalResult => ({
  kind: "indeterminate",
  detail: `Plutus evaluation stopped without a verdict: ${String(cause)}`,
  cause,
});

/**
 * Runs one Phase B script. Verdict mode runs the carried Flat program on the
 * PV11 evaluator. Proof mode derives the local structural trace; a budget
 * stop records the spent budget for the caller to compare with the declared
 * ex-units.
 *
 * Phase A decodes every carried Plutus script before any run, so a script
 * that reaches here decodes. A throw is the evaluator's own failure, never
 * the script's: `indeterminate`. That includes a local capability failure
 * (`MidgardUplcCapabilityError`), every bound of the evaluator and a proof
 * self-check.
 */
export const evaluateMidgardCekScript = (
  run: MidgardCekScriptRun,
  mode: MidgardCekExecutionMode,
): {
  readonly result: LocalScriptEvalResult;
  readonly phase2Class?: ScriptDecodePhase2Class;
  readonly graph: MidgardCekExecutionGraph | null;
  readonly execution: MidgardCekStructuralExecution | null;
} => {
  if (mode === "verdict") {
    const result = evaluateMidgardCekVerdict(run);
    // A run with a verdict decoded, so its class is the cached record's.
    return {
      result,
      ...(result.kind === "indeterminate"
        ? {}
        : { phase2Class: storedMidgardPlutusScriptPhase2Class(run) }),
      graph: null,
      execution: null,
    };
  }
  let graph: MidgardCekExecutionGraph | null = null;
  try {
    const decoded = scriptDecodeRecordOfScript(
      run.scriptLanguage === "PlutusV3" ? 3 : 128,
      run.scriptBytes,
    );
    if (decoded.k === "refused")
      throw new MidgardUplcCapabilityError(
        "Proof evaluation received a Plutus script that failed phase-one decoding",
      );
    if (decoded.phase2Class !== 0)
      return {
        result: {
          kind: "script_invalid",
          detail: `UPLC PV11 phase-two class ${decoded.phase2Class}`,
        },
        phase2Class: decoded.phase2Class,
        graph: null,
        execution: null,
      };
    const program = buildMidgardCanonicalCekProgram(run.scriptBytes);
    if (!decoded.termRoot.equals(program.termRoot))
      throw new Error(
        "Derived CEK material differs from the decoded record term root",
      );
    graph = buildMidgardCekExecutionGraph(
      program.termRoot,
      program.material.values(),
      run.contextCbor,
    );
    const outcome = executeMidgardCekStructuralProgram({
      root: graph.root,
      material: graph.material.values(),
      constantWitnesses: graph.constantWitnesses,
      executionIndex: run.executionIndex,
      maxSteps: run.maxSteps,
      executionBudget: run.executionBudget,
      mode,
    });
    return {
      phase2Class: 0,
      graph,
      // Only a proof-mode run carries the per-step evidence a trace consumer
      // needs; a verdict-mode run is reported without it.
      execution: "steps" in outcome ? outcome : null,
      result:
        outcome.stopReason === "budgetExceeded" ||
        outcome.terminalState.mode === "haltSuccess"
          ? {
              kind: "accepted",
              budget: {
                cpu: outcome.terminalState.cpu,
                memory: outcome.terminalState.memory,
              },
            }
          : {
              kind: "script_invalid",
              detail: `V1 CEK halted with error ${outcome.terminalState.auxiliary.toString(10)}`,
            },
    };
  } catch (cause) {
    return { graph, execution: null, result: indeterminate(cause) };
  }
};

/**
 * The verdict of one script run, as Phase B computes it in process. A pool
 * worker runs this same function, so a verdict never depends on where it was
 * computed. An evaluator failure is `indeterminate`, not a verdict.
 */
export const evaluateMidgardCekVerdict = (
  request: MidgardCekVerdictRequest,
): LocalScriptEvalResult => {
  try {
    return evaluateStoredMidgardPlutusScript(request);
  } catch (cause) {
    return indeterminate(cause);
  }
};
