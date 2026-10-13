import {
  decodeMidgardCekProgramEnvelope,
  decodeMidgardCekProgramMaterialSidecar,
  type MidgardCekProgramMaterialSidecar,
} from "@al-ft/midgard-core/cek-proof";

import {
  buildMidgardCekExecutionGraph,
  executeMidgardCekStructuralProgram,
  type MidgardCekExecutionGraph,
  type MidgardCekExecutionMode,
  type MidgardCekStructuralExecution,
} from "./cek-executor.js";
import { type LocalScriptEvalResult } from "./types.js";

/** One Phase B script execution: its program, context and limits. */
export type MidgardCekScriptRun = {
  readonly scriptBytes: Uint8Array;
  readonly contextCbor: Uint8Array;
  readonly executionBudget?: {
    readonly cpu: bigint;
    readonly memory: bigint;
  };
  readonly executionIndex: bigint;
  readonly maxSteps: number;
};

/**
 * A verdict-mode run as it crosses a thread boundary: the transaction's
 * program-material sidecar travels as its canonical bytes.
 */
export type MidgardCekVerdictRequest = MidgardCekScriptRun & {
  readonly programMaterialSidecarCbor: Uint8Array | null;
};

/**
 * Runs one Phase B script on the structural CEK executor. Every failure,
 * thrown or halted, is a `script_invalid` verdict. A budget stop is
 * reported as accepted with the spent budget; the caller compares it with
 * the declared ex-units.
 */
export const evaluateMidgardCekScript = (
  run: MidgardCekScriptRun,
  material: MidgardCekProgramMaterialSidecar,
  mode: MidgardCekExecutionMode,
): {
  readonly result: LocalScriptEvalResult;
  readonly graph: MidgardCekExecutionGraph | null;
  readonly execution: MidgardCekStructuralExecution | null;
} => {
  let graph: MidgardCekExecutionGraph | null = null;
  try {
    const envelope = decodeMidgardCekProgramEnvelope(run.scriptBytes);
    graph = buildMidgardCekExecutionGraph(envelope, material, run.contextCbor);
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
    return {
      graph,
      execution: null,
      result: {
        kind: "script_invalid",
        detail: `V1 CEK execution failed closed: ${String(cause)}`,
      },
    };
  }
};

/**
 * The verdict of one script run, as Phase B computes it in process. A pool
 * worker runs this same function, so a verdict never depends on where it was
 * computed.
 */
export const evaluateMidgardCekVerdict = (
  request: MidgardCekVerdictRequest,
): LocalScriptEvalResult => {
  let material: MidgardCekProgramMaterialSidecar = [];
  if (request.programMaterialSidecarCbor !== null) {
    try {
      material = decodeMidgardCekProgramMaterialSidecar(
        request.programMaterialSidecarCbor,
      );
    } catch (cause) {
      return {
        kind: "script_invalid",
        detail: `V1 CEK execution failed closed: ${String(cause)}`,
      };
    }
  }
  return evaluateMidgardCekScript(request, material, "verdict").result;
};
