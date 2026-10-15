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
  MidgardCekWallCapExceeded,
} from "./cek-executor.js";
import {
  type MidgardCekGatedSet,
  MidgardCekOutsideGatedSet,
} from "./cek-gated-set.js";
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
  /**
   * `Date.now()` time past which the run stops as `wall_cap_exceeded`. An
   * absolute time, so a run moved to another thread keeps the same deadline.
   */
  readonly wallDeadlineMs?: number;
  /**
   * Stops the run as `outside_gated_set` at its first step outside this set.
   * Plain data, so it crosses a thread boundary with the run.
   */
  readonly gatedSet?: MidgardCekGatedSet;
};

/**
 * A transaction whose script runs passed the caller's wall-clock cap
 * (`PhaseBConfig.scriptWallCapMs`). It is the node's local resource limit,
 * not the transaction's verdict: admission refuses the transaction and a
 * commit evicts it, and neither records a script failure.
 */
export class ScriptWallCapExceededError extends Error {
  readonly _tag = "ScriptWallCapExceeded";
  constructor(
    readonly txId: Buffer,
    readonly wallCapMs: number,
    detail: string,
  ) {
    super(
      `transaction ${txId.toString("hex")} passed the ${wallCapMs.toString(10)} ms script wall cap: ${detail}`,
    );
    this.name = "ScriptWallCapExceededError";
  }
}

/**
 * A transaction whose script runs touch a builtin or machine arm outside the
 * caller's gated set (`PhaseBConfig.cekGatedSet`). Local admission policy,
 * not the transaction's verdict: admission refuses the transaction and
 * records no script failure.
 */
export class ScriptOutsideGatedSetError extends Error {
  readonly _tag = "ScriptOutsideGatedSet";
  constructor(
    readonly txId: Buffer,
    detail: string,
  ) {
    super(`transaction ${txId.toString("hex")}: ${detail}`);
    this.name = "ScriptOutsideGatedSetError";
  }
}

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

/**
 * A verdict-mode run as it crosses a thread boundary: the transaction's
 * program-material sidecar travels as its canonical bytes.
 */
export type MidgardCekVerdictRequest = MidgardCekScriptRun & {
  readonly programMaterialSidecarCbor: Uint8Array | null;
};

/**
 * Runs one Phase B script on the structural CEK executor. A run that halts
 * with an error is a `script_invalid` verdict. A budget stop is reported as
 * accepted with the spent budget; the caller compares it with the declared
 * ex-units. A run that passes its wall-clock deadline is
 * `wall_cap_exceeded`, and one stopped outside its gated set is
 * `outside_gated_set`. Anything else the run throws is the evaluator's own
 * failure, never the script's: `indeterminate`. The program envelope and
 * material were verified before the run, so a throw here is a bound or a
 * fault of the evaluator.
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
      wallDeadlineMs: run.wallDeadlineMs,
      gatedSet: run.gatedSet,
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
    if (cause instanceof MidgardCekWallCapExceeded) {
      return {
        graph,
        execution: null,
        result: { kind: "wall_cap_exceeded", detail: cause.message },
      };
    }
    if (cause instanceof MidgardCekOutsideGatedSet) {
      return {
        graph,
        execution: null,
        result: { kind: "outside_gated_set", detail: cause.message },
      };
    }
    return {
      graph,
      execution: null,
      result: {
        kind: "indeterminate",
        detail: `V1 CEK evaluation stopped without a verdict: ${String(cause)}`,
        cause,
      },
    };
  }
};

/**
 * The verdict of one script run, as Phase B computes it in process. A pool
 * worker runs this same function, so a verdict never depends on where it was
 * computed. Phase B decodes the same material before it runs any script, so
 * material this cannot decode is `indeterminate`, not a verdict.
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
        kind: "indeterminate",
        detail: `V1 CEK material did not decode for evaluation: ${String(cause)}`,
        cause,
      };
    }
  }
  return evaluateMidgardCekScript(request, material, "verdict").result;
};
