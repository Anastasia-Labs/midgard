import { PLUTUS_V3_CANONICAL_COST_MODEL_VIEW } from "@al-ft/midgard-core";
import * as UPLC from "@lucid-evolution/uplc";

import type { MidgardCekDirectValueWitness } from "../../src/cek-builtin.argument-kinds.js";
import {
  referenceConstantToDirectWitness,
  semanticizeMidgardCekDirectConstant,
} from "../../src/cek-builtin.evaluate-reference-builtin.js";
import type { MidgardCekBuiltinBudget } from "../../src/cek-cost.js";
import type { MidgardCekExecutionStep } from "../../src/cek-executor.build-midgard-cek-execution-graph.js";
import { encodeMidgardCekCardanoFlatProgram } from "../../src/cek-program.cardano-flat.js";
import type { MidgardCekWriteBitsWitness } from "../../src/cek-write-bits-builtin.js";
import type { MidgardUplcEvalProgram } from "../../src/plutus-script-evaluator.js";
import { constT, UPLCConst } from "../../src/uplc-constant.js";
import { ErrorUPLC, UPLCProgram, type UPLCTerm } from "../../src/uplc-term.js";
import { bytesConstant } from "./cek-staged-failure-charge.js";
import { call } from "./cek-staged-route-terms.js";
import {
  discard,
  ifThenElse,
  lam,
  MIDGARD_CEK_TX_BUDGET,
} from "./cek-uplc-terms.js";
import { builtin } from "./uplc-builtins.js";

/** writeBits runs, and their arguments as a step presents them, checked
 * against the pinned PV11 evaluator. */

export const WRITE_BITS = 80n;

export const indicesConstant = (values: readonly bigint[]): UPLCConst =>
  UPLCConst.listOf(constT.int)(
    values.map((value) => UPLCConst.int(value).value),
  );

/** writeBits over program constants. */
export const writeBitsOver = (
  input: Uint8Array,
  indices: readonly bigint[],
  set: boolean,
): UPLCTerm =>
  call(
    builtin("writeBits"),
    bytesConstant(input),
    indicesConstant(indices),
    UPLCConst.bool(set),
  );

/** The step's compacted arguments: what the roots hop commits. */
export const compactedArguments = (
  input: Uint8Array,
  indices: readonly bigint[],
  set: boolean,
): MidgardCekDirectValueWitness[] =>
  [
    UPLCConst.byteString(Buffer.from(input)),
    indicesConstant(indices),
    UPLCConst.bool(set),
  ].map((value) =>
    semanticizeMidgardCekDirectConstant(
      referenceConstantToDirectWitness(value, true),
    ),
  );

/** The compacted byte string `bytes`. */
export const compactedBytes = (
  bytes: Uint8Array,
): MidgardCekDirectValueWitness =>
  semanticizeMidgardCekDirectConstant(
    referenceConstantToDirectWitness(
      UPLCConst.byteString(Buffer.from(bytes)),
      true,
    ),
  );

const CONTEXT = Buffer.from("d87980", "hex");

/** A budget past any transaction's, for checks of a result alone. */
const UNBOUNDED = { cpu: 1_000_000_000_000n, memory: 1_000_000_000n };

const evaluate = (
  body: UPLCTerm,
  budget: { readonly cpu: bigint; readonly memory: bigint },
) => {
  const evaluateProgram = UPLC.eval_program_raw as MidgardUplcEvalProgram;
  const result = evaluateProgram(
    Buffer.from(
      encodeMidgardCekCardanoFlatProgram(
        new UPLCProgram([1, 1, 0], lam(discard(body))),
      ),
    ),
    [CONTEXT],
    3,
    11,
    BigInt64Array.from(PLUTUS_V3_CANONICAL_COST_MODEL_VIEW.map(BigInt)),
    budget.cpu,
    budget.memory,
    false,
  );
  try {
    return {
      ok: result.ok,
      cpu: BigInt(result.cpu),
      memory: BigInt(result.mem),
    };
  } finally {
    result.free();
  }
};

/** The PV11 charge of one machine step. */
const MACHINE_STEP = { cpu: 16_000n, memory: 100n };

/**
 * The pinned PV11 evaluator's verdict on writeBits over program constants
 * within the transaction budget, and on success its charge: the run's spend
 * less the spend of its input alone, less the six machine steps the call
 * adds (three applications, the builtin and two constants).
 */
export const evaluatorWriteBits = (
  input: Uint8Array,
  indices: readonly bigint[],
  set: boolean,
):
  | { readonly ok: false }
  | {
      readonly ok: true;
      readonly charge: MidgardCekBuiltinBudget;
    } => {
  const withCall = evaluate(
    writeBitsOver(input, indices, set),
    MIDGARD_CEK_TX_BUDGET,
  );
  if (!withCall.ok) return { ok: false };
  const without = evaluate(bytesConstant(input), MIDGARD_CEK_TX_BUDGET);
  return {
    ok: true,
    charge: {
      cpu: withCall.cpu - without.cpu - 6n * MACHINE_STEP.cpu,
      memory: withCall.memory - without.memory - 6n * MACHINE_STEP.memory,
    },
  };
};

/** Whether the pinned PV11 evaluator accepts `body` within the transaction
 * budget. */
export const evaluatorAccepts = (body: UPLCTerm): boolean =>
  evaluate(body, MIDGARD_CEK_TX_BUDGET).ok;

/** Whether the pinned PV11 evaluator's writeBits over these constants gives
 * `out`. */
export const evaluatorWritesTo = (
  input: Uint8Array,
  indices: readonly bigint[],
  set: boolean,
  out: Uint8Array,
): boolean =>
  evaluate(
    ifThenElse(
      call(
        builtin("equalsByteString"),
        writeBitsOver(input, indices, set),
        bytesConstant(out),
      ),
      UPLCConst.unit,
      new ErrorUPLC(),
    ),
    UNBOUNDED,
  ).ok;

export type MidgardCekWriteBitsStep = MidgardCekExecutionStep & {
  readonly witness: MidgardCekWriteBitsWitness;
};

/** The run's writeBits loop steps. */
export const writeBitsSteps = (
  steps: readonly MidgardCekExecutionStep[],
): MidgardCekWriteBitsStep[] =>
  steps.filter(
    (step): step is MidgardCekWriteBitsStep =>
      step.witness.kind === "executeBuiltinWriteBits" ||
      step.witness.kind === "executeBuiltinWriteBitsFailure",
  );

/** What a step charges. */
export const stepCharge = (step: MidgardCekExecutionStep) => ({
  cpu: BigInt(step.post.cpu) - BigInt(step.pre.cpu),
  memory: BigInt(step.post.memory) - BigInt(step.pre.memory),
});

export {
  pattern,
  spread,
} from "../fixtures/cek-write-bits-loop-v1.vectors.mjs";
