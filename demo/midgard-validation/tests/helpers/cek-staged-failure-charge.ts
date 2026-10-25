import {
  encodeCbor,
  PLUTUS_V3_CANONICAL_COST_MODEL_VIEW,
} from "@al-ft/midgard-core";
import * as UPLC from "@lucid-evolution/uplc";

import type { MidgardCekDirectValueWitness } from "../../src/cek-builtin.argument-kinds.js";
import type { MidgardCekBuiltinBudget } from "../../src/cek-cost.js";
import type { MidgardCekCoreStepWitness } from "../../src/cek-machine.js";
import { errorSuccessor } from "../../src/cek-machine.midgard-cek-builtin-argument-count.js";
import { MidgardCekErrorCodes } from "../../src/cek-machine.midgard-cek-core-step-witness.js";
import { verifyMidgardCekCoreStep } from "../../src/cek-machine.verify-builtin.js";
import { encodeMidgardCekCardanoFlatProgram } from "../../src/cek-program.cardano-flat.js";
import { evaluateMidgardCekScript } from "../../src/phase-b.evaluate-cek-script.js";
import type { MidgardUplcEvalProgram } from "../../src/plutus-script-evaluator.js";
import { UPLCConst } from "../../src/uplc-constant.js";
import { DataB } from "../../src/uplc-data.js";
import { UPLCProgram, type UPLCTerm } from "../../src/uplc-term.js";
import { builtinState } from "./cek-staged-failure-steps.js";
import { discard, lam, MIDGARD_CEK_TX_BUDGET } from "./cek-uplc-terms.js";

/** Charges and runs for the staged failure tests, against the pinned PV11
 * evaluator. */

const PIECE = 8_000;

/** The bytes `built(length, salt)` builds at run time. */
export const builtBytes = (length: number, salt = 0): Buffer =>
  Buffer.concat(
    Array.from({ length: Math.ceil(length / PIECE) }, (_, piece) =>
      Uint8Array.from(
        { length: Math.min(PIECE, length - piece * PIECE) },
        (_, index) => (index * 7 + salt + piece) & 0xff,
      ),
    ),
  );

export const bytesConstant = (value: Uint8Array): UPLCTerm =>
  UPLCConst.byteString(new DataB(value).bytes);

export type Budget = { readonly cpu: bigint; readonly memory: bigint };

const CONTEXT = Buffer.from("d87980", "hex");

/** The PV11 evaluator's start-up charge, spent before the first step. */
const STARTUP = { cpu: 100n, memory: 100n };

const program = (body: UPLCTerm): Buffer =>
  Buffer.from(
    encodeMidgardCekCardanoFlatProgram(
      new UPLCProgram([1, 1, 0], lam(discard(body))),
    ),
  );

/** Runs a script that discards its context and evaluates `body` under
 * `budget`. */
export const runWithin = (
  body: UPLCTerm,
  mode: "proof" | "verdict",
  budget: Budget = MIDGARD_CEK_TX_BUDGET,
) =>
  evaluateMidgardCekScript(
    {
      scriptBytes: encodeCbor(program(body)),
      scriptLanguage: "PlutusV3",
      contextCbor: CONTEXT,
      executionIndex: 0n,
      maxSteps: 10_000,
      executionBudget: budget,
    },
    mode,
  );

/**
 * What the pinned PV11 evaluator charges the failing builtin of `body`,
 * whose arguments are program constants. It charges machine steps in
 * batches, so a run that fails within its first batch has spent only the
 * start-up charge and the builtin's charge.
 */
export const evaluatorCharge = (
  body: UPLCTerm,
  budget: Budget = MIDGARD_CEK_TX_BUDGET,
): MidgardCekBuiltinBudget => {
  const evaluate = UPLC.eval_program_raw as MidgardUplcEvalProgram;
  const result = evaluate(
    program(body),
    [CONTEXT],
    3,
    11,
    BigInt64Array.from(PLUTUS_V3_CANONICAL_COST_MODEL_VIEW.map(BigInt)),
    budget.cpu,
    budget.memory,
    false,
  );
  try {
    if (result.ok) throw new Error("the evaluator accepted the run");
    return {
      cpu: BigInt(result.cpu) - STARTUP.cpu,
      memory: BigInt(result.mem) - STARTUP.memory,
    };
  } finally {
    result.free();
  }
};

/** Whether `witness` proves that builtin `tag` fails on `arguments_` with
 * `charge`. */
export const failureVerifies = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
  witness: MidgardCekCoreStepWitness,
  charge: MidgardCekBuiltinBudget,
): boolean => {
  const pre = builtinState(tag, arguments_);
  return verifyMidgardCekCoreStep(
    pre,
    errorSuccessor(pre, MidgardCekErrorCodes.BuiltinFailure, charge),
    witness,
  );
};
