import {
  encodeCbor,
  PLUTUS_V3_CANONICAL_COST_MODEL_VIEW,
} from "@al-ft/midgard-core";
import {
  Builtin,
  UPLCConst,
  UPLCEncoder,
  UPLCProgram,
  type UPLCTerm,
} from "@harmoniclabs/uplc";
import * as UPLC from "@lucid-evolution/uplc";
import { describe, expect, it } from "vitest";

import { evaluateMidgardCekScript } from "../src/phase-b.evaluate-cek-script.js";
import type { MidgardUplcEvalProgram } from "../src/plutus-script-evaluator.js";
import {
  app,
  discard,
  lam,
  MIDGARD_CEK_TX_BUDGET,
} from "./helpers/cek-uplc-terms.js";

/**
 * multiplyInteger and the division builtins over operands past the direct
 * carriage bound. The proof-mode run reaches the PV11 evaluator's verdict and
 * budget, including the budget a division by zero spends before it fails.
 */

const CONTEXT = Buffer.from("d87980", "hex");

const int = (value: bigint): UPLCTerm => UPLCConst.int(value);
const call = (builtin: Builtin, ...args: readonly UPLCTerm[]): UPLCTerm =>
  app(builtin as never, ...args);

/** An integer of `size` magnitude bytes. */
const wide = (size: number, low = 0n): bigint =>
  (1n << BigInt(8 * size - 1)) + low;

const flat = (body: UPLCTerm): Buffer =>
  Buffer.from(
    UPLCEncoder.compile(new UPLCProgram([1, 1, 0], lam(discard(body)))),
  );

const run = (body: UPLCTerm, mode: "proof" | "verdict") =>
  evaluateMidgardCekScript(
    {
      scriptBytes: encodeCbor(flat(body)),
      scriptLanguage: "PlutusV3",
      contextCbor: CONTEXT,
      executionIndex: 0n,
      maxSteps: 10_000,
      executionBudget: MIDGARD_CEK_TX_BUDGET,
    },
    mode,
  );

/** The budget the pinned PV11 evaluator spends on `body`, failing or not. */
const spent = (body: UPLCTerm) => {
  const evaluate = UPLC.eval_program_raw as MidgardUplcEvalProgram;
  const result = evaluate(
    flat(body),
    [CONTEXT],
    3,
    11,
    BigInt64Array.from(PLUTUS_V3_CANONICAL_COST_MODEL_VIEW.map(BigInt)),
    MIDGARD_CEK_TX_BUDGET.cpu,
    MIDGARD_CEK_TX_BUDGET.memory,
    false,
  );
  try {
    return { ok: result.ok, cpu: result.cpu, memory: result.mem };
  } finally {
    result.free();
  }
};

/** The PV11 evaluator's start-up charge, spent before the first step. */
const STARTUP = { cpu: 100n, memory: 100n };

/**
 * The charge of the failing builtin step in the proof run. The PV11 evaluator
 * charges machine steps in batches, so a run that fails within its first
 * batch has spent only the start-up charge and the builtin's charge.
 */
const failureCharge = (body: UPLCTerm) => {
  const proof = run(body, "proof");
  expect(proof.result.kind).toBe("script_invalid");
  const step = proof.execution?.steps.find(
    ({ witness }) => witness.kind === "executeBuiltinFailure",
  );
  expect(step).toBeDefined();
  return {
    cpu: BigInt(step!.post.cpu) - BigInt(step!.pre.cpu),
    memory: BigInt(step!.post.memory) - BigInt(step!.pre.memory),
  };
};

const evaluatorCharge = (body: UPLCTerm) => {
  const wasm = spent(body);
  expect(wasm.ok).toBe(false);
  return {
    cpu: BigInt(wasm.cpu) - STARTUP.cpu,
    memory: BigInt(wasm.memory) - STARTUP.memory,
  };
};

const DIVIDENDS: readonly (readonly [string, bigint])[] = [
  ["7", 7n],
  ["a 4,000-byte integer", -wide(4_000, 3n)],
];

const rows = (divisions: readonly (readonly [string, Builtin])[]) =>
  divisions.flatMap(([name, builtin]) =>
    DIVIDENDS.map(
      ([label, dividend]) =>
        [`${name} of ${label}`, builtin, dividend] as const,
    ),
  );

describe("division by zero", () => {
  it.each(
    rows([
      ["quotientInteger", Builtin.quotientInteger],
      ["remainderInteger", Builtin.remainderInteger],
      ["modInteger", Builtin.modInteger],
    ]),
  )(
    "charges %s by zero what the PV11 evaluator charges",
    (_, builtin, dividend) => {
      const body = call(builtin, int(dividend), int(0n));
      expect(run(body, "verdict").result.kind).toBe("script_invalid");
      expect(failureCharge(body)).toEqual(evaluatorCharge(body));
    },
  );
});
