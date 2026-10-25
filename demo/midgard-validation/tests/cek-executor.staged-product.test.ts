import {
  encodeCbor,
  hashMidgardCekValueNode,
  PLUTUS_V3_CANONICAL_COST_MODEL_VIEW,
} from "@al-ft/midgard-core";
import * as UPLC from "@lucid-evolution/uplc";
import { describe, expect, it } from "vitest";

import { referenceConstantToDirectWitness } from "../src/cek-builtin.evaluate-reference-builtin.js";
import {
  evaluateMidgardCekDirectBuiltin,
  hashMidgardCekDirectArguments,
  midgardCekDirectBuiltinBudget,
  semanticizeMidgardCekDirectConstant,
  verifyMidgardCekDirectBuiltinFailure,
} from "../src/cek-builtin.js";
import { verifyMidgardCekCoreStep } from "../src/cek-machine.verify-builtin.js";
import { encodeMidgardCekCardanoFlatProgram } from "../src/cek-program.cardano-flat.js";
import { evaluateMidgardCekScript } from "../src/phase-b.evaluate-cek-script.js";
import type { MidgardUplcEvalProgram } from "../src/plutus-script-evaluator.js";
import { UPLCConst } from "../src/uplc-constant.js";
import { type Builtin, UPLCProgram, type UPLCTerm } from "../src/uplc-term.js";
import {
  app,
  discard,
  lam,
  MIDGARD_CEK_TX_BUDGET,
} from "./helpers/cek-uplc-terms.js";
import { builtin } from "./helpers/uplc-builtins.js";

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
    encodeMidgardCekCardanoFlatProgram(
      new UPLCProgram([1, 1, 0], lam(discard(body))),
    ),
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

const ALL_DIVISIONS: readonly (readonly [string, Builtin, bigint])[] = [
  ["divideInteger", builtin("divideInteger"), 3n],
  ["quotientInteger", builtin("quotientInteger"), 4n],
  ["remainderInteger", builtin("remainderInteger"), 5n],
  ["modInteger", builtin("modInteger"), 6n],
];

/** The failing step of the proof run of `body`, which must end in failure. */
const failureStep = (body: UPLCTerm) => {
  const proof = run(body, "proof");
  expect(proof.result.kind).toBe("script_invalid");
  const steps = proof.execution?.steps ?? [];
  const last = steps.at(-1);
  expect(last?.witness.kind).toBe("executeBuiltinFailure");
  expect(last?.post.mode).toBe("haltError");
  return last!.witness as Extract<
    NonNullable<typeof last>["witness"],
    { readonly kind: "executeBuiltinFailure" }
  >;
};

describe("division by zero", () => {
  it.each(ALL_DIVISIONS)(
    "accepts %s by zero of a dividend presented by its leaf",
    (_, __, tag) => {
      const dividend = referenceConstantToDirectWitness(
        UPLCConst.int(-wide(10_000, 5n)),
        true,
      );
      expect(dividend.kind).toBe("semanticConstant");
      const divisor = referenceConstantToDirectWitness(UPLCConst.int(0n));
      const arguments_ = [dividend, divisor];
      const committed = hashMidgardCekDirectArguments(arguments_);
      const builtinRoot = hashMidgardCekValueNode({
        kind: "builtin",
        tag,
        forcesRemaining: 0n,
        argumentsCount: committed.count,
        argumentsRoot: committed.root,
      });
      expect(
        verifyMidgardCekDirectBuiltinFailure(tag, builtinRoot, arguments_),
      ).toBe(true);
      const evaluated = evaluateMidgardCekDirectBuiltin(tag, arguments_);
      expect(evaluated.kind).toBe("failure");
      // Every division by zero is charged before it fails.
      expect(evaluated.budget).toEqual(
        midgardCekDirectBuiltinBudget(tag, arguments_),
      );
      // A divisor presented by its leaf is not a known failure.
      const leafDivisor = [
        dividend,
        semanticizeMidgardCekDirectConstant(divisor),
      ];
      const leafCommitted = hashMidgardCekDirectArguments(leafDivisor);
      expect(
        verifyMidgardCekDirectBuiltinFailure(
          tag,
          hashMidgardCekValueNode({
            kind: "builtin",
            tag,
            forcesRemaining: 0n,
            argumentsCount: leafCommitted.count,
            argumentsRoot: leafCommitted.root,
          }),
          leafDivisor,
        ),
      ).toBe(false);
    },
  );

  it.each(ALL_DIVISIONS)(
    "proves %s of a small dividend by zero with the dividend revealed",
    (_, builtin) => {
      const [dividend] = failureStep(call(builtin, int(7n), int(0n))).arguments;
      expect(dividend?.kind).toBe("constant");
    },
  );

  // An 8,932-byte dividend's payload is 9,215 bytes, the direct bound, so
  // with the zero divisor's the step's arguments are past it; a 16,000-byte
  // one is built at run time from two 8,000-byte constants.
  const PAST_BOUND: readonly (readonly [string, UPLCTerm])[] = [
    ["an 8,932-byte constant", int(-wide(8_932, 5n))],
    [
      "a 16,000-byte product",
      call(
        builtin("multiplyInteger"),
        int(-wide(8_000, 3n)),
        int(wide(8_000, 5n)),
      ),
    ],
  ];

  it.each(
    ALL_DIVISIONS.flatMap(([name, function_]) =>
      PAST_BOUND.map(
        ([label, dividend]) =>
          [`${name} of ${label}`, function_, dividend] as const,
      ),
    ),
  )(
    "proves %s by zero past the direct bound with the dividend by its leaf",
    (_, function_, dividend) => {
      const proof = run(call(function_, dividend, int(0n)), "proof");
      expect(proof.result.kind).toBe("script_invalid");
      const step = proof.execution?.steps.at(-1);
      expect(step?.witness.kind).toBe("executeBuiltinFailure");
      expect(step?.post.mode).toBe("haltError");
      if (step?.witness.kind !== "executeBuiltinFailure") return;
      expect(step.witness.arguments[0]?.kind).toBe("semanticConstant");
      expect(verifyMidgardCekCoreStep(step.pre, step.post, step.witness)).toBe(
        true,
      );
    },
  );

  it.each(
    rows([
      ["divideInteger", builtin("divideInteger")],
      ["quotientInteger", builtin("quotientInteger")],
      ["remainderInteger", builtin("remainderInteger")],
      ["modInteger", builtin("modInteger")],
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
