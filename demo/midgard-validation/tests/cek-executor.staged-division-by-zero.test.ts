import { PLUTUS_V3_CANONICAL_COST_MODEL_VIEW } from "@al-ft/midgard-core";
import * as UPLC from "@lucid-evolution/uplc";
import { describe, expect, it } from "vitest";

import type { MidgardCekDirectValueWitness } from "../src/cek-builtin.argument-kinds.js";
import {
  evaluateMidgardCekBuiltinOnValues,
  referenceConstantToDirectWitness,
  semanticizeMidgardCekDirectConstant,
} from "../src/cek-builtin.evaluate-reference-builtin.js";
import { midgardCekDirectBuiltinBudget } from "../src/cek-builtin.selected-control-result.js";
import type { MidgardCekBuiltinBudget } from "../src/cek-cost.js";
import {
  midgardCekStagedFailure,
  type MidgardCekStagedOperand,
} from "../src/cek-executor.staged-failure.js";
import type { MidgardCekCoreStepWitness } from "../src/cek-machine.js";
import { errorSuccessor } from "../src/cek-machine.midgard-cek-builtin-argument-count.js";
import { MidgardCekErrorCodes } from "../src/cek-machine.midgard-cek-core-step-witness.js";
import { verifyMidgardCekCoreStep } from "../src/cek-machine.verify-builtin.js";
import { encodeMidgardCekCardanoFlatProgram } from "../src/cek-program.cardano-flat.js";
import type { MidgardUplcEvalProgram } from "../src/plutus-script-evaluator.js";
import { UPLCConst } from "../src/uplc-constant.js";
import { type Builtin, UPLCProgram, type UPLCTerm } from "../src/uplc-term.js";
import {
  builtinState,
  integerOperand,
  presented,
} from "./helpers/cek-staged-failure-steps.js";
import {
  app,
  discard,
  lam,
  MIDGARD_CEK_TX_BUDGET,
} from "./helpers/cek-uplc-terms.js";
import { builtin } from "./helpers/uplc-builtins.js";

/**
 * The four division builtins with a zero divisor and a dividend of 4,095 to
 * 4,097 words. Each such step has exactly one provable result: the direct
 * failure arm (arm 38) proves a dividend under 4,096 words, the staged
 * failure hop (arm 36) one of 4,096 words or more, charging the builtin when
 * the dividend is in [-2^262143, 2^262143 - 1] and nothing when it is not.
 * Each proved charge is what the pinned PV11 evaluator spends on the builtin.
 */

const DIVISIONS: readonly (readonly [string, bigint, Builtin])[] = [
  ["divideInteger", 3n, builtin("divideInteger")],
  ["quotientInteger", 4n, builtin("quotientInteger")],
  ["remainderInteger", 5n, builtin("remainderInteger")],
  ["modInteger", 6n, builtin("modInteger")],
];

const MAX = 2n ** 262_143n - 1n;

/** Dividends, by the arm that proves their division by zero and whether it
 * charges the builtin. */
const DIVIDENDS: readonly (readonly [
  string,
  bigint,
  "direct" | "staged",
  boolean,
])[] = [
  ["4,095 words", 2n ** 262_079n, "direct", true],
  ["4,095 words, negative", -(2n ** 262_079n), "direct", true],
  ["4,096 words, the largest", MAX, "staged", true],
  ["4,096 words, the smallest", -MAX - 1n, "staged", true],
  ["4,096 words, one past the largest", MAX + 1n, "staged", false],
  ["4,096 words, one past the smallest", -MAX - 2n, "staged", false],
  ["4,097 words", 2n ** 262_144n, "staged", false],
];

const ROWS = DIVISIONS.flatMap(([name, tag, builtin]) =>
  DIVIDENDS.map(
    ([label, dividend, arm, charged]) =>
      [`${name} of ${label}`, tag, builtin, dividend, arm, charged] as const,
  ),
);

const ZERO = integerOperand(0n);

const verifies = (
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

/** The direct failure arm's witness: the dividend compacted, as a staged
 * step's argument is, and the divisor revealed. */
const directWitness = (
  tag: bigint,
  dividend: MidgardCekStagedOperand,
): MidgardCekCoreStepWitness => ({
  kind: "executeBuiltinFailure",
  tag,
  arguments: [
    semanticizeMidgardCekDirectConstant(presented(dividend)),
    referenceConstantToDirectWitness(UPLCConst.int(0n)),
  ],
});

/** The staged failure hop's witness revealing the dividend's CBOR. */
const stagedWitness = (
  tag: bigint,
  dividend: MidgardCekStagedOperand,
): MidgardCekCoreStepWitness => ({
  kind: "executeBuiltinStagedFailure",
  tag,
  arguments: [dividend, ZERO]
    .map(presented)
    .map(semanticizeMidgardCekDirectConstant),
  material: {
    runs: { refs: [], operands: [] },
    sources: [{ kind: "staged", operand: 0 }, ZERO.leaf],
  },
  stagedOperands: [dividend.bytes],
});

const UNCHARGED: MidgardCekBuiltinBudget = { cpu: 0n, memory: 0n };

const CONTEXT = Buffer.from("d87980", "hex");

/** The PV11 evaluator's start-up charge, spent before the first step. */
const STARTUP = { cpu: 100n, memory: 100n };

/**
 * What the pinned PV11 evaluator charges the failing builtin of `body`. It
 * charges machine steps in batches, so a run that fails within its first
 * batch has spent only the start-up charge and the builtin's charge.
 */
const evaluatorCharge = (body: UPLCTerm): MidgardCekBuiltinBudget => {
  const evaluate = UPLC.eval_program_raw as MidgardUplcEvalProgram;
  const result = evaluate(
    Buffer.from(
      encodeMidgardCekCardanoFlatProgram(
        new UPLCProgram([1, 1, 0], lam(discard(body))),
      ),
    ),
    [CONTEXT],
    3,
    11,
    BigInt64Array.from(PLUTUS_V3_CANONICAL_COST_MODEL_VIEW.map(BigInt)),
    MIDGARD_CEK_TX_BUDGET.cpu,
    MIDGARD_CEK_TX_BUDGET.memory,
    false,
  );
  try {
    expect(result.ok).toBe(false);
    return {
      cpu: BigInt(result.cpu) - STARTUP.cpu,
      memory: BigInt(result.mem) - STARTUP.memory,
    };
  } finally {
    result.free();
  }
};

describe("a division by zero has one provable result", () => {
  it.each(ROWS)("%s", (_, tag, function_, value, arm, charged) => {
    const dividend = integerOperand(value);
    const arguments_ = [dividend, ZERO].map(presented);
    const charge = charged
      ? midgardCekDirectBuiltinBudget(
          tag,
          arguments_.map(semanticizeMidgardCekDirectConstant),
        )
      : UNCHARGED;
    // The PV11 evaluator fails the builtin and charges what the arm does.
    expect(
      evaluatorCharge(app(function_, UPLCConst.int(value), UPLCConst.int(0n))),
    ).toEqual(charge);
    expect(charged).toBe(charge.cpu > 0n);

    const direct = directWitness(tag, dividend);
    const staged = stagedWitness(tag, dividend);
    expect(verifies(tag, arguments_, direct, charge)).toBe(arm === "direct");
    expect(verifies(tag, arguments_, staged, charge)).toBe(arm === "staged");
    // Neither arm proves the other charge.
    const other = charged
      ? UNCHARGED
      : midgardCekDirectBuiltinBudget(
          tag,
          arguments_.map(semanticizeMidgardCekDirectConstant),
        );
    expect(verifies(tag, arguments_, direct, other)).toBe(false);
    expect(verifies(tag, arguments_, staged, other)).toBe(false);

    // The staged route gives the staged failure hop's dividends their
    // witness and charge.
    const failure = midgardCekStagedFailure(
      tag,
      arguments_,
      [dividend, ZERO],
      () =>
        evaluateMidgardCekBuiltinOnValues(Number(tag), [
          dividend.value,
          ZERO.value,
        ]) === "failure",
    );
    if (arm === "direct") {
      expect(failure).toBeNull();
    } else {
      expect(failure!.budget).toEqual(charge);
      expect(verifies(tag, arguments_, failure!.witness, charge)).toBe(true);
    }
  });

  it("refuses the charged step with the dividend by its leaf alone", () => {
    const dividend = integerOperand(MAX);
    const arguments_ = [dividend, ZERO].map(presented);
    const witness = stagedWitness(3n, dividend);
    if (witness.kind !== "executeBuiltinStagedFailure") throw new Error("kind");
    const charge = midgardCekDirectBuiltinBudget(
      3n,
      arguments_.map(semanticizeMidgardCekDirectConstant),
    );
    expect(verifies(3n, arguments_, witness, charge)).toBe(true);
    const leafOnly: MidgardCekCoreStepWitness = {
      ...witness,
      material: {
        ...witness.material,
        sources: [dividend.leaf, ZERO.leaf],
      },
      stagedOperands: [],
    };
    expect(verifies(3n, arguments_, leafOnly, charge)).toBe(false);
    expect(verifies(3n, arguments_, leafOnly, UNCHARGED)).toBe(false);
  });

  it("refuses the charged step over a divisor other than zero", () => {
    const dividend = integerOperand(MAX);
    const one = integerOperand(1n);
    const arguments_ = [dividend, one].map(presented);
    const witness: MidgardCekCoreStepWitness = {
      kind: "executeBuiltinStagedFailure",
      tag: 3n,
      arguments: arguments_.map(semanticizeMidgardCekDirectConstant),
      material: {
        runs: { refs: [], operands: [] },
        sources: [{ kind: "staged", operand: 0 }, one.leaf],
      },
      stagedOperands: [dividend.bytes],
    };
    const charge = midgardCekDirectBuiltinBudget(
      3n,
      arguments_.map(semanticizeMidgardCekDirectConstant),
    );
    expect(verifies(3n, arguments_, witness, charge)).toBe(false);
    expect(verifies(3n, arguments_, witness, UNCHARGED)).toBe(false);
  });
});
