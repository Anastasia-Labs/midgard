import { PLUTUS_V3_CANONICAL_COST_MODEL_VIEW } from "@al-ft/midgard-core";
import * as UPLC from "@lucid-evolution/uplc";
import { describe, expect, it } from "vitest";

import type { MidgardCekDirectValueWitness } from "../src/cek-builtin.argument-kinds.js";
import {
  builtinValueToDirectWitness,
  semanticizeMidgardCekDirectConstant,
} from "../src/cek-builtin.evaluate-direct-builtin.js";
import { midgardCekDirectBuiltinBudget } from "../src/cek-builtin.selected-control-result.js";
import type { MidgardCekBuiltinBudget } from "../src/cek-cost.js";
import type { MidgardCekExecutionStep } from "../src/cek-executor.build-midgard-cek-execution-graph.js";
import type { MidgardCekCoreStepWitness } from "../src/cek-machine.js";
import { errorSuccessor } from "../src/cek-machine.midgard-cek-builtin-argument-count.js";
import { MidgardCekErrorCodes } from "../src/cek-machine.midgard-cek-core-step-witness.js";
import { verifyMidgardCekCoreStep } from "../src/cek-machine.verify-builtin.js";
import { encodeMidgardCekCardanoFlatProgram } from "../src/cek-program.cardano-flat.js";
import {
  midgardCekPlaceholderStaging,
  midgardCekStagedReferenceInputs,
  stageMidgardCekStagedBuiltinMaterial,
} from "../src/cek-staged-builtin.material.js";
import { verifyMidgardCekStagedBuiltin } from "../src/cek-staged-builtin.verify.js";
import type { MidgardUplcEvalProgram } from "../src/plutus-script-evaluator.js";
import { UPLCConst } from "../src/uplc-constant.js";
import { DataB } from "../src/uplc-data.js";
import { UPLCProgram, type UPLCTerm } from "../src/uplc-term.js";
import { builtinState } from "./helpers/cek-staged-failure-steps.js";
import { built, call, run, staged } from "./helpers/cek-staged-route-terms.js";
import {
  app,
  discard,
  lam,
  MIDGARD_CEK_TX_BUDGET,
  seq,
  v,
} from "./helpers/cek-uplc-terms.js";
import { builtin } from "./helpers/uplc-builtins.js";

/**
 * byteStringToInteger over an input past the direct carriage bound.
 * Protocol version 11 bounds the input at 65,536 bytes and fails the builtin
 * on an input past it before costing it. An input within the bound is
 * proved by the staged conversion step (arm 31, semantics group 9) in either
 * byte order; past LONGEST bytes the builtin's charge takes the run past the
 * transaction budget, so the run ends there, as on L1. The proof-mode run
 * reaches the PV11 evaluator's verdict and budget, and each step has exactly
 * one provable result.
 */

const CONVERT = 74n;

/** The longest input whose conversion fits the transaction budget: building
 * and converting 49,784 bytes costs 9,997,732,628 cpu in either byte order,
 * 49,785 bytes 10,000,900,621. */
const LONGEST = 49_784;

const SIZES = [9_215, 9_216, LONGEST, LONGEST + 1, 65_536] as const;

const ORDERS = [
  ["big-endian", true],
  ["little-endian", false],
] as const;

const ROWS = ORDERS.flatMap(([order, bigEndian]) =>
  SIZES.map((size) => [order, size, bigEndian] as const),
);

const conversion = (bigEndian: boolean, input: UPLCTerm): UPLCTerm =>
  call(builtin("byteStringToInteger"), UPLCConst.bool(bigEndian), input);

const steps = (
  outcome: {
    readonly execution: {
      readonly steps: readonly MidgardCekExecutionStep[];
    } | null;
  },
  kind: MidgardCekCoreStepWitness["kind"],
): MidgardCekExecutionStep[] =>
  (outcome.execution?.steps ?? []).filter((step) => step.witness.kind === kind);

const UNCHARGED: MidgardCekBuiltinBudget = { cpu: 0n, memory: 0n };

/** The proof-mode run stopped past the transaction budget at a proved
 * conversion. */
const expectPastBudget = (proof: ReturnType<typeof run>): void => {
  expect(proof.execution?.stopReason).toBe("budgetExceeded");
  expect(proof.execution?.steps.at(-1)?.witness.kind).toBe(
    "executeBuiltinStaged",
  );
  expect(
    proof.result.kind === "accepted" &&
      proof.result.budget.cpu > MIDGARD_CEK_TX_BUDGET.cpu,
  ).toBe(true);
};

const CONTEXT = Buffer.from("d87980", "hex");

/** The PV11 evaluator's start-up charge, spent before the first step. */
const STARTUP = { cpu: 100n, memory: 100n };

/**
 * What the pinned PV11 evaluator charges the failing builtin of `body`,
 * whose arguments are program constants: a run that fails within its first
 * batch of machine steps has spent only the start-up charge and the
 * builtin's.
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

const PIECE = 8_000;

/** The bytes `built(length, salt)` builds at run time. */
const builtBytes = (length: number, salt = 0): Buffer =>
  Buffer.concat(
    Array.from({ length: Math.ceil(length / PIECE) }, (_, piece) =>
      Uint8Array.from(
        { length: Math.min(PIECE, length - piece * PIECE) },
        (_, index) => (index * 7 + salt + piece) & 0xff,
      ),
    ),
  );

const lastConversion = (outcome: Parameters<typeof steps>[0]) => {
  const step = steps(outcome, "executeBuiltinStaged").at(-1);
  if (step?.witness.kind !== "executeBuiltinStaged") return null;
  expect(step.witness.tag).toBe(CONVERT);
  return step;
};

describe("byteStringToInteger over a staged input", () => {
  it.each(ROWS)(
    "%s over %i bytes reaches the PV11 evaluator's verdict and budget",
    (_, size, bigEndian) => {
      const body = conversion(bigEndian, built(size, 3));
      const verdict = run(body, "verdict");
      const proof = run(body, "proof");
      if (size <= LONGEST) {
        // The run accepts with the evaluator's budget.
        expect(verdict.result.kind).toBe("accepted");
        expect(proof.result).toEqual(verdict.result);
      } else {
        // From 49,785 bytes the evaluator ends the run past the budget, as
        // on L1; the proof-mode run stops at the step that takes it past,
        // its total over the budget.
        expect(verdict.result.kind).toBe("script_invalid");
        expectPastBudget(proof);
      }
      // Every staged step verifies, the last of them the conversion, which
      // is proved whether or not its charge fits the budget.
      expect(lastConversion(proof)).not.toBeNull();
      for (const step of steps(proof, "executeBuiltinStaged")) {
        expect(
          verifyMidgardCekCoreStep(step.pre, step.post, staged(step, 9)),
        ).toBe(true);
      }
    },
  );

  it("converts one input in both byte orders, little-endian first", () => {
    // Converting little-endian first leaves the input as it was.
    const body = app(
      lam(seq(conversion(true, v(0)), conversion(false, v(0)))),
      built(9_216, 3),
    );
    const proof = run(body, "proof");
    expect(proof.result).toEqual(run(body, "verdict").result);
    const stagedSteps = steps(proof, "executeBuiltinStaged");
    expect(
      stagedSteps.filter(
        ({ witness }) => "tag" in witness && witness.tag === CONVERT,
      ),
    ).toHaveLength(2);
    for (const step of stagedSteps) {
      expect(
        verifyMidgardCekCoreStep(step.pre, step.post, staged(step, 9)),
      ).toBe(true);
    }
  });

  it("keeps an input whose result fits on the direct route", () => {
    const proof = run(conversion(false, built(1_000, 3)), "proof");
    expect(proof.result).toEqual(
      run(conversion(false, built(1_000, 3)), "verdict").result,
    );
    expect(steps(proof, "executeBuiltinStaged")).toEqual([]);
  });

  it.each(ORDERS)(
    "%s over 65,537 bytes fails uncharged in the staged failure hop",
    (_, bigEndian) => {
      const body = conversion(bigEndian, built(65_537, 3));
      expect(run(body, "verdict").result.kind).toBe("script_invalid");
      const proof = run(body, "proof");
      expect(proof.result.kind).toBe("script_invalid");
      const last = proof.execution!.steps.at(-1)!;
      expect(last.witness.kind).toBe("executeBuiltinStagedFailure");
      expect("tag" in last.witness ? last.witness.tag : null).toBe(CONVERT);
      expect([last.post.cpu, last.post.memory]).toEqual([
        last.pre.cpu,
        last.pre.memory,
      ]);
      expect(
        evaluatorCharge(
          conversion(
            bigEndian,
            UPLCConst.byteString(new DataB(builtBytes(65_537, 3)).bytes),
          ),
        ),
      ).toEqual(UNCHARGED);
      expect(
        verifyMidgardCekCoreStep(last.pre, last.post, staged(last, 9)),
      ).toBe(true);
    },
  );
});

const presented = (value: UPLCConst): MidgardCekDirectValueWitness =>
  builtinValueToDirectWitness(value, true);

/** The failure witnesses for byteStringToInteger over `input`: the staged
 * failure hop's, reading the input's length from its committed argument,
 * and the direct failure arm's, carrying it inline. */
const failureWitnesses = (bigEndian: boolean, input: Buffer) => {
  const arguments_ = [
    presented(UPLCConst.bool(bigEndian)),
    presented(UPLCConst.byteString(input)),
  ];
  const committed = arguments_.map(semanticizeMidgardCekDirectConstant);
  return {
    arguments_,
    charge: midgardCekDirectBuiltinBudget(CONVERT, committed),
    staged: {
      kind: "executeBuiltinStagedFailure",
      tag: CONVERT,
      arguments: committed,
      material: { runs: { refs: [], operands: [] }, sources: [] },
      stagedOperands: [],
    } as MidgardCekCoreStepWitness,
    direct: {
      kind: "executeBuiltinFailure",
      tag: CONVERT,
      arguments: arguments_,
    } as MidgardCekCoreStepWitness,
  };
};

const failureVerifies = (
  arguments_: readonly MidgardCekDirectValueWitness[],
  witness: MidgardCekCoreStepWitness,
  charge: MidgardCekBuiltinBudget,
): boolean => {
  const pre = builtinState(CONVERT, arguments_);
  return verifyMidgardCekCoreStep(
    pre,
    errorSuccessor(pre, MidgardCekErrorCodes.BuiltinFailure, charge),
    witness,
  );
};

describe("a byteStringToInteger step has one provable result", () => {
  it.each(ORDERS)(
    "%s: no failure arm proves an input of 65,536 bytes",
    (_, bigEndian) => {
      const {
        arguments_,
        charge,
        staged: hop,
        direct,
      } = failureWitnesses(bigEndian, builtBytes(65_536, 3));
      for (const budget of [charge, UNCHARGED]) {
        expect(failureVerifies(arguments_, hop, budget)).toBe(false);
        expect(failureVerifies(arguments_, direct, budget)).toBe(false);
      }
    },
  );

  it.each(ORDERS)(
    "%s: only the uncharged staged failure proves an input of 65,537 bytes",
    (_, bigEndian) => {
      const {
        arguments_,
        charge,
        staged: hop,
        direct,
      } = failureWitnesses(bigEndian, builtBytes(65_537, 3));
      expect(failureVerifies(arguments_, hop, UNCHARGED)).toBe(true);
      expect(failureVerifies(arguments_, hop, charge)).toBe(false);
      expect(failureVerifies(arguments_, direct, UNCHARGED)).toBe(false);
      expect(failureVerifies(arguments_, direct, charge)).toBe(false);
    },
  );

  /** Whether the staged conversion step accepts `result` for `input`,
   * staged on outputs of seed 9. */
  const conversionVerifies = (
    bigEndian: boolean,
    input: Buffer,
    result: UPLCConst,
  ): boolean => {
    const material = stageMidgardCekStagedBuiltinMaterial(
      {
        runs: { refs: [], operands: [] },
        sources: [{ kind: "staged", operand: 0 }],
      },
      [input],
      midgardCekPlaceholderStaging([input]),
    );
    return verifyMidgardCekStagedBuiltin(
      CONVERT,
      [
        presented(UPLCConst.bool(bigEndian)),
        presented(UPLCConst.byteString(input)),
      ],
      presented(result),
      material,
      midgardCekStagedReferenceInputs(material, [input]),
    );
  };

  it.each(ORDERS)(
    "%s: the conversion step proves an input of 65,536 bytes, and not one of 65,537",
    (_, bigEndian) => {
      const input = builtBytes(65_536, 3);
      const value = (bytes: Buffer) =>
        BigInt(
          `0x${(bigEndian ? bytes : Buffer.from(bytes).reverse()).toString("hex")}`,
        );
      expect(
        conversionVerifies(bigEndian, input, UPLCConst.int(value(input))),
      ).toBe(true);
      const longer = Buffer.concat([input, Buffer.from([1])]);
      expect(
        conversionVerifies(bigEndian, longer, UPLCConst.int(value(longer))),
      ).toBe(false);
    },
  );

  it("refuses the other byte order's integer and a negative one", () => {
    const input = builtBytes(9_216, 3);
    const bigEndian = BigInt(`0x${input.toString("hex")}`);
    const littleEndian = BigInt(
      `0x${Buffer.from(input).reverse().toString("hex")}`,
    );
    expect(conversionVerifies(true, input, UPLCConst.int(bigEndian))).toBe(
      true,
    );
    expect(conversionVerifies(false, input, UPLCConst.int(littleEndian))).toBe(
      true,
    );
    expect(conversionVerifies(false, input, UPLCConst.int(bigEndian))).toBe(
      false,
    );
    expect(conversionVerifies(true, input, UPLCConst.int(-bigEndian))).toBe(
      false,
    );
  });

  it("binds the conversion to its input's staged bytes", () => {
    const step = lastConversion(
      run(conversion(true, built(20_000, 3)), "proof"),
    )!;
    const witness = staged(step, 9);
    const operand = Buffer.from(witness.stagedOperands[0]!);
    operand[operand.length - 1]! ^= 1;
    expect(
      verifyMidgardCekCoreStep(step.pre, step.post, {
        ...witness,
        stagedOperands: [operand],
      }),
    ).toBe(false);
  });
});
