import {
  encodeCbor,
  encodeMidgardCekCanonicalIntegerCbor,
  midgardCekCanonicalIntegerOf,
} from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import type { MidgardCekExecutionStep } from "../src/cek-executor.build-midgard-cek-execution-graph.js";
import { verifyMidgardCekCoreStep } from "../src/cek-machine.verify-builtin.js";
import { encodeMidgardCekCardanoFlatProgram } from "../src/cek-program.cardano-flat.js";
import {
  type MidgardCekStagedOperandSource,
  stageMidgardCekStagedBuiltinMaterial,
} from "../src/cek-staged-builtin.material.js";
import { evaluateMidgardCekScript } from "../src/phase-b.evaluate-cek-script.js";
import { UPLCConst } from "../src/uplc-constant.js";
import { type Builtin, UPLCProgram, type UPLCTerm } from "../src/uplc-term.js";
import {
  app,
  discard,
  lam,
  MIDGARD_CEK_TX_BUDGET,
  v,
} from "./helpers/cek-uplc-terms.js";
import { builtin } from "./helpers/uplc-builtins.js";

/**
 * multiplyInteger and the divisions whose arguments or result do not fit the
 * direct carriage bound are proved by a staged step (arm 31, semantics group
 * 9). The step carries a source per argument and the CBOR the staged product
 * loop compares: the result's for multiplyInteger, the quotient's for
 * quotientInteger and divideInteger, and the quotient's and the result's for
 * remainderInteger and modInteger. The proof-mode run reaches the PV11
 * evaluator's verdict, each staged step verifies once its operands are
 * staged, and a step with another quotient or result does not.
 */

const int = (value: bigint): UPLCTerm => UPLCConst.int(value);
const call = (builtin: Builtin, ...args: readonly UPLCTerm[]): UPLCTerm =>
  app(builtin as never, ...args);

/** An integer of `size` magnitude bytes. */
const wide = (size: number, low = 0n): bigint =>
  (1n << BigInt(8 * size - 1)) + low;

const script = (body: UPLCTerm): Buffer =>
  encodeCbor(
    Buffer.from(
      encodeMidgardCekCardanoFlatProgram(
        new UPLCProgram([1, 1, 0], lam(discard(body))),
      ),
    ),
  );

const run = (body: UPLCTerm, mode: "proof" | "verdict") =>
  evaluateMidgardCekScript(
    {
      scriptBytes: script(body),
      scriptLanguage: "PlutusV3",
      contextCbor: Buffer.from("d87980", "hex"),
      executionIndex: 0n,
      maxSteps: 10_000,
      executionBudget: MIDGARD_CEK_TX_BUDGET,
    },
    mode,
  );

const multiply = (left: UPLCTerm, right: UPLCTerm): UPLCTerm =>
  call(builtin("multiplyInteger"), left, right);

/** A 16,000-byte integer, built at run time from two 8,000-byte constants. */
const large = (negative: boolean): UPLCTerm =>
  multiply(
    int(negative ? -wide(8_000, 3n) : wide(8_000, 3n)),
    int(wide(8_000, 5n)),
  );

/** 2^262143, one past the largest integer argument, built at run time. */
const pastTop = multiply(
  multiply(int(1n << 65_536n), int(1n << 65_536n)),
  multiply(int(1n << 65_536n), int(1n << 65_535n)),
);
/** 2^262142, built at run time. */
const half = multiply(
  multiply(int(1n << 65_536n), int(1n << 65_536n)),
  multiply(int(1n << 65_536n), int(1n << 65_534n)),
);

/**
 * `body` applied to the largest integer argument, 2^262143 - 1, built once
 * at run time from arguments in range: building it costs about a third of a
 * transaction's CPU budget, and its square alone costs most of one, so the
 * largest integers are multiplied by an 8,192-byte one.
 */
const withTop = (body: (top: UPLCTerm) => UPLCTerm): UPLCTerm =>
  app(
    lam(body(v(0))),
    app(
      lam(
        call(
          builtin("addInteger"),
          call(builtin("subtractInteger"), v(0), int(1n)),
          v(0),
        ),
      ),
      half,
    ),
  );

/** The smallest integer argument, -2^262143, from the largest. */
const bottomOf = (top: UPLCTerm): UPLCTerm =>
  call(builtin("subtractInteger"), multiply(top, int(-1n)), int(1n));

/** An 8,192-byte integer, 2^65535. */
const wideConstant = int(1n << 65_535n);

const DIVISIONS: readonly (readonly [string, Builtin, bigint])[] = [
  ["divideInteger", builtin("divideInteger"), 3n],
  ["quotientInteger", builtin("quotientInteger"), 4n],
  ["remainderInteger", builtin("remainderInteger"), 5n],
  ["modInteger", builtin("modInteger"), 6n],
];

const SIGNS: readonly (readonly [string, boolean, boolean])[] = [
  ["positive by positive", false, false],
  ["negative by positive", true, false],
  ["positive by negative", false, true],
  ["negative by negative", true, true],
];

// Each correct case: its label, the script body, and the tag of the last
// staged step.
const correct: readonly (readonly [string, UPLCTerm, bigint])[] = [
  [
    "multiplyInteger of 8,000-byte integers",
    multiply(int(-wide(8_000, 3n)), int(wide(8_000, 5n))),
    2n,
  ],
  [
    "multiplyInteger whose result passes the direct bound",
    multiply(int(wide(2_000, 1n)), int(-wide(8_000, 7n))),
    2n,
  ],
  [
    "multiplyInteger of the largest integer by an 8,192-byte one",
    withTop((top) => multiply(top, wideConstant)),
    2n,
  ],
  [
    "multiplyInteger of the smallest integer by an 8,192-byte one, past the integer range",
    withTop((top) => multiply(bottomOf(top), wideConstant)),
    2n,
  ],
  ...DIVISIONS.flatMap(([name, builtin, tag]) =>
    SIGNS.map(
      ([signs, dividend, divisor]) =>
        [
          `${name} of 16,000-byte integers by 5,000-byte ones, ${signs}`,
          call(
            builtin,
            large(dividend),
            int(divisor ? -wide(5_000, 11n) : wide(5_000, 11n)),
          ),
          tag,
        ] as const,
    ),
  ),
  [
    "divideInteger of the smallest integer by -1, past the integer range",
    withTop((top) => call(builtin("divideInteger"), bottomOf(top), int(-1n))),
    3n,
  ],
  [
    "quotientInteger of the smallest integer by -1, past the integer range",
    withTop((top) => call(builtin("quotientInteger"), bottomOf(top), int(-1n))),
    4n,
  ],
  [
    "modInteger of the largest integer by the smallest",
    withTop((top) => call(builtin("modInteger"), top, bottomOf(top))),
    6n,
  ],
];

const stagedSteps = (
  outcome: ReturnType<typeof run>,
): MidgardCekExecutionStep[] =>
  (outcome.execution?.steps ?? []).filter(
    (step) => step.witness.kind === "executeBuiltinStaged",
  );

/** The step's witness with its operands staged on outputs of `seed`. */
const staged = (step: MidgardCekExecutionStep, seed: number) => {
  const witness = step.witness;
  if (witness.kind !== "executeBuiltinStaged") throw new Error("not staged");
  let index = 0;
  const staging = witness.stagedOperands.map((operand) =>
    Array.from({ length: Math.ceil(operand.length / 12_285) }, () => ({
      transactionId: Buffer.alloc(32, seed),
      outputIndex: index++,
    })),
  );
  return {
    ...witness,
    material: stageMidgardCekStagedBuiltinMaterial(
      witness.material,
      witness.stagedOperands,
      staging,
    ),
  };
};

const inlineInteger = (value: bigint): MidgardCekStagedOperandSource => ({
  kind: "inline",
  bytes: encodeMidgardCekCanonicalIntegerCbor(
    midgardCekCanonicalIntegerOf(value),
  ),
});

/** The last staged step of `body`'s proof-mode run. */
const lastStaged = (body: UPLCTerm) => {
  const step = stagedSteps(run(body, "proof")).at(-1);
  expect(step).toBeDefined();
  return { step: step!, witness: staged(step!, 9) };
};

/** Whether the step verifies with source `index` replaced by `source`. */
const verifiesWith = (
  { step, witness }: ReturnType<typeof lastStaged>,
  sources: readonly MidgardCekStagedOperandSource[],
): boolean =>
  verifyMidgardCekCoreStep(step.pre, step.post, {
    ...witness,
    material: { ...witness.material, sources },
  });

const replaced = (
  sources: readonly MidgardCekStagedOperandSource[],
  index: number,
  source: MidgardCekStagedOperandSource,
) => sources.map((known, at) => (at === index ? source : known));

describe("multiplyInteger and the divisions past the direct carriage bound", () => {
  it.each(correct)(
    "proves %s with the PV11 evaluator's verdict",
    (_, body, tag) => {
      const verdict = run(body, "verdict");
      const proof = run(body, "proof");
      expect(verdict.result.kind).toBe("accepted");
      expect(proof.result).toEqual(verdict.result);
      const steps = stagedSteps(proof);
      const last = steps.at(-1)?.witness;
      expect(last?.kind === "executeBuiltinStaged" ? last.tag : null).toBe(tag);
      for (const step of steps) {
        expect(
          verifyMidgardCekCoreStep(step.pre, step.post, staged(step, 9)),
        ).toBe(true);
      }
    },
    60_000,
  );

  it.each([
    [2n, 3],
    [3n, 3],
    [4n, 3],
    [5n, 4],
    [6n, 4],
  ] as const)(
    "carries %s's arguments and its loop's CBOR as %s sources",
    (tag, count) => {
      const [, function_] = DIVISIONS.find(([, , known]) => known === tag) ?? [
        "",
        builtin("multiplyInteger"),
      ];
      const { witness } = lastStaged(
        call(function_, large(true), int(wide(5_000, 11n))),
      );
      expect(witness.tag).toBe(tag);
      expect(witness.material.sources).toHaveLength(count);
    },
  );

  it("inlines at most two chunks of bytes in a step and stages the rest", () => {
    // The divisor, the quotient and the remainder (one below the divisor)
    // each fit one chunk; the remainder would take the step past two.
    const divisor = wide(3_950, 9n);
    const { witness } = lastStaged(
      call(
        builtin("modInteger"),
        int(-(divisor * wide(1_350, 5n) + 1n)),
        int(divisor),
      ),
    );
    const { sources } = witness.material;
    expect(sources.map((source) => source.kind)).toEqual([
      "staged",
      "inline",
      "inline",
      "staged",
    ]);
    const inline = sources.flatMap((source) =>
      source.kind === "inline" ? [source.bytes.length] : [],
    );
    expect(inline.reduce((sum, length) => sum + length, 0)).toBeLessThanOrEqual(
      8_190,
    );
  });

  it.each(SIGNS)(
    "refuses a quotient one off the divisions', %s",
    (_, dividend, divisor) => {
      for (const [, builtin, tag] of DIVISIONS) {
        const body = call(
          builtin,
          large(dividend),
          int(divisor ? -wide(5_000, 11n) : wide(5_000, 11n)),
        );
        const found = lastStaged(body);
        const { sources } = found.witness.material;
        expect(verifiesWith(found, sources)).toBe(true);
        const a = (dividend ? -1n : 1n) * wide(8_000, 3n) * wide(8_000, 5n);
        const b = (divisor ? -1n : 1n) * wide(5_000, 11n);
        const floor = a / b - (a % b !== 0n && a < 0n !== b < 0n ? 1n : 0n);
        const quotient = tag === 4n || tag === 5n ? a / b : floor;
        for (const delta of [-1n, 1n]) {
          expect(
            verifiesWith(
              found,
              replaced(sources, 2, inlineInteger(quotient + delta)),
            ),
          ).toBe(false);
        }
      }
    },
  );

  it.each([
    ["multiplyInteger", builtin("multiplyInteger"), 2],
    ["remainderInteger", builtin("remainderInteger"), 3],
    ["modInteger", builtin("modInteger"), 3],
  ] as const)("refuses %s with another result's CBOR", (_, builtin, index) => {
    const found = lastStaged(call(builtin, large(true), int(wide(5_000, 11n))));
    const { sources } = found.witness.material;
    expect(
      verifiesWith(found, replaced(sources, index, inlineInteger(1n))),
    ).toBe(false);
  });

  it("refuses a step missing the result's CBOR or carrying one more source", () => {
    const found = lastStaged(
      call(builtin("modInteger"), large(false), int(wide(5_000, 11n))),
    );
    const { sources } = found.witness.material;
    expect(verifiesWith(found, sources.slice(0, 3))).toBe(false);
    expect(verifiesWith(found, [...sources, sources[3]!])).toBe(false);
  });

  // The staged failure hop proves an integer argument past the bound fails.
  it("proves a multiplyInteger argument one past the largest fails", () => {
    const body = multiply(pastTop, int(2n));
    expect(run(body, "verdict").result.kind).toBe("script_invalid");
    expect(run(body, "proof").result.kind).toBe("script_invalid");
  });
});
