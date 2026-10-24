import { CEKConst } from "@harmoniclabs/plutus-machine";
import { Builtin, UPLCConst, type UPLCTerm } from "@harmoniclabs/uplc";
import { describe, expect, it } from "vitest";

import {
  referenceConstantToDirectWitness,
  semanticizeMidgardCekDirectConstant,
} from "../src/cek-builtin.evaluate-reference-builtin.js";
import {
  hashMidgardCekDirectValueWitness,
  midgardCekDirectBuiltinBudget,
} from "../src/cek-builtin.selected-control-result.js";
import type { MidgardCekExecutionStep } from "../src/cek-executor.build-midgard-cek-execution-graph.js";
import type { MidgardCekCoreStepWitness } from "../src/cek-machine.js";
import { verifyMidgardCekCoreStep } from "../src/cek-machine.verify-builtin.js";
import {
  builtBytes,
  failureVerifies,
} from "./helpers/cek-staged-failure-charge.js";
import {
  built,
  call,
  int,
  run,
  staged,
} from "./helpers/cek-staged-route-terms.js";

/**
 * integerToByteString whose input and result pass the aggregate direct
 * carriage bound together. Its input is at most 1,024 words (8,192 bytes)
 * and its width at most 8,192, so each fits the bound alone; from an input
 * of 4,464 bytes at width 0, or 733 bytes at width 8,192, they do not fit
 * it together. The staged conversion step (arm 31, semantics group 9)
 * proves such a success in either byte order, charged as the PV11 evaluator
 * charges it, and each step has exactly one provable result.
 */

const INTEGER_TO_BYTES = 73n;

const ORDERS = [
  ["big-endian", true],
  ["little-endian", false],
] as const;

/** The input bytes: the first is 1, so the integer has `size` bytes. */
const inputBytes = (size: number): Buffer => builtBytes(size, 1);

const magnitude = (size: number): bigint =>
  BigInt(`0x${inputBytes(size).toString("hex")}`);

/** The integer of `size` bytes, built at run time. */
const input = (size: number): UPLCTerm =>
  call(Builtin.byteStringToInteger, UPLCConst.bool(true), built(size, 1));

const body = (bigEndian: boolean, width: number, size: number): UPLCTerm =>
  call(
    Builtin.integerToByteString,
    UPLCConst.bool(bigEndian),
    int(BigInt(width)),
    input(size),
  );

/** What Cardano returns: the input's bytes padded to the width with zeros,
 * at the most significant end. */
const expected = (bigEndian: boolean, width: number, size: number): Buffer => {
  const bytes = Buffer.concat([
    Buffer.alloc(Math.max(width - size, 0)),
    inputBytes(size),
  ]);
  return bigEndian ? bytes : Buffer.from(bytes).reverse();
};

const conversions = (
  outcome: ReturnType<typeof run>,
): MidgardCekExecutionStep[] =>
  (outcome.execution?.steps ?? []).filter(
    ({ witness }) =>
      witness.kind === "executeBuiltinStaged" &&
      witness.tag === INTEGER_TO_BYTES,
  );

/** The success of `body(bigEndian, width, size)`: the PV11 evaluator's
 * verdict and budget, and the staged step that proves it. */
const provedConversion = (
  bigEndian: boolean,
  width: number,
  size: number,
): MidgardCekExecutionStep => {
  const term = body(bigEndian, width, size);
  const verdict = run(term, "verdict");
  const proof = run(term, "proof");
  expect(verdict.result.kind).toBe("accepted");
  expect(proof.result).toEqual(verdict.result);
  const [step] = conversions(proof);
  expect(conversions(proof)).toHaveLength(1);
  const witness = staged(step!, 9);
  expect(witness.kind === "executeBuiltinStaged" && witness.result).toEqual(
    semanticizeMidgardCekDirectConstant(
      referenceConstantToDirectWitness(
        CEKConst.byteString(expected(bigEndian, width, size)),
        true,
      ),
    ),
  );
  expect(verifyMidgardCekCoreStep(step!.pre, step!.post, witness)).toBe(true);
  return step!;
};

const ROWS = ORDERS.flatMap(([order, bigEndian]) =>
  (
    [
      [0, 4_464],
      [0, 8_192],
      [8_192, 733],
      [8_192, 4_096],
      [8_192, 8_192],
    ] as const
  ).map(([width, size]) => [order, width, size, bigEndian] as const),
);

describe("integerToByteString past the aggregate direct bound", () => {
  it.each(ROWS)(
    "%s at width %i over an input of %i bytes is proved with the PV11 evaluator's verdict and budget",
    (_, width, size, bigEndian) => {
      provedConversion(bigEndian, width, size);
    },
  );

  it.each(ORDERS)(
    "%s keeps an input and result that fit the bound together direct",
    (_, bigEndian) => {
      for (const [width, size] of [
        [0, 4_463],
        [8_192, 732],
      ] as const) {
        const term = body(bigEndian, width, size);
        const proof = run(term, "proof");
        expect(proof.result).toEqual(run(term, "verdict").result);
        expect(proof.result.kind).toBe("accepted");
        expect(conversions(proof)).toEqual([]);
      }
    },
  );
});

const UNCHARGED = { cpu: 0n, memory: 0n };

describe("an integerToByteString success has exactly one provable result", () => {
  const bigEndian = false;
  let proved: MidgardCekExecutionStep | undefined;
  /** The honest little-endian step at width 8,192 over 4,096 bytes. */
  const honest = () => {
    proved ??= provedConversion(bigEndian, 8_192, 4_096);
    const witness = staged(proved, 9);
    if (witness.kind !== "executeBuiltinStaged") throw new Error("not staged");
    return { step: proved, witness };
  };

  const withResult = (bytes: Buffer): boolean => {
    const { step, witness } = honest();
    const result = semanticizeMidgardCekDirectConstant(
      referenceConstantToDirectWitness(CEKConst.byteString(bytes), true),
    );
    return verifyMidgardCekCoreStep(
      step.pre,
      { ...step.post, focusRoot: hashMidgardCekDirectValueWitness(result) },
      { ...witness, result },
    );
  };

  it("refuses the other byte order's bytes and a changed byte", () => {
    expect(withResult(expected(!bigEndian, 8_192, 4_096))).toBe(false);
    const changed = expected(bigEndian, 8_192, 4_096);
    changed[0]! ^= 1;
    expect(withResult(changed)).toBe(false);
  });

  it("refuses any other charge", () => {
    const { step, witness } = honest();
    for (const post of [
      { ...step.post, cpu: step.post.cpu + 1n },
      { ...step.post, memory: step.post.memory + 1n },
      { ...step.post, memory: step.post.memory - 1n },
    ]) {
      expect(verifyMidgardCekCoreStep(step.pre, post, witness)).toBe(false);
    }
  });

  it("binds the width to its argument", () => {
    const { step, witness } = honest();
    const result = semanticizeMidgardCekDirectConstant(
      referenceConstantToDirectWitness(
        CEKConst.byteString(expected(bigEndian, 8_191, 4_096)),
        true,
      ),
    );
    expect(
      verifyMidgardCekCoreStep(
        step.pre,
        { ...step.post, focusRoot: hashMidgardCekDirectValueWitness(result) },
        {
          ...witness,
          result,
          material: {
            ...witness.material,
            sources: [
              { kind: "smallInteger", value: 8_191n },
              ...witness.material.sources.slice(1),
            ],
          },
        },
      ),
    ).toBe(false);
  });

  it("binds the step to its input's staged bytes", () => {
    const { step, witness } = honest();
    const operand = Buffer.from(witness.stagedOperands[0]!);
    operand[operand.length - 1]! ^= 1;
    expect(
      verifyMidgardCekCoreStep(step.pre, step.post, {
        ...witness,
        stagedOperands: [operand],
      }),
    ).toBe(false);
  });

  it("is proved by no failure arm", () => {
    const arguments_ = [
      CEKConst.bool(bigEndian),
      CEKConst.int(8_192n),
      CEKConst.int(magnitude(4_096)),
    ].map((value) => referenceConstantToDirectWitness(value, true));
    const committed = arguments_.map(semanticizeMidgardCekDirectConstant);
    const failures: MidgardCekCoreStepWitness[] = [
      {
        kind: "executeBuiltinStagedFailure",
        tag: INTEGER_TO_BYTES,
        arguments: committed,
        material: {
          runs: { refs: [], operands: [] },
          sources: [{ kind: "smallInteger", value: 8_192n }],
        },
        stagedOperands: [],
      },
      {
        kind: "executeBuiltinFailure",
        tag: INTEGER_TO_BYTES,
        arguments: arguments_,
      },
    ] as MidgardCekCoreStepWitness[];
    for (const failure of failures) {
      for (const charge of [
        UNCHARGED,
        midgardCekDirectBuiltinBudget(INTEGER_TO_BYTES, arguments_),
      ]) {
        expect(
          failureVerifies(INTEGER_TO_BYTES, arguments_, failure, charge),
        ).toBe(false);
      }
    }
  });
});
