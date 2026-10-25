import { CEKConst } from "@harmoniclabs/plutus-machine";
import { describe, expect, it } from "vitest";

import type { MidgardCekDirectValueWitness } from "../src/cek-builtin.argument-kinds.js";
import { referenceConstantToDirectWitness } from "../src/cek-builtin.evaluate-reference-builtin.js";
import { midgardCekDirectBuiltinBudget } from "../src/cek-builtin.selected-control-result.js";
import type { MidgardCekExecutionStep } from "../src/cek-executor.build-midgard-cek-execution-graph.js";
import type { MidgardCekCoreStepWitness } from "../src/cek-machine.js";
import { errorSuccessor } from "../src/cek-machine.midgard-cek-builtin-argument-count.js";
import { MidgardCekErrorCodes } from "../src/cek-machine.midgard-cek-core-step-witness.js";
import { verifyMidgardCekCoreStep } from "../src/cek-machine.verify-builtin.js";
import {
  midgardCekPlaceholderStaging,
  type MidgardCekStagedOperandSource,
  midgardCekStagedReferenceInputs,
  stageMidgardCekStagedBuiltinMaterial,
} from "../src/cek-staged-builtin.material.js";
import { verifyMidgardCekStagedBuiltin } from "../src/cek-staged-builtin.verify.js";
import { UPLCConst } from "../src/uplc-constant.js";
import { DataB } from "../src/uplc-data.js";
import type { UPLCTerm } from "../src/uplc-term.js";
import {
  built,
  bytes,
  call,
  int,
  run,
  staged,
} from "./helpers/cek-staged-route-terms.js";
import { builtin } from "./helpers/uplc-builtins.js";

/**
 * The bitwise builtins over operands past the direct carriage bound:
 * andByteString, orByteString, xorByteString, complementByteString, readBit,
 * countSetBits and findFirstSetBit. Protocol version 11 bounds each operand
 * at 65,536 bytes. An operand within the bound, or a result past the
 * aggregate direct payload bound, is proved by the staged bytes step (arm 31,
 * semantics group 7). The proof-mode run reaches the PV11 evaluator's verdict
 * and budget, and each step has exactly one provable result.
 */

const AND = 75n;
const OR = 76n;
const XOR = 77n;
const COMPLEMENT = 78n;
const READ_BIT = 79n;
const COUNT_SET_BITS = 84n;
const FIND_FIRST_SET_BIT = 85n;

const SIZES = [9_215, 9_216, 65_536] as const;

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

const bool = (value: boolean): UPLCTerm => UPLCConst.bool(value);

/** The logical builtin `name` over `left` and `right`, padded or not. */
const logicalTerm = (
  name: "andByteString" | "orByteString" | "xorByteString",
  padding: boolean,
  left: UPLCTerm,
  right: UPLCTerm,
): UPLCTerm => call(builtin(name), bool(padding), left, right);

type Row = readonly [
  name: string,
  tag: bigint,
  body: (size: number) => UPLCTerm,
];

/** Each bitwise builtin over operands of `size` bytes. */
const ROWS: readonly Row[] = [
  [
    "andByteString with padding",
    AND,
    (size) =>
      logicalTerm("andByteString", true, built(size, 1), built(size, 2)),
  ],
  [
    "orByteString without padding",
    OR,
    (size) =>
      logicalTerm("orByteString", false, built(size, 1), built(size, 2)),
  ],
  [
    "xorByteString over a shorter right operand, with padding",
    XOR,
    (size) =>
      logicalTerm("xorByteString", true, built(size, 1), built(size - 9, 2)),
  ],
  [
    "complementByteString",
    COMPLEMENT,
    (size) => call(builtin("complementByteString"), built(size, 1)),
  ],
  [
    "readBit of the first bit",
    READ_BIT,
    (size) => call(builtin("readBit"), built(size, 1), int(0n)),
  ],
  [
    "readBit of the last bit",
    READ_BIT,
    (size) =>
      call(builtin("readBit"), built(size, 1), int(BigInt(8 * size - 1))),
  ],
  [
    "countSetBits",
    COUNT_SET_BITS,
    (size) => call(builtin("countSetBits"), built(size, 1)),
  ],
  [
    "findFirstSetBit",
    FIND_FIRST_SET_BIT,
    (size) => call(builtin("findFirstSetBit"), built(size, 1)),
  ],
];

const stagedSteps = (
  outcome: ReturnType<typeof run>,
  tag: bigint,
): MidgardCekExecutionStep[] =>
  (outcome.execution?.steps ?? []).filter(
    ({ witness }) =>
      witness.kind === "executeBuiltinStaged" && witness.tag === tag,
  );

const UNCHARGED = { cpu: 0n, memory: 0n };

/** Whether `witness` proves that the builtin of `step` fails, charged or
 * not. */
const failureProves = (
  step: MidgardCekExecutionStep,
  witness: MidgardCekCoreStepWitness,
  charge: { readonly cpu: bigint; readonly memory: bigint },
): boolean =>
  verifyMidgardCekCoreStep(
    step.pre,
    errorSuccessor(step.pre, MidgardCekErrorCodes.BuiltinFailure, charge),
    witness,
  );

/** The step proves its result, and no failure arm proves a failure. */
const expectOneProvableResult = (step: MidgardCekExecutionStep): void => {
  const witness = step.witness;
  if (witness.kind !== "executeBuiltinStaged") throw new Error("not staged");
  expect(verifyMidgardCekCoreStep(step.pre, step.post, staged(step, 9))).toBe(
    true,
  );
  const charge = midgardCekDirectBuiltinBudget(witness.tag, witness.arguments);
  const failures: MidgardCekCoreStepWitness[] = [
    {
      kind: "executeBuiltinStagedFailure",
      tag: witness.tag,
      arguments: witness.arguments,
      material: { runs: { refs: [], operands: [] }, sources: [] },
      stagedOperands: [],
    },
    {
      kind: "executeBuiltinFailure",
      tag: witness.tag,
      arguments: witness.arguments,
    },
  ];
  for (const failure of failures) {
    for (const budget of [charge, UNCHARGED]) {
      expect(failureProves(step, failure, budget)).toBe(false);
    }
  }
};

describe("the bitwise builtins over staged operands", () => {
  it.each(
    ROWS.flatMap(([name, tag, body]) =>
      SIZES.map((size) => [name, size, tag, body] as const),
    ),
  )(
    "%s over %i bytes reaches the PV11 evaluator's verdict and budget",
    (_, size, tag, body) => {
      const verdict = run(body(size), "verdict");
      const proof = run(body(size), "proof");
      expect(verdict.result.kind).toBe("accepted");
      expect(proof.result).toEqual(verdict.result);
      const steps = stagedSteps(proof, tag);
      if (size > 9_215) expect(steps.length).toBeGreaterThan(0);
      for (const step of steps) expectOneProvableResult(step);
    },
  );

  it.each([
    [
      "andByteString with padding over 5,000-byte constants",
      AND,
      logicalTerm("andByteString", true, bytes(5_000, 1), bytes(5_000, 2)),
    ],
    [
      "xorByteString without padding over 5,000-byte constants",
      XOR,
      logicalTerm("xorByteString", false, bytes(5_000, 1), bytes(4_999, 2)),
    ],
    [
      "complementByteString over a 4,700-byte constant",
      COMPLEMENT,
      call(builtin("complementByteString"), bytes(4_700, 1)),
    ],
  ] as const)(
    "%s, whose result passes the aggregate direct bound, takes the staged route",
    (_, tag, body) => {
      const verdict = run(body, "verdict");
      const proof = run(body, "proof");
      expect(verdict.result.kind).toBe("accepted");
      expect(proof.result).toEqual(verdict.result);
      const steps = stagedSteps(proof, tag);
      expect(steps).toHaveLength(1);
      expectOneProvableResult(steps[0]!);
    },
  );

  it("keeps operands whose result fits on the direct route", () => {
    const body = call(builtin("complementByteString"), bytes(1_000, 1));
    const proof = run(body, "proof");
    expect(proof.result).toEqual(run(body, "verdict").result);
    expect(stagedSteps(proof, COMPLEMENT)).toEqual([]);
  });

  it("binds the result to the operands' staged bytes", () => {
    const [step] = stagedSteps(
      run(
        logicalTerm("xorByteString", true, built(20_000, 1), built(20_000, 2)),
        "proof",
      ),
      XOR,
    );
    const witness = staged(step!, 9);
    const operand = Buffer.from(witness.stagedOperands[1]!);
    operand[operand.length - 1]! ^= 1;
    expect(
      verifyMidgardCekCoreStep(step!.pre, step!.post, {
        ...witness,
        stagedOperands: [witness.stagedOperands[0]!, operand],
      }),
    ).toBe(false);
  });
});

const presented = (value: CEKConst): MidgardCekDirectValueWitness =>
  referenceConstantToDirectWitness(value, true);

const byteString = (value: Buffer): CEKConst =>
  CEKConst.byteString(new DataB(value).bytes as never);

/** Whether the staged bytes step accepts `result` for builtin `tag` over the
 * committed `leading` arguments, the byte-string `operands` (staged past
 * 4,095 bytes) and the revealed `index`. */
const bitwiseVerifies = (
  tag: bigint,
  leading: readonly CEKConst[],
  operands: readonly Buffer[],
  index: bigint | null,
  result: CEKConst,
): boolean => {
  const stagedOperands = operands.filter((operand) => operand.length > 4_095);
  const sources: MidgardCekStagedOperandSource[] = operands.map((operand) =>
    operand.length > 4_095
      ? { kind: "staged", operand: stagedOperands.indexOf(operand) }
      : { kind: "inline", bytes: operand },
  );
  if (index !== null) sources.push({ kind: "smallInteger", value: index });
  const material = stageMidgardCekStagedBuiltinMaterial(
    { runs: { refs: [], operands: [] }, sources },
    stagedOperands,
    midgardCekPlaceholderStaging(stagedOperands),
  );
  return verifyMidgardCekStagedBuiltin(
    tag,
    [
      ...leading,
      ...operands.map(byteString),
      ...(index === null ? [] : [CEKConst.int(index)]),
    ].map(presented),
    presented(result),
    material,
    midgardCekStagedReferenceInputs(material, stagedOperands),
  );
};

/** andByteString, orByteString or xorByteString on plain bytes: the
 * operands aligned at their first byte, the shorter one's missing bytes
 * taken from the longer with padding and dropped without it. */
const logical = (
  tag: bigint,
  padding: boolean,
  left: Buffer,
  right: Buffer,
): Buffer => {
  const op =
    tag === AND
      ? (a: number, b: number) => a & b
      : tag === OR
        ? (a: number, b: number) => a | b
        : (a: number, b: number) => a ^ b;
  const [short, long] =
    left.length <= right.length ? [left, right] : [right, left];
  const length = padding ? long.length : short.length;
  return Buffer.from(
    Array.from({ length }, (_, i) =>
      i < short.length ? op(left[i]!, right[i]!) : long[i]!,
    ),
  );
};

const bitAt = (operand: Buffer, index: number): boolean =>
  ((operand[operand.length - 1 - Math.floor(index / 8)]! >> index % 8) & 1) ===
  1;

const popCount = (operand: Buffer): bigint =>
  BigInt(
    [...operand].reduce(
      (sum, byte) => sum + byte.toString(2).replaceAll("0", "").length,
      0,
    ),
  );

const firstSetBit = (operand: Buffer): bigint => {
  for (let index = 0; index < operand.length * 8; index++) {
    if (bitAt(operand, index)) return BigInt(index);
  }
  return -1n;
};

describe("the staged bytes step proves the bitwise builtins' results", () => {
  const left = builtBytes(65_536, 1);
  const right = builtBytes(9_216, 2);

  it.each([
    ["andByteString", AND],
    ["orByteString", OR],
    ["xorByteString", XOR],
  ] as const)(
    "%s: proves its result for each padding at the argument bound, and refuses the other padding's",
    (_, tag) => {
      for (const padding of [true, false]) {
        const result = logical(tag, padding, left, right);
        const other = logical(tag, !padding, left, right);
        const pad = CEKConst.bool(padding);
        expect(
          bitwiseVerifies(tag, [pad], [left, right], null, byteString(result)),
        ).toBe(true);
        expect(
          bitwiseVerifies(tag, [pad], [left, right], null, byteString(other)),
        ).toBe(false);
      }
    },
  );

  it("refuses a padding that is not a Boolean", () => {
    const result = logical(AND, true, right, right);
    expect(
      bitwiseVerifies(
        AND,
        [CEKConst.int(1n)],
        [right, right],
        null,
        byteString(result),
      ),
    ).toBe(false);
  });

  it("refuses a logical operand past the argument bound", () => {
    const longer = Buffer.concat([left, Buffer.from([1])]);
    expect(
      bitwiseVerifies(
        XOR,
        [CEKConst.bool(true)],
        [longer, right],
        null,
        byteString(logical(XOR, true, longer, right)),
      ),
    ).toBe(false);
  });

  it("proves a complement at the argument bound, and refuses one byte changed", () => {
    const result = Buffer.from(left.map((byte) => ~byte & 0xff));
    expect(
      bitwiseVerifies(COMPLEMENT, [], [left], null, byteString(result)),
    ).toBe(true);
    result[result.length - 1]! ^= 1;
    expect(
      bitwiseVerifies(COMPLEMENT, [], [left], null, byteString(result)),
    ).toBe(false);
  });

  it("proves the bits read at either end of an operand, and refuses the other bit", () => {
    for (const index of [0, 65_536 * 8 - 1, 12_345]) {
      const bit = bitAt(left, index);
      expect(
        bitwiseVerifies(
          READ_BIT,
          [],
          [left],
          BigInt(index),
          CEKConst.bool(bit),
        ),
      ).toBe(true);
      expect(
        bitwiseVerifies(
          READ_BIT,
          [],
          [left],
          BigInt(index),
          CEKConst.bool(!bit),
        ),
      ).toBe(false);
    }
  });

  it("proves no bit outside the operand", () => {
    for (const index of [-1n, 65_536n * 8n]) {
      for (const bit of [true, false]) {
        expect(
          bitwiseVerifies(READ_BIT, [], [left], index, CEKConst.bool(bit)),
        ).toBe(false);
      }
    }
  });

  it("proves the count of set bits and the first set bit, and refuses others", () => {
    const zeros = Buffer.concat([
      Buffer.alloc(9_000),
      Buffer.from([0x10]),
      Buffer.alloc(300),
    ]);
    for (const operand of [left, zeros, Buffer.alloc(9_216)]) {
      const count = popCount(operand);
      const first = firstSetBit(operand);
      expect(
        bitwiseVerifies(
          COUNT_SET_BITS,
          [],
          [operand],
          null,
          CEKConst.int(count),
        ),
      ).toBe(true);
      expect(
        bitwiseVerifies(
          COUNT_SET_BITS,
          [],
          [operand],
          null,
          CEKConst.int(count + 1n),
        ),
      ).toBe(false);
      expect(
        bitwiseVerifies(
          FIND_FIRST_SET_BIT,
          [],
          [operand],
          null,
          CEKConst.int(first),
        ),
      ).toBe(true);
      expect(
        bitwiseVerifies(
          FIND_FIRST_SET_BIT,
          [],
          [operand],
          null,
          CEKConst.int(first + 1n),
        ),
      ).toBe(false);
    }
  });
});
