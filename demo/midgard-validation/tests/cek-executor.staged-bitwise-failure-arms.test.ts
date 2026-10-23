import { CEKConst } from "@harmoniclabs/plutus-machine";
import { Builtin, UPLCConst } from "@harmoniclabs/uplc";
import { describe, expect, it } from "vitest";

import type { MidgardCekDirectValueWitness } from "../src/cek-builtin.argument-kinds.js";
import {
  referenceConstantToDirectWitness,
  semanticizeMidgardCekDirectConstant,
} from "../src/cek-builtin.evaluate-reference-builtin.js";
import type { MidgardCekBuiltinBudget } from "../src/cek-cost.js";
import type { MidgardCekStagedOperand } from "../src/cek-executor.staged-failure.js";
import type { MidgardCekCoreStepWitness } from "../src/cek-machine.js";
import type { MidgardCekStagedOperandSource } from "../src/cek-staged-builtin.material.js";
import {
  builtBytes,
  bytesConstant,
  evaluatorCharge,
  failureVerifies,
} from "./helpers/cek-staged-failure-charge.js";
import {
  bytesOperand,
  integerOperand,
  presented,
} from "./helpers/cek-staged-failure-steps.js";
import { call, int } from "./helpers/cek-staged-route-terms.js";

/**
 * A failing step of a bitwise builtin, verifyEcdsaSecp256k1Signature or
 * integerToByteString has exactly one provable result: where both the
 * direct failure arm and the staged failure hop prove it, they post the same
 * charge, and neither proves it with another.
 */

const READ_BIT = 79n;
const SHIFT = 82n;
const ECDSA = 52n;
const INTEGER_TO_BYTES = 73n;

const LONGEST = 65_536;
const INT64_MAX = 2n ** 63n - 1n;
const INT64_MIN = -(2n ** 63n);

const bool = (value: boolean) => UPLCConst.bool(value);

const bigEndian = (value: Uint8Array): bigint =>
  BigInt(`0x${Buffer.from(value).toString("hex")}`);

const KEY = bytesConstant(Buffer.from([2, ...Array<number>(32).fill(7)]));

const UNCHARGED: MidgardCekBuiltinBudget = { cpu: 0n, memory: 0n };

/** The step's arguments (`leading` of them not operands), the direct failure
 * arm's witness, with the operands `compact` names compacted and the rest
 * inline, and the staged failure hop's, which reads `sources`. */
const witnesses = (
  tag: bigint,
  leading: readonly MidgardCekDirectValueWitness[],
  operands: readonly MidgardCekStagedOperand[],
  sources: readonly MidgardCekStagedOperandSource[],
  compact: readonly number[] = [],
) => {
  const arguments_ = [...leading, ...operands.map(presented)];
  return {
    arguments_,
    direct: {
      kind: "executeBuiltinFailure",
      tag,
      arguments: arguments_.map((argument, index) =>
        compact.includes(index)
          ? semanticizeMidgardCekDirectConstant(argument)
          : argument,
      ),
    } as MidgardCekCoreStepWitness,
    staged: {
      kind: "executeBuiltinStagedFailure",
      tag,
      arguments: arguments_.map(semanticizeMidgardCekDirectConstant),
      material: { runs: { refs: [], operands: [] }, sources },
      stagedOperands: [],
    } as MidgardCekCoreStepWitness,
  };
};

const smallInteger = (value: bigint): MidgardCekStagedOperandSource => ({
  kind: "smallInteger",
  value,
});

const SOME_CHARGE: MidgardCekBuiltinBudget = { cpu: 1n, memory: 1n };

describe("a failing bitwise, ECDSA or integerToByteString step has one provable result", () => {
  const ecdsaCharge = evaluatorCharge(
    call(
      Builtin.verifyEcdsaSecp256k1Signature,
      KEY,
      bytesConstant(builtBytes(32)),
      bytesConstant(Buffer.alloc(63)),
    ),
  );
  const key = Buffer.from([2, ...Array<number>(32).fill(7)]);

  it("verifyEcdsaSecp256k1Signature: both failure arms prove a short key beside an inline message, with the same charge", () => {
    const operands = [
      bytesOperand(key.subarray(1)),
      bytesOperand(builtBytes(4_000)),
      bytesOperand(Buffer.alloc(64, 9)),
    ];
    const { arguments_, direct, staged } = witnesses(
      ECDSA,
      [],
      operands,
      operands.map((operand) => operand.leaf),
    );
    expect(ecdsaCharge.cpu > 0n).toBe(true);
    for (const witness of [direct, staged]) {
      expect(failureVerifies(ECDSA, arguments_, witness, ecdsaCharge)).toBe(
        true,
      );
      expect(failureVerifies(ECDSA, arguments_, witness, UNCHARGED)).toBe(
        false,
      );
    }
  });

  it("verifyEcdsaSecp256k1Signature: only the charged staged failure proves a message past the direct bound", () => {
    const operands = [
      bytesOperand(key),
      bytesOperand(builtBytes(9_216)),
      bytesOperand(Buffer.alloc(64, 9)),
    ];
    const { arguments_, direct, staged } = witnesses(
      ECDSA,
      [],
      operands,
      operands.map((operand) => operand.leaf),
    );
    expect(failureVerifies(ECDSA, arguments_, staged, ecdsaCharge)).toBe(true);
    expect(failureVerifies(ECDSA, arguments_, staged, UNCHARGED)).toBe(false);
    expect(failureVerifies(ECDSA, arguments_, direct, ecdsaCharge)).toBe(false);
  });

  it("verifyEcdsaSecp256k1Signature: the staged failure hop proves each wrong length alone, with the evaluator's charge", () => {
    const shapes = [
      [key.subarray(1), Buffer.alloc(32, 5), Buffer.alloc(64, 9)],
      [key, Buffer.alloc(32, 5), Buffer.alloc(63, 9)],
      [key, Buffer.alloc(31, 5), Buffer.alloc(64, 9)],
    ];
    for (const shape of shapes) {
      const charge = evaluatorCharge(
        call(
          Builtin.verifyEcdsaSecp256k1Signature,
          ...shape.map(bytesConstant),
        ),
      );
      const operands = shape.map(bytesOperand);
      const { arguments_, staged } = witnesses(
        ECDSA,
        [],
        operands,
        operands.map((operand) => operand.leaf),
      );
      expect(failureVerifies(ECDSA, arguments_, staged, charge)).toBe(true);
      expect(failureVerifies(ECDSA, arguments_, staged, UNCHARGED)).toBe(false);
    }
  });

  it("verifyEcdsaSecp256k1Signature: the staged failure hop proves no step with a key, message and signature of the right lengths", () => {
    const operands = [
      bytesOperand(key),
      bytesOperand(builtBytes(32)),
      bytesOperand(Buffer.alloc(64, 9)),
    ];
    const { arguments_, staged } = witnesses(
      ECDSA,
      [],
      operands,
      operands.map((operand) => operand.leaf),
    );
    for (const charge of [ecdsaCharge, UNCHARGED]) {
      expect(failureVerifies(ECDSA, arguments_, staged, charge)).toBe(false);
    }
  });

  const widthArgument = (width: bigint) => presented(integerOperand(width));
  const endianness = referenceConstantToDirectWitness(
    CEKConst.bool(true),
    true,
  );

  it("integerToByteString: both failure arms prove an input of 1,025 words, with the evaluator's charge", () => {
    const input = 2n ** 65_536n;
    const charge = evaluatorCharge(
      call(Builtin.integerToByteString, bool(true), int(0n), int(input)),
    );
    const { arguments_, direct, staged } = witnesses(
      INTEGER_TO_BYTES,
      [endianness, widthArgument(0n)],
      [integerOperand(input)],
      [smallInteger(0n)],
    );
    expect(charge.cpu > 0n).toBe(true);
    for (const witness of [direct, staged]) {
      expect(
        failureVerifies(INTEGER_TO_BYTES, arguments_, witness, charge),
      ).toBe(true);
      expect(
        failureVerifies(INTEGER_TO_BYTES, arguments_, witness, UNCHARGED),
      ).toBe(false);
    }
  });

  it("integerToByteString: no failure arm proves an input of 1,024 words with its width in range", () => {
    const input = 2n ** 65_536n - 1n;
    const { arguments_, direct, staged } = witnesses(
      INTEGER_TO_BYTES,
      [endianness, widthArgument(0n)],
      [integerOperand(input)],
      [smallInteger(0n)],
    );
    for (const witness of [direct, staged]) {
      for (const charge of [SOME_CHARGE, UNCHARGED]) {
        expect(
          failureVerifies(INTEGER_TO_BYTES, arguments_, witness, charge),
        ).toBe(false);
      }
    }
  });

  it("integerToByteString: only the direct failure arm proves an input of 1,024 words past a width in range", () => {
    const input = 2n ** 65_536n - 1n;
    const charge = evaluatorCharge(
      call(Builtin.integerToByteString, bool(true), int(100n), int(input)),
    );
    const { arguments_, direct, staged } = witnesses(
      INTEGER_TO_BYTES,
      [endianness, widthArgument(100n)],
      [integerOperand(input)],
      [smallInteger(100n)],
    );
    expect(failureVerifies(INTEGER_TO_BYTES, arguments_, direct, charge)).toBe(
      true,
    );
    expect(failureVerifies(INTEGER_TO_BYTES, arguments_, staged, charge)).toBe(
      false,
    );
  });

  it("integerToByteString: both failure arms prove a width out of range beside an inline input, uncharged", () => {
    const { arguments_, direct, staged } = witnesses(
      INTEGER_TO_BYTES,
      [endianness, widthArgument(8_193n)],
      [integerOperand(5n)],
      [smallInteger(8_193n)],
    );
    for (const witness of [direct, staged]) {
      expect(
        failureVerifies(INTEGER_TO_BYTES, arguments_, witness, UNCHARGED),
      ).toBe(true);
      expect(
        failureVerifies(INTEGER_TO_BYTES, arguments_, witness, SOME_CHARGE),
      ).toBe(false);
    }
  });

  it("integerToByteString: the staged failure hop proves a width out of range beside an input past 1,024 words only uncharged", () => {
    const { arguments_, staged } = witnesses(
      INTEGER_TO_BYTES,
      [endianness, widthArgument(-1n)],
      [integerOperand(bigEndian(builtBytes(10_000, 1)))],
      [smallInteger(-1n)],
    );
    expect(
      failureVerifies(INTEGER_TO_BYTES, arguments_, staged, UNCHARGED),
    ).toBe(true);
    expect(
      failureVerifies(INTEGER_TO_BYTES, arguments_, staged, SOME_CHARGE),
    ).toBe(false);
  });

  it("integerToByteString: the staged failure hop refuses a width out of range with an extra source", () => {
    const { arguments_, staged } = witnesses(
      INTEGER_TO_BYTES,
      [endianness, widthArgument(8_193n)],
      [integerOperand(bigEndian(builtBytes(10_000, 1)))],
      [smallInteger(8_193n), smallInteger(8_193n)],
    );
    expect(
      failureVerifies(INTEGER_TO_BYTES, arguments_, staged, UNCHARGED),
    ).toBe(false);
  });

  it("integerToByteString: the staged failure hop refuses an endianness or an input of another type", () => {
    const input = 2n ** 65_536n;
    const charge = evaluatorCharge(
      call(Builtin.integerToByteString, bool(true), int(0n), int(input)),
    );
    const cases = [
      {
        leading: [
          referenceConstantToDirectWitness(CEKConst.int(1n), true),
          widthArgument(0n),
        ],
        operand: integerOperand(input),
      },
      {
        leading: [endianness, widthArgument(0n)],
        operand: bytesOperand(builtBytes(8_200)),
      },
    ];
    for (const { leading, operand } of cases) {
      const { arguments_, staged } = witnesses(
        INTEGER_TO_BYTES,
        leading,
        [operand],
        [smallInteger(0n)],
      );
      for (const proved of [charge, UNCHARGED]) {
        expect(
          failureVerifies(INTEGER_TO_BYTES, arguments_, staged, proved),
        ).toBe(false);
      }
    }
  });

  it("readBit: both failure arms prove an index outside the machine integers beside inline bytes, uncharged", () => {
    const operands = [
      bytesOperand(builtBytes(100)),
      integerOperand(INT64_MAX + 1n),
    ];
    const { arguments_, direct, staged } = witnesses(READ_BIT, [], operands, [
      operands[0]!.leaf,
      smallInteger(INT64_MAX + 1n),
    ]);
    for (const witness of [direct, staged]) {
      expect(failureVerifies(READ_BIT, arguments_, witness, UNCHARGED)).toBe(
        true,
      );
      expect(failureVerifies(READ_BIT, arguments_, witness, SOME_CHARGE)).toBe(
        false,
      );
    }
  });

  it("readBit: the staged failure hop refuses an index other than the committed one, or an extra source", () => {
    const committed = [bytesOperand(builtBytes(100)), integerOperand(5n)];
    const other = witnesses(READ_BIT, [], committed, [
      committed[0]!.leaf,
      smallInteger(INT64_MAX + 1n),
    ]);
    expect(
      failureVerifies(READ_BIT, other.arguments_, other.staged, UNCHARGED),
    ).toBe(false);
    const outside = [
      bytesOperand(builtBytes(100)),
      integerOperand(INT64_MAX + 1n),
    ];
    const extra = witnesses(READ_BIT, [], outside, [
      outside[0]!.leaf,
      smallInteger(INT64_MAX + 1n),
      smallInteger(INT64_MAX + 1n),
    ]);
    expect(
      failureVerifies(READ_BIT, extra.arguments_, extra.staged, UNCHARGED),
    ).toBe(false);
  });

  it("readBit: the staged failure hop proves an index past the bits, uncharged, and no failure on an index inside them", () => {
    const bits = BigInt(LONGEST) * 8n;
    for (const [index, fails] of [
      [INT64_MAX, true],
      [INT64_MIN, true],
      [bits, true],
      [-1n, true],
      [bits - 1n, false],
      [0n, false],
    ] as const) {
      const operands = [
        bytesOperand(builtBytes(LONGEST)),
        integerOperand(index),
      ];
      const { arguments_, staged } = witnesses(READ_BIT, [], operands, [
        operands[0]!.leaf,
        smallInteger(index),
      ]);
      expect(failureVerifies(READ_BIT, arguments_, staged, UNCHARGED)).toBe(
        fails,
      );
      expect(failureVerifies(READ_BIT, arguments_, staged, SOME_CHARGE)).toBe(
        false,
      );
    }
  });

  it("shiftByteString: both failure arms prove an amount outside the machine integers beside a compacted input, uncharged", () => {
    const operands = [
      bytesOperand(builtBytes(70_000)),
      integerOperand(INT64_MIN - 1n),
    ];
    const { arguments_, direct, staged } = witnesses(
      SHIFT,
      [],
      operands,
      [operands[0]!.leaf, smallInteger(INT64_MIN - 1n)],
      [0],
    );
    for (const witness of [direct, staged]) {
      expect(failureVerifies(SHIFT, arguments_, witness, UNCHARGED)).toBe(true);
      expect(failureVerifies(SHIFT, arguments_, witness, SOME_CHARGE)).toBe(
        false,
      );
    }
  });

  it("shiftByteString: the direct failure arm proves no failure beside a compacted input of another type", () => {
    const operands = [
      integerOperand(2n ** 65_536n),
      integerOperand(INT64_MAX + 1n),
    ];
    const { arguments_, direct } = witnesses(SHIFT, [], operands, [], [0]);
    expect(failureVerifies(SHIFT, arguments_, direct, UNCHARGED)).toBe(false);
  });

  it("shiftByteString: the staged failure hop proves no failure on an amount in range beside an input past 65,536 bytes", () => {
    for (const amount of [3n, INT64_MAX, INT64_MIN]) {
      const operands = [
        bytesOperand(builtBytes(70_000)),
        integerOperand(amount),
      ];
      const { arguments_, staged } = witnesses(SHIFT, [], operands, [
        operands[0]!.leaf,
        smallInteger(amount),
      ]);
      expect(failureVerifies(SHIFT, arguments_, staged, UNCHARGED)).toBe(false);
    }
  });
});
