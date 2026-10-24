import { Builtin } from "@harmoniclabs/uplc";
import { describe, it } from "vitest";

import { bytesConstant } from "./helpers/cek-staged-failure-charge.js";
import {
  bool,
  bytes,
  type Charge,
  constant,
  expectStagedFailureProved,
  integer,
  type Operand,
  small,
} from "./helpers/cek-staged-failure-rows.js";

/**
 * The failures of the bitwise builtins, verifyEcdsaSecp256k1Signature and
 * integerToByteString beside an operand past the direct carriage bound.
 * Protocol version 11 fails the bitwise builtins on a byte string past
 * 65,536 bytes, and readBit, shiftByteString and rotateByteString on an
 * index or amount outside the machine integers, before costing them;
 * verifyEcdsaSecp256k1Signature on a key, message or signature of the wrong
 * length, and integerToByteString on an input past 1,024 words with its
 * width in range, after costing them. A width out of range fails without a
 * charge, as at the direct failure arm. The proof-mode run reaches the PV11
 * evaluator's verdict and charge. The conversion of an input of 65,536
 * bytes has its own file
 * (cek-executor.staged-integer-to-bytes-max-shape.test.ts).
 */

const AND = 75n;
const OR = 76n;
const XOR = 77n;
const COMPLEMENT = 78n;
const READ_BIT = 79n;
const SHIFT = 82n;
const ROTATE = 83n;
const COUNT_SET_BITS = 84n;
const FIND_FIRST_SET_BIT = 85n;
const ECDSA = 52n;
const INTEGER_TO_BYTES = 73n;

const KEY = bytesConstant(Buffer.from([2, ...Array<number>(32).fill(7)]));
const SIGNATURE = bytesConstant(Buffer.alloc(64, 9));

type Row = readonly [
  label: string,
  tag: bigint,
  builtin: Builtin,
  operands: readonly Operand[],
  charge: Charge,
];

const LONGEST = 65_536;
const INT64_MAX = 2n ** 63n - 1n;
const INT64_MIN = -(2n ** 63n);

const rows: readonly Row[] = [
  [
    "andByteString with a left operand of 65,537 bytes",
    AND,
    Builtin.andByteString,
    [constant(bool(true)), bytes(LONGEST + 1), bytes(10, 2)],
    "evaluator",
  ],
  [
    "orByteString with a right operand of 65,537 bytes",
    OR,
    Builtin.orByteString,
    [constant(bool(false)), bytes(10), bytes(LONGEST + 1, 2)],
    "evaluator",
  ],
  [
    "xorByteString over two operands of 65,537 bytes",
    XOR,
    Builtin.xorByteString,
    [constant(bool(true)), bytes(LONGEST + 1), bytes(LONGEST + 1, 2)],
    "evaluator",
  ],
  [
    "complementByteString of 65,537 bytes",
    COMPLEMENT,
    Builtin.complementByteString,
    [bytes(LONGEST + 1)],
    "evaluator",
  ],
  [
    "countSetBits of 65,537 bytes",
    COUNT_SET_BITS,
    Builtin.countSetBits,
    [bytes(LONGEST + 1)],
    "evaluator",
  ],
  [
    "findFirstSetBit of 65,537 bytes",
    FIND_FIRST_SET_BIT,
    Builtin.findFirstSetBit,
    [bytes(LONGEST + 1)],
    "evaluator",
  ],
  [
    "readBit of 65,537 bytes at 0",
    READ_BIT,
    Builtin.readBit,
    [bytes(LONGEST + 1), small(0n)],
    "evaluator",
  ],
  [
    "readBit of 9,216 bytes at 2^63",
    READ_BIT,
    Builtin.readBit,
    [bytes(9_216), small(INT64_MAX + 1n)],
    "evaluator",
  ],
  [
    "readBit of 65,536 bytes at -2^63 - 1",
    READ_BIT,
    Builtin.readBit,
    [bytes(LONGEST), small(INT64_MIN - 1n)],
    "evaluator",
  ],
  [
    "readBit of 65,537 bytes at 2^63",
    READ_BIT,
    Builtin.readBit,
    [bytes(LONGEST + 1), small(INT64_MAX + 1n)],
    "evaluator",
  ],
  [
    "readBit of 10,000 bytes at 2^70",
    READ_BIT,
    Builtin.readBit,
    [bytes(10_000), small(2n ** 70n)],
    "evaluator",
  ],
  [
    "readBit of 100 bytes at an index of 10,000 bytes",
    READ_BIT,
    Builtin.readBit,
    [bytes(100), integer(10_000)],
    "evaluator",
  ],
  [
    "shiftByteString of 9,216 bytes by 2^63",
    SHIFT,
    Builtin.shiftByteString,
    [bytes(9_216), small(INT64_MAX + 1n)],
    "evaluator",
  ],
  [
    "shiftByteString of 70,000 bytes by -2^63 - 1",
    SHIFT,
    Builtin.shiftByteString,
    [bytes(70_000), small(INT64_MIN - 1n)],
    "evaluator",
  ],
  [
    "shiftByteString of 100 bytes by an amount of 10,000 bytes",
    SHIFT,
    Builtin.shiftByteString,
    [bytes(100), integer(10_000)],
    "evaluator",
  ],
  [
    "rotateByteString of 65,537 bytes by 2^64 - 1",
    ROTATE,
    Builtin.rotateByteString,
    [bytes(LONGEST + 1), small(2n ** 64n - 1n)],
    "evaluator",
  ],
  [
    "rotateByteString of 9,216 bytes by -2^70",
    ROTATE,
    Builtin.rotateByteString,
    [bytes(9_216), small(-(2n ** 70n))],
    "evaluator",
  ],
  [
    "rotateByteString of 10,000 bytes by an amount of 10,000 bytes",
    ROTATE,
    Builtin.rotateByteString,
    [bytes(10_000), integer(10_000, true)],
    "evaluator",
  ],
  ...[9_215, 9_216, LONGEST, LONGEST + 1].map(
    (size): Row => [
      `verifyEcdsaSecp256k1Signature over a message of ${size} bytes`,
      ECDSA,
      Builtin.verifyEcdsaSecp256k1Signature,
      [constant(KEY), bytes(size), constant(SIGNATURE)],
      "evaluator",
    ],
  ),
  [
    "verifyEcdsaSecp256k1Signature with a signature of 63 bytes beside a message of 9,216 bytes",
    ECDSA,
    Builtin.verifyEcdsaSecp256k1Signature,
    [constant(KEY), bytes(9_216), constant(bytesConstant(Buffer.alloc(63, 9)))],
    "evaluator",
  ],
  [
    "verifyEcdsaSecp256k1Signature with a key of 65,537 bytes beside a message of 32 bytes",
    ECDSA,
    Builtin.verifyEcdsaSecp256k1Signature,
    [bytes(LONGEST + 1, 5), bytes(32), constant(SIGNATURE)],
    "evaluator",
  ],
  ...(
    [
      [true, 0n, 9_215, false],
      [false, 0n, 9_216, false],
      [true, 8_192n, 10_000, false],
      [false, 100n, 10_000, false],
      [true, 0n, 10_000, true],
      [true, 8_192n, 32_768, false],
    ] as const
  ).map(
    ([bigEndianOrder, width, length, negative]): Row => [
      `integerToByteString of width ${width} over an input of ${length} bytes${negative ? ", negative" : ""}`,
      INTEGER_TO_BYTES,
      Builtin.integerToByteString,
      [constant(bool(bigEndianOrder)), small(width), integer(length, negative)],
      "evaluator",
    ],
  ),
  ...[8_193n, -1n, INT64_MAX + 1n, 2n ** 70n].map(
    (width): Row => [
      `integerToByteString of width ${width} over an input of 10,000 bytes`,
      INTEGER_TO_BYTES,
      Builtin.integerToByteString,
      [constant(bool(true)), small(width), integer(10_000)],
      "uncharged",
    ],
  ),
];

describe("a bitwise, ECDSA or integerToByteString step that fails beside a staged operand", () => {
  it.each(rows)(
    "%s is proved with the PV11 evaluator's verdict and charge",
    (_, tag, builtin, operands, charge) => {
      expectStagedFailureProved(tag, builtin, operands, charge);
    },
  );
});
