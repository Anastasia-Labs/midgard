import { Builtin, constT, UPLCConst, type UPLCTerm } from "@harmoniclabs/uplc";
import { describe, it } from "vitest";

import {
  builtBytes,
  bytesConstant,
} from "./helpers/cek-staged-failure-charge.js";
import {
  bool,
  bytes,
  constant,
  expectStagedFailureProved,
  type Operand,
} from "./helpers/cek-staged-failure-rows.js";
import { call } from "./helpers/cek-staged-route-terms.js";

/**
 * writeBits over an input past the direct carriage bound. Protocol version
 * 11 fails writeBits on an input over 4,096 bytes whatever its indices;
 * the staged failure hop proves it from the input's committed memory alone,
 * uncharged, as the direct failure arm proves it over an inline input.
 */

const WRITE_BITS = 80n;
const LONGEST = 65_536;

const indices = (values: readonly bigint[]): Operand =>
  constant(
    UPLCConst.listOf(constT.int)(
      values.map((value) => UPLCConst.int(value).value as never),
    ) as UPLCTerm,
  );

/** 131,072 bytes: protocol version 11 bounds appendByteString's arguments
 * at 65,536 bytes, so the input appends two halves. */
const twoHalves: Operand = {
  live: call(
    Builtin.appendByteString,
    bytes(LONGEST).live,
    bytes(LONGEST, 2).live,
  ),
  constant: bytesConstant(
    Buffer.concat([builtBytes(LONGEST, 1), builtBytes(LONGEST, 2)]),
  ),
};

const rows: readonly (readonly [
  label: string,
  input: Operand,
  at: readonly bigint[],
])[] = [
  [
    "an inline input of 8,928 bytes at bit 0",
    constant(bytesConstant(builtBytes(8_928))),
    [0n],
  ],
  ["an input of 9,215 bytes at bit 0", bytes(9_215), [0n]],
  ["an input of 9,216 bytes with no index", bytes(9_216), []],
  ["an input of 9,216 bytes past its last bit", bytes(9_216), [73_728n]],
  ["an input of 65,536 bytes at bit 0", bytes(LONGEST), [0n]],
  ["an input of 65,537 bytes at bit 0", bytes(LONGEST + 1), [0n]],
  ["an input of 131,072 bytes at bit 0", twoHalves, [0n]],
];

describe("writeBits over a staged input past 4,096 bytes", () => {
  it.each(rows)(
    "fails on %s, proved uncharged by the staged failure hop",
    (_, input, at) => {
      expectStagedFailureProved(
        WRITE_BITS,
        Builtin.writeBits,
        [input, indices(at), constant(bool(true))],
        "uncharged",
      );
    },
  );
});
