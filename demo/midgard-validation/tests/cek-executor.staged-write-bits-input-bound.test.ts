import { describe, expect, it } from "vitest";

import {
  referenceConstantToDirectWitness,
  semanticizeMidgardCekDirectConstant,
} from "../src/cek-builtin.evaluate-reference-builtin.js";
import type { MidgardCekCoreStepWitness } from "../src/cek-machine.js";
import { constT, UPLCConst } from "../src/uplc-constant.js";
import type { UPLCTerm } from "../src/uplc-term.js";
import {
  builtBytes,
  bytesConstant,
  failureVerifies,
} from "./helpers/cek-staged-failure-charge.js";
import {
  bool,
  bytes,
  constant,
  expectStagedFailureProved,
  type Operand,
} from "./helpers/cek-staged-failure-rows.js";
import { bytesOperand, presented } from "./helpers/cek-staged-failure-steps.js";
import { call } from "./helpers/cek-staged-route-terms.js";
import { builtin } from "./helpers/uplc-builtins.js";

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
    builtin("appendByteString"),
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
        builtin("writeBits"),
        [input, indices(at), constant(bool(true))],
        "uncharged",
      );
    },
  );
});

describe("the staged failure hop's writeBits input bound", () => {
  it("proves an input of 4,097 bytes uncharged, and refuses one of 4,096", () => {
    const at = referenceConstantToDirectWitness(
      new UPLCConst(constT.listOf(constT.int), [0n]),
      true,
    );
    const flag = referenceConstantToDirectWitness(UPLCConst.bool(true), true);
    for (const [length, fails] of [
      [4_097, true],
      [4_096, false],
    ] as const) {
      const arguments_ = [
        presented(bytesOperand(builtBytes(length))),
        at,
        flag,
      ];
      const witness = {
        kind: "executeBuiltinStagedFailure",
        tag: WRITE_BITS,
        arguments: arguments_.map(semanticizeMidgardCekDirectConstant),
        material: { runs: { refs: [], operands: [] }, sources: [] },
        stagedOperands: [],
      } as MidgardCekCoreStepWitness;
      expect(
        failureVerifies(WRITE_BITS, arguments_, witness, {
          cpu: 0n,
          memory: 0n,
        }),
      ).toBe(fails);
    }
  });
});
