import { CEKConst } from "@harmoniclabs/plutus-machine";
import { Builtin } from "@harmoniclabs/uplc";
import { describe, expect, it } from "vitest";

import {
  referenceConstantToDirectWitness,
  semanticizeMidgardCekDirectConstant,
} from "../src/cek-builtin.evaluate-reference-builtin.js";
import type { MidgardCekCoreStepWitness } from "../src/cek-machine.js";
import {
  type Budget,
  builtBytes,
  failureVerifies,
} from "./helpers/cek-staged-failure-charge.js";
import {
  type Charge,
  expectStagedFailureProved,
  integer,
  type Operand,
  small,
} from "./helpers/cek-staged-failure-rows.js";

/**
 * replicateByte beside a count or byte past the direct carriage bound
 * always fails: such an integer is at least 2^64 in magnitude, so it is
 * neither a count in 0..8,192 nor a byte. Protocol version 11 fails a byte
 * outside 0..255 before costing, and a count out of range after costing it,
 * which the direct failure arm leaves uncharged. The proof-mode run reaches
 * the PV11 evaluator's verdict.
 */

const REPLICATE_BYTE = 81n;

type Row = readonly [
  label: string,
  operands: readonly Operand[],
  charge: Charge,
];

const rows: readonly Row[] = [
  ...[9_215, 9_216, 10_000, 32_768].map(
    (length): Row => [
      `a count of ${length} bytes and the byte 0`,
      [integer(length), small(0n)],
      "uncharged",
    ],
  ),
  [
    "a negative count of 10,000 bytes and the byte 255",
    [integer(10_000, true), small(255n)],
    "uncharged",
  ],
  [
    "the count 5 and a byte of 9,216 bytes",
    [small(5n), integer(9_216)],
    "evaluator",
  ],
  [
    "a count of 10,000 bytes and the byte 256",
    [integer(10_000), small(256n)],
    "evaluator",
  ],
  [
    "a count and a byte of 10,000 bytes, the byte negative",
    [integer(10_000), integer(10_000, true)],
    "evaluator",
  ],
];

/** The longest integer protocol version 11 makes: byteStringToInteger's
 * input and multiplyInteger's operands are bounded. Building it at run time
 * passes the transaction budget, so its run has a larger one. */
const LONGEST = 65_536;
const RAISED_BUDGET: Budget = { cpu: 40_000_000_000n, memory: 16_500_000n };

describe("a replicateByte step that fails beside a staged operand", () => {
  it("a count of 65,536 bytes is proved with the PV11 evaluator's verdict", () => {
    expectStagedFailureProved(
      REPLICATE_BYTE,
      Builtin.replicateByte,
      [integer(LONGEST), small(0n)],
      "uncharged",
      RAISED_BUDGET,
    );
  });

  it.each(rows)(
    "%s is proved with the PV11 evaluator's verdict",
    (_, operands, charge) => {
      expectStagedFailureProved(
        REPLICATE_BYTE,
        Builtin.replicateByte,
        operands,
        charge,
      );
    },
  );
});

const UNCHARGED = { cpu: 0n, memory: 0n };
const SOME_CHARGE = { cpu: 1n, memory: 0n };

const presentedInteger = (value: bigint) =>
  referenceConstantToDirectWitness(CEKConst.int(value), true);

describe("a replicateByte failure has exactly one provable result", () => {
  const past = BigInt(`0x${builtBytes(10_000, 1).toString("hex")}`);
  it.each([
    ["a count of two words", 2n ** 64n, 0n, true, true],
    ["a byte of two words", 5n, -(2n ** 64n), true, true],
    ["a count of 10,000 bytes", past, 0n, true, false],
    ["a count of one word past 8,192", 2n ** 63n, 0n, false, true],
    ["a count and a byte in range", 5n, 0n, false, false],
  ] as const)(
    "%s: the staged hop proves it uncharged only past one word",
    (_, count, byte, stagedProves, directProves) => {
      const arguments_ = [presentedInteger(count), presentedInteger(byte)];
      const staged = {
        kind: "executeBuiltinStagedFailure",
        tag: REPLICATE_BYTE,
        arguments: arguments_.map(semanticizeMidgardCekDirectConstant),
        material: { runs: { refs: [], operands: [] }, sources: [] },
        stagedOperands: [],
      } as MidgardCekCoreStepWitness;
      const direct = {
        kind: "executeBuiltinFailure",
        tag: REPLICATE_BYTE,
        arguments: arguments_,
      } as MidgardCekCoreStepWitness;
      for (const [witness, proves] of [
        [staged, stagedProves],
        [direct, directProves],
      ] as const) {
        expect(
          failureVerifies(REPLICATE_BYTE, arguments_, witness, UNCHARGED),
        ).toBe(proves);
        expect(
          failureVerifies(REPLICATE_BYTE, arguments_, witness, SOME_CHARGE),
        ).toBe(false);
      }
    },
  );
});
