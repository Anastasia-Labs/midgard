import { Builtin } from "@harmoniclabs/uplc";
import { describe, expect, it } from "vitest";

import {
  bytes,
  type Charge,
  expectStagedFailureProved,
  small,
} from "./helpers/cek-staged-failure-rows.js";
import { call, run } from "./helpers/cek-staged-route-terms.js";

/**
 * readBit beside a byte string past the direct carriage bound, at an index
 * inside the machine integers but outside its bits. Protocol version 11
 * fails it after costing; the staged failure hop proves it uncharged, as the
 * direct failure arm proves the same failure over an inline byte string.
 * Past 65,536 bytes the byte string's bound fails it first, uncharged.
 */

const READ_BIT = 79n;
const LONGEST = 65_536;
const INT64_MAX = 2n ** 63n - 1n;
const INT64_MIN = -(2n ** 63n);

const bits = (length: number): bigint => BigInt(length) * 8n;

const rows: readonly (readonly [
  label: string,
  length: number,
  index: bigint,
  charge: Charge,
])[] = [
  [
    "9,215 bytes at its first index past the end",
    9_215,
    bits(9_215),
    "uncharged",
  ],
  [
    "9,216 bytes at its first index past the end",
    9_216,
    bits(9_216),
    "uncharged",
  ],
  ["9,216 bytes at -1", 9_216, -1n, "uncharged"],
  [
    "65,536 bytes at its first index past the end",
    LONGEST,
    bits(LONGEST),
    "uncharged",
  ],
  ["65,536 bytes at 2^63 - 1", LONGEST, INT64_MAX, "uncharged"],
  ["65,536 bytes at -2^63", LONGEST, INT64_MIN, "uncharged"],
  [
    "65,537 bytes at its first index past the end",
    LONGEST + 1,
    bits(LONGEST + 1),
    "evaluator",
  ],
];

describe("readBit at an index past the bits of a staged byte string", () => {
  it.each(rows)(
    "fails on %s, proved by the staged failure hop",
    (_, length, index, charge) => {
      expectStagedFailureProved(
        READ_BIT,
        Builtin.readBit,
        [bytes(length), small(index)],
        charge,
      );
    },
  );

  it.each([9_216, LONGEST])(
    "reads the last bit of %s bytes in both modes",
    (length) => {
      const body = call(
        Builtin.readBit,
        bytes(length).live,
        small(bits(length) - 1n).live,
      );
      expect(run(body, "verdict").result.kind).toBe("accepted");
      expect(run(body, "proof").result.kind).toBe("accepted");
    },
  );
});
