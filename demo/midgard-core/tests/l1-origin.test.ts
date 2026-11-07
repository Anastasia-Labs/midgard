import { describe, expect, it } from "vitest";

import {
  checkL1OriginBeforeHubOracleNonceBlock,
  formatL1Origin,
  L1OriginFormatError,
  parseL1Origin,
  parseL1OriginRecord,
} from "../src/l1-origin.js";

const HASH = "ab".repeat(32);

describe("l1Origin text form", () => {
  it("round-trips <slot>.<hash>", () => {
    const origin = parseL1Origin(`1234.${HASH}`);
    expect(origin).toEqual({ slot: 1234, blockHash: HASH });
    expect(formatL1Origin(origin)).toBe(`1234.${HASH}`);
    expect(parseL1Origin(`0.${HASH}`).slot).toBe(0);
  });

  it.each([
    ["no separator", HASH],
    ["two separators", `1.2.${HASH}`],
    ["leading zero", `01.${HASH}`],
    ["negative slot", `-1.${HASH}`],
    ["unsafe slot", `9007199254740993.${HASH}`],
    ["uppercase hash", `1.${HASH.toUpperCase()}`],
    ["short hash", `1.${"ab".repeat(31)}`],
    ["empty", ""],
  ])("refuses %s, naming the field", (_label, text) => {
    expect(() => parseL1Origin(text, "L1_ORIGIN")).toThrow(L1OriginFormatError);
    expect(() => parseL1Origin(text, "L1_ORIGIN")).toThrow(/^L1_ORIGIN/u);
  });
});

describe("l1Origin record form", () => {
  it("accepts exactly {slot, blockHash}", () => {
    expect(parseL1OriginRecord({ slot: 7, blockHash: HASH })).toEqual({
      slot: 7,
      blockHash: HASH,
    });
  });

  it.each([
    ["an extra key", { slot: 7, blockHash: HASH, height: 1 }],
    ["a missing key", { slot: 7 }],
    ["a string slot", { slot: "7", blockHash: HASH }],
    ["a fractional slot", { slot: 7.5, blockHash: HASH }],
    ["a bad hash", { slot: 7, blockHash: "zz" }],
    ["an array", [7, HASH]],
    ["null", null],
  ])("refuses %s", (_label, value) => {
    expect(() => parseL1OriginRecord(value, "$.l1.origin")).toThrow(
      /^\$\.l1\.origin/u,
    );
  });
});

describe("origin invariant: O lies before the prepareHubOracleNonce block", () => {
  const nonceBlock = { slot: 500, blockHash: "cd".repeat(32) };

  it("holds for an origin before the block", () => {
    expect(
      checkL1OriginBeforeHubOracleNonceBlock(
        { slot: 499, blockHash: HASH },
        nonceBlock,
      ),
    ).toEqual({ ok: true });
  });

  it("fails for the block itself and any later point", () => {
    for (const slot of [500, 501]) {
      const result = checkL1OriginBeforeHubOracleNonceBlock(
        { slot, blockHash: HASH },
        nonceBlock,
      );
      expect(result.ok).toBe(false);
      if (!result.ok)
        expect(result.reason).toMatch(
          /does not lie before the prepareHubOracleNonce block/u,
        );
    }
  });
});
