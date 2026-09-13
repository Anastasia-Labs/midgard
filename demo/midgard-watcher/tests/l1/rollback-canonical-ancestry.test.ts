import { describe, expect, it } from "vitest";

import type { WatcherNormalizedL1Block } from "../../src/l1/l1-adapter.js";
import { unsafeWatcherCanonicalAncestryLinksForTest } from "../../src/l1/rollback-engine.js";

const h32 = (byte: string): string => byte.repeat(64);
const link = (
  hashByte: string,
  parentByte: string,
  blockNo: string,
  slot: string,
) =>
  Object.freeze({
    blockHash: h32(hashByte),
    parentBlockHash: h32(parentByte),
    blockNo,
    slot,
  });
const blockAt = (
  spec: readonly [
    hashByte: string,
    parentByte: string,
    blockNo: string,
    slot: string,
  ],
) =>
  ({
    chainPoint: link(...spec),
  }) as unknown as WatcherNormalizedL1Block;
const finalized = Object.freeze({
  blockHash: h32("a"),
  blockNo: "100",
  slot: "1000",
});

describe("canonical progress ancestry over unrecorded quiet blocks", () => {
  it("accepts the direct child without ancestry", () => {
    expect(
      unsafeWatcherCanonicalAncestryLinksForTest(
        finalized,
        blockAt(["b", "a", "101", "1020"]),
        [],
      ),
    ).toBe(true);
  });

  it("accepts a contiguous quiet stretch between finality and the block", () => {
    expect(
      unsafeWatcherCanonicalAncestryLinksForTest(
        finalized,
        blockAt(["d", "c", "103", "1060"]),
        [link("b", "a", "101", "1020"), link("c", "b", "102", "1040")],
      ),
    ).toBe(true);
  });

  it.each([
    [
      "a parent hash gap",
      [link("b", "9", "101", "1020")],
      ["c", "b", "102", "1040"],
    ],
    [
      "a block number gap",
      [link("b", "a", "102", "1020")],
      ["c", "b", "103", "1040"],
    ],
    [
      "a slot regression",
      [link("b", "a", "101", "1000")],
      ["c", "b", "102", "1040"],
    ],
    [
      "a detached block",
      [link("b", "a", "101", "1020")],
      ["c", "9", "102", "1040"],
    ],
    [
      "a skipped block",
      [link("b", "a", "101", "1020")],
      ["d", "b", "103", "1040"],
    ],
    ["a missing ancestry", [], ["c", "b", "102", "1040"]],
  ] as const)("refuses %s", (_label, ancestry, block) => {
    expect(
      unsafeWatcherCanonicalAncestryLinksForTest(
        finalized,
        blockAt(block),
        ancestry,
      ),
    ).toBe(false);
  });
});
