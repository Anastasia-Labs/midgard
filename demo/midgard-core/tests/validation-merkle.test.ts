import { describe, expect, it } from "vitest";

import {
  appendMidgardValidationMerkleLeafV1,
  buildMidgardValidationMerkleFrontierV1,
  buildMidgardValidationMerkleMembershipV1,
  commitMidgardValidationMerkleFrontierV1,
  emptyMidgardValidationMerkleFrontierV1,
  verifyMidgardValidationMerkleMembershipV1,
} from "../src/index.js";

const hash = (byte: number): Buffer => Buffer.alloc(32, byte);

describe("validation Merkle frontier", () => {
  it("appends compact peaks and proves every leaf", () => {
    const leaves = [hash(0xaa), hash(0xbb), hash(0xcc)];
    const frontier = buildMidgardValidationMerkleFrontierV1(leaves);
    expect(frontier.count).toBe(3);
    expect(frontier.peaks.map((peak) => peak.height)).toEqual([0, 1]);
    expect(
      leaves.every((_, index) =>
        verifyMidgardValidationMerkleMembershipV1(
          buildMidgardValidationMerkleMembershipV1(leaves, index),
        ),
      ),
    ).toBe(true);
    expect(commitMidgardValidationMerkleFrontierV1(frontier)).toHaveLength(32);
    expect(
      commitMidgardValidationMerkleFrontierV1(frontier).toString("hex"),
    ).toBe(
      "f257467b03621e7d54b952ac6be9dd6b965bd9f86da60b367ec62b3eb1118ea0",
    );
  });

  it("rejects a mutated path and malformed frontier", () => {
    const leaves = [hash(0xaa), hash(0xbb)];
    const membership = buildMidgardValidationMerkleMembershipV1(leaves, 0);
    expect(
      verifyMidgardValidationMerkleMembershipV1({
        ...membership,
        siblings: [hash(0xdd)],
      }),
    ).toBe(false);
    expect(() =>
      appendMidgardValidationMerkleLeafV1(
        { count: 0, peaks: [{ height: 0, hash: hash(1) }] },
        hash(2),
      ),
    ).toThrow(/unoccupied peak/u);
  });

  it("keeps count and peak occupancy inside the uint32 envelope", () => {
    const one = appendMidgardValidationMerkleLeafV1(
      emptyMidgardValidationMerkleFrontierV1(),
      hash(1),
    );
    expect(one).toMatchObject({ count: 1, peaks: [{ height: 0 }] });
    expect(() =>
      commitMidgardValidationMerkleFrontierV1({
        count: 0x1_0000_0000,
        peaks: [],
      }),
    ).toThrow(/uint32/u);
  });
});
