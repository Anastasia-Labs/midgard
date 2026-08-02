import { describe, expect, it } from "vitest";

import {
  appendMidgardValidationMerkleLeafV1,
  buildMidgardValidationMerkleFrontierV1,
  buildMidgardValidationMerkleMembershipIndexV1,
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
    ).toBe("f257467b03621e7d54b952ac6be9dd6b965bd9f86da60b367ec62b3eb1118ea0");
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

  it("precomputes exactly the existing paths for every size from 0 through 65", () => {
    for (let count = 0; count <= 65; count += 1) {
      const leaves = Array.from({ length: count }, (_, index) =>
        hash((index * 37 + count) % 256),
      );
      const indexed = buildMidgardValidationMerkleMembershipIndexV1(leaves);
      expect(indexed.frontier).toStrictEqual(
        buildMidgardValidationMerkleFrontierV1(leaves),
      );

      if (count === 0) {
        expect(() => indexed.membershipAt(0)).toThrow(
          /leaf index is out of range/u,
        );
        continue;
      }
      for (let leafIndex = 0; leafIndex < count; leafIndex += 1) {
        const expected = buildMidgardValidationMerkleMembershipV1(
          leaves,
          leafIndex,
        );
        const actual = indexed.membershipAt(leafIndex);
        expect(actual).toStrictEqual(expected);
        expect(verifyMidgardValidationMerkleMembershipV1(actual)).toBe(true);
      }
    }
  });

  it("isolates cached paths from mutations to returned buffers", () => {
    const leaves = Array.from({ length: 8 }, (_, index) => hash(index + 1));
    const indexed = buildMidgardValidationMerkleMembershipIndexV1(leaves);
    const corrupted = indexed.membershipAt(3);
    corrupted.leafHash.fill(0xa1);
    for (const sibling of corrupted.siblings) {
      sibling.fill(0xb2);
    }
    for (const peak of corrupted.frontier.peaks) {
      peak.hash.fill(0xc3);
    }
    for (const peak of indexed.frontier.peaks) {
      peak.hash.fill(0xd4);
    }

    const expected = buildMidgardValidationMerkleMembershipV1(leaves, 3);
    const subsequent = indexed.membershipAt(3);
    expect(subsequent).toStrictEqual(expected);
    expect(verifyMidgardValidationMerkleMembershipV1(subsequent)).toBe(true);
  });

  it("keeps sibling order consequential and fails closed for malformed requests", () => {
    const leaves = Array.from({ length: 8 }, (_, index) => hash(index + 1));
    const indexed = buildMidgardValidationMerkleMembershipIndexV1(leaves);
    const membership = indexed.membershipAt(1);
    expect(membership.siblings).toHaveLength(3);
    expect(
      verifyMidgardValidationMerkleMembershipV1({
        ...membership,
        siblings: [...membership.siblings].reverse(),
      }),
    ).toBe(false);
    for (const leafIndex of [-1, 0.5, leaves.length, Number.NaN]) {
      expect(() => indexed.membershipAt(leafIndex)).toThrow(
        /leaf index is out of range/u,
      );
    }
    expect(() =>
      buildMidgardValidationMerkleMembershipIndexV1([Buffer.alloc(31)]),
    ).toThrow(/must be 32 bytes/u);
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
