import { describe, expect, it } from "vitest";

import {
  appendMidgardValidationMerkleLeaf,
  buildMidgardValidationMerkleFrontier,
  buildMidgardValidationMerkleMembership,
  buildMidgardValidationMerkleMembershipIndex,
  commitMidgardValidationMerkleFrontier,
  emptyMidgardValidationMerkleFrontier,
  verifyMidgardValidationMerkleMembership,
} from "../src/index.js";

const hash = (byte: number): Buffer => Buffer.alloc(32, byte);

describe("validation Merkle frontier", () => {
  it("appends compact peaks and proves every leaf", () => {
    const leaves = [hash(0xaa), hash(0xbb), hash(0xcc)];
    const frontier = buildMidgardValidationMerkleFrontier(leaves);
    expect(frontier.count).toBe(3);
    expect(frontier.peaks.map((peak) => peak.height)).toEqual([0, 1]);
    expect(
      leaves.every((_, index) =>
        verifyMidgardValidationMerkleMembership(
          buildMidgardValidationMerkleMembership(leaves, index),
        ),
      ),
    ).toBe(true);
    expect(commitMidgardValidationMerkleFrontier(frontier)).toHaveLength(32);
    expect(
      commitMidgardValidationMerkleFrontier(frontier).toString("hex"),
    ).toBe("f257467b03621e7d54b952ac6be9dd6b965bd9f86da60b367ec62b3eb1118ea0");
  });

  it("rejects a mutated path and malformed frontier", () => {
    const leaves = [hash(0xaa), hash(0xbb)];
    const membership = buildMidgardValidationMerkleMembership(leaves, 0);
    expect(
      verifyMidgardValidationMerkleMembership({
        ...membership,
        siblings: [hash(0xdd)],
      }),
    ).toBe(false);
    expect(() =>
      appendMidgardValidationMerkleLeaf(
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
      const indexed = buildMidgardValidationMerkleMembershipIndex(leaves);
      expect(indexed.frontier).toStrictEqual(
        buildMidgardValidationMerkleFrontier(leaves),
      );

      if (count === 0) {
        expect(() => indexed.membershipAt(0)).toThrow(
          /leaf index is out of range/u,
        );
        continue;
      }
      for (let leafIndex = 0; leafIndex < count; leafIndex += 1) {
        const expected = buildMidgardValidationMerkleMembership(
          leaves,
          leafIndex,
        );
        const actual = indexed.membershipAt(leafIndex);
        expect(actual).toStrictEqual(expected);
        expect(verifyMidgardValidationMerkleMembership(actual)).toBe(true);
      }
    }
  });

  it("isolates cached paths from mutations to returned buffers", () => {
    const leaves = Array.from({ length: 8 }, (_, index) => hash(index + 1));
    const indexed = buildMidgardValidationMerkleMembershipIndex(leaves);
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

    const expected = buildMidgardValidationMerkleMembership(leaves, 3);
    const subsequent = indexed.membershipAt(3);
    expect(subsequent).toStrictEqual(expected);
    expect(verifyMidgardValidationMerkleMembership(subsequent)).toBe(true);
  });

  it("keeps sibling order consequential and fails closed for malformed requests", () => {
    const leaves = Array.from({ length: 8 }, (_, index) => hash(index + 1));
    const indexed = buildMidgardValidationMerkleMembershipIndex(leaves);
    const membership = indexed.membershipAt(1);
    expect(membership.siblings).toHaveLength(3);
    expect(
      verifyMidgardValidationMerkleMembership({
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
      buildMidgardValidationMerkleMembershipIndex([Buffer.alloc(31)]),
    ).toThrow(/must be 32 bytes/u);
  });

  it("keeps count and peak occupancy inside the uint32 envelope", () => {
    const one = appendMidgardValidationMerkleLeaf(
      emptyMidgardValidationMerkleFrontier(),
      hash(1),
    );
    expect(one).toMatchObject({ count: 1, peaks: [{ height: 0 }] });
    expect(() =>
      commitMidgardValidationMerkleFrontier({
        count: 0x1_0000_0000,
        peaks: [],
      }),
    ).toThrow(/uint32/u);
  });
});
