import { describe, expect, it } from "vitest";

import {
  buildMidgardBoundedCollectionItemProofV1,
  buildMidgardBoundedCollectionV1,
  verifyMidgardBoundedCollectionItemProofV1,
} from "../src/bounded-collection-v1.js";

describe("bounded collection V1", () => {
  it("authenticates each exact ordered item", () => {
    const collection = buildMidgardBoundedCollectionV1({
      fieldIndex: 2,
      items: [Buffer.from("a"), Buffer.from("bb"), Buffer.from("ccc")],
    });
    for (let itemIndex = 0; itemIndex < collection.items.length; itemIndex += 1) {
      expect(
        verifyMidgardBoundedCollectionItemProofV1({
          expectedCommitment: collection.commitment,
          proof: buildMidgardBoundedCollectionItemProofV1(
            collection,
            itemIndex,
          ),
        }),
      ).toBe(true);
    }
  });

  it("fails closed for substitution, reordering, and cross-field replay", () => {
    const collection = buildMidgardBoundedCollectionV1({
      fieldIndex: 0,
      items: [Buffer.from("first"), Buffer.from("second")],
    });
    const proof = buildMidgardBoundedCollectionItemProofV1(collection, 0);
    expect(
      verifyMidgardBoundedCollectionItemProofV1({
        expectedCommitment: collection.commitment,
        proof: {
          ...proof,
          itemCommitment: Buffer.alloc(32, 0xff),
        },
      }),
    ).toBe(false);
    expect(
      verifyMidgardBoundedCollectionItemProofV1({
        expectedCommitment: collection.commitment,
        proof: { ...proof, itemLength: proof.itemLength + 1 },
      }),
    ).toBe(false);
    expect(
      verifyMidgardBoundedCollectionItemProofV1({
        expectedCommitment: collection.commitment,
        proof: { ...proof, itemIndex: 1 },
      }),
    ).toBe(false);
    expect(
      verifyMidgardBoundedCollectionItemProofV1({
        expectedCommitment: collection.commitment,
        proof: { ...proof, fieldIndex: 1 },
      }),
    ).toBe(false);
  });

  it("commits an empty field without fabricating an item proof", () => {
    const collection = buildMidgardBoundedCollectionV1({
      fieldIndex: 4,
      items: [],
    });
    expect(collection.frontier.count).toBe(0);
    expect(() =>
      buildMidgardBoundedCollectionItemProofV1(collection, 0),
    ).toThrow(/out of range/u);
  });
});
