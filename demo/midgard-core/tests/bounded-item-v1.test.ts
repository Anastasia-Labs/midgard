import { describe, expect, it } from "vitest";

import {
  buildMidgardBoundedItemChunkProofV1,
  buildMidgardBoundedItemV1,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
  verifyMidgardBoundedItemChunkProofV1,
} from "../src/bounded-item-v1.js";

describe("bounded item V1", () => {
  it("authenticates every exact bounded chunk of an arbitrarily large item", () => {
    const bytes = Buffer.alloc(
      MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1 * 3 + 17,
      0x5a,
    );
    const item = buildMidgardBoundedItemV1({
      fieldIndex: 2,
      itemIndex: 7,
      bytes,
    });
    expect(item.chunkHashes).toHaveLength(4);
    for (let chunkIndex = 0; chunkIndex < 4; chunkIndex += 1) {
      expect(
        verifyMidgardBoundedItemChunkProofV1({
          expectedCommitment: item.commitment,
          proof: buildMidgardBoundedItemChunkProofV1(item, chunkIndex),
        }),
      ).toBe(true);
    }
  });

  it("fails closed for substitution and field, item, or chunk replay", () => {
    const item = buildMidgardBoundedItemV1({
      fieldIndex: 7,
      itemIndex: 3,
      bytes: Buffer.alloc(5_000, 0x42),
    });
    const proof = buildMidgardBoundedItemChunkProofV1(item, 0);
    for (const invalidProof of [
      { ...proof, fieldIndex: 6 },
      { ...proof, itemIndex: 4 },
      { ...proof, chunkIndex: 1 },
      {
        ...proof,
        chunk: Buffer.concat([
          Buffer.from([proof.chunk[0]! ^ 0xff]),
          proof.chunk.subarray(1),
        ]),
      },
    ]) {
      expect(
        verifyMidgardBoundedItemChunkProofV1({
          expectedCommitment: item.commitment,
          proof: invalidProof,
        }),
      ).toBe(false);
    }
  });

  it("authenticates an empty item with one explicit empty chunk", () => {
    const item = buildMidgardBoundedItemV1({
      fieldIndex: 8,
      itemIndex: 0,
      bytes: Buffer.alloc(0),
    });
    expect(item.frontier.count).toBe(1);
    expect(
      verifyMidgardBoundedItemChunkProofV1({
        expectedCommitment: item.commitment,
        proof: buildMidgardBoundedItemChunkProofV1(item, 0),
      }),
    ).toBe(true);
  });
});
