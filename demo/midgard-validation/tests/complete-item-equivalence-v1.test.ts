import {
  buildMidgardBoundedCollectionItemProofV1,
  buildMidgardBoundedItemChunkProofV1,
  buildMidgardBoundedItemV1,
  encodeMidgardFieldPreimageV1,
  midgardBoundedItemChunkCountV1,
  verifyMidgardBoundedCollectionItemProofV1,
  verifyMidgardBoundedItemChunkProofV1,
} from "@al-ft/midgard-core";
import { MIDGARD_CONSENSUS_LIMITS_V1 } from "@al-ft/midgard-core/consensus-profile-v1";
import { describe, expect, it } from "vitest";

import { countedMachineFieldTraceV1 } from "../src/validation-machine/index.js";

/**
 * §3.2 semantic equivalence for the transaction-field family: the complete
 * canonical item and its bounded chunk representation must authenticate the
 * same canonical commitment, and omission, duplication, reorder,
 * substitution, and trailing data must reject in both representations.
 */

const FIELD_INDEX = 2;

const buildFieldFixture = (itemBytes: number) => {
  // Two items so ordering is observable: the probe item plus a small sibling.
  const probeItem = Buffer.alloc(itemBytes, 0xa7);
  probeItem[0] = 0x42; // any exact bytes; commitments treat them opaquely
  const sibling = Buffer.from("d87980", "hex");
  // The machine's own counted per-item trace, built the way the machine builds
  // it: §5.1's uniform item split folded into a bounded collection. It is not a
  // §4 field commitment — see `countedMachineFieldTraceV1`'s note.
  const collection = countedMachineFieldTraceV1(
    FIELD_INDEX,
    encodeMidgardFieldPreimageV1([probeItem, sibling]),
  );
  expect(collection.items).toHaveLength(2);
  const item = collection.items[0]!;
  expect(item.bytes.length).toBe(itemBytes);
  return { collection, item, probeItem };
};

describe("complete-item versus bounded-chunk semantic equivalence V1", () => {
  it("authenticates the identical canonical commitment through both representations", () => {
    const itemBytes =
      MIDGARD_CONSENSUS_LIMITS_V1.maxSinglePublicationCompleteItemBytes;
    const { collection, item } = buildFieldFixture(itemBytes);

    // Complete representation: the collection proof binds the exact item
    // commitment, recomputable from the complete bytes alone.
    const collectionProof = buildMidgardBoundedCollectionItemProofV1(
      collection,
      0,
    );
    const recomputed = buildMidgardBoundedItemV1({
      fieldIndex: FIELD_INDEX,
      itemIndex: 0,
      bytes: item.bytes,
    });
    expect(recomputed.commitment.equals(collectionProof.itemCommitment)).toBe(
      true,
    );
    expect(
      verifyMidgardBoundedCollectionItemProofV1({
        expectedCommitment: collection.commitment,
        proof: collectionProof,
      }),
    ).toBe(true);

    // Chunked representation: every chunk proof authenticates against the
    // same item commitment — no separate chunk-level commitment exists.
    const chunkCount = midgardBoundedItemChunkCountV1(itemBytes);
    expect(chunkCount).toBe(4);
    const reassembled: Buffer[] = [];
    for (let chunkIndex = 0; chunkIndex < chunkCount; chunkIndex += 1) {
      const chunkProof = buildMidgardBoundedItemChunkProofV1(item, chunkIndex);
      expect(
        verifyMidgardBoundedItemChunkProofV1({
          expectedCommitment: collectionProof.itemCommitment,
          proof: chunkProof,
        }),
      ).toBe(true);
      reassembled.push(chunkProof.chunk);
    }
    // Identical terminal result: complete logical reconstruction from the
    // chunk representation is byte-identical to the complete item.
    expect(Buffer.concat(reassembled).equals(item.bytes)).toBe(true);
  });

  it("rejects omission, duplication, reorder, substitution, and trailing data in both representations", () => {
    const itemBytes = 9_000; // three chunks: 4,095 + 4,095 + 810
    const { collection, item } = buildFieldFixture(itemBytes);
    const collectionProof = buildMidgardBoundedCollectionItemProofV1(
      collection,
      0,
    );
    const commitment = collectionProof.itemCommitment;
    const completeCommitmentMatches = (bytes: Buffer): boolean =>
      buildMidgardBoundedItemV1({
        fieldIndex: FIELD_INDEX,
        itemIndex: 0,
        bytes,
      }).commitment.equals(commitment);

    // Baseline sanity: the exact bytes authenticate.
    expect(completeCommitmentMatches(item.bytes)).toBe(true);

    // Omission.
    expect(
      completeCommitmentMatches(item.bytes.subarray(0, itemBytes - 1)),
    ).toBe(false);
    expect(completeCommitmentMatches(Buffer.alloc(0))).toBe(false);
    const lastChunk = buildMidgardBoundedItemChunkProofV1(item, 2);
    expect(
      verifyMidgardBoundedItemChunkProofV1({
        expectedCommitment: commitment,
        proof: { ...lastChunk, chunk: lastChunk.chunk.subarray(0, 809) },
      }),
    ).toBe(false);

    // Duplication.
    expect(
      completeCommitmentMatches(Buffer.concat([item.bytes, item.bytes])),
    ).toBe(false);
    const firstChunk = buildMidgardBoundedItemChunkProofV1(item, 0);
    expect(
      verifyMidgardBoundedItemChunkProofV1({
        expectedCommitment: commitment,
        proof: { ...firstChunk, chunkIndex: 1 },
      }),
    ).toBe(false);

    // Reorder: second chunk presented at the first position and vice versa.
    const secondChunk = buildMidgardBoundedItemChunkProofV1(item, 1);
    expect(
      verifyMidgardBoundedItemChunkProofV1({
        expectedCommitment: commitment,
        proof: { ...secondChunk, chunkIndex: 0, siblings: firstChunk.siblings },
      }),
    ).toBe(false);
    const reordered = Buffer.concat([
      item.bytes.subarray(4_095, 8_190),
      item.bytes.subarray(0, 4_095),
      item.bytes.subarray(8_190),
    ]);
    expect(completeCommitmentMatches(reordered)).toBe(false);
    // Cross-item reorder: the sibling item cannot claim the probe's slot.
    const siblingProof = buildMidgardBoundedCollectionItemProofV1(
      collection,
      1,
    );
    expect(
      verifyMidgardBoundedCollectionItemProofV1({
        expectedCommitment: collection.commitment,
        proof: {
          ...siblingProof,
          itemIndex: 0,
          siblings: collectionProof.siblings,
        },
      }),
    ).toBe(false);

    // Substitution: one flipped byte, same length.
    const substituted = Buffer.from(item.bytes);
    substituted[5_000] = substituted[5_000]! ^ 0x01;
    expect(completeCommitmentMatches(substituted)).toBe(false);
    const substitutedChunk = Buffer.from(secondChunk.chunk);
    substitutedChunk[905] = substitutedChunk[905]! ^ 0x01;
    expect(
      verifyMidgardBoundedItemChunkProofV1({
        expectedCommitment: commitment,
        proof: { ...secondChunk, chunk: substitutedChunk },
      }),
    ).toBe(false);

    // Trailing data.
    expect(
      completeCommitmentMatches(Buffer.concat([item.bytes, Buffer.from([0])])),
    ).toBe(false);
    expect(
      verifyMidgardBoundedItemChunkProofV1({
        expectedCommitment: commitment,
        proof: {
          ...lastChunk,
          chunk: Buffer.concat([lastChunk.chunk, Buffer.from([0])]),
        },
      }),
    ).toBe(false);
    // Trailing full extra chunk claimed beyond the committed length.
    expect(
      verifyMidgardBoundedItemChunkProofV1({
        expectedCommitment: commitment,
        proof: { ...lastChunk, chunkIndex: 3 },
      }),
    ).toBe(false);
  });
});
