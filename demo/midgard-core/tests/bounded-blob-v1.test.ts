import { describe, expect, it } from "vitest";

import {
  buildMidgardBoundedBlob,
  buildMidgardBoundedBlobChunkProof,
  MIDGARD_BOUNDED_BLOB_CHUNK_BYTES,
  verifyMidgardBoundedBlobChunkProof,
} from "../src/bounded-blob-v1.js";

describe("bounded blob V1", () => {
  it("authenticates every fixed-size chunk and the exact final remainder", () => {
    const bytes = Buffer.alloc(3 * MIDGARD_BOUNDED_BLOB_CHUNK_BYTES + 17, 0x5a);
    const blob = buildMidgardBoundedBlob({ fieldIndex: 2, bytes });
    expect(blob.chunks.map((chunk) => chunk.length)).toEqual([
      MIDGARD_BOUNDED_BLOB_CHUNK_BYTES,
      MIDGARD_BOUNDED_BLOB_CHUNK_BYTES,
      MIDGARD_BOUNDED_BLOB_CHUNK_BYTES,
      17,
    ]);
    for (let chunkIndex = 0; chunkIndex < blob.chunks.length; chunkIndex += 1) {
      expect(
        verifyMidgardBoundedBlobChunkProof({
          expectedCommitment: blob.commitment,
          proof: buildMidgardBoundedBlobChunkProof(blob, chunkIndex),
        }),
      ).toBe(true);
    }
  });

  it("binds field, index, length, bytes, frontier, and commitment", () => {
    const blob = buildMidgardBoundedBlob({
      fieldIndex: 7,
      bytes: Buffer.alloc(MIDGARD_BOUNDED_BLOB_CHUNK_BYTES + 3, 0x44),
    });
    const proof = buildMidgardBoundedBlobChunkProof(blob, 1);
    const verify = (
      candidate: typeof proof,
      commitment: Uint8Array = blob.commitment,
    ): boolean =>
      verifyMidgardBoundedBlobChunkProof({
        expectedCommitment: commitment,
        proof: candidate,
      });
    expect(verify(proof)).toBe(true);
    expect(verify({ ...proof, fieldIndex: 8 })).toBe(false);
    expect(verify({ ...proof, chunkIndex: 0 })).toBe(false);
    expect(verify({ ...proof, totalLength: proof.totalLength + 1 })).toBe(
      false,
    );
    expect(
      verify({
        ...proof,
        chunk: Buffer.concat([proof.chunk.subarray(0, 2), Buffer.from([0])]),
      }),
    ).toBe(false);
    expect(verify({ ...proof, siblings: [] })).toBe(false);
    expect(verify(proof, Buffer.alloc(32))).toBe(false);
  });

  it("matches the canonical cross-language commitment vector", () => {
    const blob = buildMidgardBoundedBlob({
      fieldIndex: 2,
      bytes: Buffer.concat([
        Buffer.alloc(MIDGARD_BOUNDED_BLOB_CHUNK_BYTES, 0x5a),
        Buffer.from([1, 2, 3]),
      ]),
    });
    expect(blob.leafHashes.map((hash) => hash.toString("hex"))).toEqual([
      "0d56e8ba6350807a8760329ede34888c9c63884515d40ff9a809a40c9122198a",
      "08790d8ac85ae960107a2dcded04a2935110432c13f1ebbbf5a7c9de8eadc290",
    ]);
    expect(blob.commitment.toString("hex")).toBe(
      "61a3c28584495eef1a5c76661f772158a460356921e30d5ffbbdae23d4894959",
    );
  });

  it("commits an empty blob without admitting a fake chunk", () => {
    const blob = buildMidgardBoundedBlob({
      fieldIndex: 0,
      bytes: Buffer.alloc(0),
    });
    expect(blob.chunks).toEqual([]);
    expect(blob.frontier).toEqual({ count: 0, peaks: [] });
    expect(() => buildMidgardBoundedBlobChunkProof(blob, 0)).toThrow();
  });
});
