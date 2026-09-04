import { describe, expect, it } from "vitest";

import {
  appendMidgardCekBlobFrontierChunk,
  appendMidgardCekBlobFrontierChunkRoot,
  commitMidgardCekBlob,
  emptyMidgardCekBlobFrontier,
  encodeMidgardCekBlobFrontier,
  finalizeMidgardCekBlobFrontier,
  hashMidgardCekBlobChunk,
  MIDGARD_CEK_BLOB_CHUNK_BYTES,
  validateMidgardCekBlobFrontier,
} from "../src/index.js";

const buildFrontier = (bytes: Buffer) => {
  let frontier = emptyMidgardCekBlobFrontier();
  if (bytes.length === 0) {
    return appendMidgardCekBlobFrontierChunk(frontier, Buffer.alloc(0));
  }
  for (
    let offset = 0;
    offset < bytes.length;
    offset += MIDGARD_CEK_BLOB_CHUNK_BYTES
  ) {
    frontier = appendMidgardCekBlobFrontierChunk(
      frontier,
      bytes.subarray(
        offset,
        Math.min(offset + MIDGARD_CEK_BLOB_CHUNK_BYTES, bytes.length),
      ),
    );
  }
  return frontier;
};

describe("streaming CEK blob frontier V1", () => {
  it.each([0, 1, 4_095, 4_096, 8_190, 8_191, 16_384])(
    "reproduces the canonical left-balanced blob root for %i bytes",
    (length) => {
      const bytes = Buffer.alloc(length, 0x6a);
      const frontier = buildFrontier(bytes);
      expect(finalizeMidgardCekBlobFrontier(frontier)).toStrictEqual(
        commitMidgardCekBlob(bytes).root,
      );
      expect(frontier.byteLength).toBe(BigInt(length));
    },
  );

  it("encodes a compact cross-language frontier deterministically", () => {
    const frontier = buildFrontier(Buffer.alloc(8_191, 0x6a));
    expect(encodeMidgardCekBlobFrontier(frontier).toString("hex")).toBe(
      "840103191fff8283005820344b2b0f0e31517cd429e9ed6bc07028defbc437648e4988fbd3f20c64f87d7b01830158203160ec563e32826cc3fc245286437e12ab09796f078e4780a8596b2a09995d8b191ffe",
    );
  });

  it("appends an authenticated chunk root without retaining its bytes", () => {
    const chunk = Buffer.alloc(MIDGARD_CEK_BLOB_CHUNK_BYTES, 0x6a);
    const fromBytes = appendMidgardCekBlobFrontierChunk(
      emptyMidgardCekBlobFrontier(),
      chunk,
    );
    const fromRoot = appendMidgardCekBlobFrontierChunkRoot(
      emptyMidgardCekBlobFrontier(),
      {
        root: hashMidgardCekBlobChunk(chunk),
        byteLength: chunk.length,
      },
    );

    expect(fromRoot).toStrictEqual(fromBytes);
    expect(() =>
      appendMidgardCekBlobFrontierChunkRoot(emptyMidgardCekBlobFrontier(), {
        root: Buffer.alloc(31),
        byteLength: chunk.length,
      }),
    ).toThrow(/32 bytes/u);
  });

  it("fails closed for malformed occupancy and append after a partial leaf", () => {
    const partial = buildFrontier(Buffer.alloc(4_096, 0x6a));
    expect(() =>
      appendMidgardCekBlobFrontierChunk(partial, Buffer.from([0x01])),
    ).toThrow(/final leaf/u);
    expect(() =>
      validateMidgardCekBlobFrontier({
        ...partial,
        count: 4,
      }),
    ).toThrow(/frontier/u);
  });
});
