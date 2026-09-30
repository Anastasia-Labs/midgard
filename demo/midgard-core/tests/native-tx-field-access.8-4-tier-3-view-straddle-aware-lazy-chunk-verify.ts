import { describe, expect, it } from "vitest";

import { MidgardTxCodecError } from "../src/codec/errors.js";
import {
  buildMidgardChunkedFieldView,
  deriveMidgardFieldPreimageCertificate,
  encodeMidgardFieldPreimage,
  MIDGARD_CHUNK_BYTES_K,
  MIDGARD_FIELD_CARRIAGE_CONSTRUCTORS,
  MIDGARD_FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME,
  MIDGARD_FIELD_VIEW_CONSTRUCTORS,
  MIDGARD_HASH28_STRIDE,
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
  MIDGARD_MAX_TIER3_CHUNK_COUNT,
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
  midgardExpectedChunkCount,
  midgardFieldCommitment,
  midgardFieldItemAt,
  midgardFieldItemCount,
  midgardFieldTotalLength,
  selectMidgardFieldCarriageTier,
  splitMidgardFieldPreimageIntoChunks,
} from "../src/codec/native-tx-field-access.js";
import {
  filler,
  hex,
} from "./native-tx-field-access.7-access-invariants-over-a-whole-view.js";

describe("§8 carriage ladder", () => {
  it("partitions the tiers simplest-fitting-first", () => {
    expect(selectMidgardFieldCarriageTier(1)).toBe("Inline");
    expect(
      selectMidgardFieldCarriageTier(MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES),
    ).toBe("Inline");
    expect(
      selectMidgardFieldCarriageTier(
        MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES + 1,
      ),
    ).toBe("RawUtxo");
    expect(selectMidgardFieldCarriageTier(MIDGARD_CHUNK_BYTES_K)).toBe(
      "RawUtxo",
    );
    expect(selectMidgardFieldCarriageTier(MIDGARD_CHUNK_BYTES_K + 1)).toBe(
      "Certified",
    );
    expect(() =>
      selectMidgardFieldCarriageTier(
        MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES + 1,
      ),
    ).toThrow(/aggregate bound/u);
  });

  it("freezes the §8.8 constructor order", () => {
    expect([...MIDGARD_FIELD_CARRIAGE_CONSTRUCTORS]).toEqual([
      "Inline",
      "RawUtxo",
      "Certified",
    ]);
    expect([...MIDGARD_FIELD_VIEW_CONSTRUCTORS]).toEqual([
      "Whole",
      "Chunked",
      "ProvisionalWhole",
    ]);
  });

  it("splits a tier-3 preimage by the §8.4 deterministic rule", () => {
    const preimage = filler(MIDGARD_CHUNK_BYTES_K + 517, 7);
    const chunks = splitMidgardFieldPreimageIntoChunks(preimage);
    expect(chunks.map((chunk) => chunk.length)).toEqual([
      MIDGARD_CHUNK_BYTES_K,
      517,
    ]);
    expect(hex(Buffer.concat(chunks))).toBe(hex(preimage));
    expect(midgardExpectedChunkCount(preimage.length)).toBe(2);
    expect(midgardExpectedChunkCount(MIDGARD_CHUNK_BYTES_K)).toBe(1);
    expect(
      midgardExpectedChunkCount(MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES),
    ).toBe(MIDGARD_MAX_TIER3_CHUNK_COUNT);
  });

  it("pins the §8.6 constant certificate asset name (#606)", () => {
    // One constant for every certificate of the policy; identity lives in the
    // datum. Pinned as bytes so the on-chain constant and this producer
    // cannot drift apart (the .ak golden channel pins the same value).
    expect(
      MIDGARD_FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME.toString("ascii"),
    ).toBe("MIDGARD_FIELD_PREIMAGE_CERT");
    expect(MIDGARD_FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME.length).toBe(27);
  });

  it("welds the §4 commitment into the certificate datum (#606)", () => {
    const preimage = filler(MIDGARD_CHUNK_BYTES_K + 1, 7);
    const certificate = deriveMidgardFieldPreimageCertificate({
      owner: filler(28, 1),
      txId: filler(32, 42),
      fieldIndex: 5,
      preimage,
    });
    expect(hex(certificate.fieldHash)).toBe(
      hex(midgardFieldCommitment(preimage)),
    );
    expect(() =>
      deriveMidgardFieldPreimageCertificate({
        owner: filler(28, 1),
        txId: filler(32, 42),
        fieldIndex: 9,
        preimage,
      }),
    ).toThrow(MidgardTxCodecError);
    expect(() =>
      deriveMidgardFieldPreimageCertificate({
        owner: filler(28, 1),
        txId: filler(31, 1),
        fieldIndex: 0,
        preimage,
      }),
    ).toThrow(/32 bytes/u);
  });

  it("refuses to certify a preimage that fits tier 1 or tier 2 (§8.4)", () => {
    expect(() =>
      deriveMidgardFieldPreimageCertificate({
        owner: filler(28, 1),
        txId: filler(32, 2),
        fieldIndex: 0,
        preimage: filler(MIDGARD_CHUNK_BYTES_K, 3),
      }),
    ).toThrow(/preimage_len > K/u);
  });
});

describe("§8.4 tier-3 view — straddle-aware, lazy chunk verify", () => {
  const fieldIndex = 3;
  const itemCount = Math.floor(
    (MIDGARD_CHUNK_BYTES_K * 2 - 2) / MIDGARD_HASH28_STRIDE,
  );
  const items = Array.from({ length: itemCount }, (_, index) =>
    filler(28, index + 1),
  );
  const preimage = encodeMidgardFieldPreimage(items);
  const commitment = midgardFieldCommitment(preimage);
  const txId = filler(32, 13);
  const certificate = deriveMidgardFieldPreimageCertificate({
    owner: filler(28, 5),
    txId,
    fieldIndex,
    preimage,
  });
  const chunks = splitMidgardFieldPreimageIntoChunks(preimage);

  it("carries a preimage larger than one chunk", () => {
    expect(preimage.length).toBeGreaterThan(MIDGARD_CHUNK_BYTES_K);
    expect(chunks).toHaveLength(2);
    expect(selectMidgardFieldCarriageTier(preimage.length)).toBe("Certified");
  });

  it("derives a fixed-stride count from the certified total length", () => {
    const view = buildMidgardChunkedFieldView({
      fieldIndex,
      txId,
      certificate,
      chunks,
      expectedCommitment: commitment,
    });
    expect(view.view).toBe("Chunked");
    expect(midgardFieldItemCount(view)).toBe(items.length);
    expect(midgardFieldTotalLength(view)).toBe(preimage.length);
  });

  it("refuses a manifest whose welded field_hash is not the anchored commitment (#606)", () => {
    // The certificate is honest about *these* bytes — its welded `fieldHash`
    // is their commitment — but the caller's anchored commitment names
    // different ones, which is exactly the forged-certificate shape the
    // on-chain door refuses.
    expect(() =>
      buildMidgardChunkedFieldView({
        fieldIndex,
        txId,
        certificate,
        chunks,
        expectedCommitment: Buffer.alloc(32),
      }),
    ).toThrow(/field_hash does not match the anchored commitment/u);
  });

  it("refuses chunks that do not hash to the committed field hash", () => {
    // Off-chain there is no minting policy to have checked the chunks against
    // the field hash, so the tier-3 builder discharges that obligation itself.
    // The certificate here *lies about its own weld* — its `fieldHash` states
    // the caller's commitment while its chunks are something else — so the
    // welded equality passes and the §4 reconstruction is what refuses.
    expect(() =>
      buildMidgardChunkedFieldView({
        fieldIndex,
        txId,
        certificate: { ...certificate, fieldHash: Buffer.alloc(32) },
        chunks,
        expectedCommitment: Buffer.alloc(32),
      }),
    ).toThrow(/does not match the committed field hash/u);
  });

  it("stitches an item that straddles the chunk boundary", () => {
    const view = buildMidgardChunkedFieldView({
      fieldIndex,
      txId,
      certificate,
      chunks,
      expectedCommitment: commitment,
    });
    const boundaryIndex = items.findIndex((_, index) => {
      const start = 2 + MIDGARD_HASH28_STRIDE * index;
      return (
        start < MIDGARD_CHUNK_BYTES_K &&
        start + MIDGARD_HASH28_STRIDE > MIDGARD_CHUNK_BYTES_K
      );
    });
    expect(boundaryIndex).toBeGreaterThan(0);
    expect(hex(midgardFieldItemAt(view, boundaryIndex))).toBe(
      hex(items[boundaryIndex]),
    );
    // Every item still reads back, on both sides of the boundary.
    expect(hex(midgardFieldItemAt(view, 0))).toBe(hex(items[0]));
    expect(hex(midgardFieldItemAt(view, items.length - 1))).toBe(
      hex(items[items.length - 1]),
    );
  });

  it("never hashes a chunk nobody reads, and rejects one that is read", () => {
    const forgedDigests = [
      certificate.chunkDigests[0],
      Buffer.alloc(32),
    ] as const;
    const view = buildMidgardChunkedFieldView({
      fieldIndex,
      txId,
      certificate: { ...certificate, chunkDigests: [...forgedDigests] },
      chunks,
      expectedCommitment: commitment,
    });
    // Item 0 lives entirely in chunk 0, whose digest is intact.
    expect(hex(midgardFieldItemAt(view, 0))).toBe(hex(items[0]));
    // The last item lives in chunk 1, whose digest is not.
    expect(() => midgardFieldItemAt(view, items.length - 1)).toThrow(
      /certified digest/u,
    );
  });

  it("aborts on a variable-width field's tier-3 item count but still reads", () => {
    const variableItems = [
      filler(MIDGARD_CHUNK_BYTES_K - 10, 1),
      filler(600, 2),
    ];
    const variablePreimage = encodeMidgardFieldPreimage(variableItems);
    const variableCertificate = deriveMidgardFieldPreimageCertificate({
      owner: filler(28, 5),
      txId,
      fieldIndex: 2,
      preimage: variablePreimage,
    });
    const view = buildMidgardChunkedFieldView({
      fieldIndex: 2,
      txId,
      certificate: variableCertificate,
      chunks: splitMidgardFieldPreimageIntoChunks(variablePreimage),
      expectedCommitment: midgardFieldCommitment(variablePreimage),
    });
    expect(() => midgardFieldItemCount(view)).toThrow(
      /no authenticated item count/u,
    );
    expect(hex(midgardFieldItemAt(view, 0))).toBe(hex(variableItems[0]));
    expect(hex(midgardFieldItemAt(view, 1))).toBe(hex(variableItems[1]));
  });

  it("rejects a certificate bound to another transaction or field", () => {
    expect(() =>
      buildMidgardChunkedFieldView({
        fieldIndex,
        txId: filler(32, 99),
        certificate,
        chunks,
        expectedCommitment: commitment,
      }),
    ).toThrow(/tx_id does not match/u);
    expect(() =>
      buildMidgardChunkedFieldView({
        fieldIndex: 4,
        txId,
        certificate,
        chunks,
        expectedCommitment: commitment,
      }),
    ).toThrow(/field_index does not match/u);
  });

  it("rejects a chunk whose length departs from the deterministic split", () => {
    expect(() =>
      buildMidgardChunkedFieldView({
        fieldIndex,
        txId,
        certificate,
        chunks: [chunks[0], chunks[1].subarray(1)],
        expectedCommitment: commitment,
      }),
    ).toThrow(/deterministic split/u);
  });
});
