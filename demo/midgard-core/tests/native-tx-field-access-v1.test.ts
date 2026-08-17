import { describe, expect, it } from "vitest";

import { MidgardTxCodecError } from "../src/codec/errors.js";
import {
  authenticatedMidgardFieldViewV1,
  buildMidgardChunkedFieldViewV1,
  buildMidgardWholeFieldViewV1,
  decodeMidgardFieldArrayHeaderV1,
  decodeMidgardFieldPreimageV1,
  deriveMidgardFieldPreimageCertificateV1,
  encodeMidgardDefiniteBytesV1,
  encodeMidgardFieldArrayHeaderV1,
  encodeMidgardFieldPreimageV1,
  MIDGARD_ADDRESS_WITNESS_STRIDE_V1,
  MIDGARD_CHUNK_BYTES_K_V1,
  MIDGARD_EMPTY_FIELD_COMMITMENT_V1,
  MIDGARD_FIELD_CARRIAGE_CONSTRUCTORS_V1,
  MIDGARD_FIELD_VIEW_CONSTRUCTORS_V1,
  MIDGARD_HASH28_STRIDE_V1,
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1,
  MIDGARD_MAX_TIER3_CHUNK_COUNT_V1,
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES_V1,
  MIDGARD_SPEND_INPUT_STRIDE_V1,
  midgardExpectedChunkCountV1,
  midgardFieldCommitmentFromItemsV1,
  midgardFieldCommitmentV1,
  midgardFieldItemAtV1,
  midgardFieldItemCountV1,
  midgardFieldItemExtentV1,
  MIDGARD_FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_V1,
  midgardFieldReadRangeV1,
  midgardFieldStrideV1,
  midgardFieldTotalLengthV1,
  selectMidgardFieldCarriageTierV1,
  splitMidgardFieldPreimageIntoChunksV1,
} from "../src/codec/native-tx-field-access-v1.js";

const hex = (value: Uint8Array): string => Buffer.from(value).toString("hex");
const bytes = (value: string): Buffer => Buffer.from(value, "hex");
const filler = (length: number, seed = 0): Buffer =>
  Buffer.from(
    Array.from({ length }, (_, index) => (index * 7 + 3 + seed * 31) % 256),
  );

const wholeView = (fieldIndex: number, items: readonly Uint8Array[]) => {
  const preimage = encodeMidgardFieldPreimageV1(items);
  return buildMidgardWholeFieldViewV1({
    fieldIndex,
    preimage,
    expectedCommitment: midgardFieldCommitmentV1(preimage),
  });
};

describe("§5.1 enveloped preimage grammar", () => {
  it("encodes an empty field as exactly `80` for every one of the nine", () => {
    for (let fieldIndex = 0; fieldIndex < 9; fieldIndex += 1) {
      const view = wholeView(fieldIndex, []);
      expect(hex(encodeMidgardFieldPreimageV1([]))).toBe("80");
      expect(midgardFieldItemCountV1(view)).toBe(0);
    }
  });

  it("emits minimal array headers at every width boundary and rejects wider", () => {
    expect(hex(encodeMidgardFieldArrayHeaderV1(0))).toBe("80");
    expect(hex(encodeMidgardFieldArrayHeaderV1(23))).toBe("97");
    expect(hex(encodeMidgardFieldArrayHeaderV1(24))).toBe("9818");
    expect(hex(encodeMidgardFieldArrayHeaderV1(255))).toBe("98ff");
    expect(hex(encodeMidgardFieldArrayHeaderV1(256))).toBe("990100");
    expect(hex(encodeMidgardFieldArrayHeaderV1(65535))).toBe("99ffff");
    expect(() => encodeMidgardFieldArrayHeaderV1(65536)).toThrow(
      MidgardTxCodecError,
    );
  });

  it("emits minimal item wrappers at every width boundary", () => {
    expect(hex(encodeMidgardDefiniteBytesV1(Buffer.alloc(0)))).toBe("40");
    expect(hex(encodeMidgardDefiniteBytesV1(Buffer.alloc(23)))).toBe(
      `57${"00".repeat(23)}`,
    );
    expect(hex(encodeMidgardDefiniteBytesV1(Buffer.alloc(24)))).toBe(
      `5818${"00".repeat(24)}`,
    );
    expect(
      hex(encodeMidgardDefiniteBytesV1(Buffer.alloc(256))).slice(0, 6),
    ).toBe("590100");
  });

  it("rejects the non-minimal and out-of-grammar array heads §5.1 excludes", () => {
    // `98 17` spells 23 in the two-byte form; `99 00ff` spells 255 in three.
    expect(() => decodeMidgardFieldArrayHeaderV1(bytes("9817"))).toThrow(
      MidgardTxCodecError,
    );
    expect(() => decodeMidgardFieldArrayHeaderV1(bytes("9900ff"))).toThrow(
      MidgardTxCodecError,
    );
    // `9a` is well-formed CBOR and outside the §5.1 acceptance set.
    expect(() => decodeMidgardFieldArrayHeaderV1(bytes("9a00010000"))).toThrow(
      MidgardTxCodecError,
    );
    expect(() => decodeMidgardFieldArrayHeaderV1(bytes("a0"))).toThrow(
      MidgardTxCodecError,
    );
  });

  it("round-trips items and fails closed on every §5.1 deviation", () => {
    const items = [filler(1, 1), filler(24, 2), filler(300, 3)];
    const preimage = encodeMidgardFieldPreimageV1(items);
    expect(decodeMidgardFieldPreimageV1(preimage).map(hex)).toEqual(
      items.map(hex),
    );

    // Trailing bytes after item N-1.
    expect(() =>
      decodeMidgardFieldPreimageV1(Buffer.concat([preimage, bytes("00")])),
    ).toThrow(/trailing bytes/u);
    // A header that over-counts its items.
    const overCounted = Buffer.from(preimage);
    overCounted[0] = 0x84;
    expect(() => decodeMidgardFieldPreimageV1(overCounted)).toThrow(
      MidgardTxCodecError,
    );
    // A header that under-counts leaves trailing bytes.
    const underCounted = Buffer.from(preimage);
    underCounted[0] = 0x82;
    expect(() => decodeMidgardFieldPreimageV1(underCounted)).toThrow(
      /trailing bytes/u,
    );
    // A non-minimal item wrapper: `58 01` where `41` is the one spelling.
    expect(() => decodeMidgardFieldPreimageV1(bytes("8158010f"))).toThrow(
      /non-minimal/u,
    );
  });
});

describe("§4 flat commitment", () => {
  it("pins the field-independent empty-field commitment", () => {
    // The exact cross-language vector: blake2b_256(#"80"). The Aiken twin pins
    // the same 32 bytes as `native_tx_field_access_v1.empty_field_commitment`.
    expect(hex(MIDGARD_EMPTY_FIELD_COMMITMENT_V1)).toBe(
      "45b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0",
    );
    expect(hex(midgardFieldCommitmentV1(bytes("80")))).toBe(
      hex(MIDGARD_EMPTY_FIELD_COMMITMENT_V1),
    );
    expect(hex(midgardFieldCommitmentFromItemsV1([]))).toBe(
      hex(MIDGARD_EMPTY_FIELD_COMMITMENT_V1),
    );
  });

  it("carries no domain tag, version prefix or field index", () => {
    const preimage = encodeMidgardFieldPreimageV1([filler(4, 1)]);
    expect(hex(midgardFieldCommitmentV1(preimage))).toBe(
      hex(midgardFieldCommitmentFromItemsV1([filler(4, 1)])),
    );
  });
});

describe("§5.3 stride table", () => {
  it("matches the Aiken table field by field", () => {
    expect(
      Array.from({ length: 9 }, (_, fieldIndex) =>
        midgardFieldStrideV1(fieldIndex),
      ),
    ).toEqual([
      MIDGARD_SPEND_INPUT_STRIDE_V1,
      MIDGARD_SPEND_INPUT_STRIDE_V1,
      0,
      MIDGARD_HASH28_STRIDE_V1,
      MIDGARD_HASH28_STRIDE_V1,
      0,
      0,
      MIDGARD_ADDRESS_WITNESS_STRIDE_V1,
      0,
    ]);
  });

  it("rejects a field index outside 0..8", () => {
    expect(() => midgardFieldStrideV1(9)).toThrow(MidgardTxCodecError);
    expect(() => midgardFieldStrideV1(-1)).toThrow(MidgardTxCodecError);
  });
});

describe("§7 access invariants over a Whole view", () => {
  const items = [filler(28, 1), filler(28, 2), filler(28, 3)];

  it("authenticates once against the committed hash", () => {
    const preimage = encodeMidgardFieldPreimageV1(items);
    expect(() =>
      buildMidgardWholeFieldViewV1({
        fieldIndex: 3,
        preimage,
        expectedCommitment: Buffer.alloc(32),
      }),
    ).toThrow(/does not match the committed field hash/u);
  });

  it("resolves fixed-stride items arithmetically and reads their wrapper", () => {
    const view = wholeView(3, items);
    expect(view.view).toBe("Whole");
    expect(midgardFieldItemCountV1(view)).toBe(3);
    expect(midgardFieldTotalLengthV1(view)).toBe(1 + 30 * 3);
    for (const [index, item] of items.entries()) {
      expect(midgardFieldItemExtentV1(view, index)).toEqual({
        offset: 1 + 30 * index + 2,
        length: 28,
      });
      expect(hex(midgardFieldItemAtV1(view, index))).toBe(hex(item));
    }
  });

  it("aborts rather than clamps an out-of-range index or read", () => {
    const view = wholeView(3, items);
    expect(() => midgardFieldItemAtV1(view, 3)).toThrow(/out of range/u);
    expect(() => midgardFieldItemAtV1(view, -1)).toThrow(/out of range/u);
    expect(() =>
      midgardFieldReadRangeV1(view, midgardFieldTotalLengthV1(view), 1),
    ).toThrow(/leaves the authenticated bytes/u);
    // Two clamped out-of-range reads would be byte-equal; neither is reachable.
    expect(() => midgardFieldReadRangeV1(view, 1_000, 4)).toThrow(
      /leaves the authenticated bytes/u,
    );
    expect(() => midgardFieldReadRangeV1(view, 2_000, 4)).toThrow(
      /leaves the authenticated bytes/u,
    );
  });

  it("refuses a fixed-stride item whose wrapper is not the canonical spelling", () => {
    // The two counterexamples the on-chain door was hardened against: a
    // 28-byte payload opened behind `00 00` or `ff ff` instead of `58 1c`.
    for (const wrapper of ["0000", "ffff"]) {
      const forged = Buffer.concat([
        bytes("81"),
        bytes(wrapper),
        filler(28, 9),
      ]);
      const view = buildMidgardWholeFieldViewV1({
        fieldIndex: 3,
        preimage: forged,
        expectedCommitment: midgardFieldCommitmentV1(forged),
      });
      expect(() => midgardFieldItemAtV1(view, 0)).toThrow(MidgardTxCodecError);
    }
  });

  it("enforces §7.4 count consistency at view construction", () => {
    // A fixed-stride field whose header count does not reconcile with length.
    const forged = Buffer.concat([bytes("82"), bytes("581c"), filler(28, 1)]);
    expect(() =>
      buildMidgardWholeFieldViewV1({
        fieldIndex: 3,
        preimage: forged,
        expectedCommitment: midgardFieldCommitmentV1(forged),
      }),
    ).toThrow(/count consistency/u);
    // A variable-width field is checked by the full walk instead.
    const walked = Buffer.concat([bytes("83"), bytes("4100"), bytes("4101")]);
    expect(() =>
      buildMidgardWholeFieldViewV1({
        fieldIndex: 2,
        preimage: walked,
        expectedCommitment: midgardFieldCommitmentV1(walked),
      }),
    ).toThrow(MidgardTxCodecError);
  });

  it("walks variable-width items with no offset table", () => {
    const variable = [filler(1, 1), filler(24, 2), filler(300, 3)];
    const view = wholeView(2, variable);
    expect(midgardFieldItemCountV1(view)).toBe(3);
    expect(midgardFieldItemExtentV1(view, 0)).toEqual({ offset: 2, length: 1 });
    expect(midgardFieldItemExtentV1(view, 1)).toEqual({
      offset: 5,
      length: 24,
    });
    expect(midgardFieldItemExtentV1(view, 2)).toEqual({
      offset: 32,
      length: 300,
    });
    for (const [index, item] of variable.entries()) {
      expect(hex(midgardFieldItemAtV1(view, index))).toBe(hex(item));
    }
  });

  it("rejects a preimage above the §5.4 aggregate bound", () => {
    const oversized = Buffer.alloc(
      MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES_V1 + 1,
    );
    oversized[0] = 0x80;
    expect(() =>
      buildMidgardWholeFieldViewV1({
        fieldIndex: 2,
        preimage: oversized,
        expectedCommitment: midgardFieldCommitmentV1(oversized),
      }),
    ).toThrow(/aggregate bound/u);
  });
});

describe("§8 carriage ladder", () => {
  it("partitions the tiers simplest-fitting-first", () => {
    expect(selectMidgardFieldCarriageTierV1(1)).toBe("Inline");
    expect(
      selectMidgardFieldCarriageTierV1(
        MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1,
      ),
    ).toBe("Inline");
    expect(
      selectMidgardFieldCarriageTierV1(
        MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1 + 1,
      ),
    ).toBe("RawUtxo");
    expect(selectMidgardFieldCarriageTierV1(MIDGARD_CHUNK_BYTES_K_V1)).toBe(
      "RawUtxo",
    );
    expect(selectMidgardFieldCarriageTierV1(MIDGARD_CHUNK_BYTES_K_V1 + 1)).toBe(
      "Certified",
    );
    expect(() =>
      selectMidgardFieldCarriageTierV1(
        MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES_V1 + 1,
      ),
    ).toThrow(/aggregate bound/u);
  });

  it("freezes the §8.8 constructor order", () => {
    expect([...MIDGARD_FIELD_CARRIAGE_CONSTRUCTORS_V1]).toEqual([
      "Inline",
      "RawUtxo",
      "Certified",
    ]);
    expect([...MIDGARD_FIELD_VIEW_CONSTRUCTORS_V1]).toEqual([
      "Whole",
      "Chunked",
    ]);
  });

  it("splits a tier-3 preimage by the §8.4 deterministic rule", () => {
    const preimage = filler(MIDGARD_CHUNK_BYTES_K_V1 + 517, 7);
    const chunks = splitMidgardFieldPreimageIntoChunksV1(preimage);
    expect(chunks.map((chunk) => chunk.length)).toEqual([
      MIDGARD_CHUNK_BYTES_K_V1,
      517,
    ]);
    expect(hex(Buffer.concat(chunks))).toBe(hex(preimage));
    expect(midgardExpectedChunkCountV1(preimage.length)).toBe(2);
    expect(midgardExpectedChunkCountV1(MIDGARD_CHUNK_BYTES_K_V1)).toBe(1);
    expect(
      midgardExpectedChunkCountV1(
        MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES_V1,
      ),
    ).toBe(MIDGARD_MAX_TIER3_CHUNK_COUNT_V1);
  });

  it("pins the §8.6 constant certificate asset name (#606)", () => {
    // One constant for every certificate of the policy; identity lives in the
    // datum. Pinned as bytes so the on-chain constant and this producer
    // cannot drift apart (the .ak golden channel pins the same value).
    expect(
      MIDGARD_FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_V1.toString("ascii"),
    ).toBe("MIDGARD_FIELD_PREIMAGE_CERT");
    expect(MIDGARD_FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_V1.length).toBe(27);
  });

  it("welds the §4 commitment into the certificate datum (#606)", () => {
    const preimage = filler(MIDGARD_CHUNK_BYTES_K_V1 + 1, 7);
    const certificate = deriveMidgardFieldPreimageCertificateV1({
      owner: filler(28, 1),
      txId: filler(32, 42),
      fieldIndex: 5,
      preimage,
    });
    expect(hex(certificate.fieldHash)).toBe(
      hex(midgardFieldCommitmentV1(preimage)),
    );
    expect(() =>
      deriveMidgardFieldPreimageCertificateV1({
        owner: filler(28, 1),
        txId: filler(32, 42),
        fieldIndex: 9,
        preimage,
      }),
    ).toThrow(MidgardTxCodecError);
    expect(() =>
      deriveMidgardFieldPreimageCertificateV1({
        owner: filler(28, 1),
        txId: filler(31, 1),
        fieldIndex: 0,
        preimage,
      }),
    ).toThrow(/32 bytes/u);
  });

  it("refuses to certify a preimage that fits tier 1 or tier 2 (§8.4)", () => {
    expect(() =>
      deriveMidgardFieldPreimageCertificateV1({
        owner: filler(28, 1),
        txId: filler(32, 2),
        fieldIndex: 0,
        preimage: filler(MIDGARD_CHUNK_BYTES_K_V1, 3),
      }),
    ).toThrow(/preimage_len > K/u);
  });
});

describe("§8.4 tier-3 view — straddle-aware, lazy chunk verify", () => {
  const fieldIndex = 3;
  const itemCount = Math.floor(
    (MIDGARD_CHUNK_BYTES_K_V1 * 2 - 2) / MIDGARD_HASH28_STRIDE_V1,
  );
  const items = Array.from({ length: itemCount }, (_, index) =>
    filler(28, index + 1),
  );
  const preimage = encodeMidgardFieldPreimageV1(items);
  const commitment = midgardFieldCommitmentV1(preimage);
  const txId = filler(32, 13);
  const certificate = deriveMidgardFieldPreimageCertificateV1({
    owner: filler(28, 5),
    txId,
    fieldIndex,
    preimage,
  });
  const chunks = splitMidgardFieldPreimageIntoChunksV1(preimage);

  it("carries a preimage larger than one chunk", () => {
    expect(preimage.length).toBeGreaterThan(MIDGARD_CHUNK_BYTES_K_V1);
    expect(chunks).toHaveLength(2);
    expect(selectMidgardFieldCarriageTierV1(preimage.length)).toBe("Certified");
  });

  it("derives a fixed-stride count from the certified total length", () => {
    const view = buildMidgardChunkedFieldViewV1({
      fieldIndex,
      txId,
      certificate,
      chunks,
      expectedCommitment: commitment,
    });
    expect(view.view).toBe("Chunked");
    expect(midgardFieldItemCountV1(view)).toBe(items.length);
    expect(midgardFieldTotalLengthV1(view)).toBe(preimage.length);
  });

  it("refuses a manifest whose welded field_hash is not the anchored commitment (#606)", () => {
    // The certificate is honest about *these* bytes — its welded `fieldHash`
    // is their commitment — but the caller's anchored commitment names
    // different ones, which is exactly the forged-certificate shape the
    // on-chain door refuses.
    expect(() =>
      buildMidgardChunkedFieldViewV1({
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
      buildMidgardChunkedFieldViewV1({
        fieldIndex,
        txId,
        certificate: { ...certificate, fieldHash: Buffer.alloc(32) },
        chunks,
        expectedCommitment: Buffer.alloc(32),
      }),
    ).toThrow(/does not match the committed field hash/u);
  });

  it("stitches an item that straddles the chunk boundary", () => {
    const view = buildMidgardChunkedFieldViewV1({
      fieldIndex,
      txId,
      certificate,
      chunks,
      expectedCommitment: commitment,
    });
    const boundaryIndex = items.findIndex((_, index) => {
      const start = 2 + MIDGARD_HASH28_STRIDE_V1 * index;
      return (
        start < MIDGARD_CHUNK_BYTES_K_V1 &&
        start + MIDGARD_HASH28_STRIDE_V1 > MIDGARD_CHUNK_BYTES_K_V1
      );
    });
    expect(boundaryIndex).toBeGreaterThan(0);
    expect(hex(midgardFieldItemAtV1(view, boundaryIndex))).toBe(
      hex(items[boundaryIndex]),
    );
    // Every item still reads back, on both sides of the boundary.
    expect(hex(midgardFieldItemAtV1(view, 0))).toBe(hex(items[0]));
    expect(hex(midgardFieldItemAtV1(view, items.length - 1))).toBe(
      hex(items[items.length - 1]),
    );
  });

  it("never hashes a chunk nobody reads, and rejects one that is read", () => {
    const forgedDigests = [
      certificate.chunkDigests[0],
      Buffer.alloc(32),
    ] as const;
    const view = buildMidgardChunkedFieldViewV1({
      fieldIndex,
      txId,
      certificate: { ...certificate, chunkDigests: [...forgedDigests] },
      chunks,
      expectedCommitment: commitment,
    });
    // Item 0 lives entirely in chunk 0, whose digest is intact.
    expect(hex(midgardFieldItemAtV1(view, 0))).toBe(hex(items[0]));
    // The last item lives in chunk 1, whose digest is not.
    expect(() => midgardFieldItemAtV1(view, items.length - 1)).toThrow(
      /certified digest/u,
    );
  });

  it("aborts on a variable-width field's tier-3 item count but still reads", () => {
    const variableItems = [
      filler(MIDGARD_CHUNK_BYTES_K_V1 - 10, 1),
      filler(600, 2),
    ];
    const variablePreimage = encodeMidgardFieldPreimageV1(variableItems);
    const variableCertificate = deriveMidgardFieldPreimageCertificateV1({
      owner: filler(28, 5),
      txId,
      fieldIndex: 2,
      preimage: variablePreimage,
    });
    const view = buildMidgardChunkedFieldViewV1({
      fieldIndex: 2,
      txId,
      certificate: variableCertificate,
      chunks: splitMidgardFieldPreimageIntoChunksV1(variablePreimage),
      expectedCommitment: midgardFieldCommitmentV1(variablePreimage),
    });
    expect(() => midgardFieldItemCountV1(view)).toThrow(
      /no authenticated item count/u,
    );
    expect(hex(midgardFieldItemAtV1(view, 0))).toBe(hex(variableItems[0]));
    expect(hex(midgardFieldItemAtV1(view, 1))).toBe(hex(variableItems[1]));
  });

  it("rejects a certificate bound to another transaction or field", () => {
    expect(() =>
      buildMidgardChunkedFieldViewV1({
        fieldIndex,
        txId: filler(32, 99),
        certificate,
        chunks,
        expectedCommitment: commitment,
      }),
    ).toThrow(/tx_id does not match/u);
    expect(() =>
      buildMidgardChunkedFieldViewV1({
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
      buildMidgardChunkedFieldViewV1({
        fieldIndex,
        txId,
        certificate,
        chunks: [chunks[0], chunks[1].subarray(1)],
        expectedCommitment: commitment,
      }),
    ).toThrow(/deterministic split/u);
  });
});

describe("the off-chain door", () => {
  const fieldIndex = 3;
  const items = [filler(28, 1), filler(28, 2)];
  const preimage = encodeMidgardFieldPreimageV1(items);
  const commitment = midgardFieldCommitmentV1(preimage);
  const txId = filler(32, 21);

  it("serves tier 1 from the redeemer's own bytes", () => {
    const view = authenticatedMidgardFieldViewV1({
      fieldIndex,
      txId,
      expectedCommitment: commitment,
      carriage: { carriage: "Inline", preimage },
    });
    expect(midgardFieldItemCountV1(view)).toBe(2);
  });

  it("serves tier 2 from a positional reference input", () => {
    const view = authenticatedMidgardFieldViewV1({
      fieldIndex,
      txId,
      expectedCommitment: commitment,
      carriage: { carriage: "RawUtxo", refInputIndex: 1 },
      referenceInputs: [{}, { inlineDatumBytes: preimage }],
    });
    expect(midgardFieldItemCountV1(view)).toBe(2);
  });

  it("fails closed when a named reference input is absent or carries nothing", () => {
    expect(() =>
      authenticatedMidgardFieldViewV1({
        fieldIndex,
        txId,
        expectedCommitment: commitment,
        carriage: { carriage: "RawUtxo", refInputIndex: 3 },
        referenceInputs: [{ inlineDatumBytes: preimage }],
      }),
    ).toThrow(/not present/u);
    expect(() =>
      authenticatedMidgardFieldViewV1({
        fieldIndex,
        txId,
        expectedCommitment: commitment,
        carriage: { carriage: "RawUtxo", refInputIndex: 0 },
        referenceInputs: [{}],
      }),
    ).toThrow(/no nothing-but-bytes inline datum/u);
  });

  // One tier-3 carriage, reused by the three assertions below. The preimage is
  // 600 items — larger than one chunk, so §8.4 admits exactly this tier.
  const bigItems = Array.from({ length: 600 }, (_, index) =>
    filler(28, index + 1),
  );
  const bigPreimage = encodeMidgardFieldPreimageV1(bigItems);
  const bigCommitment = midgardFieldCommitmentV1(bigPreimage);
  const bigCertificate = deriveMidgardFieldPreimageCertificateV1({
    owner: filler(28, 5),
    txId,
    fieldIndex,
    preimage: bigPreimage,
  });
  const bigChunks = splitMidgardFieldPreimageIntoChunksV1(bigPreimage);
  const tier3ReferenceInputs = [
    {
      certificate: bigCertificate,
      certificateAssetName: MIDGARD_FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_V1,
    },
    { inlineDatumBytes: bigChunks[0] },
    { inlineDatumBytes: bigChunks[1] },
  ];
  const tier3Carriage = {
    carriage: "Certified",
    certRefInputIndex: 0,
    chunkRefInputIndices: [1, 2],
  } as const;

  it("serves tier 3 from a certificate and its positional chunks", () => {
    const view = authenticatedMidgardFieldViewV1({
      fieldIndex,
      txId,
      expectedCommitment: bigCommitment,
      carriage: tier3Carriage,
      referenceInputs: tier3ReferenceInputs,
    });
    expect(midgardFieldItemCountV1(view)).toBe(bigItems.length);
    expect(hex(midgardFieldItemAtV1(view, 599))).toBe(hex(bigItems[599]));
  });

  it("refuses a tier-3 carriage over bytes the field never committed to", () => {
    // The counterexample the tier-3 door has to answer. `commitment` here is
    // the honest commitment of the two-item preimage this block opens with;
    // `bigChunks` carry a 600-item preimage. Nothing may come back — not a
    // short read, not a count. The honest manifest wears its own (foreign)
    // commitment and fails the welded-hash equality (#606) — the same check
    // the on-chain door runs.
    expect(() =>
      authenticatedMidgardFieldViewV1({
        fieldIndex,
        txId,
        expectedCommitment: commitment,
        carriage: tier3Carriage,
        referenceInputs: tier3ReferenceInputs,
      }),
    ).toThrow(/field_hash does not match the anchored commitment/u);
    // And a manifest that lies about its own weld is caught one check later,
    // by the §4 reconstruction the off-chain door runs for itself (on-chain
    // this is the certificate policy's mint-time proof).
    expect(() =>
      authenticatedMidgardFieldViewV1({
        fieldIndex,
        txId,
        expectedCommitment: commitment,
        carriage: tier3Carriage,
        referenceInputs: [
          {
            ...tier3ReferenceInputs[0],
            certificate: { ...bigCertificate, fieldHash: commitment },
          },
          tier3ReferenceInputs[1],
          tier3ReferenceInputs[2],
        ],
      }),
    ).toThrow(/does not match the committed field hash/u);
    expect(() =>
      authenticatedMidgardFieldViewV1({
        fieldIndex,
        txId,
        expectedCommitment: commitment,
        carriage: tier3Carriage,
        referenceInputs: tier3ReferenceInputs,
      }),
    ).toThrow(MidgardTxCodecError);
  });

  it("rejects the same commitment mismatch at every tier", () => {
    // Tiers 1 and 2 already refused this; tier 3 refusing it too is what makes
    // carriage an encoding detail rather than a choice of how hard to check.
    // (Under tier 3 the refusal happens one check earlier since #606 — at the
    // welded-hash equality — hence the two-message alternation.)
    for (const carriage of [
      { carriage: "Inline", preimage: bigPreimage } as const,
      { carriage: "RawUtxo", refInputIndex: 3 } as const,
      tier3Carriage,
    ]) {
      expect(() =>
        authenticatedMidgardFieldViewV1({
          fieldIndex,
          txId,
          expectedCommitment: commitment,
          carriage,
          referenceInputs: [
            ...tier3ReferenceInputs,
            { inlineDatumBytes: bigPreimage },
          ],
        }),
      ).toThrow(
        /does not match the (committed field hash|anchored commitment)/u,
      );
    }
  });

  it("requires the §8.6 constant token name at the tier-3 manifest input", () => {
    expect(() =>
      authenticatedMidgardFieldViewV1({
        fieldIndex,
        txId,
        expectedCommitment: bigCommitment,
        carriage: tier3Carriage,
        referenceInputs: [
          {
            ...tier3ReferenceInputs[0],
            certificateAssetName: Buffer.alloc(32),
          },
          tier3ReferenceInputs[1],
          tier3ReferenceInputs[2],
        ],
      }),
    ).toThrow(/§8.6 constant name/u);
  });
});
