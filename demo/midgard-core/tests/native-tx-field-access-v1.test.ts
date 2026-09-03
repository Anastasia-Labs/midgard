import { describe, expect, it } from "vitest";

import { MidgardTxCodecError } from "../src/codec/errors.js";
import {
  authenticatedMidgardFieldView,
  buildMidgardChunkedFieldView,
  buildMidgardWholeFieldView,
  decodeMidgardFieldArrayHeader,
  decodeMidgardFieldPreimage,
  deriveMidgardFieldPreimageCertificate,
  encodeMidgardDefiniteBytes,
  encodeMidgardFieldArrayHeader,
  encodeMidgardFieldPreimage,
  MIDGARD_ADDRESS_WITNESS_STRIDE,
  MIDGARD_CHUNK_BYTES_K,
  MIDGARD_EMPTY_FIELD_COMMITMENT,
  MIDGARD_FIELD_CARRIAGE_CONSTRUCTORS,
  MIDGARD_FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME,
  MIDGARD_FIELD_VIEW_CONSTRUCTORS,
  MIDGARD_HASH28_STRIDE,
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
  MIDGARD_MAX_TIER3_CHUNK_COUNT,
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
  MIDGARD_SPEND_INPUT_STRIDE,
  midgardExpectedChunkCount,
  midgardFieldCommitment,
  midgardFieldCommitmentFromItems,
  midgardFieldItemAt,
  midgardFieldItemCount,
  midgardFieldItemExtent,
  midgardFieldReadRange,
  midgardFieldStride,
  midgardFieldTotalLength,
  selectMidgardFieldCarriageTier,
  splitMidgardFieldPreimageIntoChunks,
} from "../src/codec/native-tx-field-access-v1.js";

const hex = (value: Uint8Array): string => Buffer.from(value).toString("hex");
const bytes = (value: string): Buffer => Buffer.from(value, "hex");
const filler = (length: number, seed = 0): Buffer =>
  Buffer.from(
    Array.from({ length }, (_, index) => (index * 7 + 3 + seed * 31) % 256),
  );

const wholeView = (fieldIndex: number, items: readonly Uint8Array[]) => {
  const preimage = encodeMidgardFieldPreimage(items);
  return buildMidgardWholeFieldView({
    fieldIndex,
    preimage,
    expectedCommitment: midgardFieldCommitment(preimage),
  });
};

describe("§5.1 enveloped preimage grammar", () => {
  it("encodes an empty field as exactly `80` for every one of the nine", () => {
    for (let fieldIndex = 0; fieldIndex < 9; fieldIndex += 1) {
      const view = wholeView(fieldIndex, []);
      expect(hex(encodeMidgardFieldPreimage([]))).toBe("80");
      expect(midgardFieldItemCount(view)).toBe(0);
    }
  });

  it("emits minimal array headers at every width boundary and rejects wider", () => {
    expect(hex(encodeMidgardFieldArrayHeader(0))).toBe("80");
    expect(hex(encodeMidgardFieldArrayHeader(23))).toBe("97");
    expect(hex(encodeMidgardFieldArrayHeader(24))).toBe("9818");
    expect(hex(encodeMidgardFieldArrayHeader(255))).toBe("98ff");
    expect(hex(encodeMidgardFieldArrayHeader(256))).toBe("990100");
    expect(hex(encodeMidgardFieldArrayHeader(65535))).toBe("99ffff");
    expect(() => encodeMidgardFieldArrayHeader(65536)).toThrow(
      MidgardTxCodecError,
    );
  });

  it("emits minimal item wrappers at every width boundary", () => {
    expect(hex(encodeMidgardDefiniteBytes(Buffer.alloc(0)))).toBe("40");
    expect(hex(encodeMidgardDefiniteBytes(Buffer.alloc(23)))).toBe(
      `57${"00".repeat(23)}`,
    );
    expect(hex(encodeMidgardDefiniteBytes(Buffer.alloc(24)))).toBe(
      `5818${"00".repeat(24)}`,
    );
    expect(hex(encodeMidgardDefiniteBytes(Buffer.alloc(256))).slice(0, 6)).toBe(
      "590100",
    );
  });

  it("rejects the non-minimal and out-of-grammar array heads §5.1 excludes", () => {
    // `98 17` spells 23 in the two-byte form; `99 00ff` spells 255 in three.
    expect(() => decodeMidgardFieldArrayHeader(bytes("9817"))).toThrow(
      MidgardTxCodecError,
    );
    expect(() => decodeMidgardFieldArrayHeader(bytes("9900ff"))).toThrow(
      MidgardTxCodecError,
    );
    // `9a` is well-formed CBOR and outside the §5.1 acceptance set.
    expect(() => decodeMidgardFieldArrayHeader(bytes("9a00010000"))).toThrow(
      MidgardTxCodecError,
    );
    expect(() => decodeMidgardFieldArrayHeader(bytes("a0"))).toThrow(
      MidgardTxCodecError,
    );
  });

  it("round-trips items and fails closed on every §5.1 deviation", () => {
    const items = [filler(1, 1), filler(24, 2), filler(300, 3)];
    const preimage = encodeMidgardFieldPreimage(items);
    expect(decodeMidgardFieldPreimage(preimage).map(hex)).toEqual(
      items.map(hex),
    );

    // Trailing bytes after item N-1.
    expect(() =>
      decodeMidgardFieldPreimage(Buffer.concat([preimage, bytes("00")])),
    ).toThrow(/trailing bytes/u);
    // A header that over-counts its items.
    const overCounted = Buffer.from(preimage);
    overCounted[0] = 0x84;
    expect(() => decodeMidgardFieldPreimage(overCounted)).toThrow(
      MidgardTxCodecError,
    );
    // A header that under-counts leaves trailing bytes.
    const underCounted = Buffer.from(preimage);
    underCounted[0] = 0x82;
    expect(() => decodeMidgardFieldPreimage(underCounted)).toThrow(
      /trailing bytes/u,
    );
    // A non-minimal item wrapper: `58 01` where `41` is the one spelling.
    expect(() => decodeMidgardFieldPreimage(bytes("8158010f"))).toThrow(
      /non-minimal/u,
    );
  });
});

describe("§4 flat commitment", () => {
  it("pins the field-independent empty-field commitment", () => {
    // The exact cross-language vector: blake2b_256(#"80"). The Aiken twin pins
    // the same 32 bytes as `native_tx_field_access_v1.empty_field_commitment`.
    expect(hex(MIDGARD_EMPTY_FIELD_COMMITMENT)).toBe(
      "45b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0",
    );
    expect(hex(midgardFieldCommitment(bytes("80")))).toBe(
      hex(MIDGARD_EMPTY_FIELD_COMMITMENT),
    );
    expect(hex(midgardFieldCommitmentFromItems([]))).toBe(
      hex(MIDGARD_EMPTY_FIELD_COMMITMENT),
    );
  });

  it("carries no domain tag, version prefix or field index", () => {
    const preimage = encodeMidgardFieldPreimage([filler(4, 1)]);
    expect(hex(midgardFieldCommitment(preimage))).toBe(
      hex(midgardFieldCommitmentFromItems([filler(4, 1)])),
    );
  });
});

describe("§5.3 stride table", () => {
  it("matches the Aiken table field by field", () => {
    expect(
      Array.from({ length: 9 }, (_, fieldIndex) =>
        midgardFieldStride(fieldIndex),
      ),
    ).toEqual([
      MIDGARD_SPEND_INPUT_STRIDE,
      MIDGARD_SPEND_INPUT_STRIDE,
      0,
      MIDGARD_HASH28_STRIDE,
      MIDGARD_HASH28_STRIDE,
      0,
      0,
      MIDGARD_ADDRESS_WITNESS_STRIDE,
      0,
    ]);
  });

  it("rejects a field index outside 0..8", () => {
    expect(() => midgardFieldStride(9)).toThrow(MidgardTxCodecError);
    expect(() => midgardFieldStride(-1)).toThrow(MidgardTxCodecError);
  });
});

describe("§7 access invariants over a Whole view", () => {
  const items = [filler(28, 1), filler(28, 2), filler(28, 3)];

  it("authenticates once against the committed hash", () => {
    const preimage = encodeMidgardFieldPreimage(items);
    expect(() =>
      buildMidgardWholeFieldView({
        fieldIndex: 3,
        preimage,
        expectedCommitment: Buffer.alloc(32),
      }),
    ).toThrow(/does not match the committed field hash/u);
  });

  it("resolves fixed-stride items arithmetically and reads their wrapper", () => {
    const view = wholeView(3, items);
    expect(view.view).toBe("Whole");
    expect(midgardFieldItemCount(view)).toBe(3);
    expect(midgardFieldTotalLength(view)).toBe(1 + 30 * 3);
    for (const [index, item] of items.entries()) {
      expect(midgardFieldItemExtent(view, index)).toEqual({
        offset: 1 + 30 * index + 2,
        length: 28,
      });
      expect(hex(midgardFieldItemAt(view, index))).toBe(hex(item));
    }
  });

  it("aborts rather than clamps an out-of-range index or read", () => {
    const view = wholeView(3, items);
    expect(() => midgardFieldItemAt(view, 3)).toThrow(/out of range/u);
    expect(() => midgardFieldItemAt(view, -1)).toThrow(/out of range/u);
    expect(() =>
      midgardFieldReadRange(view, midgardFieldTotalLength(view), 1),
    ).toThrow(/leaves the authenticated bytes/u);
    // Two clamped out-of-range reads would be byte-equal; neither is reachable.
    expect(() => midgardFieldReadRange(view, 1_000, 4)).toThrow(
      /leaves the authenticated bytes/u,
    );
    expect(() => midgardFieldReadRange(view, 2_000, 4)).toThrow(
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
      const view = buildMidgardWholeFieldView({
        fieldIndex: 3,
        preimage: forged,
        expectedCommitment: midgardFieldCommitment(forged),
      });
      expect(() => midgardFieldItemAt(view, 0)).toThrow(MidgardTxCodecError);
    }
  });

  it("enforces §7.4 count consistency at view construction", () => {
    // A fixed-stride field whose header count does not reconcile with length.
    const forged = Buffer.concat([bytes("82"), bytes("581c"), filler(28, 1)]);
    expect(() =>
      buildMidgardWholeFieldView({
        fieldIndex: 3,
        preimage: forged,
        expectedCommitment: midgardFieldCommitment(forged),
      }),
    ).toThrow(/count consistency/u);
    // A variable-width field is checked by the full walk instead.
    const walked = Buffer.concat([bytes("83"), bytes("4100"), bytes("4101")]);
    expect(() =>
      buildMidgardWholeFieldView({
        fieldIndex: 2,
        preimage: walked,
        expectedCommitment: midgardFieldCommitment(walked),
      }),
    ).toThrow(MidgardTxCodecError);
  });

  it("walks variable-width items with no offset table", () => {
    const variable = [filler(1, 1), filler(24, 2), filler(300, 3)];
    const view = wholeView(2, variable);
    expect(midgardFieldItemCount(view)).toBe(3);
    expect(midgardFieldItemExtent(view, 0)).toEqual({ offset: 2, length: 1 });
    expect(midgardFieldItemExtent(view, 1)).toEqual({
      offset: 5,
      length: 24,
    });
    expect(midgardFieldItemExtent(view, 2)).toEqual({
      offset: 32,
      length: 300,
    });
    for (const [index, item] of variable.entries()) {
      expect(hex(midgardFieldItemAt(view, index))).toBe(hex(item));
    }
  });

  it("rejects a preimage above the §5.4 aggregate bound", () => {
    const oversized = Buffer.alloc(
      MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES + 1,
    );
    oversized[0] = 0x80;
    expect(() =>
      buildMidgardWholeFieldView({
        fieldIndex: 2,
        preimage: oversized,
        expectedCommitment: midgardFieldCommitment(oversized),
      }),
    ).toThrow(/aggregate bound/u);
  });
});

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

describe("the off-chain door", () => {
  const fieldIndex = 3;
  const items = [filler(28, 1), filler(28, 2)];
  const preimage = encodeMidgardFieldPreimage(items);
  const commitment = midgardFieldCommitment(preimage);
  const txId = filler(32, 21);

  it("serves tier 1 from the redeemer's own bytes", () => {
    const view = authenticatedMidgardFieldView({
      fieldIndex,
      txId,
      expectedCommitment: commitment,
      carriage: { carriage: "Inline", preimage },
    });
    expect(midgardFieldItemCount(view)).toBe(2);
  });

  it("serves tier 2 from a positional reference input", () => {
    const view = authenticatedMidgardFieldView({
      fieldIndex,
      txId,
      expectedCommitment: commitment,
      carriage: { carriage: "RawUtxo", refInputIndex: 1 },
      referenceInputs: [{}, { inlineDatumBytes: preimage }],
    });
    expect(midgardFieldItemCount(view)).toBe(2);
  });

  it("fails closed when a named reference input is absent or carries nothing", () => {
    expect(() =>
      authenticatedMidgardFieldView({
        fieldIndex,
        txId,
        expectedCommitment: commitment,
        carriage: { carriage: "RawUtxo", refInputIndex: 3 },
        referenceInputs: [{ inlineDatumBytes: preimage }],
      }),
    ).toThrow(/not present/u);
    expect(() =>
      authenticatedMidgardFieldView({
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
  const bigPreimage = encodeMidgardFieldPreimage(bigItems);
  const bigCommitment = midgardFieldCommitment(bigPreimage);
  const bigCertificate = deriveMidgardFieldPreimageCertificate({
    owner: filler(28, 5),
    txId,
    fieldIndex,
    preimage: bigPreimage,
  });
  const bigChunks = splitMidgardFieldPreimageIntoChunks(bigPreimage);
  const tier3ReferenceInputs = [
    {
      certificate: bigCertificate,
      certificateAssetName: MIDGARD_FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME,
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
    const view = authenticatedMidgardFieldView({
      fieldIndex,
      txId,
      expectedCommitment: bigCommitment,
      carriage: tier3Carriage,
      referenceInputs: tier3ReferenceInputs,
    });
    expect(midgardFieldItemCount(view)).toBe(bigItems.length);
    expect(hex(midgardFieldItemAt(view, 599))).toBe(hex(bigItems[599]));
  });

  it("refuses a tier-3 carriage over bytes the field never committed to", () => {
    // The counterexample the tier-3 door has to answer. `commitment` here is
    // the honest commitment of the two-item preimage this block opens with;
    // `bigChunks` carry a 600-item preimage. Nothing may come back — not a
    // short read, not a count. The honest manifest wears its own (foreign)
    // commitment and fails the welded-hash equality (#606) — the same check
    // the on-chain door runs.
    expect(() =>
      authenticatedMidgardFieldView({
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
      authenticatedMidgardFieldView({
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
      authenticatedMidgardFieldView({
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
        authenticatedMidgardFieldView({
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
      authenticatedMidgardFieldView({
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
