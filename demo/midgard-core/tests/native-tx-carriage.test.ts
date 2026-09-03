import { describe, expect, it } from "vitest";

import {
  healMidgardFieldCarriage,
  layOutMidgardFieldCarriage,
  MIDGARD_CARRIAGE_RELIABILITY_RESERVE_BYTES,
  MIDGARD_EXACT_PUBLISHABLE_CARRIAGE_BYTES,
  MIDGARD_MAX_PUBLISHABLE_CARRIAGE_BYTES,
  midgardCarriageDataByteStringBytes,
  midgardCarriagePublicationBytes,
  midgardCarriagePublicationFramingBytes,
  midgardFieldCarriageBounds,
  midgardFieldCarriagePlansAreInterchangeable,
  midgardFieldCarriagePublishability,
  planMidgardFieldCarriage,
} from "../src/codec/native-tx-carriage.js";
import {
  authenticatedMidgardFieldView,
  MIDGARD_CHUNK_BYTES_K,
  MIDGARD_FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME,
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
  midgardFieldCommitment,
  midgardFieldItemAt,
  splitMidgardFieldPreimageIntoChunks,
} from "../src/codec/native-tx-field-access.js";

/**
 * The publication half of the §8 ladder, at its own seam.
 *
 * The end-to-end exercise — real transactions, a real ledger, a real yank and
 * heal — lives in
 * `demo/midgard-validation/tests/field-preimage-carriage-fit-emulator.test.ts`.
 * What is here is what does not need a ledger: that the tier is a total
 * function of length, that a plan is a pure function of its inputs, and that
 * the layout and the reference inputs it indexes are emitted together and
 * cannot disagree.
 */

const OWNER = Buffer.alloc(28, 0x11);
const HEALER = Buffer.alloc(28, 0x22);
const TX_ID = Buffer.alloc(32, 0x5a);
const FIELD_INDEX = 1;
const STRIDE = 40;

/** A §5.1 preimage of `itemCount` well-formed field-1 items. */
const preimageOf = (itemCount: number): Buffer => {
  const header =
    itemCount <= 23
      ? Buffer.from([0x80 + itemCount])
      : itemCount <= 255
        ? Buffer.from([0x98, itemCount])
        : Buffer.from([0x99, itemCount >> 8, itemCount & 0xff]);
  const items = Array.from({ length: itemCount }, (_unused, index) =>
    Buffer.concat([
      Buffer.from([0x58, 0x26, 0x82, 0x58, 0x20]),
      Buffer.from(
        Array.from(
          { length: 32 },
          (_byte, offset) => (index * 7 + offset) & 0xff,
        ),
      ),
      Buffer.from([0x19, (index >> 8) & 0xff, index & 0xff]),
    ]),
  );
  return Buffer.concat([header, ...items]);
};

/** The largest §5.1 preimage at or under `bytes`. */
const preimageUnder = (bytes: number): Buffer => {
  for (let count = Math.floor(bytes / STRIDE) + 1; count >= 0; count -= 1) {
    const candidate = preimageOf(count);
    if (candidate.length <= bytes) {
      return candidate;
    }
  }
  throw new Error("no item count fits");
};

const plan = (preimage: Buffer, owner: Uint8Array = OWNER) =>
  planMidgardFieldCarriage({
    owner,
    txId: TX_ID,
    fieldIndex: FIELD_INDEX,
    preimage,
  });

describe("§8 carriage plan — the tier is a total function of length", () => {
  it("partitions the ladder at the §8.3 bounds", () => {
    expect(plan(preimageOf(1)).tier).toBe("Inline");
    expect(
      plan(preimageUnder(MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES)).tier,
    ).toBe("Inline");
    expect(
      plan(preimageUnder(MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES + STRIDE))
        .tier,
    ).toBe("RawUtxo");
    expect(plan(preimageUnder(MIDGARD_CHUNK_BYTES_K)).tier).toBe("RawUtxo");
    expect(plan(preimageUnder(MIDGARD_CHUNK_BYTES_K + STRIDE)).tier).toBe(
      "Certified",
    );
  });

  it("publishes nothing under tier 1 and exactly one UTxO under tier 2", () => {
    const tier1 = plan(preimageOf(1));
    expect(tier1.publications).toEqual([]);
    expect(tier1.certificate).toBeNull();
    expect(tier1.inlinePreimage).not.toBeNull();

    const tier2 = plan(preimageUnder(MIDGARD_CHUNK_BYTES_K));
    expect(tier2.publications.length).toBe(1);
    expect(tier2.certificate).toBeNull();
    expect(tier2.inlinePreimage).toBeNull();
    // Under tier 2 the publication's own digest *is* the §4 field commitment,
    // because the published bytes are the whole preimage.
    expect(tier2.publications[0]?.digest).toEqual(tier2.commitment);
  });

  it("splits the three-chunk corner exactly as the §8.4 rule does", () => {
    const preimage = preimageUnder(
      MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
    );
    const corner = plan(preimage);
    expect(corner.tier).toBe("Certified");
    expect(corner.publications.map((entry) => entry.bytes)).toEqual([
      ...splitMidgardFieldPreimageIntoChunks(preimage),
    ]);
    expect(corner.publications.map((entry) => entry.chunkIndex)).toEqual([
      0, 1, 2,
    ]);
    // Each publication's digest is the digest of its own bytes, and the
    // certificate's vector is those digests in order — the two cannot drift
    // because the plan builds them from one split.
    expect(corner.publications.map((entry) => entry.digest)).toEqual([
      ...(corner.certificate?.chunkDigests ?? []),
    ]);
    for (const entry of corner.publications) {
      expect(entry.digest).toEqual(midgardFieldCommitment(entry.bytes));
    }
    expect(corner.certificateAssetName).toEqual(
      MIDGARD_FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME,
    );
    // The welded datum commitment (#606): the certificate's `fieldHash` is
    // the plan's own §4 commitment.
    expect(corner.certificate?.fieldHash).toEqual(corner.commitment);
  });

  it("is fail-closed on inputs no tier would catch for it", () => {
    // An empty preimage: the §5.1 empty field is one byte (`80`), never zero.
    expect(() => plan(Buffer.alloc(0))).toThrow();
    // Above the §5.4 aggregate cap.
    expect(() =>
      plan(Buffer.alloc(MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES + 1)),
    ).toThrow();
    // Out-of-range field index and a short tx id, on a *tier-1* preimage —
    // the tier that derives no certificate and would otherwise never check.
    expect(() =>
      planMidgardFieldCarriage({
        owner: OWNER,
        txId: TX_ID,
        fieldIndex: 9,
        preimage: preimageOf(1),
      }),
    ).toThrow();
    expect(() =>
      planMidgardFieldCarriage({
        owner: OWNER,
        txId: Buffer.alloc(31, 0x5a),
        fieldIndex: FIELD_INDEX,
        preimage: preimageOf(1),
      }),
    ).toThrow();
  });
});

describe("§8.7 healing — content addressing, checked rather than trusted", () => {
  it("makes a second identity's plan interchangeable with the first", () => {
    const preimage = preimageUnder(
      MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
    );
    const original = plan(preimage, OWNER);
    const healed = healMidgardFieldCarriage({
      healer: HEALER,
      txId: TX_ID,
      fieldIndex: FIELD_INDEX,
      preimage,
    });

    expect(midgardFieldCarriagePlansAreInterchangeable(original, healed)).toBe(
      true,
    );
    // Interchangeable, and yet demonstrably a different party: the owner is the
    // one thing that differs, and it is the one thing no consuming step reads.
    expect(healed.certificate?.owner).toEqual(HEALER);
    expect(original.certificate?.owner).toEqual(OWNER);
    expect(healed.certificateAssetName).toEqual(original.certificateAssetName);
    expect(healed.publications.map((entry) => entry.bytes)).toEqual(
      original.publications.map((entry) => entry.bytes),
    );
  });

  it("refuses to call two plans over different content interchangeable", () => {
    const left = plan(
      preimageUnder(MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES),
    );
    // One item shorter: same tier, same field, same transaction, different
    // bytes. This is the case a comparison that only checked the metadata would
    // wave through, and it is the one that matters — a certificate accepted
    // over the wrong chunks is the whole failure mode tier 3 exists to prevent.
    const right = plan(
      preimageUnder(MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES - STRIDE),
    );
    expect(midgardFieldCarriagePlansAreInterchangeable(left, right)).toBe(
      false,
    );
    // And a plan for another field is not interchangeable either, even over
    // byte-identical carriage.
    const otherField = planMidgardFieldCarriage({
      owner: OWNER,
      txId: TX_ID,
      fieldIndex: 0,
      preimage: preimageUnder(MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES),
    });
    expect(midgardFieldCarriagePlansAreInterchangeable(left, otherField)).toBe(
      false,
    );
  });
});

describe("§8.8 layout — carriage and its reference inputs, emitted together", () => {
  it("indexes the manifest first and the chunks in §8.4 order", () => {
    const corner = plan(
      preimageUnder(MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES),
    );
    const layout = layOutMidgardFieldCarriage({ plan: corner, baseIndex: 4 });
    expect(layout.carriage).toEqual({
      carriage: "Certified",
      certRefInputIndex: 4,
      chunkRefInputIndices: [5, 6, 7],
    });
    expect(layout.referenceInputIndices).toEqual([4, 5, 6, 7]);
    // The list the indices point into is the same length and in the same order,
    // which is the property that makes an off-by-one impossible rather than
    // merely unlikely.
    expect(layout.referenceInputs.length).toBe(4);
    expect(layout.referenceInputs[0]?.certificate).toEqual(corner.certificate);
    expect(layout.referenceInputs[1]?.inlineDatumBytes).toEqual(
      corner.publications[0]?.bytes,
    );
    expect(layout.referenceInputs[3]?.inlineDatumBytes).toEqual(
      corner.publications[2]?.bytes,
    );
  });

  it("hands every tier to the same door with the same three arguments", () => {
    const cases = [
      preimageOf(4),
      preimageUnder(MIDGARD_CHUNK_BYTES_K),
      preimageUnder(MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES),
    ];
    const tiers = new Set<string>();
    for (const preimage of cases) {
      const current = plan(preimage);
      tiers.add(current.tier);
      const layout = layOutMidgardFieldCarriage({ plan: current });
      // No tier branch here, and none anywhere downstream of it.
      const view = authenticatedMidgardFieldView({
        fieldIndex: current.fieldIndex,
        txId: current.txId,
        expectedCommitment: current.commitment,
        carriage: layout.carriage,
        referenceInputs: layout.referenceInputs,
      });
      const headerLength =
        preimage[0] === 0x99 ? 3 : preimage[0] === 0x98 ? 2 : 1;
      const lastIndex = (current.totalLength - headerLength) / STRIDE - 1;
      const expectedItem = preimage.subarray(
        headerLength + STRIDE * lastIndex + 2,
        headerLength + STRIDE * lastIndex + 40,
      );
      expect(midgardFieldItemAt(view, lastIndex)).toEqual(expectedItem);
    }
    // The loop really did span the ladder rather than running one tier thrice.
    expect([...tiers].sort()).toEqual(["Certified", "Inline", "RawUtxo"]);
  });

  it("refuses a negative reference-input base", () => {
    const corner = plan(
      preimageUnder(MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES),
    );
    expect(() =>
      layOutMidgardFieldCarriage({ plan: corner, baseIndex: -1 }),
    ).toThrow();
  });
});

describe("§8.3 bounds", () => {
  it("re-exports the table callers must not restate", () => {
    expect(midgardFieldCarriageBounds).toEqual({
      maxTier1RedeemerPreimageBytes: MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
      chunkBytesK: MIDGARD_CHUNK_BYTES_K,
      maxTransactionAggregateFieldBytes:
        MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
      maxTier3ChunkCount: 3,
      maxPublishableCarriageBytes: MIDGARD_MAX_PUBLISHABLE_CARRIAGE_BYTES,
      exactPublishableCarriageBytes: MIDGARD_EXACT_PUBLISHABLE_CARRIAGE_BYTES,
    });
    // The derived relationship §8.3 states, asserted rather than assumed: the
    // chunk count ceiling really is the ceiling of the aggregate cap over K.
    expect(
      Math.ceil(
        MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES / MIDGARD_CHUNK_BYTES_K,
      ),
    ).toBe(midgardFieldCarriageBounds.maxTier3ChunkCount);
  });
});

describe("§8.3 erratum E1 — the publishable frontier", () => {
  it("encodes a Plutus Data byte string at, below and above the 64-byte boundary", () => {
    // At or below 64 bytes a definite byte string; strictly above it, an
    // indefinite-length string of 64-byte chunks. The `>` vs `>=` at exactly 64
    // is the one place this can be wrong without being obvious.
    expect(midgardCarriageDataByteStringBytes(0)).toBe(1);
    expect(midgardCarriageDataByteStringBytes(23)).toBe(24);
    expect(midgardCarriageDataByteStringBytes(24)).toBe(26);
    expect(midgardCarriageDataByteStringBytes(63)).toBe(65);
    expect(midgardCarriageDataByteStringBytes(64)).toBe(66);
    // 65 is 5f + (5840 + 64) + (41 + 1) + ff.
    expect(midgardCarriageDataByteStringBytes(65)).toBe(70);
    // Exactly divisible: no ragged chunk head at all.
    expect(midgardCarriageDataByteStringBytes(14_336)).toBe(14_786);
    expect(midgardCarriageDataByteStringBytes(15_900)).toBe(16_400);
  });

  it("refuses a nonsensical payload length rather than returning a plausible size", () => {
    expect(() => midgardCarriageDataByteStringBytes(-1)).toThrow();
    expect(() => midgardCarriageDataByteStringBytes(1.5)).toThrow();
  });

  it("splits a publication into fixed framing, the datum head and the payload's own encoding", () => {
    for (const payloadBytes of [1_000, 8_000, 14_336, 15_148, 15_644, 15_900]) {
      const datumBytes = midgardCarriageDataByteStringBytes(payloadBytes);
      expect(midgardCarriagePublicationBytes(payloadBytes) - datumBytes).toBe(
        midgardCarriagePublicationFramingBytes(datumBytes),
      );
      // Across the whole ladder the datum sits in [256, 65_536) and the framing
      // is the flat 248 §8.3 E1 publishes.
      expect(midgardCarriagePublicationFramingBytes(datumBytes)).toBe(248);
    }
    // The three figures §8.3 E1 publishes, as the function reproduces them. The
    // emulator suite is what proves these are the *real* signed sizes; this is
    // what proves the constants below are derived from them and not typed in.
    expect(midgardCarriagePublicationBytes(15_900)).toBe(16_648);
    expect(midgardCarriagePublicationBytes(15_644)).toBe(16_384);
    expect(midgardCarriagePublicationBytes(15_148)).toBe(15_872);
  });

  it("pins the framing step function on both sides of every head boundary", () => {
    // The flat 248 an earlier revision published as a constant is a plateau
    // between two steps, and only one of the two edges is safe to be wrong
    // about. Both are pinned here, at the byte, so the collapse cannot be
    // reintroduced silently.
    expect(midgardCarriagePublicationFramingBytes(23)).toBe(246);
    expect(midgardCarriagePublicationFramingBytes(24)).toBe(247);
    expect(midgardCarriagePublicationFramingBytes(255)).toBe(247);
    expect(midgardCarriagePublicationFramingBytes(256)).toBe(248);
    expect(midgardCarriagePublicationFramingBytes(65_535)).toBe(248);
    expect(midgardCarriagePublicationFramingBytes(65_536)).toBe(250);

    // The same boundaries expressed in payload bytes, which is what a caller
    // holds. 22 and 63,548 are the two payloads at which the collapsed model
    // was wrong; the emulator suite measures the low one against a real signed
    // transaction, and the high one is above the §5.4 cap so it can only be
    // modelled — which is exactly why it has to be modelled correctly.
    expect(midgardCarriagePublicationBytes(22)).toBe(269);
    expect(midgardCarriagePublicationBytes(23)).toBe(271);
    expect(midgardCarriagePublicationBytes(245)).toBe(502);
    expect(midgardCarriagePublicationBytes(246)).toBe(504);
    expect(midgardCarriageDataByteStringBytes(63_547)).toBe(65_535);
    expect(midgardCarriageDataByteStringBytes(63_548)).toBe(65_536);
    expect(midgardCarriagePublicationBytes(63_547)).toBe(65_783);
    // Two bytes larger than the collapsed model would have said — the
    // understatement that would have handed a caller an oversized transaction.
    expect(midgardCarriagePublicationBytes(63_548)).toBe(65_786);
  });

  it("pins the payload-proportional and framing figures §8.3 E1 quotes in prose", () => {
    // §8.3 E1 and §8.10 quote these inline. They are derivations of the cost
    // model, so they are asserted here rather than left as prose a reader has
    // to recompute — the same footing as the frontiers themselves.
    const chunkingOverhead = (payloadBytes: number): number =>
      midgardCarriageDataByteStringBytes(payloadBytes) - payloadBytes;
    expect(chunkingOverhead(15_644)).toBe(492);
    expect(chunkingOverhead(15_900)).toBe(500);
    expect(chunkingOverhead(14_336)).toBe(450);

    const nonPayloadFraming = (payloadBytes: number): number =>
      midgardCarriagePublicationBytes(payloadBytes) - payloadBytes;
    expect(nonPayloadFraming(15_644)).toBe(740);
    expect(nonPayloadFraming(15_123)).toBe(723);
    expect(nonPayloadFraming(14_336)).toBe(698);

    // The worked tier-2 example E1 used to show that the (15,148, 15,900]
    // window was unpublishable: 363 over the reserve, 149 under `maxTxSize`. It
    // is now simply above `K`, so tier 2 does not admit it at all.
    expect(midgardCarriagePublicationBytes(15_500)).toBe(16_235);
    expect(
      midgardCarriagePublicationBytes(15_500) -
        (16_384 - MIDGARD_CARRIAGE_RELIABILITY_RESERVE_BYTES),
    ).toBe(363);
    expect(16_384 - midgardCarriagePublicationBytes(15_500)).toBe(149);

    // And the tier-3 half of the outage, which E1's repair closed: at the
    // superseded K = 15,900 every tier-3 plan's first chunk was a full K, 264
    // bytes over `maxTxSize`, and that is the figure that made the window the
    // whole of (15,148, 32,768] rather than the (15,148, 15,900] sliver. Kept as
    // a measurement of the superseded value rather than as prose.
    expect(midgardCarriagePublicationBytes(15_900) - 16_384).toBe(264);
    // The repaired K, and the property that closes the window: a full chunk
    // publishes exactly on the reserve-clearing budget, so it is 512 bytes
    // *under* `maxTxSize` rather than 264 over.
    expect(
      midgardCarriagePublicationBytes(MIDGARD_CHUNK_BYTES_K) - 16_384,
    ).toBe(-512);
    expect(MIDGARD_CHUNK_BYTES_K).toBe(MIDGARD_MAX_PUBLISHABLE_CARRIAGE_BYTES);
  });

  it("derives both frontiers as the largest payload inside each budget", () => {
    expect(MIDGARD_EXACT_PUBLISHABLE_CARRIAGE_BYTES).toBe(15_644);
    expect(MIDGARD_MAX_PUBLISHABLE_CARRIAGE_BYTES).toBe(15_148);
    // A frontier is the *last* payload inside the budget, so the byte after it
    // must be outside — asserted rather than assumed, because an off-by-one in
    // the search would be invisible from the value alone.
    expect(
      midgardCarriagePublicationBytes(MIDGARD_EXACT_PUBLISHABLE_CARRIAGE_BYTES),
    ).toBe(16_384);
    expect(
      midgardCarriagePublicationBytes(
        MIDGARD_EXACT_PUBLISHABLE_CARRIAGE_BYTES + 1,
      ),
    ).toBeGreaterThan(16_384);
    expect(
      midgardCarriagePublicationBytes(MIDGARD_MAX_PUBLISHABLE_CARRIAGE_BYTES),
    ).toBe(16_384 - MIDGARD_CARRIAGE_RELIABILITY_RESERVE_BYTES);
    expect(
      midgardCarriagePublicationBytes(
        MIDGARD_MAX_PUBLISHABLE_CARRIAGE_BYTES + 1,
      ),
    ).toBeGreaterThan(16_384 - MIDGARD_CARRIAGE_RELIABILITY_RESERVE_BYTES);
  });

  it("reports the largest tier-3 plan as publishable, and still names a chunk over a tightened budget", () => {
    const corner = plan(
      preimageUnder(MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES),
    );
    const report = midgardFieldCarriagePublishability({ plan: corner });
    // E1's repair, at the largest plan the format admits. Until the re-pin this
    // row asserted the opposite — chunks 0 and 1 unpublishable at 16,648 signed
    // bytes each — because the chunker cut at a `K` no publication could carry.
    expect(report.publishable).toBe(true);
    expect(report.unpublishableChunks).toEqual([]);
    // And the guard is still a guard: one byte under the full-`K` publication it
    // names the two full chunks and the overrun exactly. Without this the row
    // would have become a gate that cannot fail.
    const tightened = midgardFieldCarriagePublishability({
      plan: corner,
      budgetBytes: midgardCarriagePublicationBytes(MIDGARD_CHUNK_BYTES_K) - 1,
    });
    expect(tightened.publishable).toBe(false);
    expect(
      tightened.unpublishableChunks.map((chunk) => chunk.chunkIndex),
    ).toEqual([0, 1]);
    expect(tightened.unpublishableChunks[0]).toEqual({
      chunkIndex: 0,
      byteLength: MIDGARD_CHUNK_BYTES_K,
      publicationBytes: midgardCarriagePublicationBytes(MIDGARD_CHUNK_BYTES_K),
      overrunBytes: 1,
    });
  });

  it("reports a plan at or under the frontier as publishable", () => {
    const atFrontier = plan(
      preimageUnder(MIDGARD_MAX_PUBLISHABLE_CARRIAGE_BYTES),
    );
    expect(atFrontier.tier).toBe("RawUtxo");
    expect(
      midgardFieldCarriagePublishability({ plan: atFrontier }).publishable,
    ).toBe(true);
    // Tier 1 publishes nothing, so there is nothing to be unpublishable.
    const inline = plan(preimageUnder(4_000));
    expect(inline.tier).toBe("Inline");
    expect(midgardFieldCarriagePublishability({ plan: inline })).toEqual({
      publishable: true,
      budgetBytes: 16_384 - MIDGARD_CARRIAGE_RELIABILITY_RESERVE_BYTES,
      unpublishableChunks: [],
    });
  });

  it("takes an explicit budget, so a measurement can raise it deliberately", () => {
    const corner = plan(
      preimageUnder(MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES),
    );
    expect(
      midgardFieldCarriagePublishability({
        plan: corner,
        budgetBytes: 65_536,
      }).publishable,
    ).toBe(true);
  });
});
