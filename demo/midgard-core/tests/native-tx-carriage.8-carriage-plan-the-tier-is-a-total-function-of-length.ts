import { describe, expect, it } from "vitest";

import {
  healMidgardFieldCarriage,
  layOutMidgardFieldCarriage,
  MIDGARD_EXACT_PUBLISHABLE_CARRIAGE_BYTES,
  MIDGARD_MAX_PUBLISHABLE_CARRIAGE_BYTES,
  midgardFieldCarriageBounds,
  midgardFieldCarriagePlansAreInterchangeable,
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
export const preimageUnder = (bytes: number): Buffer => {
  for (let count = Math.floor(bytes / STRIDE) + 1; count >= 0; count -= 1) {
    const candidate = preimageOf(count);
    if (candidate.length <= bytes) {
      return candidate;
    }
  }
  throw new Error("no item count fits");
};

export const plan = (preimage: Buffer, owner: Uint8Array = OWNER) =>
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
