import { describe, expect, it } from "vitest";

import {
  MIDGARD_CARRIAGE_RELIABILITY_RESERVE_BYTES,
  MIDGARD_EXACT_PUBLISHABLE_CARRIAGE_BYTES,
  MIDGARD_MAX_PUBLISHABLE_CARRIAGE_BYTES,
  midgardCarriageDataByteStringBytes,
  midgardCarriagePublicationBytes,
  midgardCarriagePublicationFramingBytes,
  midgardFieldCarriagePublishability,
} from "../src/codec/native-tx-carriage.js";
import {
  MIDGARD_CHUNK_BYTES_K,
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
} from "../src/codec/native-tx-field-access.js";
import {
  plan,
  preimageUnder,
} from "./native-tx-carriage.8-carriage-plan-the-tier-is-a-total-function-of-length.js";

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
