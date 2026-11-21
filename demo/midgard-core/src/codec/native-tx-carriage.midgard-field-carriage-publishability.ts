import {
  MAX_L1_TX_BYTES,
  MIDGARD_CARRIAGE_RELIABILITY_RESERVE_BYTES,
  midgardCarriagePublicationBytes,
} from "./native-tx-carriage.lay-out-midgard-field-carriage.js";
import { type MidgardFieldCarriagePlan } from "./native-tx-carriage.plan-midgard-field-carriage.js";
import {
  MIDGARD_CHUNK_BYTES_K,
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
  MIDGARD_MAX_TIER3_CHUNK_COUNT,
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
} from "./native-tx-field-access.js";

const largestPayloadWithin = (budget: number): number => {
  let payload = budget;
  while (payload > 0 && midgardCarriagePublicationBytes(payload) > budget) {
    payload -= 1;
  }
  return payload;
};

/**
 * §8.3 erratum E1. The largest payload whose signed publication lands **on**
 * `maxTxSize` — 15,644 bytes, in a 16,384-byte transaction.
 *
 * Derived from {@link midgardCarriagePublicationBytes} rather than written
 * down, so the frontier and the cost model can never disagree; the emulator
 * measurement pins both against real transactions.
 */
export const MIDGARD_EXACT_PUBLISHABLE_CARRIAGE_BYTES =
  largestPayloadWithin(MAX_L1_TX_BYTES);

/**
 * §8.3 erratum E1. **The bound a publisher builds against**: the largest
 * payload whose signed publication clears `maxTxSize` with the 512-byte
 * reliability reserve — 15,148 bytes, in a 15,872-byte transaction.
 *
 * This is the operative limit on §8.5 raw carriage, and since E1's repair landed
 * it is **exactly** `chunk_bytes_k` ({@link MIDGARD_CHUNK_BYTES_K}): the
 * chunker cuts at the publishable frontier, so every chunk of every tier-3 plan
 * publishes, and a tier-2 preimage is admissible precisely when it fits one
 * publication. The two are asserted equal in
 * `demo/midgard-core/tests/native-tx-carriage.test.ts` rather than left to
 * coincide — that assertion is the whole of E1's repair as a checkable property.
 *
 * Before the repair `K` read 15,900, a 15,900-byte chunk measured 16,648 signed
 * (264 over `maxTxSize`), and because the chunker cut at `K` the unpublishable
 * window was the whole of `(15,148, 32,768]` rather than a tier-2 sliver — tier 3
 * did not function at any preimage size. {@link
 * midgardFieldCarriagePublishability} is the guard that made that failure
 * visible at build time; it stays, because the frontier is a measurement and a
 * caller may still exceed it deliberately (a raised-limit measurement, or a
 * future `maxTxSize`).
 */
export const MIDGARD_MAX_PUBLISHABLE_CARRIAGE_BYTES = largestPayloadWithin(
  MAX_L1_TX_BYTES - MIDGARD_CARRIAGE_RELIABILITY_RESERVE_BYTES,
);

/** One publication that a plan requires and `maxTxSize` will not accept. */
export type MidgardUnpublishableChunk = {
  readonly chunkIndex: number;
  readonly byteLength: number;
  /** Size of the signed publication transaction these bytes would produce. */
  readonly publicationBytes: number;
  /** By how much it exceeds the budget it was judged against. */
  readonly overrunBytes: number;
};

/**
 * Whether every publication a plan requires can actually be published, and if
 * not, exactly which ones cannot and by how much.
 *
 * **Why this is a report and not an exception at plan time — and why §8.3 E1's
 * prohibition is worded against publication.** A plan is a statement about
 * bytes, and the §8.4 split is a pure function that healing and certification
 * both depend on reproducing; making {@link planMidgardFieldCarriage} refuse
 * would take the erratum's diagnosis away from the caller who needs it and make
 * the measurement that found it unrunnable. E1 therefore prohibits
 * **publishing** carriage above the frontier, not planning it, and the refusal
 * lives where a transaction is actually built: the SDK's
 * `buildUnsignedFieldPreimagePublicationV1Program` consumes this report.
 *
 * With E1's repair applied the chunker cuts at the frontier, so an honest §8.4
 * plan now reports `publishable: true` at every preimage size up to the §5.4
 * cap. What this function still catches is the two cases that are not an honest
 * plan at the compiled `K`: a chunk list assembled by hand or by an older
 * chunker, and a deliberate raised-limit measurement. Before the repair it
 * caught *every* tier-3 plan, which is what made the outage visible at build
 * time instead of at submission.
 *
 * `budgetBytes` defaults to the reliable frontier's transaction budget. A
 * caller measuring the frontier itself passes a larger one deliberately.
 */
export const midgardFieldCarriagePublishability = ({
  plan,
  budgetBytes = MAX_L1_TX_BYTES - MIDGARD_CARRIAGE_RELIABILITY_RESERVE_BYTES,
}: {
  readonly plan: MidgardFieldCarriagePlan;
  readonly budgetBytes?: number;
}): {
  readonly publishable: boolean;
  readonly budgetBytes: number;
  readonly unpublishableChunks: readonly MidgardUnpublishableChunk[];
} => {
  const unpublishableChunks = plan.publications.flatMap((publication) => {
    const publicationBytes = midgardCarriagePublicationBytes(
      publication.bytes.length,
    );
    return publicationBytes <= budgetBytes
      ? []
      : [
          {
            chunkIndex: publication.chunkIndex,
            byteLength: publication.bytes.length,
            publicationBytes,
            overrunBytes: publicationBytes - budgetBytes,
          },
        ];
  });
  return {
    publishable: unpublishableChunks.length === 0,
    budgetBytes,
    unpublishableChunks,
  };
};

/**
 * The §8.3 table, as the one place a builder asks "will this fit that tier?".
 *
 * Exposed as one frozen object rather than left to callers comparing against
 * the constants, because the tier-1 bound is still **provisional pending
 * Phase-4 measurement** (§8.3) and a re-pin has to move every call site at
 * once. `maxPublishableCarriageBytes` is the row erratum E1 added; since E1's
 * repair landed it **equals** `chunkBytesK`, which is what makes the ladder
 * publishable end to end, and the two are asserted equal in this module's tests.
 */
export const midgardFieldCarriageBounds = Object.freeze({
  maxTier1RedeemerPreimageBytes: MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
  chunkBytesK: MIDGARD_CHUNK_BYTES_K,
  maxTransactionAggregateFieldBytes:
    MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
  maxTier3ChunkCount: MIDGARD_MAX_TIER3_CHUNK_COUNT,
  maxPublishableCarriageBytes: MIDGARD_MAX_PUBLISHABLE_CARRIAGE_BYTES,
  exactPublishableCarriageBytes: MIDGARD_EXACT_PUBLISHABLE_CARRIAGE_BYTES,
});
