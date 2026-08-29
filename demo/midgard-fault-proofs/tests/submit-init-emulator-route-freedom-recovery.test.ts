/**
 * #621 route freedom, the recovery journeys: no routing input can strand a
 * staged dispute.
 *
 * Journey one drives the inline route into the wall on purpose — the largest
 * §8.4 tier-1 complete item the fixture family can stage projects past the
 * 16,384-byte L1 proof envelope at the observe stage — and pins that the
 * builder refuses **pre-sign** (the emulator never sees an oversized build),
 * records the refusal, publishes the §8 publication itself, and completes by
 * reference on the same staged thread, mid-chain.
 *
 * Journey two hands the builder a publication out-ref that genuinely existed
 * on this ledger and was spent: the refusal is an off-chain error naming the
 * §8.7 content-addressing and both recoveries, and recovery (a) — omit the
 * out-ref and let the builder re-publish — runs to award. (Recovery (b),
 * inline delivery, is the flips file's second journey.)
 *
 * Split from `submit-init-emulator-route-freedom-flips.test.ts` to keep each
 * file's leaked wasm heap far below the ~4 GiB wasm32 ceiling; see
 * tests/support/uplc-heap-guard.ts.
 */

import { MIDGARD_V1_ENVELOPE_MEASUREMENTS } from "@al-ft/midgard-core";
import { MIDGARD_CONSENSUS_LIMITS_V1 } from "@al-ft/midgard-core/consensus-profile-v1";
import { PROTOCOL_PARAMETERS_DEFAULT } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  OPTION_B_SKIP_REASON,
  prepareRouteFreedomJourneyV1,
  realBlueprintSpeaksOptionBV1,
} from "./support/route-freedom-journey.js";

const RELIABLE_DIRECT_PIN =
  MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxReliableDirectCompleteItemBytes;
const MAX_L1_TX_BYTES = PROTOCOL_PARAMETERS_DEFAULT.maxTxSize;

const optionB = realBlueprintSpeaksOptionBV1();
if (!optionB) {
  console.warn(OPTION_B_SKIP_REASON);
}

/** Arm-reachability bookkeeping (#621); see the flips file. */
const exercisedArms = new Set<string>();

describe.skipIf(!optionB)(
  "route freedom: envelope fallback and spent-publication recovery (#621)",
  () => {
    it("refuses an oversized inline door pre-sign and completes by automatic publication fallback", async () => {
      const journey = await prepareRouteFreedomJourneyV1({
        // The largest payload whose complete item still selects §8.4 tier-1
        // `Inline`: one step further and the fixture's carriage goes tier-2
        // `RawUtxo`, which admits no routing override at all.
        inlineDatumPayloadBytes: 13_840,
        minimumCompleteItemBytes: RELIABLE_DIRECT_PIN,
      });
      // Pinned exactly: the journey's whole claim is "tier-1 item whose
      // inline door projects past the envelope", so if the fixture family
      // ever resizes this item the premise must be re-established, not
      // silently drifted past.
      expect(journey.completeItemBytes).toBe(14_324);
      // ... and the fallback publication is possible at all because the item
      // sits under the owner-signed single-publication ceiling.
      expect(journey.completeItemBytes).toBeLessThanOrEqual(
        MIDGARD_CONSENSUS_LIMITS_V1.maxSinglePublicationCompleteItemBytes,
      );

      // The explicit inline request is honored up to the envelope, and the
      // envelope wins: the projected signed observe transaction exceeds
      // 16,384 bytes, the build is refused before signing, and the builder
      // demotes the request to the §8 publication route by itself —
      // liveness over routing preference.
      const semantic = await journey.submitSemanticResolution({
        proofItemDelivery: "inline",
      });
      const result = semantic.result;
      const refusal = result.proofItemInlineEnvelopeRefusal;
      if (refusal === undefined) {
        throw new Error(
          "oversized inline journey recorded no pre-sign envelope refusal",
        );
      }
      expect(refusal.maxTransactionBytes).toBe(MAX_L1_TX_BYTES);
      expect(refusal.projectedSignedBytes).toBeGreaterThan(MAX_L1_TX_BYTES);
      exercisedArms.add("refusal:inline-envelope-pre-sign");

      // The same staged thread completed by reference — the fallback
      // publication was submitted mid-chain, between the source stage and
      // the door that dereferences it, so the capture order pins that the
      // demotion happened at the observe stage rather than up front.
      expect(result.proofItemCarriage).toBe("reference");
      expect(result.proofItemPublication).toBeDefined();
      expect(result.proofItemReferenceOutRef).toBe(
        result.proofItemPublication?.outRef,
      );
      const stageTransactions = result.stageTransactions ?? [];
      expect(stageTransactions).toHaveLength(5);
      expect(semantic.measurements).toHaveLength(6);
      expect(
        semantic.measurements.map(
          (measurement) => measurement.referenceInputCount,
        ),
      ).toEqual([1, 1, 0, 2, 1, 1]);
      // Every transaction that actually reached the emulator fits the L1
      // envelope — the oversized build was refused pre-sign and never
      // submitted, which is the only reason this list can be all-green.
      for (const measurement of semantic.measurements) {
        expect(measurement.completeSignedBytes).toBeLessThanOrEqual(
          MAX_L1_TX_BYTES,
        );
      }
      // The observe stage that finally ran rode the reference route; the
      // recorded projection lives on the refusal, not the stage record.
      const observeStage = stageTransactions.find(
        (stage) => stage.kind === "observe",
      );
      expect(observeStage?.projectedSignedBytes).toBeUndefined();
      exercisedArms.add("fallback:auto-publish-mid-chain");
      exercisedArms.add("observe:reference-after-fallback");

      await journey.submitAward(result.nextThreadOutRef);
      exercisedArms.add("award:after-envelope-fallback");
    }, 900_000);

    it("recovers from a genuinely spent publication out-ref by re-publishing", async () => {
      const journey = await prepareRouteFreedomJourneyV1({
        inlineDatumPayloadBytes: 13_600,
        minimumCompleteItemBytes: RELIABLE_DIRECT_PIN,
      });
      expect(journey.completeItemBytes).toBeGreaterThan(RELIABLE_DIRECT_PIN);

      // The out-ref existed on this very ledger and is spent — the honest
      // shape of "some other flow consumed the publication first". The
      // refusal is off-chain, before any stage transaction exists, and names
      // both recoveries.
      await expect(
        journey.submitSemanticResolution({
          proofItemReferenceOutRef: journey.spentOutRef,
        }),
      ).rejects.toThrow(
        /is spent or missing on chain.*content-addressed.*re-publishing.*inline delivery/u,
      );
      exercisedArms.add("refusal:spent-publication");
      await journey.expectStagedThreadUnspent();

      // Recovery (a): drop the out-ref. The publication is content-addressed
      // (§8.7), so a fresh copy of the same bytes serves; the heuristic
      // routes this item — larger than the re-pinned 13,522-byte heuristic
      // (#622 ruling (b)) — by reference and publishes up front.
      const semantic = await journey.submitSemanticResolution();
      const result = semantic.result;
      expect(result.proofItemCarriage).toBe("reference");
      expect(result.proofItemPublication).toBeDefined();
      expect(result.proofItemReferenceOutRef).toBe(
        result.proofItemPublication?.outRef,
      );
      expect(result.proofItemReferenceOutRef).not.toBe(journey.spentOutRef);
      expect(result.proofItemInlineEnvelopeRefusal).toBeUndefined();
      expect(result.stageTransactions).toHaveLength(5);
      expect(semantic.measurements).toHaveLength(6);
      expect(
        semantic.measurements.map(
          (measurement) => measurement.referenceInputCount,
        ),
      ).toEqual([0, 1, 1, 2, 1, 1]);
      for (const measurement of semantic.measurements) {
        expect(measurement.completeSignedBytes).toBeLessThanOrEqual(
          MAX_L1_TX_BYTES,
        );
      }
      exercisedArms.add("recovery:re-publish");

      await journey.submitAward(result.nextThreadOutRef);
      exercisedArms.add("award:after-re-publish");
    }, 900_000);

    it("exercised every route-freedom arm this file owns", () => {
      expect([...exercisedArms].sort()).toEqual(
        [
          "award:after-envelope-fallback",
          "award:after-re-publish",
          "fallback:auto-publish-mid-chain",
          "observe:reference-after-fallback",
          "recovery:re-publish",
          "refusal:inline-envelope-pre-sign",
          "refusal:spent-publication",
        ].sort(),
      );
    });
  },
);
