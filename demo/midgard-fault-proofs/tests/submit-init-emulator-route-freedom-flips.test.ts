/**
 * #621 route freedom, the flip journeys: since Option B the committed
 * evidence is transition-only, so how a tier-1 complete item's preimage
 * reaches the §8.8 door is a build-time routing decision — and this file
 * proves the freedom by flipping both defaults against the same staged
 * machinery the production submitter drives.
 *
 * Each journey is a genuine full lifecycle on the emulator (real applied
 * validators, local UPLC evaluation, sign, submit): a hostile routing input
 * is refused off chain with the staged thread untouched, then the flipped
 * route runs the staged chain to settlement and the dispute to award.
 *
 * The heuristic threshold pinned here is the owner-signed
 * `maxReliableDirectCompleteItemBytes` = 13,522 — a cost steer since Option
 * B, not a soundness bound. #622 re-measured it and the owner approved the
 * lane-level rebind 12,810 -> 13,522 at the #617 wave sign-off (ruling (b),
 * 2026-08-22); this file reads the constant rather than a literal.
 *
 * Split from `submit-init-emulator-route-freedom-recovery.test.ts` to keep
 * each file's leaked wasm heap far below the ~4 GiB wasm32 ceiling; see
 * tests/support/uplc-heap-guard.ts.
 */

import { MIDGARD_V1_ENVELOPE_MEASUREMENTS } from "@al-ft/midgard-core";
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
const NEVER_EXISTED_OUT_REF = `${"0".repeat(64)}#0`;

const optionB = realBlueprintSpeaksOptionBV1();
if (!optionB) {
  console.warn(OPTION_B_SKIP_REASON);
}

/**
 * Arm-reachability bookkeeping (#621): every refusal and route arm this file
 * owns must actually fire; the closing test refuses a silently-vacuous suite.
 */
const exercisedArms = new Set<string>();

describe.skipIf(!optionB)(
  "route freedom: build-time delivery flips (#621)",
  () => {
    it("flips a heuristically-inline item onto the reference route on request, refusing the contradictory inline+out-ref pairing first", async () => {
      const journey = await prepareRouteFreedomJourneyV1({
        inlineDatumPayloadBytes: 7_976,
        minimumCompleteItemBytes: 0,
      });
      // The flip's premise: this item is small enough that the cost heuristic
      // alone rides inline (the recorded direct row stages this same payload
      // and does exactly that).
      expect(journey.completeItemBytes).toBeLessThanOrEqual(
        RELIABLE_DIRECT_PIN,
      );

      // Hostile routing input: "inline" beside a publication out-ref is a
      // contradiction, refused by the build-time resolver before the out-ref
      // is even fetched — nothing is published, nothing is submitted.
      await expect(
        journey.submitSemanticResolution({
          proofItemDelivery: "inline",
          proofItemReferenceOutRef: NEVER_EXISTED_OUT_REF,
        }),
      ).rejects.toThrow(/"inline" contradicts `proofItemReferenceOutRef`/u);
      exercisedArms.add("refusal:inline-contradicts-out-ref");
      await journey.expectStagedThreadUnspent();

      // The flip: an explicit "reference" request overrides the heuristic —
      // the builder publishes the §8 publication up front and the door
      // dereferences it.
      const semantic = await journey.submitSemanticResolution({
        proofItemDelivery: "reference",
      });
      const result = semantic.result;
      expect(result.proofItemCarriage).toBe("reference");
      expect(result.proofItemPublication).toBeDefined();
      expect(result.proofItemReferenceOutRef).toBe(
        result.proofItemPublication?.outRef,
      );
      expect(result.proofItemInlineEnvelopeRefusal).toBeUndefined();
      const stageTransactions = result.stageTransactions ?? [];
      expect(stageTransactions).toHaveLength(5);
      // Publication first, then the five stage transactions; the publication
      // is dereferenced exactly once, at the observe stage's door, beside the
      // published observe validator (#620/#621).
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
      // The pre-sign envelope projection is the inline route's gate; the
      // reference route records none.
      const observeStage = stageTransactions.find(
        (stage) => stage.kind === "observe",
      );
      expect(observeStage?.projectedSignedBytes).toBeUndefined();
      exercisedArms.add("route:forced-reference");
      exercisedArms.add("observe:reference");

      const award = await journey.submitAward(result.nextThreadOutRef);
      expect(award.result.fraudProofUnit).toMatch(/^[0-9a-f]+$/u);
      exercisedArms.add("award:after-forced-reference");
    }, 900_000);

    it("flips a heuristically-reference item onto the inline route on request, recovering from a missing publication out-ref", async () => {
      const journey = await prepareRouteFreedomJourneyV1({
        inlineDatumPayloadBytes: 13_100,
        minimumCompleteItemBytes: RELIABLE_DIRECT_PIN,
      });
      // The flip's premise: past the owner-signed 13,522-byte measurement
      // (#622 ruling (b), re-pinned at the #617 wave sign-off) the heuristic
      // rides by reference (the recorded reference row does). The payload
      // tracks the pin — it stages an item just over 13,522 and well inside
      // the contiguous 14,058 inline fit, so the flip below is a routing
      // choice rather than an envelope accident.
      expect(journey.completeItemBytes).toBeGreaterThan(RELIABLE_DIRECT_PIN);

      // Hostile routing input: an out-ref that never existed. The refusal is
      // off-chain, names the out-ref, and names both recoveries (#621) —
      // and the staged thread is untouched, so either recovery targets it.
      await expect(
        journey.submitSemanticResolution({
          proofItemReferenceOutRef: NEVER_EXISTED_OUT_REF,
        }),
      ).rejects.toThrow(
        /is spent or missing on chain.*content-addressed.*re-publishing.*inline delivery/u,
      );
      exercisedArms.add("refusal:missing-publication");
      await journey.expectStagedThreadUnspent();

      // Recovery (b), which is also the flip: explicit inline delivery — the
      // 13k-byte preimage rides the observe redeemer, no publication exists
      // anywhere, and the projected signed size clears the L1 envelope.
      const semantic = await journey.submitSemanticResolution({
        proofItemDelivery: "inline",
      });
      const result = semantic.result;
      expect(result.proofItemCarriage).toBe("direct");
      expect(result.proofItemPublication).toBeUndefined();
      expect(result.proofItemReferenceOutRef).toBeUndefined();
      expect(result.proofItemInlineEnvelopeRefusal).toBeUndefined();
      const stageTransactions = result.stageTransactions ?? [];
      expect(stageTransactions).toHaveLength(5);
      expect(semantic.measurements).toHaveLength(5);
      expect(
        semantic.measurements.map(
          (measurement) => measurement.referenceInputCount,
        ),
      ).toEqual([1, 1, 1, 1, 1]);
      // Projection exactness: the CML dummy-witness projection the pre-sign
      // gate ran is byte-identical to what signing actually produced — the
      // gate measures the true envelope, not an estimate.
      const observeStage = stageTransactions.find(
        (stage) => stage.kind === "observe",
      );
      if (observeStage === undefined) {
        throw new Error("inline journey lost its observe stage record");
      }
      expect(observeStage.projectedSignedBytes).toBe(
        observeStage.completeSignedBytes,
      );
      expect(observeStage.completeSignedBytes).toBeLessThanOrEqual(
        MAX_L1_TX_BYTES,
      );
      exercisedArms.add("route:forced-inline");
      exercisedArms.add("observe:inline");
      exercisedArms.add("pin:projection-exact");

      await journey.submitAward(result.nextThreadOutRef);
      exercisedArms.add("award:after-forced-inline");
    }, 900_000);

    it("exercised every route-freedom arm this file owns", () => {
      expect([...exercisedArms].sort()).toEqual(
        [
          "award:after-forced-inline",
          "award:after-forced-reference",
          "observe:inline",
          "observe:reference",
          "pin:projection-exact",
          "refusal:inline-contradicts-out-ref",
          "refusal:missing-publication",
          "route:forced-inline",
          "route:forced-reference",
        ].sort(),
      );
    });
  },
);
