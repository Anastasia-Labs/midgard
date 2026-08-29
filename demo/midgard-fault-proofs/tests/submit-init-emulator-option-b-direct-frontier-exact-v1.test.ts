/**
 * #622 measurement campaign, file 2 of 3: the post-Option-B direct-route
 * EXACT frontier — the last §5.1 complete item whose observe door signs
 * inside the 16,384-byte L1 proof envelope at all — and the adjacent-item
 * probe one byte past it.
 *
 *   item 14,004 -> observe signs at 16,369 (fits; the last CONTIGUOUS fit —
 *                  this file's first journey, projection == signed)
 *   item 14,005 -> projected 16,385: the builder refuses PRE-SIGN, records
 *                  the refusal, and completes the same staged thread by the
 *                  automatic §8 publication fallback — the exact frontier +1
 *                  is a route demotion since #621, not the stranding the
 *                  retired pre-change ledger recorded at its own exact+1
 *                  (13,295, "builder refuses").
 *
 * A measurement note the campaign owes honesty about: near the envelope the
 * signed observe size is NOT monotone in item bytes. The measured ladder —
 * 14,004 -> 16,369; 14,005 -> 16,385; 14,006 -> 16,386; 14,007 -> 16,387;
 * 14,008 -> 16,388; 14,009 -> 16,385; 14,017 -> 16,394; 14,019 -> 16,396 —
 * shows a +16 jump and a -4 drop, the transaction-balancing fixed point
 * quantizing across CBOR integer-width boundaries in the fee/change values.
 * Consequences pinned here: NO item in this fixture family signs at exactly
 * 16,384, and the frontier the routing table can actually use is the
 * CONTIGUOUS one — every item up to 14,004 measured or slope-implied inside
 * the envelope, 14,005 through 14,009 measured over it. Isolated larger
 * fits, if any exist inside a later quantization dip, are unusable by a
 * threshold heuristic and are not claimed. Every pin in this file is a
 * measurement, not a model value.
 *
 * Together with file 1 this is the measured post-change frontier pair the
 * owner sign-off table carries: reserve 13,522 / exact 14,004, against the
 * owner-signed pre-change 12,810 / 13,294. The owner answered #619's
 * question (b) on 2026-08-22 and those pins now hold 13,522 / 14,004 in
 * `consensus-profile-v1.ts`, re-pinned at the #617 wave sign-off.
 *
 * Two journeys per file for the wasm32-heap reason; see
 * tests/support/uplc-heap-guard.ts.
 */

import { MIDGARD_CONSENSUS_LIMITS_V1 } from "@al-ft/midgard-core/consensus-profile-v1";
import { PROTOCOL_PARAMETERS_DEFAULT } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  OPTION_B_SKIP_REASON,
  prepareRouteFreedomJourneyV1,
  printRouteFreedomCampaignTableV1,
  realBlueprintSpeaksOptionBV1,
  type RouteFreedomJourneyV1,
} from "./support/route-freedom-journey.js";
import {
  type CompleteSignedTransactionMeasurement,
  expectProofFitV1,
} from "./support/submit-init-emulator-shared.js";

const MAX_L1_TX_BYTES = PROTOCOL_PARAMETERS_DEFAULT.maxTxSize;

/**
 * The measured post-Option-B direct-route exact frontier: the largest §5.1
 * complete-item preimage below which every observe transaction signs inside
 * the 16,384-byte L1 proof envelope (the contiguous frontier; see the file
 * header for the measured quantization ladder above it). Payload 13,529
 * stages exactly this preimage (fixture map: item = datumSize(payload)+49).
 */
const EXACT_FRONTIER_ITEM_BYTES = 14_004;
const EXACT_FRONTIER_PAYLOAD_BYTES = 13_529;
/** The measured signed observe size at the exact frontier (margin 15). */
const EXACT_FRONTIER_OBSERVE_BYTES = 16_369;

/** File 1's item-size-independent six-stage rows, re-pinned at these items. */
const SIX_STAGE_CONSTANT_ROW_BYTES_V1 = {
  prepareSelected: 1_864,
  authenticate: 2_656,
  source: 1_911,
  proof: 1_936,
  settle: 679,
} as const;

const stageBytesByKind = (
  stageTransactions: readonly {
    readonly kind: string;
    readonly completeSignedBytes: number;
  }[],
  kind: string,
): number => {
  const stage = stageTransactions.find((entry) => entry.kind === kind);
  if (stage === undefined) {
    throw new Error(`journey lost its ${kind} stage record`);
  }
  return stage.completeSignedBytes;
};

const lastLifecycleMeasurement = (
  journey: RouteFreedomJourneyV1,
  label: string,
): CompleteSignedTransactionMeasurement => {
  const stage = journey.lifecycleMeasurements.find(
    (entry) => entry.label === label,
  );
  const measurement = stage?.measurements[stage.measurements.length - 1];
  if (measurement === undefined) {
    throw new Error(`journey captured no ${label} stage`);
  }
  return measurement;
};

const expectItemIndependentRows = (
  journey: RouteFreedomJourneyV1,
  stageTransactions: readonly {
    readonly kind: string;
    readonly completeSignedBytes: number;
  }[],
): void => {
  expect(
    lastLifecycleMeasurement(journey, "prepare-selected").completeSignedBytes,
  ).toBe(SIX_STAGE_CONSTANT_ROW_BYTES_V1.prepareSelected);
  expect(stageBytesByKind(stageTransactions, "authenticate")).toBe(
    SIX_STAGE_CONSTANT_ROW_BYTES_V1.authenticate,
  );
  expect(stageBytesByKind(stageTransactions, "source")).toBe(
    SIX_STAGE_CONSTANT_ROW_BYTES_V1.source,
  );
  expect(stageBytesByKind(stageTransactions, "proof")).toBe(
    SIX_STAGE_CONSTANT_ROW_BYTES_V1.proof,
  );
  expect(stageBytesByKind(stageTransactions, "settle")).toBe(
    SIX_STAGE_CONSTANT_ROW_BYTES_V1.settle,
  );
};

/**
 * See file 1: dispute-chain byte + 20%-reserve execution fit (§3.3). The
 * "setup" stage is emulator scaffolding under the relaxed test envelope,
 * excluded by name.
 */
const expectWholeJourneyProofFit = (
  headline: string,
  journey: RouteFreedomJourneyV1,
  semanticMeasurements: readonly CompleteSignedTransactionMeasurement[],
  awardMeasurement: CompleteSignedTransactionMeasurement,
): void => {
  const { maxTxExMem, maxTxExSteps } = journey.emulator.protocolParameters;
  const stages: [string, CompleteSignedTransactionMeasurement][] = [];
  for (const stage of journey.lifecycleMeasurements) {
    if (stage.label === "setup") {
      continue;
    }
    for (const [txIndex, measurement] of stage.measurements.entries()) {
      stages.push([`${stage.label}.${txIndex.toString()}`, measurement]);
    }
  }
  for (const [txIndex, measurement] of semanticMeasurements.entries()) {
    stages.push([`semantic.${txIndex.toString()}`, measurement]);
  }
  stages.push(["award", awardMeasurement]);
  for (const [stage, measurement] of stages) {
    expectProofFitV1({
      stage: `${headline} ${stage}`,
      measurement,
      maxTxExMem,
      maxTxExSteps,
    });
  }
};

const optionB = realBlueprintSpeaksOptionBV1();
if (!optionB) {
  console.warn(OPTION_B_SKIP_REASON);
}

describe.skipIf(!optionB)(
  "post-Option-B direct-route exact frontier (#622)",
  () => {
    it("signs the observe door at the measured 16,369 bytes at the contiguous exact frontier, item 14,004", async () => {
      const journey = await prepareRouteFreedomJourneyV1({
        inlineDatumPayloadBytes: EXACT_FRONTIER_PAYLOAD_BYTES,
        minimumCompleteItemBytes: EXACT_FRONTIER_ITEM_BYTES - 1,
      });
      expect(journey.completeItemBytes).toBe(EXACT_FRONTIER_ITEM_BYTES);

      const semantic = await journey.submitSemanticResolution({
        proofItemDelivery: "inline",
      });
      printRouteFreedomCampaignTableV1(
        "#622 exact-frontier item 14,004",
        journey,
        semantic,
      );
      const result = semantic.result;
      expect(result.proofItemCarriage).toBe("direct");
      expect(result.proofItemPublication).toBeUndefined();
      expect(result.proofItemInlineEnvelopeRefusal).toBeUndefined();
      const stageTransactions = result.stageTransactions ?? [];
      expect(stageTransactions).toHaveLength(5);
      expect(semantic.measurements).toHaveLength(5);

      // The exact-frontier claim: the signed observe transaction fits the
      // envelope at its measured 16,369 bytes (margin 15 — the balancing
      // quantization means no item in this family signs at exactly 16,384;
      // see the file header), and the pre-sign projection measured the same
      // bytes signing produced.
      const observeBytes = stageBytesByKind(stageTransactions, "observe");
      expect(observeBytes).toBe(EXACT_FRONTIER_OBSERVE_BYTES);
      expect(observeBytes).toBeLessThanOrEqual(MAX_L1_TX_BYTES);
      const observeStage = stageTransactions.find(
        (stage) => stage.kind === "observe",
      );
      expect(observeStage?.projectedSignedBytes).toBe(observeBytes);

      // Every other stage holds file 1's item-independent literals.
      expectItemIndependentRows(journey, stageTransactions);

      const award = await journey.submitAward(result.nextThreadOutRef);
      expectWholeJourneyProofFit(
        "#622 exact-frontier item 14,004",
        journey,
        semantic.measurements,
        award.measurement,
      );
    }, 900_000);

    it("refuses item 14,005 pre-sign at a projected 16,385 bytes and completes by automatic publication fallback — demotion, not stranding", async () => {
      const journey = await prepareRouteFreedomJourneyV1({
        inlineDatumPayloadBytes: EXACT_FRONTIER_PAYLOAD_BYTES + 1,
        minimumCompleteItemBytes: EXACT_FRONTIER_ITEM_BYTES,
      });
      expect(journey.completeItemBytes).toBe(EXACT_FRONTIER_ITEM_BYTES + 1);
      // The fallback is available at all because the item sits under the
      // owner-signed single-publication ceiling.
      expect(journey.completeItemBytes).toBeLessThanOrEqual(
        MIDGARD_CONSENSUS_LIMITS_V1.maxSinglePublicationCompleteItemBytes,
      );

      const semantic = await journey.submitSemanticResolution({
        proofItemDelivery: "inline",
      });
      printRouteFreedomCampaignTableV1(
        "#622 exact-frontier+1 item 14,005",
        journey,
        semantic,
      );
      const result = semantic.result;

      // The adjacent-item probe: one preimage byte past the frontier lands
      // the projection at 16,385 — one byte over the envelope, though by way
      // of the measured +16 quantization jump (the balancing fixed point
      // crossed a CBOR width boundary), not by +1 — and the refusal records
      // both numbers.
      const refusal = result.proofItemInlineEnvelopeRefusal;
      if (refusal === undefined) {
        throw new Error(
          "exact-frontier+1 journey recorded no pre-sign envelope refusal",
        );
      }
      expect(refusal.maxTransactionBytes).toBe(MAX_L1_TX_BYTES);
      expect(refusal.projectedSignedBytes).toBe(MAX_L1_TX_BYTES + 1);

      // ... and the same staged thread completes by reference: the builder
      // published the §8 publication itself, mid-chain (#621's fallback).
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
      // The reference-route observe door does not carry the preimage; the
      // recorded projection lives on the refusal, not the stage record.
      const observeStage = stageTransactions.find(
        (stage) => stage.kind === "observe",
      );
      expect(observeStage?.projectedSignedBytes).toBeUndefined();

      // The demoted journey's non-observe stages still hold the
      // item-independent literals — demotion changed the route, not the
      // stage machinery.
      expectItemIndependentRows(journey, stageTransactions);

      const award = await journey.submitAward(result.nextThreadOutRef);
      expectWholeJourneyProofFit(
        "#622 exact-frontier+1 item 14,005",
        journey,
        semantic.measurements,
        award.measurement,
      );
    }, 900_000);
  },
);
