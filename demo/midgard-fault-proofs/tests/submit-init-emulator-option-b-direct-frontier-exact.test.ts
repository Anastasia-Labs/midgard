/**
 * Real-contract inline-envelope checks around the measured direct-route limit.
 * Fee/change CBOR widths and generated accounts can move the exact frontier.
 * Keep one fitting item and an over-limit item separated by a full 64-byte
 * datum chunk, then assert measured transaction sizes and automatic fallback.
 * The old adjacent-item pin (14,059 -> 16,385) is no longer reproducible:
 * that item can sign at 16,370 bytes.
 */

import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { PROTOCOL_PARAMETERS_DEFAULT } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  assertRealBlueprintSpeaksOptionBV1,
  prepareRouteFreedomJourney,
  printRouteFreedomCampaignTable,
  type RouteFreedomJourney,
} from "./support/route-freedom-journey.js";
import {
  type CompleteSignedTransactionMeasurement,
  expectProofFit,
} from "./support/submit-init-emulator-shared.js";

const MAX_L1_TX_BYTES = PROTOCOL_PARAMETERS_DEFAULT.maxTxSize;

const FITTING_ITEM_BYTES = 14_058;
const FITTING_PAYLOAD_BYTES = 13_582;
// A 64-byte payload increase adds one two-byte CBOR byte-string chunk header.
const FALLBACK_ITEM_BYTES = FITTING_ITEM_BYTES + 66;
const FALLBACK_PAYLOAD_BYTES = FITTING_PAYLOAD_BYTES + 64;

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

/**
 * See file 1: dispute-chain byte + 20%-reserve execution fit (§3.3). The
 * "setup" stage is emulator scaffolding under the relaxed test envelope,
 * excluded by name.
 */
const expectWholeJourneyProofFit = (
  headline: string,
  journey: RouteFreedomJourney,
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
    expectProofFit({
      stage: `${headline} ${stage}`,
      measurement,
      maxTxExMem,
      maxTxExSteps,
    });
  }
};

assertRealBlueprintSpeaksOptionBV1();

describe("inline proof-item transaction envelope", () => {
  it("signs a near-limit inline observe transaction with an exact size projection", async () => {
    const journey = await prepareRouteFreedomJourney({
      inlineDatumPayloadBytes: FITTING_PAYLOAD_BYTES,
      minimumCompleteItemBytes: FITTING_ITEM_BYTES - 1,
    });
    expect(journey.completeItemBytes).toBe(FITTING_ITEM_BYTES);

    const semantic = await journey.submitSemanticResolution({
      proofItemDelivery: "inline",
    });
    printRouteFreedomCampaignTable(
      "fitting inline item 14,058",
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

    // Admission must project the exact signed bytes, within the L1 envelope.
    const observeBytes = stageBytesByKind(stageTransactions, "observe");
    expect(observeBytes).toBeLessThanOrEqual(MAX_L1_TX_BYTES);
    const observeStage = stageTransactions.find(
      (stage) => stage.kind === "observe",
    );
    expect(observeStage?.projectedSignedBytes).toBe(observeBytes);

    const award = await journey.submitAward(result.nextThreadOutRef);
    expectWholeJourneyProofFit(
      "fitting inline item 14,058",
      journey,
      semantic.measurements,
      award.measurement,
    );
  }, 900_000);

  it("refuses an over-limit inline observe transaction pre-sign and completes by automatic publication fallback", async () => {
    const journey = await prepareRouteFreedomJourney({
      inlineDatumPayloadBytes: FALLBACK_PAYLOAD_BYTES,
      minimumCompleteItemBytes: FITTING_ITEM_BYTES,
    });
    expect(journey.completeItemBytes).toBe(FALLBACK_ITEM_BYTES);
    // The fallback is available at all because the item sits under the
    // owner-signed single-publication ceiling.
    expect(journey.completeItemBytes).toBeLessThanOrEqual(
      MIDGARD_CONSENSUS_LIMITS.maxSinglePublicationCompleteItemBytes,
    );

    const semantic = await journey.submitSemanticResolution({
      proofItemDelivery: "inline",
    });
    printRouteFreedomCampaignTable(
      "over-limit inline item 14,124",
      journey,
      semantic,
    );
    const result = semantic.result;

    // Refuse before signing, recording both the projection and actual limit.
    const refusal = result.proofItemInlineEnvelopeRefusal;
    if (refusal === undefined) {
      throw new Error(
        "over-limit inline journey recorded no pre-sign envelope refusal",
      );
    }
    expect(refusal.maxTransactionBytes).toBe(MAX_L1_TX_BYTES);
    expect(refusal.projectedSignedBytes).toBeGreaterThan(MAX_L1_TX_BYTES);

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

    const award = await journey.submitAward(result.nextThreadOutRef);
    expectWholeJourneyProofFit(
      "over-limit inline item 14,124",
      journey,
      semantic.measurements,
      award.measurement,
    );
  }, 900_000);
});
