/**
 * #622 reference-route frontier and sweep-shape fit campaign.
 * The reference journey exercises the tier-1 ceiling of 14,336 bytes;
 * the adjacent tier-2 item remains an explicit carriage refusal.
 * The sweep journey retains its historical item shape and verifies current
 * proof fit and the removal of the claim-registry reference witness.
 * Historical execution costs predate the computation-thread ADA-preservation
 * guard, so they cannot bound execution costs of this validator build.
 *
 * Two journeys per file. The split was made while `@lucid-evolution/uplc`
 * (through 0.2.22) leaked wasm linear memory on every script evaluation and
 * vitest isolates per FILE; that leak is fixed upstream, and the split is kept
 * so each file runs in its own fresh process.
 */

import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import {
  MIDGARD_ENVELOPE_MEASUREMENTS,
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
} from "@al-ft/midgard-core";
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { describe, expect, it } from "vitest";

import {
  assertRealBlueprintSpeaksOptionBV1,
  prepareRouteFreedomJourney,
  printRouteFreedomCampaignTable,
  type RouteFreedomJourney,
} from "./support/route-freedom-journey.js";
import { buildInvalidForcedValidationDisputeFixture } from "./support/submit-init-emulator-fixtures.js";
import {
  type CompleteSignedTransactionMeasurement,
  expectProofFit,
} from "./support/submit-init-emulator-shared.js";

const RELIABLE_DIRECT_PIN =
  MIDGARD_ENVELOPE_MEASUREMENTS.maxReliableDirectCompleteItemBytes;

/** Payload staging exactly the tier-1 ceiling preimage of 14,336 bytes. */
const MAX_TIER1_PAYLOAD_BYTES = 13_851;

/** Historical item shape; historical execution rows are not current limits. */
const SWEEP_PAYLOAD_BYTES = 7_976;
const SWEEP_ITEM_BYTES = 8_277;

const preChangeBaseline = JSON.parse(
  readFileSync(
    fileURLToPath(
      new URL(
        "./fixtures/pre-option-b-resolver-proof-fit-sweep-baseline-v1.json",
        import.meta.url,
      ),
    ),
    "utf8",
  ),
) as {
  readonly provenance: {
    readonly sourceCommit: string;
    readonly sweepPayloadBytes: number;
    readonly sweepCompleteItemBytes: number;
  };
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

describe("post-Option-B reference-route frontier and sweep-shape fit (#622)", () => {
  it("measures the full reference journey at the tier-1 ceiling item 14,336 — the reference route's own frontier", async () => {
    const journey = await prepareRouteFreedomJourney({
      inlineDatumPayloadBytes: MAX_TIER1_PAYLOAD_BYTES,
      minimumCompleteItemBytes: MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES - 1,
    });
    expect(journey.completeItemBytes).toBe(
      MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
    );
    // The route is open at this size only because the tier-1 ceiling sits
    // under the owner-signed single-publication ceiling (60 bytes apart).
    expect(journey.completeItemBytes).toBeLessThanOrEqual(
      MIDGARD_CONSENSUS_LIMITS.maxSinglePublicationCompleteItemBytes,
    );

    // No routing input: the build-time heuristic itself carries anything
    // past the owner-signed direct frontier — re-pinned 12,810 -> 13,522 at
    // the #617 wave sign-off (#622 ruling (b)) — by reference.
    const semantic = await journey.submitSemanticResolution();
    printRouteFreedomCampaignTable(
      "#622 reference-frontier item 14,336",
      journey,
      semantic,
    );
    const result = semantic.result;
    expect(result.proofItemCarriage).toBe("reference");
    expect(result.proofItemPublication).toBeDefined();
    expect(result.proofItemReferenceOutRef).toBe(
      result.proofItemPublication?.outRef,
    );
    expect(result.proofItemInlineEnvelopeRefusal).toBeUndefined();
    const stageTransactions = result.stageTransactions ?? [];
    expect(stageTransactions).toHaveLength(5);
    expect(semantic.measurements).toHaveLength(6);
    expect(
      semantic.measurements.map(
        (measurement) => measurement.referenceInputCount,
      ),
    ).toEqual([0, 1, 1, 2, 1, 1]);

    const observeStage = stageTransactions.find(
      (stage) => stage.kind === "observe",
    );
    expect(observeStage?.projectedSignedBytes).toBeUndefined();

    const award = await journey.submitAward(result.nextThreadOutRef);
    expectWholeJourneyProofFit(
      "#622 reference-frontier item 14,336",
      journey,
      semantic.measurements,
      award.measurement,
    );
  }, 900_000);

  it("fits the sweep shape without the removed claim-registry reference witness", async () => {
    const journey = await prepareRouteFreedomJourney({
      inlineDatumPayloadBytes: SWEEP_PAYLOAD_BYTES,
      minimumCompleteItemBytes: 0,
    });
    // The baseline is only a baseline for THIS shape.
    expect(preChangeBaseline.provenance.sweepPayloadBytes).toBe(
      SWEEP_PAYLOAD_BYTES,
    );
    expect(preChangeBaseline.provenance.sweepCompleteItemBytes).toBe(
      SWEEP_ITEM_BYTES,
    );
    expect(journey.completeItemBytes).toBe(SWEEP_ITEM_BYTES);
    expect(journey.completeItemBytes).toBeLessThanOrEqual(RELIABLE_DIRECT_PIN);

    // No routing input: the heuristic rides a sweep-shaped item inline.
    const semantic = await journey.submitSemanticResolution();
    printRouteFreedomCampaignTable(
      "#622 sweep-shape item 8,277",
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
    expect(
      semantic.measurements.map(
        (measurement) => measurement.referenceInputCount,
      ),
    ).toEqual([1, 1, 1, 1, 1]);

    const award = await journey.submitAward(result.nextThreadOutRef);
    expectWholeJourneyProofFit(
      "#622 sweep-shape item 8,277",
      journey,
      semantic.measurements,
      award.measurement,
    );
  }, 900_000);

  it("cannot stage the reference frontier + 1: item 14,337 is tier-2 carriage, deferred to #617's tiers revival", async () => {
    // The boundary pair, fixture-level (no emulator): the tier-1 ceiling
    // itself stages...
    const atCeiling = await buildInvalidForcedValidationDisputeFixture({
      operatorVkey: "11".repeat(28),
      now: 1_700_000_000_000,
      inlineDatumPayloadBytes: MAX_TIER1_PAYLOAD_BYTES,
      minimumCompleteItemBytes: 0,
    });
    expect(atCeiling.completeItemBytes).toBe(
      MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
    );
    // ... and one payload byte further the §8.4 partition says RawUtxo,
    // which the evidence bundle's inline-only carriage resolution cannot
    // thread — the fixture family's hard edge, measured as such. The
    // tier-2/3 journey machinery is #617's owed revival; until it lands,
    // "reference frontier + 1" is a build refusal, not a measurement.
    // The refusal must be the §8.4 partition's, naming the item that
    // crossed the tier-1 cap — not any failure the builder happens to
    // raise one payload byte later.
    await expect(
      buildInvalidForcedValidationDisputeFixture({
        operatorVkey: "11".repeat(28),
        now: 1_700_000_000_000,
        inlineDatumPayloadBytes: MAX_TIER1_PAYLOAD_BYTES + 1,
        minimumCompleteItemBytes: 0,
      }),
    ).rejects.toThrow(
      new RegExp(
        "has a " +
          String(MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES + 1) +
          "-byte .*preimage, which .*carries as tier-2 `RawUtxo` rather " +
          "than tier-1 `Inline` \\(cap " +
          String(MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES) +
          " bytes\\)",
        "u",
      ),
    );
  }, 300_000);
});
