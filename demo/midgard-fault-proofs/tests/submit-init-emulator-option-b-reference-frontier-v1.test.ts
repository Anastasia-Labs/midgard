/**
 * #622 measurement campaign, file 3 of 3: the reference route measured
 * end-to-end at its own frontier, and the like-for-like execution-unit
 * comparison against the pre-change sweep baseline at the sweep's own shape.
 *
 * The reference route's frontier is NOT an envelope: the publication and the
 * by-reference door are both far inside 16,384 at every stageable size. It
 * is the §8.4 tier partition — the largest §5.1 preimage this fixture family
 * can stage at all is tier-1's 14,336 (`MIDGARD_MAX_TIER1_REDEEMER_
 * PREIMAGE_BYTES_V1`); one payload byte further the item is tier-2 RawUtxo
 * and the fixture build refuses outright (this file's closing probe). So:
 *
 *   item 14,336 -> full reference journey, publication + five stages, every
 *                  transaction measured, end-to-end byte total pinned
 *   item 14,337 -> not stageable: tier-2 carriage, which the evidence
 *                  bundle's inline-only resolution cannot thread — the
 *                  tiers-2/3 journey revival is #617's owed checklist row,
 *                  recorded in the owner table as deferred, not measured
 *
 * The second journey re-runs the resolver proof-fit sweep's exact shape
 * (payload 7,976 -> item 8,277) through the post-Option-B chain and compares
 * against the committed pre-change sweep rows
 * (`demo/midgard-validation/tests/fixtures/resolver-proof-fit-sweep-v1
 * .generated.json`, measured at 2476d358 with the pre-#620 blueprint):
 * untouched stages must bill byte- and unit-identically, the reworked
 * stages must have become strictly cheaper, and the observe door — same
 * wire bytes, one hash fewer in the bill since #620 deleted the frozen-hash
 * equality — must bill strictly below its pre-change row. The sweep fixture
 * itself is NOT regenerated here: that regeneration rides #617's batched
 * ABI wave.
 *
 * Two journeys per file for the wasm32-heap reason; see
 * tests/support/uplc-heap-guard.ts.
 */

import {
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1,
  MIDGARD_V1_ENVELOPE_MEASUREMENTS,
} from "@al-ft/midgard-core";
import { MIDGARD_CONSENSUS_LIMITS_V1 } from "@al-ft/midgard-core/consensus-profile-v1";
import { describe, expect, it } from "vitest";

import {
  expectExecutionWithinBandV1,
  OPTION_B_SKIP_REASON,
  prepareRouteFreedomJourneyV1,
  printRouteFreedomCampaignTableV1,
  realBlueprintSpeaksOptionBV1,
  type RouteFreedomJourneyV1,
} from "./support/route-freedom-journey.js";
import { buildInvalidForcedValidationDisputeFixture } from "./support/submit-init-emulator-fixtures.js";
import {
  type CompleteSignedTransactionMeasurement,
  expectProofFitV1,
} from "./support/submit-init-emulator-shared.js";

const RELIABLE_DIRECT_PIN =
  MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxReliableDirectCompleteItemBytes;

/** Payload staging exactly the tier-1 ceiling preimage of 14,336 bytes. */
const MAX_TIER1_PAYLOAD_BYTES = 13_851;

/**
 * The resolver proof-fit sweep's shape (its generator stages payload 7,976,
 * item 8,277) and its committed PRE-change rows — the #622 baseline. Copied
 * literals, provenance: resolver-proof-fit-sweep-v1.generated.json at
 * 2476d358 (pre-#620 blueprint), stages authenticate/source/observe/proof/
 * settlement plus the prepare row.
 */
const SWEEP_PAYLOAD_BYTES = 7_976;
const SWEEP_ITEM_BYTES = 8_277;
const CLAIM_REGISTRY_REMOVAL_HEADROOM_BYTES = 56;
const PRE_CHANGE_SWEEP_ROWS = {
  prepare: { bytes: 10_409, mem: 613_326n, cpu: 544_789_001n },
  authenticate: { bytes: 11_197, mem: 179_092n, cpu: 341_706_855n },
  source: { bytes: 7_895, mem: 861_983n, cpu: 305_737_122n },
  observe: { bytes: 10_463, mem: 890_227n, cpu: 574_227_003n },
  proof: { bytes: 5_701, mem: 668_047n, cpu: 211_499_642n },
  settle: { bytes: 5_064, mem: 449_767n, cpu: 172_281_854n },
} as const;

/** Files 1-2's item-size-independent six-stage rows, re-pinned here. */
const SIX_STAGE_CONSTANT_ROW_BYTES_V1 = {
  prepareSelected: 1_808,
  authenticate: 2_600,
  source: 1_855,
  proof: 1_880,
  settle: 623,
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

const semanticMeasurementAt = (
  measurements: readonly CompleteSignedTransactionMeasurement[],
  index: number,
  kind: string,
): CompleteSignedTransactionMeasurement => {
  const measurement = measurements[index];
  if (measurement === undefined) {
    throw new Error(`semantic leg captured no ${kind} transaction`);
  }
  return measurement;
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

/**
 * Every byte the dispute chain itself put on L1 — the four reference-script
 * publications, init through prepare-selected, the semantic leg (publication
 * included on this route), and the award. The "setup" scaffolding stage is
 * excluded for the same reason the fit policy excludes it.
 */
const totalJourneyBytes = (
  journey: RouteFreedomJourneyV1,
  semanticMeasurements: readonly CompleteSignedTransactionMeasurement[],
  awardMeasurement: CompleteSignedTransactionMeasurement,
): number => {
  let total = awardMeasurement.completeSignedBytes;
  for (const stage of journey.lifecycleMeasurements) {
    if (stage.label === "setup") {
      continue;
    }
    for (const measurement of stage.measurements) {
      total += measurement.completeSignedBytes;
    }
  }
  for (const measurement of semanticMeasurements) {
    total += measurement.completeSignedBytes;
  }
  return total;
};

const optionB = realBlueprintSpeaksOptionBV1();
if (!optionB) {
  console.warn(OPTION_B_SKIP_REASON);
}

describe.skipIf(!optionB)(
  "post-Option-B reference-route frontier and sweep-shape baseline (#622)",
  () => {
    it("measures the full reference journey at the tier-1 ceiling item 14,336 — the reference route's own frontier", async () => {
      const journey = await prepareRouteFreedomJourneyV1({
        inlineDatumPayloadBytes: MAX_TIER1_PAYLOAD_BYTES,
        minimumCompleteItemBytes:
          MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1 - 1,
      });
      expect(journey.completeItemBytes).toBe(
        MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1,
      );
      // The route is open at this size only because the tier-1 ceiling sits
      // under the owner-signed single-publication ceiling (60 bytes apart).
      expect(journey.completeItemBytes).toBeLessThanOrEqual(
        MIDGARD_CONSENSUS_LIMITS_V1.maxSinglePublicationCompleteItemBytes,
      );

      // No routing input: the build-time heuristic itself carries anything
      // past the owner-signed direct frontier — re-pinned 12,810 -> 13,522 at
      // the #617 wave sign-off (#622 ruling (b)) — by reference.
      const semantic = await journey.submitSemanticResolution();
      printRouteFreedomCampaignTableV1(
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

      // The reference route's two size-bound transactions at the frontier:
      // the up-front §8 publication (the item rides its inline datum) and
      // the by-reference observe door (constant size — the preimage stays
      // behind the reference input).
      const publication = semanticMeasurementAt(
        semantic.measurements,
        0,
        "publication",
      );
      expect(publication.completeSignedBytes).toBe(15_107);
      expect(stageBytesByKind(stageTransactions, "observe")).toBe(1_903);
      const observeStage = stageTransactions.find(
        (stage) => stage.kind === "observe",
      );
      expect(observeStage?.projectedSignedBytes).toBeUndefined();

      // The by-reference door's execution bill at maximum tier-1 content —
      // the door still hashes the full 14,336-byte preimage it dereferences.
      // Band-pinned: the by-reference lookup walks run-dependent out-refs,
      // so its bill wobbles <1% run to run (see the helper's doc).
      const observeMeasurement = semanticMeasurementAt(
        semantic.measurements,
        3,
        "observe",
      );
      expectExecutionWithinBandV1(
        "#622 reference-frontier observe-by-reference",
        observeMeasurement,
        { memoryUnits: 931_806n, stepUnits: 325_654_977n },
      );

      // Every non-observe stage holds the item-independent literals even at
      // the largest stageable item.
      expectItemIndependentRows(journey, stageTransactions);

      const award = await journey.submitAward(result.nextThreadOutRef);
      // End-to-end: every transaction this dispute cost L1, from the four
      // reference-script publications through init, open, the bisection, the
      // semantic leg (publication included), and the by-reference award.
      expect(award.measurement.completeSignedBytes).toBe(889);
      expect(
        totalJourneyBytes(journey, semantic.measurements, award.measurement),
      ).toBe(125_474);
      expectWholeJourneyProofFit(
        "#622 reference-frontier item 14,336",
        journey,
        semantic.measurements,
        award.measurement,
      );
    }, 900_000);

    it("bills strictly below the pre-change sweep rows at the sweep's own shape and records the removed claim-registry witness headroom", async () => {
      const journey = await prepareRouteFreedomJourneyV1({
        inlineDatumPayloadBytes: SWEEP_PAYLOAD_BYTES,
        minimumCompleteItemBytes: 0,
      });
      expect(journey.completeItemBytes).toBe(SWEEP_ITEM_BYTES);
      expect(journey.completeItemBytes).toBeLessThanOrEqual(
        RELIABLE_DIRECT_PIN,
      );

      // No routing input: the heuristic rides a sweep-shaped item inline.
      const semantic = await journey.submitSemanticResolution();
      printRouteFreedomCampaignTableV1(
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

      // Every dispute-stage row is 56 bytes smaller after removing the
      // claim-registry witness; source, proof, and settlement retain the
      // additional savings from their validators arriving by reference.
      expect(stageBytesByKind(stageTransactions, "observe")).toBe(
        PRE_CHANGE_SWEEP_ROWS.observe.bytes -
          CLAIM_REGISTRY_REMOVAL_HEADROOM_BYTES,
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
      expect(SIX_STAGE_CONSTANT_ROW_BYTES_V1.source).toBeLessThan(
        PRE_CHANGE_SWEEP_ROWS.source.bytes,
      );
      expect(SIX_STAGE_CONSTANT_ROW_BYTES_V1.proof).toBeLessThan(
        PRE_CHANGE_SWEEP_ROWS.proof.bytes,
      );
      expect(SIX_STAGE_CONSTANT_ROW_BYTES_V1.settle).toBeLessThan(
        PRE_CHANGE_SWEEP_ROWS.settle.bytes,
      );
      // ... and the two collapsed stages against their pre-change rows.
      expect(stageBytesByKind(stageTransactions, "authenticate")).toBe(
        SIX_STAGE_CONSTANT_ROW_BYTES_V1.authenticate,
      );
      expect(SIX_STAGE_CONSTANT_ROW_BYTES_V1.authenticate).toBeLessThan(
        PRE_CHANGE_SWEEP_ROWS.authenticate.bytes,
      );
      const prepareSelected = lastLifecycleMeasurement(
        journey,
        "prepare-selected",
      );
      expect(prepareSelected.completeSignedBytes).toBe(
        SIX_STAGE_CONSTANT_ROW_BYTES_V1.prepareSelected,
      );
      expect(SIX_STAGE_CONSTANT_ROW_BYTES_V1.prepareSelected).toBeLessThan(
        PRE_CHANGE_SWEEP_ROWS.prepare.bytes,
      );

      // The like-for-like execution comparison the campaign owes (#622):
      // same item, same route, same stages — the post-change bill. Pinned
      // exactly, and ordered against the pre-change rows.
      const observeMeasurement = semanticMeasurementAt(
        semantic.measurements,
        2,
        "observe",
      );
      expect(observeMeasurement.executionMemory).toBe(878_878n);
      expect(observeMeasurement.executionSteps).toBe(299_816_995n);
      expect(
        observeMeasurement.executionMemory < PRE_CHANGE_SWEEP_ROWS.observe.mem,
      ).toBe(true);
      expect(
        observeMeasurement.executionSteps < PRE_CHANGE_SWEEP_ROWS.observe.cpu,
      ).toBe(true);
      const authenticateMeasurement = semanticMeasurementAt(
        semantic.measurements,
        0,
        "authenticate",
      );
      expect(authenticateMeasurement.executionMemory).toBe(163_390n);
      expect(authenticateMeasurement.executionSteps).toBe(106_674_927n);
      expect(
        authenticateMeasurement.executionMemory <
          PRE_CHANGE_SWEEP_ROWS.authenticate.mem,
      ).toBe(true);
      expect(
        authenticateMeasurement.executionSteps <
          PRE_CHANGE_SWEEP_ROWS.authenticate.cpu,
      ).toBe(true);
      expect(prepareSelected.executionMemory).toBe(601_666n);
      expect(prepareSelected.executionSteps).toBe(311_092_640n);
      expect(
        prepareSelected.executionMemory < PRE_CHANGE_SWEEP_ROWS.prepare.mem,
      ).toBe(true);
      expect(
        prepareSelected.executionSteps < PRE_CHANGE_SWEEP_ROWS.prepare.cpu,
      ).toBe(true);
      // Untouched validators bill identically — the no-regression rows.
      const sourceMeasurement = semanticMeasurementAt(
        semantic.measurements,
        1,
        "source",
      );
      expect(sourceMeasurement.executionMemory).toBe(
        PRE_CHANGE_SWEEP_ROWS.source.mem,
      );
      expect(sourceMeasurement.executionSteps).toBe(
        PRE_CHANGE_SWEEP_ROWS.source.cpu,
      );
      const proofMeasurement = semanticMeasurementAt(
        semantic.measurements,
        3,
        "proof",
      );
      expect(proofMeasurement.executionMemory).toBe(
        PRE_CHANGE_SWEEP_ROWS.proof.mem,
      );
      expect(proofMeasurement.executionSteps).toBe(
        PRE_CHANGE_SWEEP_ROWS.proof.cpu,
      );
      const settleMeasurement = semanticMeasurementAt(
        semantic.measurements,
        4,
        "settle",
      );
      expect(settleMeasurement.executionMemory).toBe(
        PRE_CHANGE_SWEEP_ROWS.settle.mem,
      );
      expect(settleMeasurement.executionSteps).toBe(
        PRE_CHANGE_SWEEP_ROWS.settle.cpu,
      );

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
        MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1,
      );
      // ... and one payload byte further the §8.4 partition says RawUtxo,
      // which the evidence bundle's inline-only carriage resolution cannot
      // thread — the fixture family's hard edge, measured as such. The
      // tier-2/3 journey machinery is #617's owed revival; until it lands,
      // "reference frontier + 1" is a build refusal, not a measurement.
      await expect(
        buildInvalidForcedValidationDisputeFixture({
          operatorVkey: "11".repeat(28),
          now: 1_700_000_000_000,
          inlineDatumPayloadBytes: MAX_TIER1_PAYLOAD_BYTES + 1,
          minimumCompleteItemBytes: 0,
        }),
      ).rejects.toThrow();
    }, 300_000);
  },
);
