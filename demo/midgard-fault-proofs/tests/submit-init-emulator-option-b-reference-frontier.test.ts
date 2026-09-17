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
 * sibling stages must stay within the historical execution budgets, the
 * reworked stages must remain strictly cheaper, and the observe door — same
 * wire bytes, one hash fewer in the bill since #620 deleted the frozen-hash
 * equality — must bill strictly below its pre-change row. The sweep fixture
 * itself is NOT regenerated here: that regeneration rides #617's batched
 * ABI wave.
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
  OPTION_B_SKIP_REASON,
  prepareRouteFreedomJourney,
  printRouteFreedomCampaignTable,
  realBlueprintSpeaksOptionBV1,
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

/**
 * The resolver proof-fit sweep's shape (its generator stages payload 7,976,
 * item 8,277) and its PRE-Option-B billing baseline.
 *
 * The baseline is loaded, not transcribed. It is NOT the committed sweep
 * fixture: that file has since been regenerated against the post-change
 * blueprint, so comparing against it would compare this build to itself.
 * `pre-option-b-resolver-proof-fit-sweep-baseline-v1.json` carries the rows
 * as they stood at 2476d358 (the pre-#620 blueprint) together with their
 * provenance, and is a reviewed historical baseline in the sense of the
 * test-quality requirements: it establishes the "before" side of an ordering
 * claim and nothing else.
 */
const SWEEP_PAYLOAD_BYTES = 7_976;
const SWEEP_ITEM_BYTES = 8_277;

type BaselineRow = {
  readonly title: string;
  readonly completeSignedBytes: number;
  readonly memoryUnits: string;
  readonly cpuUnits: string;
};

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
  readonly rows: Record<string, BaselineRow>;
};

const baselineRow = (name: string): BaselineRow => {
  const row = preChangeBaseline.rows[name];
  if (row === undefined) {
    throw new Error(`pre-Option-B baseline carries no ${name} row`);
  }
  return row;
};

const PRE_CHANGE_SWEEP_ROWS = {
  prepare: baselineRow("prepare"),
  authenticate: baselineRow("authenticate"),
  source: baselineRow("source"),
  observe: baselineRow("observe"),
  proof: baselineRow("proof"),
  settle: baselineRow("settle"),
} as const;

const lastLifecycleMeasurement = (
  journey: RouteFreedomJourney,
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

// Fail closed (test-quality rule 14): Option B is the shipped complete-item
// wire, so a blueprint that still declares the retired carriage parameter is
// a broken precondition, not a reason to report a silent pass.
if (!realBlueprintSpeaksOptionBV1()) {
  throw new Error(OPTION_B_SKIP_REASON);
}

describe("post-Option-B reference-route frontier and sweep-shape baseline (#622)", () => {
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

  it("bills strictly below the pre-change sweep rows at the sweep's own shape and records the removed claim-registry witness headroom", async () => {
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

    const prepareSelected = lastLifecycleMeasurement(
      journey,
      "prepare-selected",
    );

    // The like-for-like execution comparison the campaign owes (#622):
    // same item, same route, same stages. Re-measured against the frozen
    // combined testnet blueprint and ordered against the historical rows.
    const observeMeasurement = semanticMeasurementAt(
      semantic.measurements,
      2,
      "observe",
    );
    expect(observeMeasurement.executionMemory).toBeLessThan(
      BigInt(PRE_CHANGE_SWEEP_ROWS.observe.memoryUnits),
    );
    expect(observeMeasurement.executionSteps).toBeLessThan(
      BigInt(PRE_CHANGE_SWEEP_ROWS.observe.cpuUnits),
    );
    const authenticateMeasurement = semanticMeasurementAt(
      semantic.measurements,
      0,
      "authenticate",
    );
    expect(authenticateMeasurement.executionMemory).toBeLessThan(
      BigInt(PRE_CHANGE_SWEEP_ROWS.authenticate.memoryUnits),
    );
    expect(authenticateMeasurement.executionSteps).toBeLessThan(
      BigInt(PRE_CHANGE_SWEEP_ROWS.authenticate.cpuUnits),
    );
    expect(prepareSelected.executionMemory).toBeLessThan(
      BigInt(PRE_CHANGE_SWEEP_ROWS.prepare.memoryUnits),
    );
    expect(prepareSelected.executionSteps).toBeLessThan(
      BigInt(PRE_CHANGE_SWEEP_ROWS.prepare.cpuUnits),
    );

    // The source redesign also changes sibling validator costs. Preserve the
    // historical upper bounds: a cheaper observe door must not move its cost
    // into source authentication, proof or settlement. Lower costs are valid;
    // their current measurements are emitted by the journey below.
    for (const [index, name] of [
      [1, "source"],
      [3, "proof"],
      [4, "settle"],
    ] as const) {
      const measurement = semanticMeasurementAt(
        semantic.measurements,
        index,
        name,
      );
      expect(measurement.executionMemory, name).toBeLessThanOrEqual(
        BigInt(PRE_CHANGE_SWEEP_ROWS[name].memoryUnits),
      );
      expect(measurement.executionSteps, name).toBeLessThanOrEqual(
        BigInt(PRE_CHANGE_SWEEP_ROWS[name].cpuUnits),
      );
    }

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
