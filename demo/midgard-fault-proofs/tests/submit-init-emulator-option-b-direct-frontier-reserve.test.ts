/**
 * #622 measurement campaign, file 1 of 3: the post-Option-B direct-route
 * RESERVE frontier, measured — not predicted — through the genuine
 * build-sign-submit lifecycle (the resolver proof-fit sweep precedent), with
 * the adjacent-item probe the consensus-profile ledger style demands.
 *
 * Since #620 narrowed the committed evidence to the transition alone and
 * #621 moved delivery routing to build time, the one stage that still grows
 * with the §5.1 complete-item preimage on the direct route is the observe
 * stage — the §8.8 door carries the preimage in its redeemer. This file pins
 * the measured frontier against the 15,872-byte reliability budget
 * (16,384 − 512, `proofItemEnvelopeReliabilityReserveBytes`):
 *
 *   item 13,522 -> observe 15,816 (= budget - 56 after claim-registry removal)
 *   item 13,523 -> observe 15,817 (one byte larger, second journey)
 *
 * Measured observe shape on this fixture family: item bytes + 64-byte
 * Plutus-data chunk headers + framing, where the framing is quantized by
 * the transaction-balancing fixed point (file 2's header carries the
 * measured ladder near the envelope). Both of this file's pins are direct
 * measurements — the adjacent pair 13,522 -> 15,816 / 13,523 -> 15,817
 * proves the owner-signed policy threshold now retains 56 extra bytes of
 * headroom. The pre-change binder was
 * authenticate (which double-carried the item; owner-signed reserve
 * 12,810); post-change authenticate is item-size-independent, measured
 * byte-identical across this file's two item sizes and files 2-3's probes.
 *
 * Every pinned number is a measurement of this suite's own journey — a
 * change in any stage's shape moves a pin and must be re-pinned
 * deliberately, never absorbed. The consensus-profile pins this suite
 * measured (12,810 / 13,294) were NOT rebound by this file: #619's question
 * (b) went to the owner, who approved the lane-level rebind to 13,522 /
 * 14,004 on 2026-08-22, executed at the #617 wave sign-off. Removing the
 * claim-registry witness subsequently moved that exact frontier to 14,058;
 * this suite remains the measured table behind the reliability pin.
 *
 * Lives in its own file (two journeys), split alongside the #621 route-freedom
 * files. The split was made while `@lucid-evolution/uplc` (through 0.2.22)
 * leaked wasm linear memory on every script evaluation and vitest isolates per
 * FILE; that leak is fixed upstream, and the split is kept so each file runs
 * in its own fresh process.
 */

import { MIDGARD_ENVELOPE_MEASUREMENTS } from "@al-ft/midgard-core";
import { PROTOCOL_PARAMETERS_DEFAULT } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  OPTION_B_SKIP_REASON,
  prepareRouteFreedomJourney,
  printRouteFreedomCampaignTable,
  realBlueprintSpeaksOptionBV1,
  type RouteFreedomJourney,
} from "./support/route-freedom-journey.js";
import {
  type CompleteSignedTransactionMeasurement,
  expectProofFit,
} from "./support/submit-init-emulator-shared.js";

const MAX_L1_TX_BYTES = PROTOCOL_PARAMETERS_DEFAULT.maxTxSize;
const RELIABILITY_BUDGET_BYTES =
  MAX_L1_TX_BYTES -
  MIDGARD_ENVELOPE_MEASUREMENTS.proofItemEnvelopeReliabilityReserveBytes;
const CLAIM_REGISTRY_REMOVAL_HEADROOM_BYTES = 56;

/**
 * The measured post-Option-B direct-route reserve frontier: the largest §5.1
 * complete-item preimage below which every observe transaction signs at or
 * under the 15,872-byte reliability budget (contiguous frontier — the next
 * item is measured over it in this file's second journey). Payload 13,062
 * stages exactly this preimage (fixture map: item = datumSize(payload)+49).
 */
const RESERVE_FRONTIER_ITEM_BYTES = 13_522;
const RESERVE_FRONTIER_PAYLOAD_BYTES = 13_062;

/**
 * The item-size-INDEPENDENT rows of the measured six-stage table, identical
 * at items 13,522 and 13,523 here and re-pinned identically in files 2-3 at
 * 14,058 / 14,059 / 14,336 / 8,277 — the measured form of #619's "prepare
 * and authenticate become item-size-independent". Observe, the sole
 * item-bound stage, is pinned per journey.
 */
const SIX_STAGE_CONSTANT_ROW_BYTES = {
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

const expectItemIndependentRows = (
  journey: RouteFreedomJourney,
  stageTransactions: readonly {
    readonly kind: string;
    readonly completeSignedBytes: number;
  }[],
): void => {
  expect(
    lastLifecycleMeasurement(journey, "prepare-selected").completeSignedBytes,
  ).toBe(SIX_STAGE_CONSTANT_ROW_BYTES.prepareSelected);
  expect(stageBytesByKind(stageTransactions, "authenticate")).toBe(
    SIX_STAGE_CONSTANT_ROW_BYTES.authenticate,
  );
  expect(stageBytesByKind(stageTransactions, "source")).toBe(
    SIX_STAGE_CONSTANT_ROW_BYTES.source,
  );
  expect(stageBytesByKind(stageTransactions, "proof")).toBe(
    SIX_STAGE_CONSTANT_ROW_BYTES.proof,
  );
  expect(stageBytesByKind(stageTransactions, "settle")).toBe(
    SIX_STAGE_CONSTANT_ROW_BYTES.settle,
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
 * Every DISPUTE transaction this journey put on the emulator — the
 * reference-script publications, init through prepare-selected, the semantic
 * leg, the award — fits the real L1 byte envelope and the 20%-reserve
 * execution ceilings (the sweep's §3.3 policy). The "setup" stage is
 * excluded by name: it is emulator scaffolding (account funding and protocol
 * bootstrap under the relaxed test envelope), not part of the dispute's L1
 * transaction chain.
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

const optionB = realBlueprintSpeaksOptionBV1();
if (!optionB) {
  console.warn(OPTION_B_SKIP_REASON);
}

describe.skipIf(!optionB)(
  "post-Option-B direct-route reserve frontier (#622)",
  () => {
    it("keeps 56 bytes of added headroom at the owner-signed 13,522-byte direct threshold", async () => {
      const journey = await prepareRouteFreedomJourney({
        inlineDatumPayloadBytes: RESERVE_FRONTIER_PAYLOAD_BYTES,
        minimumCompleteItemBytes: RESERVE_FRONTIER_ITEM_BYTES - 1,
      });
      expect(journey.completeItemBytes).toBe(RESERVE_FRONTIER_ITEM_BYTES);

      const semantic = await journey.submitSemanticResolution({
        proofItemDelivery: "inline",
      });
      printRouteFreedomCampaignTable(
        "#622 reserve-frontier item 13,522",
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

      // Removing the claim-registry reference from the deployment reduced the
      // signed observe transaction by 56 bytes without rebinding the
      // owner-signed direct-route threshold. The pre-sign projection must
      // still measure the exact bytes signing produced.
      const observeBytes = stageBytesByKind(stageTransactions, "observe");
      expect(observeBytes).toBe(
        RELIABILITY_BUDGET_BYTES - CLAIM_REGISTRY_REMOVAL_HEADROOM_BYTES,
      );
      const observeStage = stageTransactions.find(
        (stage) => stage.kind === "observe",
      );
      expect(observeStage?.projectedSignedBytes).toBe(observeBytes);

      // The measured six-stage table at the reserve frontier: observe is the
      // binder, every other stage is item-size-independent.
      expectItemIndependentRows(journey, stageTransactions);

      // Re-measured execution units at the frontier shape (#622): the §8.8
      // door hashes the delivered preimage once — the deleted frozen-hash
      // equality is out of the bill — and prepare/authenticate no longer
      // touch item bytes at all. Pinned exactly, sweep-fixture style; the
      // 20%-reserve policy is asserted for every journey transaction below.
      const observeMeasurement = semanticMeasurementAt(
        semantic.measurements,
        2,
        "observe",
      );
      expect(observeMeasurement.executionMemory).toBe(878_878n);
      expect(observeMeasurement.executionSteps).toBe(305_301_155n);
      const authenticateMeasurement = semanticMeasurementAt(
        semantic.measurements,
        0,
        "authenticate",
      );
      expect(authenticateMeasurement.executionMemory).toBe(163_390n);
      expect(authenticateMeasurement.executionSteps).toBe(106_674_927n);
      const prepareSelected = lastLifecycleMeasurement(
        journey,
        "prepare-selected",
      );
      expect(prepareSelected.executionMemory).toBe(601_666n);
      expect(prepareSelected.executionSteps).toBe(311_092_640n);

      const award = await journey.submitAward(result.nextThreadOutRef);
      expectWholeJourneyProofFit(
        "#622 reserve-frontier item 13,522",
        journey,
        semantic.measurements,
        award.measurement,
      );
    }, 900_000);

    it("measures the adjacent 13,523-byte direct probe one byte larger and still completes", async () => {
      const journey = await prepareRouteFreedomJourney({
        inlineDatumPayloadBytes: RESERVE_FRONTIER_PAYLOAD_BYTES + 1,
        minimumCompleteItemBytes: RESERVE_FRONTIER_ITEM_BYTES,
      });
      expect(journey.completeItemBytes).toBe(RESERVE_FRONTIER_ITEM_BYTES + 1);

      const semantic = await journey.submitSemanticResolution({
        proofItemDelivery: "inline",
      });
      printRouteFreedomCampaignTable(
        "#622 reserve-frontier+1 item 13,523",
        journey,
        semantic,
      );
      const result = semantic.result;
      expect(result.proofItemCarriage).toBe("direct");
      expect(result.proofItemInlineEnvelopeRefusal).toBeUndefined();
      const stageTransactions = result.stageTransactions ?? [];

      // The adjacent-item probe: one more preimage byte is one more signed
      // observe byte (no 64-byte chunk boundary sits between 13,522 and
      // 13,523). The journey is forced down the direct route to prove the
      // owner-signed threshold remains a cost steer rather than a liveness
      // cliff.
      expect(stageBytesByKind(stageTransactions, "observe")).toBe(
        RELIABILITY_BUDGET_BYTES - CLAIM_REGISTRY_REMOVAL_HEADROOM_BYTES + 1,
      );

      // Item-size independence, measured: every non-observe stage signs at
      // byte-identical size to the frontier journey.
      expectItemIndependentRows(journey, stageTransactions);

      const award = await journey.submitAward(result.nextThreadOutRef);
      expectWholeJourneyProofFit(
        "#622 reserve-frontier+1 item 13,523",
        journey,
        semantic.measurements,
        award.measurement,
      );
    }, 900_000);
  },
);
