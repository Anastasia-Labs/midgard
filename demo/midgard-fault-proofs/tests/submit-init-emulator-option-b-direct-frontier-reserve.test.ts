/**
 * #622 measurement campaign, file 1 of 3: the post-Option-B direct-route
 * RESERVE frontier, driven — not predicted — through the genuine
 * build-sign-submit lifecycle, with the adjacent-item probe.
 *
 * Since #620 narrowed the committed evidence to the transition alone and
 * #621 moved delivery routing to build time, the one stage that still grows
 * with the §5.1 complete-item preimage on the direct route is the observe
 * stage — the §8.8 door carries the preimage in its redeemer. The claims:
 *
 *   1. At the owner-signed direct threshold
 *      (`MIDGARD_ENVELOPE_MEASUREMENTS.maxReliableDirectCompleteItemBytes`,
 *      13,522) the observe transaction signs at or under the reliability
 *      budget — `minSupportedL1MaxTxBytes` less
 *      `proofItemEnvelopeReliabilityReserveBytes`, 16,384 - 512 = 15,872 —
 *      so the reserve the routing policy claims really exists.
 *   2. The pre-sign projection the routing heuristic admits the transaction
 *      on equals the bytes signing produced.
 *   3. The threshold is a policy line, not a cliff: item 13,523 still signs
 *      and still completes on the direct route.
 *   4. Every non-observe stage is item-size independent — asserted by
 *      comparing this file's two journeys against EACH OTHER, not against
 *      transcribed byte counts. The pre-change binder was `authenticate`,
 *      which double-carried the item; post-change it must not move at all
 *      when the item does.
 *
 * No absolute byte count is asserted here. Absolute stage sizes and
 * execution units belong in the generated fit ledger, whose `--check` mode
 * is the drift gate; a suite that transcribes them only reports that the
 * compiler, the fee balancer or lucid produced different bytes.
 *
 * Lives in its own file (two journeys), split alongside the #621
 * route-freedom files. The split was made while `@lucid-evolution/uplc`
 * (through 0.2.22) leaked wasm linear memory on every script evaluation and
 * vitest isolates per FILE; that leak is fixed upstream, and the split is
 * kept so each file runs in its own fresh process.
 */

import {
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_ENVELOPE_MEASUREMENTS,
} from "@al-ft/midgard-core/consensus-profile";
import { PROTOCOL_PARAMETERS_DEFAULT } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

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

/**
 * The reliability budget this file measures against: the smallest supported
 * L1 `max_tx_size` less the publication reliability reserve, both read from
 * the consensus profile rather than transcribed (16,384 - 512 = 15,872).
 */
const RELIABILITY_BUDGET_BYTES =
  MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxBytes -
  MIDGARD_ENVELOPE_MEASUREMENTS.proofItemEnvelopeReliabilityReserveBytes;

/**
 * The owner-signed direct-route reserve frontier, read from the consensus
 * profile it governs — this suite is the measured evidence BEHIND that
 * number, so the profile is the claim and the journeys are the check.
 * Payload 13,062 stages exactly this preimage (fixture map:
 * item = datumSize(payload) + 49).
 */
const RESERVE_FRONTIER_ITEM_BYTES =
  MIDGARD_ENVELOPE_MEASUREMENTS.maxReliableDirectCompleteItemBytes;
const RESERVE_FRONTIER_PAYLOAD_BYTES = 13_062;

type StageRecord = {
  readonly kind: string;
  readonly completeSignedBytes: number;
  readonly projectedSignedBytes?: number;
};

const stageByKind = (
  stageTransactions: readonly StageRecord[],
  kind: string,
): StageRecord => {
  const stage = stageTransactions.find((entry) => entry.kind === kind);
  if (stage === undefined) {
    throw new Error(`journey lost its ${kind} stage record`);
  }
  return stage;
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

assertRealBlueprintSpeaksOptionBV1();

type FrontierRun = {
  readonly journey: RouteFreedomJourney;
  readonly stageTransactions: readonly StageRecord[];
  readonly result: Awaited<
    ReturnType<RouteFreedomJourney["submitSemanticResolution"]>
  >["result"];
  readonly semanticMeasurements: readonly CompleteSignedTransactionMeasurement[];
};

/**
 * One direct-route journey at `payloadBytes`, driven all the way to the
 * award, with every transaction proof-fit checked. Both runs below are
 * prepared in a single `beforeAll` so the item-size-independence comparison
 * has both of them without one test depending on another having run.
 */
const runDirectFrontierJourney = async (
  headline: string,
  payloadBytes: number,
  expectedItemBytes: number,
): Promise<FrontierRun> => {
  const journey = await prepareRouteFreedomJourney({
    inlineDatumPayloadBytes: payloadBytes,
    minimumCompleteItemBytes: expectedItemBytes - 1,
  });
  if (journey.completeItemBytes !== expectedItemBytes) {
    throw new Error(
      `${headline} staged a ${journey.completeItemBytes.toString()}-byte complete item, not ${expectedItemBytes.toString()}`,
    );
  }
  const semantic = await journey.submitSemanticResolution({
    proofItemDelivery: "inline",
  });
  printRouteFreedomCampaignTable(headline, journey, semantic);
  const award = await journey.submitAward(semantic.result.nextThreadOutRef);
  expectWholeJourneyProofFit(
    headline,
    journey,
    semantic.measurements,
    award.measurement,
  );
  return {
    journey,
    stageTransactions: semantic.result.stageTransactions ?? [],
    result: semantic.result,
    semanticMeasurements: semantic.measurements,
  };
};

describe("post-Option-B direct-route reserve frontier (#622)", () => {
  let atFrontier: FrontierRun;
  let pastFrontier: FrontierRun;

  beforeAll(async () => {
    atFrontier = await runDirectFrontierJourney(
      "#622 reserve-frontier item 13,522",
      RESERVE_FRONTIER_PAYLOAD_BYTES,
      RESERVE_FRONTIER_ITEM_BYTES,
    );
    pastFrontier = await runDirectFrontierJourney(
      "#622 reserve-frontier+1 item 13,523",
      RESERVE_FRONTIER_PAYLOAD_BYTES + 1,
      RESERVE_FRONTIER_ITEM_BYTES + 1,
    );
  }, 1_800_000);

  it("signs the observe door inside the reliability budget at the owner-signed direct threshold", () => {
    // The claim the owner-signed `maxReliableDirectCompleteItemBytes` makes:
    // at that item size the one preimage-carrying door signs at or under the
    // reliability budget, so the 512-byte reserve is really there.
    const observe = stageByKind(atFrontier.stageTransactions, "observe");
    expect(observe.completeSignedBytes).toBeLessThanOrEqual(
      RELIABILITY_BUDGET_BYTES,
    );
    // ... and the pre-sign projection the routing heuristic admitted the
    // transaction on measured the exact bytes signing produced. A projection
    // that were merely conservative would admit items it cannot sign.
    expect(observe.projectedSignedBytes).toBe(observe.completeSignedBytes);
  });

  it("keeps the whole frontier journey on the direct route with no publication", () => {
    expect(atFrontier.result.proofItemCarriage).toBe("direct");
    expect(atFrontier.result.proofItemPublication).toBeUndefined();
    expect(atFrontier.result.proofItemInlineEnvelopeRefusal).toBeUndefined();
    expect(atFrontier.stageTransactions).toHaveLength(5);
    expect(atFrontier.semanticMeasurements).toHaveLength(5);
  });

  it("still rides the direct route one item byte past the threshold", () => {
    // The adjacent probe: the owner-signed threshold is a policy line with
    // headroom under the envelope, not a cliff — the next item still signs
    // and still completes directly.
    expect(pastFrontier.result.proofItemCarriage).toBe("direct");
    expect(pastFrontier.result.proofItemInlineEnvelopeRefusal).toBeUndefined();
    const observe = stageByKind(pastFrontier.stageTransactions, "observe");
    expect(observe.completeSignedBytes).toBeLessThanOrEqual(MAX_L1_TX_BYTES);
  });

  it("bills every non-observe stage independently of the item size", () => {
    // The structural claim #620/#621 bought: since the transition alone is
    // committed and routing moved to build time, the §8.8 observe door is the
    // ONLY stage that carries the §5.1 complete-item preimage. Compared
    // across the two journeys rather than against transcribed sizes: a stage
    // that started double-carrying the item (the pre-change `authenticate`
    // shape) would move here.
    const sizes = (run: FrontierRun) =>
      Object.fromEntries(
        run.stageTransactions.map((stage) => [
          stage.kind,
          stage.completeSignedBytes,
        ]),
      );
    const atSizes = sizes(atFrontier);
    const pastSizes = sizes(pastFrontier);
    expect(Object.keys(pastSizes).sort()).toEqual(Object.keys(atSizes).sort());
    for (const kind of Object.keys(atSizes)) {
      if (kind === "observe") {
        continue;
      }
      expect(pastSizes[kind], kind).toBe(atSizes[kind]);
    }
    // ... and the observe door does grow with the item it carries, so the
    // equality above is item-size independence and not a dead comparison of
    // two identical journeys.
    expect(pastSizes["observe"]).toBeGreaterThan(atSizes["observe"]!);
  });
});
