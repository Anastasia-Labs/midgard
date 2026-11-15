/**
 * Which family the watcher proves when a transaction names an output index
 * that a producer committed by the same block never created (decision 0010,
 * owner ruling 2026-11-15).
 *
 * Such an input is also absent from the transaction's event pre-state
 * ledger, so both the ledger-absence family and the same-block NoIdx family
 * find it at the same event. With the predecessor ledger present the
 * classifier must still select the NoIdx family, and that family's four-step
 * route must convict on the emulator. An input that is absent with no
 * same-block producer has no NoIdx finding and stays with the ledger-absence
 * family.
 */
import { outRefLabel } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  submitInputNoIdxStep01,
  submitInputNoIdxStep02,
  submitInputNoIdxStep03,
  submitInputNoIdxStep04,
  submitReferenceInputNoIdxStep01,
  submitReferenceInputNoIdxStep02,
  submitReferenceInputNoIdxStep03,
  submitReferenceInputNoIdxStep04,
} from "../src/index.js";
import { submitInit } from "../src/submit-init.js";
import {
  type CanonicalBlockClassification,
  classifyCanonicalBlockViolations,
} from "../src/workflow/classification.js";
import {
  INPUT_NO_IDX_COMPLETE_CANONICAL_REPLAY,
  NO_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY,
  NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY,
  REFERENCE_INPUT_NO_IDX_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import {
  admitInputNoIdxArtifact,
  prepareInputNoIdxArtifact,
} from "../src/workflow/input-no-idx.js";
import {
  admitReferenceInputNoIdxArtifact,
  prepareReferenceInputNoIdxArtifact,
} from "../src/workflow/reference-input-no-idx.js";
import { inputExistenceChronologyFixture } from "./helpers/input-existence-chronology-fixture.js";
import { chronologySeedDeposits } from "./helpers/input-existence-chronology-seed.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { submitSecondHeaderTx } from "./support/emulator/setup-tx.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  buildRemovalDeploymentInfo,
  expectSingleUtxoWithUnit,
  funderPaymentKeyHash,
  network,
  publishPlainReferenceScriptUtxo,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";
import { depositEventsRetainedBlock } from "./support/transition-trace-retained.deposit-events-retained-block.js";

type Harness = Awaited<ReturnType<typeof makeFaultProofEmulatorHarness>>;
type FaultDetected = Extract<
  CanonicalBlockClassification,
  { readonly decision: "fault_detected" }
>;

const REPLAYERS = {
  0: [
    NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY,
    INPUT_NO_IDX_COMPLETE_CANONICAL_REPLAY,
  ],
  1: [
    NO_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY,
    REFERENCE_INPUT_NO_IDX_COMPLETE_CANONICAL_REPLAY,
  ],
} as const;

const NO_IDX_CATEGORY = {
  0: "nonExistentInputNoIndex",
  1: "referenceInputNoIdx",
} as const;

const LEDGER_ABSENCE_CATEGORY = {
  0: "nonExistentInput",
  1: "noReferenceInput",
} as const;

/** A predecessor block of seed deposits and a block on it that runs the
 * scenario, both with headers that can be committed on the emulator. */
const chronologyBlocks = async (
  h: Harness | undefined,
  scenario: "normal_creator_missing_index" | "normal_spend",
  field: 0 | 1,
) => {
  const operatorVkey =
    h === undefined
      ? "00".repeat(28)
      : await funderPaymentKeyHash(h.funderLucid);
  const startTime =
    h === undefined
      ? 1_800_000_000_000n
      : BigInt(
          alignUnixTimeToEmulatorSlotBoundary(
            h.funderLucid,
            h.emulator.now() + 120_000,
          ) - 1,
        );
  const predecessor = await depositEventsRetainedBlock({
    operatorVkey,
    startTime: startTime - 30_000n,
    endTime: startTime,
    blockSlot: 0n,
    prevHeaderHash: SDK.GENESIS_HEADER_HASH,
    prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    priorLedger: [],
    events: chronologySeedDeposits(),
  });
  const block = await inputExistenceChronologyFixture(scenario, field, {
    operatorVkey,
    startTime,
    predecessorHeaderHash: predecessor.headerHash,
  });
  return { predecessor, block };
};

const classifyWithLedger = async (
  block: Awaited<ReturnType<typeof inputExistenceChronologyFixture>>,
  field: 0 | 1,
) => {
  const detections = [];
  for (const replayer of REPLAYERS[field])
    detections.push(
      ...(await replayer.replay(block.evidence, block.context)).detections,
    );
  return await classifyCanonicalBlockViolations({
    evidence: block.evidence,
    detections,
  });
};

const publishSteps = async (h: Harness, field: 0 | 1) => {
  const steps: UTxO[] = [];
  for (const [index, step] of h.contracts.fraudProofContracts[
    NO_IDX_CATEGORY[field]
  ].steps.entries())
    steps.push(
      (
        await publishPlainReferenceScriptUtxo({
          lucid: h.funderLucid,
          script: step.spendingScript,
          label: `${NO_IDX_CATEGORY[field]} step${(index + 1).toString()}`,
        })
      ).utxo,
    );
  return steps;
};

/** The selected NoIdx family's registered route, from the production
 * artifact the watcher would prepare, through to the permanent proof. */
const convictWithNoIdxRoute = async ({
  h,
  field,
  block,
  classification,
  fraudulentBlockOutRef,
}: {
  readonly h: Harness;
  readonly field: 0 | 1;
  readonly block: Awaited<ReturnType<typeof inputExistenceChronologyFixture>>;
  readonly classification: FaultDetected;
  readonly fraudulentBlockOutRef: string;
}) => {
  const category = NO_IDX_CATEGORY[field];
  const steps = await publishSteps(h, field);
  const deploymentInfo = buildRemovalDeploymentInfo(h.contracts, h.catalogue);
  const common = {
    lucid: h.proverLucid,
    blueprint: h.realBlueprint,
    deploymentInfo,
    network,
    signer: h.proverSigner,
    awaitConfirmation: true,
  } as const;
  const init = await submitInit({
    ...common,
    fraudCategory: category,
    fraudulentBlockOutRef,
    witnessReferenceScripts: h.witnessReferenceScripts,
  });
  const thread = async (address: string) =>
    outRefLabel(
      await expectSingleUtxoWithUnit(
        h.proverLucid,
        address,
        init.computationThreadUnit,
      ),
    );
  let finalStep: {
    readonly fraudProofAddress: string;
    readonly fraudProofUnit: string;
  };
  if (field === 0) {
    const admitted = admitInputNoIdxArtifact(
      await prepareInputNoIdxArtifact({
        evidence: block.evidence,
        classification,
      }),
    );
    const step01 = await submitInputNoIdxStep01({
      ...common,
      referenceScriptUtxo: steps[0]!,
      witnessReferenceScripts: h.witnessReferenceScripts,
      threadOutRef: await thread(init.firstStepAddress),
      stateQueueBlockOutRef: fraudulentBlockOutRef,
      txInclusion: admitted.badInclusion,
    });
    const step02 = await submitInputNoIdxStep02({
      ...common,
      referenceScriptUtxo: steps[1]!,
      threadOutRef: await thread(step01.secondStepAddress),
      inputsPreimage: admitted.inputs,
      nativeTxCompactCbor: admitted.badInclusion.nativeTxCompactCbor,
    });
    const step03 = await submitInputNoIdxStep03({
      ...common,
      referenceScriptUtxo: steps[2]!,
      witnessReferenceScripts: h.witnessReferenceScripts,
      threadOutRef: await thread(step02.thirdStepAddress),
      stateQueueBlockOutRef: fraudulentBlockOutRef,
      txInclusion: admitted.producingInclusion,
    });
    finalStep = await submitInputNoIdxStep04({
      ...common,
      referenceScriptUtxo: steps[3]!,
      witnessReferenceScripts: h.witnessReferenceScripts,
      threadOutRef: await thread(step03.fourthStepAddress),
      outputsPreimage: admitted.outputs,
      nativeTxCompactCbor: admitted.producingInclusion.nativeTxCompactCbor,
    });
  } else {
    const admitted = admitReferenceInputNoIdxArtifact(
      await prepareReferenceInputNoIdxArtifact({
        evidence: block.evidence,
        classification,
      }),
    );
    const step01 = await submitReferenceInputNoIdxStep01({
      ...common,
      referenceScriptUtxo: steps[0]!,
      witnessReferenceScripts: h.witnessReferenceScripts,
      threadOutRef: await thread(init.firstStepAddress),
      stateQueueBlockOutRef: fraudulentBlockOutRef,
      txInclusion: admitted.badInclusion,
    });
    const step02 = await submitReferenceInputNoIdxStep02({
      ...common,
      referenceScriptUtxo: steps[1]!,
      threadOutRef: await thread(step01.secondStepAddress),
      referenceInputsPreimage: admitted.referenceInputs,
      nativeTxCompactCbor: admitted.badInclusion.nativeTxCompactCbor,
    });
    const step03 = await submitReferenceInputNoIdxStep03({
      ...common,
      referenceScriptUtxo: steps[2]!,
      witnessReferenceScripts: h.witnessReferenceScripts,
      threadOutRef: await thread(step02.thirdStepAddress),
      stateQueueBlockOutRef: fraudulentBlockOutRef,
      txInclusion: admitted.producingInclusion,
    });
    finalStep = await submitReferenceInputNoIdxStep04({
      ...common,
      referenceScriptUtxo: steps[3]!,
      witnessReferenceScripts: h.witnessReferenceScripts,
      threadOutRef: await thread(step03.fourthStepAddress),
      outputsPreimage: admitted.outputs,
      nativeTxCompactCbor: admitted.producingInclusion.nativeTxCompactCbor,
    });
  }
  // Conviction: the permanent proof token is minted at the proof address.
  const proof = await expectSingleUtxoWithUnit(
    h.proverLucid,
    finalStep.fraudProofAddress,
    finalStep.fraudProofUnit,
  );
  expect(proof.assets[finalStep.fraudProofUnit]).toBe(1n);
};

describe("an out-of-range output index into a same-block producer", () => {
  for (const field of [0, 1] as const) {
    it(`selects ${NO_IDX_CATEGORY[field]} over ${LEDGER_ABSENCE_CATEGORY[field]} and convicts with it`, async () => {
      const h = await makeFaultProofEmulatorHarness({
        contractOptions: {
          realInputNoIdx: field === 0,
          realReferenceInputNoIdx: field === 1,
          alwaysFraudProofCatalogue: true,
        },
      });
      const { predecessor, block } = await chronologyBlocks(
        h,
        "normal_creator_missing_index",
        field,
      );
      const classification = await classifyWithLedger(block, field);
      expect(classification).toMatchObject({
        decision: "fault_detected",
        category: NO_IDX_CATEGORY[field],
      });
      if (classification.decision !== "fault_detected")
        throw new Error("classification found no fault");

      await submitSetupTx({
        lucid: h.funderLucid,
        contracts: h.contracts,
        nonceUtxo: h.nonceUtxo,
        catalogue: h.catalogue,
        header: predecessor.header,
      });
      const committed = await submitSecondHeaderTx({
        lucid: h.funderLucid,
        contracts: h.contracts,
        header: block.evidence.header,
      });
      expect(committed.headerHash).toBe(block.evidence.headerHash);
      await convictWithNoIdxRoute({
        h,
        field,
        block,
        classification,
        fraudulentBlockOutRef: committed.blockOutRef,
      });
    }, 600_000);

    it(`keeps ${LEDGER_ABSENCE_CATEGORY[field]} for an absent input with no same-block producer`, async () => {
      const { block } = await chronologyBlocks(
        undefined,
        "normal_spend",
        field,
      );
      expect(await classifyWithLedger(block, field)).toMatchObject({
        decision: "fault_detected",
        category: LEDGER_ABSENCE_CATEGORY[field],
      });
    });
  }
});
