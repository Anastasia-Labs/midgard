/** One registered input-existence thread over a committed chronology block:
 * the prior deposit block and the chronology block are on the state queue,
 * the target event is prepared, the step scripts are published and the thread
 * is initialised. Each caller drives the steps and states its own verdict. */
import * as SDK from "@al-ft/midgard-sdk";
import type { Script, UTxO } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { inputExistenceWithPublishedFields } from "../../src/input-existence/field-carriage.js";
import {
  type PreparedInputExistence,
  prepareInputExistence,
} from "../../src/input-existence/prepare.js";
import { submitInputExistenceStep } from "../../src/input-existence/submit.js";
import { submitInit } from "../../src/submit-init.js";
import {
  type InputChronology,
  inputExistenceChronologyFixture,
} from "../helpers/input-existence-chronology-fixture.js";
import { chronologySeedDeposits } from "../helpers/input-existence-chronology-seed.js";
import { makeFaultProofEmulatorHarness } from "./emulator/harness.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
} from "./emulator/measurement.js";
import { submitSecondHeaderTx } from "./emulator/setup-tx.js";
import { publishInputExistenceSelectionCarriage } from "./input-existence-execution-context.js";
import { submitRawInputExistenceStep } from "./raw-input-existence-step.js";
import { registerPexcludesExclusionRewardAccount } from "./submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  buildRemovalDeploymentInfo,
  funderPaymentKeyHash,
  network,
  publishPlainReferenceScriptUtxo,
  submitSetupTx,
} from "./submit-init-emulator-shared.js";
import { depositEventsRetainedBlock } from "./transition-trace-retained.deposit-events-retained-block.js";

export type JourneyStep = 0 | 1 | 2 | 3;

/** How one step is carried. `raw` sends it through the raw producer, which
 * carries `prepared` as given, without local admission. */
export type JourneyStepCarriage = {
  readonly raw?: boolean;
  readonly prepared?: PreparedInputExistence;
  readonly claimedPreUtxosRoot?: string;
};

export const checkStepFit = (
  measurement: CompleteSignedTransactionMeasurement,
) => {
  expect(measurement.completeSignedBytes).toBeLessThanOrEqual(16_384);
  expect(measurement.executionMemory).toBeLessThanOrEqual(13_200_000n);
  expect(measurement.executionSteps).toBeLessThanOrEqual(8_000_000_000n);
};

export const startChronologyJourney = async ({
  field,
  scenario,
  publishedFieldCarriage = false,
}: {
  readonly field: 0 | 1;
  readonly scenario: InputChronology;
  /** Publish the selected field as a UTxO even when it fits the redeemer. */
  readonly publishedFieldCarriage?: boolean;
}) => {
  const h = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realNonExistentInput: field === 0,
      realNoReferenceInput: field === 1,
      alwaysFraudProofCatalogue: true,
    },
    registerAdditionalRewardAccounts: registerPexcludesExclusionRewardAccount,
  });
  const category = field === 0 ? "nonExistentInput" : "noReferenceInput";
  const family = h.contracts.fraudProofContracts[category];
  const operatorVkey = await funderPaymentKeyHash(h.funderLucid);
  const startTime = BigInt(
    alignUnixTimeToEmulatorSlotBoundary(
      h.funderLucid,
      h.emulator.now() + 120_000,
    ) - 1,
  );
  const prior = await depositEventsRetainedBlock({
    operatorVkey,
    startTime: startTime - 30_000n,
    endTime: startTime,
    blockSlot: 0n,
    prevHeaderHash: SDK.GENESIS_HEADER_HASH,
    prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    priorLedger: [],
    events: chronologySeedDeposits(),
  });
  const history = await inputExistenceChronologyFixture(scenario, field, {
    operatorVkey,
    startTime,
    predecessorHeaderHash: prior.headerHash,
  });
  expect(history.predecessor.utxosRoot).toBe(prior.header.utxosRoot);
  await submitSetupTx({
    lucid: h.funderLucid,
    contracts: h.contracts,
    nonceUtxo: h.nonceUtxo,
    catalogue: h.catalogue,
    header: prior.header,
  });
  const committed = await submitSecondHeaderTx({
    lucid: h.funderLucid,
    contracts: h.contracts,
    header: history.evidence.header,
  });
  expect(committed.headerHash).toBe(history.evidence.headerHash);
  const rejected =
    scenario === "prior_ledger_present_rejection" ||
    scenario === "absent_input_rejection";
  const prepared = await prepareInputExistence({
    block: history.evidence,
    eventKey: history.targetEvent,
    field,
    inputIndex: rejected ? undefined : 0n,
    predecessor: history.predecessor,
    owner: h.proverSigner.paymentKeyHash,
  });
  const fieldCarriage = publishedFieldCarriage
    ? await publishInputExistenceSelectionCarriage(h, prepared, true)
    : undefined;
  const fieldTier = (
    publishedFieldCarriage
      ? inputExistenceWithPublishedFields(
          prepared,
          h.proverSigner.paymentKeyHash,
        )
      : prepared
  ).fieldPlan.plan.tier;
  const steps: UTxO[] = [];
  for (const [index, step] of family.steps.entries())
    steps.push(
      (
        await publishPlainReferenceScriptUtxo({
          lucid: h.funderLucid,
          script: step.spendingScript,
          label: `chronology ${category} step${index + 1}`,
        })
      ).utxo,
    );
  const publishMint = async (script: Script, label: string) =>
    (
      await publishPlainReferenceScriptUtxo({
        lucid: h.funderLucid,
        script,
        label,
      })
    ).utxo;
  const referenceScripts = {
    steps,
    computationThreadMint: await publishMint(
      h.contracts.computationThread.mintingScript,
      "chronology CT mint",
    ),
    fraudProofMint: await publishMint(
      h.contracts.fraudProof.mintingScript,
      "chronology proof mint",
    ),
  };
  const init = await captureEmulatorSubmission(h.emulator, () =>
    submitInit({
      lucid: h.proverLucid,
      blueprint: h.realBlueprint,
      deploymentInfo: buildRemovalDeploymentInfo(h.contracts, h.catalogue),
      network,
      signer: h.proverSigner,
      fraudCategory: category,
      fraudulentBlockOutRef: committed.blockOutRef,
      witnessReferenceScripts: h.witnessReferenceScripts,
    }),
  );
  checkStepFit(init.measurement);
  let threadOutRef = `${init.result.txHash}#${init.result.firstStepOutputIndex}`;
  const common = (stepIndex: JourneyStep) => ({
    lucid: h.proverLucid,
    blueprint: h.realBlueprint,
    network,
    contracts: {
      hubOraclePolicyId: h.contracts.hubOracle.policyId,
      steps: family.steps,
      computationThread: h.contracts.computationThread,
      fraudProof: h.contracts.fraudProof,
    },
    categoryId: h.catalogue.categories[category].categoryId,
    signer: h.proverSigner,
    threadOutRef,
    stepIndex,
    stateQueueBlockOutRef: committed.blockOutRef,
    referenceScripts,
    witnessReferenceScripts: h.witnessReferenceScripts,
    ...(fieldCarriage === undefined
      ? {}
      : {
          fieldCarriageUtxos: fieldCarriage.fieldCarriageUtxos,
          certificatePolicyId: fieldCarriage.certificatePolicyId,
        }),
  });
  /** Submit one step; on success the thread moves to its output. */
  const send = async (
    stepIndex: JourneyStep,
    carriage: JourneyStepCarriage = {},
  ) => {
    const carried = carriage.prepared ?? prepared;
    const submitted = await captureEmulatorSubmission(h.emulator, () =>
      carriage.raw === true
        ? submitRawInputExistenceStep({
            ...common(stepIndex),
            prepared:
              fieldCarriage === undefined
                ? carried
                : inputExistenceWithPublishedFields(
                    carried,
                    h.proverSigner.paymentKeyHash,
                  ),
            ...(carriage.claimedPreUtxosRoot === undefined
              ? {}
              : { claimedPreUtxosRoot: carriage.claimedPreUtxosRoot }),
          })
        : submitInputExistenceStep({
            ...common(stepIndex),
            prepared: carried,
            usePublishedFieldCarriage: fieldCarriage !== undefined,
          }),
    );
    checkStepFit(submitted.measurement);
    threadOutRef = submitted.result.nextThreadOutRef;
    return submitted.result;
  };
  /** The thread is still at its current step, unspent. */
  const expectThreadKept = async () => {
    const [txHash, index] = threadOutRef.split("#");
    expect(
      await h.proverLucid.utxosByOutRef([
        { txHash: txHash!, outputIndex: Number(index) },
      ]),
    ).toHaveLength(1);
  };
  /** The terminal step minted exactly one proof token at the proof address. */
  const expectConviction = async (result: Awaited<ReturnType<typeof send>>) => {
    expect(result.fraudProofOutRef).toBe(result.nextThreadOutRef);
    const [txHash, index] = result.fraudProofOutRef!.split("#");
    const [proof] = await h.proverLucid.utxosByOutRef([
      { txHash: txHash!, outputIndex: Number(index) },
    ]);
    expect(proof!.address).toBe(h.contracts.fraudProof.spendingScriptAddress);
    expect(
      Object.keys(proof!.assets).filter((unit) =>
        unit.startsWith(h.contracts.fraudProof.policyId),
      ),
    ).toHaveLength(1);
  };
  return {
    h,
    history,
    prepared,
    fieldTier,
    send,
    expectThreadKept,
    expectConviction,
  };
};
