import { fileURLToPath } from "node:url";

import { AddressData, addressDataFromBech32 } from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { type VanRossemFitMeasurement } from "../src/proof-fit/van-rossem-fit-ledger.js";
import { COMPLETE_LIFECYCLE_BASE_SCENARIOS } from "../src/testing/complete-lifecycle.js";
import {
  applyUnusedScriptWitnessScripts,
  type UnusedScriptWitnessContracts,
} from "../src/unused-script-witness/contracts.js";
import { UNUSED_SCRIPT_WITNESS_CATEGORY_ID } from "../src/unused-script-witness/family.js";
import { submitUnusedScriptWitnessCancel } from "../src/unused-script-witness/submit-cancel.js";
import { submitUnusedScriptWitnessInit } from "../src/unused-script-witness/submit-init.js";
import { buildCatalogueDeploymentInfo } from "./support/emulator/catalogue.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
} from "./support/emulator/measurement.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  buildRemovalDeploymentInfo,
  makeFaultProofEmulatorHarness,
  network,
  publishPlainReferenceScriptUtxo,
  publishRemovalReferenceScripts,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";
import { type UnusedScriptWitnessFixture } from "./support/unused-script-witness-emulator.js";

/**
 * The maximum evidence shape the suite drives through the real chain: 64
 * distinct inline native scripts (the accused one last, so the alternate-source
 * walk of step 04 opens 63 earlier sources, six siblings each, over three
 * self-loop batches) and 256 script purposes of every kind (spend, mint,
 * observer, receive; eight siblings each, eleven step-05 batches), with the
 * validation-traces trie widened to sixteen leaves. Every widened transaction
 * field is checked against the consensus preimage bounds by the fixture. The
 * per-transaction ceiling is fixed by `maximum_scan_batch` (24 openings); the
 * on-chain envelope at consensus-bounded frontier depth is measured by the
 * Aiken selectors in `rule.test.ak`.
 */
export const MAXIMUM_SOURCE_COUNT = 64;

export const MAXIMUM_PURPOSE_COUNT = 256;

export const MAXIMUM_DECOY_TRANSACTION_COUNT = 15;

export const REASON_ARM = "UnusedScriptWitness";

export const AUTHENTICATION_SEAMS = [
  "forced_leaf_reason_coordinate",
  "forced_leaf_header",
  "forced_leaf_root",
  "forced_leaf_direction",
  "validation_traces_root",
  "trace_descriptor",
  "subject_event_key",
  "machine_state",
  "trace_proof",
  "retained_control",
  "source_item",
  "source_language",
  "source_length",
  "source_commitment",
  "source_membership",
  "source_frontier",
  "wrong_successor",
  "alternate_source_item",
  "alternate_source_membership",
  "alternate_source_order",
  "alternate_batch_short",
  "alternate_budget_over_bound",
  "purpose_item",
  "purpose_kind",
  "purpose_membership",
  "purpose_order",
  "scan_checkpoint",
  "scan_batch_short",
  "scan_budget_over_bound",
] as const;

export const CANCELLABLE_STEPS = [
  "step01",
  "step02",
  "step03",
  "step04",
  "step05",
  "step06",
] as const;

export const ledgerPath = fileURLToPath(
  new URL(
    "../../../docs/fault-proofs/size-plans/unused-script-witness-v1-fit-ledger.json",
    import.meta.url,
  ),
);

export const measurements: VanRossemFitMeasurement[] = [];

export const coverage = {
  reasonArms: new Set<string>(),
  successfulDirections: new Set<
    "accepted_invalid" | "forced_rejection_wrong"
  >(),
  scenarios: new Set<(typeof COMPLETE_LIFECYCLE_BASE_SCENARIOS)[number]>(),
  seams: new Set<string>(),
  cancelledSteps: new Set<string>(),
  resumedAfterCheckpoint: false,
  adjacentOverBoundRefused: false,
};

export let publicationsRecorded = false;

export const record = (
  name: string,
  maximumShape: string,
  measurement: CompleteSignedTransactionMeasurement,
) => {
  expect(measurement.l1ByteMargin, name).toBeGreaterThan(0);
  expect(measurement.executionMemory, name).toBeGreaterThan(0n);
  expect(measurement.executionSteps, name).toBeGreaterThan(0n);
  measurements.push({
    name,
    kind: "lifecycle",
    maximumShape,
    signedBytes: measurement.completeSignedBytes,
    memoryUnits: measurement.executionMemory,
    cpuUnits: measurement.executionSteps,
  });
};

export const progress = (message: string) =>
  console.info(`[unused-script-witness-progress] ${message}`);

export const hex = (value: Uint8Array) => Buffer.from(value).toString("hex");

export const makeHarness = async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: { alwaysFraudProofCatalogue: true },
  });
  const addressData = await Effect.runPromise(
    addressDataFromBech32(
      harness.contracts.fraudProof.spendingScriptAddress,
    ).pipe(Effect.map((address) => Data.from(Data.to(address, AddressData)))),
  );
  const applied = applyUnusedScriptWitnessScripts({
    blueprint: harness.realBlueprint,
    network,
    computationThreadPolicyId: harness.contracts.computationThread.policyId,
    fraudProofPolicyId: harness.contracts.fraudProof.policyId,
    fraudProofTokenAddressData: addressData,
    hubOracleScriptHash: harness.contracts.hubOracle.policyId,
  });
  const contracts: UnusedScriptWitnessContracts = {
    steps: applied,
    computationThread: harness.contracts.computationThread,
    fraudProof: harness.contracts.fraudProof,
    hubOraclePolicyId: harness.contracts.hubOracle.policyId,
    stateQueuePolicyId: harness.contracts.stateQueue.policyId,
  };
  const catalogue = await buildCatalogueDeploymentInfo({
    ...harness.contracts.fraudProofs,
    unusedScriptWitness: {
      ...harness.contracts.fraudProofs.unusedScriptWitness,
      spendingScriptHash: applied[0].spendingScriptHash,
    },
  });
  const category = catalogue.categories.unusedScriptWitness;
  expect(category.categoryId).toBe(UNUSED_SCRIPT_WITNESS_CATEGORY_ID);
  expect(category.scriptHash).toBe(applied[0].spendingScriptHash);
  const references: UTxO[] = [];
  // Published after the block setup so the harness nonce UTxO is still
  // unspent when the state-queue block is committed.
  const publishReferences = async () => {
    for (const [index, step] of applied.entries()) {
      const published = await publishPlainReferenceScriptUtxo({
        lucid: harness.funderLucid,
        script: step.spendingScript,
        label: `unused-script-witness-step-${(index + 1).toString()}`,
      });
      references.push(published.utxo);
      expect(
        published.publicationMeasurement.completeSignedBytes,
        `step ${(index + 1).toString()} publication`,
      ).toBeLessThanOrEqual(15_872);
      if (!publicationsRecorded)
        measurements.push({
          name: `publish-step0${(index + 1).toString()}`,
          kind: "publication",
          maximumShape: `applied testnet ${step.blueprintTitle}`,
          signedBytes: published.publicationMeasurement.completeSignedBytes,
          memoryUnits: published.publicationMeasurement.executionMemory,
          cpuUnits: published.publicationMeasurement.executionSteps,
        });
    }
    publicationsRecorded = true;
  };
  const startTime = () =>
    BigInt(
      alignUnixTimeToEmulatorSlotBoundary(
        harness.funderLucid,
        harness.emulator.now() + 120_000,
      ) - 1,
    );
  const common = (threadOutRef: string, stepIndex: number) =>
    ({
      lucid: harness.proverLucid,
      contracts,
      categoryId: category.categoryId,
      signer: harness.proverSigner,
      threadOutRef,
      referenceScriptUtxo: references[stepIndex]!,
    }) as const;
  const init = async (setup: {
    fraudulentBlockOutRef: string;
    headerHash: string;
  }) =>
    await captureEmulatorSubmission(
      harness.emulator,
      async () =>
        await submitUnusedScriptWitnessInit({
          lucid: harness.proverLucid,
          blueprint: harness.realBlueprint,
          network,
          contracts,
          category: category as never,
          catalogue: {
            policyId: harness.contracts.fraudProofCatalogue.policyId,
            spendingScriptAddress:
              harness.contracts.fraudProofCatalogue.spendingScriptAddress,
            root: catalogue.root,
          },
          signer: harness.proverSigner,
          fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
          fraudulentHeaderHash: setup.headerHash,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
    );
  const cancel = async (threadOutRef: string, stepIndex: number) => {
    const captured = await captureEmulatorSubmission(
      harness.emulator,
      async () =>
        await submitUnusedScriptWitnessCancel({
          ...common(threadOutRef, stepIndex),
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
    );
    expect(captured.result.txHash).toMatch(/^[0-9a-f]{64}$/u);
    coverage.cancelledSteps.add(CANCELLABLE_STEPS[stepIndex]!);
    return captured;
  };
  const setupBlock = async (fixture: UnusedScriptWitnessFixture) => {
    const setup = await submitSetupTx({
      lucid: harness.funderLucid,
      contracts: harness.contracts,
      nonceUtxo: harness.nonceUtxo,
      catalogue,
      header: fixture.header,
    });
    await publishReferences();
    return setup;
  };
  const removalDeployment = async () => {
    const removalReferences = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    const base = buildRemovalDeploymentInfo(harness.contracts, catalogue, {
      removalReferenceScripts: removalReferences.published,
    });
    const entry = (step: (typeof applied)[number]) => ({
      scriptHash: step.spendingScriptHash,
      contract: {
        type: step.spendingScript.type,
        cborHex: step.spendingScript.script,
      },
    });
    return {
      ...base,
      contracts: {
        ...base.contracts,
        fraudProofUnusedScriptWitness: entry(applied[0]),
        fraudProofUnusedScriptWitnessStep02: entry(applied[1]),
        fraudProofUnusedScriptWitnessStep03: entry(applied[2]),
        fraudProofUnusedScriptWitnessStep04: entry(applied[3]),
        fraudProofUnusedScriptWitnessStep05: entry(applied[4]),
        fraudProofUnusedScriptWitnessStep06: entry(applied[5]),
      },
    };
  };
  const leaseCoordinator = (token: string) => ({
    acquire: async () => ({
      token,
      source: "emulator" as const,
      renew: async () => undefined,
      release: async () => undefined,
      fail: async () => undefined,
    }),
  });
  return {
    harness,
    applied,
    contracts,
    catalogue,
    category,
    references,
    startTime,
    common,
    init,
    cancel,
    setupBlock,
    removalDeployment,
    leaseCoordinator,
  };
};

export type Harness = Awaited<ReturnType<typeof makeHarness>>;
