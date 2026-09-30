import { fileURLToPath } from "node:url";

import { AddressData, addressDataFromBech32 } from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { type VanRossemFitMeasurement } from "../src/proof-fit/van-rossem-fit-ledger.js";
import {
  applyReceivePurposeLanguageScripts,
  type ReceivePurposeLanguageContracts,
} from "../src/receive-purpose-language/contracts.js";
import { submitReceivePurposeLanguageCancel } from "../src/receive-purpose-language/submit-cancel.js";
import { submitReceivePurposeLanguageInit } from "../src/receive-purpose-language/submit-init.js";
import { type ReceivePurposeLanguageAuthentication } from "../src/receive-purpose-language/submit-step-02.js";
import { COMPLETE_LIFECYCLE_BASE_SCENARIOS } from "../src/testing/complete-lifecycle.js";
import { buildCatalogueDeploymentInfo } from "./support/emulator/catalogue.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
} from "./support/emulator/measurement.js";
import { type ReceivePurposeFixture } from "./support/receive-purpose-language-emulator.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  buildRemovalDeploymentInfo,
  makeFaultProofEmulatorHarness,
  network,
  publishPlainReferenceScriptUtxo,
  publishRemovalReferenceScripts,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";

/**
 * The maximum evidence shape the suite drives through the real chain: the
 * accused receive purpose plus 255 native spend purposes (distinct spent
 * out-refs under one trivial script), so the purpose and execution frontiers
 * of the native-scripts control carry 256 leaves (an eight-sibling path from
 * every position) while every widened transaction field stays inside its
 * 32,768-byte consensus bound; the decoys widen the validation-traces trie so
 * the descriptor membership proof has real branch steps. The on-chain envelope itself (every
 * frontier at 4,095 leaves) is measured by
 * `receive_authenticates_consensus_bounded_frontiers` in
 * `onchain/aiken/lib/midgard/fraud-proofs/receive-purpose-language/rule.test.ak`;
 * the size plan combines both rows.
 */
export const MAXIMUM_PURPOSE_COUNT = 256;

export const MAXIMUM_DECOY_TRANSACTION_COUNT = 15;

export const REASON_ARM = "ReceivePurposePlutusV3Forbidden";

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
  "native_control",
  "purpose_item",
  "source_language",
  "execution_membership",
] as const;

export const CANCELLABLE_STEPS = ["step01", "step02", "step03"] as const;

export const ledgerPath = fileURLToPath(
  new URL(
    "../../../docs/fault-proofs/size-plans/receive-purpose-language-v1-fit-ledger.json",
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
  console.info(`[receive-purpose-language-progress] ${message}`);

export const makeHarness = async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: { alwaysFraudProofCatalogue: true },
  });
  const addressData = await Effect.runPromise(
    addressDataFromBech32(
      harness.contracts.fraudProof.spendingScriptAddress,
    ).pipe(Effect.map((address) => Data.from(Data.to(address, AddressData)))),
  );
  const applied = applyReceivePurposeLanguageScripts({
    blueprint: harness.realBlueprint,
    network,
    computationThreadPolicyId: harness.contracts.computationThread.policyId,
    fraudProofPolicyId: harness.contracts.fraudProof.policyId,
    fraudProofTokenAddressData: addressData,
    hubOracleScriptHash: harness.contracts.hubOracle.spendingScriptHash,
  });
  const contracts: ReceivePurposeLanguageContracts = {
    steps: applied,
    computationThread: harness.contracts.computationThread,
    fraudProof: harness.contracts.fraudProof,
    hubOraclePolicyId: harness.contracts.hubOracle.policyId,
    stateQueuePolicyId: harness.contracts.stateQueue.policyId,
  };
  const catalogue = await buildCatalogueDeploymentInfo({
    ...harness.contracts.fraudProofs,
    receivePurposeLanguage: {
      ...harness.contracts.fraudProofs.receivePurposeLanguage,
      spendingScriptHash: applied[0].spendingScriptHash,
    },
  });
  const category = catalogue.categories.receivePurposeLanguage;
  expect(category.categoryId).toBe("00000034");
  expect(category.scriptHash).toBe(applied[0].spendingScriptHash);
  const references: UTxO[] = [];
  // Published after the block setup so the harness nonce UTxO is still
  // unspent when the state-queue block is committed.
  const publishReferences = async () => {
    for (const [index, step] of applied.entries()) {
      const published = await publishPlainReferenceScriptUtxo({
        lucid: harness.funderLucid,
        script: step.spendingScript,
        label: `receive-purpose-language-step-${(index + 1).toString()}`,
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
        await submitReceivePurposeLanguageInit({
          lucid: harness.proverLucid,
          blueprint: harness.realBlueprint,
          network,
          contracts,
          category,
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
        await submitReceivePurposeLanguageCancel({
          ...common(threadOutRef, stepIndex),
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
    );
    expect(captured.result.txHash).toMatch(/^[0-9a-f]{64}$/u);
    coverage.cancelledSteps.add(CANCELLABLE_STEPS[stepIndex]!);
    return captured;
  };
  const setupBlock = async (fixture: ReceivePurposeFixture) => {
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
        fraudProofReceivePurposeLanguage: entry(applied[0]),
        fraudProofReceivePurposeLanguageStep02: entry(applied[1]),
        fraudProofReceivePurposeLanguageStep03: entry(applied[2]),
      },
    };
  };
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
  };
};

export type Harness = Awaited<ReturnType<typeof makeHarness>>;

export const shapeLabel = (fixture: ReceivePurposeFixture) =>
  `${fixture.spec.purposeCount.toString()} purposes (${fixture.spec.language} receive at execution ${fixture.executionIndex.toString()}), ${fixture.header.validationTraceCount.toString()} validation traces`;

export const mutate = (
  authentication: ReceivePurposeLanguageAuthentication,
  patch: Partial<ReceivePurposeLanguageAuthentication>,
): ReceivePurposeLanguageAuthentication => ({ ...authentication, ...patch });
