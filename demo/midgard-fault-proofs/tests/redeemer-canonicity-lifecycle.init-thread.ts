import { type UTxO } from "@lucid-evolution/lucid";

import { submitCommittedFieldShapeInit } from "../src/committed-field-shape/submit-committed-field-shape-init.js";
import { requireLinearFaultThreadUtxo } from "../src/linear-fault-family.js";
import { submitLinearFaultFinalize } from "../src/linear-fault-finalize.js";
import type { RedeemerCanonicityContracts } from "../src/redeemer-canonicity/contracts.js";
import type { RedeemerCanonicityEvidence } from "../src/redeemer-canonicity/family.js";
import { RedeemerCanonicityStep03RedeemerSchema } from "../src/redeemer-canonicity/schemas.js";
import { submitRedeemerCanonicityStep02Data } from "../src/redeemer-canonicity/submit-step-02-data.js";
import { network } from "./support/emulator/blueprints.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
} from "./support/emulator/measurement.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";
import { createMeasuredFitRecorder } from "./support/measured-fit-ledger.js";

export const measuredFit = createMeasuredFitRecorder(
  "redeemer-canonicity",
  "lifecycle",
  "224 redeemers in certified field 8, accepted-invalid and forced-rejection paths and cancellation",
);

export const emitFit = (
  stage: string,
  measurement: CompleteSignedTransactionMeasurement,
): void => {
  measuredFit.record(
    stage,
    measurement,
    measurement.executionMemory === 0n ? "publication" : "lifecycle",
  );
  if (process.env.MIDGARD_PRINT_FIT !== "1") return;
  console.info(
    `[redeemer-canonicity-fit] ${JSON.stringify({
      stage,
      signedBytes: measurement.completeSignedBytes,
      byteMargin: measurement.l1ByteMargin,
      memory: measurement.executionMemory.toString(),
      memoryMargin: (16_500_000n - measurement.executionMemory).toString(),
      cpu: measurement.executionSteps.toString(),
      cpuMargin: (10_000_000_000n - measurement.executionSteps).toString(),
    })}`,
  );
};

export const familyContracts = (
  harness: Awaited<ReturnType<typeof makeFaultProofEmulatorHarness>>,
): RedeemerCanonicityContracts => {
  const chain = harness.contracts.fraudProofContracts.redeemerCanonicity;
  return {
    steps: chain.steps.map((step, index) => ({
      ...step,
      blueprintTitle: [
        "fraud_proofs/redeemer_canonicity/step_01.main.spend",
        "fraud_proofs/redeemer_canonicity/step_02.main.spend",
        "fraud_proofs/redeemer_canonicity/step_02_data.main.spend",
        "fraud_proofs/redeemer_canonicity/step_03.main.spend",
      ][index]!,
      referenceOutRef: `${"0".repeat(64)}#0`,
    })) as unknown as RedeemerCanonicityContracts["steps"],
    computationThread: harness.contracts.computationThread,
    fraudProof: harness.contracts.fraudProof,
    hubOraclePolicyId: harness.contracts.hubOracle.policyId,
    stateQueuePolicyId: harness.contracts.stateQueue.policyId,
    fieldPreimageCertificatePolicyId:
      harness.contracts.fieldPreimageCertificate.policyId,
    fieldPreimageCertificateMintingScript:
      harness.contracts.fieldPreimageCertificate.mintingScript,
  };
};

export const publishFamilyReferences = async (
  harness: Awaited<ReturnType<typeof makeFaultProofEmulatorHarness>>,
): Promise<readonly [UTxO, UTxO, UTxO, UTxO]> => {
  const result: UTxO[] = [];
  for (const [
    index,
    step,
  ] of harness.contracts.fraudProofContracts.redeemerCanonicity.steps.entries())
    result.push(
      (
        await publishPlainReferenceScriptUtxo({
          lucid: harness.funderLucid,
          script: step.spendingScript,
          label: `redeemer-canonicity-${index.toString()}`,
        })
      ).utxo,
    );
  return result as unknown as readonly [UTxO, UTxO, UTxO, UTxO];
};

export const initThread = async ({
  harness,
  contracts,
  category,
  fraudulentBlockOutRef,
}: {
  readonly harness: Awaited<ReturnType<typeof makeFaultProofEmulatorHarness>>;
  readonly contracts: RedeemerCanonicityContracts;
  readonly category: NonNullable<
    Awaited<
      ReturnType<typeof makeFaultProofEmulatorHarness>
    >["catalogue"]["categories"]["redeemerCanonicity"]
  >;
  readonly fraudulentBlockOutRef: string;
}) =>
  await submitCommittedFieldShapeInit({
    lucid: harness.proverLucid,
    blueprint: harness.realBlueprint,
    network,
    contracts: contracts as never,
    category,
    catalogue: {
      policyId: harness.contracts.fraudProofCatalogue.policyId,
      spendingScriptAddress:
        harness.contracts.fraudProofCatalogue.spendingScriptAddress,
      root: harness.catalogue.root,
    },
    signer: harness.proverSigner,
    fraudulentBlockOutRef,
    witnessReferenceScripts: harness.witnessReferenceScripts,
  });

/**
 * Drives the data step from `threadOutRef` until it hands its verdict to the
 * terminal step; each step's capture goes to `onStep`.
 */
export const readRedeemerData = async ({
  harness,
  contracts,
  categoryId,
  threadOutRef,
  evidence,
  referenceScriptUtxo,
  onStep,
}: {
  readonly harness: Awaited<ReturnType<typeof makeFaultProofEmulatorHarness>>;
  readonly contracts: RedeemerCanonicityContracts;
  readonly categoryId: string;
  readonly threadOutRef: string;
  readonly evidence: RedeemerCanonicityEvidence;
  readonly referenceScriptUtxo: UTxO;
  readonly onStep?: (
    measurement: CompleteSignedTransactionMeasurement,
    index: number,
  ) => void;
}): Promise<{ readonly threadOutRef: string; readonly steps: number }> => {
  let current = threadOutRef;
  for (let steps = 0; ; ) {
    const captured = await captureEmulatorSubmission(harness.emulator, () =>
      submitRedeemerCanonicityStep02Data({
        lucid: harness.proverLucid,
        contracts,
        categoryId,
        signer: harness.proverSigner,
        threadOutRef: current,
        evidence,
        referenceScriptUtxo,
      }),
    );
    onStep?.(captured.measurement, steps);
    steps += 1;
    current = captured.result.nextThreadOutRef;
    if (captured.result.finished) return { threadOutRef: current, steps };
  }
};

/**
 * The step-03 finalize exactly as `submitRedeemerCanonicityStep03` builds it,
 * without its off-chain closing guards, so the validator itself decides.
 */
export const finalizeWithoutGuards = async ({
  harness,
  contracts,
  categoryId,
  threadOutRef,
  referenceScriptUtxo,
}: {
  readonly harness: Awaited<ReturnType<typeof makeFaultProofEmulatorHarness>>;
  readonly contracts: RedeemerCanonicityContracts;
  readonly categoryId: string;
  readonly threadOutRef: string;
  readonly referenceScriptUtxo: Parameters<
    typeof submitLinearFaultFinalize
  >[0]["referenceScriptUtxo"];
}) => {
  const stepIndex = 3;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid: harness.proverLucid,
    contracts,
    categoryId,
    family: "redeemer-canonicity",
    stepIndex,
    threadOutRef,
  });
  return await submitLinearFaultFinalize({
    lucid: harness.proverLucid,
    family: "redeemer-canonicity",
    stepIndex,
    step: contracts.steps[3],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer: harness.proverSigner,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: RedeemerCanonicityStep03RedeemerSchema,
    buildFamilyArgs: ({
      inputIndex,
      outputIndex,
      fraudProofMintRedeemerIndex,
    }) => ({
      input_index: inputIndex,
      output_index: outputIndex,
      fraud_proof_mint_redeemer_index: fraudProofMintRedeemerIndex,
    }),
    referenceScriptUtxo,
    witnessReferenceScripts: harness.witnessReferenceScripts,
    awaitConfirmation: true,
  });
};
