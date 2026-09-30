import { type UTxO } from "@lucid-evolution/lucid";

import { submitCommittedFieldShapeInit } from "../src/committed-field-shape/submit-committed-field-shape-init.js";
import type { RedeemerCanonicityContracts } from "../src/redeemer-canonicity/contracts.js";
import { network } from "./support/emulator/blueprints.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { type CompleteSignedTransactionMeasurement } from "./support/emulator/measurement.js";
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
): Promise<readonly [UTxO, UTxO, UTxO]> => {
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
  return result as unknown as readonly [UTxO, UTxO, UTxO];
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
