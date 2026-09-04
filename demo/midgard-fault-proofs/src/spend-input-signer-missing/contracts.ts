import {
  applyParamsToScript,
  type Data,
  type Network,
  type Script,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import {
  SPEND_INPUT_SIGNER_MISSING_CATEGORY,
  SPEND_INPUT_SIGNER_MISSING_ID,
} from "./spend-input-signer-missing.js";

export const SPEND_INPUT_SIGNER_MISSING_BLUEPRINT_TITLES = [
  "fraud_proofs/spend_input_signer_missing/step_01.main.spend",
  "fraud_proofs/spend_input_signer_missing/step_02.main.spend",
  "fraud_proofs/spend_input_signer_missing/step_03.main.spend",
  "fraud_proofs/spend_input_signer_missing/step_04.main.spend",
  "fraud_proofs/spend_input_signer_missing/step_05.main.spend",
] as const;

export type SpendInputSignerMissingAppliedStep = Readonly<{
  blueprintTitle: string;
  spendingScript: Script;
  spendingScriptHash: string;
  spendingScriptAddress: string;
  referenceOutRef: string;
}>;

export type SpendInputSignerMissingContracts = Readonly<{
  steps: readonly [
    SpendInputSignerMissingAppliedStep,
    SpendInputSignerMissingAppliedStep,
    SpendInputSignerMissingAppliedStep,
    SpendInputSignerMissingAppliedStep,
    SpendInputSignerMissingAppliedStep,
  ];
  computationThread: {
    readonly policyId: string;
    readonly mintingScript: Script;
  };
  fraudProof: {
    readonly policyId: string;
    readonly mintingScript: Script;
    readonly spendingScriptAddress: string;
  };
  hubOraclePolicyId: string;
  stateQueuePolicyId: string;
  fieldPreimageCertificatePolicyId: string;
  fieldPreimageCertificateMintingScript: Script;
}>;

export type SpendInputSignerMissingManifest = Readonly<{
  schemaVersion: "spend-input-signer-missing-production-manifest-v1";
  category: typeof SPEND_INPUT_SIGNER_MISSING_CATEGORY;
  categoryId: typeof SPEND_INPUT_SIGNER_MISSING_ID;
  network: Network;
  steps: SpendInputSignerMissingContracts["steps"];
  firstStepHash: string;
  computationThreadPolicyId: string;
  fraudProofPolicyId: string;
  fraudProofAddress: string;
  hubOraclePolicyId: string;
  fieldPreimageCertificatePolicyId: string;
  stateQueuePolicyId: string;
}>;

export type SpendInputSignerMissingBlueprint = Readonly<{
  validators: readonly Readonly<{
    title: string;
    compiledCode: string;
    parameters?: readonly unknown[];
  }>[];
}>;

export const applySpendInputSignerMissingScripts = ({
  blueprint,
  network,
  computationThreadPolicyId,
  fraudProofPolicyId,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
  hubOracleScriptHash,
}: {
  readonly blueprint: SpendInputSignerMissingBlueprint;
  readonly network: Network;
  readonly computationThreadPolicyId: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofTokenAddressData: Data;
  readonly fieldPreimageCertificatePolicyId: string;
  readonly hubOracleScriptHash: string;
}): SpendInputSignerMissingContracts["steps"] => {
  const applied = (
    index: number,
    parameters: readonly Data[],
  ): SpendInputSignerMissingAppliedStep => {
    const blueprintTitle = SPEND_INPUT_SIGNER_MISSING_BLUEPRINT_TITLES[index]!;
    const validator = blueprint.validators.find(
      (value) => value.title === blueprintTitle,
    );
    if (validator === undefined)
      throw new Error(
        `${SPEND_INPUT_SIGNER_MISSING_CATEGORY}: blueprint omitted ${blueprintTitle}`,
      );
    if ((validator.parameters?.length ?? 0) !== parameters.length)
      throw new Error(
        `${SPEND_INPUT_SIGNER_MISSING_CATEGORY}: ${blueprintTitle} parameter arity changed`,
      );
    const spendingScript: Script = {
      type: "PlutusV3",
      script: applyParamsToScript(validator.compiledCode, [...parameters]),
    };
    return Object.freeze({
      blueprintTitle,
      spendingScript,
      spendingScriptHash: validatorToScriptHash(spendingScript),
      spendingScriptAddress: validatorToAddress(network, spendingScript),
      referenceOutRef: `${"0".repeat(64)}#0`,
    });
  };
  const step05 = applied(4, [
    fraudProofPolicyId,
    fraudProofTokenAddressData,
    computationThreadPolicyId,
  ]);
  const step04 = applied(3, [
    step05.spendingScriptHash,
    computationThreadPolicyId,
    fieldPreimageCertificatePolicyId,
  ]);
  const step03 = applied(2, [
    step04.spendingScriptHash,
    computationThreadPolicyId,
    fieldPreimageCertificatePolicyId,
  ]);
  const step02 = applied(1, [
    step03.spendingScriptHash,
    computationThreadPolicyId,
    fieldPreimageCertificatePolicyId,
  ]);
  const step01 = applied(0, [
    step02.spendingScriptHash,
    computationThreadPolicyId,
    hubOracleScriptHash,
  ]);
  return [step01, step02, step03, step04, step05];
};

const requireHash = (value: string, label: string): void => {
  if (!/^[0-9a-f]{56}$/u.test(value))
    throw new Error(`${SPEND_INPUT_SIGNER_MISSING_CATEGORY}: invalid ${label}`);
};

export const loadSpendInputSignerMissingManifest = (
  manifest: SpendInputSignerMissingManifest,
): SpendInputSignerMissingManifest => {
  if (
    manifest.schemaVersion !==
      "spend-input-signer-missing-production-manifest-v1" ||
    manifest.category !== SPEND_INPUT_SIGNER_MISSING_CATEGORY ||
    manifest.categoryId !== SPEND_INPUT_SIGNER_MISSING_ID
  )
    throw new Error(
      `${SPEND_INPUT_SIGNER_MISSING_CATEGORY}: wrong manifest identity`,
    );
  manifest.steps.forEach((step, index) => {
    if (
      step.blueprintTitle !== SPEND_INPUT_SIGNER_MISSING_BLUEPRINT_TITLES[index]
    )
      throw new Error(
        `${SPEND_INPUT_SIGNER_MISSING_CATEGORY}: physical step order changed`,
      );
    if (validatorToScriptHash(step.spendingScript) !== step.spendingScriptHash)
      throw new Error(
        `${SPEND_INPUT_SIGNER_MISSING_CATEGORY}: applied step hash mismatch`,
      );
    if (
      validatorToAddress(manifest.network, step.spendingScript) !==
      step.spendingScriptAddress
    )
      throw new Error(
        `${SPEND_INPUT_SIGNER_MISSING_CATEGORY}: applied step address mismatch`,
      );
    if (!/^[0-9a-f]{64}#[0-9]+$/u.test(step.referenceOutRef))
      throw new Error(
        `${SPEND_INPUT_SIGNER_MISSING_CATEGORY}: invalid step reference out-ref`,
      );
  });
  if (manifest.firstStepHash !== manifest.steps[0].spendingScriptHash)
    throw new Error(
      `${SPEND_INPUT_SIGNER_MISSING_CATEGORY}: first-step identity mismatch`,
    );
  requireHash(manifest.computationThreadPolicyId, "computation-thread policy");
  requireHash(manifest.fraudProofPolicyId, "fraud-proof policy");
  requireHash(manifest.hubOraclePolicyId, "hub oracle");
  requireHash(manifest.fieldPreimageCertificatePolicyId, "certificate policy");
  requireHash(manifest.stateQueuePolicyId, "state queue policy");
  return Object.freeze(manifest);
};
