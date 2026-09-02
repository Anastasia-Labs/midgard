import {
  applyParamsToScript,
  type Data,
  type Network,
  type Script,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import {
  SPEND_INPUT_SIGNER_MISSING_CATEGORY_V1,
  SPEND_INPUT_SIGNER_MISSING_ID_V1,
} from "./spend-input-signer-missing-v1.js";

export const SPEND_INPUT_SIGNER_MISSING_BLUEPRINT_TITLES_V1 = [
  "fraud_proofs/spend_input_signer_missing/step_01.main.spend",
  "fraud_proofs/spend_input_signer_missing/step_02.main.spend",
  "fraud_proofs/spend_input_signer_missing/step_03.main.spend",
  "fraud_proofs/spend_input_signer_missing/step_04.main.spend",
  "fraud_proofs/spend_input_signer_missing/step_05.main.spend",
] as const;

export type SpendInputSignerMissingAppliedStepV1 = Readonly<{
  blueprintTitle: string;
  spendingScript: Script;
  spendingScriptHash: string;
  spendingScriptAddress: string;
  referenceOutRef: string;
}>;

export type SpendInputSignerMissingContractsV1 = Readonly<{
  steps: readonly [
    SpendInputSignerMissingAppliedStepV1,
    SpendInputSignerMissingAppliedStepV1,
    SpendInputSignerMissingAppliedStepV1,
    SpendInputSignerMissingAppliedStepV1,
    SpendInputSignerMissingAppliedStepV1,
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

export type SpendInputSignerMissingProductionManifestV1 = Readonly<{
  schemaVersion: "spend-input-signer-missing-production-manifest-v1";
  category: typeof SPEND_INPUT_SIGNER_MISSING_CATEGORY_V1;
  categoryId: typeof SPEND_INPUT_SIGNER_MISSING_ID_V1;
  network: Network;
  steps: SpendInputSignerMissingContractsV1["steps"];
  firstStepHash: string;
  computationThreadPolicyId: string;
  fraudProofPolicyId: string;
  fraudProofAddress: string;
  hubOraclePolicyId: string;
  fieldPreimageCertificatePolicyId: string;
  stateQueuePolicyId: string;
}>;

export type SpendInputSignerMissingBlueprintV1 = Readonly<{
  validators: readonly Readonly<{
    title: string;
    compiledCode: string;
    parameters?: readonly unknown[];
  }>[];
}>;

export const applySpendInputSignerMissingScriptsV1 = ({
  blueprint,
  network,
  computationThreadPolicyId,
  fraudProofPolicyId,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
  hubOracleScriptHash,
}: {
  readonly blueprint: SpendInputSignerMissingBlueprintV1;
  readonly network: Network;
  readonly computationThreadPolicyId: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofTokenAddressData: Data;
  readonly fieldPreimageCertificatePolicyId: string;
  readonly hubOracleScriptHash: string;
}): SpendInputSignerMissingContractsV1["steps"] => {
  const applied = (
    index: number,
    parameters: readonly Data[],
  ): SpendInputSignerMissingAppliedStepV1 => {
    const blueprintTitle =
      SPEND_INPUT_SIGNER_MISSING_BLUEPRINT_TITLES_V1[index]!;
    const validator = blueprint.validators.find(
      (value) => value.title === blueprintTitle,
    );
    if (validator === undefined)
      throw new Error(
        `${SPEND_INPUT_SIGNER_MISSING_CATEGORY_V1}: blueprint omitted ${blueprintTitle}`,
      );
    if ((validator.parameters?.length ?? 0) !== parameters.length)
      throw new Error(
        `${SPEND_INPUT_SIGNER_MISSING_CATEGORY_V1}: ${blueprintTitle} parameter arity changed`,
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
    throw new Error(
      `${SPEND_INPUT_SIGNER_MISSING_CATEGORY_V1}: invalid ${label}`,
    );
};

export const loadSpendInputSignerMissingProductionManifestV1 = (
  manifest: SpendInputSignerMissingProductionManifestV1,
): SpendInputSignerMissingProductionManifestV1 => {
  if (
    manifest.schemaVersion !==
      "spend-input-signer-missing-production-manifest-v1" ||
    manifest.category !== SPEND_INPUT_SIGNER_MISSING_CATEGORY_V1 ||
    manifest.categoryId !== SPEND_INPUT_SIGNER_MISSING_ID_V1
  )
    throw new Error(
      `${SPEND_INPUT_SIGNER_MISSING_CATEGORY_V1}: wrong manifest identity`,
    );
  manifest.steps.forEach((step, index) => {
    if (
      step.blueprintTitle !==
      SPEND_INPUT_SIGNER_MISSING_BLUEPRINT_TITLES_V1[index]
    )
      throw new Error(
        `${SPEND_INPUT_SIGNER_MISSING_CATEGORY_V1}: physical step order changed`,
      );
    if (validatorToScriptHash(step.spendingScript) !== step.spendingScriptHash)
      throw new Error(
        `${SPEND_INPUT_SIGNER_MISSING_CATEGORY_V1}: applied step hash mismatch`,
      );
    if (
      validatorToAddress(manifest.network, step.spendingScript) !==
      step.spendingScriptAddress
    )
      throw new Error(
        `${SPEND_INPUT_SIGNER_MISSING_CATEGORY_V1}: applied step address mismatch`,
      );
    if (!/^[0-9a-f]{64}#[0-9]+$/u.test(step.referenceOutRef))
      throw new Error(
        `${SPEND_INPUT_SIGNER_MISSING_CATEGORY_V1}: invalid step reference out-ref`,
      );
  });
  if (manifest.firstStepHash !== manifest.steps[0].spendingScriptHash)
    throw new Error(
      `${SPEND_INPUT_SIGNER_MISSING_CATEGORY_V1}: first-step identity mismatch`,
    );
  requireHash(manifest.computationThreadPolicyId, "computation-thread policy");
  requireHash(manifest.fraudProofPolicyId, "fraud-proof policy");
  requireHash(manifest.hubOraclePolicyId, "hub oracle");
  requireHash(manifest.fieldPreimageCertificatePolicyId, "certificate policy");
  requireHash(manifest.stateQueuePolicyId, "state queue policy");
  return Object.freeze(manifest);
};
