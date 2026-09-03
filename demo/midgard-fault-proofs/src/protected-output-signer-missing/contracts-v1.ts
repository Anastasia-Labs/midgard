import {
  applyParamsToScript,
  type Data,
  type Network,
  type Script,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import {
  PROTECTED_OUTPUT_SIGNER_MISSING_CATEGORY,
  PROTECTED_OUTPUT_SIGNER_MISSING_ID,
} from "./protected-output-signer-missing-v1.js";

export const PROTECTED_OUTPUT_SIGNER_MISSING_BLUEPRINT_TITLES = [
  "fraud_proofs/protected_output_signer_missing/step_01.main.spend",
  "fraud_proofs/protected_output_signer_missing/step_02.main.spend",
  "fraud_proofs/protected_output_signer_missing/step_03.main.spend",
  "fraud_proofs/protected_output_signer_missing/step_04.main.spend",
  "fraud_proofs/protected_output_signer_missing/step_05.main.spend",
] as const;

export type ProtectedOutputSignerAppliedStep = Readonly<{
  blueprintTitle: string;
  spendingScript: Script;
  spendingScriptHash: string;
  spendingScriptAddress: string;
  referenceOutRef: string;
}>;
export type ProtectedOutputSignerMissingContracts = Readonly<{
  steps: ProtectedOutputSignerManifest["steps"];
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
export type ProtectedOutputSignerBlueprint = Readonly<{
  validators: readonly Readonly<{
    title: string;
    compiledCode: string;
    parameters?: readonly unknown[];
  }>[];
}>;
export type ProtectedOutputSignerManifest = Readonly<{
  schemaVersion: "protected-output-signer-missing-production-manifest-v1";
  category: typeof PROTECTED_OUTPUT_SIGNER_MISSING_CATEGORY;
  categoryId: typeof PROTECTED_OUTPUT_SIGNER_MISSING_ID;
  network: Network;
  steps: readonly [
    ProtectedOutputSignerAppliedStep,
    ProtectedOutputSignerAppliedStep,
    ProtectedOutputSignerAppliedStep,
    ProtectedOutputSignerAppliedStep,
    ProtectedOutputSignerAppliedStep,
  ];
  firstStepHash: string;
  computationThreadPolicyId: string;
  fraudProofPolicyId: string;
  fraudProofAddress: string;
  hubOraclePolicyId: string;
  fieldPreimageCertificatePolicyId: string;
  stateQueuePolicyId: string;
}>;

export const applyProtectedOutputSignerMissingScripts = ({
  blueprint,
  network,
  computationThreadPolicyId,
  fraudProofPolicyId,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
  hubOracleScriptHash,
}: {
  readonly blueprint: ProtectedOutputSignerBlueprint;
  readonly network: Network;
  readonly computationThreadPolicyId: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofTokenAddressData: Data;
  readonly fieldPreimageCertificatePolicyId: string;
  readonly hubOracleScriptHash: string;
}): ProtectedOutputSignerManifest["steps"] => {
  const applied = (
    index: number,
    parameters: readonly Data[],
  ): ProtectedOutputSignerAppliedStep => {
    const blueprintTitle =
      PROTECTED_OUTPUT_SIGNER_MISSING_BLUEPRINT_TITLES[index]!;
    const validator = blueprint.validators.find(
      (entry) => entry.title === blueprintTitle,
    );
    if (validator === undefined)
      throw new Error(
        `protectedOutputSignerMissing blueprint omitted ${blueprintTitle}`,
      );
    if ((validator.parameters?.length ?? 0) !== parameters.length)
      throw new Error(
        `protectedOutputSignerMissing ${blueprintTitle} parameter arity changed`,
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

export const loadProtectedOutputSignerMissingManifest = (
  manifest: ProtectedOutputSignerManifest,
): ProtectedOutputSignerManifest => {
  if (
    manifest.schemaVersion !==
      "protected-output-signer-missing-production-manifest-v1" ||
    manifest.category !== PROTECTED_OUTPUT_SIGNER_MISSING_CATEGORY ||
    manifest.categoryId !== PROTECTED_OUTPUT_SIGNER_MISSING_ID ||
    manifest.firstStepHash !== manifest.steps[0].spendingScriptHash
  )
    throw new Error("protectedOutputSignerMissing manifest identity changed");
  manifest.steps.forEach((step, index) => {
    if (
      step.blueprintTitle !==
        PROTECTED_OUTPUT_SIGNER_MISSING_BLUEPRINT_TITLES[index] ||
      validatorToScriptHash(step.spendingScript) !== step.spendingScriptHash ||
      validatorToAddress(manifest.network, step.spendingScript) !==
        step.spendingScriptAddress ||
      !/^[0-9a-f]{64}#[0-9]+$/u.test(step.referenceOutRef)
    )
      throw new Error(
        `protectedOutputSignerMissing step ${(index + 1).toString()} identity changed`,
      );
  });
  return Object.freeze(manifest);
};
