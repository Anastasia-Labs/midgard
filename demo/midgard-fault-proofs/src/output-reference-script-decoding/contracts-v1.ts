import {
  applyParamsToScript,
  type Data,
  type Network,
  type Script,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import {
  OUTPUT_REFERENCE_SCRIPT_DECODING_CATEGORY_V1,
  OUTPUT_REFERENCE_SCRIPT_DECODING_ID_V1,
} from "./output-reference-script-decoding-v1.js";

export const OUTPUT_REFERENCE_SCRIPT_DECODING_CATEGORY_LABEL =
  "output-reference-script-decoding";
export const OUTPUT_REFERENCE_SCRIPT_DECODING_BLUEPRINT_TITLES_V1 = [
  "fraud_proofs/output_reference_script_decoding/step_01.main.spend",
  "fraud_proofs/output_reference_script_decoding/step_02.main.spend",
  "fraud_proofs/output_reference_script_decoding/step_03.main.spend",
  "fraud_proofs/output_reference_script_decoding/step_04.main.spend",
  "fraud_proofs/output_reference_script_decoding/step_05.main.spend",
  "fraud_proofs/output_reference_script_decoding/step_06.main.spend",
] as const;

export type OutputReferenceScriptAppliedStepV1 = Readonly<{
  blueprintTitle: string;
  spendingScript: Script;
  spendingScriptHash: string;
  spendingScriptAddress: string;
  referenceOutRef: string;
}>;
export type OutputReferenceScriptDecodingContractsV1 = Readonly<{
  steps: readonly [
    OutputReferenceScriptAppliedStepV1,
    OutputReferenceScriptAppliedStepV1,
    OutputReferenceScriptAppliedStepV1,
    OutputReferenceScriptAppliedStepV1,
    OutputReferenceScriptAppliedStepV1,
    OutputReferenceScriptAppliedStepV1,
  ];
  computationThread: { policyId: string; mintingScript: Script };
  fraudProof: {
    policyId: string;
    mintingScript: Script;
    spendingScriptAddress: string;
  };
  hubOraclePolicyId: string;
  stateQueuePolicyId: string;
  fieldPreimageCertificatePolicyId: string;
  fieldPreimageCertificateMintingScript: Script;
}>;
export type OutputReferenceScriptBlueprintV1 = Readonly<{
  validators: readonly Readonly<{
    title: string;
    compiledCode: string;
    parameters?: readonly unknown[];
  }>[];
}>;

export const applyOutputReferenceScriptDecodingScriptsV1 = ({
  blueprint,
  network,
  computationThreadPolicyId,
  fraudProofPolicyId,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
  hubOracleScriptHash,
}: {
  blueprint: OutputReferenceScriptBlueprintV1;
  network: Network;
  computationThreadPolicyId: string;
  fraudProofPolicyId: string;
  fraudProofTokenAddressData: Data;
  fieldPreimageCertificatePolicyId: string;
  hubOracleScriptHash: string;
}): OutputReferenceScriptDecodingContractsV1["steps"] => {
  const applied = (
    index: number,
    parameters: readonly Data[],
  ): OutputReferenceScriptAppliedStepV1 => {
    const blueprintTitle =
      OUTPUT_REFERENCE_SCRIPT_DECODING_BLUEPRINT_TITLES_V1[index]!;
    const validator = blueprint.validators.find(
      (entry) => entry.title === blueprintTitle,
    );
    if (validator === undefined)
      throw new Error(
        `outputReferenceScriptDecoding blueprint omitted ${blueprintTitle}`,
      );
    if ((validator.parameters?.length ?? 0) !== parameters.length)
      throw new Error(
        `outputReferenceScriptDecoding ${blueprintTitle} parameter arity changed`,
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
  const step06 = applied(5, [
    fraudProofPolicyId,
    fraudProofTokenAddressData,
    computationThreadPolicyId,
  ]);
  const step05 = applied(4, [
    step06.spendingScriptHash,
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
  return [step01, step02, step03, step04, step05, step06];
};

export type OutputReferenceScriptDecodingManifestV1 = Readonly<{
  schemaVersion: "output-reference-script-decoding-production-manifest-v1";
  category: typeof OUTPUT_REFERENCE_SCRIPT_DECODING_CATEGORY_V1;
  categoryId: typeof OUTPUT_REFERENCE_SCRIPT_DECODING_ID_V1;
  network: Network;
  steps: OutputReferenceScriptDecodingContractsV1["steps"];
  firstStepHash: string;
}>;

export const loadOutputReferenceScriptDecodingManifestV1 = (
  manifest: OutputReferenceScriptDecodingManifestV1,
): OutputReferenceScriptDecodingManifestV1 => {
  if (
    manifest.schemaVersion !==
      "output-reference-script-decoding-production-manifest-v1" ||
    manifest.category !== OUTPUT_REFERENCE_SCRIPT_DECODING_CATEGORY_V1 ||
    manifest.categoryId !== OUTPUT_REFERENCE_SCRIPT_DECODING_ID_V1 ||
    manifest.firstStepHash !== manifest.steps[0].spendingScriptHash
  )
    throw new Error("outputReferenceScriptDecoding manifest identity changed");
  manifest.steps.forEach((step, index) => {
    if (
      step.blueprintTitle !==
        OUTPUT_REFERENCE_SCRIPT_DECODING_BLUEPRINT_TITLES_V1[index] ||
      validatorToScriptHash(step.spendingScript) !== step.spendingScriptHash ||
      validatorToAddress(manifest.network, step.spendingScript) !==
        step.spendingScriptAddress ||
      !/^[0-9a-f]{64}#[0-9]+$/u.test(step.referenceOutRef)
    )
      throw new Error(
        `outputReferenceScriptDecoding step ${(index + 1).toString()} identity changed`,
      );
  });
  return Object.freeze(manifest);
};
