import {
  applyParamsToScript,
  type Data,
  type Network,
  type Script,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import {
  RESOLVED_OUTPUT_NON_CANONICAL_CATEGORY_V1,
  RESOLVED_OUTPUT_NON_CANONICAL_ID_V1,
} from "./resolved-output-non-canonical-v1.js";

export const RESOLVED_OUTPUT_NON_CANONICAL_BLUEPRINT_TITLES_V1 = [
  "fraud_proofs/resolved_output_non_canonical/step_01.main.spend",
  "fraud_proofs/resolved_output_non_canonical/step_02.main.spend",
  "fraud_proofs/resolved_output_non_canonical/step_03.main.spend",
  "fraud_proofs/resolved_output_non_canonical/step_04.main.spend",
  "fraud_proofs/resolved_output_non_canonical/step_05.main.spend",
] as const;

export type ResolvedOutputNonCanonicalAppliedStepV1 = Readonly<{
  blueprintTitle: string;
  spendingScript: Script;
  spendingScriptHash: string;
  spendingScriptAddress: string;
  referenceOutRef: string;
}>;

export type ResolvedOutputNonCanonicalContractsV1 = Readonly<{
  steps: readonly [
    ResolvedOutputNonCanonicalAppliedStepV1,
    ResolvedOutputNonCanonicalAppliedStepV1,
    ResolvedOutputNonCanonicalAppliedStepV1,
    ResolvedOutputNonCanonicalAppliedStepV1,
    ResolvedOutputNonCanonicalAppliedStepV1,
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

export type ResolvedOutputNonCanonicalProductionManifestV1 = Readonly<{
  schemaVersion: "resolved-output-non-canonical-production-manifest-v1";
  category: typeof RESOLVED_OUTPUT_NON_CANONICAL_CATEGORY_V1;
  categoryId: typeof RESOLVED_OUTPUT_NON_CANONICAL_ID_V1;
  network: Network;
  steps: readonly [
    ResolvedOutputNonCanonicalAppliedStepV1,
    ResolvedOutputNonCanonicalAppliedStepV1,
    ResolvedOutputNonCanonicalAppliedStepV1,
    ResolvedOutputNonCanonicalAppliedStepV1,
    ResolvedOutputNonCanonicalAppliedStepV1,
  ];
  firstStepHash: string;
  computationThreadPolicyId: string;
  fraudProofPolicyId: string;
  fraudProofAddress: string;
  hubOraclePolicyId: string;
  fieldPreimageCertificatePolicyId: string;
  stateQueuePolicyId: string;
}>;

export type ResolvedOutputNonCanonicalBlueprintV1 = Readonly<{
  validators: readonly Readonly<{
    title: string;
    compiledCode: string;
    parameters?: readonly unknown[];
  }>[];
}>;

/** Applies the hash-linked chain from its terminal validator back to step 01. */
export const applyResolvedOutputNonCanonicalScriptsV1 = ({
  blueprint,
  network,
  computationThreadPolicyId,
  fraudProofPolicyId,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
  hubOracleScriptHash,
}: {
  readonly blueprint: ResolvedOutputNonCanonicalBlueprintV1;
  readonly network: Network;
  readonly computationThreadPolicyId: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofTokenAddressData: Data;
  readonly fieldPreimageCertificatePolicyId: string;
  readonly hubOracleScriptHash: string;
}): ResolvedOutputNonCanonicalProductionManifestV1["steps"] => {
  const applied = (
    index: number,
    parameters: readonly Data[],
  ): ResolvedOutputNonCanonicalAppliedStepV1 => {
    const blueprintTitle =
      RESOLVED_OUTPUT_NON_CANONICAL_BLUEPRINT_TITLES_V1[index]!;
    const validator = blueprint.validators.find(
      (entry) => entry.title === blueprintTitle,
    );
    if (validator === undefined)
      throw new Error(
        `resolvedOutputNonCanonical: blueprint omitted ${blueprintTitle}`,
      );
    if ((validator.parameters?.length ?? 0) !== parameters.length)
      throw new Error(
        `resolvedOutputNonCanonical: ${blueprintTitle} parameter arity changed`,
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
  return [step01, step02, step03, step04, step05];
};

const h = (value: string, bytes: number, label: string): void => {
  if (!new RegExp(`^[0-9a-f]{${(bytes * 2).toString()}}$`, "u").test(value))
    throw new Error(`resolvedOutputNonCanonical: invalid ${label}`);
};

export const loadResolvedOutputNonCanonicalProductionManifestV1 = (
  manifest: ResolvedOutputNonCanonicalProductionManifestV1,
): ResolvedOutputNonCanonicalProductionManifestV1 => {
  if (
    manifest.schemaVersion !==
      "resolved-output-non-canonical-production-manifest-v1" ||
    manifest.category !== RESOLVED_OUTPUT_NON_CANONICAL_CATEGORY_V1 ||
    manifest.categoryId !== RESOLVED_OUTPUT_NON_CANONICAL_ID_V1
  )
    throw new Error("resolvedOutputNonCanonical: wrong manifest identity");
  manifest.steps.forEach((step, i) => {
    if (
      step.blueprintTitle !==
      RESOLVED_OUTPUT_NON_CANONICAL_BLUEPRINT_TITLES_V1[i]
    )
      throw new Error(
        `resolvedOutputNonCanonical: step ${(i + 1).toString()} title mismatch`,
      );
    if (validatorToScriptHash(step.spendingScript) !== step.spendingScriptHash)
      throw new Error(
        `resolvedOutputNonCanonical: step ${(i + 1).toString()} hash mismatch`,
      );
    if (
      validatorToAddress(manifest.network, step.spendingScript) !==
      step.spendingScriptAddress
    )
      throw new Error(
        `resolvedOutputNonCanonical: step ${(i + 1).toString()} address mismatch`,
      );
    if (!/^[0-9a-f]{64}#[0-9]+$/u.test(step.referenceOutRef))
      throw new Error(
        `resolvedOutputNonCanonical: step ${(i + 1).toString()} reference out-ref mismatch`,
      );
  });
  if (manifest.firstStepHash !== manifest.steps[0].spendingScriptHash)
    throw new Error(
      "resolvedOutputNonCanonical: first-step hash is not step 01",
    );
  h(manifest.computationThreadPolicyId, 28, "computation-thread policy");
  h(manifest.fraudProofPolicyId, 28, "fraud-proof policy");
  h(manifest.hubOraclePolicyId, 28, "hub oracle");
  h(manifest.fieldPreimageCertificatePolicyId, 28, "certificate policy");
  h(manifest.stateQueuePolicyId, 28, "state-queue policy");
  return Object.freeze(manifest);
};
