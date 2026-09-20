import {
  buildProtectedOutputSignerMissingChain,
  parseFaultProofBlueprint,
  type SpendingValidator,
} from "@al-ft/midgard-sdk";
import {
  type Data,
  type Network,
  type Script,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  PROTECTED_OUTPUT_SIGNER_MISSING_CATEGORY,
  PROTECTED_OUTPUT_SIGNER_MISSING_ID,
} from "./protected-output-signer-missing.js";

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
  const { steps } = Effect.runSync(
    buildProtectedOutputSignerMissingChain({
      blueprint: parseFaultProofBlueprint(blueprint),
      network,
      hubOraclePolicyId: hubOracleScriptHash,
      computationThread: { policyId: computationThreadPolicyId },
      fraudProof: { policyId: fraudProofPolicyId },
      fraudProofTokenAddressData,
      fieldPreimageCertificatePolicyId,
    }),
  );
  const titles = Object.values(
    PROTECTED_OUTPUT_SIGNER_MISSING_BLUEPRINT_TITLES,
  );
  const adapt = (step: SpendingValidator, index: number) =>
    Object.freeze({
      blueprintTitle: titles[index]!,
      spendingScript: step.spendingScript,
      spendingScriptHash: step.spendingScriptHash,
      spendingScriptAddress: step.spendingScriptAddress,
      referenceOutRef: `${"0".repeat(64)}#0`,
    });
  return [
    adapt(steps[0], 0),
    adapt(steps[1], 1),
    adapt(steps[2], 2),
    adapt(steps[3], 3),
    adapt(steps[4], 4),
  ];
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
