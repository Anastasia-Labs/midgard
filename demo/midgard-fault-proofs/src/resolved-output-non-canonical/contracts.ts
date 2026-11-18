import {
  buildResolvedOutputNonCanonicalChain,
  parseFaultProofBlueprint,
  type SpendingValidator,
} from "@al-ft/midgard-sdk";
import { type Data, type Network, type Script } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  RESOLVED_OUTPUT_NON_CANONICAL_CATEGORY,
  RESOLVED_OUTPUT_NON_CANONICAL_ID,
} from "./resolved-output-non-canonical.js";

export const RESOLVED_OUTPUT_NON_CANONICAL_BLUEPRINT_TITLES = [
  "fraud_proofs/resolved_output_non_canonical/step_01.main.spend",
  "fraud_proofs/resolved_output_non_canonical/step_02.main.spend",
  "fraud_proofs/resolved_output_non_canonical/step_03.main.spend",
  "fraud_proofs/resolved_output_non_canonical/step_04.main.spend",
  "fraud_proofs/resolved_output_non_canonical/step_04_datum.main.spend",
  "fraud_proofs/resolved_output_non_canonical/step_05.main.spend",
] as const;

export type ResolvedOutputNonCanonicalAppliedStep = Readonly<{
  blueprintTitle: string;
  spendingScript: Script;
  spendingScriptHash: string;
  spendingScriptAddress: string;
  referenceOutRef: string;
}>;

export type ResolvedOutputNonCanonicalContracts = Readonly<{
  steps: readonly [
    ResolvedOutputNonCanonicalAppliedStep,
    ResolvedOutputNonCanonicalAppliedStep,
    ResolvedOutputNonCanonicalAppliedStep,
    ResolvedOutputNonCanonicalAppliedStep,
    ResolvedOutputNonCanonicalAppliedStep,
    ResolvedOutputNonCanonicalAppliedStep,
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

export type ResolvedOutputNonCanonicalManifest = Readonly<{
  schemaVersion: "resolved-output-non-canonical-production-manifest-v1";
  category: typeof RESOLVED_OUTPUT_NON_CANONICAL_CATEGORY;
  categoryId: typeof RESOLVED_OUTPUT_NON_CANONICAL_ID;
  network: Network;
  steps: readonly [
    ResolvedOutputNonCanonicalAppliedStep,
    ResolvedOutputNonCanonicalAppliedStep,
    ResolvedOutputNonCanonicalAppliedStep,
    ResolvedOutputNonCanonicalAppliedStep,
    ResolvedOutputNonCanonicalAppliedStep,
    ResolvedOutputNonCanonicalAppliedStep,
  ];
  firstStepHash: string;
  computationThreadPolicyId: string;
  fraudProofPolicyId: string;
  fraudProofAddress: string;
  hubOraclePolicyId: string;
  fieldPreimageCertificatePolicyId: string;
  stateQueuePolicyId: string;
}>;

export type ResolvedOutputNonCanonicalBlueprint = Readonly<{
  validators: readonly Readonly<{
    title: string;
    compiledCode: string;
    parameters?: readonly unknown[];
  }>[];
}>;

/** Applies the hash-linked chain from its terminal validator back to step 01. */
export const applyResolvedOutputNonCanonicalScripts = ({
  blueprint,
  network,
  computationThreadPolicyId,
  fraudProofPolicyId,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
  hubOracleScriptHash,
}: {
  readonly blueprint: ResolvedOutputNonCanonicalBlueprint;
  readonly network: Network;
  readonly computationThreadPolicyId: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofTokenAddressData: Data;
  readonly fieldPreimageCertificatePolicyId: string;
  readonly hubOracleScriptHash: string;
}): ResolvedOutputNonCanonicalManifest["steps"] => {
  const { steps } = Effect.runSync(
    buildResolvedOutputNonCanonicalChain({
      blueprint: parseFaultProofBlueprint(blueprint),
      network,
      hubOraclePolicyId: hubOracleScriptHash,
      computationThread: { policyId: computationThreadPolicyId },
      fraudProof: { policyId: fraudProofPolicyId },
      fraudProofTokenAddressData,
      fieldPreimageCertificatePolicyId,
    }),
  );
  const titles = Object.values(RESOLVED_OUTPUT_NON_CANONICAL_BLUEPRINT_TITLES);
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
    adapt(steps[5], 5),
  ];
};
