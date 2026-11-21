import {
  parseFaultProofBlueprint,
  type SpendingValidator,
} from "@al-ft/midgard-sdk/core";
import {
  buildSpendInputSignerMissingChain,
  SIGNER_WITNESS_SCAN_TITLES,
} from "@al-ft/midgard-sdk/fraud-proof";
import { type Data, type Network, type Script } from "@lucid-evolution/lucid";
import { Effect } from "effect";

export const SPEND_INPUT_SIGNER_MISSING_BLUEPRINT_TITLES = [
  "fraud_proofs/spend_input_signer_missing/step_01.main.spend",
  "fraud_proofs/spend_input_signer_missing/step_02.main.spend",
  SIGNER_WITNESS_SCAN_TITLES.scan,
  SIGNER_WITNESS_SCAN_TITLES.terminal,
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
  const { steps } = Effect.runSync(
    buildSpendInputSignerMissingChain({
      blueprint: parseFaultProofBlueprint(blueprint),
      network,
      hubOraclePolicyId: hubOracleScriptHash,
      computationThread: { policyId: computationThreadPolicyId },
      fraudProof: { policyId: fraudProofPolicyId },
      fraudProofTokenAddressData,
      fieldPreimageCertificatePolicyId,
    }),
  );
  const titles = Object.values(SPEND_INPUT_SIGNER_MISSING_BLUEPRINT_TITLES);
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
  ];
};
