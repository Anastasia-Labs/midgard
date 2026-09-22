import {
  buildReceivePurposeLanguageChain,
  parseFaultProofBlueprint,
  type SpendingValidator,
} from "@al-ft/midgard-sdk";
import { type Data, type Network, type Script } from "@lucid-evolution/lucid";
import { Effect } from "effect";

export const RECEIVE_PURPOSE_LANGUAGE_BLUEPRINT_TITLES = Object.freeze([
  "fraud_proofs/receive_purpose_language/step_01.main.spend",
  "fraud_proofs/receive_purpose_language/step_02.main.spend",
  "fraud_proofs/receive_purpose_language/step_03.main.spend",
] as const);

export type ReceivePurposeLanguageStepContract = Readonly<{
  blueprintTitle: string;
  spendingScript: Script;
  spendingScriptHash: string;
  spendingScriptAddress: string;
  referenceOutRef: string;
}>;
export type ReceivePurposeLanguageContracts = Readonly<{
  steps: readonly [
    ReceivePurposeLanguageStepContract,
    ReceivePurposeLanguageStepContract,
    ReceivePurposeLanguageStepContract,
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
  /** Authenticates the accused state-queue block on the accepted step-01 path. */
  stateQueuePolicyId: string;
}>;
type Blueprint = Readonly<{
  validators: readonly Readonly<{
    title: string;
    compiledCode: string;
    parameters?: readonly unknown[];
  }>[];
}>;

export const applyReceivePurposeLanguageScripts = ({
  blueprint,
  network,
  computationThreadPolicyId,
  fraudProofPolicyId,
  fraudProofTokenAddressData,
  hubOracleScriptHash,
}: {
  blueprint: Blueprint;
  network: Network;
  computationThreadPolicyId: string;
  fraudProofPolicyId: string;
  fraudProofTokenAddressData: Data;
  hubOracleScriptHash: string;
}): ReceivePurposeLanguageContracts["steps"] => {
  const { steps } = Effect.runSync(
    buildReceivePurposeLanguageChain({
      blueprint: parseFaultProofBlueprint(blueprint),
      network,
      hubOraclePolicyId: hubOracleScriptHash,
      computationThread: { policyId: computationThreadPolicyId },
      fraudProof: { policyId: fraudProofPolicyId },
      fraudProofTokenAddressData,
    }),
  );
  const titles = Object.values(RECEIVE_PURPOSE_LANGUAGE_BLUEPRINT_TITLES);
  const adapt = (step: SpendingValidator, index: number) =>
    Object.freeze({
      blueprintTitle: titles[index]!,
      spendingScript: step.spendingScript,
      spendingScriptHash: step.spendingScriptHash,
      spendingScriptAddress: step.spendingScriptAddress,
      referenceOutRef: `${"0".repeat(64)}#0`,
    });
  return [adapt(steps[0], 0), adapt(steps[1], 1), adapt(steps[2], 2)];
};
