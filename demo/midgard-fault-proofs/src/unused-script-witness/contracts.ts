import {
  buildUnusedScriptWitnessChain,
  parseFaultProofBlueprint,
  type SpendingValidator,
} from "@al-ft/midgard-sdk";
import { type Data, type Network, type Script } from "@lucid-evolution/lucid";
import { Effect } from "effect";

export const UNUSED_SCRIPT_WITNESS_BLUEPRINT_TITLES = Object.freeze([
  "fraud_proofs/unused_script_witness/step_01.main.spend",
  "fraud_proofs/unused_script_witness/step_02.main.spend",
  "fraud_proofs/unused_script_witness/step_03.main.spend",
  "fraud_proofs/unused_script_witness/step_04.main.spend",
  "fraud_proofs/unused_script_witness/step_05.main.spend",
  "fraud_proofs/unused_script_witness/step_06.main.spend",
] as const);
export type UnusedScriptWitnessStepContract = Readonly<{
  blueprintTitle: string;
  spendingScript: Script;
  spendingScriptHash: string;
  spendingScriptAddress: string;
  referenceOutRef: string;
}>;
export type UnusedScriptWitnessContracts = Readonly<{
  steps: readonly [
    UnusedScriptWitnessStepContract,
    UnusedScriptWitnessStepContract,
    UnusedScriptWitnessStepContract,
    UnusedScriptWitnessStepContract,
    UnusedScriptWitnessStepContract,
    UnusedScriptWitnessStepContract,
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
export const applyUnusedScriptWitnessScripts = ({
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
}): UnusedScriptWitnessContracts["steps"] => {
  const { steps } = Effect.runSync(
    buildUnusedScriptWitnessChain({
      blueprint: parseFaultProofBlueprint(blueprint),
      network,
      hubOraclePolicyId: hubOracleScriptHash,
      computationThread: { policyId: computationThreadPolicyId },
      fraudProof: { policyId: fraudProofPolicyId },
      fraudProofTokenAddressData,
    }),
  );
  const titles = Object.values(UNUSED_SCRIPT_WITNESS_BLUEPRINT_TITLES);
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
