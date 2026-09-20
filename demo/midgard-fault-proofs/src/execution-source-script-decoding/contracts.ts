import {
  buildExecutionSourceScriptDecodingChain,
  parseFaultProofBlueprint,
  type SpendingValidator,
} from "@al-ft/midgard-sdk";
import { type Data, type Network, type Script } from "@lucid-evolution/lucid";
import { Effect } from "effect";

export const EXECUTION_SOURCE_SCRIPT_DECODING_BLUEPRINT_TITLES = Object.freeze([
  "fraud_proofs/execution_source_script_decoding/step_01.main.spend",
  "fraud_proofs/execution_source_script_decoding/step_02.main.spend",
  "fraud_proofs/execution_source_script_decoding/step_03.main.spend",
  "fraud_proofs/execution_source_script_decoding/step_04.main.spend",
  "fraud_proofs/execution_source_script_decoding/step_05.main.spend",
] as const);

export type ExecutionSourceScriptDecodingStepContract = Readonly<{
  blueprintTitle: string;
  spendingScript: Script;
  spendingScriptHash: string;
  spendingScriptAddress: string;
  referenceOutRef: string;
}>;
export type ExecutionSourceScriptDecodingContracts = Readonly<{
  steps: readonly [
    ExecutionSourceScriptDecodingStepContract,
    ExecutionSourceScriptDecodingStepContract,
    ExecutionSourceScriptDecodingStepContract,
    ExecutionSourceScriptDecodingStepContract,
    ExecutionSourceScriptDecodingStepContract,
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
  /** The accepted door (`submitMissingNativeScriptTxBinding`) reads the state-queue block by this policy. */
  stateQueuePolicyId: string;
}>;
type Blueprint = Readonly<{
  validators: readonly Readonly<{
    title: string;
    compiledCode: string;
    parameters?: readonly unknown[];
  }>[];
}>;

export const applyExecutionSourceScriptDecodingScripts = ({
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
}): ExecutionSourceScriptDecodingContracts["steps"] => {
  const { steps } = Effect.runSync(
    buildExecutionSourceScriptDecodingChain({
      blueprint: parseFaultProofBlueprint(blueprint),
      network,
      hubOraclePolicyId: hubOracleScriptHash,
      computationThread: { policyId: computationThreadPolicyId },
      fraudProof: { policyId: fraudProofPolicyId },
      fraudProofTokenAddressData,
    }),
  );
  const titles = Object.values(
    EXECUTION_SOURCE_SCRIPT_DECODING_BLUEPRINT_TITLES,
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
