import {
  applyParamsToScript,
  type Data,
  type Network,
  type Script,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

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
  const apply = (index: number, parameters: readonly Data[]) => {
    const blueprintTitle =
      EXECUTION_SOURCE_SCRIPT_DECODING_BLUEPRINT_TITLES[index]!;
    const validator = blueprint.validators.find(
      ({ title }) => title === blueprintTitle,
    );
    if (validator === undefined)
      throw new Error(
        `executionSourceScriptDecoding: blueprint omitted ${blueprintTitle}`,
      );
    if ((validator.parameters?.length ?? 0) !== parameters.length)
      throw new Error(
        `executionSourceScriptDecoding: ${blueprintTitle} parameter arity changed`,
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
  const step05 = apply(4, [
    computationThreadPolicyId,
    fraudProofPolicyId,
    fraudProofTokenAddressData,
  ]);
  const step04 = apply(3, [
    step05.spendingScriptHash,
    computationThreadPolicyId,
  ]);
  const step03 = apply(2, [
    step04.spendingScriptHash,
    computationThreadPolicyId,
  ]);
  const step02 = apply(1, [
    step03.spendingScriptHash,
    computationThreadPolicyId,
  ]);
  const step01 = apply(0, [
    step02.spendingScriptHash,
    computationThreadPolicyId,
    hubOracleScriptHash,
  ]);
  return [step01, step02, step03, step04, step05];
};
