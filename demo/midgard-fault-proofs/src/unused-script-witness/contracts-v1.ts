import {
  applyParamsToScript,
  type Data,
  type Network,
  type Script,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

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
  const apply = (index: number, parameters: readonly Data[]) => {
    const blueprintTitle = UNUSED_SCRIPT_WITNESS_BLUEPRINT_TITLES[index]!;
    const validator = blueprint.validators.find(
      ({ title }) => title === blueprintTitle,
    );
    if (validator === undefined)
      throw new Error(
        `unusedScriptWitness: blueprint omitted ${blueprintTitle}`,
      );
    if ((validator.parameters?.length ?? 0) !== parameters.length)
      throw new Error(`unusedScriptWitness: ${blueprintTitle} arity changed`);
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
  const step06 = apply(5, [
    computationThreadPolicyId,
    fraudProofPolicyId,
    fraudProofTokenAddressData,
  ]);
  const step05 = apply(4, [
    step06.spendingScriptHash,
    computationThreadPolicyId,
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
  return [step01, step02, step03, step04, step05, step06];
};
