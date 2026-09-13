import {
  applyParamsToScript,
  type Data,
  type Network,
  type Script,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

export const UNUSED_REDEEMER_BLUEPRINT_TITLES = Object.freeze([
  "fraud_proofs/unused_redeemer/step_01.main.spend",
  "fraud_proofs/unused_redeemer/step_02.main.spend",
  "fraud_proofs/unused_redeemer/step_02a.main.spend",
  "fraud_proofs/unused_redeemer/step_02b.main.spend",
  "fraud_proofs/unused_redeemer/step_02c.main.spend",
  "fraud_proofs/unused_redeemer/step_03.main.spend",
  "fraud_proofs/unused_redeemer/step_04.main.spend",
  "fraud_proofs/unused_redeemer/step_05.main.spend",
  "fraud_proofs/unused_redeemer/step_06.main.spend",
] as const);
export type UnusedRedeemerStepContract = Readonly<{
  blueprintTitle: string;
  spendingScript: Script;
  spendingScriptHash: string;
  spendingScriptAddress: string;
  referenceOutRef: string;
}>;
export type UnusedRedeemerContracts = Readonly<{
  steps: readonly [
    UnusedRedeemerStepContract,
    UnusedRedeemerStepContract,
    UnusedRedeemerStepContract,
    UnusedRedeemerStepContract,
    UnusedRedeemerStepContract,
    UnusedRedeemerStepContract,
    UnusedRedeemerStepContract,
    UnusedRedeemerStepContract,
    UnusedRedeemerStepContract,
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
}>;
type Blueprint = Readonly<{
  validators: readonly Readonly<{
    title: string;
    compiledCode: string;
    parameters?: readonly unknown[];
  }>[];
}>;
export const applyUnusedRedeemerScripts = ({
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
}): UnusedRedeemerContracts["steps"] => {
  const apply = (index: number, parameters: readonly Data[]) => {
    const blueprintTitle = UNUSED_REDEEMER_BLUEPRINT_TITLES[index]!;
    const validator = blueprint.validators.find(
      ({ title }) => title === blueprintTitle,
    );
    if (validator === undefined)
      throw new Error(`unusedRedeemer: blueprint omitted ${blueprintTitle}`);
    if ((validator.parameters?.length ?? 0) !== parameters.length)
      throw new Error(`unusedRedeemer: ${blueprintTitle} arity changed`);
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
  const step06 = apply(8, [
    computationThreadPolicyId,
    fraudProofPolicyId,
    fraudProofTokenAddressData,
  ]);
  const step05 = apply(7, [
    step06.spendingScriptHash,
    computationThreadPolicyId,
  ]);
  const step04 = apply(6, [
    step05.spendingScriptHash,
    computationThreadPolicyId,
  ]);
  const step03 = apply(5, [
    step04.spendingScriptHash,
    computationThreadPolicyId,
  ]);
  const step02c = apply(4, [
    step03.spendingScriptHash,
    computationThreadPolicyId,
  ]);
  const step02b = apply(3, [
    step02c.spendingScriptHash,
    computationThreadPolicyId,
  ]);
  const step02a = apply(2, [
    step02b.spendingScriptHash,
    computationThreadPolicyId,
  ]);
  const step02 = apply(1, [
    step02a.spendingScriptHash,
    computationThreadPolicyId,
  ]);
  const step01 = apply(0, [
    step02.spendingScriptHash,
    computationThreadPolicyId,
    hubOracleScriptHash,
  ]);
  return [
    step01,
    step02,
    step02a,
    step02b,
    step02c,
    step03,
    step04,
    step05,
    step06,
  ];
};
