import {
  applyParamsToScript,
  type Data,
  type Network,
  type Script,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

export const SCRIPT_INTEGRITY_HASH_MISMATCH_BLUEPRINT_TITLES = Object.freeze([
  "fraud_proofs/script_integrity_hash_mismatch/step_01.main.spend",
  "fraud_proofs/script_integrity_hash_mismatch/step_02.main.spend",
  "fraud_proofs/script_integrity_hash_mismatch/step_03.main.spend",
  "fraud_proofs/script_integrity_hash_mismatch/step_04.main.spend",
  "fraud_proofs/script_integrity_hash_mismatch/step_05.main.spend",
] as const);

export type ScriptIntegrityHashMismatchStepContract = Readonly<{
  blueprintTitle: string;
  spendingScript: Script;
  spendingScriptHash: string;
  spendingScriptAddress: string;
  referenceOutRef: string;
}>;
export type ScriptIntegrityHashMismatchContracts = Readonly<{
  steps: readonly [
    ScriptIntegrityHashMismatchStepContract,
    ScriptIntegrityHashMismatchStepContract,
    ScriptIntegrityHashMismatchStepContract,
    ScriptIntegrityHashMismatchStepContract,
    ScriptIntegrityHashMismatchStepContract,
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

export const applyScriptIntegrityHashMismatchScripts = ({
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
}): ScriptIntegrityHashMismatchContracts["steps"] => {
  const apply = (index: number, parameters: readonly Data[]) => {
    const blueprintTitle =
      SCRIPT_INTEGRITY_HASH_MISMATCH_BLUEPRINT_TITLES[index]!;
    const validator = blueprint.validators.find(
      ({ title }) => title === blueprintTitle,
    );
    if (validator === undefined)
      throw new Error(
        `scriptIntegrityHashMismatch: blueprint omitted ${blueprintTitle}`,
      );
    if ((validator.parameters?.length ?? 0) !== parameters.length)
      throw new Error(
        `scriptIntegrityHashMismatch: ${blueprintTitle} parameter arity changed`,
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
