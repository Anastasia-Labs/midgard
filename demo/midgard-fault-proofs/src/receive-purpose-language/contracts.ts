import {
  applyParamsToScript,
  type Data,
  type Network,
  type Script,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

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
  const apply = (index: number, parameters: readonly Data[]) => {
    const blueprintTitle = RECEIVE_PURPOSE_LANGUAGE_BLUEPRINT_TITLES[index]!;
    const validator = blueprint.validators.find(
      ({ title }) => title === blueprintTitle,
    );
    if (validator === undefined)
      throw new Error(
        `receivePurposeLanguage: blueprint omitted ${blueprintTitle}`,
      );
    if ((validator.parameters?.length ?? 0) !== parameters.length)
      throw new Error(
        `receivePurposeLanguage: ${blueprintTitle} parameter arity changed`,
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
  const step03 = apply(2, [
    computationThreadPolicyId,
    fraudProofPolicyId,
    fraudProofTokenAddressData,
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
  return [step01, step02, step03];
};
