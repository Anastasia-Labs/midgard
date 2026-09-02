import {
  applyParamsToScript,
  type Data,
  type Network,
  type Script,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

export const ZERO_INPUT_BLUEPRINT_TITLES_V1 = Object.freeze([
  "fraud_proofs/zero_input/step_01.main.spend",
  "fraud_proofs/zero_input/step_02.main.spend",
] as const);

export type ZeroInputStepContractV1 = Readonly<{
  blueprintTitle: string;
  spendingScript: Script;
  spendingScriptHash: string;
  spendingScriptAddress: string;
  referenceOutRef: string;
}>;

export type ZeroInputContractsV1 = Readonly<{
  steps: readonly [ZeroInputStepContractV1, ZeroInputStepContractV1];
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
  /** Reserved for certificate publication; terminal submission only reads the policy id. */
  fieldPreimageCertificateMintingScript?: Script;
}>;

type Blueprint = Readonly<{
  validators: readonly Readonly<{
    title: string;
    compiledCode: string;
    parameters?: readonly unknown[];
  }>[];
}>;

const applyExact = (
  blueprint: Blueprint,
  title: string,
  parameters: readonly Data[],
): Script => {
  const validator = blueprint.validators.find((entry) => entry.title === title);
  if (validator === undefined)
    throw new Error(`zeroInput: blueprint omitted ${title}`);
  if ((validator.parameters?.length ?? 0) !== parameters.length)
    throw new Error(`zeroInput: ${title} parameter arity changed`);
  return {
    type: "PlutusV3",
    script: applyParamsToScript(validator.compiledCode, [...parameters]),
  };
};

export const applyZeroInputScriptsV1 = ({
  blueprint,
  network,
  computationThreadPolicyId,
  fraudProofPolicyId,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
  hubOracleScriptHash,
}: {
  readonly blueprint: Blueprint;
  readonly network: Network;
  readonly computationThreadPolicyId: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofTokenAddressData: Data;
  readonly fieldPreimageCertificatePolicyId: string;
  readonly hubOracleScriptHash: string;
}): ZeroInputContractsV1["steps"] => {
  const applied = (index: number, parameters: readonly Data[]) => {
    const blueprintTitle = ZERO_INPUT_BLUEPRINT_TITLES_V1[index]!;
    const spendingScript = applyExact(blueprint, blueprintTitle, parameters);
    return Object.freeze({
      blueprintTitle,
      spendingScript,
      spendingScriptHash: validatorToScriptHash(spendingScript),
      spendingScriptAddress: validatorToAddress(network, spendingScript),
      referenceOutRef: `${"0".repeat(64)}#0`,
    });
  };
  const step02 = applied(1, [
    fraudProofPolicyId,
    fraudProofTokenAddressData,
    computationThreadPolicyId,
    fieldPreimageCertificatePolicyId,
  ]);
  const step01 = applied(0, [
    step02.spendingScriptHash,
    computationThreadPolicyId,
    hubOracleScriptHash,
  ]);
  return [step01, step02];
};
