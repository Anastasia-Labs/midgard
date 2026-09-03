import {
  applyParamsToScript,
  type Data,
  type Network,
  type Script,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

export const REDEEMER_CANONICITY_BLUEPRINT_TITLES = Object.freeze([
  "fraud_proofs/redeemer_canonicity/step_01.main.spend",
  "fraud_proofs/redeemer_canonicity/step_02.main.spend",
  "fraud_proofs/redeemer_canonicity/step_03.main.spend",
] as const);

export type RedeemerCanonicityStepContract = Readonly<{
  blueprintTitle: string;
  spendingScript: Script;
  spendingScriptHash: string;
  spendingScriptAddress: string;
  referenceOutRef: string;
}>;

export type RedeemerCanonicityContracts = Readonly<{
  steps: readonly [
    RedeemerCanonicityStepContract,
    RedeemerCanonicityStepContract,
    RedeemerCanonicityStepContract,
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
    throw new Error(`redeemerCanonicity: blueprint omitted ${title}`);
  if ((validator.parameters?.length ?? 0) !== parameters.length)
    throw new Error(`redeemerCanonicity: ${title} parameter arity changed`);
  return {
    type: "PlutusV3",
    script: applyParamsToScript(validator.compiledCode, [...parameters]),
  };
};

export const applyRedeemerCanonicityScripts = ({
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
}): RedeemerCanonicityContracts["steps"] => {
  const applied = (
    index: number,
    parameters: readonly Data[],
  ): RedeemerCanonicityStepContract => {
    const blueprintTitle = REDEEMER_CANONICITY_BLUEPRINT_TITLES[index]!;
    const spendingScript = applyExact(blueprint, blueprintTitle, parameters);
    return Object.freeze({
      blueprintTitle,
      spendingScript,
      spendingScriptHash: validatorToScriptHash(spendingScript),
      spendingScriptAddress: validatorToAddress(network, spendingScript),
      referenceOutRef: `${"0".repeat(64)}#0`,
    });
  };
  const step03 = applied(2, [
    fraudProofPolicyId,
    fraudProofTokenAddressData,
    computationThreadPolicyId,
  ]);
  const step02 = applied(1, [
    step03.spendingScriptHash,
    computationThreadPolicyId,
    fieldPreimageCertificatePolicyId,
  ]);
  const step01 = applied(0, [
    step02.spendingScriptHash,
    computationThreadPolicyId,
    hubOracleScriptHash,
  ]);
  return [step01, step02, step03];
};
