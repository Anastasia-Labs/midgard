import {
  applyParamsToScript,
  type Data,
  type Network,
  type Script,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

export const WITNESS_SCRIPT_DECODING_CATEGORY_LABEL = "witness-script-decoding";

export const WITNESS_SCRIPT_DECODING_BLUEPRINT_TITLES = {
  step01: "fraud_proofs/witness_script_decoding/step_01.main.spend",
  step02: "fraud_proofs/witness_script_decoding/step_02.main.spend",
  step03: "fraud_proofs/witness_script_decoding/step_03.main.spend",
  step04: "fraud_proofs/witness_script_decoding/step_04.main.spend",
} as const;

export type WitnessScriptDecodingStepContract = {
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
};

/** Manifest-bound contracts consumed by every concrete family submitter. */
export type WitnessScriptDecodingContracts = {
  readonly steps: readonly [
    WitnessScriptDecodingStepContract,
    WitnessScriptDecodingStepContract,
    WitnessScriptDecodingStepContract,
    WitnessScriptDecodingStepContract,
  ];
  readonly computationThread: {
    readonly policyId: string;
    readonly mintingScript: Script;
  };
  readonly fraudProof: {
    readonly policyId: string;
    readonly mintingScript: Script;
    readonly spendingScriptAddress: string;
  };
  readonly hubOraclePolicyId: string;
  readonly stateQueuePolicyId: string;
  readonly fieldPreimageCertificatePolicyId: string;
  readonly fieldPreimageCertificateMintingScript: Script;
};

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
    throw new Error(`witnessScriptDecoding: blueprint omitted ${title}`);
  if ((validator.parameters?.length ?? 0) !== parameters.length)
    throw new Error(`witnessScriptDecoding: ${title} parameter arity changed`);
  return {
    type: "PlutusV3",
    script: applyParamsToScript(validator.compiledCode, [...parameters]),
  };
};

/**
 * Family-side application of the four physical validators in deployment
 * order (step 04 first, since each earlier step is parameterised on its
 * successor's hash). A lifecycle suite asserts this reproduces the registered
 * SDK chain step for step before driving the registered chain.
 */
export const applyWitnessScriptDecodingScripts = ({
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
}): WitnessScriptDecodingContracts["steps"] => {
  const applied = (
    blueprintTitle: string,
    parameters: readonly Data[],
  ): WitnessScriptDecodingStepContract => {
    const spendingScript = applyExact(blueprint, blueprintTitle, parameters);
    return Object.freeze({
      spendingScript,
      spendingScriptHash: validatorToScriptHash(spendingScript),
      spendingScriptAddress: validatorToAddress(network, spendingScript),
    });
  };
  const step04 = applied(WITNESS_SCRIPT_DECODING_BLUEPRINT_TITLES.step04, [
    computationThreadPolicyId,
    fraudProofPolicyId,
    fraudProofTokenAddressData,
  ]);
  const step03 = applied(WITNESS_SCRIPT_DECODING_BLUEPRINT_TITLES.step03, [
    step04.spendingScriptHash,
    computationThreadPolicyId,
  ]);
  const step02 = applied(WITNESS_SCRIPT_DECODING_BLUEPRINT_TITLES.step02, [
    step03.spendingScriptHash,
    computationThreadPolicyId,
    fieldPreimageCertificatePolicyId,
  ]);
  const step01 = applied(WITNESS_SCRIPT_DECODING_BLUEPRINT_TITLES.step01, [
    step02.spendingScriptHash,
    computationThreadPolicyId,
    hubOracleScriptHash,
  ]);
  return [step01, step02, step03, step04];
};
