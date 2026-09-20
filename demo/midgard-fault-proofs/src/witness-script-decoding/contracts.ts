import {
  buildWitnessScriptDecodingChain,
  parseFaultProofBlueprint,
  type SpendingValidator,
} from "@al-ft/midgard-sdk";
import { type Data, type Network, type Script } from "@lucid-evolution/lucid";
import { Effect } from "effect";

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

/**
 * Submission adapter for the SDK's four physical validators in deployment order.
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
  const { steps } = Effect.runSync(
    buildWitnessScriptDecodingChain({
      blueprint: parseFaultProofBlueprint(blueprint),
      network,
      hubOraclePolicyId: hubOracleScriptHash,
      computationThread: { policyId: computationThreadPolicyId },
      fraudProof: { policyId: fraudProofPolicyId },
      fraudProofTokenAddressData,
      fieldPreimageCertificatePolicyId,
    }),
  );
  const adapt = (step: SpendingValidator) =>
    Object.freeze({
      spendingScript: step.spendingScript,
      spendingScriptHash: step.spendingScriptHash,
      spendingScriptAddress: step.spendingScriptAddress,
    });
  return [adapt(steps[0]), adapt(steps[1]), adapt(steps[2]), adapt(steps[3])];
};
