/**
 * Explicit pre-registration contract record for `l2-tx-mistag`.
 *
 * Blueprint parameter order (apply step 02 first, then step 01):
 *
 * - step_01: `[step_02_validator_script_hash, computation_thread_token_policy_id, hub_oracle]`
 * - step_02: `[fraud_proof_token_policy_id, fraud_proof_token_address, computation_thread_token_policy_id]`
 */
import {
  applyParamsToScript,
  type Data,
  type Network,
  type Script,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

export const L2_TX_MISTAG_CATEGORY_LABEL = "l2-tx-mistag";

export const L2_TX_MISTAG_BLUEPRINT_TITLES_V1 = {
  step01: "fraud_proofs/l2_tx_mistag/step_01.main.spend",
  step02: "fraud_proofs/l2_tx_mistag/step_02.main.spend",
} as const;

export type L2TxMistagStepContractV1 = {
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
};

export type L2TxMistagContractsV1 = {
  readonly steps: readonly [L2TxMistagStepContractV1, L2TxMistagStepContractV1];
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
};

export type L2TxMistagBlueprintV1 = {
  readonly validators: readonly {
    readonly title: string;
    readonly compiledCode: string;
    readonly parameters?: readonly unknown[];
  }[];
};

const applyExact = (
  blueprint: L2TxMistagBlueprintV1,
  title: string,
  params: readonly Data[],
): string => {
  const matches = blueprint.validators.filter((entry) => entry.title === title);
  if (matches.length !== 1) {
    throw new Error(
      `${L2_TX_MISTAG_CATEGORY_LABEL}: blueprint must contain exactly one ${title} validator; found ${matches.length.toString()}.`,
    );
  }
  const validator = matches[0];
  const arity = validator.parameters?.length ?? 0;
  if (arity !== params.length) {
    throw new Error(
      `${L2_TX_MISTAG_CATEGORY_LABEL}: ${title} declares ${arity.toString()} parameter(s), but ${params.length.toString()} were supplied.`,
    );
  }
  return applyParamsToScript(validator.compiledCode, [...params]);
};

const spendingContract = (
  network: Network,
  scriptCbor: string,
): L2TxMistagStepContractV1 => {
  const spendingScript: Script = { type: "PlutusV3", script: scriptCbor };
  return {
    spendingScript,
    spendingScriptHash: validatorToScriptHash(spendingScript),
    spendingScriptAddress: validatorToAddress(network, spendingScript),
  };
};

export const buildL2TxMistagChainV1 = ({
  blueprint,
  network,
  computationThreadPolicyId,
  fraudProofPolicyId,
  fraudProofTokenAddressData,
  hubOraclePolicyId,
}: {
  readonly blueprint: L2TxMistagBlueprintV1;
  readonly network: Network;
  readonly computationThreadPolicyId: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofTokenAddressData: Data;
  readonly hubOraclePolicyId: string;
}): readonly [L2TxMistagStepContractV1, L2TxMistagStepContractV1] => {
  const step02 = spendingContract(
    network,
    applyExact(blueprint, L2_TX_MISTAG_BLUEPRINT_TITLES_V1.step02, [
      fraudProofPolicyId,
      fraudProofTokenAddressData,
      computationThreadPolicyId,
    ]),
  );
  const step01 = spendingContract(
    network,
    applyExact(blueprint, L2_TX_MISTAG_BLUEPRINT_TITLES_V1.step01, [
      step02.spendingScriptHash,
      computationThreadPolicyId,
      hubOraclePolicyId,
    ]),
  );
  return [step01, step02];
};
