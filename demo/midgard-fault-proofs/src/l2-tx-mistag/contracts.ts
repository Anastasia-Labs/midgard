import {
  buildL2TxMistagChain as buildSdkChain,
  parseFaultProofBlueprint,
  type SpendingValidator,
} from "@al-ft/midgard-sdk";
/**
 * Explicit pre-registration contract record for `l2-tx-mistag`.
 *
 * Blueprint parameter order (apply step 02 first, then step 01):
 *
 * - step_01: `[step_02_validator_script_hash, computation_thread_token_policy_id, hub_oracle]`
 * - step_02: `[fraud_proof_token_policy_id, fraud_proof_token_address, computation_thread_token_policy_id]`
 */
import { type Data, type Network, type Script } from "@lucid-evolution/lucid";
import { Effect } from "effect";

export const L2_TX_MISTAG_CATEGORY_LABEL = "l2-tx-mistag";

export const L2_TX_MISTAG_BLUEPRINT_TITLES = {
  step01: "fraud_proofs/l2_tx_mistag/step_01.main.spend",
  step02: "fraud_proofs/l2_tx_mistag/step_02.main.spend",
} as const;

export type L2TxMistagStepContract = {
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
};

export type L2TxMistagContracts = {
  readonly steps: readonly [L2TxMistagStepContract, L2TxMistagStepContract];
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

export type L2TxMistagBlueprint = unknown;

export const buildL2TxMistagChain = ({
  blueprint,
  network,
  computationThreadPolicyId,
  fraudProofPolicyId,
  fraudProofTokenAddressData,
  hubOraclePolicyId,
}: {
  readonly blueprint: L2TxMistagBlueprint;
  readonly network: Network;
  readonly computationThreadPolicyId: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofTokenAddressData: Data;
  readonly hubOraclePolicyId: string;
}): readonly [L2TxMistagStepContract, L2TxMistagStepContract] => {
  const { steps } = Effect.runSync(
    buildSdkChain({
      blueprint: parseFaultProofBlueprint(blueprint),
      network,
      hubOraclePolicyId,
      computationThread: { policyId: computationThreadPolicyId },
      fraudProof: { policyId: fraudProofPolicyId },
      fraudProofTokenAddressData,
    }),
  );
  const adapt = (step: SpendingValidator): L2TxMistagStepContract => ({
    spendingScript: step.spendingScript,
    spendingScriptHash: step.spendingScriptHash,
    spendingScriptAddress: step.spendingScriptAddress,
  });
  return [adapt(steps[0]), adapt(steps[1])];
};
