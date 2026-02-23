use aiken/crypto.{ScriptHash, VerificationKeyHash}
use midgard/common/types.{Proof}

pub type StepDatum<a> {
  fraud_prover: VerificationKeyHash,
  data: Option<a>,
}

pub type StepRedeemer<a> {
  Cancel { input_index: Int, computation_thread_mint_redeemer_index: Int }
  Continue(a)
}

pub type MintRedeemer {
  Init {
    first_step_output_index: Int,
    fraud_category_id: ByteArray,
    fraud_category: ScriptHash,
    fraud_category_membership_proof: Proof,
    fraud_proof_catalogue_ref_input_index: Int,
    inclusion_proof_script_redeemer_index: Int,
    hub_oracle_ref_input_index: Int,
    fraudulent_block_ref_input_index: Int,
  }
  Success { burning_token_asset_name: ByteArray }
  BurnForCancellation { burning_token_asset_name: ByteArray }
}
