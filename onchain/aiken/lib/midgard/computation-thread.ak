use aiken/crypto.{VerificationKeyHash}
use aiken/primitive/bytearray
use cardano/assets.{AssetName}

pub const fraud_proof_id_byte_count = 4

pub const hub_oracle_fraud_proof_asset_name_prefix: AssetName = "FraudProof"

pub const hub_oracle_first_fraud_proof_step_id: ByteArray = #"00000000"

pub fn hub_oracle_fraud_proof_step_asset_name(
  fraud_proof_id: ByteArray,
  fraud_proof_step_id: ByteArray,
) -> AssetName {
  expect bytearray.length(fraud_proof_id) == fraud_proof_id_byte_count
  expect bytearray.length(fraud_proof_step_id) == fraud_proof_id_byte_count
  hub_oracle_fraud_proof_asset_name_prefix
    |> bytearray.concat(fraud_proof_id)
    |> bytearray.concat(fraud_proof_step_id)
}

pub fn hub_oracle_first_fraud_proof_step_asset_name(
  fraud_proof_id: ByteArray,
) -> AssetName {
  hub_oracle_fraud_proof_step_asset_name(
    fraud_proof_id,
    hub_oracle_first_fraud_proof_step_id,
  )
}

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
    fraud_proof_id: ByteArray,
    fraud_proof_step_hub_oracle_ref_index: Int,
    state_queue_mint_hub_oracle_ref_index: Int,
    fraudulent_block_ref_input_index: Int,
  }
  Success { burning_token_asset_name: ByteArray }
  BurnForCancellation { burning_token_asset_name: ByteArray }
}
