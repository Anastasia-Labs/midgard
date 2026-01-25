use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{Input}
use midgard/common/utils
use midgard/fraud_proof_catalogue
use midgard/ledger_state.{HeaderHash}

pub type Datum {
  fraudulent_nodes_prev_header_hash: HeaderHash,
  fraud_prover: ByteArray,
}

pub type MintRedeemer {
  computation_thread_token_asset_name: ByteArray,
  computation_thread_mint_redeemer_index: Int,
}

pub fn asset_name_to_header_hash(
  fraud_proof_asset_name: AssetName,
) -> HeaderHash {
  fraud_proof_asset_name |> bytearray.drop(fraud_proof_catalogue.id_byte_count)
}

pub fn get_proven_fraudulent_blocks_header_hash(
  reference_inputs: List<Input>,
  fraud_proof_policy_id: PolicyId,
  fraud_proof_ref_input_index: Int,
) -> HeaderHash {
  let fraud_proof_asset_name =
    utils.get_authentic_input_asset_name_with_policy_at(
      reference_inputs,
      fraud_proof_policy_id,
      fraud_proof_ref_input_index,
    )
  asset_name_to_header_hash(fraud_proof_asset_name)
}
