use aiken/crypto.{VerificationKeyHash}
use aiken/primitive/bytearray
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{InlineDatum, Input}
use midgard/common/utils
use midgard/fraud_proof_catalogue
use midgard/ledger_state.{HeaderHash}

pub type Datum {
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

/// Variant used by the state-correction lock. The permanent token's complete
/// asset name is the immutable proof identity: category id || header hash.
/// Keeping it in the lock prevents a proof for the same header but a different
/// category from resuming an in-flight correction.
pub fn get_proven_fraud_record_with_identity(
  reference_inputs: List<Input>,
  fraud_proof_policy_id: PolicyId,
  fraud_proof_ref_input_index: Int,
  return: fn(HeaderHash, VerificationKeyHash, AssetName) -> result,
) -> result {
  let fraud_proof_input, fraud_proof_asset_name <- utils.get_authentic_input_and_asset_name_with_policy_at(
    reference_inputs,
    fraud_proof_policy_id,
    fraud_proof_ref_input_index,
  )
  expect InlineDatum(fraud_proof_datum_data) = fraud_proof_input.output.datum
  expect Datum { fraud_prover }: Datum = fraud_proof_datum_data
  return(
    asset_name_to_header_hash(fraud_proof_asset_name),
    fraud_prover,
    fraud_proof_asset_name,
  )
}
