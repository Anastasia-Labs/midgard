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

/// Reads the authentic fraud-proof reference input and yields both halves of
/// the terminal fraud record it carries: the fraudulent block's header hash
/// (the asset-name suffix) and the fraud prover recorded in its datum.
///
/// Both halves are equally trustworthy. The asset name is authenticated by the
/// fraud-proof policy id exactly as before, and the datum is pinned to the
/// computation thread's `fraud_prover` by
/// [`fraud_proofs/common.finalize`](./fraud-proofs/common.html#finalize) at the
/// terminal step — which in turn carries the identity `computation_thread.Init`
/// authenticated with the prover's own signature. The fraud-proof spending
/// validator never permits the token to be spent, so the pairing is immutable.
///
/// Supersedes the header-hash-only reader: every caller needs the prover too,
/// because F04 §2.3 routes `fraud_prover_reward` to exactly this key hash.
pub fn get_proven_fraud_record(
  reference_inputs: List<Input>,
  fraud_proof_policy_id: PolicyId,
  fraud_proof_ref_input_index: Int,
  return: fn(HeaderHash, VerificationKeyHash) -> result,
) -> result {
  get_proven_fraud_record_with_identity(
    reference_inputs,
    fraud_proof_policy_id,
    fraud_proof_ref_input_index,
    fn(header_hash, fraud_prover, _fraud_proof_asset_name) {
      return(header_hash, fraud_prover)
    },
  )
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
