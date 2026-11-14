use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray
use cardano/assets.{AssetName}
use cardano/transaction.{InlineDatum, Input, Output}
use midgard/common/types.{MerkleRoot}
use midgard/common/utils

pub const asset_name: AssetName = "MIDGARD_FRAUD_PROOF_CATALOGUE"

pub const id_byte_count = 4

/// Category id of the validation-trace dispute family. Descendants of a block
/// convicted under it are removed without slashing their operators.
pub const validation_trace_dispute_category_id: ByteArray = #"00000006"

/// Whether an authenticated fraud-proof asset name (category id followed by
/// the header hash) records a validation-trace conviction.
pub fn is_validation_trace_dispute(fraud_proof_asset_name: AssetName) -> Bool {
  bytearray.take(fraud_proof_asset_name, id_byte_count) == validation_trace_dispute_category_id
}

/// Keys are integer indices, each represented with `id_byte_count` number of
/// bytes.
pub type Datum =
  MerkleRoot<Int, ByteArray>

pub fn get_datum(
  reference_inputs: List<Input>,
  catalogue_script_hash: ScriptHash,
  catalogue_ref_input_index: Int,
) -> Datum {
  expect Input { output: Output { datum: InlineDatum(datum_data), .. }, .. } = reference_inputs
    |> utils.get_authentic_input_with_nft_at(
        catalogue_script_hash,
        asset_name,
        catalogue_ref_input_index,
      )
  expect datum: Datum = datum_data
  datum
}
