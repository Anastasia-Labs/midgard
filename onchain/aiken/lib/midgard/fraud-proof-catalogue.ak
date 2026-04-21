use aiken/crypto.{ScriptHash}
use cardano/assets.{AssetName}
use cardano/transaction.{InlineDatum, Input, Output}
use midgard/common/types.{MerkleRoot}
use midgard/common/utils

pub const asset_name: AssetName = "MIDGARD_FRAUD_PROOF_CATALOGUE"

pub const id_byte_count = 4

/// Keys are integer indices, each represented with `id_byte_count` number of
/// bytes.
pub type Datum =
  MerkleRoot<Int, ByteArray>

pub fn get_datum(
  reference_inputs: List<Input>,
  catalogue_script_hash: ScriptHash,
  catalogue_ref_input_index: Int,
) -> Datum {
  expect Input { output: Output { datum: InlineDatum(datum_data), .. }, .. } =
    reference_inputs
      |> utils.get_authentic_input_with_nft_at(
          catalogue_script_hash,
          asset_name,
          catalogue_ref_input_index,
        )
  expect datum: Datum = datum_data
  datum
}
