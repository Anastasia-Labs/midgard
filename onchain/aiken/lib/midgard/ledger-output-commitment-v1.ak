use aiken/builtin
use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/bounded_item_v1
use midgard/cek_data_v1
use midgard/fraud_proofs/native_tx/codec.{
  encode_definite_array_header, encode_definite_bytes,
}
use midgard/ledger_output_v1
use midgard/validation_merkle_v1

pub const version: Int = 1

pub const output_field_index: Int = 2

pub const max_cardano_value_cbor_bytes: Int = 5000

const uint64_max = 18446744073709551615

const asset_leaf_domain =
  #"4d6964676172644c65646765724f757470757441737365744c6561665631"

/// Compact ledger MPF value. Complete output bytes remain in DA and
/// persistence; this descriptor binds them to the independently foldable
/// facts consumed by authorization, value, script-source, and CEK proofs.
pub type LedgerOutputCommitmentV1 {
  version: Int,
  output_index: Int,
  total_length: Int,
  item_commitment: ByteArray,
  address: ByteArray,
  lovelace: Int,
  asset_count: Int,
  asset_frontier_commitment: ByteArray,
  cardano_value_size: Int,
  reference_script_language: Int,
  reference_script_hash: ByteArray,
  reference_script_total_length: Int,
  reference_script_item_commitment: ByteArray,
  cardano_tx_out: cek_data_v1.DataSummaryV1,
  midgard_tx_out: cek_data_v1.DataSummaryV1,
  cardano_spend_datum: cek_data_v1.DataSummaryV1,
}

fn uint64_is_valid(value: Int) -> Bool {
  value >= 0 && value <= uint64_max
}

fn summary_is_well_formed(summary: cek_data_v1.DataSummaryV1) -> Bool {
  and {
    bytearray.length(summary.root) == 32,
    uint64_is_valid(summary.cbor_length),
    uint64_is_valid(summary.memory),
  }
}

fn encode_summary(summary: cek_data_v1.DataSummaryV1) -> ByteArray {
  expect summary_is_well_formed(summary)
  encode_definite_array_header(3)
    |> bytearray.concat(encode_definite_bytes(summary.root))
    |> bytearray.concat(cbor.serialise(summary.cbor_length))
    |> bytearray.concat(cbor.serialise(summary.memory))
}

fn summary_from_data(data: Data) -> cek_data_v1.DataSummaryV1 {
  expect [root_data, cbor_length_data, memory_data] = builtin.un_list_data(data)
  let summary = cek_data_v1.DataSummaryV1 {
    root: builtin.un_b_data(root_data),
    cbor_length: builtin.un_i_data(cbor_length_data),
    memory: builtin.un_i_data(memory_data),
  }
  expect summary_is_well_formed(summary)
  summary
}

fn reference_script_language_is_valid(language: Int) -> Bool {
  or {
    language == -1,
    language == 0,
    language == 3,
    language == 128,
  }
}

fn reference_script_is_well_formed(descriptor: LedgerOutputCommitmentV1) -> Bool {
  if descriptor.reference_script_language == -1 {
    and {
      descriptor.reference_script_hash == #"",
      descriptor.reference_script_total_length == 0,
      descriptor.reference_script_item_commitment == #"",
    }
  } else {
    and {
      or {
        descriptor.reference_script_language == 0,
        descriptor.reference_script_language == 3,
        descriptor.reference_script_language == 128,
      },
      bytearray.length(descriptor.reference_script_hash) == 28,
      descriptor.reference_script_total_length > 0,
      bytearray.length(descriptor.reference_script_item_commitment) == 32,
    }
  }
}

fn descriptor_is_well_formed(descriptor: LedgerOutputCommitmentV1) -> Bool {
  and {
    descriptor.version == version,
    descriptor.output_index >= 0,
    descriptor.output_index <= 65535,
    descriptor.total_length >= 0,
    bytearray.length(descriptor.item_commitment) == 32,
    ledger_output_v1.decode_canonical_address_bytes(descriptor.address) != None,
    uint64_is_valid(descriptor.lovelace),
    descriptor.asset_count >= 0,
    descriptor.asset_count <= validation_merkle_v1.maximum_leaf_count,
    bytearray.length(descriptor.asset_frontier_commitment) == 32,
    descriptor.cardano_value_size >= 0,
    descriptor.cardano_value_size <= max_cardano_value_cbor_bytes,
    reference_script_language_is_valid(descriptor.reference_script_language),
    reference_script_is_well_formed(descriptor),
    summary_is_well_formed(descriptor.cardano_tx_out),
    summary_is_well_formed(descriptor.midgard_tx_out),
    summary_is_well_formed(descriptor.cardano_spend_datum),
  }
}

pub fn encode(descriptor: LedgerOutputCommitmentV1) -> ByteArray {
  expect descriptor_is_well_formed(descriptor)
  encode_definite_array_header(16)
    |> bytearray.concat(cbor.serialise(version))
    |> bytearray.concat(cbor.serialise(descriptor.output_index))
    |> bytearray.concat(cbor.serialise(descriptor.total_length))
    |> bytearray.concat(encode_definite_bytes(descriptor.item_commitment))
    |> bytearray.concat(encode_definite_bytes(descriptor.address))
    |> bytearray.concat(cbor.serialise(descriptor.lovelace))
    |> bytearray.concat(cbor.serialise(descriptor.asset_count))
    |> bytearray.concat(
        encode_definite_bytes(descriptor.asset_frontier_commitment),
      )
    |> bytearray.concat(cbor.serialise(descriptor.cardano_value_size))
    |> bytearray.concat(cbor.serialise(descriptor.reference_script_language))
    |> bytearray.concat(encode_definite_bytes(descriptor.reference_script_hash))
    |> bytearray.concat(
        cbor.serialise(descriptor.reference_script_total_length),
      )
    |> bytearray.concat(
        encode_definite_bytes(descriptor.reference_script_item_commitment),
      )
    |> bytearray.concat(encode_summary(descriptor.cardano_tx_out))
    |> bytearray.concat(encode_summary(descriptor.midgard_tx_out))
    |> bytearray.concat(encode_summary(descriptor.cardano_spend_datum))
}

pub fn decode(descriptor_cbor: ByteArray) -> LedgerOutputCommitmentV1 {
  expect Some(data) = cbor.deserialise(descriptor_cbor)
  expect
      [
        version_data, output_index_data, total_length_data, item_commitment_data,
        address_data, lovelace_data, asset_count_data,
        asset_frontier_commitment_data, cardano_value_size_data,
        reference_script_language_data, reference_script_hash_data,
        reference_script_total_length_data,
        reference_script_item_commitment_data, cardano_tx_out_data,
        midgard_tx_out_data, cardano_spend_datum_data,
      ]
    = builtin.un_list_data(data)
  let descriptor = LedgerOutputCommitmentV1 {
    version: builtin.un_i_data(version_data),
    output_index: builtin.un_i_data(output_index_data),
    total_length: builtin.un_i_data(total_length_data),
    item_commitment: builtin.un_b_data(item_commitment_data),
    address: builtin.un_b_data(address_data),
    lovelace: builtin.un_i_data(lovelace_data),
    asset_count: builtin.un_i_data(asset_count_data),
    asset_frontier_commitment: builtin.un_b_data(asset_frontier_commitment_data),
    cardano_value_size: builtin.un_i_data(cardano_value_size_data),
    reference_script_language: builtin.un_i_data(reference_script_language_data),
    reference_script_hash: builtin.un_b_data(reference_script_hash_data),
    reference_script_total_length: builtin.un_i_data(
      reference_script_total_length_data,
    ),
    reference_script_item_commitment: builtin.un_b_data(
      reference_script_item_commitment_data,
    ),
    cardano_tx_out: summary_from_data(cardano_tx_out_data),
    midgard_tx_out: summary_from_data(midgard_tx_out_data),
    cardano_spend_datum: summary_from_data(cardano_spend_datum_data),
  }
  expect descriptor_is_well_formed(descriptor)
  expect encode(descriptor) == descriptor_cbor
  descriptor
}

pub fn output_item_commitment(
  output_index: Int,
  output_cbor: ByteArray,
) -> ByteArray {
  bounded_item_v1.from_bytes(output_field_index, output_index, output_cbor)
}

pub fn asset_leaf_hash(
  policy_id: ByteArray,
  asset_name: ByteArray,
  quantity: Int,
) -> ByteArray {
  expect bytearray.length(policy_id) == 28
  expect bytearray.length(asset_name) <= 32
  expect quantity > 0
  asset_leaf_domain
    |> bytearray.concat(
        encode_definite_array_header(3)
          |> bytearray.concat(encode_definite_bytes(policy_id))
          |> bytearray.concat(encode_definite_bytes(asset_name))
          |> bytearray.concat(cbor.serialise(quantity)),
      )
    |> blake2b_256
}

pub fn verify_asset_membership(
  descriptor: LedgerOutputCommitmentV1,
  asset_index: Int,
  policy_id: ByteArray,
  asset_name: ByteArray,
  quantity: Int,
  peaks: List<validation_merkle_v1.FrontierPeak>,
  siblings: List<ByteArray>,
) -> Bool {
  and {
    descriptor_is_well_formed(descriptor),
    validation_merkle_v1.frontier_is_well_formed(descriptor.asset_count, peaks),
    validation_merkle_v1.frontier_commitment(descriptor.asset_count, peaks) == descriptor.asset_frontier_commitment,
    validation_merkle_v1.verify_membership(
      descriptor.asset_count,
      peaks,
      asset_index,
      asset_leaf_hash(policy_id, asset_name, quantity),
      siblings,
    ),
  }
}

pub fn verify_chunk(
  descriptor: LedgerOutputCommitmentV1,
  proof: bounded_item_v1.ChunkProofV1,
) -> Bool {
  and {
    descriptor_is_well_formed(descriptor),
    proof.field_index == output_field_index,
    proof.item_index == descriptor.output_index,
    proof.total_length == descriptor.total_length,
    bounded_item_v1.verify_chunk(descriptor.item_commitment, proof),
  }
}

pub fn verify_reference_script_chunk(
  descriptor: LedgerOutputCommitmentV1,
  proof: bounded_item_v1.ChunkProofV1,
) -> Bool {
  and {
    descriptor_is_well_formed(descriptor),
    descriptor.reference_script_language != -1,
    proof.field_index == output_field_index,
    proof.item_index == descriptor.output_index,
    proof.total_length == descriptor.reference_script_total_length,
    bounded_item_v1.verify_chunk(
      descriptor.reference_script_item_commitment,
      proof,
    ),
  }
}
