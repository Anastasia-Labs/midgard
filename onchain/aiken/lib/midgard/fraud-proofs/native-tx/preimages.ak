use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/codec.{
  decode_definite_array_header_at, decode_definite_bytes_at,
  encode_definite_array_header, encode_definite_bytes,
  encode_definite_map_header, encode_native_byte_list, expect_deserialise,
}
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_address_witness_cbor, decode_midgard_redeemer_witness_at,
  decode_midgard_tx_input_cbor, decode_midgard_tx_output_cbor,
  decode_midgard_versioned_script_at, encode_midgard_address_witness,
  encode_midgard_redeemer_witness, encode_midgard_tx_input,
  encode_midgard_tx_output, encode_midgard_versioned_script,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddressWitness, MidgardRedeemerWitness, MidgardScriptWitness,
  MidgardTxInput, MidgardTxOutput, MidgardVersionedScript,
}

pub fn encode_input_preimage(inputs: List<MidgardTxInput>) -> ByteArray {
  inputs |> list.map(encode_midgard_tx_input) |> encode_native_byte_list
}

pub fn encode_output_preimage(outputs: List<MidgardTxOutput>) -> ByteArray {
  outputs |> list.map(encode_midgard_tx_output) |> encode_native_byte_list
}

pub fn encode_address_witness_preimage(
  witnesses: List<MidgardAddressWitness>,
) -> ByteArray {
  witnesses
    |> list.map(encode_midgard_address_witness)
    |> encode_native_byte_list
}

pub fn encode_script_witness_preimage(
  witnesses: List<MidgardScriptWitness>,
) -> ByteArray {
  bytearray.concat(
    encode_definite_array_header(list.length(witnesses)),
    list.foldr(
      witnesses,
      #"",
      fn(witness, encoded_tail) {
        let script: MidgardVersionedScript = witness
        bytearray.concat(encode_midgard_versioned_script(script), encoded_tail)
      },
    ),
  )
}

pub fn encode_redeemer_witness_preimage(
  witnesses: List<MidgardRedeemerWitness>,
) -> ByteArray {
  bytearray.concat(
    encode_definite_array_header(list.length(witnesses)),
    list.foldr(
      witnesses,
      #"",
      fn(witness, encoded_tail) {
        bytearray.concat(encode_midgard_redeemer_witness(witness), encoded_tail)
      },
    ),
  )
}

pub fn decode_midgard_tx_byte_list_preimage_cbor(
  preimage_cbor: ByteArray,
) -> List<ByteArray> {
  let Pair(offset, item_count) =
    decode_definite_array_header_at(preimage_cbor, 0)
  let Pair(offset, items) =
    decode_byte_items_at(preimage_cbor, offset, item_count)
  expect offset == bytearray.length(preimage_cbor)
  items
}

pub fn decode_midgard_tx_data_list_preimage_cbor(
  preimage_cbor: ByteArray,
) -> List<Data> {
  expect_deserialise(preimage_cbor) |> builtin.un_list_data
}

fn decode_byte_data_list(items: List<Data>) -> List<ByteArray> {
  when items is {
    [] -> []
    [item, ..rest] -> [builtin.un_b_data(item), ..decode_byte_data_list(rest)]
  }
}

fn decode_byte_items_at(
  preimage_cbor: ByteArray,
  offset: Int,
  remaining: Int,
) -> Pair<Int, List<ByteArray>> {
  if remaining <= 0 {
    Pair(offset, [])
  } else {
    let Pair(offset, item_cbor) =
      decode_definite_bytes_at(preimage_cbor, offset)
    let Pair(offset, rest) =
      decode_byte_items_at(preimage_cbor, offset, remaining - 1)
    Pair(offset, [item_cbor, ..rest])
  }
}

fn decode_input_cbor_items_at(
  preimage_cbor: ByteArray,
  offset: Int,
  remaining: Int,
) -> Pair<Int, List<MidgardTxInput>> {
  if remaining <= 0 {
    Pair(offset, [])
  } else {
    let Pair(offset, input_cbor) =
      decode_definite_bytes_at(preimage_cbor, offset)
    let input = decode_midgard_tx_input_cbor(input_cbor)
    let Pair(offset, rest) =
      decode_input_cbor_items_at(preimage_cbor, offset, remaining - 1)
    Pair(offset, [input, ..rest])
  }
}

fn decode_output_cbor_items_at(
  preimage_cbor: ByteArray,
  offset: Int,
  remaining: Int,
) -> Pair<Int, List<MidgardTxOutput>> {
  if remaining <= 0 {
    Pair(offset, [])
  } else {
    let Pair(offset, output_cbor) =
      decode_definite_bytes_at(preimage_cbor, offset)
    let output = decode_midgard_tx_output_cbor(output_cbor)
    let Pair(offset, rest) =
      decode_output_cbor_items_at(preimage_cbor, offset, remaining - 1)
    Pair(offset, [output, ..rest])
  }
}

fn decode_address_witness_cbor_items_at(
  preimage_cbor: ByteArray,
  offset: Int,
  remaining: Int,
) -> Pair<Int, List<MidgardAddressWitness>> {
  if remaining <= 0 {
    Pair(offset, [])
  } else {
    let Pair(offset, witness_cbor) =
      decode_definite_bytes_at(preimage_cbor, offset)
    let witness = decode_midgard_address_witness_cbor(witness_cbor)
    let Pair(offset, rest) =
      decode_address_witness_cbor_items_at(preimage_cbor, offset, remaining - 1)
    Pair(offset, [witness, ..rest])
  }
}

fn decode_script_items_at(
  preimage_cbor: ByteArray,
  offset: Int,
  remaining: Int,
) -> Pair<Int, List<MidgardScriptWitness>> {
  if remaining <= 0 {
    Pair(offset, [])
  } else {
    let Pair(offset, script) =
      decode_midgard_versioned_script_at(preimage_cbor, offset)
    let Pair(offset, rest) =
      decode_script_items_at(preimage_cbor, offset, remaining - 1)
    Pair(offset, [script, ..rest])
  }
}

fn decode_redeemer_witness_items_at(
  preimage_cbor: ByteArray,
  offset: Int,
  remaining: Int,
) -> Pair<Int, List<MidgardRedeemerWitness>> {
  if remaining <= 0 {
    Pair(offset, [])
  } else {
    let Pair(offset, witness) =
      decode_midgard_redeemer_witness_at(preimage_cbor, offset)
    let Pair(offset, rest) =
      decode_redeemer_witness_items_at(preimage_cbor, offset, remaining - 1)
    Pair(offset, [witness, ..rest])
  }
}

pub fn decode_midgard_tx_inputs_preimage_cbor(
  preimage_cbor: ByteArray,
) -> List<MidgardTxInput> {
  let Pair(offset, item_count) =
    decode_definite_array_header_at(preimage_cbor, 0)
  let Pair(offset, inputs) =
    decode_input_cbor_items_at(preimage_cbor, offset, item_count)
  expect offset == bytearray.length(preimage_cbor)
  inputs
}

pub fn decode_midgard_tx_outputs_preimage_cbor(
  preimage_cbor: ByteArray,
) -> List<MidgardTxOutput> {
  let Pair(offset, item_count) =
    decode_definite_array_header_at(preimage_cbor, 0)
  let Pair(offset, outputs) =
    decode_output_cbor_items_at(preimage_cbor, offset, item_count)
  expect offset == bytearray.length(preimage_cbor)
  outputs
}

pub fn decode_midgard_tx_address_witnesses_preimage_cbor(
  preimage_cbor: ByteArray,
) -> List<MidgardAddressWitness> {
  let Pair(offset, item_count) =
    decode_definite_array_header_at(preimage_cbor, 0)
  let Pair(offset, witnesses) =
    decode_address_witness_cbor_items_at(preimage_cbor, offset, item_count)
  expect offset == bytearray.length(preimage_cbor)
  witnesses
}

pub fn decode_midgard_tx_script_witnesses_preimage_cbor(
  preimage_cbor: ByteArray,
) -> List<MidgardScriptWitness> {
  let Pair(offset, item_count) =
    decode_definite_array_header_at(preimage_cbor, 0)
  let Pair(offset, witnesses) =
    decode_script_items_at(preimage_cbor, offset, item_count)
  expect offset == bytearray.length(preimage_cbor)
  witnesses
}

pub fn decode_midgard_tx_redeemer_witnesses_preimage_cbor(
  preimage_cbor: ByteArray,
) -> List<MidgardRedeemerWitness> {
  let Pair(offset, item_count) =
    decode_definite_array_header_at(preimage_cbor, 0)
  let Pair(offset, witnesses) =
    decode_redeemer_witness_items_at(preimage_cbor, offset, item_count)
  expect offset == bytearray.length(preimage_cbor)
  witnesses
}

fn encode_mint_asset_quantities(pairs: List<Pair<Data, Data>>) -> ByteArray {
  when pairs is {
    [] -> #""
    [Pair(asset_name_data, quantity_data), ..rest] -> {
      let asset_name = builtin.un_b_data(asset_name_data)
      let quantity = builtin.un_i_data(quantity_data)
      expect bytearray.length(asset_name) <= 32
      expect quantity != 0
      encode_definite_bytes(asset_name)
        |> bytearray.concat(cbor.serialise(quantity))
        |> bytearray.concat(encode_mint_asset_quantities(rest))
    }
  }
}

fn encode_mint_policy_assets(pairs: List<Pair<Data, Data>>) -> ByteArray {
  when pairs is {
    [] -> #""
    [Pair(policy_id_data, asset_map_data), ..rest] -> {
      let policy_id = builtin.un_b_data(policy_id_data)
      let asset_pairs = builtin.un_map_data(asset_map_data)
      expect bytearray.length(policy_id) == 28
      expect list.length(asset_pairs) > 0
      encode_definite_bytes(policy_id)
        |> bytearray.concat(
            encode_definite_map_header(list.length(asset_pairs))
              |> bytearray.concat(encode_mint_asset_quantities(asset_pairs)),
          )
        |> bytearray.concat(encode_mint_policy_assets(rest))
    }
  }
}

pub fn encode_mint_preimage(mint: Data) -> ByteArray {
  let empty_mint: Data = []
  if mint == empty_mint {
    encode_definite_array_header(0)
  } else {
    let policy_pairs = builtin.un_map_data(mint)
    encode_definite_map_header(list.length(policy_pairs))
      |> bytearray.concat(encode_mint_policy_assets(policy_pairs))
  }
}

pub fn decode_midgard_tx_mint_preimage_cbor(preimage_cbor: ByteArray) -> Data {
  expect_deserialise(preimage_cbor)
}

pub fn verify_spend_inputs_preimage(
  spend_input_cbors: List<ByteArray>,
  expected_spend_inputs_hash: ByteArray,
) -> Bool {
  blake2b_256(encode_native_byte_list(spend_input_cbors)) == expected_spend_inputs_hash
}

pub fn spend_input_cbor_at(
  spend_input_cbors: List<ByteArray>,
  input_index: Int,
) -> ByteArray {
  spend_input_cbors |> list.expect_at(input_index)
}
