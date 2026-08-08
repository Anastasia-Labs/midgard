use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/codec.{
  byte_at, decode_definite_bytes_at, decode_definite_map_header_at,
  decode_uint_at, encode_definite_bytes, encode_definite_map_header, expect_byte,
  expect_h28, expect_h32, slice_len,
}
use midgard/fraud_proofs/native_tx/types.{
  CertRedeemer, MidgardAddress, MidgardAddressWitness, MidgardCredential,
  MidgardExecutionUnits, MidgardRedeemerPurpose, MidgardRedeemerWitness,
  MidgardScriptLanguage, MidgardTxInput, MidgardTxOutput, MidgardV1Script,
  MidgardValue, MidgardVersionedScript, MintRedeemer, NativeCardanoScript,
  PlutusV3Script, ProposeRedeemer, PubKeyCredential, ReceiveRedeemer,
  RewardRedeemer, ScriptCredential, SpendRedeemer, VoteRedeemer,
}

fn expect_output_index(index: Int) -> Int {
  expect index >= 0
  expect index <= 65535
  index
}

fn expect_asset_name(asset_name: ByteArray) -> ByteArray {
  expect bytearray.length(asset_name) <= 32
  asset_name
}

/// The output index of a spend/reference input is **always** the fixed 3-byte
/// `19 XXXX` form (CBOR uint16 head, big-endian value 0–65,535), even for
/// values 0–23 that minimal CBOR would spell in one byte. This is the sole
/// deliberately non-minimal encoding in the format (spec §5.3): it makes every
/// input item exactly 38 bytes with wrapper `58 26`, giving stride 40 and pure
/// arithmetic item access instead of a walk. Picking a different canon does
/// not waive uniqueness — `18 XX`, the minimal one-byte forms, and any wider
/// form all reject.
pub fn encode_fixed_output_index(output_index: Int) -> ByteArray {
  expect _output_index = expect_output_index(output_index)
  bytearray.concat(#"19", bytearray.from_int_big_endian(output_index, 2))
}

fn decode_fixed_output_index_at(bytes: ByteArray, offset: Int) -> Pair<Int, Int> {
  expect byte_at(bytes, offset) == 25
  Pair(
    offset + 3,
    byte_at(bytes, offset + 1) * 256 + byte_at(bytes, offset + 2),
  )
}

pub fn encode_midgard_tx_input(input: MidgardTxInput) -> ByteArray {
  let MidgardTxInput { tx_id, output_index } = input
  expect _tx_id = expect_h32(tx_id)
  #"82"
    |> bytearray.concat(encode_definite_bytes(tx_id))
    |> bytearray.concat(encode_fixed_output_index(output_index))
}

pub fn decode_midgard_tx_input_cbor(input_cbor: ByteArray) -> MidgardTxInput {
  let offset = expect_byte(input_cbor, 0, 130)
  let Pair(offset, tx_id) = decode_definite_bytes_at(input_cbor, offset)
  expect bytearray.length(tx_id) == 32
  let Pair(offset, output_index) = decode_fixed_output_index_at(
    input_cbor,
    offset,
  )
  expect offset == bytearray.length(input_cbor)
  MidgardTxInput { tx_id, output_index }
}

fn midgard_credential_hash(credential: MidgardCredential) -> ByteArray {
  when credential is {
    PubKeyCredential(hash) -> expect_h28(hash)
    ScriptCredential(hash) -> expect_h28(hash)
  }
}

fn midgard_credential_is_script(credential: MidgardCredential) -> Bool {
  when credential is {
    PubKeyCredential(_) -> False
    ScriptCredential(_) -> True
  }
}

fn midgard_credential_from_hash(
  is_script: Bool,
  hash: ByteArray,
) -> MidgardCredential {
  if is_script {
    ScriptCredential(hash)
  } else {
    PubKeyCredential(hash)
  }
}

pub fn encode_midgard_address(address: MidgardAddress) -> ByteArray {
  let
      MidgardAddress {
        protected,
        network_id,
        payment_credential,
        stake_credential,
      }
    = address
  expect or {
      network_id == 0,
      network_id == 1,
    }
  let payment_hash = midgard_credential_hash(payment_credential)
  let address_type = when stake_credential is {
    Some(stake_credential) -> {
      let payment_offset = if midgard_credential_is_script(payment_credential) {
        1
      } else {
        0
      }
      let stake_offset = if midgard_credential_is_script(stake_credential) {
        2
      } else {
        0
      }
      payment_offset + stake_offset
    }
    None ->
      if midgard_credential_is_script(payment_credential) {
        7
      } else {
        6
      }
  }
  let protected_offset = if protected {
    8
  } else {
    0
  }
  let header = address_type * 16 + network_id + protected_offset
  let payment_address = bytearray.push(payment_hash, header)
  when stake_credential is {
    Some(stake_credential) ->
      payment_address
        |> bytearray.concat(midgard_credential_hash(stake_credential))
    None -> payment_address
  }
}

fn decode_midgard_address_payload(
  bytes: ByteArray,
  offset: Int,
  length: Int,
) -> MidgardAddress {
  expect or {
      length == 29,
      length == 57,
    }
  let header = byte_at(bytes, offset)
  let address_type = header / 16
  let network_nibble = header - address_type * 16
  let protected = network_nibble >= 8
  let network_id = if protected {
    network_nibble - 8
  } else {
    network_nibble
  }
  expect or {
      network_id == 0,
      network_id == 1,
    }
  let payment_is_script = or {
    address_type == 1,
    address_type == 3,
    address_type == 7,
  }
  let payment_credential = midgard_credential_from_hash(
    payment_is_script,
    slice_len(bytes, offset + 1, 28),
  )
  let stake_credential = if length == 57 {
    expect address_type <= 3
    let stake_is_script = or {
      address_type == 2,
      address_type == 3,
    }
    Some(
      midgard_credential_from_hash(
        stake_is_script,
        slice_len(bytes, offset + 29, 28),
      ),
    )
  } else {
    expect or {
        address_type == 6,
        address_type == 7,
      }
    None
  }

  let address = MidgardAddress {
    protected,
    network_id,
    payment_credential,
    stake_credential,
  }
  address
}

pub fn decode_midgard_address_bytes(address_bytes: ByteArray) -> MidgardAddress {
  decode_midgard_address_payload(
    address_bytes,
    0,
    bytearray.length(address_bytes),
  )
}

fn decode_midgard_address_at(
  bytes: ByteArray,
  offset: Int,
) -> Pair<Int, MidgardAddress> {
  expect byte_at(bytes, offset) == 88
  let length = byte_at(bytes, offset + 1)
  Pair(
    offset + 2 + length,
    decode_midgard_address_payload(bytes, offset + 2, length),
  )
}

fn expect_asset_unit(unit: ByteArray) -> ByteArray {
  let length = bytearray.length(unit)
  expect length >= 28
  expect length <= 60
  unit
}

pub fn asset_unit_from_policy_asset(
  policy_id: ByteArray,
  asset_name: ByteArray,
) -> ByteArray {
  expect _policy_id = expect_h28(policy_id)
  expect _asset_name = expect_asset_name(asset_name)
  policy_id |> bytearray.concat(asset_name)
}

fn asset_unit_from_decoded_policy_asset(
  policy_id: ByteArray,
  asset_name: ByteArray,
) -> ByteArray {
  expect _asset_name = expect_asset_name(asset_name)
  policy_id |> bytearray.concat(asset_name)
}

fn asset_unit_policy_id(unit: ByteArray) -> ByteArray {
  expect _unit = expect_asset_unit(unit)
  bytearray.take(unit, 28)
}

fn asset_unit_name(unit: ByteArray) -> ByteArray {
  expect _unit = expect_asset_unit(unit)
  bytearray.drop(unit, 28)
}

fn decode_h28_at(bytes: ByteArray, offset: Int) -> Pair<Int, ByteArray> {
  expect byte_at(bytes, offset) == 88
  expect byte_at(bytes, offset + 1) == 28
  Pair(offset + 30, slice_len(bytes, offset + 2, 28))
}

fn decode_midgard_asset_quantities(
  policy_id: ByteArray,
  pairs: List<Pair<Data, Data>>,
  tail: List<Pair<ByteArray, Int>>,
) -> List<Pair<ByteArray, Int>> {
  when pairs is {
    [] -> tail
    [Pair(asset_name_data, quantity_data), ..rest] -> {
      let asset_name = builtin.un_b_data(asset_name_data)
      let quantity = builtin.un_i_data(quantity_data)
      expect quantity > 0
      [
        Pair(
          asset_unit_from_decoded_policy_asset(policy_id, asset_name),
          quantity,
        ),
        ..decode_midgard_asset_quantities(policy_id, rest, tail)
      ]
    }
  }
}

fn decode_midgard_policy_assets(
  pairs: List<Pair<Data, Data>>,
) -> List<Pair<ByteArray, Int>> {
  when pairs is {
    [] -> []
    [pair, ..rest] -> {
      let Pair(policy_id_data, assets_data) = pair
      let policy_id = builtin.un_b_data(policy_id_data)
      expect _policy_id = expect_h28(policy_id)
      let asset_pairs = builtin.un_map_data(assets_data)
      expect [_, ..] = asset_pairs
      decode_midgard_asset_quantities(
        policy_id,
        asset_pairs,
        decode_midgard_policy_assets(rest),
      )
    }
  }
}

fn encode_midgard_asset_quantities(
  policy_id: ByteArray,
  assets: List<Pair<ByteArray, Int>>,
  remaining: Int,
) -> ByteArray {
  if remaining <= 0 {
    #""
  } else {
    expect [Pair(unit, quantity), ..rest] = assets
    expect asset_unit_policy_id(unit) == policy_id
    expect quantity > 0
    encode_definite_bytes(asset_unit_name(unit))
      |> bytearray.concat(cbor.serialise(quantity))
      |> bytearray.concat(
          encode_midgard_asset_quantities(policy_id, rest, remaining - 1),
        )
  }
}

fn count_policy_prefix(
  policy_id: ByteArray,
  assets: List<Pair<ByteArray, Int>>,
) -> Int {
  when assets is {
    [] -> 0
    [Pair(unit, _), ..rest] ->
      if asset_unit_policy_id(unit) == policy_id {
        1 + count_policy_prefix(policy_id, rest)
      } else {
        0
      }
  }
}

fn count_policy_groups(
  assets: List<Pair<ByteArray, Int>>,
  previous_policy_id: Option<ByteArray>,
) -> Int {
  when assets is {
    [] -> 0
    [Pair(unit, _), ..rest] -> {
      let policy_id = asset_unit_policy_id(unit)
      let increment = when previous_policy_id is {
        Some(previous_policy_id) ->
          if policy_id == previous_policy_id {
            0
          } else {
            1
          }
        None -> 1
      }
      increment + count_policy_groups(rest, Some(policy_id))
    }
  }
}

fn encode_midgard_policy_assets_body(
  assets: List<Pair<ByteArray, Int>>,
) -> ByteArray {
  when assets is {
    [] -> #""
    [Pair(unit, _), ..] -> {
      let policy_id = asset_unit_policy_id(unit)
      let asset_count = count_policy_prefix(policy_id, assets)
      encode_definite_bytes(policy_id)
        |> bytearray.concat(
            encode_definite_map_header(asset_count)
              |> bytearray.concat(
                  encode_midgard_asset_quantities(
                    policy_id,
                    assets,
                    asset_count,
                  ),
                ),
          )
        |> bytearray.concat(
            encode_midgard_policy_assets_body(list.drop(assets, asset_count)),
          )
    }
  }
}

fn encode_midgard_policy_assets(assets: List<Pair<ByteArray, Int>>) -> ByteArray {
  encode_definite_map_header(count_policy_groups(assets, None))
    |> bytearray.concat(encode_midgard_policy_assets_body(assets))
}

pub fn encode_midgard_value(value: MidgardValue) -> ByteArray {
  expect value.lovelace >= 0
  #"82"
    |> bytearray.concat(cbor.serialise(value.lovelace))
    |> bytearray.concat(encode_midgard_policy_assets(value.assets))
}

fn decode_midgard_value_data(data: Data) -> MidgardValue {
  expect [lovelace_data, assets_data] = builtin.un_list_data(data)
  let lovelace = builtin.un_i_data(lovelace_data)
  expect lovelace >= 0
  MidgardValue {
    lovelace,
    assets: decode_midgard_policy_assets(builtin.un_map_data(assets_data)),
  }
}

fn append_assets(
  left: List<Pair<ByteArray, Int>>,
  right: List<Pair<ByteArray, Int>>,
) -> List<Pair<ByteArray, Int>> {
  when left is {
    [] -> right
    [asset, ..rest] -> [asset, ..append_assets(rest, right)]
  }
}

fn decode_midgard_asset_quantities_at(
  bytes: ByteArray,
  offset: Int,
  remaining: Int,
  policy_id: ByteArray,
) -> Pair<Int, List<Pair<ByteArray, Int>>> {
  if remaining <= 0 {
    Pair(offset, [])
  } else {
    let Pair(offset, asset_name) = decode_definite_bytes_at(bytes, offset)
    expect _asset_name = expect_asset_name(asset_name)
    let Pair(offset, quantity) = decode_uint_at(bytes, offset)
    expect quantity > 0
    let Pair(offset, rest) = decode_midgard_asset_quantities_at(
      bytes,
      offset,
      remaining - 1,
      policy_id,
    )
    Pair(
      offset,
      [Pair(policy_id |> bytearray.concat(asset_name), quantity), ..rest],
    )
  }
}

fn decode_midgard_policy_assets_at(
  bytes: ByteArray,
  offset: Int,
  remaining: Int,
) -> Pair<Int, List<Pair<ByteArray, Int>>> {
  if remaining <= 0 {
    Pair(offset, [])
  } else {
    let Pair(offset, policy_id) = decode_h28_at(bytes, offset)
    let Pair(offset, asset_count) = decode_definite_map_header_at(bytes, offset)
    expect asset_count > 0
    let Pair(offset, policy_assets) = decode_midgard_asset_quantities_at(
      bytes,
      offset,
      asset_count,
      policy_id,
    )
    let Pair(offset, rest_assets) = decode_midgard_policy_assets_at(
      bytes,
      offset,
      remaining - 1,
    )
    Pair(offset, append_assets(policy_assets, rest_assets))
  }
}

fn decode_midgard_value_at(
  bytes: ByteArray,
  offset: Int,
) -> Pair<Int, MidgardValue> {
  let offset = expect_byte(bytes, offset, 130)
  let Pair(offset, lovelace) = decode_uint_at(bytes, offset)
  let Pair(offset, policy_count) = decode_definite_map_header_at(bytes, offset)
  let Pair(offset, assets) = decode_midgard_policy_assets_at(
    bytes,
    offset,
    policy_count,
  )
  Pair(offset, MidgardValue { lovelace, assets })
}

fn midgard_script_language_to_tag(language: MidgardScriptLanguage) -> Int {
  when language is {
    NativeCardanoScript -> 0
    PlutusV3Script -> 3
    MidgardV1Script -> 128
  }
}

fn midgard_script_language_from_tag(tag: Int) -> MidgardScriptLanguage {
  if tag == 0 {
    NativeCardanoScript
  } else if tag == 3 {
    PlutusV3Script
  } else {
    expect tag == 128
    MidgardV1Script
  }
}

fn decode_midgard_script_language_tag_at(
  bytes: ByteArray,
  offset: Int,
) -> Pair<Int, Int> {
  let tag = byte_at(bytes, offset)
  if or {
    tag == 0,
    tag == 3,
  } {
    Pair(offset + 1, tag)
  } else {
    expect tag == 24
    let value = byte_at(bytes, offset + 1)
    Pair(offset + 2, value)
  }
}

pub fn decode_midgard_versioned_script_at(
  bytes: ByteArray,
  offset: Int,
) -> Pair<Int, MidgardVersionedScript> {
  let offset = expect_byte(bytes, offset, 130)
  let Pair(offset, tag) = decode_midgard_script_language_tag_at(bytes, offset)
  let Pair(offset, script_bytes) = decode_definite_bytes_at(bytes, offset)
  Pair(
    offset,
    MidgardVersionedScript {
      language: tag |> midgard_script_language_from_tag,
      script_bytes,
    },
  )
}

pub fn encode_midgard_versioned_script(
  script: MidgardVersionedScript,
) -> ByteArray {
  #"82"
    |> bytearray.concat(
        cbor.serialise(script.language |> midgard_script_language_to_tag),
      )
    |> bytearray.concat(encode_definite_bytes(script.script_bytes))
}

pub fn decode_midgard_versioned_script_data(
  data: Data,
) -> MidgardVersionedScript {
  expect [tag_data, script_bytes_data] = builtin.un_list_data(data)
  MidgardVersionedScript {
    language: tag_data |> builtin.un_i_data |> midgard_script_language_from_tag,
    script_bytes: script_bytes_data |> builtin.un_b_data,
  }
}

pub fn encode_midgard_tx_output(output: MidgardTxOutput) -> ByteArray {
  let MidgardTxOutput { address, value, datum_cbor, script_ref } = output
  let required_entries = #"a2"
    |> bytearray.concat(cbor.serialise(0))
    |> bytearray.concat(encode_definite_bytes(encode_midgard_address(address)))
    |> bytearray.concat(cbor.serialise(1))
    |> bytearray.concat(encode_midgard_value(value))
  when datum_cbor is {
    Some(datum_cbor) ->
      when script_ref is {
        Some(script_ref) ->
          #"a4"
            |> bytearray.concat(cbor.serialise(0))
            |> bytearray.concat(
                encode_definite_bytes(encode_midgard_address(address)),
              )
            |> bytearray.concat(cbor.serialise(1))
            |> bytearray.concat(encode_midgard_value(value))
            |> bytearray.concat(cbor.serialise(2))
            |> bytearray.concat(encode_definite_bytes(datum_cbor))
            |> bytearray.concat(cbor.serialise(3))
            |> bytearray.concat(encode_midgard_versioned_script(script_ref))
        None ->
          #"a3"
            |> bytearray.concat(cbor.serialise(0))
            |> bytearray.concat(
                encode_definite_bytes(encode_midgard_address(address)),
              )
            |> bytearray.concat(cbor.serialise(1))
            |> bytearray.concat(encode_midgard_value(value))
            |> bytearray.concat(cbor.serialise(2))
            |> bytearray.concat(encode_definite_bytes(datum_cbor))
      }
    None ->
      when script_ref is {
        Some(script_ref) ->
          #"a3"
            |> bytearray.concat(cbor.serialise(0))
            |> bytearray.concat(
                encode_definite_bytes(encode_midgard_address(address)),
              )
            |> bytearray.concat(cbor.serialise(1))
            |> bytearray.concat(encode_midgard_value(value))
            |> bytearray.concat(cbor.serialise(3))
            |> bytearray.concat(encode_midgard_versioned_script(script_ref))
        None -> required_entries
      }
  }
}

pub fn decode_midgard_tx_output_data(data: Data) -> MidgardTxOutput {
  let entries = builtin.un_map_data(data)
  let output = when entries is {
    [Pair(key_0, address_data), Pair(key_1, value_data)] -> {
      expect builtin.un_i_data(key_0) == 0
      expect builtin.un_i_data(key_1) == 1
      MidgardTxOutput {
        address: address_data
          |> builtin.un_b_data
          |> decode_midgard_address_bytes,
        value: decode_midgard_value_data(value_data),
        datum_cbor: None,
        script_ref: None,
      }
    }
    [
      Pair(key_0, address_data), Pair(key_1, value_data),
      Pair(key_2, extra_data),
    ] -> {
      expect builtin.un_i_data(key_0) == 0
      expect builtin.un_i_data(key_1) == 1
      let extra_key = builtin.un_i_data(key_2)
      if extra_key == 2 {
        MidgardTxOutput {
          address: address_data
            |> builtin.un_b_data
            |> decode_midgard_address_bytes,
          value: decode_midgard_value_data(value_data),
          datum_cbor: Some(builtin.un_b_data(extra_data)),
          script_ref: None,
        }
      } else {
        expect extra_key == 3
        MidgardTxOutput {
          address: address_data
            |> builtin.un_b_data
            |> decode_midgard_address_bytes,
          value: decode_midgard_value_data(value_data),
          datum_cbor: None,
          script_ref: Some(decode_midgard_versioned_script_data(extra_data)),
        }
      }
    }
    [
      Pair(key_0, address_data), Pair(key_1, value_data),
      Pair(key_2, datum_data), Pair(key_3, script_ref_data),
    ] -> {
      expect builtin.un_i_data(key_0) == 0
      expect builtin.un_i_data(key_1) == 1
      expect builtin.un_i_data(key_2) == 2
      expect builtin.un_i_data(key_3) == 3
      MidgardTxOutput {
        address: address_data
          |> builtin.un_b_data
          |> decode_midgard_address_bytes,
        value: decode_midgard_value_data(value_data),
        datum_cbor: Some(builtin.un_b_data(datum_data)),
        script_ref: Some(decode_midgard_versioned_script_data(script_ref_data)),
      }
    }
    _ -> fail
  }
  output
}

pub fn decode_midgard_tx_output_cbor(output_cbor: ByteArray) -> MidgardTxOutput {
  let entry_count_tag = byte_at(output_cbor, 0)
  expect and {
      entry_count_tag >= 162,
      entry_count_tag <= 164,
    }
  let entry_count = entry_count_tag - 160
  let offset = 1
  let offset = expect_byte(output_cbor, offset, 0)
  let Pair(offset, address) = decode_midgard_address_at(output_cbor, offset)
  let offset = expect_byte(output_cbor, offset, 1)
  let Pair(offset, value) = decode_midgard_value_at(output_cbor, offset)

  if entry_count == 2 {
    expect offset == bytearray.length(output_cbor)
    MidgardTxOutput { address, value, datum_cbor: None, script_ref: None }
  } else {
    let extra_key = byte_at(output_cbor, offset)
    let offset = offset + 1
    if extra_key == 2 {
      let Pair(offset, datum_cbor) = decode_definite_bytes_at(
        output_cbor,
        offset,
      )
      if entry_count == 3 {
        expect offset == bytearray.length(output_cbor)
        MidgardTxOutput {
          address,
          value,
          datum_cbor: Some(datum_cbor),
          script_ref: None,
        }
      } else {
        expect entry_count == 4
        let offset = expect_byte(output_cbor, offset, 3)
        let Pair(offset, script_ref) = decode_midgard_versioned_script_at(
          output_cbor,
          offset,
        )
        expect offset == bytearray.length(output_cbor)
        MidgardTxOutput {
          address,
          value,
          datum_cbor: Some(datum_cbor),
          script_ref: Some(script_ref),
        }
      }
    } else {
      expect extra_key == 3
      expect entry_count == 3
      let Pair(offset, script_ref) = decode_midgard_versioned_script_at(
        output_cbor,
        offset,
      )
      expect offset == bytearray.length(output_cbor)
      MidgardTxOutput {
        address,
        value,
        datum_cbor: None,
        script_ref: Some(script_ref),
      }
    }
  }
}

pub fn encode_midgard_address_witness(
  witness: MidgardAddressWitness,
) -> ByteArray {
  expect bytearray.length(witness.verification_key) == 32
  expect bytearray.length(witness.signature) == 64
  #"82"
    |> bytearray.concat(encode_definite_bytes(witness.verification_key))
    |> bytearray.concat(encode_definite_bytes(witness.signature))
}

pub fn decode_midgard_address_witness_cbor(
  witness_cbor: ByteArray,
) -> MidgardAddressWitness {
  let offset = expect_byte(witness_cbor, 0, 130)
  let Pair(offset, verification_key) = decode_definite_bytes_at(
    witness_cbor,
    offset,
  )
  expect bytearray.length(verification_key) == 32
  let Pair(offset, signature) = decode_definite_bytes_at(witness_cbor, offset)
  expect bytearray.length(signature) == 64
  expect offset == bytearray.length(witness_cbor)
  MidgardAddressWitness { verification_key, signature }
}

fn midgard_redeemer_purpose_to_tag(purpose: MidgardRedeemerPurpose) -> Int {
  when purpose is {
    SpendRedeemer -> 0
    MintRedeemer -> 1
    CertRedeemer -> 2
    RewardRedeemer -> 3
    VoteRedeemer -> 4
    ProposeRedeemer -> 5
    ReceiveRedeemer -> 6
  }
}

fn midgard_redeemer_purpose_from_tag(tag: Int) -> MidgardRedeemerPurpose {
  if tag == 0 {
    SpendRedeemer
  } else if tag == 1 {
    MintRedeemer
  } else if tag == 2 {
    CertRedeemer
  } else if tag == 3 {
    RewardRedeemer
  } else if tag == 4 {
    VoteRedeemer
  } else if tag == 5 {
    ProposeRedeemer
  } else {
    expect tag == 6
    ReceiveRedeemer
  }
}

fn decode_midgard_redeemer_purpose_tag_at(
  bytes: ByteArray,
  offset: Int,
) -> Pair<Int, Int> {
  let tag = byte_at(bytes, offset)
  Pair(offset + 1, tag)
}

pub fn encode_midgard_redeemer_witness_data(
  witness: MidgardRedeemerWitness,
) -> Data {
  let MidgardRedeemerWitness { purpose, index, redeemer_cbor, execution_units }
    = witness
  expect index >= 0
  expect execution_units.memory >= 0
  expect execution_units.steps >= 0
  builtin.list_data(
    [
      builtin.i_data(purpose |> midgard_redeemer_purpose_to_tag),
      builtin.i_data(index),
      builtin.b_data(redeemer_cbor),
      builtin.list_data(
        [
          builtin.i_data(execution_units.memory),
          builtin.i_data(execution_units.steps),
        ],
      ),
    ],
  )
}

pub fn encode_midgard_redeemer_witness(
  witness: MidgardRedeemerWitness,
) -> ByteArray {
  let MidgardRedeemerWitness { purpose, index, redeemer_cbor, execution_units }
    = witness
  expect index >= 0
  expect execution_units.memory >= 0
  expect execution_units.steps >= 0
  #"84"
    |> bytearray.concat(
        cbor.serialise(purpose |> midgard_redeemer_purpose_to_tag),
      )
    |> bytearray.concat(cbor.serialise(index))
    |> bytearray.concat(encode_definite_bytes(redeemer_cbor))
    |> bytearray.concat(#"82")
    |> bytearray.concat(cbor.serialise(execution_units.memory))
    |> bytearray.concat(cbor.serialise(execution_units.steps))
}

pub fn decode_midgard_redeemer_witness_at(
  bytes: ByteArray,
  offset: Int,
) -> Pair<Int, MidgardRedeemerWitness> {
  let offset = expect_byte(bytes, offset, 132)
  let Pair(offset, purpose_tag) = decode_midgard_redeemer_purpose_tag_at(
    bytes,
    offset,
  )
  let Pair(offset, index) = decode_uint_at(bytes, offset)
  let Pair(offset, redeemer_cbor) = decode_definite_bytes_at(bytes, offset)
  let offset = expect_byte(bytes, offset, 130)
  let Pair(offset, memory) = decode_uint_at(bytes, offset)
  let Pair(offset, steps) = decode_uint_at(bytes, offset)
  Pair(
    offset,
    MidgardRedeemerWitness {
      purpose: purpose_tag |> midgard_redeemer_purpose_from_tag,
      index,
      redeemer_cbor,
      execution_units: MidgardExecutionUnits { memory, steps },
    },
  )
}

pub fn decode_midgard_redeemer_witness_data(
  data: Data,
) -> MidgardRedeemerWitness {
  expect [purpose_data,
        index_data,
        redeemer_cbor_data, execution_units_data] = builtin.un_list_data(data)
  expect [memory_data, steps_data] = builtin.un_list_data(execution_units_data)
  let witness = MidgardRedeemerWitness {
    purpose: purpose_data
      |> builtin.un_i_data
      |> midgard_redeemer_purpose_from_tag,
    index: builtin.un_i_data(index_data),
    redeemer_cbor: builtin.un_b_data(redeemer_cbor_data),
    execution_units: MidgardExecutionUnits {
      memory: builtin.un_i_data(memory_data),
      steps: builtin.un_i_data(steps_data),
    },
  }
  witness
}
