use aiken/collection/dict
use aiken/crypto.{VerificationKeyHash, blake2b_256}
use aiken/primitive/bytearray
use cardano/address.{Address, Script}
use cardano/assets.{PolicyId, Value}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction,
}
use midgard/da_attestation_types.{
  DaParamsDatum, da_params_asset_name, max_indexed_signer_count,
  verification_key_byte_count,
}

const verification_key_hash_byte_count: Int = 28

fn bytearray_slice(bytes: ByteArray, offset: Int, byte_count: Int) -> ByteArray {
  bytearray.slice(bytes, offset, offset + byte_count - 1)
}

fn sorted_unique_packed_len_at_most(
  keys: ByteArray,
  max: Int,
  byte_count: Int,
) -> Int {
  expect max > 0
  let keys_len = bytearray.length(keys)
  expect keys_len >= byte_count
  let first = bytearray_slice(keys, 0, byte_count)
  sorted_unique_packed_len_from(
    keys,
    byte_count,
    first,
    1,
    max,
    byte_count,
    keys_len,
  )
}

fn sorted_unique_packed_len_from(
  keys: ByteArray,
  offset: Int,
  prev: ByteArray,
  len: Int,
  max: Int,
  byte_count: Int,
  keys_len: Int,
) -> Int {
  if offset == keys_len {
    len
  } else {
    expect offset + byte_count <= keys_len
    let key = bytearray_slice(keys, offset, byte_count)
    expect bytearray.compare(prev, key) == Less
    let next_len = len + 1
    expect next_len <= max
    sorted_unique_packed_len_from(
      keys,
      offset + byte_count,
      key,
      next_len,
      max,
      byte_count,
      keys_len,
    )
  }
}

fn sorted_unique_len_at_most(
  keys: List<ByteArray>,
  max: Int,
  byte_count: Int,
) -> Int {
  expect [first, ..rest] = keys
  expect bytearray.length(first) == byte_count
  sorted_unique_len_from(first, rest, 1, max, byte_count)
}

fn sorted_unique_len_from(
  prev: ByteArray,
  keys: List<ByteArray>,
  len: Int,
  max: Int,
  byte_count: Int,
) -> Int {
  when keys is {
    [] -> len
    [key, ..rest] -> {
      expect bytearray.length(key) == byte_count
      expect bytearray.compare(prev, key) == Less
      let next_len = len + 1
      expect next_len <= max
      sorted_unique_len_from(key, rest, next_len, max, byte_count)
    }
  }
}

fn valid_datum(
  datum: DaParamsDatum,
  max_committee_size: Int,
  max_owner_count: Int,
) -> Bool {
  expect max_committee_size > 0
  expect max_committee_size <= max_indexed_signer_count
  expect max_owner_count > 0

  expect datum.da_threshold > 0
  let committee_len = sorted_unique_packed_len_at_most(
    datum.committee,
    max_committee_size,
    verification_key_byte_count,
  )
  expect datum.committee_signers_hash == blake2b_256(datum.committee)
  expect datum.da_threshold <= committee_len

  expect datum.update_threshold > 0
  let owner_len = sorted_unique_len_at_most(
    datum.owners,
    max_owner_count,
    verification_key_hash_byte_count,
  )
  expect datum.update_threshold <= owner_len

  True
}

fn has_input_ref(inputs: List<Input>, out_ref: OutputReference) -> Bool {
  when inputs is {
    [] -> False
    [input, ..rest] ->
      if input.output_reference == out_ref {
        True
      } else {
        has_input_ref(rest, out_ref)
      }
  }
}

fn input_at_ref(inputs: List<Input>, out_ref: OutputReference) -> Input {
  when inputs is {
    [] -> fail
    [input, ..rest] ->
      if input.output_reference == out_ref {
        input
      } else {
        input_at_ref(rest, out_ref)
      }
  }
}

fn only_output_with_nft(
  outputs: List<Output>,
  policy_id: PolicyId,
  found: Option<Output>,
) -> Output {
  when outputs is {
    [] -> {
      expect Some(output) = found
      output
    }
    [output, ..rest] -> {
      let quantity = assets.quantity_of(
        output.value,
        policy_id,
        da_params_asset_name,
      )

      if quantity == 0 {
        only_output_with_nft(rest, policy_id, found)
      } else {
        expect quantity == 1
        expect found == None
        only_output_with_nft(rest, policy_id, Some(output))
      }
    }
  }
}

fn params_output_datum_at_script(
  output: Output,
  policy_id: PolicyId,
  max_committee_size: Int,
  max_owner_count: Int,
) -> DaParamsDatum {
  expect
      Output {
        address,
        datum: InlineDatum(datum_data),
        reference_script: None,
        ..
      }
    = output
  expect Script(output_script_hash) = address.payment_credential
  expect output_script_hash == policy_id
  expect datum: DaParamsDatum = datum_data
  expect valid_datum(datum, max_committee_size, max_owner_count)
  datum
}

fn params_output_datum_at_address(
  output: Output,
  address: Address,
  max_committee_size: Int,
  max_owner_count: Int,
) -> DaParamsDatum {
  expect
      Output {
        address: output_address,
        datum: InlineDatum(datum_data),
        reference_script: None,
        ..
      }
    = output
  expect output_address == address
  expect datum: DaParamsDatum = datum_data
  expect valid_datum(datum, max_committee_size, max_owner_count)
  datum
}

fn value_not_drained(input_value: Value, output_value: Value) -> Bool {
  let output_value_data: Data = output_value
  assets.expect_match(
    input_value,
    output_value_data,
    fn(input_lovelace, output_lovelace) { output_lovelace >= input_lovelace },
  )
}

fn has_key(keys: List<VerificationKeyHash>, key: VerificationKeyHash) -> Bool {
  when keys is {
    [] -> False
    [current, ..rest] ->
      if current == key {
        True
      } else {
        has_key(rest, key)
      }
  }
}

fn owner_quorum_met(
  owners: List<VerificationKeyHash>,
  signers: List<VerificationKeyHash>,
  threshold: Int,
  count: Int,
) -> Bool {
  if count >= threshold {
    True
  } else {
    when owners is {
      [] -> False
      [owner, ..rest] -> {
        let next_count = if has_key(signers, owner) {
          count + 1
        } else {
          count
        }
        owner_quorum_met(rest, signers, threshold, next_count)
      }
    }
  }
}

validator da_params_governor(
  init_ref: OutputReference,
  max_committee_size: Int,
  max_owner_count: Int,
) {
  mint(_redeemer: Data, policy_id: PolicyId, tx: Transaction) {
    expect [Pair(asset_name, minted_quantity)] = tx.mint
      |> assets.tokens(policy_id)
      |> dict.to_pairs
    expect asset_name == da_params_asset_name
    expect minted_quantity == 1

    expect has_input_ref(tx.inputs, init_ref)

    expect _output_datum = tx.outputs
      |> only_output_with_nft(policy_id, None)
      |> params_output_datum_at_script(
          policy_id,
          max_committee_size,
          max_owner_count,
        )

    True
  }

  spend(
    datum: Option<DaParamsDatum>,
    _redeemer: Data,
    own_ref: OutputReference,
    tx: Transaction,
  ) {
    expect Some(input_datum) = datum
    expect valid_datum(input_datum, max_committee_size, max_owner_count)

    let own_input = input_at_ref(tx.inputs, own_ref)
    expect
        Output {
          address: input_address,
          value: input_value,
          datum: InlineDatum(input_datum_data),
          reference_script: None,
        }
      = own_input.output
    let expected_input_datum_data: Data = input_datum
    expect input_datum_data == expected_input_datum_data
    expect Script(policy_id) = input_address.payment_credential
    expect assets.quantity_of(input_value, policy_id, da_params_asset_name) == 1

    expect
      owner_quorum_met(
        input_datum.owners,
        tx.extra_signatories,
        input_datum.update_threshold,
        0,
      )

    let continued_output = tx.outputs |> only_output_with_nft(policy_id, None)

    expect _continued_datum = continued_output
      |> params_output_datum_at_address(
          input_address,
          max_committee_size,
          max_owner_count,
        )
    expect value_not_drained(input_value, continued_output.value)

    True
  }

  else(_) {
    fail
  }
}
