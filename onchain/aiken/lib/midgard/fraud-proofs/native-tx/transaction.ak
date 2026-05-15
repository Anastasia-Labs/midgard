use aiken/builtin
use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/codec.{
  decode_definite_array_header_at, decode_definite_bytes_at, decode_int_at,
  decode_uint_at, encode_definite_bytes, encode_native_byte_list,
  expect_deserialise, expect_preimage_hash, validity_from_code, validity_to_code,
}
use midgard/fraud_proofs/native_tx/compact.{
  encode_native_tx_body_compact, encode_native_tx_compact,
  encode_native_tx_witness_set_compact,
}
use midgard/fraud_proofs/native_tx/preimages.{
  decode_midgard_tx_address_witnesses_preimage_cbor,
  decode_midgard_tx_byte_list_preimage_cbor,
  decode_midgard_tx_inputs_preimage_cbor, decode_midgard_tx_mint_preimage_cbor,
  decode_midgard_tx_outputs_preimage_cbor,
  decode_midgard_tx_redeemer_witnesses_preimage_cbor,
  decode_midgard_tx_script_witnesses_preimage_cbor,
  encode_address_witness_preimage, encode_input_preimage, encode_mint_preimage,
  encode_output_preimage, encode_redeemer_witness_preimage,
  encode_script_witness_preimage,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddressWitness, MidgardRedeemerWitness, MidgardScriptWitness,
  MidgardTransaction, MidgardTransactionBody,
  MidgardTransactionBodyPartialPreimages, MidgardTransactionBodyPartialView,
  MidgardTransactionPartialView, MidgardTransactionWitnessSet,
  MidgardTransactionWitnessSetPartialPreimages,
  MidgardTransactionWitnessSetPartialView, MidgardTxInput, MidgardTxOutput,
  NativeTxBodyCompact, NativeTxCompact, NativeTxWitnessSetCompact,
  VerifiedMidgardNativeTxCompact, VerifiedMidgardTransaction, native_tx_version,
}

fn decode_midgard_tx_inputs_preimage(
  preimage_cbor: ByteArray,
  expected_hash: ByteArray,
) -> List<MidgardTxInput> {
  expect_preimage_hash(preimage_cbor, expected_hash)
    |> decode_midgard_tx_inputs_preimage_cbor
}

fn decode_midgard_tx_outputs_preimage(
  preimage_cbor: ByteArray,
  expected_hash: ByteArray,
) -> List<MidgardTxOutput> {
  expect_preimage_hash(preimage_cbor, expected_hash)
    |> decode_midgard_tx_outputs_preimage_cbor
}

fn decode_midgard_tx_byte_list_preimage(
  preimage_cbor: ByteArray,
  expected_hash: ByteArray,
) -> List<ByteArray> {
  expect_preimage_hash(preimage_cbor, expected_hash)
    |> decode_midgard_tx_byte_list_preimage_cbor
}

fn decode_midgard_tx_mint_preimage(
  preimage_cbor: ByteArray,
  expected_hash: ByteArray,
) -> Data {
  expect_preimage_hash(preimage_cbor, expected_hash)
    |> decode_midgard_tx_mint_preimage_cbor
}

fn decode_midgard_tx_address_witnesses_preimage(
  preimage_cbor: ByteArray,
  expected_hash: ByteArray,
) -> List<MidgardAddressWitness> {
  expect_preimage_hash(preimage_cbor, expected_hash)
    |> decode_midgard_tx_address_witnesses_preimage_cbor
}

fn decode_midgard_tx_script_witnesses_preimage(
  preimage_cbor: ByteArray,
  expected_hash: ByteArray,
) -> List<MidgardScriptWitness> {
  expect_preimage_hash(preimage_cbor, expected_hash)
    |> decode_midgard_tx_script_witnesses_preimage_cbor
}

fn decode_midgard_tx_redeemer_witnesses_preimage(
  preimage_cbor: ByteArray,
  expected_hash: ByteArray,
) -> List<MidgardRedeemerWitness> {
  expect_preimage_hash(preimage_cbor, expected_hash)
    |> decode_midgard_tx_redeemer_witnesses_preimage_cbor
}

pub fn encode_midgard_transaction_body_full(
  body: MidgardTransactionBody,
) -> ByteArray {
  #"8c"
    |> bytearray.concat(
        encode_definite_bytes(encode_input_preimage(body.inputs)),
      )
    |> bytearray.concat(
        encode_definite_bytes(encode_input_preimage(body.reference_inputs)),
      )
    |> bytearray.concat(
        encode_definite_bytes(encode_output_preimage(body.outputs)),
      )
    |> bytearray.concat(cbor.serialise(body.fee))
    |> bytearray.concat(cbor.serialise(body.validity_interval_start))
    |> bytearray.concat(cbor.serialise(body.validity_interval_end))
    |> bytearray.concat(
        encode_definite_bytes(encode_native_byte_list(body.required_observers)),
      )
    |> bytearray.concat(
        encode_definite_bytes(encode_native_byte_list(body.required_signers)),
      )
    |> bytearray.concat(encode_definite_bytes(encode_mint_preimage(body.mint)))
    |> bytearray.concat(encode_definite_bytes(body.script_integrity_hash))
    |> bytearray.concat(encode_definite_bytes(body.auxiliary_data_hash))
    |> bytearray.concat(cbor.serialise(body.network_id))
}

pub fn encode_midgard_transaction_witness_set_full(
  witness_set: MidgardTransactionWitnessSet,
) -> ByteArray {
  #"83"
    |> bytearray.concat(
        encode_definite_bytes(
          encode_address_witness_preimage(witness_set.addr_tx_wits),
        ),
      )
    |> bytearray.concat(
        encode_definite_bytes(
          encode_script_witness_preimage(witness_set.script_tx_wits),
        ),
      )
    |> bytearray.concat(
        encode_definite_bytes(
          encode_redeemer_witness_preimage(witness_set.redeemer_tx_wits),
        ),
      )
}

pub fn midgard_transaction_body_to_compact(
  body: MidgardTransactionBody,
) -> NativeTxBodyCompact {
  NativeTxBodyCompact {
    spend_inputs_hash: encode_input_preimage(body.inputs) |> blake2b_256,
    reference_inputs_hash: encode_input_preimage(body.reference_inputs)
      |> blake2b_256,
    outputs_hash: encode_output_preimage(body.outputs) |> blake2b_256,
    fee: body.fee,
    validity_interval_start: body.validity_interval_start,
    validity_interval_end: body.validity_interval_end,
    required_observers_hash: encode_native_byte_list(body.required_observers)
      |> blake2b_256,
    required_signers_hash: encode_native_byte_list(body.required_signers)
      |> blake2b_256,
    mint_hash: encode_mint_preimage(body.mint) |> blake2b_256,
    script_integrity_hash: body.script_integrity_hash,
    auxiliary_data_hash: body.auxiliary_data_hash,
    network_id: body.network_id,
  }
}

pub fn midgard_transaction_witness_set_to_compact(
  witness_set: MidgardTransactionWitnessSet,
) -> NativeTxWitnessSetCompact {
  NativeTxWitnessSetCompact {
    addr_tx_wits_hash: encode_address_witness_preimage(witness_set.addr_tx_wits)
      |> blake2b_256,
    script_tx_wits_hash: encode_script_witness_preimage(
      witness_set.script_tx_wits,
    )
      |> blake2b_256,
    redeemer_tx_wits_hash: encode_redeemer_witness_preimage(
      witness_set.redeemer_tx_wits,
    )
      |> blake2b_256,
  }
}

pub fn midgard_transaction_to_compact(tx: MidgardTransaction) -> NativeTxCompact {
  let witness_set_hash =
    tx.witness_set
      |> midgard_transaction_witness_set_to_compact
      |> encode_native_tx_witness_set_compact
      |> blake2b_256

  NativeTxCompact {
    body: tx.body |> midgard_transaction_body_to_compact,
    witness_set_hash,
    validity_code: tx.validity |> validity_to_code,
  }
}

pub fn encode_midgard_transaction(tx: MidgardTransaction) -> ByteArray {
  expect tx.version == native_tx_version
  #"84"
    |> bytearray.concat(cbor.serialise(tx.version))
    |> bytearray.concat(encode_midgard_transaction_body_full(tx.body))
    |> bytearray.concat(
        encode_midgard_transaction_witness_set_full(tx.witness_set),
      )
    |> bytearray.concat(cbor.serialise(tx.validity |> validity_to_code))
}

fn decode_midgard_transaction_body_data(data: Data) -> MidgardTransactionBody {
  expect [
    inputs_preimage_cbor_data, reference_inputs_preimage_cbor_data,
    outputs_preimage_cbor_data, fee_data, validity_interval_start_data,
    validity_interval_end_data, required_observers_preimage_cbor_data,
    required_signers_preimage_cbor_data, mint_preimage_cbor_data,
    script_integrity_hash_data, auxiliary_data_hash_data, network_id_data,
  ] = builtin.un_list_data(data)

  MidgardTransactionBody {
    inputs: inputs_preimage_cbor_data
      |> builtin.un_b_data
      |> decode_midgard_tx_inputs_preimage_cbor,
    reference_inputs: reference_inputs_preimage_cbor_data
      |> builtin.un_b_data
      |> decode_midgard_tx_inputs_preimage_cbor,
    outputs: outputs_preimage_cbor_data
      |> builtin.un_b_data
      |> decode_midgard_tx_outputs_preimage_cbor,
    fee: builtin.un_i_data(fee_data),
    validity_interval_start: builtin.un_i_data(validity_interval_start_data),
    validity_interval_end: builtin.un_i_data(validity_interval_end_data),
    required_observers: required_observers_preimage_cbor_data
      |> builtin.un_b_data
      |> decode_midgard_tx_byte_list_preimage_cbor,
    required_signers: required_signers_preimage_cbor_data
      |> builtin.un_b_data
      |> decode_midgard_tx_byte_list_preimage_cbor,
    mint: mint_preimage_cbor_data
      |> builtin.un_b_data
      |> decode_midgard_tx_mint_preimage_cbor,
    script_integrity_hash: builtin.un_b_data(script_integrity_hash_data),
    auxiliary_data_hash: builtin.un_b_data(auxiliary_data_hash_data),
    network_id: builtin.un_i_data(network_id_data),
  }
}

fn decode_midgard_transaction_witness_set_data(
  data: Data,
) -> MidgardTransactionWitnessSet {
  expect [
    addr_tx_wits_preimage_cbor_data, script_tx_wits_preimage_cbor_data,
    redeemer_tx_wits_preimage_cbor_data,
  ] = builtin.un_list_data(data)

  MidgardTransactionWitnessSet {
    addr_tx_wits: addr_tx_wits_preimage_cbor_data
      |> builtin.un_b_data
      |> decode_midgard_tx_address_witnesses_preimage_cbor,
    script_tx_wits: script_tx_wits_preimage_cbor_data
      |> builtin.un_b_data
      |> decode_midgard_tx_script_witnesses_preimage_cbor,
    redeemer_tx_wits: redeemer_tx_wits_preimage_cbor_data
      |> builtin.un_b_data
      |> decode_midgard_tx_redeemer_witnesses_preimage_cbor,
  }
}

fn decode_midgard_transaction_body_at(
  tx_cbor: ByteArray,
  offset: Int,
) -> Pair<Int, MidgardTransactionBody> {
  let Pair(offset, field_count) =
    decode_definite_array_header_at(tx_cbor, offset)
  expect field_count == 12

  let Pair(offset, inputs_preimage_cbor) =
    decode_definite_bytes_at(tx_cbor, offset)
  let Pair(offset, reference_inputs_preimage_cbor) =
    decode_definite_bytes_at(tx_cbor, offset)
  let Pair(offset, outputs_preimage_cbor) =
    decode_definite_bytes_at(tx_cbor, offset)
  let Pair(offset, fee) = decode_uint_at(tx_cbor, offset)
  let Pair(offset, validity_interval_start) = decode_int_at(tx_cbor, offset)
  let Pair(offset, validity_interval_end) = decode_int_at(tx_cbor, offset)
  let Pair(offset, required_observers_preimage_cbor) =
    decode_definite_bytes_at(tx_cbor, offset)
  let Pair(offset, required_signers_preimage_cbor) =
    decode_definite_bytes_at(tx_cbor, offset)
  let Pair(offset, mint_preimage_cbor) =
    decode_definite_bytes_at(tx_cbor, offset)
  let Pair(offset, script_integrity_hash) =
    decode_definite_bytes_at(tx_cbor, offset)
  let Pair(offset, auxiliary_data_hash) =
    decode_definite_bytes_at(tx_cbor, offset)
  let Pair(offset, network_id) = decode_uint_at(tx_cbor, offset)

  Pair(
    offset,
    MidgardTransactionBody {
      inputs: inputs_preimage_cbor |> decode_midgard_tx_inputs_preimage_cbor,
      reference_inputs: reference_inputs_preimage_cbor
        |> decode_midgard_tx_inputs_preimage_cbor,
      outputs: outputs_preimage_cbor |> decode_midgard_tx_outputs_preimage_cbor,
      fee,
      validity_interval_start,
      validity_interval_end,
      required_observers: required_observers_preimage_cbor
        |> decode_midgard_tx_byte_list_preimage_cbor,
      required_signers: required_signers_preimage_cbor
        |> decode_midgard_tx_byte_list_preimage_cbor,
      mint: mint_preimage_cbor |> decode_midgard_tx_mint_preimage_cbor,
      script_integrity_hash,
      auxiliary_data_hash,
      network_id,
    },
  )
}

fn decode_midgard_transaction_witness_set_at(
  tx_cbor: ByteArray,
  offset: Int,
) -> Pair<Int, MidgardTransactionWitnessSet> {
  let Pair(offset, field_count) =
    decode_definite_array_header_at(tx_cbor, offset)
  expect field_count == 3

  let Pair(offset, addr_tx_wits_preimage_cbor) =
    decode_definite_bytes_at(tx_cbor, offset)
  let Pair(offset, script_tx_wits_preimage_cbor) =
    decode_definite_bytes_at(tx_cbor, offset)
  let Pair(offset, redeemer_tx_wits_preimage_cbor) =
    decode_definite_bytes_at(tx_cbor, offset)

  Pair(
    offset,
    MidgardTransactionWitnessSet {
      addr_tx_wits: addr_tx_wits_preimage_cbor
        |> decode_midgard_tx_address_witnesses_preimage_cbor,
      script_tx_wits: script_tx_wits_preimage_cbor
        |> decode_midgard_tx_script_witnesses_preimage_cbor,
      redeemer_tx_wits: redeemer_tx_wits_preimage_cbor
        |> decode_midgard_tx_redeemer_witnesses_preimage_cbor,
    },
  )
}

pub fn decode_midgard_transaction(tx_cbor: ByteArray) -> MidgardTransaction {
  let Pair(offset, field_count) = decode_definite_array_header_at(tx_cbor, 0)
  expect field_count == 4
  let Pair(offset, version) = decode_uint_at(tx_cbor, offset)
  expect version == native_tx_version
  let Pair(offset, body) = decode_midgard_transaction_body_at(tx_cbor, offset)
  let Pair(offset, witness_set) =
    decode_midgard_transaction_witness_set_at(tx_cbor, offset)
  let Pair(offset, validity_code) = decode_uint_at(tx_cbor, offset)
  expect offset == bytearray.length(tx_cbor)

  MidgardTransaction {
    version,
    validity: validity_code |> validity_from_code,
    body,
    witness_set,
  }
}

pub fn verify_midgard_transaction(
  tx_id: ByteArray,
  tx_cbor: ByteArray,
) -> VerifiedMidgardTransaction {
  let tx = decode_midgard_transaction(tx_cbor)
  let compact = midgard_transaction_to_compact(tx)
  let compact_cbor = encode_native_tx_compact(compact)
  expect blake2b_256(encode_native_tx_body_compact(compact.body)) == tx_id

  VerifiedMidgardTransaction { tx_id, transaction: tx, compact, compact_cbor }
}

pub fn partial_view_from_compact(
  compact: VerifiedMidgardNativeTxCompact,
) -> MidgardTransactionPartialView {
  let body = compact.body
  MidgardTransactionPartialView {
    version: Some(compact.version),
    validity: Some(compact.validity),
    body: Some(
      MidgardTransactionBodyPartialView {
        inputs: None,
        reference_inputs: None,
        outputs: None,
        fee: Some(body.fee),
        validity_interval_start: Some(body.validity_interval_start),
        validity_interval_end: Some(body.validity_interval_end),
        required_observers: None,
        required_signers: None,
        mint: None,
        script_integrity_hash: Some(body.script_integrity_hash),
        auxiliary_data_hash: Some(body.auxiliary_data_hash),
        network_id: Some(body.network_id),
      },
    ),
    witness_set: None,
  }
}

fn decode_optional_inputs_preimage(
  preimage_cbor: Option<ByteArray>,
  expected_hash: ByteArray,
) -> Option<List<MidgardTxInput>> {
  when preimage_cbor is {
    Some(preimage_cbor) ->
      Some(decode_midgard_tx_inputs_preimage(preimage_cbor, expected_hash))
    None -> None
  }
}

fn decode_optional_outputs_preimage(
  preimage_cbor: Option<ByteArray>,
  expected_hash: ByteArray,
) -> Option<List<MidgardTxOutput>> {
  when preimage_cbor is {
    Some(preimage_cbor) ->
      Some(decode_midgard_tx_outputs_preimage(preimage_cbor, expected_hash))
    None -> None
  }
}

fn decode_optional_byte_list_preimage(
  preimage_cbor: Option<ByteArray>,
  expected_hash: ByteArray,
) -> Option<List<ByteArray>> {
  when preimage_cbor is {
    Some(preimage_cbor) ->
      Some(decode_midgard_tx_byte_list_preimage(preimage_cbor, expected_hash))
    None -> None
  }
}

fn decode_optional_mint_preimage(
  preimage_cbor: Option<ByteArray>,
  expected_hash: ByteArray,
) -> Option<Data> {
  when preimage_cbor is {
    Some(preimage_cbor) ->
      Some(decode_midgard_tx_mint_preimage(preimage_cbor, expected_hash))
    None -> None
  }
}

fn decode_optional_address_witnesses_preimage(
  preimage_cbor: Option<ByteArray>,
  expected_hash: ByteArray,
) -> Option<List<MidgardAddressWitness>> {
  when preimage_cbor is {
    Some(preimage_cbor) ->
      Some(
        decode_midgard_tx_address_witnesses_preimage(
          preimage_cbor,
          expected_hash,
        ),
      )
    None -> None
  }
}

fn decode_optional_script_witnesses_preimage(
  preimage_cbor: Option<ByteArray>,
  expected_hash: ByteArray,
) -> Option<List<MidgardScriptWitness>> {
  when preimage_cbor is {
    Some(preimage_cbor) ->
      Some(
        decode_midgard_tx_script_witnesses_preimage(
          preimage_cbor,
          expected_hash,
        ),
      )
    None -> None
  }
}

fn decode_optional_redeemer_witnesses_preimage(
  preimage_cbor: Option<ByteArray>,
  expected_hash: ByteArray,
) -> Option<List<MidgardRedeemerWitness>> {
  when preimage_cbor is {
    Some(preimage_cbor) ->
      Some(
        decode_midgard_tx_redeemer_witnesses_preimage(
          preimage_cbor,
          expected_hash,
        ),
      )
    None -> None
  }
}

pub fn partial_body_view_from_compact_and_preimages(
  body: NativeTxBodyCompact,
  preimages: MidgardTransactionBodyPartialPreimages,
) -> MidgardTransactionBodyPartialView {
  MidgardTransactionBodyPartialView {
    inputs: decode_optional_inputs_preimage(
      preimages.inputs,
      body.spend_inputs_hash,
    ),
    reference_inputs: decode_optional_inputs_preimage(
      preimages.reference_inputs,
      body.reference_inputs_hash,
    ),
    outputs: decode_optional_outputs_preimage(
      preimages.outputs,
      body.outputs_hash,
    ),
    fee: Some(body.fee),
    validity_interval_start: Some(body.validity_interval_start),
    validity_interval_end: Some(body.validity_interval_end),
    required_observers: decode_optional_byte_list_preimage(
      preimages.required_observers,
      body.required_observers_hash,
    ),
    required_signers: decode_optional_byte_list_preimage(
      preimages.required_signers,
      body.required_signers_hash,
    ),
    mint: decode_optional_mint_preimage(preimages.mint, body.mint_hash),
    script_integrity_hash: Some(body.script_integrity_hash),
    auxiliary_data_hash: Some(body.auxiliary_data_hash),
    network_id: Some(body.network_id),
  }
}

pub fn partial_witness_set_view_from_compact_and_preimages(
  witness_set: NativeTxWitnessSetCompact,
  preimages: MidgardTransactionWitnessSetPartialPreimages,
) -> MidgardTransactionWitnessSetPartialView {
  MidgardTransactionWitnessSetPartialView {
    addr_tx_wits: decode_optional_address_witnesses_preimage(
      preimages.addr_tx_wits,
      witness_set.addr_tx_wits_hash,
    ),
    script_tx_wits: decode_optional_script_witnesses_preimage(
      preimages.script_tx_wits,
      witness_set.script_tx_wits_hash,
    ),
    redeemer_tx_wits: decode_optional_redeemer_witnesses_preimage(
      preimages.redeemer_tx_wits,
      witness_set.redeemer_tx_wits_hash,
    ),
  }
}

pub fn partial_view_from_compact_and_preimages(
  compact: VerifiedMidgardNativeTxCompact,
  body_preimages: MidgardTransactionBodyPartialPreimages,
  witness_set: Option<NativeTxWitnessSetCompact>,
  witness_set_preimages: MidgardTransactionWitnessSetPartialPreimages,
) -> MidgardTransactionPartialView {
  let witness_set_view =
    when witness_set is {
      Some(witness_set) -> {
        expect
          blake2b_256(encode_native_tx_witness_set_compact(witness_set)) == compact.witness_set_hash
        Some(
          partial_witness_set_view_from_compact_and_preimages(
            witness_set,
            witness_set_preimages,
          ),
        )
      }
      None -> None
    }

  MidgardTransactionPartialView {
    version: Some(compact.version),
    validity: Some(compact.validity),
    body: Some(
      partial_body_view_from_compact_and_preimages(compact.body, body_preimages),
    ),
    witness_set: witness_set_view,
  }
}
