use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/fuzz
use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/codec.{encode_native_byte_list}
use midgard/fraud_proofs/native_tx/compact.{
  decode_native_tx_compact_v1, encode_native_tx_body_compact,
  encode_native_tx_compact_v1, verify_native_tx_compact_cbor_v1,
  verify_native_tx_compact_v1,
}
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_tx_output_cbor, encode_midgard_tx_output,
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
  encode_script_witness_preimage, spend_input_cbor_at,
  verify_spend_inputs_preimage,
}
use midgard/fraud_proofs/native_tx/transaction.{
  decode_midgard_transaction_v1, encode_midgard_transaction_body_full,
  encode_midgard_transaction_v1, encode_midgard_transaction_witness_set_full,
  midgard_transaction_to_compact, midgard_transaction_witness_set_to_compact,
  partial_view_from_compact_and_preimages, verify_midgard_transaction_v1,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddress, MidgardAddressWitness, MidgardExecutionUnits,
  MidgardRedeemerWitness, MidgardTransaction, MidgardTransactionBody,
  MidgardTransactionBodyPartialPreimages, MidgardTransactionWitnessSet,
  MidgardTransactionWitnessSetPartialPreimages, MidgardTxInput, MidgardTxOutput,
  MidgardValue, MidgardVersionedScript, NativeTxBodyCompact, NativeTxCompact,
  PlutusV3Script, PubKeyCredential, RewardRedeemer, SpendRedeemer, TxIsValid,
  native_tx_version_v1,
}

const golden_core_native_full_tx_cbor =
  #"84018c58278158248258201111111111111111111111111111111111111111111111111111111111111111005827815824825820222222222222222222222222222222222222222222222222222222222222222201585081584da200581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01821a001e8480a1581c7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7aa1434d494405182a2018644180418041805820333333333333333333333333333333333333333333333333333333333333333358204444444444444444444444444444444444444444444444444444444444444444008341804180418000"

const golden_core_native_compact_tx_cbor =
  #"84018c582044bc7621c0c1b05789bfd7fae817a582b32ce0d7d357d1cb717e0512cf32eeb65820e190369d719bcefc386ac4f56886a69d3f66ff8bb7f6ce1bc01defafc7164513582090c0662cc2568cf510b3d965486a0a7a4494e811045b237c945158b3e14d16fb182a201864582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582033333333333333333333333333333333333333333333333333333333333333335820444444444444444444444444444444444444444444444444444444444444444400582052408b440567f475c3eee21c9a8be91ed05a31e81e175c69fcaec5ccd187258c00"

const golden_core_native_tx_id =
  #"ea7685b2e05809b8aa190df6cc9699f327a2e61db73ea6cd7c54309d0a8a7cd8"

const golden_core_spend_inputs_preimage_cbor =
  #"815824825820111111111111111111111111111111111111111111111111111111111111111100"

const golden_core_reference_inputs_preimage_cbor =
  #"815824825820222222222222222222222222222222222222222222222222222222222222222201"

const golden_core_outputs_preimage_cbor =
  #"81584da200581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01821a001e8480a1581c7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7aa1434d494405"

const golden_core_empty_preimage_cbor = #"80"

fn sample_golden_core_native_full_tx_cbor(_size: Int) -> Fuzzer<ByteArray> {
  fuzz.constant(golden_core_native_full_tx_cbor)
}

fn sample_golden_core_native_compact_tx_cbor(_size: Int) -> Fuzzer<ByteArray> {
  fuzz.constant(golden_core_native_compact_tx_cbor)
}

fn sample_golden_core_native_compact_fixture(
  _size: Int,
) -> Fuzzer<Pair<NativeTxCompact, ByteArray>> {
  fuzz.constant(
    Pair(
      decode_native_tx_compact_v1(golden_core_native_compact_tx_cbor),
      golden_core_native_compact_tx_cbor,
    ),
  )
}

fn sample_golden_core_spend_inputs_preimage_cbor(
  _size: Int,
) -> Fuzzer<ByteArray> {
  fuzz.constant(golden_core_spend_inputs_preimage_cbor)
}

fn sample_golden_core_reference_inputs_preimage_cbor(
  _size: Int,
) -> Fuzzer<ByteArray> {
  fuzz.constant(golden_core_reference_inputs_preimage_cbor)
}

fn sample_golden_core_outputs_preimage_cbor(_size: Int) -> Fuzzer<ByteArray> {
  fuzz.constant(golden_core_outputs_preimage_cbor)
}

fn sample_golden_core_empty_preimage_cbor(_size: Int) -> Fuzzer<ByteArray> {
  fuzz.constant(golden_core_empty_preimage_cbor)
}

bench golden_core_decode_midgard_transaction_bench(
  tx_cbor: ByteArray via sample_golden_core_native_full_tx_cbor,
) {
  let decoded = decode_midgard_transaction_v1(tx_cbor)
  and {
    decoded.version == native_tx_version_v1,
    list.length(decoded.body.inputs) == 1,
    list.length(decoded.body.reference_inputs) == 1,
    list.length(decoded.body.outputs) == 1,
  }
}

bench golden_core_decode_native_tx_compact_bench(
  compact_cbor: ByteArray via sample_golden_core_native_compact_tx_cbor,
) {
  let compact = decode_native_tx_compact_v1(compact_cbor)
  compact.validity_code == 0
}

bench golden_core_verify_native_tx_compact_reencode_bench(
  fixture: Pair<NativeTxCompact, ByteArray> via sample_golden_core_native_compact_fixture,
) {
  let Pair(compact, compact_cbor) = fixture
  let verified = verify_native_tx_compact_v1(
    golden_core_native_tx_id,
    compact,
    compact_cbor,
  )
  verified.tx_compact.validity_code == 0
}

bench golden_core_verify_native_tx_compact_cbor_bench(
  compact_cbor: ByteArray via sample_golden_core_native_compact_tx_cbor,
) {
  let verified = verify_native_tx_compact_cbor_v1(
    golden_core_native_tx_id,
    compact_cbor,
  )
  verified.tx_compact.validity_code == 0
}

bench golden_core_decode_then_verify_native_tx_compact_reencode_bench(
  compact_cbor: ByteArray via sample_golden_core_native_compact_tx_cbor,
) {
  let compact = decode_native_tx_compact_v1(compact_cbor)
  let verified = verify_native_tx_compact_v1(
    golden_core_native_tx_id,
    compact,
    compact_cbor,
  )
  verified.tx_compact.validity_code == 0
}

bench golden_core_decode_spend_inputs_preimage_bench(
  preimage_cbor: ByteArray via sample_golden_core_spend_inputs_preimage_cbor,
) {
  list.length(decode_midgard_tx_inputs_preimage_cbor(preimage_cbor)) == 1
}

bench golden_core_decode_reference_inputs_preimage_bench(
  preimage_cbor: ByteArray via sample_golden_core_reference_inputs_preimage_cbor,
) {
  list.length(decode_midgard_tx_inputs_preimage_cbor(preimage_cbor)) == 1
}

bench golden_core_decode_outputs_preimage_bench(
  preimage_cbor: ByteArray via sample_golden_core_outputs_preimage_cbor,
) {
  list.length(decode_midgard_tx_outputs_preimage_cbor(preimage_cbor)) == 1
}

bench golden_core_decode_empty_mint_preimage_bench(
  preimage_cbor: ByteArray via sample_golden_core_empty_preimage_cbor,
) {
  let empty_mint: Data = []
  decode_midgard_tx_mint_preimage_cbor(preimage_cbor) == empty_mint
}

bench golden_core_decode_empty_address_witnesses_preimage_bench(
  preimage_cbor: ByteArray via sample_golden_core_empty_preimage_cbor,
) {
  list.length(decode_midgard_tx_address_witnesses_preimage_cbor(preimage_cbor)) == 0
}

bench golden_core_decode_empty_script_witnesses_preimage_bench(
  preimage_cbor: ByteArray via sample_golden_core_empty_preimage_cbor,
) {
  list.length(decode_midgard_tx_script_witnesses_preimage_cbor(preimage_cbor)) == 0
}

bench golden_core_decode_empty_redeemer_witnesses_preimage_bench(
  preimage_cbor: ByteArray via sample_golden_core_empty_preimage_cbor,
) {
  list.length(decode_midgard_tx_redeemer_witnesses_preimage_cbor(preimage_cbor)) == 0
}

test encode_native_tx_compact_matches_node_cbor_shape() {
  let body = NativeTxBodyCompact {
    spend_inputs_hash: #"0101010101010101010101010101010101010101010101010101010101010101",
    reference_inputs_hash: #"0202020202020202020202020202020202020202020202020202020202020202",
    outputs_hash: #"0303030303030303030303030303030303030303030303030303030303030303",
    fee: 0,
    validity_interval_start: -1,
    validity_interval_end: -1,
    required_observers_hash: #"0404040404040404040404040404040404040404040404040404040404040404",
    required_signers_hash: #"0505050505050505050505050505050505050505050505050505050505050505",
    mint_hash: #"0606060606060606060606060606060606060606060606060606060606060606",
    script_integrity_hash: #"0707070707070707070707070707070707070707070707070707070707070707",
    auxiliary_data_hash: #"0808080808080808080808080808080808080808080808080808080808080808",
    network_id: 0,
  }
  let body_cbor = #"8c582001010101010101010101010101010101010101010101010101010101010101015820020202020202020202020202020202020202020202020202020202020202020258200303030303030303030303030303030303030303030303030303030303030303002020582004040404040404040404040404040404040404040404040404040404040404045820050505050505050505050505050505050505050505050505050505050505050558200606060606060606060606060606060606060606060606060606060606060606582007070707070707070707070707070707070707070707070707070707070707075820080808080808080808080808080808080808080808080808080808080808080800"
  let compact = NativeTxCompact {
    body,
    witness_set_hash: #"0909090909090909090909090909090909090909090909090909090909090909",
    validity_code: 0,
  }
  let compact_cbor = #"8401"
    |> bytearray.concat(body_cbor)
    |> bytearray.concat(
        #"5820090909090909090909090909090909090909090909090909090909090909090900",
      )
  let view = verify_native_tx_compact_v1(
    blake2b_256(body_cbor),
    compact,
    compact_cbor,
  )
  and {
    encode_native_tx_body_compact(body) == body_cbor,
    encode_native_tx_compact_v1(compact) == compact_cbor,
    view.tx_id == blake2b_256(body_cbor),
    view.tx_compact.body.spend_inputs_hash == #"0101010101010101010101010101010101010101010101010101010101010101",
    encode_native_tx_compact_v1(view.tx_compact) == compact_cbor,
  }
}

test canonical_redeemer_bridge_golden_vector_is_stable() {
  let golden_redeemers_cbor = #"8284000043d8798082050784030142182a820b0d"
  let decoded = decode_midgard_tx_redeemer_witnesses_preimage_cbor(
    golden_redeemers_cbor,
  )
  and {
    decoded == [
      MidgardRedeemerWitness {
        purpose: SpendRedeemer,
        index: 0,
        redeemer_cbor: #"d87980",
        execution_units: MidgardExecutionUnits { memory: 5, steps: 7 },
      },
      MidgardRedeemerWitness {
        purpose: RewardRedeemer,
        index: 1,
        redeemer_cbor: #"182a",
        execution_units: MidgardExecutionUnits { memory: 11, steps: 13 },
      },
    ],
    encode_redeemer_witness_preimage(decoded) == golden_redeemers_cbor,
  }
}

test midgard_transaction_full_decode_round_trip() {
  let empty_mint: Data = []
  let pub_key_hash = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  let tx_1_id = #"1111111111111111111111111111111111111111111111111111111111111111"
  let tx_2_id = #"2222222222222222222222222222222222222222222222222222222222222222"
  let output = MidgardTxOutput {
    address: MidgardAddress {
      protected: False,
      network_id: 0,
      payment_credential: PubKeyCredential(pub_key_hash),
      stake_credential: None,
    },
    value: MidgardValue { lovelace: 2_000_000, assets: [] },
    datum_cbor: None,
    script_ref: None,
  }
  let tx = MidgardTransaction {
    version: native_tx_version_v1,
    validity: TxIsValid,
    body: MidgardTransactionBody {
      inputs: [MidgardTxInput { tx_id: tx_1_id, output_index: 0 }],
      reference_inputs: [MidgardTxInput { tx_id: tx_2_id, output_index: 1 }],
      outputs: [output],
      fee: 42,
      validity_interval_start: -1,
      validity_interval_end: 100,
      required_observers: [
        #"01010101010101010101010101010101010101010101010101010101",
      ],
      required_signers: [
        #"02020202020202020202020202020202020202020202020202020202",
      ],
      mint: empty_mint,
      script_integrity_hash: #"0303030303030303030303030303030303030303030303030303030303030303",
      auxiliary_data_hash: #"0404040404040404040404040404040404040404040404040404040404040404",
      network_id: 0,
    },
    witness_set: MidgardTransactionWitnessSet {
      addr_tx_wits: [
        MidgardAddressWitness {
          verification_key: #"1010101010101010101010101010101010101010101010101010101010101010",
          signature: #"20202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020",
        },
      ],
      script_tx_wits: [
        MidgardVersionedScript {
          language: PlutusV3Script,
          script_bytes: #"010203",
        },
      ],
      redeemer_tx_wits: [
        MidgardRedeemerWitness {
          purpose: SpendRedeemer,
          index: 0,
          redeemer_cbor: cbor.serialise(builtin.i_data(42)),
          execution_units: MidgardExecutionUnits { memory: 1, steps: 2 },
        },
      ],
    },
  }
  let tx_cbor = encode_midgard_transaction_v1(tx)
  let decoded = decode_midgard_transaction_v1(tx_cbor)
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = compact.body |> encode_native_tx_body_compact |> blake2b_256
  let expected_tx_cbor = #"84"
    |> bytearray.concat(cbor.serialise(native_tx_version_v1))
    |> bytearray.concat(encode_midgard_transaction_body_full(tx.body))
    |> bytearray.concat(
        encode_midgard_transaction_witness_set_full(tx.witness_set),
      )
    |> bytearray.concat(cbor.serialise(0))
  let verified_tx = verify_midgard_transaction_v1(tx_id, tx_cbor)
  let body_preimages = MidgardTransactionBodyPartialPreimages {
    inputs: Some(encode_input_preimage(tx.body.inputs)),
    reference_inputs: Some(encode_input_preimage(tx.body.reference_inputs)),
    outputs: Some(encode_output_preimage(tx.body.outputs)),
    required_observers: Some(
      encode_native_byte_list(tx.body.required_observers),
    ),
    required_signers: Some(encode_native_byte_list(tx.body.required_signers)),
    mint: Some(encode_mint_preimage(tx.body.mint)),
  }
  let witness_set_compact = midgard_transaction_witness_set_to_compact(
    tx.witness_set,
  )
  let witness_set_preimages = MidgardTransactionWitnessSetPartialPreimages {
    addr_tx_wits: Some(
      encode_address_witness_preimage(tx.witness_set.addr_tx_wits),
    ),
    script_tx_wits: Some(
      encode_script_witness_preimage(tx.witness_set.script_tx_wits),
    ),
    redeemer_tx_wits: Some(
      encode_redeemer_witness_preimage(tx.witness_set.redeemer_tx_wits),
    ),
  }
  let verified = verify_native_tx_compact_v1(
    tx_id,
    compact,
    encode_native_tx_compact_v1(compact),
  )
  let partial = partial_view_from_compact_and_preimages(
    verified,
    body_preimages,
    Some(witness_set_compact),
    witness_set_preimages,
  )
  expect Some(partial_body) = partial.body
  expect Some(partial_witness_set) = partial.witness_set

  and {
    tx_cbor == expected_tx_cbor,
    decoded == tx,
    encode_midgard_transaction_v1(decoded) == tx_cbor,
    verified_tx.transaction == tx,
    verified_tx.compact == compact,
    verified_tx.compact_cbor == encode_native_tx_compact_v1(compact),
    partial.version == Some(native_tx_version_v1),
    partial.validity == Some(TxIsValid),
    partial_body.inputs == Some(tx.body.inputs),
    partial_body.reference_inputs == Some(tx.body.reference_inputs),
    partial_body.outputs == Some(tx.body.outputs),
    partial_body.required_observers == Some(tx.body.required_observers),
    partial_body.required_signers == Some(tx.body.required_signers),
    partial_body.mint == Some(tx.body.mint),
    partial_witness_set.addr_tx_wits == Some(tx.witness_set.addr_tx_wits),
    partial_witness_set.script_tx_wits == Some(tx.witness_set.script_tx_wits),
    partial_witness_set.redeemer_tx_wits == Some(
      tx.witness_set.redeemer_tx_wits,
    ),
  }
}

test decode_midgard_value_preserves_unsorted_asset_order() {
  let policy_id = #"01010101010101010101010101010101010101010101010101010101"
  let output_cbor = #"a200581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a1581c"
    |> bytearray.concat(policy_id)
    |> bytearray.concat(#"a2410201410102")
  let output = decode_midgard_tx_output_cbor(output_cbor)

  and {
    output.value.lovelace == 0,
    output.value.assets == [
      Pair(policy_id |> bytearray.concat(#"02"), 1),
      Pair(policy_id |> bytearray.concat(#"01"), 2),
    ],
    encode_midgard_tx_output(output) == output_cbor,
  }
}

test golden_midgard_core_native_full_tx_decodes() {
  let tx_cbor = #"84018c58278158248258201111111111111111111111111111111111111111111111111111111111111111005827815824825820222222222222222222222222222222222222222222222222222222222222222201585081584da200581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01821a001e8480a1581c7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7aa1434d494405182a2018644180418041805820333333333333333333333333333333333333333333333333333333333333333358204444444444444444444444444444444444444444444444444444444444444444008341804180418000"
  let compact_cbor = #"84018c582044bc7621c0c1b05789bfd7fae817a582b32ce0d7d357d1cb717e0512cf32eeb65820e190369d719bcefc386ac4f56886a69d3f66ff8bb7f6ce1bc01defafc7164513582090c0662cc2568cf510b3d965486a0a7a4494e811045b237c945158b3e14d16fb182a201864582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582033333333333333333333333333333333333333333333333333333333333333335820444444444444444444444444444444444444444444444444444444444444444400582052408b440567f475c3eee21c9a8be91ed05a31e81e175c69fcaec5ccd187258c00"
  let tx_id = #"ea7685b2e05809b8aa190df6cc9699f327a2e61db73ea6cd7c54309d0a8a7cd8"
  let decoded = decode_midgard_transaction_v1(tx_cbor)
  expect [input] = decoded.body.inputs
  expect [reference_input] = decoded.body.reference_inputs
  expect [output] = decoded.body.outputs
  expect [Pair(asset_unit, quantity)] = output.value.assets
  let compact = midgard_transaction_to_compact(decoded)

  and {
    decoded.version == native_tx_version_v1,
    decoded.validity == TxIsValid,
    input.tx_id == #"1111111111111111111111111111111111111111111111111111111111111111",
    input.output_index == 0,
    reference_input.tx_id == #"2222222222222222222222222222222222222222222222222222222222222222",
    reference_input.output_index == 1,
    output.address.payment_credential == PubKeyCredential(
      #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    ),
    output.value.lovelace == 2_000_000,
    asset_unit == #"7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a4d4944",
    quantity == 5,
    compact.body.spend_inputs_hash == #"44bc7621c0c1b05789bfd7fae817a582b32ce0d7d357d1cb717e0512cf32eeb6",
    compact.body.reference_inputs_hash == #"e190369d719bcefc386ac4f56886a69d3f66ff8bb7f6ce1bc01defafc7164513",
    compact.body.outputs_hash == #"90c0662cc2568cf510b3d965486a0a7a4494e811045b237c945158b3e14d16fb",
    compact.witness_set_hash == #"52408b440567f475c3eee21c9a8be91ed05a31e81e175c69fcaec5ccd187258c",
    encode_native_tx_compact_v1(compact) == compact_cbor,
    blake2b_256(encode_native_tx_body_compact(compact.body)) == tx_id,
  }
}

test golden_midgard_core_native_preimage_cbor_decoders() {
  let spend_preimage_cbor = #"815824825820111111111111111111111111111111111111111111111111111111111111111100"
  let reference_preimage_cbor = #"815824825820222222222222222222222222222222222222222222222222222222222222222201"
  let outputs_preimage_cbor = #"81584da200581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01821a001e8480a1581c7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7aa1434d494405"
  let empty_preimage_cbor = #"80"
  let empty_preimage_hash = #"45b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0"
  let empty_mint: Data = []

  expect [spend_input] = decode_midgard_tx_inputs_preimage_cbor(
    spend_preimage_cbor,
  )
  expect [reference_input] = decode_midgard_tx_inputs_preimage_cbor(
    reference_preimage_cbor,
  )
  expect [output] = decode_midgard_tx_outputs_preimage_cbor(
    outputs_preimage_cbor,
  )
  expect [Pair(asset_unit, quantity)] = output.value.assets

  and {
    blake2b_256(spend_preimage_cbor) == #"44bc7621c0c1b05789bfd7fae817a582b32ce0d7d357d1cb717e0512cf32eeb6",
    spend_input.tx_id == #"1111111111111111111111111111111111111111111111111111111111111111",
    spend_input.output_index == 0,
    blake2b_256(reference_preimage_cbor) == #"e190369d719bcefc386ac4f56886a69d3f66ff8bb7f6ce1bc01defafc7164513",
    reference_input.tx_id == #"2222222222222222222222222222222222222222222222222222222222222222",
    reference_input.output_index == 1,
    output.value.lovelace == 2_000_000,
    asset_unit == #"7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a4d4944",
    quantity == 5,
    decode_midgard_tx_byte_list_preimage_cbor(empty_preimage_cbor) == [],
    blake2b_256(empty_preimage_cbor) == empty_preimage_hash,
    decode_midgard_tx_mint_preimage_cbor(empty_preimage_cbor) == empty_mint,
    decode_midgard_tx_address_witnesses_preimage_cbor(empty_preimage_cbor) == [],
    decode_midgard_tx_script_witnesses_preimage_cbor(empty_preimage_cbor) == [],
    decode_midgard_tx_redeemer_witnesses_preimage_cbor(empty_preimage_cbor) == [],
  }
}

test golden_midgard_core_native_compact_tx_decodes() {
  let compact_cbor = #"84018c582044bc7621c0c1b05789bfd7fae817a582b32ce0d7d357d1cb717e0512cf32eeb65820e190369d719bcefc386ac4f56886a69d3f66ff8bb7f6ce1bc01defafc7164513582090c0662cc2568cf510b3d965486a0a7a4494e811045b237c945158b3e14d16fb182a201864582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582033333333333333333333333333333333333333333333333333333333333333335820444444444444444444444444444444444444444444444444444444444444444400582052408b440567f475c3eee21c9a8be91ed05a31e81e175c69fcaec5ccd187258c00"
  let tx_id = golden_core_native_tx_id
  let compact = decode_native_tx_compact_v1(compact_cbor)
  let verified = verify_native_tx_compact_v1(tx_id, compact, compact_cbor)
  let verified_from_cbor = verify_native_tx_compact_cbor_v1(tx_id, compact_cbor)

  and {
    verified_from_cbor == verified,
    verified.tx_compact.validity_code == 0,
    verified.tx_compact.body.spend_inputs_hash == #"44bc7621c0c1b05789bfd7fae817a582b32ce0d7d357d1cb717e0512cf32eeb6",
    verified.tx_compact.body.reference_inputs_hash == #"e190369d719bcefc386ac4f56886a69d3f66ff8bb7f6ce1bc01defafc7164513",
    verified.tx_compact.body.outputs_hash == #"90c0662cc2568cf510b3d965486a0a7a4494e811045b237c945158b3e14d16fb",
    verified.tx_compact.witness_set_hash == #"52408b440567f475c3eee21c9a8be91ed05a31e81e175c69fcaec5ccd187258c",
  }
}

test encode_native_byte_list_matches_node_preimage_shape() {
  let input_0 = #"1111"
  let input_1 = #"222222"
  let spend_input_cbors = [input_0, input_1]
  let preimage_cbor = #"8242"
    |> bytearray.concat(input_0)
    |> bytearray.concat(#"43")
    |> bytearray.concat(input_1)

  and {
    encode_native_byte_list(spend_input_cbors) == preimage_cbor,
    verify_spend_inputs_preimage(spend_input_cbors, blake2b_256(preimage_cbor)),
    spend_input_cbor_at(spend_input_cbors, 1) == input_1,
  }
}
