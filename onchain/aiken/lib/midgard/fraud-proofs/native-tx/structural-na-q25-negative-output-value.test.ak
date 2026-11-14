//// Executable structural-N/A closure for GOAL_SPEC §9.3 row Q25
//// (`negative-output-value`).
////
//// The claim under test: in canonical V1 an output value cannot carry a
//// negative lovelace amount, nor an asset quantity that is zero or negative.
//// The byte-level decoder reads both through unsigned CBOR readers and then
//// re-checks the sign, and the total (`Option`-returning) ledger decoder
//// yields `None`. Each violating vector below differs from its positive
//// control by exactly one byte.
////
//// Rejection sites:
////   * lib/midgard/fraud-proofs/native-tx/components.ak:253
////       `expect quantity > 0` in `decode_midgard_asset_quantities`
////       (Plutus-`Data` output path)
////   * lib/midgard/fraud-proofs/native-tx/components.ak:373
////       `expect value.lovelace >= 0` in `encode_midgard_value`
////   * lib/midgard/fraud-proofs/native-tx/components.ak:382
////       `expect lovelace >= 0` in `decode_midgard_value_data`
////   * lib/midgard/fraud-proofs/native-tx/components.ak:410-411
////       `decode_uint_at` + `expect quantity > 0` in
////       `decode_midgard_asset_quantities_at` (raw-CBOR output path)
////   * lib/midgard/ledger-output-v1.ak:129 `quantity > 0` in
////       `parse_asset_quantities`
////   * lib/midgard/ledger-output-v1.ak:231 `if lovelace >= 0` in
////       `parse_value`

use aiken/cbor
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_tx_output_cbor, decode_midgard_tx_output_data,
  encode_midgard_value,
}
use midgard/fraud_proofs/native_tx/types.{MidgardTxOutput, MidgardValue}
use midgard/ledger_output_v1

// Common prefix of every vector below:
//   a2                 map(2)
//   00                 key 0 (address)
//   581d 60 aa*28      bytes(29) = enterprise address, pubkey credential
//   01                 key 1 (value)
// The value item then follows and is the only thing that varies.

/// value = [-1, {}] — `8220a0`, where `20` is CBOR negative integer -1.
const negative_lovelace_output: ByteArray =
  #"a200581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018220a0"

/// Positive control: the same bytes with `20` corrected to `00` (lovelace 0).
const zero_lovelace_output: ByteArray =
  #"a200581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a0"

/// value = [0, { h'0101..'(28) : { h'01' : 0 } }] — trailing `00` is qty 0.
const zero_quantity_output: ByteArray =
  #"a200581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a1581c01010101010101010101010101010101010101010101010101010101a1410100"

/// Same vector with the trailing byte set to `20` (asset quantity -1).
const negative_quantity_output: ByteArray =
  #"a200581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a1581c01010101010101010101010101010101010101010101010101010101a1410120"

/// Positive control: the same vector with the trailing byte set to `01`.
const positive_quantity_output: ByteArray =
  #"a200581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a1581c01010101010101010101010101010101010101010101010101010101a1410101"

fn as_data(bytes: ByteArray) -> Data {
  expect Some(data) = cbor.deserialise(bytes)
  data
}

// --- negative lovelace -------------------------------------------------------

test q25_negative_lovelace_rejected_by_raw_cbor_output_decoder() fail {
  decode_midgard_tx_output_cbor(negative_lovelace_output).value.lovelace < 0
}

test q25_negative_lovelace_rejected_by_data_output_decoder() fail {
  decode_midgard_tx_output_data(as_data(negative_lovelace_output)).value.lovelace < 0
}

test q25_negative_lovelace_rejected_by_ledger_output_v1() {
  ledger_output_v1.decode_canonical_output(negative_lovelace_output) == None
}

test q25_negative_lovelace_rejected_by_canonical_value_encoder() fail {
  encode_midgard_value(MidgardValue { lovelace: -1, assets: [] }) != #""
}

// --- zero / negative asset quantity ------------------------------------------

test q25_zero_asset_quantity_rejected_by_raw_cbor_output_decoder() fail {
  decode_midgard_tx_output_cbor(zero_quantity_output).value.lovelace == 0
}

test q25_zero_asset_quantity_rejected_by_data_output_decoder() fail {
  decode_midgard_tx_output_data(as_data(zero_quantity_output)).value.lovelace == 0
}

test q25_negative_asset_quantity_rejected_by_raw_cbor_output_decoder() fail {
  decode_midgard_tx_output_cbor(negative_quantity_output).value.lovelace == 0
}

test q25_negative_asset_quantity_rejected_by_data_output_decoder() fail {
  decode_midgard_tx_output_data(as_data(negative_quantity_output)).value.lovelace == 0
}

test q25_nonpositive_asset_quantities_rejected_by_ledger_output_v1() {
  and {
    ledger_output_v1.decode_canonical_output(zero_quantity_output) == None,
    ledger_output_v1.decode_canonical_output(negative_quantity_output) == None,
  }
}

test q25_negative_asset_quantity_rejected_by_canonical_value_encoder() fail {
  encode_midgard_value(
    MidgardValue {
      lovelace: 0,
      assets: [
        Pair(#"0101010101010101010101010101010101010101010101010101010101", -1),
      ],
    },
  ) != #""
}

// --- adjacent positive controls ----------------------------------------------

test q25_positive_control_zero_lovelace_output_decodes() {
  let output = decode_midgard_tx_output_cbor(zero_lovelace_output)
  and {
    output.value.lovelace == 0,
    output.value.assets == [],
    decode_midgard_tx_output_data(as_data(zero_lovelace_output)) == output,
    ledger_output_v1.decode_canonical_output(zero_lovelace_output) == Some(
      output,
    ),
  }
}

test q25_positive_control_positive_asset_quantity_output_decodes() {
  let output = decode_midgard_tx_output_cbor(positive_quantity_output)
  and {
    output.value.lovelace == 0,
    output.value.assets == [
      Pair(#"0101010101010101010101010101010101010101010101010101010101", 1),
    ],
    decode_midgard_tx_output_data(as_data(positive_quantity_output)) == output,
    ledger_output_v1.decode_canonical_output(positive_quantity_output) == Some(
      output,
    ),
  }
}

test q25_positive_control_reencodes_to_the_same_bytes() {
  let output: MidgardTxOutput = decode_midgard_tx_output_cbor(
    positive_quantity_output,
  )
  encode_midgard_value(output.value) == #"8200a1581c01010101010101010101010101010101010101010101010101010101a1410101"
}
