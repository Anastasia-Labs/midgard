use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_output}
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddress, MidgardTxOutput, MidgardV1Script, MidgardValue,
  MidgardVersionedScript, PubKeyCredential,
}
use midgard/ledger_output_v1

const pub_key_hash = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

fn simple_output() -> MidgardTxOutput {
  MidgardTxOutput {
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
}

test canonical_ledger_output_v1_round_trips() {
  let output = simple_output()
  ledger_output_v1.decode_canonical_output(encode_midgard_tx_output(output)) == Some(
    output,
  )
}

test canonical_ledger_output_v1_supports_datum_and_reference_script() {
  let base = simple_output()
  let output = MidgardTxOutput {
    ..base,
    datum_cbor: Some(#"d87980"),
    script_ref: Some(
      MidgardVersionedScript {
        language: MidgardV1Script,
        script_bytes: #"010203",
      },
    ),
  }
  ledger_output_v1.decode_canonical_output(encode_midgard_tx_output(output)) == Some(
    output,
  )
}

test ledger_output_v1_rejects_malformed_cbor_without_aborting() {
  and {
    ledger_output_v1.decode_canonical_output(#"") == None,
    ledger_output_v1.decode_canonical_output(#"a100") == None,
    ledger_output_v1.decode_canonical_output(#"bf00ff") == None,
  }
}

test ledger_output_v1_rejects_unsorted_asset_names() {
  let policy_id = #"01010101010101010101010101010101010101010101010101010101"
  let output_cbor = #"a200581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a1581c"
    |> bytearray.concat(policy_id)
    |> bytearray.concat(#"a2410201410102")
  ledger_output_v1.decode_canonical_output(output_cbor) == None
}

test ledger_output_v1_rejects_noncanonical_embedded_datum() {
  // 0x18 0x17 is a non-minimal encoding of integer 23.
  let output_cbor = #"a300581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a002421817"
  ledger_output_v1.decode_canonical_output(output_cbor) == None
}

test ledger_output_v1_rejects_unknown_reference_script_language() {
  let output_cbor = #"a300581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a00382184140"
  ledger_output_v1.decode_canonical_output(output_cbor) == None
}
