use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray
use cardano/address.{Credential}
use cardano/transaction.{Transaction}
use midgard/fraud_proofs/native_tx/types.{
  MidgardTxOutput, MidgardV1Script, MidgardValue, MidgardVersionedScript,
  NativeCardanoScript, PlutusV3Script,
}
use midgard/fraud_proofs/structured_data_carriage as carriage
use midgard/fraud_proofs/transition_trace/final_yield as delegation
use midgard/ledger_output_scan_v1 as scan
use midgard/ledger_output_v1
use midgard/script_context_v1

// Serialization equality below proves canonical bytes. Preserve the original
// stdlib decoder's materializable domain without rescanning long byte strings.
fn materialisable(data: Data) -> Bool {
  if builtin.choose_data(data, True, False, False, False, False) {
    let Pair(tag, fields) = Pair(
      builtin.unconstr_index(data),
      builtin.unconstr_fields(data),
    )
    tag >= 0 && tag < 128 && list.all(fields, materialisable)
  } else if builtin.choose_data(data, False, True, False, False, False) {
    list.all(
      builtin.un_map_data(data),
      fn(pair) { materialisable(pair.1st) && materialisable(pair.2nd) },
    )
  } else if builtin.choose_data(data, False, False, True, False, False) {
    list.all(builtin.un_list_data(data), materialisable)
  } else if builtin.choose_data(data, False, False, False, True, False) {
    let value = builtin.un_i_data(data)
    value >= -18446744073709551616 && value <= 18446744073709551615
  } else {
    True
  }
}

validator summaries(dispatcher: ScriptHash) {
  withdraw(
    evidence: (delegation.OutputSummaries, carriage.Evidence),
    _credential: Credential,
    tx: Transaction,
  ) {
    let (datum, args) = delegation.dispatch(dispatcher, tx.inputs, tx.redeemers)
    expect Some(state) = datum.data
    expect state.phase == 8 && state.kind != 2
    let (committed_summaries, raw_datum) = evidence
    let bytes = delegation.output_bytes(state, args, tx.reference_inputs)
    let facts = scan.decode_control_v1(state.scan_cbor)
    expect Some(address) = ledger_output_v1.decode_canonical_address_bytes(
      facts.address,
    )
    let script_ref = if facts.reference_script_item_offset == -1 {
      None
    } else {
      Some(
        MidgardVersionedScript {
          language: when facts.reference_script_language is {
            0 -> NativeCardanoScript
            3 -> PlutusV3Script
            128 -> MidgardV1Script
            _ -> fail
          },
          script_bytes: bytearray.slice(
            bytes,
            facts.reference_script_offset,
            facts.reference_script_offset + facts.reference_script_length - 1,
          ),
        },
      )
    }
    let output = MidgardTxOutput {
      address,
      value: MidgardValue { lovelace: 0, assets: [] },
      datum_cbor: None,
      script_ref,
    }
    let datum_field = if facts.datum_offset == -1 {
      builtin.constr_data(0, [])
    } else {
      let expected = bytearray.slice(
        bytes,
        facts.datum_offset,
        facts.datum_offset + facts.datum_length - 1,
      )
      let data = carriage.resolve(raw_datum, tx.reference_inputs)
      expect materialisable(data)
      expect cbor.serialise(data) == expected
      builtin.constr_data(2, [data])
    }
    expect Some(value_summary) = state.value_summary
    let summary = script_context_v1.ledger_output_summaries_of_authenticated_parts_v1(
      output,
      value_summary,
      datum_field,
    )
    expect committed_summaries.summaries == [summary]
    delegation.advance(
      datum,
      args,
      tx.outputs,
      delegation.State { ..state, phase: 3, summaries: committed_summaries },
    )
  }

  else(_) {
    fail
  }
}

test materialisable_integer_boundaries_preserve_decoder_domain() {
  and {
    materialisable(builtin.i_data(-18446744073709551616)),
    materialisable(builtin.i_data(18446744073709551615)),
    !materialisable(builtin.i_data(-18446744073709551617)),
    !materialisable(builtin.i_data(18446744073709551616)),
  }
}

test materialisable_constructor_boundaries_preserve_decoder_domain() {
  and {
    materialisable(builtin.constr_data(0, [])),
    materialisable(builtin.constr_data(127, [])),
    !materialisable(builtin.constr_data(128, [])),
  }
}

test materialisable_checks_nested_lists_maps_and_constructors() {
  let rejected = builtin.i_data(18446744073709551616)
  and {
    !materialisable(builtin.list_data([rejected])),
    !materialisable(builtin.map_data([Pair(builtin.i_data(0), rejected)])),
    !materialisable(builtin.constr_data(0, [rejected])),
    materialisable(
      builtin.map_data(
        [
          Pair(
            builtin.b_data(#""),
            builtin.constr_data(127, [builtin.i_data(0)]),
          ),
        ],
      ),
    ),
  }
}
