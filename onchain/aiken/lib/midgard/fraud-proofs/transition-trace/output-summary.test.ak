use aiken/builtin
use aiken/collection/list
use aiken/primitive/bytearray
use midgard/cek_data_v1
use midgard/fraud_proofs/native_tx/types.{MidgardTxOutput, MidgardValue}
use midgard/fraud_proofs/transition_trace/final_yield.{State}
use midgard/fraud_proofs/transition_trace/output_summaries.{WalkWitness}
use midgard/fraud_proofs/transition_trace/proof_carriage
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_scan_v1 as scan
use midgard/ledger_output_v1
use midgard/ledger_output_value_v1
use midgard/script_context_v1
use midgard/validation_merkle_v1

/// One policy holding asset names `aa` and `0102`. The output's canonical CBOR
/// and the script context both order them as plain bytes, so `0102` precedes
/// the shorter `aa`.
const mixed_width_output_cbor =
  #"a200581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01821a007a1200a1581c11111111111111111111111111111111111111111111111111111111a24201020741aa05"

const lovelace = 8000000

fn policy() -> ByteArray {
  builtin.replicate_byte(28, 17)
}

/// The `Value` summary the staged output-value step folds for that output,
/// stepping the assets in descending byte order.
fn folded_value_summary() -> cek_data_v1.DataSummaryV1 {
  let narrow = ledger_output_commitment_v1.asset_leaf_hash(policy(), #"aa", 5)
  let wide = ledger_output_commitment_v1.asset_leaf_hash(policy(), #"0102", 7)
  let peaks = validation_merkle_v1.append_leaf(
    1,
    validation_merkle_v1.append_leaf(0, [], wide),
    narrow,
  )
  let step = fn(control, witness) {
    ledger_output_value_v1.step_v1(control, 2, peaks, lovelace, witness)
  }
  expect Some(first) = step(
    ledger_output_value_v1.initial_control_v1(2),
    ledger_output_value_v1.LedgerOutputValueAsset {
      asset_index: 1,
      policy_id: policy(),
      asset_name: #"aa",
      quantity: 5,
      siblings: [wide],
      previous: None,
    },
  )
  expect Some(second) = step(
    first,
    ledger_output_value_v1.LedgerOutputValueAsset {
      asset_index: 0,
      policy_id: policy(),
      asset_name: #"0102",
      quantity: 7,
      siblings: [narrow],
      previous: Some(
        ledger_output_value_v1.LedgerOutputValueHeadV1 {
          asset_name: #"aa",
          quantity: 5,
          tail: cek_data_v1.empty_data_pair_summary_v1(),
        },
      ),
    },
  )
  expect Some(finalizing) = step(
    second,
    ledger_output_value_v1.LedgerOutputValueNoWitness,
  )
  expect Some(terminal) = step(
    finalizing,
    ledger_output_value_v1.LedgerOutputValueNoWitness,
  )
  expect Some(summary) = ledger_output_value_v1.finalize_v1(terminal)
  summary
}

/// The output-summaries step composes the folded `Value` with the address,
/// reference script and datum it authenticated, over an output whose own value
/// it zeroes. The result must be the summaries the one-shot path derives from
/// the output bytes, or a descriptor built either way would commit a different
/// ledger value.
test folded_value_composes_to_the_one_shot_output_summaries() {
  expect Some(output) = ledger_output_v1.decode_canonical_output(
    mixed_width_output_cbor,
  )
  script_context_v1.ledger_output_summaries_v1(mixed_width_output_cbor) == Some(
    script_context_v1.ledger_output_summaries_of_authenticated_parts_v1(
      MidgardTxOutput {
        ..output,
        value: MidgardValue { lovelace: 0, assets: [] },
      },
      folded_value_summary(),
      None,
    ),
  )
}

/// Phase 8 of an output with address `60 aa..aa` whose datum is `datum`, at
/// the output-bytes offset 30, where `bytes` places it.
fn datum_phase(datum: ByteArray) -> (State, ByteArray) {
  let address = bytearray.push(builtin.replicate_byte(28, 170), 96)
  let bytes = builtin.replicate_byte(30, 0)
    |> bytearray.concat(datum)
    |> bytearray.concat(#"00")
  let control = scan.LedgerOutputScanControlV1 {
    ..scan.initial_control_v1(),
    address,
    datum_offset: 30,
    datum_length: bytearray.length(datum),
  }
  (
    State {
      ..final_yield.initial(0, proof_carriage.Commitment { hash: #"" }),
      phase: 8,
      scan_cbor: scan.encode_control_v1(control),
      value_summary: Some(folded_value_summary()),
    },
    bytes,
  )
}

fn steps(count: Int) -> WalkWitness {
  WalkWitness { restore: [], base: #"", steps: count }
}

/// A canonical datum one step long is summarised in that step, which commits
/// the output's summaries and leaves phase 8.
test a_canonical_datum_is_summarised() {
  let (state, bytes) = datum_phase(#"80")
  let next = output_summaries.step(state, bytes, steps(1))
  and {
    next.phase == 3,
    next.datum_walk == None,
    list.length(next.summaries.summaries) == 1,
  }
}

/// An empty indefinite-length list is outside the canonical grammar. Its
/// steps open the list, read the break, and then find nothing to fold: that
/// step has no successor State, so the thread can only be cancelled.
test a_datum_outside_the_canonical_grammar_has_no_successor() fail {
  let (state, bytes) = datum_phase(#"9fff")
  output_summaries.step(state, bytes, steps(3)).phase == 8
}
