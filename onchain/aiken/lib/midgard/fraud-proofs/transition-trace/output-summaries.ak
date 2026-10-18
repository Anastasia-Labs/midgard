//// Phase 8 of an opened output: walk its inline datum, then commit the
//// output's three summaries.
////
//// The datum span is read straight from the output bytes, whose hash the
//// opening stage committed, so every transaction of the walk reads bytes that
//// one hash authenticates. A deposit's output was matched byte for byte
//// against its projected output (phase 10), whose datum is `cbor.serialise` of
//// the deposit datum.

use aiken/primitive/bytearray
use midgard/cek_data_v1.{DataSummaryV1}
use midgard/data_summary_walk_v1.{Entry,
  Summarised, Unreadable, Walk, Walking} as walk
use midgard/fraud_proofs/native_tx/types.{
  MidgardTxOutput, MidgardV1Script, MidgardValue, MidgardVersionedScript,
  NativeCardanoScript, PlutusV3Script,
}
use midgard/fraud_proofs/transition_trace/final_yield.{OutputSummaries, State}
use midgard/ledger_output_scan_v1 as scan
use midgard/ledger_output_v1
use midgard/script_context_v1

/// The witness of one transaction of the walk: the stack entries its steps
/// pop below their own pushes, the chain root under them, and how many steps
/// to take.
pub type WalkWitness {
  restore: List<Entry>,
  base: ByteArray,
  steps: Int,
}

fn output_parts(
  facts: scan.LedgerOutputScanControlV1,
  bytes: ByteArray,
) -> MidgardTxOutput {
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
  MidgardTxOutput {
    address,
    value: MidgardValue { lovelace: 0, assets: [] },
    datum_cbor: None,
    script_ref,
  }
}

/// The three summaries of the output whose committed bytes are `bytes`, from
/// its terminal scan facts, its Value summary and, when it has an inline
/// datum, that datum's summary.
pub fn summary_of(
  facts: scan.LedgerOutputScanControlV1,
  bytes: ByteArray,
  value: DataSummaryV1,
  datum: Option<DataSummaryV1>,
) -> (DataSummaryV1, DataSummaryV1, DataSummaryV1) {
  script_context_v1.ledger_output_summaries_of_authenticated_parts_v1(
    output_parts(facts, bytes),
    value,
    datum,
  )
}

/// One transaction of the inline-datum walk, from `checkpoint`.
pub fn resume_datum(
  facts: scan.LedgerOutputScanControlV1,
  bytes: ByteArray,
  checkpoint: Walk,
  witness: WalkWitness,
) -> walk.Outcome {
  walk.resume(
    checkpoint,
    bytes,
    facts.datum_offset + facts.datum_length,
    witness.restore,
    witness.base,
    witness.steps,
  )
}

fn summarised(
  state: State,
  facts: scan.LedgerOutputScanControlV1,
  bytes: ByteArray,
  datum: Option<DataSummaryV1>,
) -> State {
  expect Some(value) = state.value_summary
  State {
    ..state,
    phase: 3,
    summaries: OutputSummaries {
      summaries: [summary_of(facts, bytes, value, datum)],
    },
    datum_walk: None,
  }
}

/// The next checkpoint after `witness.steps` steps of the datum walk of the
/// output whose committed bytes are `bytes`: still phase 8 with the advanced
/// walk, or phase 3 with the summaries once the datum is summarised. An output
/// without an inline datum is summarised at once.
pub fn step(state: State, bytes: ByteArray, witness: WalkWitness) -> State {
  let facts = scan.decode_control_v1(state.scan_cbor)
  if facts.datum_offset == -1 {
    summarised(state, facts, bytes, None)
  } else {
    let checkpoint = when state.datum_walk is {
      None -> walk.start(facts.datum_offset)
      Some(raw) -> {
        expect checkpoint: Walk = raw
        checkpoint
      }
    }
    when resume_datum(facts, bytes, checkpoint, witness) is {
      Walking(next) -> {
        let raw: Data = next
        State { ..state, datum_walk: Some(raw) }
      }
      Summarised(datum) -> summarised(state, facts, bytes, Some(datum))
      // A datum outside the canonical grammar has no summary, so the thread
      // cannot continue and its prover cancels it. A deposit's datum is
      // `cbor.serialise` output and never lands here. An accepted L2 output
      // with such a datum is the `OutputNonCanonical` fault, which the
      // transaction-output-non-canonical family proves with the same grammar.
      Unreadable -> fail @"output datum is not canonical Data"
    }
  }
}
