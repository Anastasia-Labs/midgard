//// The split span-attach step (stage role 23). The window it records is
//// derived from the pinned pre control alone: it starts at the span the
//// consuming stage reads next (`ledger_output_proof_v1.demanded_span_v1`,
//// restated on the raw frame) and runs for the maximal
//// `min(chunk_bytes, total_length - start)` bytes. The attach is admitted
//// only when the recorded window does not already cover that span, so the
//// chunk proofs are the step's only witness data and a pre control has at
//// most one span-attach successor.

use aiken/builtin
use aiken/collection/list
use aiken/crypto
use midgard/blake2b_224_trace_v1 as blake
use midgard/bounded_item_v1 as bounded
use midgard/ledger_output_proof_datum as datum
use midgard/ledger_output_proof_raw as raw
use midgard/ledger_output_proof_stages as stages
use midgard/ledger_output_proof_v1 as proof

/// Scan item indices (LOP item 5) of `reference_script_item_offset` and
/// `reference_script_offset`.
const scan_reference_script_item_offset_index: Int = 20

const scan_reference_script_offset_index: Int = 21

fn scan_integer(control: raw.Frame, index: Int) -> Int {
  expect Some(value) = list.at(
    builtin.un_list_data(raw.item(control, 5)),
    index,
  )
  builtin.un_i_data(value)
}

/// `ledger_output_proof_v1.demanded_span_v1` on the raw frame.
pub fn demanded_span(control: raw.Frame) -> Option<(Int, Int)> {
  let stage = raw.integer(control, 1)
  if stage == proof.stage_datum_traversal {
    datum.next_source_span(control)
  } else if stage == proof.stage_reference_script_commitment {
    let item_offset = scan_integer(
      control,
      scan_reference_script_item_offset_index,
    )
    let item_length = raw.integer(control, 3) - item_offset
    let chunk_index = raw.integer(control, 8)
    if chunk_index == bounded.chunk_count(item_length) {
      None
    } else {
      Some(
        proof.reference_script_chunk_span_v1(
          item_offset,
          item_length,
          chunk_index,
        ),
      )
    }
  } else {
    expect stage == proof.stage_script_hash
    expect Pair(0, [hash_data]) = builtin.un_constr_data(raw.item(control, 10))
    expect [_version,
          hash_stage,
          cursor, total_length, ..] = builtin.un_list_data(hash_data)
    if builtin.un_i_data(hash_stage) == blake.stage_ready {
      let span = proof.script_hash_content_span_v1(
        scan_integer(control, scan_reference_script_offset_index),
        builtin.un_i_data(cursor),
        builtin.un_i_data(total_length),
      )
      if span.2nd > 0 {
        Some(span)
      } else {
        None
      }
    } else {
      None
    }
  }
}

/// The span-attach step on the raw frame: verify the chunk merkle membership
/// of the demanded window once and record its `(start, length, digest)` in
/// frame item 12. Mirrors `ledger_output_proof_v1`'s `span_attach_step`.
pub fn attach(
  control: raw.Frame,
  witness: proof.LedgerOutputProofWitnessV1,
) -> Option<stages.StepResult> {
  let stage = raw.integer(control, 1)
  expect or {
      stage == proof.stage_datum_traversal,
      stage == proof.stage_reference_script_commitment,
      stage == proof.stage_script_hash,
    }
  expect stages.facts_empty(control)
  expect proof.LedgerOutputProofSpanAttach { chunk_proof, next_chunk_proof } = witness
  when demanded_span(control) is {
    None -> None
    Some((start, length)) ->
      if proof.window_covers_v1(
        stages.window_from_frame(control),
        start,
        length,
      ) {
        None
      } else {
        let total_length = raw.integer(control, 3)
        let window_length = proof.attach_window_length_v1(total_length, start)
        when
          proof.authenticated_output_span_for_output(
            raw.integer(control, 2),
            total_length,
            raw.bytes(control, 4),
            start,
            window_length,
            proof.LedgerOutputProofChunks { chunk_proof, next_chunk_proof },
          )
        is {
          None -> None
          Some(bytes) ->
            Some(
              stages.Advanced {
                control: raw.replace(
                  control,
                  raw.span_window_index,
                  raw.span_window_data(
                    start,
                    window_length,
                    crypto.blake2b_256(bytes),
                  ),
                ),
              },
            )
        }
      }
  }
}
