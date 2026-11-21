//// Source discovery from witness bytes or ledger descriptors opened against
//// the subject event's authenticated pre-root. Spent and reference inputs are
//// distinct physical fields, and both can supply a script reference.

use aiken/cbor
use aiken/merkle_patricia_forestry.{Proof}
use aiken/primitive/bytearray
use cardano/transaction.{OutputReference}
use midgard/bounded_item_v1
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/types.{PurposeV1}
use midgard/fraud_proofs/field_opening_v1.{
  reference_inputs_field_index, script_witnesses_field_index,
  spend_inputs_field_index,
}
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_tx_input_cbor, decode_midgard_versioned_script_at,
  encode_midgard_versioned_script,
}
use midgard/fraud_proofs/transition_trace/proof.{ledger_outref_key}
use midgard/ledger_output_commitment_v1
use midgard/mpf_proof_v1
use midgard/native_tx_field_access_v1.{FieldViewV1}
use midgard/native_tx_machine_walk_v1 as walk
use midgard/script_proof_v1

pub type SourceV1 {
  field_index: Int,
  item_index: Int,
  source_key: ByteArray,
  language_tag: Int,
  script_hash: ByteArray,
  total_length: Int,
  item_commitment: ByteArray,
}

pub type StateV1 {
  cursor: cursor.CursorV1,
  purpose: PurposeV1,
  selected: Option<SourceV1>,
  complete: Bool,
}

pub type ResolvedWitnessV1 {
  descriptor_cbor: ByteArray,
  proof: Proof,
}

pub fn initial_v1(field_cursor: cursor.CursorV1, purpose: PurposeV1) -> StateV1 {
  expect or {
      field_cursor.field_index == script_witnesses_field_index,
      field_cursor.field_index == spend_inputs_field_index,
      field_cursor.field_index == reference_inputs_field_index,
    }
  StateV1 { cursor: field_cursor, purpose, selected: None, complete: False }
}

pub fn witness_source_v1(item_index: Int, item: ByteArray) -> SourceV1 {
  let Pair(offset, script) = decode_midgard_versioned_script_at(item, 0)
  expect and {
      offset == bytearray.length(item),
      encode_midgard_versioned_script(script) == item,
    }
  SourceV1 {
    field_index: script_witnesses_field_index,
    item_index,
    source_key: cbor.serialise(item_index),
    language_tag: script_proof_v1.language_tag(script.language),
    script_hash: script_proof_v1.versioned_script_hash(script),
    total_length: bytearray.length(item),
    item_commitment: bounded_item_v1.from_bytes(
      script_witnesses_field_index,
      item_index,
      item,
    ),
  }
}

fn select_v1(
  selected: Option<SourceV1>,
  wanted: ByteArray,
  candidate: Option<SourceV1>,
) -> Option<SourceV1> {
  when selected is {
    Some(_) -> selected
    None ->
      when candidate is {
        Some(source) ->
          if source.script_hash == wanted {
            candidate
          } else {
            None
          }
        None -> None
      }
  }
}

pub fn resolved_source_v1(
  event_pre_root: ByteArray,
  field_index: Int,
  item_index: Int,
  out_ref_cbor: ByteArray,
  witness: ResolvedWitnessV1,
) -> Option<SourceV1> {
  expect or {
      field_index == spend_inputs_field_index,
      field_index == reference_inputs_field_index,
    }
  let input = decode_midgard_tx_input_cbor(out_ref_cbor)
  let out_ref = OutputReference {
    transaction_id: input.tx_id,
    output_index: input.output_index,
  }
  expect
    mpf_proof_v1.has(
      event_pre_root,
      ledger_outref_key(out_ref),
      witness.descriptor_cbor,
      witness.proof,
    )
  let descriptor = ledger_output_commitment_v1.decode(witness.descriptor_cbor)
  expect descriptor.output_index == input.output_index
  if descriptor.reference_script_language == -1 {
    None
  } else {
    Some(
      SourceV1 {
        field_index,
        item_index,
        source_key: out_ref_cbor,
        language_tag: descriptor.reference_script_language,
        script_hash: descriptor.reference_script_hash,
        total_length: descriptor.reference_script_total_length,
        item_commitment: descriptor.reference_script_item_commitment,
      },
    )
  }
}

fn fold_resolved_v1(
  state: StateV1,
  view: FieldViewV1,
  position: walk.FieldWalkCheckpointV1,
  selected: Option<SourceV1>,
  witnesses: List<ResolvedWitnessV1>,
) -> Pair<Option<SourceV1>, walk.FieldWalkCheckpointV1> {
  when witnesses is {
    [] -> Pair(selected, position)
    [witness, ..rest] -> {
      let index = walk.walk_next_item_index(position)
      let Pair(item, advanced) = walk.walk_next(view, position)
      let candidate = resolved_source_v1(
        state.cursor.bound.event_pre_root,
        state.cursor.field_index,
        index,
        item,
        witness,
      )
      fold_resolved_v1(
        state,
        view,
        advanced,
        select_v1(selected, state.purpose.script_hash, candidate),
        rest,
      )
    }
  }
}
