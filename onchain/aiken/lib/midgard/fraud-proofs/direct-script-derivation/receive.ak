//// One authenticated output pass records each protected script credential's
//// earliest output index. One sorted declaration pass reconstructs that same
//// canonical map; equality proves the exact set and earliest-index bindings.
//// MPF proof work is bounded by the existing protocol proof ceiling and must
//// be measured together with cursor, datum and carriage costs for each batch.

use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/merkle_patricia_forestry.{Proof}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{Input}
use env
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/purpose
use midgard/fraud_proofs/direct_script_derivation/source_index
use midgard/fraud_proofs/direct_script_derivation/source_scan.{BoundIndexV1}
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1, PurposeV1}
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1, outputs_field_index}
use midgard/fraud_proofs/first_fault_v1.{PurposeCoordinateV1}
use midgard/mpf_proof_v1 as mpf
use midgard/native_tx_field_access_v1.{FieldViewV1, field_read_range}
use midgard/native_tx_machine_walk_v1 as walk

pub type OutputWitnessV1 {
  NoCredential
  FirstCredential { proof: Proof }
  RepeatedCredential { first_output_index: Int, proof: Proof }
}

pub type OutputStateV1 {
  cursor: cursor.CursorV1,
  source_index: source_index.StateV1,
  credential_root: ByteArray,
  credential_count: Int,
  complete: Bool,
}

pub type DeclaredCredentialV1 {
  script_hash: ByteArray,
  first_output_index: Int,
  proof: Proof,
}

pub type ReceivePurposeV1 {
  purpose: PurposeV1,
  first_output_index: Int,
}

pub type DeclarationStateV1 {
  bound: BoundV1,
  source_index: source_index.StateV1,
  output_root: ByteArray,
  expected_count: Int,
  declared_root: ByteArray,
  declared_count: Int,
  previous_credential: ByteArray,
  target_ordinal: Option<Int>,
  selected: Option<ReceivePurposeV1>,
}

/// The completed source receipt is carried by the thread, never supplied by
/// the initializer's redeemer. The authenticated output opening creates the
/// only initial cursor; callers cannot replace the bound or the source map.
pub fn initial_outputs_v1(
  source: BoundIndexV1,
  field_opening: FieldOpeningV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> Pair<OutputStateV1, ByteArray> {
  let (field_cursor, checkpoint, complete) = cursor.initial_with_completion_v1(
    source.bound,
    outputs_field_index,
    field_opening,
    reference_inputs,
    certificate_policy_id,
  )
  Pair(
    OutputStateV1 {
      cursor: field_cursor,
      source_index: source.source_index,
      credential_root: env.empty_merkle_tree_root,
      credential_count: 0,
      complete,
    },
    checkpoint,
  )
}

/// The output address occupies at most 61 bytes, even when its value, datum
/// or script reference fills the remaining transaction field-byte allowance.
pub fn next_output_credential_v1(
  view: FieldViewV1,
  position: walk.FieldWalkCheckpointV1,
) -> Pair<Option<ByteArray>, walk.FieldWalkCheckpointV1> {
  let Pair(Pair(offset, length), advanced) = walk.walk_next_extent(
    view,
    position,
  )
  let prefix = field_read_range(
    view,
    offset,
    if length < 61 {
      length
    } else {
      61
    },
  )
  Pair(purpose.protected_script_credential_v1(prefix), advanced)
}

pub fn admit_output_v1(
  root: ByteArray,
  credential_count: Int,
  output_index: Int,
  credential: Option<ByteArray>,
  witness: OutputWitnessV1,
) -> Pair<ByteArray, Int> {
  // The output index is a resumed walk checkpoint's next item index, which
  // decode_field_walk_checkpoint already holds non-negative; the credential is
  // the 28-byte payment hash of a 29- or 57-byte decoded Midgard address.
  when credential is {
    None -> {
      expect witness == NoCredential
      Pair(root, credential_count)
    }
    Some(script_hash) ->
      when witness is {
        NoCredential -> fail
        FirstCredential { proof } -> {
          expect Some(advanced) = mpf.insert_root(
            root,
            script_hash,
            cbor.serialise(output_index),
            proof,
          )
          Pair(advanced, credential_count + 1)
        }
        RepeatedCredential { first_output_index, proof } -> {
          expect and {
              first_output_index >= 0,
              first_output_index < output_index,
              mpf.has_value_hash(
                root,
                script_hash,
                blake2b_256(cbor.serialise(first_output_index)),
                proof,
              ),
            }
          Pair(root, credential_count)
        }
      }
  }
}

fn fold_outputs_v1(
  view: FieldViewV1,
  position: walk.FieldWalkCheckpointV1,
  root: ByteArray,
  count: Int,
  witnesses: List<OutputWitnessV1>,
) -> (walk.FieldWalkCheckpointV1, ByteArray, Int) {
  when witnesses is {
    [] -> (position, root, count)
    [witness, ..rest] -> {
      let index = walk.walk_next_item_index(position)
      let Pair(credential, advanced) = next_output_credential_v1(view, position)
      let Pair(root, count) = admit_output_v1(
        root,
        count,
        index,
        credential,
        witness,
      )
      fold_outputs_v1(view, advanced, root, count, rest)
    }
  }
}

/// Exact, nonempty adjacent batch. Every supplied witness consumes one actual
/// authenticated output; neither a short witness list nor an invented output
/// can advance the cursor or claim completion.
pub fn advance_outputs_v1(
  state: OutputStateV1,
  field_opening: FieldOpeningV1,
  checkpoint_bytes: ByteArray,
  witnesses: List<OutputWitnessV1>,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> Pair<OutputStateV1, ByteArray> {
  expect !state.complete
  expect state.cursor.field_index == outputs_field_index
  expect !list.is_empty(witnesses)
  let Pair(view, position) = cursor.resume_at_v1(
    state.cursor,
    outputs_field_index,
    field_opening,
    checkpoint_bytes,
    reference_inputs,
    certificate_policy_id,
  )
  let (advanced, root, count) = fold_outputs_v1(
    view,
    position,
    state.credential_root,
    state.credential_count,
    witnesses,
  )
  Pair(
    OutputStateV1 {
      ..state,
      cursor: cursor.after_v1(state.cursor, advanced),
      credential_root: root,
      credential_count: count,
      complete: walk.walk_is_complete(advanced),
    },
    walk.encode_field_walk_checkpoint(advanced),
  )
}

pub fn initial_declaration_v1(
  outputs: OutputStateV1,
  target_ordinal: Option<Int>,
) -> DeclarationStateV1 {
  expect outputs.complete
  DeclarationStateV1 {
    bound: outputs.cursor.bound,
    source_index: outputs.source_index,
    output_root: outputs.credential_root,
    expected_count: outputs.credential_count,
    declared_root: env.empty_merkle_tree_root,
    declared_count: 0,
    previous_credential: #"",
    target_ordinal,
    selected: None,
  }
}

pub fn advance_declaration_v1(
  state: DeclarationStateV1,
  entries: List<DeclaredCredentialV1>,
) -> DeclarationStateV1 {
  expect !list.is_empty(entries)
  list.foldl(
    entries,
    state,
    fn(entry, current) {
      expect and {
          current.declared_count < current.expected_count,
          bytearray.length(entry.script_hash) == 28,
          entry.first_output_index >= 0,
          or {
            current.declared_count == 0,
            bytearray.compare(current.previous_credential, entry.script_hash) == Less,
          },
        }
      expect Some(root) = mpf.insert_root_paired_fold(
        current.declared_root,
        entry.script_hash,
        cbor.serialise(entry.first_output_index),
        entry.proof,
      )
      DeclarationStateV1 {
        ..current,
        declared_root: root,
        declared_count: current.declared_count + 1,
        previous_credential: entry.script_hash,
        selected: if current.target_ordinal == Some(current.declared_count) {
          Some(
            ReceivePurposeV1 {
              purpose: PurposeV1 {
                coordinate: PurposeCoordinateV1 {
                  kind: 3,
                  index: current.declared_count,
                },
                script_hash: entry.script_hash,
                subject: entry.script_hash,
              },
              first_output_index: entry.first_output_index,
            },
          )
        } else {
          current.selected
        },
      }
    },
  )
}

pub fn declaration_is_complete_v1(state: DeclarationStateV1) -> Bool {
  and {
    state.declared_count == state.expected_count,
    state.declared_root == state.output_root,
  }
}

/// Empty output fields finish through the same authenticated exhaustion check;
/// they never need a fabricated item or a compatibility default.
pub fn finish_outputs_v1(
  state: OutputStateV1,
  field_opening: FieldOpeningV1,
  checkpoint_bytes: ByteArray,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> OutputStateV1 {
  expect state.cursor.field_index == outputs_field_index
  let Pair(_, position) = cursor.resume_at_v1(
    state.cursor,
    outputs_field_index,
    field_opening,
    checkpoint_bytes,
    reference_inputs,
    certificate_policy_id,
  )
  expect walk.walk_is_complete(position)
  OutputStateV1 { ..state, complete: True }
}

/// Receive redeemer coordinates are sorted-credential ordinals. The first
/// output index is independently authenticated auxiliary location evidence.
pub fn selected_purpose_v1(
  state: DeclarationStateV1,
) -> Option<ReceivePurposeV1> {
  expect declaration_is_complete_v1(state)
  state.selected
}
