//// Authenticated, bounded field traversal shared by direct script proofs.
//// Variable fields certify their envelope in a resumable pass before semantic
//// use. A continuation carries only the existing opaque checkpoint digest;
//// public preimage bytes and positions are supplied again and reauthenticated.

use cardano/assets.{PolicyId}
use cardano/transaction.{Input}
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1}
use midgard/fraud_proofs/field_opening_v1 as opening
use midgard/fraud_proofs/native_tx/compact.{decode_tx_compact_view_v1}
use midgard/native_tx_field_access_v1.{FieldViewV1, field_stride}
use midgard/native_tx_machine_walk_v1 as walk

pub type PhaseV1 {
  Certifying
  Walking
}

pub type CursorV1 {
  bound: BoundV1,
  field_index: Int,
  phase: PhaseV1,
  checkpoint_hash: ByteArray,
}

pub fn anchor_v1(bound: BoundV1, field_index: Int) -> opening.NativeTxAnchorV1 {
  if field_index < opening.first_witness_set_field_index {
    opening.BodyAnchor {
      tx_id: bound.subject.transaction_id,
      source_kind: bound.subject.source_kind,
    }
  } else {
    let compact = decode_tx_compact_view_v1(
      opening.source_kind_of_anchor(bound.subject.source_kind),
      bound.compact_cbor,
    )
    opening.WitnessAnchor {
      tx_id: bound.subject.transaction_id,
      witness_set_hash: compact.witness_set_hash,
      source_kind: bound.subject.source_kind,
    }
  }
}

/// The field index is a literal in the family door, and remains in its thread
/// state thereafter. It is never taken from a continuation redeemer.
pub fn initial_v1(
  bound: BoundV1,
  field_index: Int,
  field_opening: opening.FieldOpeningV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> Pair<CursorV1, ByteArray> {
  let (cursor, checkpoint, _) = initial_with_completion_v1(
    bound,
    field_index,
    field_opening,
    reference_inputs,
    certificate_policy_id,
  )
  Pair(cursor, checkpoint)
}

/// Completion is read from the checkpoint created by the authenticated door.
pub fn initial_with_completion_v1(
  bound: BoundV1,
  field_index: Int,
  field_opening: opening.FieldOpeningV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> (CursorV1, ByteArray, Bool) {
  let anchor = anchor_v1(bound, field_index)
  if field_stride(field_index) == 0 {
    let Pair(_, position) = opening.opened_field_grammar_certification(
      field_opening,
      anchor,
      field_index,
      reference_inputs,
      certificate_policy_id,
    )
    (
      CursorV1 {
        bound,
        field_index,
        phase: Certifying,
        checkpoint_hash: walk.field_grammar_checkpoint_hash(position),
      },
      walk.encode_field_grammar_checkpoint(position),
      False,
    )
  } else {
    let Pair(_, position) = opening.opened_field_walk(
      field_opening,
      anchor,
      field_index,
      reference_inputs,
      certificate_policy_id,
    )
    (
      CursorV1 {
        bound,
        field_index,
        phase: Walking,
        checkpoint_hash: walk.field_walk_checkpoint_hash(position),
      },
      walk.encode_field_walk_checkpoint(position),
      walk.walk_is_complete(position),
    )
  }
}

/// A grammar batch advances from its committed position, including at the
/// largest admitted preimage. Completion creates a semantic cursor at zero.
pub fn certify_v1(
  cursor: CursorV1,
  field_opening: opening.FieldOpeningV1,
  checkpoint_bytes: ByteArray,
  budget: Int,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> Pair<CursorV1, ByteArray> {
  certify_at_v1(
    cursor,
    cursor.field_index,
    field_opening,
    checkpoint_bytes,
    budget,
    reference_inputs,
    certificate_policy_id,
  )
}

pub fn certify_at_v1(
  cursor: CursorV1,
  field_index: Int,
  field_opening: opening.FieldOpeningV1,
  checkpoint_bytes: ByteArray,
  budget: Int,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> Pair<CursorV1, ByteArray> {
  let (advanced, checkpoint, _) = certify_with_completion_at_v1(
    cursor,
    field_index,
    field_opening,
    checkpoint_bytes,
    budget,
    reference_inputs,
    certificate_policy_id,
  )
  Pair(advanced, checkpoint)
}

/// A completed grammar pass yields an authenticated semantic checkpoint;
/// an empty field is already exhausted at that checkpoint.
pub fn certify_with_completion_at_v1(
  cursor: CursorV1,
  field_index: Int,
  field_opening: opening.FieldOpeningV1,
  checkpoint_bytes: ByteArray,
  budget: Int,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> (CursorV1, ByteArray, Bool) {
  expect cursor.field_index == field_index
  expect cursor.phase == Certifying
  expect budget > 0
  let anchor = anchor_v1(cursor.bound, field_index)
  let Pair(view, position) = opening.resume_opened_field_grammar_certification(
    field_opening,
    anchor,
    field_index,
    cursor.checkpoint_hash,
    checkpoint_bytes,
    reference_inputs,
    certificate_policy_id,
  )
  let advanced = walk.certify_field_grammar(view, position, budget)
  let advanced_hash = walk.field_grammar_checkpoint_hash(advanced)
  let advanced_bytes = walk.encode_field_grammar_checkpoint(advanced)
  if walk.field_grammar_is_complete(advanced) {
    let Pair(_, semantic) = opening.opened_certified_field_walk_from_grammar(
      field_opening,
      anchor,
      field_index,
      advanced_hash,
      advanced_bytes,
      reference_inputs,
      certificate_policy_id,
    )
    expect walk.walk_field_index(semantic) == field_index
    (
      CursorV1 {
        ..cursor,
        phase: Walking,
        checkpoint_hash: walk.field_walk_checkpoint_hash(semantic),
      },
      walk.encode_field_walk_checkpoint(semantic),
      walk.walk_is_complete(semantic),
    )
  } else {
    (
      CursorV1 { ..cursor, checkpoint_hash: advanced_hash },
      advanced_bytes,
      False,
    )
  }
}

pub fn resume_v1(
  cursor: CursorV1,
  field_opening: opening.FieldOpeningV1,
  checkpoint_bytes: ByteArray,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> Pair<FieldViewV1, walk.FieldWalkCheckpointV1> {
  resume_at_v1(
    cursor,
    cursor.field_index,
    field_opening,
    checkpoint_bytes,
    reference_inputs,
    certificate_policy_id,
  )
}

pub fn resume_at_v1(
  cursor: CursorV1,
  field_index: Int,
  field_opening: opening.FieldOpeningV1,
  checkpoint_bytes: ByteArray,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> Pair<FieldViewV1, walk.FieldWalkCheckpointV1> {
  expect cursor.field_index == field_index
  expect cursor.phase == Walking
  let opened = opening.resume_opened_field_walk(
    field_opening,
    anchor_v1(cursor.bound, field_index),
    field_index,
    cursor.checkpoint_hash,
    checkpoint_bytes,
    reference_inputs,
    certificate_policy_id,
  )
  expect walk.walk_field_index(opened.2nd) == field_index
  opened
}

pub fn after_v1(
  cursor: CursorV1,
  position: walk.FieldWalkCheckpointV1,
) -> CursorV1 {
  expect and {
      cursor.phase == Walking,
      walk.walk_field_index(position) == cursor.field_index,
      walk.walk_tx_id(position) == cursor.bound.subject.transaction_id,
    }
  CursorV1 {
    ..cursor,
    checkpoint_hash: walk.field_walk_checkpoint_hash(position),
  }
}
