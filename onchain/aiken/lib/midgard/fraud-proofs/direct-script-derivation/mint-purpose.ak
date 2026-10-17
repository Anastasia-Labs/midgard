//// Bounded field-5 purpose discovery. This certifies policy coordinates and
//// canonical nonempty policy headers; asset semantics remain the earlier
//// mint-item/fold predicates. No variable-field random-access claim is made.

use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{Input}
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/first_fault_seam.{
  PurposeCoordinateV1,
}
use midgard/fraud_proofs/direct_script_derivation/types.{PurposeV1}
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1, mint_field_index}
use midgard/fraud_proofs/mint_declared_asset_limit/rule.{policy_header_v1}
use midgard/native_tx_field_access_v1.{FieldViewV1, field_read_range}
use midgard/native_tx_machine_walk_v1 as walk

pub type StateV1 {
  cursor: cursor.CursorV1,
  target_ordinal: Int,
  previous_policy: ByteArray,
  selected: Option<PurposeV1>,
  complete: Bool,
}

pub fn initial_v1(field_cursor: cursor.CursorV1, target_ordinal: Int) -> StateV1 {
  expect and {
      field_cursor.field_index == mint_field_index,
      target_ordinal >= 0,
    }
  StateV1 {
    cursor: field_cursor,
    target_ordinal,
    previous_policy: #"",
    selected: None,
    complete: False,
  }
}

/// The largest policy header is a pair tag, a 28-byte key and a map header.
/// One extra byte establishes that the declared nonempty map has a body.
fn fold_v1(
  view: FieldViewV1,
  position: walk.FieldWalkCheckpointV1,
  budget: Int,
  target_ordinal: Int,
  previous_policy: ByteArray,
  selected: Option<PurposeV1>,
) -> (walk.FieldWalkCheckpointV1, ByteArray, Option<PurposeV1>) {
  if budget == 0 || walk.walk_is_complete(position) {
    (position, previous_policy, selected)
  } else {
    let ordinal = walk.walk_next_item_index(position)
    let Pair(Pair(offset, length), advanced) = walk.walk_next_extent(
      view,
      position,
    )
    let prefix = field_read_range(
      view,
      offset,
      if length < 41 {
        length
      } else {
        41
      },
    )
    let header = policy_header_v1(prefix)
    expect or {
        ordinal == 0,
        bytearray.compare(previous_policy, header.policy_id) == Less,
      }
    fold_v1(
      view,
      advanced,
      budget - 1,
      target_ordinal,
      header.policy_id,
      if ordinal == target_ordinal {
        Some(
          PurposeV1 {
            coordinate: PurposeCoordinateV1 { kind: 1, index: ordinal },
            script_hash: header.policy_id,
            subject: header.policy_id,
          },
        )
      } else {
        selected
      },
    )
  }
}

pub fn advance_v1(
  state: StateV1,
  field_opening: FieldOpeningV1,
  checkpoint_bytes: ByteArray,
  budget: Int,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> Pair<StateV1, ByteArray> {
  expect and {
      !state.complete,
      budget > 0,
    }
  let Pair(view, position) = cursor.resume_at_v1(
    state.cursor,
    mint_field_index,
    field_opening,
    checkpoint_bytes,
    reference_inputs,
    certificate_policy_id,
  )
  let (advanced, previous_policy, selected) = fold_v1(
    view,
    position,
    budget,
    state.target_ordinal,
    state.previous_policy,
    state.selected,
  )
  Pair(
    StateV1 {
      ..state,
      cursor: cursor.after_v1(state.cursor, advanced),
      previous_policy,
      selected,
      complete: walk.walk_is_complete(advanced),
    },
    walk.encode_field_walk_checkpoint(advanced),
  )
}

pub fn selected_purpose_v1(state: StateV1) -> Option<PurposeV1> {
  expect state.complete
  state.selected
}
