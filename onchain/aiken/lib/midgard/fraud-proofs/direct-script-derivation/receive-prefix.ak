//// Authenticate the first-output location auxiliary to a named receive purpose.
//// This pass reads each earlier output exactly once, in bounded batches.

use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{Input}
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/receive
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1, outputs_field_index}
use midgard/native_tx_field_access_v1.{FieldViewV1}
use midgard/native_tx_machine_walk_v1 as walk

pub type StateV1 {
  cursor: cursor.CursorV1,
  target_index: Int,
  script_hash: ByteArray,
  complete: Bool,
}

pub fn initial_v1(
  field_cursor: cursor.CursorV1,
  target_index: Int,
  script_hash: ByteArray,
) -> StateV1 {
  expect and {
      field_cursor.field_index == outputs_field_index,
      target_index >= 0,
      bytearray.length(script_hash) == 28,
    }
  StateV1 { cursor: field_cursor, target_index, script_hash, complete: False }
}

fn fold_prefix_v1(
  view: FieldViewV1,
  position: walk.FieldWalkCheckpointV1,
  remaining: Int,
  target_index: Int,
  script_hash: ByteArray,
) -> Pair<Bool, walk.FieldWalkCheckpointV1> {
  if remaining == 0 {
    Pair(False, position)
  } else {
    let index = walk.walk_next_item_index(position)
    expect index <= target_index
    let Pair(credential, advanced) = receive.next_output_credential_v1(
      view,
      position,
    )
    if index == target_index {
      expect credential == Some(script_hash)
      Pair(True, advanced)
    } else {
      expect credential != Some(script_hash)
      fold_prefix_v1(view, advanced, remaining - 1, target_index, script_hash)
    }
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
  expect !state.complete
  expect budget > 0
  let Pair(view, position) = cursor.resume_v1(
    state.cursor,
    field_opening,
    checkpoint_bytes,
    reference_inputs,
    certificate_policy_id,
  )
  let Pair(complete, advanced) = fold_prefix_v1(
    view,
    position,
    budget,
    state.target_index,
    state.script_hash,
  )
  Pair(
    StateV1 {
      ..state,
      cursor: cursor.after_v1(state.cursor, advanced),
      complete,
    },
    walk.encode_field_walk_checkpoint(advanced),
  )
}

pub type LocationV1 {
  script_hash: ByteArray,
  first_output_index: Int,
}

pub fn location_v1(state: StateV1) -> LocationV1 {
  expect state.complete
  LocationV1 {
    first_output_index: state.target_index,
    script_hash: state.script_hash,
  }
}
