use midgard/cek_data_bytes_v1 as bytes_control
use midgard/cek_data_integer_v1 as integer
use midgard/cek_data_traverse_v1 as traversal
use midgard/redeemer_item_proof_v1 as item
use midgard/script_sources_item_normalization as normalized

pub fn head_scalar(state: normalized.TraversalExecution) -> Bool {
  expect state.current.stage == traversal.stage_head
  expect traversal.HeadScalar { item_length } = state.action
  expect Some(bytes) = state.source_bytes
  traversal.prevalidated_head_scalar(state.current, bytes, item_length) == Some(
    state.next,
  )
}

pub fn head_sequence(state: normalized.TraversalExecution) -> Bool {
  expect state.current.stage == traversal.stage_head
  expect traversal.HeadSequence { expected_children } = state.action
  expect Some(bytes) = state.source_bytes
  traversal.prevalidated_head_sequence(state.current, bytes, expected_children) == Some(
    state.next,
  )
}

pub fn head_map(state: normalized.TraversalExecution) -> Bool {
  expect state.current.stage == traversal.stage_head
  expect traversal.HeadMap = state.action
  expect Some(bytes) = state.source_bytes
  traversal.prevalidated_head_map(state.current, bytes) == Some(state.next)
}

pub fn head_large_constructor(state: normalized.TraversalExecution) -> Bool {
  expect state.current.stage == traversal.stage_head
  expect
      traversal.HeadLargeConstructor {
        constructor_cbor_length,
        expected_children,
      }
    = state.action
  expect Some(bytes) = state.source_bytes
  traversal.prevalidated_head_large_constructor(
    state.current,
    bytes,
    constructor_cbor_length,
    expected_children,
  ) == Some(state.next)
}

pub fn attach_integer(state: normalized.TraversalExecution) -> Bool {
  expect state.current.stage == traversal.stage_integer
  expect traversal.AttachScalar { parent } = state.action
  expect state.source_bytes == None
  traversal.prevalidated_attach_integer(state.current, parent) == Some(
    state.next,
  )
}

pub fn advance_integer(state: normalized.TraversalExecution) -> Bool {
  expect state.current.stage == traversal.stage_integer
  expect state.action == traversal.NoAction
  expect Some(scalar) = state.current.integer
  expect scalar.stage != integer.stage_terminal
  traversal.prevalidated_advance_integer(state.current, state.source_bytes) == Some(
    state.next,
  )
}

pub fn attach_bytes(state: normalized.TraversalExecution) -> Bool {
  expect state.current.stage == traversal.stage_bytes
  expect traversal.AttachScalar { parent } = state.action
  expect state.source_bytes == None
  traversal.prevalidated_attach_bytes(state.current, parent) == Some(state.next)
}

pub fn advance_bytes(state: normalized.TraversalExecution) -> Bool {
  expect state.current.stage == traversal.stage_bytes
  expect state.action == traversal.NoAction
  expect Some(scalar) = state.current.bytes
  expect scalar.stage != bytes_control.stage_terminal
  traversal.prevalidated_advance_bytes(state.current, state.source_bytes) == Some(
    state.next,
  )
}

pub fn advance_large_constructor(state: normalized.TraversalExecution) -> Bool {
  expect state.current.stage == traversal.stage_large_constructor
  expect state.action == traversal.NoAction
  traversal.prevalidated_advance_large_constructor(
    state.current,
    state.source_bytes,
    traversal.NoAction,
  ) == Some(state.next)
}

pub fn advance_large_fields(state: normalized.TraversalExecution) -> Bool {
  expect state.current.stage == traversal.stage_large_fields
  expect state.action == traversal.NoAction
  traversal.prevalidated_advance_large_fields(
    state.current,
    state.source_bytes,
    traversal.NoAction,
  ) == Some(state.next)
}

pub fn close(state: normalized.TraversalExecution) -> Bool {
  expect state.current.stage == traversal.stage_close
  expect state.action == traversal.NoAction
  traversal.prevalidated_close(
    state.current,
    state.source_bytes,
    traversal.NoAction,
  ) == Some(state.next)
}

pub fn fold_list(state: normalized.TraversalExecution) -> Bool {
  expect state.current.stage == traversal.stage_fold
  expect traversal.FoldList { frame, child_index, child, siblings } = state.action
  expect state.source_bytes == None
  expect Some(root) = traversal.prevalidated_fold_list_next_frame_root(
    state.current.frame_root,
    frame,
    child_index,
    child,
    siblings,
  )
  state.next == traversal.DataTraverseControlV1 {
    ..state.current,
    frame_root: root,
  }
}

pub fn fold_map(state: normalized.TraversalExecution) -> Bool {
  expect state.current.stage == traversal.stage_fold
  expect
      traversal.FoldMap {
        frame,
        pair_index,
        key,
        value,
        key_siblings,
        value_siblings,
      }
    = state.action
  expect state.source_bytes == None
  expect Some(root) = traversal.prevalidated_fold_map_next_frame_root_v1(
    state.current.frame_root,
    frame,
    pair_index,
    key,
    value,
    key_siblings,
    value_siblings,
  )
  state.next == traversal.DataTraverseControlV1 {
    ..state.current,
    frame_root: root,
  }
}

pub fn finalize_frame(state: normalized.TraversalExecution) -> Bool {
  expect state.current.stage == traversal.stage_fold
  expect traversal.FinalizeFrame { frame, parent } = state.action
  expect state.source_bytes == None
  expect Some(transition) = traversal.prevalidated_finalize_frame_transition_v1(
    state.current.frame_root,
    state.current.offset,
    state.current.source_length,
    frame,
    parent,
  )
  state.next == traversal.DataTraverseControlV1 {
    ..state.current,
    stage: transition.next_stage,
    frame_root: transition.next_frame_root,
    pending_large_expected_children: None,
    integer: None,
    bytes: None,
    result: transition.next_result,
  }
}

// Both carriers: the CEK context binds descriptor-mode selection items into
// this same chain (cek-context-item-bind requires mode_descriptor at context
// stage 0), so the executor domain is the full step_v1 header domain. The
// canonical machine derives the mode-dependent successor itself.
pub fn open_header(state: normalized.OuterExecution) -> Bool {
  expect state.current.stage == item.stage_header
  expect
    state.current.mode == item.mode_descriptor || state.current.mode == item.mode_data
  expect state.action == item.RedeemerItemOpenHeader
  expect Some(bytes) = state.source_bytes
  item.prevalidated_open_header(state.current, bytes) == item.RedeemerItemProofAdvanced {
    control: state.next,
  }
}

// Same step_v1 domain as open_header: descriptor-mode tails advance to the
// terminal stage with no traversal, data-mode tails open the traversal; the
// canonical prevalidated_open_tail successor pins whichever shape applies.
pub fn open_tail(state: normalized.OuterExecution) -> Bool {
  expect state.current.stage == item.stage_tail
  expect
    state.current.mode == item.mode_descriptor || state.current.mode == item.mode_data
  expect state.action == item.RedeemerItemOpenTail
  expect Some(bytes) = state.source_bytes
  item.prevalidated_open_tail(state.current, bytes) == item.RedeemerItemProofAdvanced {
    control: state.next,
  }
}

pub fn finish_data(state: normalized.OuterExecution) -> Bool {
  expect state.current.stage == item.stage_data
  expect state.current.mode == item.mode_data
  expect state.action == item.RedeemerItemFinishData
  expect state.source_bytes == None
  expect Some(current) = state.current.traversal
  expect current.stage == traversal.stage_terminal
  state.next == item.RedeemerItemProofControlV1 {
    ..state.current,
    stage: item.stage_terminal,
  }
}

// Invalid is a canonical machine result, never a missing source witness.
pub fn invalid_header(state: normalized.OuterExecution) -> Bool {
  expect state.current.mode == item.mode_data
  expect state.current.stage == item.stage_header
  expect state.action == item.RedeemerItemOpenHeader
  expect state.next == state.current
  expect Some(bytes) = state.source_bytes
  item.prevalidated_open_header(state.current, bytes) == item.RedeemerItemProofInvalid
}

// Invalid is a canonical machine result, never a missing source witness.
pub fn invalid_tail(state: normalized.OuterExecution) -> Bool {
  expect state.current.mode == item.mode_data
  expect state.current.stage == item.stage_tail
  expect state.action == item.RedeemerItemOpenTail
  expect state.next == state.current
  expect Some(bytes) = state.source_bytes
  item.prevalidated_open_tail(state.current, bytes) == item.RedeemerItemProofInvalid
}
