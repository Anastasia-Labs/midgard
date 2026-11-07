//// The field-8 walk of `unusedRedeemer`. The prover names one redeemer by its
//// raw field-8 index and declares its pointer. The walk certifies the field's
//// grammar, then reads every pointer before that index, and at that index
//// requires the declared pointer. The result is one of three facts: the
//// redeemer exists and no earlier redeemer has its pointer (claim custody
//// then reopens the pointer's purpose), an earlier redeemer has its pointer,
//// or the field ends before the index. Constructor and field order below is
//// wire format.

use cardano/assets.{PolicyId}
use cardano/transaction.{Input}
use midgard/computation_thread as ct
use midgard/fraud_proofs/direct_script_derivation/claim.{
  ClaimContextV1, FinalStateV1, RedeemerPointer,
}
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/purpose_workers.{
  ProgressArgsV1,
}
use midgard/fraud_proofs/direct_script_derivation/redeemer
use midgard/fraud_proofs/direct_script_derivation/redeemer_scan.{scan_batch}
use midgard/fraud_proofs/direct_script_derivation/source_scan.{BoundIndexV1}
use midgard/fraud_proofs/field_opening_v1.{
  FieldOpeningV1, redeemers_field_index,
}
use midgard/fraud_proofs/first_fault_v1.{PurposeCoordinateV1}
use midgard/native_tx_field_access_v1.{FieldViewV1, field_read_range}
use midgard/native_tx_machine_walk_v1 as walk

/// Constr 0 [bound, redeemer_index, pointer_tag, pointer_index, cursor,
/// repeated, reached, complete]. `repeated`: an item before
/// `redeemer_index` carries the declared pointer. `reached`: the item at
/// `redeemer_index` was read and carries the declared pointer. `complete`:
/// the field is exhausted.
pub type WalkStateV1 {
  bound: BoundIndexV1,
  redeemer_index: Int,
  pointer_tag: Int,
  pointer_index: Int,
  cursor: cursor.CursorV1,
  repeated: Bool,
  reached: Bool,
  complete: Bool,
}

pub type EntryArgsV1 {
  input_index: Int,
  output_index: Int,
  redeemer_index: Int,
  pointer_tag: Int,
  pointer_index: Int,
  opening: FieldOpeningV1,
}

pub type ScanActionV1 {
  Scan { args: ProgressArgsV1 }
  Finish { input_index: Int, output_index: Int }
}

pub type EntryDatum =
  ct.StepDatum<BoundIndexV1>

pub type EntryRedeemer =
  ct.StepRedeemer<EntryArgsV1>

pub type WalkDatum =
  ct.StepDatum<WalkStateV1>

pub type GrammarRedeemer =
  ct.StepRedeemer<ProgressArgsV1>

pub type ScanRedeemer =
  ct.StepRedeemer<ScanActionV1>

pub fn scan_indices_v1(action: ScanActionV1) -> Pair<Int, Int> {
  when action is {
    Scan { args } -> Pair(args.input_index, args.output_index)
    Finish { input_index, output_index } -> Pair(input_index, output_index)
  }
}

/// The purpose coordinate a redeemer pointer names. A tag with no purpose
/// kind maps to an invalid coordinate, which claim custody resolves as an
/// impossible purpose.
pub fn coordinate_of_pointer_v1(tag: Int, index: Int) -> PurposeCoordinateV1 {
  PurposeCoordinateV1 {
    kind: when tag is {
      0 -> 0
      1 -> 1
      3 -> 2
      6 -> 3
      _ -> -1
    },
    index,
  }
}

/// Opens field 8 under the source-finish binding. The field is variable
/// width, so the walk starts by certifying its grammar.
pub fn open_v1(
  bound: BoundIndexV1,
  args: EntryArgsV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> WalkStateV1 {
  let (field_cursor, _, complete) = cursor.initial_redeemers_v1(
    bound.bound,
    args.opening,
    reference_inputs,
    certificate_policy_id,
  )
  WalkStateV1 {
    bound,
    redeemer_index: args.redeemer_index,
    pointer_tag: args.pointer_tag,
    pointer_index: args.pointer_index,
    cursor: field_cursor,
    repeated: False,
    reached: False,
    complete,
  }
}

/// One bounded batch of field-8 grammar certification.
pub fn certify_v1(
  state: WalkStateV1,
  args: ProgressArgsV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> WalkStateV1 {
  expect args.budget <= scan_batch
  let (advanced, _, complete) = cursor.certify_with_completion_at_v1(
    state.cursor,
    redeemers_field_index,
    args.opening,
    args.checkpoint_bytes,
    args.budget,
    reference_inputs,
    certificate_policy_id,
  )
  WalkStateV1 { ..state, cursor: advanced, complete }
}

/// One item of the pointer walk: before the target index it records a repeat
/// of the declared pointer; at the target index it requires the declared
/// pointer.
pub fn read_item_v1(
  state: WalkStateV1,
  item_index: Int,
  pointer: Pair<Int, Int>,
) -> WalkStateV1 {
  let declared = pointer == Pair(state.pointer_tag, state.pointer_index)
  if item_index == state.redeemer_index {
    expect declared
    WalkStateV1 { ..state, reached: True }
  } else {
    WalkStateV1 { ..state, repeated: state.repeated || declared }
  }
}

fn fold_v1(
  view: FieldViewV1,
  position: walk.FieldWalkCheckpointV1,
  budget: Int,
  state: WalkStateV1,
) -> Pair<WalkStateV1, walk.FieldWalkCheckpointV1> {
  if budget == 0 || state.reached || walk.walk_is_complete(position) {
    Pair(state, position)
  } else {
    let item_index = walk.walk_next_item_index(position)
    let Pair(Pair(offset, length), advanced) = walk.walk_next_extent(
      view,
      position,
    )
    // The pointer is the item's array head, tag and index: at most 19 bytes.
    let prefix = field_read_range(
      view,
      offset,
      if length < 19 {
        length
      } else {
        19
      },
    )
    fold_v1(
      view,
      advanced,
      budget - 1,
      read_item_v1(state, item_index, redeemer.pointer_from_prefix_v1(prefix)),
    )
  }
}

/// One bounded batch of the pointer walk, from the committed checkpoint. A
/// walk that has reached its redeemer or exhausted the field reads nothing
/// further (`fold_v1`), so a batch after either leaves the state unchanged.
pub fn advance_v1(
  state: WalkStateV1,
  args: ProgressArgsV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> WalkStateV1 {
  expect args.budget <= scan_batch
  let Pair(view, position) = cursor.resume_at_v1(
    state.cursor,
    redeemers_field_index,
    args.opening,
    args.checkpoint_bytes,
    reference_inputs,
    certificate_policy_id,
  )
  let Pair(read, advanced) = fold_v1(view, position, args.budget, state)
  WalkStateV1 {
    ..read,
    cursor: cursor.after_v1(state.cursor, advanced),
    complete: walk.walk_is_complete(advanced),
  }
}

fn context_v1(state: WalkStateV1) -> ClaimContextV1 {
  ClaimContextV1 {
    bound: state.bound,
    observed: None,
    target: RedeemerPointer {
      redeemer_index: state.redeemer_index,
      coordinate: coordinate_of_pointer_v1(
        state.pointer_tag,
        state.pointer_index,
      ),
    },
  }
}

/// A walk that read the target redeemer with no earlier repeat hands its
/// pointer to claim custody.
pub fn claim_context_v1(state: WalkStateV1) -> ClaimContextV1 {
  expect and {
      state.cursor.phase == cursor.Walking,
      state.reached,
      !state.repeated,
    }
  context_v1(state)
}

/// A repeat before the target redeemer, or a field that ends before it,
/// needs no purpose lookup.
pub fn final_state_v1(state: WalkStateV1) -> FinalStateV1 {
  expect state.cursor.phase == cursor.Walking
  let fact = if state.reached {
    expect state.repeated
    claim.RedeemerRepeated
  } else {
    expect state.complete
    claim.RedeemerOutOfRange
  }
  FinalStateV1 { context: context_v1(state), claim: fact }
}
