//// The canonical spend ordinal of a key-locked spend: its rank among every
//// spent outref, completed over the whole of field 0. A key-locked spend has
//// no native execution, so the leaf's recorded spend coordinate is absent
//// exactly when the completed rank equals it.

use cardano/assets.{PolicyId}
use cardano/transaction.{Input, Redeemer, ScriptPurpose}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{MembershipCarriage}
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/purpose
use midgard/fraud_proofs/direct_script_derivation/purpose_workers as workers
use midgard/fraud_proofs/direct_script_derivation/source_scan.{BoundIndexV1}
use midgard/fraud_proofs/execution_native_script_invalid/entry
use midgard/fraud_proofs/execution_native_script_invalid/types.{
  FinalV1, NoExecution, SpendRankV1,
}
use midgard/fraud_proofs/field_opening_v1.{
  FieldOpeningV1, opened_field_walk, spend_inputs_field_index,
}
use midgard/native_tx_machine_walk_v1 as walk

/// Opens the raw spend item and ranks the first page of spent outrefs.
pub type OpenActionV1 {
  input_index: Int,
  output_index: Int,
  raw_item_index: Int,
  opening: FieldOpeningV1,
  descriptor_cbor: ByteArray,
  membership: MembershipCarriage,
  budget: Int,
}

pub type OpenDatum =
  ct.StepDatum<BoundIndexV1>

pub type OpenRedeemer =
  ct.StepRedeemer<OpenActionV1>

pub type Datum =
  ct.StepDatum<SpendRankV1>

pub type SpendRedeemer =
  ct.StepRedeemer<workers.MintActionV1>

/// The raw item's ledger output must be key-locked: no script executes for
/// it. The same authenticated walk view ranks the first page.
pub fn open_v1(
  state: BoundIndexV1,
  action: OpenActionV1,
  reference_inputs: List<Input>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  certificate_policy_id: PolicyId,
) -> SpendRankV1 {
  expect and {
      action.budget > 0,
      action.budget <= workers.scan_batch,
    }
  let bound = state.bound
  let Pair(view, position) = opened_field_walk(
    action.opening,
    cursor.anchor_v1(bound, spend_inputs_field_index),
    spend_inputs_field_index,
    reference_inputs,
    certificate_policy_id,
  )
  let credential = purpose.spend_credential_in_view_v1(
    bound,
    view,
    action.raw_item_index,
    action.descriptor_cbor,
    action.membership,
    reference_inputs,
    redeemers,
  )
  expect None = credential.script_hash
  let Pair(rank, advanced) = workers.fold_spend_rank_v1(
    view,
    position,
    action.budget,
    0,
    credential.subject,
  )
  SpendRankV1 {
    cursor: cursor.CursorV1 {
      bound,
      field_index: spend_inputs_field_index,
      phase: cursor.Walking,
      checkpoint_hash: walk.field_walk_checkpoint_hash(advanced),
    },
    subject: credential.subject,
    rank,
    complete: walk.walk_is_complete(advanced),
  }
}

pub fn advance_v1(
  state: SpendRankV1,
  args: workers.ProgressArgsV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> SpendRankV1 {
  expect !state.complete
  let (field_cursor, rank, complete) = workers.advance_spend_rank_v1(
    state.cursor,
    state.subject,
    state.rank,
    args,
    reference_inputs,
    certificate_policy_id,
  )
  SpendRankV1 { ..state, cursor: field_cursor, rank, complete }
}

pub fn finish_v1(state: SpendRankV1) -> FinalV1 {
  let bound = state.cursor.bound
  let coordinate = entry.recorded_coordinate_v1(bound)
  expect and {
      state.complete,
      coordinate.kind == purpose.spend,
      state.rank == coordinate.index,
    }
  FinalV1 { bound, fact: NoExecution { coordinate } }
}
