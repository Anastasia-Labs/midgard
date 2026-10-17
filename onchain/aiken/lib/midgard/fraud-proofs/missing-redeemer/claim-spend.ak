//// A key spend is impossible only at its completed canonical spend rank.

use cardano/assets.{PolicyId}
use cardano/transaction.{Input, Redeemer, ScriptPurpose}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{MembershipCarriage}
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/purpose
use midgard/fraud_proofs/direct_script_derivation/purpose_workers as workers
use midgard/fraud_proofs/direct_script_derivation/source_index
use midgard/fraud_proofs/direct_script_derivation/source_scan.{BoundIndexV1}
use midgard/fraud_proofs/direct_script_derivation/types.{PurposeV1}
use midgard/fraud_proofs/field_opening_v1.{
  FieldOpeningV1, spend_inputs_field_index,
}
use midgard/fraud_proofs/missing_redeemer/claim.{ClaimContextV1}
use midgard/fraud_proofs/missing_redeemer/claim_purpose.{
  ClaimPurposeStateV1, ExistingPurpose, ImpossiblePurpose,
}
use midgard/fraud_proofs/missing_redeemer/claim_start

pub type StateV1 {
  context: ClaimContextV1,
  source_index: source_index.StateV1,
  cursor: cursor.CursorV1,
  raw_item_index: Int,
  target: purpose.SpendCredentialV1,
  rank: Int,
  complete: Bool,
}

pub type Datum =
  ct.StepDatum<StateV1>

pub type SpendRedeemer =
  ct.StepRedeemer<workers.MintActionV1>

pub fn initial_v1(
  context: ClaimContextV1,
  raw_item_index: Int,
  opening: FieldOpeningV1,
  descriptor_cbor: ByteArray,
  membership: MembershipCarriage,
  reference_inputs: List<Input>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  certificate_policy_id: PolicyId,
) -> StateV1 {
  expect claim_start.distinct_coordinate_v1(context, purpose.spend).index >= 0
  let bound = context.bound
  let target = purpose.open_spend_credential_v1(
    bound.bound,
    raw_item_index,
    opening,
    descriptor_cbor,
    membership,
    reference_inputs,
    redeemers,
    certificate_policy_id,
  )
  let (field_cursor, _, complete) = cursor.initial_with_completion_v1(
    bound.bound,
    spend_inputs_field_index,
    opening,
    reference_inputs,
    certificate_policy_id,
  )
  StateV1 {
    context,
    source_index: bound.source_index,
    cursor: field_cursor,
    raw_item_index,
    target,
    rank: 0,
    complete,
  }
}

pub fn advance_v1(
  state: StateV1,
  args: workers.ProgressArgsV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> StateV1 {
  expect !state.complete
  let (field_cursor, rank, complete) = workers.advance_spend_rank_v1(
    state.cursor,
    state.target.subject,
    state.rank,
    args,
    reference_inputs,
    certificate_policy_id,
  )
  StateV1 { ..state, cursor: field_cursor, rank, complete }
}

pub fn finish_v1(state: StateV1) -> ClaimPurposeStateV1 {
  let coordinate = claim_start.distinct_coordinate_v1(
    state.context,
    purpose.spend,
  )
  expect and {
      state.complete,
      state.cursor.phase == cursor.Walking,
      state.cursor.field_index == spend_inputs_field_index,
      state.rank == coordinate.index,
    }
  claim_start.finish_v1(
    state.context,
    BoundIndexV1 { bound: state.cursor.bound, source_index: state.source_index },
    when state.target.script_hash is {
      Some(script_hash) ->
        ExistingPurpose {
          purpose: PurposeV1 {
            coordinate,
            script_hash,
            subject: state.target.subject,
          },
        }
      None -> ImpossiblePurpose { coordinate }
    },
  )
}
