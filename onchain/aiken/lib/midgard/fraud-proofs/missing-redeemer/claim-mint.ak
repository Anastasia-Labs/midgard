//// The final authenticated checkpoint supplies the complete policy count.

use cardano/assets.{PolicyId}
use cardano/transaction.{Input}
use midgard/computation_thread as ct
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/mint_purpose
use midgard/fraud_proofs/direct_script_derivation/purpose
use midgard/fraud_proofs/direct_script_derivation/purpose_workers as workers
use midgard/fraud_proofs/direct_script_derivation/source_scan.{BoundIndexV1}
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1, mint_field_index}
use midgard/fraud_proofs/missing_redeemer/claim.{ClaimContextV1}
use midgard/fraud_proofs/missing_redeemer/claim_purpose.{
  ClaimPurposeStateV1, ExistingPurpose, ImpossiblePurpose,
}
use midgard/fraud_proofs/missing_redeemer/claim_start.{FinishOpeningArgsV1}
use midgard/native_tx_machine_walk_v1 as walk

pub type StateV1 {
  context: ClaimContextV1,
  work: workers.MintStateV1,
}

pub type ActionV1 {
  Scan { args: workers.ProgressArgsV1 }
  Finish { args: FinishOpeningArgsV1 }
}

pub type Datum =
  ct.StepDatum<StateV1>

pub type GrammarRedeemer =
  ct.StepRedeemer<workers.ProgressArgsV1>

pub type ScanRedeemer =
  ct.StepRedeemer<ActionV1>

pub fn indices_v1(action: ActionV1) -> Pair<Int, Int> {
  when action is {
    Scan { args } -> Pair(args.input_index, args.output_index)
    Finish { args } -> Pair(args.input_index, args.output_index)
  }
}

pub fn initial_v1(
  context: ClaimContextV1,
  opening: FieldOpeningV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> StateV1 {
  let coordinate = claim_start.distinct_coordinate_v1(context, purpose.mint)
  StateV1 {
    context,
    work: workers.initial_mint_v1(
      context.bound,
      coordinate.index,
      opening,
      reference_inputs,
      certificate_policy_id,
    ),
  }
}

pub fn certify_v1(
  state: StateV1,
  args: workers.ProgressArgsV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> StateV1 {
  let scan = state.work.scan
  expect and {
      !scan.complete,
      scan.selected == None,
      scan.previous_policy == #"",
      args.budget <= workers.scan_batch,
    }
  let (field_cursor, _, complete) = cursor.certify_with_completion_at_v1(
    scan.cursor,
    mint_field_index,
    args.opening,
    args.checkpoint_bytes,
    args.budget,
    reference_inputs,
    certificate_policy_id,
  )
  StateV1 {
    ..state,
    work: workers.MintStateV1 {
      ..state.work,
      scan: mint_purpose.StateV1 { ..scan, cursor: field_cursor, complete },
    },
  }
}

pub fn advance_v1(
  state: StateV1,
  args: workers.ProgressArgsV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> StateV1 {
  expect args.budget <= workers.scan_batch
  let Pair(scan, _) = mint_purpose.advance_v1(
    state.work.scan,
    args.opening,
    args.checkpoint_bytes,
    args.budget,
    reference_inputs,
    certificate_policy_id,
  )
  StateV1 { ..state, work: workers.MintStateV1 { ..state.work, scan } }
}

pub fn finish_v1(
  state: StateV1,
  args: FinishOpeningArgsV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> ClaimPurposeStateV1 {
  let coordinate = claim_start.distinct_coordinate_v1(
    state.context,
    purpose.mint,
  )
  let scan = state.work.scan
  expect scan.target_ordinal == coordinate.index
  let selected = mint_purpose.selected_purpose_v1(scan)
  let Pair(_, checkpoint) = cursor.resume_at_v1(
    scan.cursor,
    mint_field_index,
    args.opening,
    args.checkpoint_bytes,
    reference_inputs,
    certificate_policy_id,
  )
  expect walk.walk_is_complete(checkpoint)
  let count = walk.walk_next_item_index(checkpoint)
  let fact = when selected is {
    Some(actual) -> {
      expect and {
          actual.coordinate == coordinate,
          coordinate.index < count,
        }
      ExistingPurpose { purpose: actual }
    }
    None -> {
      expect coordinate.index >= count
      ImpossiblePurpose { coordinate }
    }
  }
  claim_start.finish_v1(
    state.context,
    BoundIndexV1 {
      bound: scan.cursor.bound,
      source_index: state.work.source_index,
    },
    fact,
  )
}
