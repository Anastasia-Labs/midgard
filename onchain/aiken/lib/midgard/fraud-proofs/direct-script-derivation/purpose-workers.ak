//// Physical purpose selection and unique source-map lookup. Free selectors
//// belong to the thread owner; all subsequent progress is deterministic.

use aiken/merkle_patricia_forestry.{Proof}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{Input, Redeemer, ScriptPurpose}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{MembershipCarriage}
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/first_fault_seam.{
  PurposeCoordinateV1,
}
use midgard/fraud_proofs/direct_script_derivation/mint_purpose
use midgard/fraud_proofs/direct_script_derivation/purpose
use midgard/fraud_proofs/direct_script_derivation/selection_types.{SelectionV1}
use midgard/fraud_proofs/direct_script_derivation/source.{SourceV1}
use midgard/fraud_proofs/direct_script_derivation/source_index
use midgard/fraud_proofs/direct_script_derivation/source_scan.{BoundIndexV1}
use midgard/fraud_proofs/direct_script_derivation/types.{PurposeV1}
use midgard/fraud_proofs/field_opening_v1.{
  FieldOpeningV1, mint_field_index, opened_field_walk, spend_inputs_field_index,
}
use midgard/native_tx_field_access_v1.{FieldViewV1}
use midgard/native_tx_machine_walk_v1 as walk

/// The selection door chooses the purpose kind. A spend selection is opened by
/// the spend door, which receives this door's state unchanged.
pub type SelectionActionV1 {
  Spend { input_index: Int, output_index: Int }
  Observe {
    input_index: Int,
    output_index: Int,
    purpose_index: Int,
    opening: FieldOpeningV1,
  }
  Mint {
    input_index: Int,
    output_index: Int,
    purpose_index: Int,
    opening: FieldOpeningV1,
  }
  Receive { input_index: Int, output_index: Int }
}

pub type SpendSelectionArgsV1 {
  input_index: Int,
  output_index: Int,
  raw_item_index: Int,
  opening: FieldOpeningV1,
  descriptor_cbor: ByteArray,
  membership: MembershipCarriage,
}

pub type MintStateV1 {
  source_index: source_index.StateV1,
  scan: mint_purpose.StateV1,
}

pub type SpendStateV1 {
  source_index: source_index.StateV1,
  cursor: cursor.CursorV1,
  raw_item_index: Int,
  target: purpose.SpendCandidateV1,
  rank: Int,
  complete: Bool,
}

pub type ProgressArgsV1 {
  input_index: Int,
  output_index: Int,
  opening: FieldOpeningV1,
  checkpoint_bytes: ByteArray,
  budget: Int,
}

pub type MintActionV1 {
  Scan { args: ProgressArgsV1 }
  Finish { input_index: Int, output_index: Int }
}

pub type SourceSelectionV1 {
  selection: SelectionV1,
  source: Option<SourceV1>,
}

pub type LookupArgsV1 {
  input_index: Int,
  output_index: Int,
  source: Option<SourceV1>,
  proof: Proof,
}

pub type SelectionDatum =
  ct.StepDatum<BoundIndexV1>

pub type SelectionRedeemer =
  ct.StepRedeemer<SelectionActionV1>

pub type SpendSelectionRedeemer =
  ct.StepRedeemer<SpendSelectionArgsV1>

pub type MintDatum =
  ct.StepDatum<MintStateV1>

pub type MintGrammarRedeemer =
  ct.StepRedeemer<ProgressArgsV1>

pub type MintScanRedeemer =
  ct.StepRedeemer<MintActionV1>

pub type SpendDatum =
  ct.StepDatum<SpendStateV1>

pub type SpendRedeemer =
  ct.StepRedeemer<MintActionV1>

pub type LookupDatum =
  ct.StepDatum<SelectionV1>

pub type LookupRedeemer =
  ct.StepRedeemer<LookupArgsV1>

pub const scan_batch: Int = 16

pub fn selection_indices_v1(action: SelectionActionV1) -> Pair<Int, Int> {
  when action is {
    Spend { input_index, output_index } -> Pair(input_index, output_index)
    Observe { input_index, output_index, .. } -> Pair(input_index, output_index)
    Mint { input_index, output_index, .. } -> Pair(input_index, output_index)
    Receive { input_index, output_index } -> Pair(input_index, output_index)
  }
}

pub fn selected_v1(
  bound: BoundIndexV1,
  coordinate: PurposeCoordinateV1,
  selected: Option<PurposeV1>,
) -> SelectionV1 {
  expect purpose.coordinate_is_valid_v1(coordinate)
  expect
    when selected is {
      Some(actual) -> actual.coordinate == coordinate
      None -> True
    }
  SelectionV1 {
    bound: bound.bound,
    source_index: bound.source_index,
    coordinate,
    purpose: selected,
  }
}

pub fn initial_mint_v1(
  source: BoundIndexV1,
  target_ordinal: Int,
  opening: FieldOpeningV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> MintStateV1 {
  let Pair(field_cursor, _) = cursor.initial_v1(
    source.bound,
    mint_field_index,
    opening,
    reference_inputs,
    certificate_policy_id,
  )
  MintStateV1 {
    source_index: source.source_index,
    scan: mint_purpose.initial_v1(field_cursor, target_ordinal),
  }
}

pub fn finish_mint_v1(state: MintStateV1) -> SelectionV1 {
  selected_v1(
    BoundIndexV1 {
      bound: state.scan.cursor.bound,
      source_index: state.source_index,
    },
    PurposeCoordinateV1 { kind: purpose.mint, index: state.scan.target_ordinal },
    mint_purpose.selected_purpose_v1(state.scan),
  )
}

/// The spend field has a fixed stride, so its cursor starts in the walking
/// phase (the field-0 branch of `cursor.initial_with_completion_v1`), and the
/// same authenticated walk view yields the selected spend credential. A key
/// credential has no script to derive and is refused.
pub fn initial_spend_v1(
  source: BoundIndexV1,
  args: SpendSelectionArgsV1,
  reference_inputs: List<Input>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  certificate_policy_id: PolicyId,
) -> SpendStateV1 {
  let Pair(view, position) = opened_field_walk(
    args.opening,
    cursor.anchor_v1(source.bound, spend_inputs_field_index),
    spend_inputs_field_index,
    reference_inputs,
    certificate_policy_id,
  )
  let credential = purpose.spend_credential_in_view_v1(
    source.bound,
    view,
    args.raw_item_index,
    args.descriptor_cbor,
    args.membership,
    reference_inputs,
    redeemers,
  )
  expect Some(script_hash) = credential.script_hash
  SpendStateV1 {
    source_index: source.source_index,
    cursor: cursor.CursorV1 {
      bound: source.bound,
      field_index: spend_inputs_field_index,
      phase: cursor.Walking,
      checkpoint_hash: walk.field_walk_checkpoint_hash(position),
    },
    raw_item_index: args.raw_item_index,
    target: purpose.SpendCandidateV1 {
      script_hash,
      subject: credential.subject,
    },
    rank: 0,
    complete: walk.walk_is_complete(position),
  }
}

/// The resolution producer sorts the union, then increments replaySpendIndex
/// for each spend. Its spend-only ordinal is the rank among field-0 outrefs,
/// regardless of raw field order. Never reinterpret raw_item_index as a pointer.
pub fn advance_spend_v1(
  state: SpendStateV1,
  args: ProgressArgsV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> SpendStateV1 {
  expect and {
      !state.complete,
      args.budget > 0,
      args.budget <= scan_batch,
    }
  let (field_cursor, rank, complete) = advance_spend_rank_v1(
    state.cursor,
    state.target.subject,
    state.rank,
    args,
    reference_inputs,
    certificate_policy_id,
  )
  SpendStateV1 { ..state, cursor: field_cursor, rank, complete }
}

/// Shared rank transition for observed script targets and claimed key/script
/// targets. The authenticated raw field contributes every spent outref.
/// Counts the walked spend items that sort before `subject`.
pub fn fold_spend_rank_v1(
  view: FieldViewV1,
  position: walk.FieldWalkCheckpointV1,
  budget: Int,
  initial_rank: Int,
  subject: ByteArray,
) -> Pair<Int, walk.FieldWalkCheckpointV1> {
  walk.walk_fold(
    view,
    position,
    budget,
    initial_rank,
    fn(rank, _, item) {
      rank + if bytearray.compare(item, subject) == Less {
        1
      } else {
        0
      }
    },
  )
}

pub fn advance_spend_rank_v1(
  field_cursor: cursor.CursorV1,
  subject: ByteArray,
  initial_rank: Int,
  args: ProgressArgsV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> (cursor.CursorV1, Int, Bool) {
  expect and {
      args.budget > 0,
      args.budget <= scan_batch,
    }
  let Pair(view, position) = cursor.resume_at_v1(
    field_cursor,
    spend_inputs_field_index,
    args.opening,
    args.checkpoint_bytes,
    reference_inputs,
    certificate_policy_id,
  )
  let Pair(rank, advanced) = fold_spend_rank_v1(
    view,
    position,
    args.budget,
    initial_rank,
    subject,
  )
  (
    cursor.after_v1(field_cursor, advanced),
    rank,
    walk.walk_is_complete(advanced),
  )
}

pub fn finish_spend_v1(state: SpendStateV1) -> SelectionV1 {
  expect state.complete
  let coordinate = PurposeCoordinateV1 {
    kind: purpose.spend,
    index: state.rank,
  }
  selected_v1(
    BoundIndexV1 { bound: state.cursor.bound, source_index: state.source_index },
    coordinate,
    Some(
      PurposeV1 {
        coordinate,
        script_hash: state.target.script_hash,
        subject: state.target.subject,
      },
    ),
  )
}

pub fn lookup_v1(
  selection: SelectionV1,
  source: Option<SourceV1>,
  proof: Proof,
) -> SourceSelectionV1 {
  expect Some(actual) = selection.purpose
  expect and {
      purpose.coordinate_is_valid_v1(selection.coordinate),
      actual.coordinate == selection.coordinate,
      source_index.lookup_v1(
        selection.source_index,
        actual.script_hash,
        source,
        proof,
      ),
    }
  SourceSelectionV1 { selection, source }
}
