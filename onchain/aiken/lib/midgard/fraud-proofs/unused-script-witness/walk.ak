//// The field-6 walk of `unusedScriptWitness`. Once the all-purposes walk is
//// complete, the prover names one script witness by its field-6 index. The
//// walk certifies the field's grammar, steps over every item before that
//// index by its extent alone, and at that index reads the item and records its
//// script hash. It ends with that hash, or with the field exhausted before the
//// index. Constructor and field order below is wire format.

use cardano/assets.{PolicyId}
use cardano/transaction.{Input}
use midgard/computation_thread as ct
use midgard/fraud_proofs/direct_script_derivation/all_purposes_scan as scan
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/purpose_workers.{
  ProgressArgsV1, scan_batch,
}
use midgard/fraud_proofs/field_opening_v1.{
  FieldOpeningV1, script_witnesses_field_index,
}
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_versioned_script_at,
}
use midgard/native_tx_field_access_v1.{FieldViewV1, field_read_range}
use midgard/native_tx_machine_walk_v1 as walk
use midgard/script_proof_v1

/// Constr 0 [result, script_index, cursor, witness_hash, complete].
/// `witness_hash`: the script hash of the item at `script_index`, once read.
/// `complete`: the field is exhausted.
pub type WalkStateV1 {
  result: scan.ResultV1,
  script_index: Int,
  cursor: cursor.CursorV1,
  witness_hash: Option<ByteArray>,
  complete: Bool,
}

pub type EntryArgsV1 {
  input_index: Int,
  output_index: Int,
  script_index: Int,
  opening: FieldOpeningV1,
}

pub type EntryDatum =
  ct.StepDatum<scan.StateV1>

pub type EntryRedeemer =
  ct.StepRedeemer<EntryArgsV1>

pub type WalkDatum =
  ct.StepDatum<WalkStateV1>

pub type GrammarRedeemer =
  ct.StepRedeemer<ProgressArgsV1>

pub type ScanRedeemer =
  ct.StepRedeemer<ProgressArgsV1>

/// Takes the completed all-purposes walk and opens field 6 under its binding.
/// The field is variable width, so the walk starts by certifying its grammar.
pub fn open_v1(
  state: scan.StateV1,
  args: EntryArgsV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> WalkStateV1 {
  let result = scan.result_v1(state)
  let (field_cursor, _, complete) = cursor.initial_with_completion_v1(
    result.bound,
    script_witnesses_field_index,
    args.opening,
    reference_inputs,
    certificate_policy_id,
  )
  WalkStateV1 {
    result,
    script_index: args.script_index,
    cursor: field_cursor,
    witness_hash: None,
    complete,
  }
}

/// One bounded batch of field-6 grammar certification.
pub fn certify_v1(
  state: WalkStateV1,
  args: ProgressArgsV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> WalkStateV1 {
  expect args.budget <= scan_batch
  let (advanced, _, complete) = cursor.certify_with_completion_at_v1(
    state.cursor,
    script_witnesses_field_index,
    args.opening,
    args.checkpoint_bytes,
    args.budget,
    reference_inputs,
    certificate_policy_id,
  )
  WalkStateV1 { ..state, cursor: advanced, complete }
}

/// The script hash of one field-6 item. The source walk already decoded every
/// field-6 item of this binding and required its canonical, fully consumed
/// encoding (`source.witness_source_v1`, from
/// `source_scan.select_witness_batch_v1`), so that is not repeated here.
pub fn item_hash_v1(item: ByteArray) -> ByteArray {
  let Pair(_, script) = decode_midgard_versioned_script_at(item, 0)
  script_proof_v1.versioned_script_hash(script)
}

fn fold_v1(
  view: FieldViewV1,
  position: walk.FieldWalkCheckpointV1,
  budget: Int,
  state: WalkStateV1,
) -> Pair<WalkStateV1, walk.FieldWalkCheckpointV1> {
  if budget == 0 || walk.walk_is_complete(position) {
    Pair(state, position)
  } else {
    let item_index = walk.walk_next_item_index(position)
    let Pair(Pair(offset, length), advanced) = walk.walk_next_extent(
      view,
      position,
    )
    if item_index == state.script_index {
      Pair(
        WalkStateV1 {
          ..state,
          witness_hash: Some(
            item_hash_v1(field_read_range(view, offset, length)),
          ),
        },
        advanced,
      )
    } else {
      fold_v1(view, advanced, budget - 1, state)
    }
  }
}

/// One bounded batch of the witness walk, from the committed checkpoint. The
/// batch stops at the named witness; the scan role then hands the walk to the
/// terminal, so no batch runs after it.
pub fn advance_v1(
  state: WalkStateV1,
  args: ProgressArgsV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> WalkStateV1 {
  expect args.budget <= scan_batch
  let Pair(view, position) = cursor.resume_at_v1(
    state.cursor,
    script_witnesses_field_index,
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

/// Whether the walk has its answer: the named witness's hash, or the field
/// exhausted before it.
pub fn is_finished_v1(state: WalkStateV1) -> Bool {
  state.witness_hash != None || state.complete
}
