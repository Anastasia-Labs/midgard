//// A single authenticated source pass builds the map used by every purpose.
//// Ordinary batches consume adjacent items. A pending operation separates a
//// ledger opening from its map mutation when two proof envelopes do not fit
//// together; the pending candidate and advanced cursor remain in thread data.

use aiken/collection/list
use cardano/assets.{PolicyId}
use cardano/transaction.{Input}
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/source
use midgard/fraud_proofs/direct_script_derivation/source_index as index
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1}
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/native_tx_machine_walk_v1 as walk

pub type StateV1 {
  cursor: cursor.CursorV1,
  source_index: index.StateV1,
  complete: Bool,
}

pub type BoundIndexV1 {
  bound: BoundV1,
  source_index: index.StateV1,
}

pub type ResolvedInputsV1 {
  advanced: StateV1,
  first_item_index: Int,
  out_refs: List<ByteArray>,
}

pub type PendingV1 {
  advanced: StateV1,
  candidates: List<Option<source.SourceV1>>,
}

pub fn initial_v1(
  bound: BoundV1,
  opening: FieldOpeningV1,
  refs: List<Input>,
  certificate_policy: PolicyId,
) -> Pair<StateV1, ByteArray> {
  let (position, checkpoint, complete) = cursor.initial_with_completion_v1(
    bound,
    0,
    opening,
    refs,
    certificate_policy,
  )
  Pair(
    StateV1 { cursor: position, source_index: index.initial_v1(), complete },
    checkpoint,
  )
}

fn admit_candidate_v1(
  current: index.StateV1,
  candidate: Option<source.SourceV1>,
  admission: Option<index.AdmissionV1>,
) -> index.StateV1 {
  when candidate is {
    None -> {
      expect admission == None
      current
    }
    Some(candidate) -> {
      expect Some(admission) = admission
      index.admit_v1(current, candidate, admission)
    }
  }
}

/// Commit exactly the bounded adjacent input locations before ledger proofs.
pub fn select_resolved_inputs_v1(
  state: StateV1,
  opening: FieldOpeningV1,
  checkpoint: ByteArray,
  budget: Int,
  refs: List<Input>,
  certificate_policy: PolicyId,
) -> ResolvedInputsV1 {
  expect !state.complete && budget > 0
  expect state.cursor.field_index == 0 || state.cursor.field_index == 1
  let Pair(view, position) = cursor.resume_v1(
    state.cursor,
    opening,
    checkpoint,
    refs,
    certificate_policy,
  )
  let first_item_index = walk.walk_next_item_index(position)
  let Pair(out_refs, advanced) = walk.walk_fold(
    view,
    position,
    budget,
    [],
    fn(reversed, _, item) { [item, ..reversed] },
  )
  expect !list.is_empty(out_refs)
  ResolvedInputsV1 {
    advanced: StateV1 {
      ..state,
      cursor: cursor.after_v1(state.cursor, advanced),
      complete: walk.walk_is_complete(advanced),
    },
    first_item_index,
    out_refs: list.reverse(out_refs),
  }
}

fn authenticate_resolved_v1(
  root: ByteArray,
  field_index: Int,
  item_index: Int,
  out_refs: List<ByteArray>,
  witnesses: List<source.ResolvedWitnessV1>,
) -> List<Option<source.SourceV1>> {
  when out_refs is {
    [] -> {
      expect witnesses == []
      []
    }
    [out_ref, ..rest] -> {
      expect [witness, ..remaining] = witnesses
      let candidate = source.resolved_source_v1(
        root,
        field_index,
        item_index,
        out_ref,
        witness,
      )
      [
        candidate,
        ..authenticate_resolved_v1(
          root,
          field_index,
          item_index + 1,
          rest,
          remaining,
        )
      ]
    }
  }
}

/// The exact ordered location list and old map state came from the field door;
/// authenticate every descriptor against its original event root, then hand the
/// ordered optional candidates to the existing map mutation door.
pub fn authenticate_resolved_inputs_v1(
  resolved: ResolvedInputsV1,
  witnesses: List<source.ResolvedWitnessV1>,
) -> PendingV1 {
  expect resolved.first_item_index >= 0 && !list.is_empty(resolved.out_refs)
  let position = resolved.advanced.cursor
  PendingV1 {
    advanced: resolved.advanced,
    candidates: authenticate_resolved_v1(
      position.bound.event_pre_root,
      position.field_index,
      resolved.first_item_index,
      resolved.out_refs,
      witnesses,
    ),
  }
}

/// Authenticate each actual witness once before the following map mutations.
pub fn select_witness_batch_v1(
  state: StateV1,
  opening: FieldOpeningV1,
  checkpoint: ByteArray,
  budget: Int,
  refs: List<Input>,
  certificate_policy: PolicyId,
) -> PendingV1 {
  expect !state.complete && budget > 0
  let Pair(view, position) = cursor.resume_at_v1(
    state.cursor,
    6,
    opening,
    checkpoint,
    refs,
    certificate_policy,
  )
  let Pair(candidates, advanced) = walk.walk_fold(
    view,
    position,
    budget,
    [],
    fn(reversed, item_index, item) {
      [Some(source.witness_source_v1(item_index, item)), ..reversed]
    },
  )
  PendingV1 {
    advanced: StateV1 {
      ..state,
      cursor: cursor.after_v1(state.cursor, advanced),
      complete: walk.walk_is_complete(advanced),
    },
    candidates: list.reverse(candidates),
  }
}

fn admit_candidates_v1(
  current: index.StateV1,
  candidates: List<Option<source.SourceV1>>,
  admissions: List<Option<index.AdmissionV1>>,
) -> index.StateV1 {
  when candidates is {
    [] -> {
      expect admissions == []
      current
    }
    [candidate, ..rest] -> {
      expect [admission, ..remaining] = admissions
      admit_candidates_v1(
        admit_candidate_v1(current, candidate, admission),
        rest,
        remaining,
      )
    }
  }
}

/// Exact lengths and order bind every mutation to the committed batch.
pub fn admit_pending_v1(
  pending: PendingV1,
  admissions: List<Option<index.AdmissionV1>>,
) -> StateV1 {
  expect !list.is_empty(pending.candidates)
  StateV1 {
    ..pending.advanced,
    source_index: admit_candidates_v1(
      pending.advanced.source_index,
      pending.candidates,
      admissions,
    ),
  }
}

pub fn certify_witnesses_v1(
  state: StateV1,
  opening: FieldOpeningV1,
  checkpoint: ByteArray,
  budget: Int,
  refs: List<Input>,
  certificate_policy: PolicyId,
) -> Pair<StateV1, ByteArray> {
  expect !state.complete
  let (advanced, checkpoint, complete) = cursor.certify_with_completion_at_v1(
    state.cursor,
    6,
    opening,
    checkpoint,
    budget,
    refs,
    certificate_policy,
  )
  Pair(StateV1 { ..state, cursor: advanced, complete }, checkpoint)
}

/// Field order is fixed here, never selected by a caller. Completion of the
/// old field is independently reopened before the next field is initialized.
pub fn advance_field_v1(
  state: StateV1,
  opening: FieldOpeningV1,
  checkpoint: ByteArray,
  next_opening: FieldOpeningV1,
  refs: List<Input>,
  certificate_policy: PolicyId,
) -> Pair<StateV1, ByteArray> {
  let next_field = when state.cursor.field_index is {
    0 -> 1
    1 -> 6
    _ -> fail
  }
  let Pair(_, position) = cursor.resume_v1(
    state.cursor,
    opening,
    checkpoint,
    refs,
    certificate_policy,
  )
  expect walk.walk_is_complete(position)
  let (next, checkpoint, complete) = cursor.initial_with_completion_v1(
    state.cursor.bound,
    next_field,
    next_opening,
    refs,
    certificate_policy,
  )
  Pair(StateV1 { ..state, cursor: next, complete }, checkpoint)
}

pub fn finish_v1(
  state: StateV1,
  opening: FieldOpeningV1,
  checkpoint: ByteArray,
  refs: List<Input>,
  certificate_policy: PolicyId,
) -> BoundIndexV1 {
  let Pair(_, position) = cursor.resume_at_v1(
    state.cursor,
    6,
    opening,
    checkpoint,
    refs,
    certificate_policy,
  )
  expect walk.walk_is_complete(position)
  BoundIndexV1 { bound: state.cursor.bound, source_index: state.source_index }
}
