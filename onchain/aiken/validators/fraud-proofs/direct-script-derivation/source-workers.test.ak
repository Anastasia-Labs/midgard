use aiken/collection/list
use env
use fraud_proofs/direct_script_derivation/source_map as map_validator
use midgard/computation_thread as ct
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/first_fault_seam.{
  NetworkOrderContextV1,
}
use midgard/fraud_proofs/direct_script_derivation/source_index as index
use midgard/fraud_proofs/direct_script_derivation/source_scan as scan
use midgard/fraud_proofs/direct_script_derivation/source_workers as worker
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1}
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{
  encode_midgard_tx_input, encode_midgard_versioned_script,
}
use midgard/fraud_proofs/native_tx/types as native
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/native_tx_machine_walk_v1 as walk

// The real signed source prefix covers every positive physical stage. These
// cheapest-level controls isolate None admission and independently reopened
// exhaustion so a later refactor cannot trust a caller's complete flag.
fn bound_and_opening(
  empty: Bool,
  witness: Bool,
) -> Pair<BoundV1, FieldOpeningV1> {
  let original = fixture.valid_native_tx_v1()
  let tx = native.MidgardTransaction {
    ..original,
    body: native.MidgardTransactionBody {
      ..original.body,
      inputs: if empty {
        []
      } else {
        original.body.inputs
      },
    },
  }
  let block = fixture.native_block_fixture_v1(tx)
  let bound = BoundV1 {
    subject: substrate.VerdictSubjectV1 {
      version: 1,
      direction: 0,
      source_kind: 0,
      transaction_id: block.tx_id,
      source_key: #"",
      rejection_reason: None,
    },
    compact_cbor: block.compact_cbor,
    network_order_context: NetworkOrderContextV1 {
      body_network_mismatch: False,
    },
    event_pre_root: env.empty_merkle_tree_root,
  }
  let opening = if witness {
    fixture.inline_witness_opening(
      block.compact_cbor,
      block.witness_set_compact,
      list.map(tx.witness_set.script_tx_wits, encode_midgard_versioned_script),
    )
  } else {
    fixture.inline_body_opening(
      block.compact_cbor,
      list.map(tx.body.inputs, encode_midgard_tx_input),
    )
  }
  Pair(bound, opening)
}

fn input_state(empty: Bool) -> (scan.StateV1, ByteArray, FieldOpeningV1) {
  let Pair(bound, opening) = bound_and_opening(empty, False)
  let Pair(state, checkpoint) = scan.initial_v1(
    bound,
    opening,
    [],
    fixture.test_certificate_policy_id,
  )
  (state, checkpoint, opening)
}

fn witness_state(advance: Bool) -> (scan.StateV1, ByteArray, FieldOpeningV1) {
  let Pair(bound, opening) = bound_and_opening(False, True)
  let Pair(initial, checkpoint) = cursor.initial_v1(
    bound,
    6,
    opening,
    [],
    fixture.test_certificate_policy_id,
  )
  let Pair(ready, checkpoint) = cursor.certify_v1(
    initial,
    opening,
    checkpoint,
    1,
    [],
    fixture.test_certificate_policy_id,
  )
  let state = scan.StateV1 {
    cursor: ready,
    source_index: index.initial_v1(),
    complete: False,
  }
  if advance {
    let pending = scan.select_witness_batch_v1(
      state,
      opening,
      checkpoint,
      1,
      [],
      fixture.test_certificate_policy_id,
    )
    let state = scan.admit_pending_v1(
      pending,
      [Some(index.First { proof: [] })],
    )
    let Pair(view, position) = cursor.resume_v1(
      ready,
      opening,
      checkpoint,
      [],
      fixture.test_certificate_policy_id,
    )
    let Pair(_, advanced) = walk.walk_next(view, position)
    (state, walk.encode_field_walk_checkpoint(advanced), opening)
  } else {
    (state, checkpoint, opening)
  }
}

fn applied_map(fresh_none: Bool, wrong_resume: Bool) -> Bool {
  let (state, _, _) = input_state(False)
  let pending = worker.PendingStateV1 {
    resume_script_hash: if wrong_resume {
      fixture.step_script_hash
    } else {
      fixture.next_step_script_hash
    },
    pending: scan.PendingV1 { advanced: state, candidates: [None] },
  }
  let expected: Data = state
  map_validator.main.spend(
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: if fresh_none {
          None
        } else {
          Some(pending)
        },
      },
    ),
    ct.Continue(
      worker.MapArgs { input_index: 0, output_index: 0, admissions: [None] },
    ),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(
      state.cursor.bound.subject.transaction_id,
      expected,
    ),
  )
}

test applied_map_accepts_exact_committed_no_script_none() {
  applied_map(False, False)
}

test applied_map_refuses_a_different_committed_resume_destination() {
  !applied_map(False, True)
}

test applied_map_cannot_invent_pending_custody_from_fresh_init_none() fail {
  applied_map(True, False)
}

fn reference_opening(state: scan.StateV1) -> FieldOpeningV1 {
  let tx = fixture.valid_native_tx_v1()
  fixture.inline_body_opening(
    state.cursor.bound.compact_cbor,
    list.map(tx.body.reference_inputs, encode_midgard_tx_input),
  )
}

test input_exhaustion_reopens_an_authentic_empty_field_before_field_one() {
  let (state, checkpoint, opening) = input_state(True)
  // The next field is nonempty, so use its independently committed field bytes.
  let next = reference_opening(state)
  let Pair(advanced, _) = scan.advance_field_v1(
    state,
    opening,
    checkpoint,
    next,
    [],
    fixture.test_certificate_policy_id,
  )
  advanced.cursor.field_index == 1
}

test input_exhaustion_cannot_trust_a_false_flag_before_actual_exhaustion() fail {
  let (state, checkpoint, opening) = input_state(False)
  let Pair(advanced, _) = scan.advance_field_v1(
    state,
    opening,
    checkpoint,
    reference_opening(state),
    [],
    fixture.test_certificate_policy_id,
  )
  advanced.cursor.field_index == 1
}

test input_exhaustion_cannot_trust_a_forged_true_flag_before_actual_exhaustion() fail {
  let (state, checkpoint, opening) = input_state(False)
  let Pair(advanced, _) = scan.advance_field_v1(
    scan.StateV1 { ..state, complete: True },
    opening,
    checkpoint,
    reference_opening(state),
    [],
    fixture.test_certificate_policy_id,
  )
  advanced.cursor.field_index == 1
}

test witness_finish_accepts_an_authentic_consumed_and_admitted_source() {
  let (state, checkpoint, opening) = witness_state(True)
  scan.finish_v1(
    state,
    opening,
    checkpoint,
    [],
    fixture.test_certificate_policy_id,
  ).source_index.count == 1
}

test witness_finish_refuses_a_false_flag_before_actual_exhaustion() fail {
  let (state, checkpoint, opening) = witness_state(False)
  scan.finish_v1(
    state,
    opening,
    checkpoint,
    [],
    fixture.test_certificate_policy_id,
  ).source_index.count == 0
}

test witness_finish_refuses_a_forged_true_flag_before_actual_exhaustion() fail {
  let (state, checkpoint, opening) = witness_state(False)
  scan.finish_v1(
    scan.StateV1 { ..state, complete: True },
    opening,
    checkpoint,
    [],
    fixture.test_certificate_policy_id,
  ).source_index.count == 0
}

test witness_finish_refuses_a_substituted_checkpoint_after_actual_exhaustion() fail {
  let (state, _, opening) = witness_state(True)
  scan.finish_v1(state, opening, #"00", [], fixture.test_certificate_policy_id).source_index.count == 1
}
