use midgard/fraud_proofs/all_purposes_scan_fixture_v1.{
  ShapeV1, WitnessesV1, accepted_normal, at, key_spend, policy, source_map,
  walk_steps, walk_tx,
}
use midgard/fraud_proofs/direct_script_derivation/all_purposes_scan as scan
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/purpose_workers.{
  ProgressArgsV1,
}
use midgard/fraud_proofs/direct_script_derivation/source_scan.{BoundIndexV1}
use midgard/fraud_proofs/unused_script_witness/fixture.{
  hash_of, native_script, plutus_script, progress, stages, witness_source,
}
use midgard/fraud_proofs/unused_script_witness/walk

// Three witnesses: two native scripts and a Plutus one. The transaction's
// only purposes are a key spend and one observer, the second witness.

fn scripts() {
  [native_script(0x31), native_script(0x32), plutus_script(0x41, 40)]
}

fn walk_state(budget: Int, script_index: Int) -> fixture.StagesV1 {
  let tx = fixture.case_tx(
    ShapeV1 {
      spends: [key_spend],
      policies: [],
      observers: [hash_of(native_script(0x32))],
      outputs: [],
    },
    accepted_normal,
    scripts(),
  )
  let Pair(index, sources) = source_map(
    [witness_source(1, native_script(0x32))],
  )
  let state = walk_tx(
    tx.1st,
    index,
    Some(hash_of(native_script(0x32))),
    WitnessesV1 {
      spend_executions: [None],
      mint: [],
      observers: [at(sources, 0)],
      receive: [],
    },
  )
  stages(state, tx.2nd, script_index, budget)
}

// ## Opening

test entry_opens_field_six_for_grammar_certification() {
  let stage = walk_state(16, 0)
  and {
    stage.opened.cursor.field_index == 6,
    stage.opened.cursor.phase == cursor.Certifying,
    stage.opened.witness_hash == None,
    !stage.opened.complete,
    stage.opened.script_index == 0,
  }
}

test certification_hands_over_a_walking_cursor() {
  let stage = walk_state(16, 0)
  and {
    stage.certified.cursor.phase == cursor.Walking,
    !stage.certified.complete,
  }
}

test certification_refuses_a_batch_past_the_bound() fail {
  let stage = walk_state(16, 0)
  walk.certify_v1(
    stage.opened,
    ProgressArgsV1 { ..stage.grammar, budget: 17 },
    [],
    policy,
  ).complete
}

// ## The walk

test walk_reads_the_first_witness_hash() {
  let stage = walk_state(16, 0)
  stage.walked.witness_hash == Some(hash_of(native_script(0x31)))
}

test walk_reads_a_plutus_witness_hash() {
  let stage = walk_state(16, 2)
  and {
    stage.walked.witness_hash == Some(hash_of(plutus_script(0x41, 40))),
    stage.walked.complete,
  }
}

test walk_stops_at_the_named_witness() {
  let stage = walk_state(16, 1)
  and {
    stage.walked.witness_hash == Some(hash_of(native_script(0x32))),
    !stage.walked.complete,
    walk.is_finished_v1(stage.walked),
  }
}

test walk_past_the_field_ends_without_a_witness() {
  let stage = walk_state(16, 3)
  and {
    stage.walked.witness_hash == None,
    stage.walked.complete,
    walk.is_finished_v1(stage.walked),
  }
}

test walk_at_a_negative_index_ends_without_a_witness() {
  let stage = walk_state(16, -1)
  and {
    stage.walked.witness_hash == None,
    stage.walked.complete,
  }
}

test walk_resumes_across_batches() {
  let stage = walk_state(1, 2)
  let first = stage.walked
  let Pair(view, position) = cursor.resume_at_v1(
    stage.certified.cursor,
    6,
    stage.scan.opening,
    stage.scan.checkpoint_bytes,
    [],
    policy,
  )
  let second = walk.advance_v1(
    first,
    progress(stage.scan.opening, walk_steps(view, position, 1), 1),
    [],
    policy,
  )
  let third = walk.advance_v1(
    second,
    progress(stage.scan.opening, walk_steps(view, position, 2), 1),
    [],
    policy,
  )
  and {
    first.witness_hash == None,
    !walk.is_finished_v1(first),
    second.witness_hash == None,
    !walk.is_finished_v1(second),
    third.witness_hash == Some(hash_of(plutus_script(0x41, 40))),
  }
}

test walk_refuses_a_stale_checkpoint() fail {
  let stage = walk_state(1, 2)
  walk.advance_v1(stage.walked, stage.scan, [], policy).complete
}

test walk_refuses_a_batch_past_the_bound() fail {
  let stage = walk_state(16, 2)
  walk.advance_v1(
    stage.certified,
    ProgressArgsV1 { ..stage.scan, budget: 17 },
    [],
    policy,
  ).complete
}

test walk_cannot_start_before_certification() fail {
  let stage = walk_state(16, 0)
  walk.advance_v1(stage.opened, stage.scan, [], policy).complete
}

test entry_requires_a_completed_purpose_walk() fail {
  let tx = fixture.case_tx(
    ShapeV1 { spends: [key_spend], policies: [], observers: [], outputs: [] },
    accepted_normal,
    scripts(),
  )
  let Pair(index, _) = source_map([witness_source(0, native_script(0x31))])
  let opened = scan.initial_v1(
    BoundIndexV1 { bound: tx.1st.bound, source_index: index },
    Some(hash_of(native_script(0x31))),
    tx.1st.spend_opening,
    [],
    policy,
  )
  walk.open_v1(opened, fixture.entry_args(tx.2nd, 0), [], policy).complete
}
