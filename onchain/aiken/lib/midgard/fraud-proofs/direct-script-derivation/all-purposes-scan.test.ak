use aiken/builtin
use aiken/collection/list
use midgard/fraud_proofs/all_purposes_scan_fixture_v1.{
  ShapeV1, WitnessesV1, a, at, b, c, facts_of, key_spend, no_purposes, none,
  opened_checkpoint, opened_variable, policy, protected_script_output, resume,
  source_map, source_of, spend_witnesses, spent_descriptor, spent_input, tx_of,
  unprotected_script_output, walked,
}
use midgard/fraud_proofs/direct_script_derivation/all_purposes_scan as scan
use midgard/fraud_proofs/direct_script_derivation/source.{SourceV1}
use midgard/fraud_proofs/direct_script_derivation/source_index
use midgard/fraud_proofs/direct_script_derivation/source_scan.{BoundIndexV1}
use midgard/fraud_proofs/field_opening_v1.{
  mint_field_index, required_observers_field_index, spend_inputs_field_index,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_input}
use midgard/fraud_proofs/native_tx/types as native_types

// ## Every purpose kind

test key_locked_spends_and_no_purposes_need_no_language() {
  facts_of(no_purposes(), [source_of(a, 3)], Some(a), none) == scan.FactsV1 {
    language_bitmap: 0,
    probe: Some(a),
    probe_reached: False,
  }
}

test script_locked_spend_admits_its_source_language() {
  let facts = facts_of(
    ShapeV1 {
      ..no_purposes(),
      spends: [key_spend, native_types.ScriptCredential(a)],
    },
    [source_of(a, 3)],
    Some(a),
    fn(e) {
      WitnessesV1 { ..none(e), spend_executions: [None, Some(at(e, 0))] }
    },
  )
  facts == scan.FactsV1 {
    language_bitmap: 1,
    probe: Some(a),
    probe_reached: True,
  }
}

test mint_policy_admits_its_source_language() {
  let facts = facts_of(
    ShapeV1 { ..no_purposes(), policies: [b] },
    [source_of(b, 128)],
    Some(b),
    fn(e) { WitnessesV1 { ..none(e), mint: [at(e, 0)] } },
  )
  facts == scan.FactsV1 {
    language_bitmap: 2,
    probe: Some(b),
    probe_reached: True,
  }
}

test required_observer_admits_its_source_language() {
  let facts = facts_of(
    ShapeV1 { ..no_purposes(), observers: [c] },
    [source_of(c, 3)],
    Some(c),
    fn(e) { WitnessesV1 { ..none(e), observers: [at(e, 0)] } },
  )
  facts == scan.FactsV1 {
    language_bitmap: 1,
    probe: Some(c),
    probe_reached: True,
  }
}

test protected_script_output_admits_its_receive_language() {
  let facts = facts_of(
    ShapeV1 {
      ..no_purposes(),
      outputs: [fixture.sample_native_output(), protected_script_output(a)],
    },
    [source_of(a, 128)],
    Some(a),
    fn(e) { WitnessesV1 { ..none(e), receive: [None, Some(at(e, 0))] } },
  )
  facts == scan.FactsV1 {
    language_bitmap: 2,
    probe: Some(a),
    probe_reached: True,
  }
}

test unprotected_script_output_is_not_a_receive_purpose() {
  facts_of(
    ShapeV1 { ..no_purposes(), outputs: [unprotected_script_output(a)] },
    [source_of(a, 3)],
    Some(a),
    fn(e) { WitnessesV1 { ..none(e), receive: [None] } },
  ) == scan.FactsV1 { language_bitmap: 0, probe: Some(a), probe_reached: False }
}

test unprotected_script_output_cannot_carry_an_execution() fail {
  facts_of(
    ShapeV1 { ..no_purposes(), outputs: [unprotected_script_output(a)] },
    [source_of(a, 3)],
    None,
    fn(e) { WitnessesV1 { ..none(e), receive: [Some(at(e, 0))] } },
  ).language_bitmap == 1
}

test all_four_purpose_kinds_fold_both_languages() {
  let facts = facts_of(
    ShapeV1 {
      spends: [native_types.ScriptCredential(a), key_spend],
      policies: [b],
      observers: [a],
      outputs: [protected_script_output(b)],
    },
    [source_of(a, 3), source_of(b, 128)],
    None,
    fn(e) {
      WitnessesV1 {
        spend_executions: [Some(at(e, 0)), None],
        mint: [at(e, 1)],
        observers: [at(e, 0)],
        receive: [Some(at(e, 1))],
      }
    },
  )
  facts == scan.FactsV1 {
    language_bitmap: 3,
    probe: None,
    probe_reached: False,
  }
}

// ## Native against Plutus

test native_source_adds_no_language_but_reaches_the_probe() {
  facts_of(
    ShapeV1 { ..no_purposes(), observers: [c] },
    [source_of(c, 0)],
    Some(c),
    fn(e) { WitnessesV1 { ..none(e), observers: [at(e, 0)] } },
  ) == scan.FactsV1 { language_bitmap: 0, probe: Some(c), probe_reached: True }
}

test native_execution_is_reported_native() {
  and {
    scan.is_native_v1(scan.ExecutionV1 { script_hash: a, language_tag: 0 }),
    !scan.is_native_v1(scan.ExecutionV1 { script_hash: a, language_tag: 3 }),
    !scan.is_native_v1(scan.ExecutionV1 { script_hash: a, language_tag: 128 }),
  }
}

test repeated_languages_set_each_bit_once() {
  let facts = list.foldl(
    [3, 3, 128, 0, 128, 3],
    scan.FactsV1 { language_bitmap: 0, probe: None, probe_reached: False },
    fn(tag, facts) {
      scan.admit_execution_v1(
        facts,
        scan.ExecutionV1 { script_hash: a, language_tag: tag },
      )
    },
  )
  facts.language_bitmap == 3
}

test probe_reached_stays_reached() {
  let facts = list.foldl(
    [a, b],
    scan.FactsV1 { language_bitmap: 0, probe: Some(a), probe_reached: False },
    fn(hash, facts) {
      scan.admit_execution_v1(
        facts,
        scan.ExecutionV1 { script_hash: hash, language_tag: 0 },
      )
    },
  )
  facts.probe_reached
}

// ## Source authentication

test execution_with_another_hash_source_is_refused() fail {
  facts_of(
    ShapeV1 { ..no_purposes(), observers: [c] },
    [source_of(a, 0)],
    None,
    fn(e) { WitnessesV1 { ..none(e), observers: [at(e, 0)] } },
  ).language_bitmap == 0
}

test execution_with_a_substituted_language_is_refused() fail {
  let Pair(index, executions) = source_map([source_of(c, 0)])
  let honest = at(executions, 0)
  scan.execution_v1(
    index,
    c,
    scan.ExecutionWitnessV1 {
      ..honest,
      source: SourceV1 { ..honest.source, language_tag: 3 },
    },
  ).language_tag == 3
}

test script_spend_without_an_execution_is_refused() fail {
  facts_of(
    ShapeV1 { ..no_purposes(), spends: [native_types.ScriptCredential(a)] },
    [source_of(a, 3)],
    None,
    fn(e) { WitnessesV1 { ..none(e), spend_executions: [None] } },
  ).language_bitmap == 0
}

test key_spend_with_an_execution_is_refused() fail {
  facts_of(
    no_purposes(),
    [source_of(a, 3)],
    None,
    fn(e) { WitnessesV1 { ..none(e), spend_executions: [Some(at(e, 0))] } },
  ).language_bitmap == 1
}

test spend_descriptor_must_be_the_ledger_value() fail {
  let shape = ShapeV1 {
    ..no_purposes(),
    spends: [native_types.ScriptCredential(a)],
  }
  let tx = tx_of(shape)
  let Pair(index, _executions) = source_map([source_of(a, 3)])
  let state = scan.initial_v1(
    BoundIndexV1 { bound: tx.bound, source_index: index },
    None,
    tx.spend_opening,
    [],
    policy,
  )
  let pending = scan.take_spends_batch_v1(
    state,
    resume,
    tx.spend_opening,
    opened_checkpoint(tx.bound, spend_inputs_field_index, tx.spend_opening),
    1,
    [],
    policy,
  )
  let Pair(_, proof) = builtin.head_list(tx.descriptors)
  scan.resolve_spends_v1(
    pending,
    [
      scan.SpendWitnessV1 {
        descriptor_cbor: spent_descriptor(0, key_spend),
        proof,
        execution: None,
      },
    ],
  ).complete
}

// ## Batch boundaries

fn two_spends() -> ShapeV1 {
  ShapeV1 {
    ..no_purposes(),
    spends: [native_types.ScriptCredential(a), native_types.ScriptCredential(b)],
  }
}

fn spends_in_batches(first: Int) -> scan.StateV1 {
  let tx = tx_of(two_spends())
  let Pair(index, executions) = source_map([source_of(a, 3), source_of(b, 128)])
  let witnesses = spend_witnesses(
    tx,
    [Some(at(executions, 0)), Some(at(executions, 1))],
  )
  let state = scan.initial_v1(
    BoundIndexV1 { bound: tx.bound, source_index: index },
    None,
    tx.spend_opening,
    [],
    policy,
  )
  let start = opened_checkpoint(
    tx.bound,
    spend_inputs_field_index,
    tx.spend_opening,
  )
  let half = scan.resolve_spends_v1(
    scan.take_spends_batch_v1(
      state,
      resume,
      tx.spend_opening,
      start,
      first,
      [],
      policy,
    ),
    list.take(witnesses, first),
  )
  if first == 2 {
    half
  } else {
    expect !half.complete
    scan.resolve_spends_v1(
      scan.take_spends_batch_v1(
        half,
        resume,
        tx.spend_opening,
        walked(state.cursor, tx.spend_opening, start, first),
        2 - first,
        [],
        policy,
      ),
      list.drop(witnesses, first),
    )
  }
}

test spend_batches_of_one_and_two_reach_the_same_state() {
  let split = spends_in_batches(1)
  let whole = spends_in_batches(2)
  and {
    split.complete,
    split == whole,
    whole.facts.language_bitmap == 3,
  }
}

test spend_batch_records_the_resume_hash_and_pending_items() {
  let tx = tx_of(two_spends())
  let state = scan.initial_v1(
    BoundIndexV1 { bound: tx.bound, source_index: source_index.initial_v1() },
    None,
    tx.spend_opening,
    [],
    policy,
  )
  let pending = scan.take_spends_batch_v1(
    state,
    resume,
    tx.spend_opening,
    opened_checkpoint(tx.bound, spend_inputs_field_index, tx.spend_opening),
    2,
    [],
    policy,
  )
  and {
    pending.resume_script_hash == resume,
    pending.pending == [
      encode_midgard_tx_input(spent_input(0)),
      encode_midgard_tx_input(spent_input(1)),
    ],
    pending.state.complete,
    !state.complete,
  }
}

test spend_batch_cannot_be_empty() fail {
  let tx = tx_of(two_spends())
  let state = scan.initial_v1(
    BoundIndexV1 { bound: tx.bound, source_index: source_index.initial_v1() },
    None,
    tx.spend_opening,
    [],
    policy,
  )
  scan.take_spends_batch_v1(
    state,
    resume,
    tx.spend_opening,
    opened_checkpoint(tx.bound, spend_inputs_field_index, tx.spend_opening),
    0,
    [],
    policy,
  ).state.complete
}

test spend_batch_cannot_pass_the_field_end() fail {
  let tx = tx_of(two_spends())
  let state = scan.initial_v1(
    BoundIndexV1 { bound: tx.bound, source_index: source_index.initial_v1() },
    None,
    tx.spend_opening,
    [],
    policy,
  )
  scan.take_spends_batch_v1(
    state,
    resume,
    tx.spend_opening,
    opened_checkpoint(tx.bound, spend_inputs_field_index, tx.spend_opening),
    3,
    [],
    policy,
  ).state.complete
}

test resolve_refuses_fewer_witnesses_than_pending_items() fail {
  let tx = tx_of(two_spends())
  let Pair(index, executions) = source_map([source_of(a, 3), source_of(b, 128)])
  let state = scan.initial_v1(
    BoundIndexV1 { bound: tx.bound, source_index: index },
    None,
    tx.spend_opening,
    [],
    policy,
  )
  scan.resolve_spends_v1(
    scan.take_spends_batch_v1(
      state,
      resume,
      tx.spend_opening,
      opened_checkpoint(tx.bound, spend_inputs_field_index, tx.spend_opening),
      2,
      [],
      policy,
    ),
    list.take(
      spend_witnesses(tx, [Some(at(executions, 0)), Some(at(executions, 1))]),
      1,
    ),
  ).complete
}

fn mint_in_batches(first: Int) -> scan.StateV1 {
  let shape = ShapeV1 { ..no_purposes(), policies: [a, b] }
  let tx = tx_of(shape)
  let Pair(index, executions) = source_map([source_of(a, 3), source_of(b, 0)])
  let state = scan.initial_v1(
    BoundIndexV1 { bound: tx.bound, source_index: index },
    None,
    tx.spend_opening,
    [],
    policy,
  )
  let state = scan.resolve_spends_v1(
    scan.take_spends_batch_v1(
      state,
      resume,
      tx.spend_opening,
      opened_checkpoint(tx.bound, spend_inputs_field_index, tx.spend_opening),
      1,
      [],
      policy,
    ),
    spend_witnesses(tx, [None]),
  )
  let Pair(walking, start) = opened_variable(
    state,
    spend_inputs_field_index,
    mint_field_index,
    tx.mint_opening,
  )
  let witnesses = [at(executions, 0), at(executions, 1)]
  let half = scan.advance_mint_v1(
    walking,
    tx.mint_opening,
    start,
    list.take(witnesses, first),
    [],
    policy,
  )
  if first == 2 {
    half
  } else {
    expect !half.complete
    scan.advance_mint_v1(
      half,
      tx.mint_opening,
      walked(walking.cursor, tx.mint_opening, start, first),
      list.drop(witnesses, first),
      [],
      policy,
    )
  }
}

test mint_batches_of_one_and_two_reach_the_same_state() {
  let split = mint_in_batches(1)
  and {
    split.complete,
    split == mint_in_batches(2),
    split.facts.language_bitmap == 1,
  }
}

test mint_batch_cannot_be_empty() fail {
  mint_in_batches(0).complete
}

// ## Field order

test next_field_cannot_open_before_the_previous_is_exhausted() fail {
  let tx = tx_of(two_spends())
  let state = scan.initial_v1(
    BoundIndexV1 { bound: tx.bound, source_index: source_index.initial_v1() },
    None,
    tx.spend_opening,
    [],
    policy,
  )
  scan.open_next_v1(
    state,
    spend_inputs_field_index,
    mint_field_index,
    tx.mint_opening,
    [],
    policy,
  ).complete
}

test next_field_cannot_skip_a_purpose_field() fail {
  let tx = tx_of(ShapeV1 { ..no_purposes(), spends: [] })
  let state = scan.initial_v1(
    BoundIndexV1 { bound: tx.bound, source_index: source_index.initial_v1() },
    None,
    tx.spend_opening,
    [],
    policy,
  )
  scan.open_next_v1(
    state,
    mint_field_index,
    required_observers_field_index,
    tx.observer_opening,
    [],
    policy,
  ).complete
}

test result_requires_the_receive_field() fail {
  let tx = tx_of(ShapeV1 { ..no_purposes(), spends: [] })
  let state = scan.initial_v1(
    BoundIndexV1 { bound: tx.bound, source_index: source_index.initial_v1() },
    None,
    tx.spend_opening,
    [],
    policy,
  )
  scan.result_v1(state).facts.language_bitmap == 0
}

test empty_spend_field_is_complete_at_opening() {
  let tx = tx_of(ShapeV1 { ..no_purposes(), spends: [] })
  scan.initial_v1(
    BoundIndexV1 { bound: tx.bound, source_index: source_index.initial_v1() },
    None,
    tx.spend_opening,
    [],
    policy,
  ).complete
}

test honest_execution_reports_its_source_language() {
  let Pair(index, executions) = source_map([source_of(c, 128)])
  scan.execution_v1(index, c, at(executions, 0)) == scan.ExecutionV1 {
    script_hash: c,
    language_tag: 128,
  }
}

test probe_is_not_reached_by_another_script() {
  facts_of(
    ShapeV1 { ..no_purposes(), observers: [c] },
    [source_of(c, 3)],
    Some(a),
    fn(e) { WitnessesV1 { ..none(e), observers: [at(e, 0)] } },
  ) == scan.FactsV1 { language_bitmap: 1, probe: Some(a), probe_reached: False }
}
