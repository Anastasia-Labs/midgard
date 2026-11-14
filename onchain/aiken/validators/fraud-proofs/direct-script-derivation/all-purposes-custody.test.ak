use aiken/collection/list
use cardano/transaction.{Transaction}
use fraud_proofs/direct_script_derivation/all_purposes_mint as mint_validator
use fraud_proofs/direct_script_derivation/all_purposes_mint_grammar as mint_grammar_validator
use fraud_proofs/direct_script_derivation/all_purposes_observe as observe_validator
use fraud_proofs/direct_script_derivation/all_purposes_observe_open as observe_open_validator
use fraud_proofs/direct_script_derivation/all_purposes_receive as receive_validator
use fraud_proofs/direct_script_derivation/all_purposes_receive_grammar as receive_grammar_validator
use fraud_proofs/direct_script_derivation/all_purposes_spend as spend_validator
use fraud_proofs/direct_script_derivation/all_purposes_spend_resolve as resolve_validator
use midgard/computation_thread as ct
use midgard/fraud_proofs/all_purposes_scan_fixture_v1.{
  ShapeV1, TxV1, a, at, b, key_spend, opened_checkpoint, policy,
  protected_script_output, source_map, source_of, spend_witnesses, tx_of, walked,
}
use midgard/fraud_proofs/direct_script_derivation/all_purposes_scan as scan
use midgard/fraud_proofs/direct_script_derivation/all_purposes_workers as worker
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/source_scan.{BoundIndexV1}
use midgard/fraud_proofs/field_opening_v1.{
  FieldOpeningV1, mint_field_index, outputs_field_index,
  required_observers_field_index, spend_inputs_field_index,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/types as native_types

// Each walk validator advances the thread by exactly the scan step its
// redeemer names, writes that state, and routes it: to itself while its field
// has items left or its grammar is uncertified, otherwise to the next field's
// door. Only the thread's prover may move it.

const own = fixture.step_script_hash

const next = fixture.next_step_script_hash

const elsewhere = #"abababababababababababababababababababababababababababab"

const stranger = #"cdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcd"

const ct_policy = fixture.computation_thread_policy_id

// ## The walk under custody

/// Every purpose kind once, and two spends: a script-locked one and a
/// key-locked one.
fn every_purpose(policies: List<ByteArray>) -> ShapeV1 {
  ShapeV1 {
    spends: [native_types.ScriptCredential(a), key_spend],
    policies,
    observers: [a],
    outputs: [protected_script_output(b)],
  }
}

fn no_optional_purposes() -> ShapeV1 {
  ShapeV1 { spends: [key_spend], policies: [], observers: [], outputs: [] }
}

type StagesV1 {
  tx: TxV1,
  sources: List<scan.ExecutionWitnessV1>,
  start: scan.StateV1,
  spend_checkpoint: ByteArray,
  spent: scan.StateV1,
  mint_opened: scan.StateV1,
  mint_grammar: ByteArray,
  mint_ready: scan.StateV1,
  mint_checkpoint: ByteArray,
  minted: scan.StateV1,
  observed_open: scan.StateV1,
  observer_checkpoint: ByteArray,
  observed: scan.StateV1,
  outputs_opened: scan.StateV1,
  outputs_grammar: ByteArray,
  outputs_ready: scan.StateV1,
  outputs_checkpoint: ByteArray,
}

fn grammar_checkpoint(
  tx: TxV1,
  field_index: Int,
  opening: FieldOpeningV1,
) -> ByteArray {
  let (_, checkpoint, _) = cursor.initial_with_completion_v1(
    tx.bound,
    field_index,
    opening,
    [],
    policy,
  )
  checkpoint
}

fn semantic_checkpoint(
  opened: scan.StateV1,
  field_index: Int,
  opening: FieldOpeningV1,
  grammar: ByteArray,
) -> ByteArray {
  let (_, checkpoint, _) = cursor.certify_with_completion_at_v1(
    opened.cursor,
    field_index,
    opening,
    grammar,
    1000,
    [],
    policy,
  )
  checkpoint
}

/// The states a prover moves the thread through, one batch per field.
fn stages(shape: ShapeV1) -> StagesV1 {
  let tx = tx_of(shape)
  let Pair(index, sources) = source_map([source_of(a, 3), source_of(b, 128)])
  let start = scan.initial_v1(
    BoundIndexV1 { bound: tx.bound, source_index: index },
    None,
    tx.spend_opening,
    [],
    policy,
  )
  let spend_checkpoint = opened_checkpoint(
    tx.bound,
    spend_inputs_field_index,
    tx.spend_opening,
  )
  let spent = scan.resolve_spends_v1(
    scan.take_spends_batch_v1(
      start,
      own,
      tx.spend_opening,
      spend_checkpoint,
      list.length(tx.descriptors),
      [],
      policy,
    ),
    spend_witnesses(
      tx,
      if list.length(tx.descriptors) == 2 {
        [Some(at(sources, 0)), None]
      } else {
        [None]
      },
    ),
  )
  let mint_opened = scan.open_next_v1(
    spent,
    spend_inputs_field_index,
    mint_field_index,
    tx.mint_opening,
    [],
    policy,
  )
  let mint_grammar = grammar_checkpoint(tx, mint_field_index, tx.mint_opening)
  let mint_ready = scan.certify_v1(
    mint_opened,
    mint_field_index,
    tx.mint_opening,
    mint_grammar,
    1000,
    [],
    policy,
  )
  let mint_checkpoint = semantic_checkpoint(
    mint_opened,
    mint_field_index,
    tx.mint_opening,
    mint_grammar,
  )
  let minted = if mint_ready.complete {
    mint_ready
  } else {
    scan.advance_mint_v1(
      mint_ready,
      tx.mint_opening,
      mint_checkpoint,
      mint_sources(sources, shape),
      [],
      policy,
    )
  }
  let observed_open = scan.open_next_v1(
    minted,
    mint_field_index,
    required_observers_field_index,
    tx.observer_opening,
    [],
    policy,
  )
  let observer_checkpoint = opened_checkpoint(
    tx.bound,
    required_observers_field_index,
    tx.observer_opening,
  )
  let observed = if observed_open.complete {
    observed_open
  } else {
    scan.advance_observers_v1(
      observed_open,
      tx.observer_opening,
      observer_checkpoint,
      [at(sources, 0)],
      [],
      policy,
    )
  }
  let outputs_opened = scan.open_next_v1(
    observed,
    required_observers_field_index,
    outputs_field_index,
    tx.output_opening,
    [],
    policy,
  )
  let outputs_grammar = grammar_checkpoint(
    tx,
    outputs_field_index,
    tx.output_opening,
  )
  let outputs_ready = scan.certify_v1(
    outputs_opened,
    outputs_field_index,
    tx.output_opening,
    outputs_grammar,
    1000,
    [],
    policy,
  )
  StagesV1 {
    tx,
    sources,
    start,
    spend_checkpoint,
    spent,
    mint_opened,
    mint_grammar,
    mint_ready,
    mint_checkpoint,
    minted,
    observed_open,
    observer_checkpoint,
    observed,
    outputs_opened,
    outputs_grammar,
    outputs_ready,
    outputs_checkpoint: semantic_checkpoint(
      outputs_opened,
      outputs_field_index,
      tx.output_opening,
      outputs_grammar,
    ),
  }
}

/// Policy `a` executes source 0, policy `b` source 1.
fn mint_sources(
  sources: List<scan.ExecutionWitnessV1>,
  shape: ShapeV1,
) -> List<scan.ExecutionWitnessV1> {
  when shape.policies is {
    [] -> []
    [p] ->
      if p == a {
        [at(sources, 0)]
      } else {
        [at(sources, 1)]
      }
    _ -> [at(sources, 0), at(sources, 1)]
  }
}

// ## The L1 move

type MoveV1 {
  ToOwn
  ToNext
}

fn moved(s: StagesV1, to: MoveV1, state: Data, signer: ByteArray) -> Transaction {
  let id = s.tx.bound.subject.transaction_id
  let tx = when to is {
    ToOwn -> fixture.continue_same_step_l1_tx_v1(id, state)
    ToNext -> fixture.continue_mid_step_l1_tx_v1(id, state)
  }
  Transaction { ..tx, extra_signatories: [signer] }
}

fn datum(state: a) -> Option<ct.StepDatum<a>> {
  Some(
    ct.StepDatum {
      fraud_prover: fixture.fraud_prover_key_hash,
      data: Some(state),
    },
  )
}

const prover = fixture.fraud_prover_key_hash

// ## Spend take

fn spend_take(
  route: ByteArray,
  count: Int,
  to: MoveV1,
  forged: Bool,
  signer: ByteArray,
) -> Bool {
  let s = stages(every_purpose([b]))
  let taken = scan.take_spends_batch_v1(
    s.start,
    own,
    s.tx.spend_opening,
    s.spend_checkpoint,
    count,
    [],
    policy,
  )
  let written = if forged {
    scan.PendingSpendsV1 { ..taken, resume_script_hash: elsewhere }
  } else {
    taken
  }
  spend_validator.main.spend(
    route,
    ct_policy,
    policy,
    datum(s.start),
    ct.Continue(
      worker.SpendTakeArgsV1 {
        input_index: 0,
        output_index: 0,
        opening: s.tx.spend_opening,
        checkpoint_bytes: s.spend_checkpoint,
        count,
      },
    ),
    fixture.own_out_ref(),
    moved(s, to, written, signer),
  )
}

test spend_take_hands_its_batch_to_the_resolver() {
  spend_take(next, 2, ToNext, False, prover)
}

test spend_take_records_its_own_hash_to_resume_at() {
  !spend_take(next, 2, ToNext, True, prover)
}

test spend_take_refuses_to_stay_in_place() {
  !spend_take(own, 1, ToNext, False, prover) && !spend_take(
    next,
    1,
    ToOwn,
    False,
    prover,
  )
}

test spend_take_refuses_an_empty_batch() fail {
  spend_take(next, 0, ToNext, False, prover)
}

test spend_take_refuses_a_stranger() fail {
  spend_take(next, 2, ToNext, False, stranger)
}

// ## Spend resolve

fn spend_resolve(
  first: Int,
  mint_grammar: ByteArray,
  to: MoveV1,
  forged: Bool,
  signer: ByteArray,
) -> Bool {
  let s = stages(every_purpose([b]))
  let pending = scan.take_spends_batch_v1(
    s.start,
    next,
    s.tx.spend_opening,
    s.spend_checkpoint,
    first,
    [],
    policy,
  )
  let witnesses = spend_witnesses(s.tx, [Some(at(s.sources, 0)), None])
    |> take(first)
  let resolved = scan.resolve_spends_v1(pending, witnesses)
  let written = if forged {
    scan.StateV1 {
      ..resolved,
      facts: scan.FactsV1 { ..resolved.facts, language_bitmap: 0 },
    }
  } else {
    resolved
  }
  resolve_validator.main.spend(
    mint_grammar,
    ct_policy,
    datum(pending),
    ct.Continue(
      worker.SpendResolveArgsV1 { input_index: 0, output_index: 0, witnesses },
    ),
    fixture.own_out_ref(),
    moved(s, to, written, signer),
  )
}

fn take(items: List<a>, n: Int) -> List<a> {
  if n == 0 {
    []
  } else {
    when items is {
      [] -> []
      [x, ..rest] -> [x, ..take(rest, n - 1)]
    }
  }
}

test spend_resolve_returns_an_unfinished_field_to_the_taker() {
  spend_resolve(1, elsewhere, ToNext, False, prover)
}

test spend_resolve_hands_an_exhausted_field_to_the_mint_door() {
  spend_resolve(2, next, ToNext, False, prover)
}

test spend_resolve_refuses_the_wrong_door() {
  !spend_resolve(2, elsewhere, ToNext, False, prover)
}

test spend_resolve_refuses_a_forged_language_set() {
  !spend_resolve(2, next, ToNext, True, prover)
}

test spend_resolve_refuses_a_stranger() fail {
  spend_resolve(2, next, ToNext, False, stranger)
}

// ## Mint grammar door

type DoorMoveV1 {
  input: scan.StateV1,
  output: scan.StateV1,
  action: worker.GrammarActionV1,
}

fn mint_door(
  shape: ShapeV1,
  certify: Bool,
  scan_hash: ByteArray,
  observe_hash: ByteArray,
  to: MoveV1,
  signer: ByteArray,
) -> Bool {
  let s = stages(shape)
  let DoorMoveV1 { input, output, action } = if certify {
    DoorMoveV1 {
      input: s.mint_opened,
      output: s.mint_ready,
      action: worker.Certify {
        input_index: 0,
        output_index: 0,
        opening: s.tx.mint_opening,
        checkpoint_bytes: s.mint_grammar,
        budget: 1000,
      },
    }
  } else {
    DoorMoveV1 {
      input: s.spent,
      output: s.mint_opened,
      action: worker.Open {
        input_index: 0,
        output_index: 0,
        opening: s.tx.mint_opening,
      },
    }
  }
  mint_grammar_validator.main.spend(
    scan_hash,
    observe_hash,
    ct_policy,
    policy,
    datum(input),
    ct.Continue(action),
    fixture.own_out_ref(),
    moved(s, to, output, signer),
  )
}

test mint_door_opening_stays_to_certify() {
  mint_door(every_purpose([b]), False, elsewhere, elsewhere, ToOwn, prover)
}

test mint_door_certified_field_goes_to_the_mint_walk() {
  mint_door(every_purpose([b]), True, next, elsewhere, ToNext, prover)
}

test mint_door_empty_field_goes_to_the_observer_door() {
  mint_door(no_optional_purposes(), True, elsewhere, next, ToNext, prover)
}

test mint_door_refuses_the_wrong_route() {
  and {
    !mint_door(every_purpose([b]), False, elsewhere, elsewhere, ToNext, prover),
    !mint_door(every_purpose([b]), True, elsewhere, next, ToNext, prover),
    !mint_door(no_optional_purposes(), True, next, elsewhere, ToNext, prover),
  }
}

test mint_door_refuses_to_open_an_unfinished_spend_field() fail {
  let s = stages(every_purpose([b]))
  mint_grammar_validator.main.spend(
    next,
    next,
    ct_policy,
    policy,
    datum(s.start),
    ct.Continue(
      worker.Open {
        input_index: 0,
        output_index: 0,
        opening: s.tx.mint_opening,
      },
    ),
    fixture.own_out_ref(),
    moved(s, ToOwn, s.mint_opened, prover),
  )
}

test mint_door_refuses_a_stranger() fail {
  mint_door(every_purpose([b]), True, next, elsewhere, ToNext, stranger)
}

// ## Mint walk

fn mint_walk(
  first: Int,
  observe_hash: ByteArray,
  to: MoveV1,
  forged: Bool,
  signer: ByteArray,
) -> Bool {
  let s = stages(every_purpose([a, b]))
  let witnesses = take([at(s.sources, 0), at(s.sources, 1)], first)
  let advanced = scan.advance_mint_v1(
    s.mint_ready,
    s.tx.mint_opening,
    s.mint_checkpoint,
    witnesses,
    [],
    policy,
  )
  let written = if forged {
    scan.StateV1 { ..advanced, complete: !advanced.complete }
  } else {
    advanced
  }
  mint_validator.main.spend(
    observe_hash,
    ct_policy,
    policy,
    datum(s.mint_ready),
    ct.Continue(
      worker.ExecutionArgsV1 {
        input_index: 0,
        output_index: 0,
        opening: s.tx.mint_opening,
        checkpoint_bytes: s.mint_checkpoint,
        witnesses,
      },
    ),
    fixture.own_out_ref(),
    moved(s, to, written, signer),
  )
}

test mint_walk_partial_batch_stays() {
  mint_walk(1, elsewhere, ToOwn, False, prover)
}

test mint_walk_exhausted_field_goes_to_the_observer_door() {
  mint_walk(2, next, ToNext, False, prover)
}

test mint_walk_refuses_the_wrong_route() {
  !mint_walk(1, own, ToNext, False, prover) && !mint_walk(
    2,
    next,
    ToOwn,
    False,
    prover,
  )
}

test mint_walk_refuses_a_forged_completion() {
  !mint_walk(1, elsewhere, ToOwn, True, prover)
}

test mint_walk_refuses_a_stranger() fail {
  mint_walk(2, next, ToNext, False, stranger)
}

// ## Observer door and walk

fn observer_door(
  shape: ShapeV1,
  scan_hash: ByteArray,
  receive_hash: ByteArray,
  signer: ByteArray,
) -> Bool {
  let s = stages(shape)
  observe_open_validator.main.spend(
    scan_hash,
    receive_hash,
    ct_policy,
    policy,
    datum(s.minted),
    ct.Continue(
      worker.OpenArgsV1 {
        input_index: 0,
        output_index: 0,
        opening: s.tx.observer_opening,
      },
    ),
    fixture.own_out_ref(),
    moved(s, ToNext, s.observed_open, signer),
  )
}

test observer_door_nonempty_field_goes_to_the_observer_walk() {
  observer_door(every_purpose([b]), next, elsewhere, prover)
}

test observer_door_empty_field_goes_to_the_receive_door() {
  observer_door(no_optional_purposes(), elsewhere, next, prover)
}

test observer_door_refuses_the_wrong_route() {
  !observer_door(every_purpose([b]), elsewhere, next, prover) && !observer_door(
    no_optional_purposes(),
    next,
    elsewhere,
    prover,
  )
}

test observer_door_refuses_a_stranger() fail {
  observer_door(every_purpose([b]), next, elsewhere, stranger)
}

fn observer_walk(
  receive_hash: ByteArray,
  witness: Int,
  forged: Bool,
  signer: ByteArray,
) -> Bool {
  let s = stages(every_purpose([b]))
  let witnesses = [at(s.sources, witness)]
  let advanced = scan.advance_observers_v1(
    s.observed_open,
    s.tx.observer_opening,
    s.observer_checkpoint,
    witnesses,
    [],
    policy,
  )
  let written = if forged {
    scan.StateV1 {
      ..advanced,
      facts: scan.FactsV1 { ..advanced.facts, language_bitmap: 2 },
    }
  } else {
    advanced
  }
  observe_validator.main.spend(
    receive_hash,
    ct_policy,
    policy,
    datum(s.observed_open),
    ct.Continue(
      worker.ExecutionArgsV1 {
        input_index: 0,
        output_index: 0,
        opening: s.tx.observer_opening,
        checkpoint_bytes: s.observer_checkpoint,
        witnesses,
      },
    ),
    fixture.own_out_ref(),
    moved(s, ToNext, written, signer),
  )
}

test observer_walk_exhausted_field_goes_to_the_receive_door() {
  observer_walk(next, 0, False, prover)
}

test observer_walk_refuses_the_wrong_route() {
  !observer_walk(elsewhere, 0, False, prover)
}

test observer_walk_refuses_a_forged_language_set() {
  !observer_walk(next, 0, True, prover)
}

test observer_walk_refuses_another_scripts_source() fail {
  observer_walk(next, 1, False, prover)
}

test observer_walk_refuses_a_stranger() fail {
  observer_walk(next, 0, False, stranger)
}

// ## Receive door and walk

fn receive_door(
  shape: ShapeV1,
  certify: Bool,
  scan_hash: ByteArray,
  next_hash: ByteArray,
  to: MoveV1,
  signer: ByteArray,
) -> Bool {
  let s = stages(shape)
  let DoorMoveV1 { input, output, action } = if certify {
    DoorMoveV1 {
      input: s.outputs_opened,
      output: s.outputs_ready,
      action: worker.Certify {
        input_index: 0,
        output_index: 0,
        opening: s.tx.output_opening,
        checkpoint_bytes: s.outputs_grammar,
        budget: 1000,
      },
    }
  } else {
    DoorMoveV1 {
      input: s.observed,
      output: s.outputs_opened,
      action: worker.Open {
        input_index: 0,
        output_index: 0,
        opening: s.tx.output_opening,
      },
    }
  }
  receive_grammar_validator.main.spend(
    scan_hash,
    next_hash,
    ct_policy,
    policy,
    datum(input),
    ct.Continue(action),
    fixture.own_out_ref(),
    moved(s, to, output, signer),
  )
}

test receive_door_opening_stays_to_certify() {
  receive_door(every_purpose([b]), False, elsewhere, elsewhere, ToOwn, prover)
}

test receive_door_certified_field_goes_to_the_receive_walk() {
  receive_door(every_purpose([b]), True, next, elsewhere, ToNext, prover)
}

test receive_door_empty_field_completes_the_walk() {
  receive_door(no_optional_purposes(), True, elsewhere, next, ToNext, prover)
}

test receive_door_refuses_the_wrong_route() {
  and {
    !receive_door(
      every_purpose([b]),
      False,
      elsewhere,
      elsewhere,
      ToNext,
      prover,
    ),
    !receive_door(every_purpose([b]), True, elsewhere, next, ToNext, prover),
    !receive_door(no_optional_purposes(), True, next, elsewhere, ToNext, prover),
  }
}

test receive_door_refuses_a_stranger() fail {
  receive_door(every_purpose([b]), True, next, elsewhere, ToNext, stranger)
}

fn receive_walk(
  next_hash: ByteArray,
  witness: Option<Int>,
  to: MoveV1,
  signer: ByteArray,
) -> Bool {
  let s = stages(every_purpose([b]))
  let witnesses = [
    when witness is {
      Some(i) -> Some(at(s.sources, i))
      None -> None
    },
  ]
  let advanced = scan.advance_receive_v1(
    s.outputs_ready,
    s.tx.output_opening,
    s.outputs_checkpoint,
    witnesses,
    [],
    policy,
  )
  receive_validator.main.spend(
    next_hash,
    ct_policy,
    policy,
    datum(s.outputs_ready),
    ct.Continue(
      worker.ReceiveArgsV1 {
        input_index: 0,
        output_index: 0,
        opening: s.tx.output_opening,
        checkpoint_bytes: s.outputs_checkpoint,
        witnesses,
      },
    ),
    fixture.own_out_ref(),
    moved(s, to, advanced, signer),
  )
}

test receive_walk_exhausted_field_completes_the_walk() {
  receive_walk(next, Some(1), ToNext, prover)
}

test receive_walk_refuses_the_wrong_route() {
  !receive_walk(elsewhere, Some(1), ToNext, prover) && !receive_walk(
    next,
    Some(1),
    ToOwn,
    prover,
  )
}

test receive_walk_refuses_a_missing_receive_execution() fail {
  receive_walk(next, None, ToNext, prover)
}

test receive_walk_refuses_a_stranger() fail {
  receive_walk(next, Some(1), ToNext, stranger)
}

// ## The completed walk

test completed_walk_folds_every_purpose_kind() {
  let s = stages(every_purpose([b]))
  let state = scan.advance_receive_v1(
    s.outputs_ready,
    s.tx.output_opening,
    s.outputs_checkpoint,
    [Some(at(s.sources, 1))],
    [],
    policy,
  )
  scan.result_v1(state).facts.language_bitmap == 3
}

test walked_checkpoint_resumes_a_second_spend_batch() {
  let s = stages(every_purpose([b]))
  let first = scan.resolve_spends_v1(
    scan.take_spends_batch_v1(
      s.start,
      own,
      s.tx.spend_opening,
      s.spend_checkpoint,
      1,
      [],
      policy,
    ),
    take(spend_witnesses(s.tx, [Some(at(s.sources, 0)), None]), 1),
  )
  let second = scan.take_spends_batch_v1(
    first,
    own,
    s.tx.spend_opening,
    walked(s.start.cursor, s.tx.spend_opening, s.spend_checkpoint, 1),
    1,
    [],
    policy,
  )
  second.state.complete
}
