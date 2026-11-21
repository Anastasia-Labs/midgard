use cardano/transaction.{Transaction}
use fraud_proofs/execution_native_script_invalid/evaluate as evaluate_step
use fraud_proofs/execution_native_script_invalid/family_entry as entry_step
use fraud_proofs/execution_native_script_invalid/lookup as lookup_step
use fraud_proofs/execution_native_script_invalid/signers as signers_step
use fraud_proofs/execution_native_script_invalid/spend_key as spend_key_step
use fraud_proofs/execution_native_script_invalid/spend_rank as spend_rank_step
use fraud_proofs/execution_native_script_invalid/terminal as terminal_step
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{RedeemerCarriedMembership}
use midgard/fraud_proofs/direct_script_derivation/claim_adjudicate.{
  Lookup, LookupScan, Resolve,
}
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/purpose_workers as workers
use midgard/fraud_proofs/direct_script_derivation/selection_types.{SelectionV1}
use midgard/fraud_proofs/direct_script_derivation/source.{SourceV1}
use midgard/fraud_proofs/direct_script_derivation/source_index
use midgard/fraud_proofs/direct_script_derivation/source_scan.{BoundIndexV1}
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1,
  PurposeV1} as dd_types
use midgard/fraud_proofs/execution_native_script_invalid/entry
use midgard/fraud_proofs/execution_native_script_invalid/evaluate.{
  Finished, PageV1, Paged,
}
use midgard/fraud_proofs/execution_native_script_invalid/fixture as ens_fixture
use midgard/fraud_proofs/execution_native_script_invalid/lookup
use midgard/fraud_proofs/execution_native_script_invalid/signers
use midgard/fraud_proofs/execution_native_script_invalid/spend_rank.{
  OpenActionV1,
}
use midgard/fraud_proofs/execution_native_script_invalid/terminal
use midgard/fraud_proofs/execution_native_script_invalid/types.{
  EvalV1, Evaluated, FactV1, FinalV1, NoExecution, SpendRankV1,
}
use midgard/fraud_proofs/field_opening_v1
use midgard/fraud_proofs/first_fault_v1.{PurposeCoordinateV1}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/native_tx_machine_walk_v1 as walk

const hash = #"33333333333333333333333333333333333333333333333333333333"

const other = #"55555555555555555555555555555555555555555555555555555555"

const stranger = #"cdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcd"

const observer_a = #"a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1"

const prover = [fixture.fraud_prover_key_hash]

fn at(kind: Int, index: Int) -> PurposeCoordinateV1 {
  PurposeCoordinateV1 { kind, index }
}

fn step_datum(state: a) -> Option<ct.StepDatum<a>> {
  Some(
    ct.StepDatum {
      fraud_prover: fixture.fraud_prover_key_hash,
      data: Some(state),
    },
  )
}

/// A step that hands `state` to the next validator.
fn next_tx(state: Data, signatures: List<ByteArray>) -> Transaction {
  Transaction {
    ..fixture.continue_mid_step_l1_tx_v1(ens_fixture.root, state),
    extra_signatories: signatures,
  }
}

/// A step that keeps `state` at its own validator.
fn own_tx(state: Data) -> Transaction {
  fixture.continue_same_step_l1_tx_v1(ens_fixture.root, state)
}

/// A forced transaction with one observer and an interval starting at slot 10.
fn tx() -> native_types.MidgardTransaction {
  let tx = fixture.valid_native_tx_v1()
  native_types.MidgardTransaction {
    ..tx,
    body: native_types.MidgardTransactionBody {
      ..tx.body,
      required_observers: [observer_a],
      validity_interval_start: 10,
      validity_interval_end: -1,
    },
  }
}

fn bound(kind: Int, index: Int) -> BoundV1 {
  let (bound, _) = ens_fixture.forced_bound(
    tx(),
    ens_fixture.recorded(kind, index),
    ens_fixture.root,
  )
  bound
}

fn indexed(bound: BoundV1) -> BoundIndexV1 {
  BoundIndexV1 { bound, source_index: source_index.initial_v1() }
}

// ## family-entry

fn entry_run(
  state: BoundIndexV1,
  action: entry.ActionV1,
  routes: (ByteArray, ByteArray, ByteArray),
  output: Data,
  signatures: List<ByteArray>,
) -> Bool {
  let (purpose_select, spend_key, terminal_hash) = routes
  entry_step.main.spend(
    purpose_select,
    spend_key,
    terminal_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    step_datum(state),
    ct.Continue(action),
    fixture.own_out_ref(),
    next_tx(output, signatures),
  )
}

const next = fixture.next_step_script_hash

test family_entry_routes_each_action_to_its_validator() {
  let observed = indexed(bound(0, 0))
  let as_is: Data = observed
  let contradicted = indexed(bound(4, 0))
  let contradiction: Data = entry.contradiction_v1(contradicted)
  let observer_beyond = indexed(bound(2, 1))
  let opening = fixture.inline_body_opening(
    observer_beyond.bound.compact_cbor,
    [observer_a],
  )
  let absent: Data = FinalV1 {
    bound: observer_beyond.bound,
    fact: NoExecution { coordinate: at(2, 1) },
  }
  let observe = entry.Observe { input_index: 0, output_index: 0 }
  let spend_key = entry.SpendKey { input_index: 0, output_index: 0 }
  let contradict = entry.Contradict { input_index: 0, output_index: 0 }
  let observe_absent = entry.ObserveAbsent {
    input_index: 0,
    output_index: 0,
    opening,
  }
  and {
    entry_run(observed, observe, (next, other, other), as_is, prover),
    !entry_run(observed, observe, (other, next, other), as_is, prover),
    entry_run(observed, spend_key, (other, next, other), as_is, prover),
    !entry_run(observed, spend_key, (next, other, other), as_is, prover),
    entry_run(
      contradicted,
      contradict,
      (other, other, next),
      contradiction,
      prover,
    ),
    !entry_run(
      contradicted,
      contradict,
      (next, other, other),
      contradiction,
      prover,
    ),
    entry_run(
      observer_beyond,
      observe_absent,
      (other, other, next),
      absent,
      prover,
    ),
  }
}

test family_entry_handoffs_cannot_alter_the_binding() {
  let observed = indexed(bound(0, 0))
  let altered: Data = indexed(bound(0, 1))
  and {
    !entry_run(
      observed,
      entry.Observe { input_index: 0, output_index: 0 },
      (next, other, other),
      altered,
      prover,
    ),
    !entry_run(
      observed,
      entry.SpendKey { input_index: 0, output_index: 0 },
      (other, next, other),
      altered,
      prover,
    ),
  }
}

test family_entry_requires_the_prover_signature() fail {
  let observed = indexed(bound(0, 0))
  let as_is: Data = observed
  entry_run(
    observed,
    entry.Observe { input_index: 0, output_index: 0 },
    (next, other, other),
    as_is,
    [stranger],
  )
}

// ## lookup

fn source(language: Int) -> SourceV1 {
  SourceV1 {
    field_index: 6,
    item_index: 0,
    source_key: #"00",
    language_tag: language,
    script_hash: hash,
    total_length: 40,
    item_commitment: ens_fixture.root,
  }
}

fn selection(entry: Option<SourceV1>, exists: Bool) -> SelectionV1 {
  SelectionV1 {
    bound: bound(1, 0),
    source_index: when entry is {
      None -> source_index.initial_v1()
      Some(candidate) ->
        source_index.admit_v1(
          source_index.initial_v1(),
          candidate,
          source_index.First { proof: [] },
        )
    },
    coordinate: at(1, 0),
    purpose: if exists {
      Some(PurposeV1 { coordinate: at(1, 0), script_hash: hash, subject: hash })
    } else {
      None
    },
  }
}

fn lookup_run(
  state: SelectionV1,
  action: lookup.SpendRedeemer,
  signers_hash: ByteArray,
  terminal_hash: ByteArray,
  output: Data,
  signatures: List<ByteArray>,
) -> Bool {
  lookup_step.main.spend(
    signers_hash,
    terminal_hash,
    fixture.computation_thread_policy_id,
    step_datum(state),
    action,
    fixture.own_out_ref(),
    next_tx(output, signatures),
  )
}

fn lookup_action(language: Int) -> lookup.SpendRedeemer {
  ct.Continue(
    Lookup {
      input_index: 0,
      output_index: 0,
      source: Some(source(language)),
      proof: [],
    },
  )
}

test lookup_hands_a_native_source_to_the_signer_set() {
  let state = selection(Some(source(0)), True)
  expect lookup.NativeSource { state: started } = lookup.lookup_v1(
    state,
    Some(source(0)),
    [],
  )
  let output: Data = started
  and {
    lookup_run(state, lookup_action(0), next, other, output, prover),
    !lookup_run(state, lookup_action(0), other, next, output, prover),
  }
}

test lookup_hands_another_language_to_the_terminal() {
  let state = selection(Some(source(3)), True)
  let output: Data = FinalV1 {
    bound: state.bound,
    fact: NoExecution { coordinate: at(1, 0) },
  }
  and {
    lookup_run(state, lookup_action(3), other, next, output, prover),
    !lookup_run(state, lookup_action(3), next, other, output, prover),
  }
}

test lookup_resolve_hands_the_absence_to_the_terminal() {
  let state = selection(None, False)
  let output: Data = lookup.resolve_v1(state)
  lookup_run(
    state,
    ct.Continue(Resolve { input_index: 0, output_index: 0 }),
    other,
    next,
    output,
    prover,
  )
}

test lookup_cannot_alter_the_fact() {
  let state = selection(Some(source(3)), True)
  let output: Data = FinalV1 {
    bound: state.bound,
    fact: Evaluated { coordinate: at(1, 0), satisfied: False },
  }
  !lookup_run(state, lookup_action(3), other, next, output, prover)
}

test lookup_scan_is_refused() fail {
  let state = selection(Some(source(0)), True)
  let output: Data = state
  lookup_run(
    state,
    ct.Continue(
      LookupScan {
        input_index: 0,
        output_index: 0,
        source: Some(source(0)),
        proof: [],
        opening: fixture.inline_body_opening(state.bound.compact_cbor, []),
      },
    ),
    next,
    next,
    output,
    prover,
  )
}

test lookup_requires_the_prover_signature() fail {
  let state = selection(Some(source(3)), True)
  let output: Data = FinalV1 {
    bound: state.bound,
    fact: NoExecution { coordinate: at(1, 0) },
  }
  lookup_run(state, lookup_action(3), other, next, output, [stranger])
}

// ## signers

fn signers_run(
  state: EvalV1,
  page: signers.PageV1,
  evaluate_hash: ByteArray,
  tx: Transaction,
) -> Bool {
  signers_step.main.spend(
    evaluate_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    step_datum(state),
    ct.Continue(page),
    fixture.own_out_ref(),
    tx,
  )
}

fn signers_page(state: EvalV1, budget: Int) -> (signers.PageV1, Bool, Data) {
  let (_, opening) = ens_fixture.signers_fixture(
    ens_fixture.witnesses(),
    ens_fixture.witnesses(),
  )
  let page = signers.PageV1 { input_index: 0, output_index: 0, opening, budget }
  let Pair(complete, next_state) = signers.page_v1(
    state,
    page,
    [],
    fixture.test_certificate_policy_id,
  )
  let output: Data = next_state
  (page, complete, output)
}

test signers_keep_a_partial_set_at_their_own_validator() {
  let (state, _) = ens_fixture.signers_fixture(
    ens_fixture.witnesses(),
    ens_fixture.witnesses(),
  )
  let (page, complete, output) = signers_page(state, 2)
  and {
    !complete,
    signers_run(state, page, next, own_tx(output)),
    !signers_run(state, page, next, next_tx(output, prover)),
  }
}

test signers_hand_a_complete_set_to_the_evaluation() {
  let (state, _) = ens_fixture.signers_fixture(
    ens_fixture.witnesses(),
    ens_fixture.witnesses(),
  )
  let (page, complete, output) = signers_page(state, 3)
  and {
    complete,
    signers_run(state, page, next, next_tx(output, prover)),
    !signers_run(state, page, other, next_tx(output, prover)),
    !signers_run(state, page, next, own_tx(output)),
  }
}

test signers_cannot_alter_the_set() {
  let (state, _) = ens_fixture.signers_fixture(
    ens_fixture.witnesses(),
    ens_fixture.witnesses(),
  )
  let (page, _, _) = signers_page(state, 3)
  let altered: Data = state
  !signers_run(state, page, next, next_tx(altered, prover))
}

test signers_require_the_prover_signature() fail {
  let (state, _) = ens_fixture.signers_fixture(
    ens_fixture.witnesses(),
    ens_fixture.witnesses(),
  )
  let (page, _, output) = signers_page(state, 3)
  signers_run(state, page, next, next_tx(output, [stranger]))
}

// ## evaluate

fn evaluation(payload: ByteArray) -> (EvalV1, PageV1) {
  let bytes = ens_fixture.item(payload)
  (
    EvalV1 {
      bound: bound(1, 0),
      coordinate: at(1, 0),
      item_commitment: ens_fixture.commitment_of(bytes),
      validity_interval_start: 10,
      validity_interval_end: -1,
      signer_count: 0,
      signer_peaks: [],
      total_length: 0,
      cursor: 0,
      stack_root: #"",
      stack_depth: 0,
      pending: None,
    },
    PageV1 {
      input_index: 0,
      output_index: 0,
      chunks: [ens_fixture.chunk_of(bytes)],
      frames: [],
      signers: [],
      budget: 1,
    },
  )
}

fn evaluate_run(
  state: EvalV1,
  page: PageV1,
  terminal_hash: ByteArray,
  tx: Transaction,
) -> Bool {
  evaluate_step.main.spend(
    terminal_hash,
    fixture.computation_thread_policy_id,
    step_datum(state),
    ct.Continue(page),
    fixture.own_out_ref(),
    tx,
  )
}

test evaluate_keeps_an_unfinished_walk_at_its_own_validator() {
  let (state, page) = evaluation(
    ens_fixture.all(
      [ens_fixture.after(1), ens_fixture.after(2), ens_fixture.after(3)],
    ),
  )
  expect Paged { state: paged } = evaluate.page_v1(state, page)
  let output: Data = paged
  and {
    evaluate_run(state, page, next, own_tx(output)),
    !evaluate_run(state, page, next, next_tx(output, prover)),
  }
}

test evaluate_hands_a_finished_walk_to_the_terminal() {
  let (state, page) = evaluation(ens_fixture.after(5))
  expect Finished { state: final } = evaluate.page_v1(state, page)
  let output: Data = final
  let forged: Data = FinalV1 {
    ..final,
    fact: Evaluated { coordinate: at(1, 0), satisfied: False },
  }
  and {
    final.fact == Evaluated { coordinate: at(1, 0), satisfied: True },
    evaluate_run(state, page, next, next_tx(output, prover)),
    !evaluate_run(state, page, next, own_tx(output)),
    !evaluate_run(state, page, next, next_tx(forged, prover)),
  }
}

test evaluate_requires_the_prover_signature() fail {
  let (state, page) = evaluation(ens_fixture.after(5))
  expect Finished { state: final } = evaluate.page_v1(state, page)
  let output: Data = final
  evaluate_run(state, page, next, next_tx(output, [stranger]))
}

// ## spend-key and spend-rank

fn open_action(budget: Int) -> (BoundIndexV1, OpenActionV1, Transaction) {
  let (state, opening, value, outref_key) = ens_fixture.key_spend_fixture(
    True,
    1,
  )
  let action = OpenActionV1 {
    input_index: 0,
    output_index: 0,
    raw_item_index: 2,
    opening,
    descriptor_cbor: value,
    membership: RedeemerCarriedMembership {
      membership_proof: [],
      membership_proof_script_redeemer_index: 0,
    },
    budget,
  }
  let redeemers = [
    fixture.phas_withdraw_redeemer_entry(
      state.bound.event_pre_root,
      outref_key,
      value,
    ),
  ]
  (state, action, Transaction { ..next_tx(#"", prover), redeemers })
}

fn spend_key_run(
  state: BoundIndexV1,
  action: OpenActionV1,
  spend_rank_hash: ByteArray,
  tx: Transaction,
  output: Data,
) -> Bool {
  let shaped = next_tx(output, tx.extra_signatories)
  spend_key_step.main.spend(
    spend_rank_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    step_datum(state),
    ct.Continue(action),
    fixture.own_out_ref(),
    Transaction { ..tx, outputs: shaped.outputs },
  )
}

test spend_key_hands_the_opened_rank_to_spend_rank() {
  let (state, action, tx) = open_action(3)
  let opened = spend_rank.open_v1(
    state,
    action,
    [],
    tx.redeemers,
    fixture.test_certificate_policy_id,
  )
  let output: Data = opened
  let altered: Data = SpendRankV1 { ..opened, rank: 0 }
  and {
    spend_key_run(state, action, next, tx, output),
    !spend_key_run(state, action, other, tx, output),
    !spend_key_run(state, action, next, tx, altered),
  }
}

test spend_key_requires_the_prover_signature() fail {
  let (state, action, tx) = open_action(3)
  let output: Data = spend_rank.open_v1(
    state,
    action,
    [],
    tx.redeemers,
    fixture.test_certificate_policy_id,
  )
  spend_key_run(
    state,
    action,
    next,
    Transaction { ..tx, extra_signatories: [stranger] },
    output,
  )
}

fn spend_rank_run(
  state: SpendRankV1,
  action: workers.MintActionV1,
  terminal_hash: ByteArray,
  tx: Transaction,
) -> Bool {
  spend_rank_step.main.spend(
    terminal_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    step_datum(state),
    ct.Continue(action),
    fixture.own_out_ref(),
    tx,
  )
}

/// The open state after a one-item page and the page that ranks the rest.
fn partial_rank() -> (SpendRankV1, workers.ProgressArgsV1) {
  let (state, action, tx) = open_action(1)
  let opened = spend_rank.open_v1(
    state,
    action,
    [],
    tx.redeemers,
    fixture.test_certificate_policy_id,
  )
  let Pair(view, position) = field_opening_v1.opened_field_walk(
    action.opening,
    cursor.anchor_v1(state.bound, 0),
    0,
    [],
    fixture.test_certificate_policy_id,
  )
  let Pair(_, after_first) = workers.fold_spend_rank_v1(
    view,
    position,
    1,
    0,
    #"",
  )
  (
    opened,
    workers.ProgressArgsV1 {
      input_index: 0,
      output_index: 0,
      opening: action.opening,
      checkpoint_bytes: walk.encode_field_walk_checkpoint(after_first),
      budget: 2,
    },
  )
}

test spend_rank_scan_stays_at_its_own_validator() {
  let (opened, args) = partial_rank()
  let advanced = spend_rank.advance_v1(
    opened,
    args,
    [],
    fixture.test_certificate_policy_id,
  )
  let output: Data = advanced
  let scan = workers.Scan { args }
  and {
    !opened.complete,
    advanced.complete,
    spend_rank_run(opened, scan, next, own_tx(output)),
    !spend_rank_run(opened, scan, next, next_tx(output, prover)),
  }
}

test spend_rank_finish_hands_the_absence_to_the_terminal() {
  let (opened, args) = partial_rank()
  let advanced = spend_rank.advance_v1(
    opened,
    args,
    [],
    fixture.test_certificate_policy_id,
  )
  let output: Data = spend_rank.finish_v1(advanced)
  let finish = workers.Finish { input_index: 0, output_index: 0 }
  and {
    spend_rank_run(advanced, finish, next, next_tx(output, prover)),
    !spend_rank_run(advanced, finish, other, next_tx(output, prover)),
    !spend_rank_run(advanced, finish, next, own_tx(output)),
  }
}

test spend_rank_requires_the_prover_signature() fail {
  let (opened, args) = partial_rank()
  let advanced = spend_rank.advance_v1(
    opened,
    args,
    [],
    fixture.test_certificate_policy_id,
  )
  let output: Data = spend_rank.finish_v1(advanced)
  spend_rank_run(
    advanced,
    workers.Finish { input_index: 0, output_index: 0 },
    next,
    next_tx(output, [stranger]),
  )
}

// ## terminal

fn finalized(state: FinalV1, signatures: List<ByteArray>) -> Bool {
  terminal_step.main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
    step_datum(state),
    ct.Continue(
      terminal.Args {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
      },
    ),
    fixture.own_out_ref(),
    Transaction {
      ..fixture.finalize_step_l1_tx_v1(state.bound.subject.transaction_id),
      extra_signatories: signatures,
    },
  )
}

fn accepted(fact: FactV1) -> FinalV1 {
  let (bound, _) = ens_fixture.forced_bound(tx(), None, ens_fixture.root)
  FinalV1 { bound, fact }
}

test terminal_mints_on_a_false_native_script_of_an_accepted_leaf() {
  finalized(
    accepted(Evaluated { coordinate: at(0, 0), satisfied: False }),
    prover,
  )
}

test terminal_mints_on_a_recorded_arm_whose_script_holds() {
  finalized(
    FinalV1 {
      bound: bound(1, 0),
      fact: Evaluated { coordinate: at(1, 0), satisfied: True },
    },
    prover,
  )
}

test terminal_refuses_an_established_arm() {
  !finalized(
    FinalV1 {
      bound: bound(1, 0),
      fact: Evaluated { coordinate: at(1, 0), satisfied: False },
    },
    prover,
  )
}

test terminal_refuses_an_accepted_leaf_whose_script_holds() {
  !finalized(
    accepted(Evaluated { coordinate: at(0, 0), satisfied: True }),
    prover,
  )
}

test terminal_finalization_is_open_to_any_submitter() {
  finalized(
    accepted(Evaluated { coordinate: at(0, 0), satisfied: False }),
    [stranger],
  )
}
