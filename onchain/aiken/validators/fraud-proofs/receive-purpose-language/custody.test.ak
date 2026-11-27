use cardano/transaction.{Transaction}
use fraud_proofs/receive_purpose_language/family_entry as entry_step
use fraud_proofs/receive_purpose_language/lookup as lookup_step
use fraud_proofs/receive_purpose_language/terminal as terminal_step
use midgard/computation_thread as ct
use midgard/fraud_proofs/direct_script_derivation/claim.{
  ClaimContextV1, FinalStateV1, ImpossiblePurpose, LeafPurpose, NotRequired,
  RedeemerPresent,
}
use midgard/fraud_proofs/direct_script_derivation/claim_adjudicate.{
  Lookup, Resolve,
}
use midgard/fraud_proofs/direct_script_derivation/claim_terminal
use midgard/fraud_proofs/direct_script_derivation/purpose_workers.{
  SourceSelectionV1,
}
use midgard/fraud_proofs/direct_script_derivation/redeemer_scan.{
  DecisionStateV1,
}
use midgard/fraud_proofs/direct_script_derivation/selection_types.{SelectionV1}
use midgard/fraud_proofs/direct_script_derivation/source.{SourceV1}
use midgard/fraud_proofs/direct_script_derivation/source_index
use midgard/fraud_proofs/direct_script_derivation/source_scan.{BoundIndexV1}
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1, PurposeV1}
use midgard/fraud_proofs/first_fault_v1.{
  NetworkOrderContextV1, PurposeCoordinateV1,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/proof_thread_substrate_v1.{VerdictSubjectV1}
use midgard/fraud_proofs/receive_purpose_language/entry
use midgard/fraud_proofs/receive_purpose_language/lookup
use midgard/rejection_reason_v1 as reasons

const hash = #"33333333333333333333333333333333333333333333333333333333"

const root = #"4444444444444444444444444444444444444444444444444444444444444444"

const other = #"55555555555555555555555555555555555555555555555555555555"

fn at(kind: Int, index: Int) -> PurposeCoordinateV1 {
  PurposeCoordinateV1 { kind, index }
}

fn source(language: Int) -> SourceV1 {
  SourceV1 {
    field_index: 6,
    item_index: 2,
    source_key: #"02",
    language_tag: language,
    script_hash: hash,
    total_length: 8,
    item_commitment: root,
  }
}

fn bound(
  reason: Option<reasons.RejectionReasonV1>,
  entry: Option<SourceV1>,
) -> BoundIndexV1 {
  BoundIndexV1 {
    bound: BoundV1 {
      subject: when reason is {
        None ->
          VerdictSubjectV1 {
            version: 1,
            direction: 0,
            source_kind: 0,
            transaction_id: root,
            source_key: #"",
            rejection_reason: None,
          }
        Some(_) ->
          VerdictSubjectV1 {
            version: 1,
            direction: 1,
            source_kind: 1,
            transaction_id: root,
            source_key: #"00",
            rejection_reason: reason,
          }
      },
      compact_cbor: #"",
      network_order_context: NetworkOrderContextV1 {
        body_network_mismatch: False,
      },
      event_pre_root: root,
    },
    source_index: when entry is {
      None -> source_index.initial_v1()
      Some(candidate) ->
        source_index.admit_v1(
          source_index.initial_v1(),
          candidate,
          source_index.First { proof: [] },
        )
    },
  }
}

fn arm(kind: Int, index: Int) -> Option<reasons.RejectionReasonV1> {
  Some(
    reasons.ReceivePurposePlutusV3Forbidden {
      purpose_kind: kind,
      purpose_index: index,
    },
  )
}

fn selected(entry: Option<SourceV1>, exists: Bool) -> SelectionV1 {
  let b = bound(None, entry)
  SelectionV1 {
    bound: b.bound,
    source_index: b.source_index,
    coordinate: at(3, 0),
    purpose: if exists {
      Some(PurposeV1 { coordinate: at(3, 0), script_hash: hash, subject: hash })
    } else {
      None
    },
  }
}

fn step_datum(state: a) -> Option<ct.StepDatum<a>> {
  Some(
    ct.StepDatum {
      fraud_prover: fixture.fraud_prover_key_hash,
      data: Some(state),
    },
  )
}

fn step_tx(state: Data, signatures: List<ByteArray>) -> Transaction {
  let tx = fixture.continue_mid_step_l1_tx_v1(root, state)
  Transaction { ..tx, extra_signatories: signatures }
}

// ## family-entry

fn entry_run(
  state: BoundIndexV1,
  action: entry.ActionV1,
  receive_start: ByteArray,
  terminal: ByteArray,
  output: Data,
  signatures: List<ByteArray>,
) -> Bool {
  entry_step.main.spend(
    receive_start,
    terminal,
    fixture.computation_thread_policy_id,
    step_datum(state),
    ct.Continue(action),
    fixture.own_out_ref(),
    step_tx(output, signatures),
  )
}

test family_entry_observe_hands_the_binding_to_the_receive_route() {
  let state = bound(None, None)
  let output: Data = state
  and {
    entry_run(
      state,
      entry.Observe { input_index: 0, output_index: 0 },
      fixture.next_step_script_hash,
      other,
      output,
      [fixture.fraud_prover_key_hash],
    ),
    !entry_run(
      state,
      entry.Observe { input_index: 0, output_index: 0 },
      other,
      fixture.next_step_script_hash,
      output,
      [fixture.fraud_prover_key_hash],
    ),
  }
}

test family_entry_observe_cannot_alter_the_binding() {
  let state = bound(None, None)
  let output: Data = bound(None, Some(source(3)))
  !entry_run(
    state,
    entry.Observe { input_index: 0, output_index: 0 },
    fixture.next_step_script_hash,
    other,
    output,
    [fixture.fraud_prover_key_hash],
  )
}

test family_entry_contradict_hands_the_coordinate_to_the_terminal() {
  let state = bound(arm(0, 0), None)
  let output: Data = entry.contradiction_v1(state)
  and {
    entry_run(
      state,
      entry.Contradict { input_index: 0, output_index: 0 },
      other,
      fixture.next_step_script_hash,
      output,
      [fixture.fraud_prover_key_hash],
    ),
    !entry_run(
      state,
      entry.Contradict { input_index: 0, output_index: 0 },
      fixture.next_step_script_hash,
      other,
      output,
      [fixture.fraud_prover_key_hash],
    ),
  }
}

test family_entry_contradict_cannot_alter_the_fact() {
  let state = bound(arm(0, 0), None)
  let output: Data = FinalStateV1 {
    context: ClaimContextV1 {
      bound: state,
      observed: None,
      target: LeafPurpose,
    },
    claim: NotRequired,
  }
  !entry_run(
    state,
    entry.Contradict { input_index: 0, output_index: 0 },
    other,
    fixture.next_step_script_hash,
    output,
    [fixture.fraud_prover_key_hash],
  )
}

test family_entry_contradict_refuses_a_receive_coordinate() fail {
  let state = bound(arm(3, 0), None)
  let output: Data = FinalStateV1 {
    context: entry.context_v1(state),
    claim: ImpossiblePurpose { coordinate: at(3, 0) },
  }
  entry_run(
    state,
    entry.Contradict { input_index: 0, output_index: 0 },
    other,
    fixture.next_step_script_hash,
    output,
    [fixture.fraud_prover_key_hash],
  )
}

test family_entry_requires_the_prover_signature() fail {
  let state = bound(None, None)
  let output: Data = state
  entry_run(
    state,
    entry.Observe { input_index: 0, output_index: 0 },
    fixture.next_step_script_hash,
    other,
    output,
    [other],
  )
}

// ## lookup

fn lookup_run(
  state: SelectionV1,
  action: lookup.SpendRedeemer,
  terminal: ByteArray,
  grammar: ByteArray,
  output: Data,
  signatures: List<ByteArray>,
) -> Bool {
  lookup_step.main.spend(
    terminal,
    grammar,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    step_datum(state),
    action,
    fixture.own_out_ref(),
    step_tx(output, signatures),
  )
}

test lookup_resolve_hands_the_fact_to_the_terminal() {
  let state = selected(None, False)
  let output: Data = lookup.resolve_v1(state)
  let action = ct.Continue(Resolve { input_index: 0, output_index: 0 })
  and {
    lookup_run(
      state,
      action,
      fixture.next_step_script_hash,
      other,
      output,
      [fixture.fraud_prover_key_hash],
    ),
    !lookup_run(
      state,
      action,
      other,
      fixture.next_step_script_hash,
      output,
      [fixture.fraud_prover_key_hash],
    ),
  }
}

test lookup_hands_a_non_plutus_v3_source_to_the_terminal() {
  let state = selected(Some(source(0)), True)
  let output: Data = lookup.lookup_v1(state, Some(source(0)), [])
  let action = ct.Continue(
    Lookup {
      input_index: 0,
      output_index: 0,
      source: Some(source(0)),
      proof: [],
    },
  )
  and {
    lookup_run(
      state,
      action,
      fixture.next_step_script_hash,
      other,
      output,
      [fixture.fraud_prover_key_hash],
    ),
    !lookup_run(
      state,
      action,
      other,
      fixture.next_step_script_hash,
      output,
      [fixture.fraud_prover_key_hash],
    ),
  }
}

test lookup_cannot_alter_the_fact() {
  let state = selected(None, False)
  let expected = lookup.resolve_v1(state)
  let output: Data = FinalStateV1 { ..expected, claim: NotRequired }
  !lookup_run(
    state,
    ct.Continue(Resolve { input_index: 0, output_index: 0 }),
    fixture.next_step_script_hash,
    other,
    output,
    [fixture.fraud_prover_key_hash],
  )
}

test lookup_cannot_finish_a_plutus_v3_source() fail {
  let state = selected(Some(source(3)), True)
  let output: Data = FinalStateV1 {
    context: entry.context_v1(bound(None, Some(source(3)))),
    claim: NotRequired,
  }
  lookup_run(
    state,
    ct.Continue(
      Lookup {
        input_index: 0,
        output_index: 0,
        source: Some(source(3)),
        proof: [],
      },
    ),
    fixture.next_step_script_hash,
    other,
    output,
    [fixture.fraud_prover_key_hash],
  )
}

test lookup_requires_the_prover_signature() fail {
  let state = selected(None, False)
  let output: Data = lookup.resolve_v1(state)
  lookup_run(
    state,
    ct.Continue(Resolve { input_index: 0, output_index: 0 }),
    fixture.next_step_script_hash,
    other,
    output,
    [other],
  )
}

// ## terminal

fn final(
  reason: Option<reasons.RejectionReasonV1>,
  claim: claim.ClaimedPredicateFactV1,
) -> FinalStateV1 {
  FinalStateV1 {
    context: entry.context_v1(bound(reason, Some(source(3)))),
    claim,
  }
}

fn present() -> claim.ClaimedPredicateFactV1 {
  let b = bound(None, Some(source(3)))
  RedeemerPresent {
    decision: DecisionStateV1 {
      authenticated: SourceSelectionV1 {
        selection: SelectionV1 {
          bound: b.bound,
          source_index: b.source_index,
          coordinate: at(3, 0),
          purpose: Some(
            PurposeV1 { coordinate: at(3, 0), script_hash: hash, subject: hash },
          ),
        },
        source: Some(source(3)),
      },
      redeemer_missing: False,
    },
  }
}

fn terminal_run(state: FinalStateV1) -> Bool {
  terminal_step.main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
    step_datum(state),
    ct.Continue(
      claim_terminal.Args {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(root),
  )
}

test terminal_mints_on_a_forbidden_execution_of_an_accepted_leaf() {
  terminal_run(final(None, present()))
}

test terminal_mints_on_a_recorded_arm_naming_no_receive_purpose() {
  terminal_run(final(arm(0, 0), ImpossiblePurpose { coordinate: at(0, 0) }))
}

test terminal_refuses_an_established_arm() {
  !terminal_run(final(arm(3, 0), present()))
}
