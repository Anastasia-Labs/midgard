use aiken/collection/list
use cardano/transaction.{Transaction}
use fraud_proofs/unused_redeemer/family_entry
use fraud_proofs/unused_redeemer/redeemer_grammar
use fraud_proofs/unused_redeemer/redeemer_scan
use midgard/computation_thread as ct
use midgard/fraud_proofs/direct_script_derivation/claim
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/purpose_workers as workers
use midgard/fraud_proofs/direct_script_derivation/source.{SourceV1}
use midgard/fraud_proofs/direct_script_derivation/source_index
use midgard/fraud_proofs/direct_script_derivation/source_scan.{BoundIndexV1}
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1}
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/first_fault_v1.{NetworkOrderContextV1}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_redeemer_witness}
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/fraud_proofs/proof_thread_substrate_v1.{VerdictSubjectV1}
use midgard/fraud_proofs/unused_redeemer/walk

const hash = #"33333333333333333333333333333333333333333333333333333333"

const root = #"4444444444444444444444444444444444444444444444444444444444444444"

const other = #"55555555555555555555555555555555555555555555555555555555"

// ## Fixtures
//
// One required observer, `hash`, with its source in the index. Every
// redeemer is a reward redeemer, so pointer `(3, 0)` names that observer.

fn reward(index: Int) -> native_types.MidgardRedeemerWitness {
  native_types.MidgardRedeemerWitness {
    purpose: native_types.RewardRedeemer,
    index,
    redeemer_cbor: #"00",
    execution_units: native_types.MidgardExecutionUnits { memory: 1, steps: 2 },
  }
}

fn block(
  witnesses: List<native_types.MidgardRedeemerWitness>,
) -> fixture.NativeBlockFixtureV1 {
  let tx = fixture.valid_native_tx_v1()
  fixture.native_block_fixture_v1(
    native_types.MidgardTransaction {
      ..tx,
      body: native_types.MidgardTransactionBody {
        ..tx.body,
        required_observers: [hash],
      },
      witness_set: native_types.MidgardTransactionWitnessSet {
        ..tx.witness_set,
        redeemer_tx_wits: witnesses,
      },
    },
  )
}

fn bound(fixture_block: fixture.NativeBlockFixtureV1) -> BoundIndexV1 {
  BoundIndexV1 {
    bound: BoundV1 {
      subject: VerdictSubjectV1 {
        version: 1,
        direction: 0,
        source_kind: 0,
        transaction_id: fixture_block.tx_id,
        source_key: #"",
        rejection_reason: None,
      },
      compact_cbor: fixture_block.compact_cbor,
      network_order_context: NetworkOrderContextV1 {
        body_network_mismatch: False,
      },
      event_pre_root: root,
    },
    source_index: source_index.admit_v1(
      source_index.initial_v1(),
      SourceV1 {
        field_index: 6,
        item_index: 2,
        source_key: #"02",
        language_tag: 3,
        script_hash: hash,
        total_length: 8,
        item_commitment: root,
      },
      source_index.First { proof: [] },
    ),
  }
}

fn opening(
  fixture_block: fixture.NativeBlockFixtureV1,
  witnesses: List<native_types.MidgardRedeemerWitness>,
) -> FieldOpeningV1 {
  fixture.inline_witness_opening(
    fixture_block.compact_cbor,
    fixture_block.witness_set_compact,
    list.map(witnesses, encode_midgard_redeemer_witness),
  )
}

fn progress(
  field: FieldOpeningV1,
  bytes: ByteArray,
  budget: Int,
) -> workers.ProgressArgsV1 {
  workers.ProgressArgsV1 {
    input_index: 0,
    output_index: 0,
    opening: field,
    checkpoint_bytes: bytes,
    budget,
  }
}

fn entry_args(field: FieldOpeningV1, redeemer_index: Int) -> walk.EntryArgsV1 {
  walk.EntryArgsV1 {
    input_index: 0,
    output_index: 0,
    redeemer_index,
    pointer_tag: 3,
    pointer_index: 0,
    opening: field,
  }
}

/// Every stage of a walk over `witnesses` that names `redeemer_index` with
/// pointer `(3, 0)`: the binding, the entry arguments, the opened state, the
/// grammar checkpoint, the certified state, the walk checkpoint and the
/// walked state.
type Stages {
  state: BoundIndexV1,
  entry: walk.EntryArgsV1,
  opened: walk.WalkStateV1,
  grammar: workers.ProgressArgsV1,
  certified: walk.WalkStateV1,
  scan: workers.ProgressArgsV1,
  walked: walk.WalkStateV1,
}

fn stages(
  witnesses: List<native_types.MidgardRedeemerWitness>,
  redeemer_index: Int,
) -> Stages {
  let fixture_block = block(witnesses)
  let state = bound(fixture_block)
  let field = opening(fixture_block, witnesses)
  let entry = entry_args(field, redeemer_index)
  let opened = walk.open_v1(
    state,
    entry,
    [],
    fixture.test_certificate_policy_id,
  )
  let (_, grammar_bytes, _) = cursor.initial_redeemers_v1(
    state.bound,
    field,
    [],
    fixture.test_certificate_policy_id,
  )
  let grammar = progress(field, grammar_bytes, 16)
  let certified = walk.certify_v1(
    opened,
    grammar,
    [],
    fixture.test_certificate_policy_id,
  )
  let (_, scan_bytes, _) = cursor.certify_with_completion_at_v1(
    opened.cursor,
    8,
    field,
    grammar_bytes,
    16,
    [],
    fixture.test_certificate_policy_id,
  )
  let scan = progress(field, scan_bytes, 16)
  let walked = walk.advance_v1(
    certified,
    scan,
    [],
    fixture.test_certificate_policy_id,
  )
  Stages { state, entry, opened, grammar, certified, scan, walked }
}

fn step_datum(state: a) -> Option<ct.StepDatum<a>> {
  Some(
    ct.StepDatum {
      fraud_prover: fixture.fraud_prover_key_hash,
      data: Some(state),
    },
  )
}

fn step_tx(state: Data, signatures: List<ByteArray>, same: Bool) -> Transaction {
  let tx = if same {
    fixture.continue_same_step_l1_tx_v1(root, state)
  } else {
    fixture.continue_mid_step_l1_tx_v1(root, state)
  }
  Transaction { ..tx, extra_signatories: signatures }
}

// ## family-entry

fn entry_run(
  stage: Stages,
  output: Data,
  same: Bool,
  signatures: List<ByteArray>,
) -> Bool {
  family_entry.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    step_datum(stage.state),
    ct.Continue(stage.entry),
    fixture.own_out_ref(),
    step_tx(output, signatures, same),
  )
}

test entry_hands_the_opened_walk_to_grammar_certification() {
  let stage = stages([reward(0)], 0)
  entry_run(stage, stage.opened, False, [fixture.fraud_prover_key_hash])
}

test entry_cannot_name_another_redeemer() {
  let stage = stages([reward(0)], 0)
  let changed: Data = walk.WalkStateV1 { ..stage.opened, redeemer_index: 1 }
  !entry_run(stage, changed, False, [fixture.fraud_prover_key_hash])
}

test entry_cannot_start_a_walk_already_repeated() {
  let stage = stages([reward(0)], 0)
  let changed: Data = walk.WalkStateV1 { ..stage.opened, repeated: True }
  !entry_run(stage, changed, False, [fixture.fraud_prover_key_hash])
}

test entry_cannot_stay_at_the_entry_script() {
  let stage = stages([reward(0)], 0)
  !entry_run(stage, stage.opened, True, [fixture.fraud_prover_key_hash])
}

test entry_requires_the_prover_signature() fail {
  let stage = stages([reward(0)], 0)
  entry_run(stage, stage.opened, False, [])
}

test entry_refuses_a_stranger_signature() fail {
  let stage = stages([reward(0)], 0)
  entry_run(stage, stage.opened, False, [other])
}

// ## redeemer-grammar

fn grammar_run(
  stage: Stages,
  args: workers.ProgressArgsV1,
  output: Data,
  same: Bool,
  signatures: List<ByteArray>,
) -> Bool {
  redeemer_grammar.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    step_datum(stage.opened),
    ct.Continue(args),
    fixture.own_out_ref(),
    step_tx(output, signatures, same),
  )
}

test grammar_hands_a_certified_field_to_the_pointer_walk() {
  let stage = stages([reward(0)], 0)
  and {
    stage.certified.cursor.phase == cursor.Walking,
    grammar_run(
      stage,
      stage.grammar,
      stage.certified,
      False,
      [fixture.fraud_prover_key_hash],
    ),
  }
}

test grammar_cannot_keep_a_certified_field() {
  let stage = stages([reward(0)], 0)
  !grammar_run(
    stage,
    stage.grammar,
    stage.certified,
    True,
    [fixture.fraud_prover_key_hash],
  )
}

test grammar_cannot_skip_to_a_walked_state() {
  let stage = stages([reward(0)], 0)
  !grammar_run(
    stage,
    stage.grammar,
    stage.walked,
    False,
    [fixture.fraud_prover_key_hash],
  )
}

test grammar_refuses_a_batch_past_the_bound() fail {
  let stage = stages([reward(0)], 0)
  grammar_run(
    stage,
    workers.ProgressArgsV1 { ..stage.grammar, budget: 17 },
    stage.certified,
    False,
    [fixture.fraud_prover_key_hash],
  )
}

test grammar_requires_the_prover_signature() fail {
  let stage = stages([reward(0)], 0)
  grammar_run(stage, stage.grammar, stage.certified, False, [])
}

// ## redeemer-scan

fn scan_run(
  input: walk.WalkStateV1,
  action: walk.ScanActionV1,
  output: Data,
  same: Bool,
  signatures: List<ByteArray>,
) -> Bool {
  redeemer_scan.main.spend(
    fixture.next_step_script_hash,
    other,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    step_datum(input),
    ct.Continue(action),
    fixture.own_out_ref(),
    step_tx(output, signatures, same),
  )
}

fn terminal_run(
  input: walk.WalkStateV1,
  output: Data,
  signatures: List<ByteArray>,
) -> Bool {
  redeemer_scan.main.spend(
    other,
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    step_datum(input),
    ct.Continue(walk.Finish { input_index: 0, output_index: 0 }),
    fixture.own_out_ref(),
    step_tx(output, signatures, False),
  )
}

fn finish() -> walk.ScanActionV1 {
  walk.Finish { input_index: 0, output_index: 0 }
}

test scan_batch_keeps_the_walk_at_the_scan_script() {
  let stage = stages([reward(0)], 0)
  scan_run(
    stage.certified,
    walk.Scan { args: stage.scan },
    stage.walked,
    True,
    [fixture.fraud_prover_key_hash],
  )
}

test scan_batch_cannot_leave_the_scan_script() {
  let stage = stages([reward(0)], 0)
  !scan_run(
    stage.certified,
    walk.Scan { args: stage.scan },
    stage.walked,
    False,
    [fixture.fraud_prover_key_hash],
  )
}

test scan_batch_cannot_claim_a_repeat_it_did_not_read() {
  let stage = stages([reward(0)], 0)
  let changed: Data = walk.WalkStateV1 { ..stage.walked, repeated: True }
  !scan_run(
    stage.certified,
    walk.Scan { args: stage.scan },
    changed,
    True,
    [fixture.fraud_prover_key_hash],
  )
}

test finish_hands_a_first_pointer_to_claim_custody() {
  let stage = stages([reward(0)], 0)
  scan_run(
    stage.walked,
    finish(),
    walk.claim_context_v1(stage.walked),
    False,
    [fixture.fraud_prover_key_hash],
  )
}

test finish_cannot_send_a_first_pointer_to_the_terminal() fail {
  let stage = stages([reward(0)], 0)
  terminal_run(
    stage.walked,
    walk.final_state_v1(stage.walked),
    [fixture.fraud_prover_key_hash],
  )
}

test finish_sends_a_repeated_pointer_to_the_terminal() {
  let stage = stages([reward(0), reward(0)], 1)
  let final_state = walk.final_state_v1(stage.walked)
  and {
    final_state.claim == claim.RedeemerRepeated,
    terminal_run(stage.walked, final_state, [fixture.fraud_prover_key_hash]),
  }
}

test finish_cannot_send_a_repeated_pointer_to_claim_custody() fail {
  let stage = stages([reward(0), reward(0)], 1)
  let context: Data = claim.ClaimContextV1 {
    bound: stage.state,
    observed: None,
    target: claim.RedeemerPointer {
      redeemer_index: 1,
      coordinate: walk.coordinate_of_pointer_v1(3, 0),
    },
  }
  scan_run(
    stage.walked,
    finish(),
    context,
    False,
    [fixture.fraud_prover_key_hash],
  )
}

test finish_sends_an_exhausted_field_to_the_terminal() {
  let stage = stages([reward(0)], 1)
  let final_state = walk.final_state_v1(stage.walked)
  and {
    final_state.claim == claim.RedeemerOutOfRange,
    terminal_run(stage.walked, final_state, [fixture.fraud_prover_key_hash]),
  }
}

test finish_refuses_an_unwalked_field() fail {
  let stage = stages([reward(0)], 1)
  terminal_run(
    stage.certified,
    claim.FinalStateV1 {
      context: claim.ClaimContextV1 {
        bound: stage.state,
        observed: None,
        target: claim.RedeemerPointer {
          redeemer_index: 1,
          coordinate: walk.coordinate_of_pointer_v1(3, 0),
        },
      },
      claim: claim.RedeemerOutOfRange,
    },
    [fixture.fraud_prover_key_hash],
  )
}

test finish_requires_the_prover_signature() fail {
  let stage = stages([reward(0)], 0)
  scan_run(
    stage.walked,
    finish(),
    walk.claim_context_v1(stage.walked),
    False,
    [],
  )
}
