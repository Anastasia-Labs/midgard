use cardano/transaction.{Transaction}
use fraud_proofs/unused_script_witness/family_entry as entry_validator
use fraud_proofs/unused_script_witness/terminal as terminal_validator
use fraud_proofs/unused_script_witness/witness_entry
use fraud_proofs/unused_script_witness/witness_grammar
use fraud_proofs/unused_script_witness/witness_scan
use midgard/computation_thread as ct
use midgard/fraud_proofs/all_purposes_scan_fixture_v1.{
  Arbitrary, ShapeV1, SubjectV1, WitnessesV1, at, key_spend, policy, source_map,
  walk_tx,
}
use midgard/fraud_proofs/direct_script_derivation/all_purposes_scan as scan
use midgard/fraud_proofs/direct_script_derivation/purpose_workers.{
  ProgressArgsV1,
}
use midgard/fraud_proofs/direct_script_derivation/source_scan.{BoundIndexV1}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/fraud_proofs/unused_script_witness/entry
use midgard/fraud_proofs/unused_script_witness/fixture.{
  StagesV1, case_tx, hash_of, native_script, stages, witness_source,
} as usw
use midgard/fraud_proofs/unused_script_witness/terminal
use midgard/fraud_proofs/unused_script_witness/walk.{WalkStateV1}

const elsewhere = #"abababababababababababababababababababababababababababab"

const stranger = #"cdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcd"

// ## Fixtures
//
// Two native witnesses; the only purpose is an observer of the second, so the
// first is unused. The source map holds both.

fn n0() {
  native_script(0x31)
}

fn n1() {
  native_script(0x32)
}

fn subject(source_kind: Int) -> SubjectV1 {
  SubjectV1 { source_kind, rejection_reason: None, integrity: Arbitrary }
}

fn shape(spends: List<native_types.MidgardCredential>) -> ShapeV1 {
  ShapeV1 { spends, policies: [], observers: [hash_of(n1())], outputs: [] }
}

fn sources() {
  source_map([witness_source(0, n0()), witness_source(1, n1())])
}

/// The completed all-purposes walk, probing the witness at `script_index`
/// (the first one when the index is out of range).
fn scanned_for(script_index: Int) -> scan.StateV1 {
  let tx = case_tx(shape([key_spend]), subject(0), [n0(), n1()])
  let Pair(index, executions) = sources()
  walk_tx(
    tx.1st,
    index,
    Some(
      hash_of(
        if script_index == 1 {
          n1()
        } else {
          n0()
        },
      ),
    ),
    WitnessesV1 {
      spend_executions: [None],
      mint: [],
      observers: [at(executions, 1)],
      receive: [],
    },
  )
}

fn scanned() -> scan.StateV1 {
  scanned_for(0)
}

fn staged(script_index: Int, budget: Int) -> StagesV1 {
  let tx = case_tx(shape([key_spend]), subject(0), [n0(), n1()])
  stages(scanned_for(script_index), tx.2nd, script_index, budget)
}

fn step_datum(state: a) -> Option<ct.StepDatum<a>> {
  Some(
    ct.StepDatum {
      fraud_prover: fixture.fraud_prover_key_hash,
      data: Some(state),
    },
  )
}

fn step_tx(output: Data, same: Bool, signer: ByteArray) -> Transaction {
  let tx = if same {
    fixture.continue_same_step_l1_tx_v1(#"01", output)
  } else {
    fixture.continue_mid_step_l1_tx_v1(#"01", output)
  }
  Transaction { ..tx, extra_signatories: [signer] }
}

const prover = fixture.fraud_prover_key_hash

// ## family-entry: opens the all-purposes walk with the probe

fn entered(
  spends: List<native_types.MidgardCredential>,
  spend_scan: ByteArray,
  mint_grammar: ByteArray,
  written_probe: ByteArray,
  signer: ByteArray,
) -> Bool {
  let tx = case_tx(shape(spends), subject(0), [n0(), n1()])
  let Pair(index, _) = sources()
  let bound = BoundIndexV1 { bound: tx.1st.bound, source_index: index }
  let opened = scan.initial_v1(
    bound,
    Some(hash_of(n0())),
    tx.1st.spend_opening,
    [],
    policy,
  )
  let written: Data = scan.StateV1 {
    ..opened,
    facts: scan.FactsV1 { ..opened.facts, probe: Some(written_probe) },
  }
  entry_validator.main.spend(
    spend_scan,
    mint_grammar,
    fixture.computation_thread_policy_id,
    policy,
    step_datum(bound),
    ct.Continue(
      entry.Args {
        input_index: 0,
        output_index: 0,
        probe: hash_of(n0()),
        opening: tx.1st.spend_opening,
      },
    ),
    fixture.own_out_ref(),
    step_tx(written, False, signer),
  )
}

test entry_with_spends_goes_to_the_spend_walk() {
  entered(
    [key_spend],
    fixture.next_step_script_hash,
    elsewhere,
    hash_of(n0()),
    prover,
  )
}

test entry_without_spends_goes_to_the_mint_door() {
  entered([], elsewhere, fixture.next_step_script_hash, hash_of(n0()), prover)
}

test entry_refuses_the_wrong_route() {
  and {
    !entered(
      [key_spend],
      elsewhere,
      fixture.next_step_script_hash,
      hash_of(n0()),
      prover,
    ),
    !entered(
      [],
      fixture.next_step_script_hash,
      elsewhere,
      hash_of(n0()),
      prover,
    ),
  }
}

test entry_refuses_a_probe_it_was_not_given() {
  !entered(
    [key_spend],
    fixture.next_step_script_hash,
    elsewhere,
    hash_of(n1()),
    prover,
  )
}

test entry_refuses_a_stranger() fail {
  entered(
    [key_spend],
    fixture.next_step_script_hash,
    elsewhere,
    hash_of(n0()),
    stranger,
  )
}

// ## witness-entry: takes the completed walk and opens field 6

fn witness_entered(
  input: scan.StateV1,
  output: Data,
  same: Bool,
  signer: ByteArray,
) -> Bool {
  let stage = staged(0, 16)
  witness_entry.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    policy,
    step_datum(input),
    ct.Continue(stage.entry),
    fixture.own_out_ref(),
    step_tx(output, same, signer),
  )
}

test witness_entry_hands_the_opened_walk_to_grammar() {
  witness_entered(scanned(), staged(0, 16).opened, False, prover)
}

test witness_entry_cannot_name_another_witness() {
  let changed: Data = WalkStateV1 { ..staged(0, 16).opened, script_index: 1 }
  !witness_entered(scanned(), changed, False, prover)
}

test witness_entry_cannot_write_a_hash_it_did_not_read() {
  let changed: Data = WalkStateV1 {
    ..staged(0, 16).opened,
    witness_hash: Some(hash_of(n1())),
  }
  !witness_entered(scanned(), changed, False, prover)
}

test witness_entry_cannot_stay_at_the_entry_script() {
  !witness_entered(scanned(), staged(0, 16).opened, True, prover)
}

test witness_entry_requires_a_completed_purpose_walk() fail {
  let state = scanned()
  witness_entered(
    scan.StateV1 { ..state, complete: False },
    staged(0, 16).opened,
    False,
    prover,
  )
}

test witness_entry_refuses_a_stranger() fail {
  witness_entered(scanned(), staged(0, 16).opened, False, stranger)
}

// ## witness-grammar

fn certified(
  args: ProgressArgsV1,
  output: Data,
  same: Bool,
  signer: ByteArray,
) -> Bool {
  witness_grammar.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    policy,
    step_datum(staged(0, 16).opened),
    ct.Continue(args),
    fixture.own_out_ref(),
    step_tx(output, same, signer),
  )
}

test grammar_hands_a_certified_field_to_the_scan() {
  let stage = staged(0, 16)
  certified(stage.grammar, stage.certified, False, prover)
}

test grammar_cannot_keep_a_certified_field() {
  let stage = staged(0, 16)
  !certified(stage.grammar, stage.certified, True, prover)
}

test grammar_cannot_skip_to_a_walked_state() {
  let stage = staged(0, 16)
  !certified(stage.grammar, stage.walked, False, prover)
}

test grammar_refuses_a_batch_past_the_bound() fail {
  let stage = staged(0, 16)
  certified(
    ProgressArgsV1 { ..stage.grammar, budget: 17 },
    stage.certified,
    False,
    prover,
  )
}

test grammar_refuses_a_stranger() fail {
  let stage = staged(0, 16)
  certified(stage.grammar, stage.certified, False, stranger)
}

// ## witness-scan

fn scanned_step(
  stage: StagesV1,
  output: Data,
  same: Bool,
  signer: ByteArray,
) -> Bool {
  witness_scan.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    policy,
    step_datum(stage.certified),
    ct.Continue(stage.scan),
    fixture.own_out_ref(),
    step_tx(output, same, signer),
  )
}

test scan_that_reads_the_witness_goes_to_the_terminal() {
  let stage = staged(1, 16)
  scanned_step(stage, stage.walked, False, prover)
}

test scan_past_the_field_goes_to_the_terminal() {
  let stage = staged(5, 16)
  and {
    stage.walked.witness_hash == None,
    scanned_step(stage, stage.walked, False, prover),
  }
}

test scan_short_of_the_witness_stays_at_the_scan() {
  let stage = staged(1, 1)
  and {
    stage.walked.witness_hash == None,
    scanned_step(stage, stage.walked, True, prover),
    !scanned_step(stage, stage.walked, False, prover),
  }
}

test scan_that_reads_the_witness_cannot_stay() {
  let stage = staged(1, 16)
  !scanned_step(stage, stage.walked, True, prover)
}

test scan_cannot_write_a_hash_it_did_not_read() {
  let stage = staged(1, 16)
  let changed: Data = WalkStateV1 {
    ..stage.walked,
    witness_hash: Some(hash_of(n0())),
  }
  !scanned_step(stage, changed, False, prover)
}

test scan_cannot_claim_an_exhausted_field() {
  let stage = staged(0, 16)
  let changed: Data = WalkStateV1 {
    ..stage.walked,
    witness_hash: None,
    complete: True,
  }
  !scanned_step(stage, changed, False, prover)
}

test scan_refuses_a_batch_past_the_bound() fail {
  let stage = staged(1, 16)
  scanned_step(
    StagesV1 { ..stage, scan: ProgressArgsV1 { ..stage.scan, budget: 17 } },
    stage.walked,
    False,
    prover,
  )
}

test scan_refuses_a_stranger() fail {
  let stage = staged(1, 16)
  scanned_step(stage, stage.walked, False, stranger)
}

// ## terminal: finalizes exactly when the verdict is contradicted

fn finalized(script_index: Int, signer: List<ByteArray>) -> Bool {
  let state = staged(script_index, 16).walked
  let Pair(_, executions) = sources()
  let execution = at(executions, script_index)
  terminal_validator.main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
    step_datum(state),
    ct.Continue(
      terminal.Args {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
        selection: Some(
          terminal.SelectionV1 {
            source: execution.source,
            proof: execution.proof,
          },
        ),
      },
    ),
    fixture.own_out_ref(),
    Transaction {
      ..fixture.finalize_step_l1_tx_v1(#"01"),
      extra_signatories: signer,
    },
  )
}

test terminal_finalizes_a_wrongful_acceptance() {
  finalized(0, [prover])
}

test terminal_refuses_an_honest_acceptance() {
  !finalized(1, [prover])
}

test terminal_finalization_is_open_to_any_submitter() {
  finalized(0, []) && finalized(0, [stranger])
}
