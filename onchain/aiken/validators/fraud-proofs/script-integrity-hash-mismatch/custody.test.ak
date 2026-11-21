use cardano/transaction.{Transaction}
use fraud_proofs/script_integrity_hash_mismatch/family_entry as entry_validator
use fraud_proofs/script_integrity_hash_mismatch/terminal as terminal_validator
use midgard/computation_thread as ct
use midgard/fraud_proofs/all_purposes_scan_fixture_v1.{
  Arbitrary, HonestFor, IntegrityV1, ShapeV1, SubjectV1, TxV1, WitnessesV1, a,
  at, b, key_spend, policy, protected_script_output, source_map, source_of,
  subject_tx_of, tx_of, walk_tx,
}
use midgard/fraud_proofs/direct_script_derivation/all_purposes_scan as scan
use midgard/fraud_proofs/direct_script_derivation/source_scan.{BoundIndexV1}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/fraud_proofs/script_integrity_hash_mismatch/entry
use midgard/fraud_proofs/script_integrity_hash_mismatch/terminal

const elsewhere = #"abababababababababababababababababababababababababababab"

const stranger = #"cdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcd"

// ## Family entry: opens the walk at the spend field

fn entered(
  spends: List<native_types.MidgardCredential>,
  spend_scan: ByteArray,
  mint_grammar: ByteArray,
  forged: Bool,
  signer: ByteArray,
) -> Bool {
  let tx = tx_of(ShapeV1 { spends, policies: [], observers: [], outputs: [] })
  let Pair(index, _) = source_map([source_of(a, 3)])
  let bound = BoundIndexV1 { bound: tx.bound, source_index: index }
  let opened = scan.initial_v1(bound, None, tx.spend_opening, [], policy)
  let written: Data = if forged {
    scan.StateV1 {
      ..opened,
      facts: scan.FactsV1 { ..opened.facts, probe: Some(a) },
    }
  } else {
    opened
  }
  entry_validator.main.spend(
    spend_scan,
    mint_grammar,
    fixture.computation_thread_policy_id,
    policy,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(bound),
      },
    ),
    ct.Continue(
      entry.Args { input_index: 0, output_index: 0, opening: tx.spend_opening },
    ),
    fixture.own_out_ref(),
    Transaction {
      ..fixture.continue_mid_step_l1_tx_v1(
        tx.bound.subject.transaction_id,
        written,
      ),
      extra_signatories: [signer],
    },
  )
}

test entry_with_spends_goes_to_the_spend_walk() {
  entered(
    [native_types.ScriptCredential(a)],
    fixture.next_step_script_hash,
    elsewhere,
    False,
    fixture.fraud_prover_key_hash,
  )
}

test entry_without_spends_goes_to_the_mint_door() {
  entered(
    [],
    elsewhere,
    fixture.next_step_script_hash,
    False,
    fixture.fraud_prover_key_hash,
  )
}

test entry_refuses_the_wrong_route() {
  and {
    !entered(
      [key_spend],
      elsewhere,
      fixture.next_step_script_hash,
      False,
      fixture.fraud_prover_key_hash,
    ),
    !entered(
      [],
      fixture.next_step_script_hash,
      elsewhere,
      False,
      fixture.fraud_prover_key_hash,
    ),
  }
}

test entry_refuses_a_probe_the_family_does_not_ask() {
  !entered(
    [key_spend],
    fixture.next_step_script_hash,
    elsewhere,
    True,
    fixture.fraud_prover_key_hash,
  )
}

test entry_refuses_a_stranger() fail {
  entered(
    [key_spend],
    fixture.next_step_script_hash,
    elsewhere,
    False,
    stranger,
  )
}

// ## Terminal: finalizes the thread exactly when the verdict is contradicted

fn finalized(
  subject: SubjectV1,
  witness_set: fn(TxV1) -> ByteArray,
  signer: List<ByteArray>,
) -> Bool {
  let tx = subject_tx_of(
    ShapeV1 {
      spends: [key_spend],
      policies: [],
      observers: [a],
      outputs: [protected_script_output(b)],
    },
    subject,
  )
  let Pair(index, sources) = source_map([source_of(a, 3), source_of(b, 128)])
  let state = walk_tx(
    tx,
    index,
    None,
    WitnessesV1 {
      spend_executions: [None],
      mint: [],
      observers: [at(sources, 0)],
      receive: [Some(at(sources, 1))],
    },
  )
  terminal_validator.main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state),
      },
    ),
    ct.Continue(
      terminal.Args {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
        witness_set_compact_cbor: witness_set(tx),
      },
    ),
    fixture.own_out_ref(),
    Transaction {
      ..fixture.finalize_step_l1_tx_v1(tx.bound.subject.transaction_id),
      extra_signatories: signer,
    },
  )
}

fn accepted(integrity: IntegrityV1) -> SubjectV1 {
  SubjectV1 { source_kind: 0, rejection_reason: None, integrity }
}

fn committed(tx: TxV1) -> ByteArray {
  tx.witness_set_compact_cbor
}

test terminal_finalizes_a_wrongful_acceptance() {
  finalized(accepted(Arbitrary), committed, [])
}

test terminal_refuses_an_honest_acceptance() {
  !finalized(accepted(HonestFor(3)), committed, [])
}

test terminal_finalization_is_open_to_any_submitter() {
  finalized(accepted(Arbitrary), committed, [stranger])
}

test terminal_refuses_an_uncommitted_witness_set() fail {
  finalized(accepted(HonestFor(3)), fn(_) { #"8300" }, [])
}
