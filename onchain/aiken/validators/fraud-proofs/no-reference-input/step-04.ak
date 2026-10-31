use aiken/collection/list
use aiken/crypto.{ScriptHash, blake2b_256}
use aiken/merkle_patricia_forestry as mpf
use aiken/merkle_patricia_forestry/helpers.{nibble}
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{
  Input, OutputReference, Redeemer, ScriptPurpose, Transaction,
}
use env
use midgard/computation_thread as ct
use midgard/fraud_proofs/chunked_inclusion_v1.{PublishedProofCarriage}
use midgard/fraud_proofs/common.{
  NonMembershipCarriage, PublishedChunkNonMembership,
  RedeemerCarriedNonMembership,
}
use midgard/fraud_proofs/first_fault_v1 as order
use midgard/fraud_proofs/input_existence_fixture_v1 as carriage_fixture
use midgard/fraud_proofs/input_existence_v1/terminal
use midgard/fraud_proofs/input_existence_v1/terminal_step
use midgard/fraud_proofs/input_existence_v1/types.{
  Absent, LedgerEvidenceV1, LedgerStateV1, OutOfDomain, Present, TerminalArgsV1,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_input}
use midgard/fraud_proofs/native_tx/types.{MidgardTxInput} as native_types
use midgard/fraud_proofs/no_reference_input/step_04.{Datum, SpendRedeemer}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/rejection_reason_v1.{InputNotFound}

validator main(
  fraud_proof_token_policy_id: PolicyId,
  fraud_proof_token_address: Address,
  computation_thread_token_policy_id: PolicyId,
) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    terminal_step.terminal_step_v1(
      fraud_proof_token_policy_id,
      fraud_proof_token_address,
      computation_thread_token_policy_id,
      datum,
      redeemer,
      own_out_ref,
      tx,
    )
  }

  else(_) {
    fail
  }
}

fn challenged_reference_input() -> MidgardTxInput {
  fixture.absent_midgard_tx_input()
}

fn ledger_state_v1(
  subject: substrate.VerdictSubjectV1,
  selected_input: Option<MidgardTxInput>,
  pre_utxos_root: ByteArray,
) -> LedgerStateV1 {
  let block = fixture.challenged_block_v1()
  let compact = carriage_fixture.compact_view_v1(block.compact)
  LedgerStateV1 {
    subject,
    compact,
    network_order_context: order.network_order_context_v1(compact, block.header),
    source_kind: 1,
    input_index: 0,
    selected_input,
    claimed_input: selected_input,
    pre_utxos_root,
  }
}

fn run_terminal_absence_with(
  missing_input: MidgardTxInput,
  blocks_prev_utxos_root: ByteArray,
  carriage: NonMembershipCarriage,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  reference_inputs: List<Input>,
) -> Bool {
  let subject = substrate.VerdictSubjectV1 {
    version: 1,
    direction: 0,
    source_kind: 0,
    transaction_id: fixture.challenged_block_v1().tx_id,
    source_key: #"",
    rejection_reason: None,
  }
  let name = fixture.ct_token_asset_name(fixture.challenged_block_v1())
  let base = fixture.finalize_step_l1_tx_v1(name)
  let tx = Transaction {
    ..base,
    reference_inputs,
    redeemers: list.concat(redeemers, base.redeemers),
  }
  main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(
          ledger_state_v1(subject, Some(missing_input), blocks_prev_utxos_root),
        ),
      },
    ),
    ct.Continue(
      TerminalArgsV1 {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: list.length(redeemers),
        ledger_evidence: Absent { non_membership: carriage },
      },
    ),
    fixture.own_out_ref(),
    tx,
  )
}

fn run_terminal_absence_direct(
  missing_reference_input: MidgardTxInput,
  blocks_prev_utxos_root: ByteArray,
  claimed_root: ByteArray,
  claimed_key: ByteArray,
  proof: mpf.Proof,
) -> Bool {
  run_terminal_absence_with(
    missing_reference_input,
    blocks_prev_utxos_root,
    RedeemerCarriedNonMembership {
      non_membership_proof: proof,
      non_membership_proof_script_redeemer_index: 0,
    },
    [
      fixture.pexcludes_withdraw_redeemer_entry(
        claimed_root,
        claimed_key,
        proof,
      ),
    ],
    [],
  )
}

fn absence_claim(
  root: ByteArray,
  key: ByteArray,
  indices: List<Int>,
) -> chunked_inclusion_v1.ChunkedProofClaim {
  chunked_inclusion_v1.ChunkedProofClaim {
    mode: chunked_inclusion_v1.NonMembership,
    merkle_root: root,
    key_bytes: key,
    value_hash: chunked_inclusion_v1.absent_value_hash,
    ordered_chunk_reference_input_indices: indices,
  }
}

fn run_terminal_absence_published_with_claim(
  missing_reference_input: MidgardTxInput,
  root: ByteArray,
  carriage_indices: List<Int>,
  claim_indices: List<Int>,
  proof: mpf.Proof,
) -> Bool {
  run_terminal_absence_with(
    missing_reference_input,
    root,
    PublishedChunkNonMembership(
      PublishedProofCarriage {
        ordered_chunk_reference_input_indices: carriage_indices,
      },
    ),
    [
      fixture.chunked_verify_withdraw_redeemer_entry(
        absence_claim(
          root,
          encode_midgard_tx_input(missing_reference_input),
          claim_indices,
        ),
      ),
    ],
    fixture.proof_chunk_reference_inputs(proof),
  )
}

test no_reference_input_event_ledger_terminal_accepts_fitting_direct_non_membership() {
  let missing_reference_input = challenged_reference_input()
  let (root, proof) = fixture.absence_witness()
  and {
    nibble(blake2b_256(encode_midgard_tx_input(missing_reference_input)), 0) != nibble(
      fixture.absence_witness_leaf_path(),
      0,
    ),
    run_terminal_absence_direct(
      missing_reference_input,
      root,
      root,
      encode_midgard_tx_input(missing_reference_input),
      proof,
    ),
  }
}

test no_reference_input_event_ledger_terminal_accepts_genesis_empty_direct_non_membership() {
  let missing_reference_input = challenged_reference_input()
  run_terminal_absence_direct(
    missing_reference_input,
    env.empty_merkle_tree_root,
    env.empty_merkle_tree_root,
    encode_midgard_tx_input(missing_reference_input),
    [],
  )
}

test no_reference_input_event_ledger_terminal_accepts_maximum_published_non_membership() {
  let missing_reference_input = challenged_reference_input()
  let proof = fixture.adversarial_non_membership_steps(63)
  let root = fixture.adversarial_non_membership_root(
    encode_midgard_tx_input(missing_reference_input),
    proof,
  )
  run_terminal_absence_published_with_claim(
    missing_reference_input,
    root,
    [0, 1, 2, 3],
    [0, 1, 2, 3],
    proof,
  )
}

test no_reference_input_event_ledger_terminal_rejects_reordered_published_claim() fail {
  let missing_reference_input = challenged_reference_input()
  let proof = fixture.adversarial_non_membership_steps(63)
  let root = fixture.adversarial_non_membership_root(
    encode_midgard_tx_input(missing_reference_input),
    proof,
  )
  run_terminal_absence_published_with_claim(
    missing_reference_input,
    root,
    [0, 1, 2, 3],
    [0, 2, 1, 3],
    proof,
  )
}

test no_reference_input_event_ledger_terminal_rejects_direct_claim_for_another_key() fail {
  let missing_reference_input = challenged_reference_input()
  run_terminal_absence_direct(
    missing_reference_input,
    env.empty_merkle_tree_root,
    env.empty_merkle_tree_root,
    #"00",
    [],
  )
}

// Same ledger fact, all verdict polarities, through the actual finalizer.

fn terminal_subject_v1(
  direction: Int,
  source_kind: Int,
) -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: 1,
    direction,
    source_kind,
    transaction_id: fixture.challenged_block_v1().tx_id,
    source_key: if source_kind == 0 {
      #""
    } else {
      #"01"
    },
    rejection_reason: if direction == 1 {
      Some(InputNotFound { source_kind: 1, input_index: 0 })
    } else {
      None
    },
  }
}

fn run_presence_v1(
  direction: Int,
  source_kind: Int,
  expected_value_hash: ByteArray,
) -> Bool {
  let input = fixture.absent_midgard_tx_input()
  let key = encode_midgard_tx_input(input)
  let value = #"d87980"
  let root = mpf.root(mpf.insert(mpf.empty, key, value, []))
  let subject = terminal_subject_v1(direction, source_kind)
  let name = fixture.ct_token_asset_name(fixture.challenged_block_v1())
  main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(ledger_state_v1(subject, Some(input), root)),
      },
    ),
    ct.Continue(
      TerminalArgsV1 {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
        ledger_evidence: Present { value_hash: expected_value_hash, proof: [] },
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(name),
  )
}

test event_ledger_terminal_refuses_an_honest_normal_acceptance_with_present_input() {
  !run_presence_v1(0, 0, blake2b_256(#"d87980"))
}

test event_ledger_terminal_refuses_an_honest_forced_acceptance_with_present_input() {
  !run_presence_v1(0, 1, blake2b_256(#"d87980"))
}

test event_ledger_terminal_convicts_a_forced_rejection_of_a_present_input() {
  run_presence_v1(1, 1, blake2b_256(#"d87980"))
}

test event_ledger_terminal_rejects_substituting_a_present_value_hash() fail {
  run_presence_v1(1, 1, blake2b_256(#"00"))
}

test event_ledger_terminal_rejects_presence_evidence_for_an_out_of_domain_input() fail {
  let state = ledger_state_v1(
    terminal_subject_v1(1, 1),
    None,
    env.empty_merkle_tree_root,
  )
  terminal.verdict_contradiction_v1(
    state,
    Present { value_hash: blake2b_256(#"d87980"), proof: [] },
    [],
    [],
  )
}

test event_ledger_terminal_convicts_a_forced_rejection_with_no_named_input() {
  let state = ledger_state_v1(
    terminal_subject_v1(1, 1),
    None,
    env.empty_merkle_tree_root,
  )
  terminal.verdict_contradiction_v1(state, OutOfDomain, [], [])
}

test event_ledger_terminal_refuses_honest_rejection_of_an_absent_input() {
  let input = fixture.absent_midgard_tx_input()
  let state = ledger_state_v1(
    terminal_subject_v1(1, 1),
    Some(input),
    env.empty_merkle_tree_root,
  )
  let evidence = Absent {
    non_membership: RedeemerCarriedNonMembership {
      non_membership_proof: [],
      non_membership_proof_script_redeemer_index: 0,
    },
  }
  !terminal.verdict_contradiction_v1(
    state,
    evidence,
    [],
    [
      fixture.pexcludes_withdraw_redeemer_entry(
        env.empty_merkle_tree_root,
        encode_midgard_tx_input(input),
        [],
      ),
    ],
  )
}

test event_ledger_terminal_convicts_forced_acceptance_of_an_absent_input() {
  let input = fixture.absent_midgard_tx_input()
  let state = ledger_state_v1(
    terminal_subject_v1(0, 1),
    Some(input),
    env.empty_merkle_tree_root,
  )
  let evidence = Absent {
    non_membership: RedeemerCarriedNonMembership {
      non_membership_proof: [],
      non_membership_proof_script_redeemer_index: 0,
    },
  }
  terminal.verdict_contradiction_v1(
    state,
    evidence,
    [],
    [
      fixture.pexcludes_withdraw_redeemer_entry(
        env.empty_merkle_tree_root,
        encode_midgard_tx_input(input),
        [],
      ),
    ],
  )
}

fn presence_against_rejection_reason_v1(kind: Int, index: Int) -> Bool {
  let input = fixture.absent_midgard_tx_input()
  let key = encode_midgard_tx_input(input)
  let root = mpf.root(mpf.insert(mpf.empty, key, #"d87980", []))
  let reason = InputNotFound { source_kind: kind, input_index: index }
  let subject = substrate.VerdictSubjectV1 {
    ..terminal_subject_v1(1, 1),
    rejection_reason: Some(reason),
  }
  terminal.verdict_contradiction_v1(
    ledger_state_v1(subject, Some(input), root),
    Present { value_hash: blake2b_256(#"d87980"), proof: [] },
    [],
    [],
  )
}

// The selected input is field 1, index 0. A rejection that names another
// input refuses presence evidence for the selected one.
test event_ledger_terminal_refuses_presence_against_a_rejection_naming_another_index() {
  !presence_against_rejection_reason_v1(1, 1)
}

test event_ledger_terminal_refuses_presence_against_a_rejection_naming_another_field() {
  !presence_against_rejection_reason_v1(0, 0)
}
