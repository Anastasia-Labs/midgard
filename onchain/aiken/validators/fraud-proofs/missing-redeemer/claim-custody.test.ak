use aiken/builtin
use aiken/collection/list
use aiken/merkle_patricia_forestry as mpf
use aiken/primitive/bytearray
use cardano/transaction.{OutputReference, Transaction}
use env
use fraud_proofs/missing_redeemer/claim_mint_grammar as mint_grammar_entry
use fraud_proofs/missing_redeemer/claim_mint_scan as mint_entry
use fraud_proofs/missing_redeemer/claim_receive_declaration as declaration_entry
use fraud_proofs/missing_redeemer/claim_receive_finish as finish_entry
use fraud_proofs/missing_redeemer/claim_receive_grammar as receive_grammar_entry
use fraud_proofs/missing_redeemer/claim_receive_outputs as outputs_entry
use fraud_proofs/missing_redeemer/claim_spend_scan as spend_entry
use fraud_proofs/missing_redeemer/claim_spend_start as spend_start_entry
use fraud_proofs/missing_redeemer/claim_start as start_entry
use midgard/cek_data_v1.{DataSummaryV1}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{RedeemerCarriedMembership}
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/first_fault_seam.{
  NetworkOrderContextV1, PurposeCoordinateV1,
}
use midgard/fraud_proofs/direct_script_derivation/purpose_workers as workers
use midgard/fraud_proofs/direct_script_derivation/receive
use midgard/fraud_proofs/direct_script_derivation/receive_declaration as declaration
use midgard/fraud_proofs/direct_script_derivation/selection_types.{SelectionV1}
use midgard/fraud_proofs/direct_script_derivation/source.{SourceV1}
use midgard/fraud_proofs/direct_script_derivation/source_index
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1, PurposeV1}
use midgard/fraud_proofs/field_opening_v1
use midgard/fraud_proofs/missing_redeemer/claim.{ClaimContextV1}
use midgard/fraud_proofs/missing_redeemer/claim_mint as mint
use midgard/fraud_proofs/missing_redeemer/claim_purpose.{
  ClaimPurposeStateV1, ExistingPurpose, ImpossiblePurpose, NotRequired,
}
use midgard/fraud_proofs/missing_redeemer/claim_receive as receive_claim
use midgard/fraud_proofs/missing_redeemer/claim_spend as spend
use midgard/fraud_proofs/missing_redeemer/claim_start as start
use midgard/fraud_proofs/missing_redeemer/rule
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_input}
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/fraud_proofs/proof_thread_substrate_v1.{VerdictSubjectV1}
use midgard/fraud_proofs/transition_trace/proof.{ledger_outref_key}
use midgard/ledger_output_commitment_v1 as descriptor
use midgard/native_tx_machine_walk_v1 as walk
use midgard/rejection_reason_v1 as reasons

const hash = #"33333333333333333333333333333333333333333333333333333333"

const root = #"4444444444444444444444444444444444444444444444444444444444444444"

fn decision(
  block: fixture.NativeBlockFixtureV1,
  reason: Option<reasons.RejectionReasonV1>,
  pre_root: ByteArray,
) -> rule.DecisionStateV1 {
  let coordinate = PurposeCoordinateV1 { kind: 2, index: 9 }
  rule.DecisionStateV1 {
    authenticated: workers.SourceSelectionV1 {
      selection: SelectionV1 {
        bound: BoundV1 {
          subject: VerdictSubjectV1 {
            version: 1,
            direction: 0,
            source_kind: 0,
            transaction_id: block.tx_id,
            source_key: #"",
            rejection_reason: reason,
          },
          compact_cbor: block.compact_cbor,
          network_order_context: NetworkOrderContextV1 {
            body_network_mismatch: False,
          },
          event_pre_root: pre_root,
        },
        source_index: source_index.StateV1 { root, count: 7 },
        coordinate,
        purpose: Some(
          PurposeV1 { coordinate, script_hash: hash, subject: hash },
        ),
      },
      source: Some(
        SourceV1 {
          field_index: 6,
          item_index: 0,
          source_key: #"00",
          language_tag: 3,
          script_hash: hash,
          total_length: 2,
          item_commitment: root,
        },
      ),
    },
    redeemer_missing: True,
  }
}

fn observed(
  block: fixture.NativeBlockFixtureV1,
  reason: Option<reasons.RejectionReasonV1>,
  pre_root: ByteArray,
) -> ClaimContextV1 {
  claim.observed_context_v1(decision(block, reason, pre_root))
}

fn reason(kind: Int, index: Int) -> Option<reasons.RejectionReasonV1> {
  Some(reasons.RedeemerMissing { purpose_kind: kind, purpose_index: index })
}

fn input(index: Int) {
  native_types.MidgardTxInput { tx_id: root, output_index: index }
}

fn spend_fixture(key: Bool, claimed_index: Int) {
  // Raw index two has canonical spend rank one; references straddle it.
  let inputs = [input(9), input(1), input(5)]
  let tx = fixture.valid_native_tx_v1()
  let block = fixture.native_block_fixture_v1(
    native_types.MidgardTransaction {
      ..tx,
      body: native_types.MidgardTransactionBody {
        ..tx.body,
        inputs,
        reference_inputs: [input(0), input(3)],
      },
    },
  )
  let summary = DataSummaryV1 { root, cbor_length: 1, memory: 1 }
  let value = descriptor.encode(
    descriptor.LedgerOutputCommitmentV1 {
      version: 1,
      output_index: 5,
      total_length: 50,
      item_commitment: root,
      address: bytearray.concat(
        if key {
          #"60"
        } else {
          #"70"
        },
        hash,
      ),
      lovelace: 5000000,
      asset_count: 0,
      asset_frontier_commitment: root,
      cardano_value_size: 5,
      reference_script_language: -1,
      reference_script_hash: #"",
      reference_script_total_length: 0,
      reference_script_item_commitment: #"",
      cardano_tx_out: summary,
      midgard_tx_out: summary,
      cardano_spend_datum: summary,
    },
  )
  let key = ledger_outref_key(
    OutputReference { transaction_id: root, output_index: 5 },
  )
  let pre_root = mpf.root(mpf.insert(mpf.empty, key, value, []))
  let initial = observed(block, reason(0, claimed_index), pre_root)
  let opening = fixture.inline_body_opening(
    block.compact_cbor,
    list.map(inputs, encode_midgard_tx_input),
  )
  let state = spend.open_v1(
    initial,
    2,
    opening,
    value,
    RedeemerCarriedMembership {
      membership_proof: [],
      membership_proof_script_redeemer_index: 0,
    },
    1,
    [],
    [fixture.phas_withdraw_redeemer_entry(pre_root, key, value)],
    fixture.test_certificate_policy_id,
  )
  let Pair(view, position) = field_opening_v1.opened_field_walk(
    opening,
    cursor.anchor_v1(initial.bound.bound, 0),
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
  (state, opening, walk.encode_field_walk_checkpoint(after_first), value, key)
}

fn complete_spend(key: Bool, claimed_index: Int) {
  let (state, opening, checkpoint, _, _) = spend_fixture(key, claimed_index)
  spend.advance_v1(
    state,
    workers.ProgressArgsV1 {
      input_index: 0,
      output_index: 0,
      opening,
      checkpoint_bytes: checkpoint,
      budget: 2,
    },
    [],
    fixture.test_certificate_policy_id,
  )
}

fn flipped(context: ClaimContextV1) -> ClaimContextV1 {
  expect Some(seen) = context.observed
  ClaimContextV1 {
    ..context,
    observed: Some(rule.DecisionStateV1 { ..seen, redeemer_missing: False }),
  }
}

fn empty_domain(kind: Int) {
  let original = fixture.valid_native_tx_v1()
  let tx = native_types.MidgardTransaction {
    ..original,
    body: native_types.MidgardTransactionBody { ..original.body, outputs: [] },
  }
  let block = fixture.native_block_fixture_v1(tx)
  let state = observed(block, reason(kind, 0), root)
  Pair(state, fixture.inline_body_opening(block.compact_cbor, []))
}

fn step_tx(state: Data, signatures: List<ByteArray>, same: Bool) {
  let tx = if same {
    fixture.continue_same_step_l1_tx_v1(root, state)
  } else {
    fixture.continue_mid_step_l1_tx_v1(root, state)
  }
  Transaction { ..tx, extra_signatories: signatures }
}

fn start_complete(changed: Int) {
  let Pair(initial, _) = empty_domain(8)
  let expected = start.complete_v1(initial)
  let output: Data = if changed == 1 {
    ClaimPurposeStateV1 { ..expected, purpose: NotRequired }
  } else if changed == 2 {
    ClaimPurposeStateV1 { ..expected, context: flipped(initial) }
  } else {
    expected
  }
  start_entry.main.spend(
    fixture.next_step_script_hash,
    hash,
    hash,
    hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(initial),
      },
    ),
    ct.Continue(start.Complete { input_index: 0, output_index: 0 }),
    fixture.own_out_ref(),
    step_tx(output, [], changed == 3),
  )
}

test claim_start_unsigned_deterministic_complete_is_valid() {
  start_complete(0)
}

test claim_start_cannot_drop_derived_receipt() {
  !start_complete(1)
}

test claim_start_cannot_alter_observed_state() {
  !start_complete(2)
}

test claim_start_cannot_choose_another_successor() {
  !start_complete(3)
}

fn start_spend(changed: Bool) {
  let (state, _, _, _, _) = spend_fixture(True, 1)
  let output: Data = if changed {
    flipped(state.context)
  } else {
    state.context
  }
  start_entry.main.spend(
    hash,
    fixture.next_step_script_hash,
    hash,
    hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state.context),
      },
    ),
    ct.Continue(start.Spend { input_index: 0, output_index: 0 }),
    fixture.own_out_ref(),
    step_tx(output, [], False),
  )
}

test claim_start_hands_the_spend_context_over_without_a_signature() {
  start_spend(False)
}

test claim_start_spend_handover_cannot_alter_the_context() {
  !start_spend(True)
}

// A transaction minting under one policy at mint 0, with no redeemer and no
// required observer. Its forced rejection RedeemerMissing { 1, 0 } is the true
// first fault; the step output is the receipt the observer route would derive
// if it ignored the leaf's purpose kind.
fn start_observe_on_minting_tx(leaf_kind: Int) {
  let tx = fixture.valid_native_tx_v1()
  let block = fixture.native_block_fixture_v1(
    native_types.MidgardTransaction {
      ..tx,
      body: native_types.MidgardTransactionBody {
        ..tx.body,
        required_observers: [],
        mint: builtin.map_data(
          [
            Pair(
              builtin.b_data(hash),
              builtin.map_data([Pair(builtin.b_data(#"41"), builtin.i_data(1))]),
            ),
          ],
        ),
      },
      witness_set: native_types.MidgardTransactionWitnessSet {
        ..tx.witness_set,
        redeemer_tx_wits: [],
      },
    },
  )
  let initial = observed(block, reason(leaf_kind, 0), root)
  let output: Data = ClaimPurposeStateV1 {
    context: initial,
    purpose: ImpossiblePurpose {
      coordinate: PurposeCoordinateV1 { kind: leaf_kind, index: 0 },
    },
  }
  start_entry.main.spend(
    fixture.next_step_script_hash,
    hash,
    hash,
    hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(initial),
      },
    ),
    ct.Continue(
      start.Observe {
        input_index: 0,
        output_index: 0,
        opening: fixture.inline_body_opening(block.compact_cbor, []),
      },
    ),
    fixture.own_out_ref(),
    step_tx(output, [], False),
  )
}

test claim_start_observer_route_proves_an_observer_leaf_impossible_on_a_minting_tx() {
  start_observe_on_minting_tx(2)
}

// Either result passes the body, so only an abort satisfies this row.
test claim_start_observer_route_refuses_a_mint_leaf() fail {
  let accepted = start_observe_on_minting_tx(1)
  accepted || !accepted
}

fn spend_open(signatures: List<ByteArray>) {
  let (state, opening, _, value, key) = spend_fixture(True, 1)
  let output: Data = state
  let tx = step_tx(output, signatures, False)
  spend_start_entry.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state.context),
      },
    ),
    ct.Continue(
      spend.OpenActionV1 {
        input_index: 0,
        output_index: 0,
        raw_item_index: 2,
        opening,
        descriptor_cbor: value,
        membership: RedeemerCarriedMembership {
          membership_proof: [],
          membership_proof_script_redeemer_index: 0,
        },
        budget: 1,
      },
    ),
    fixture.own_out_ref(),
    Transaction {
      ..tx,
      redeemers: [
        fixture.phas_withdraw_redeemer_entry(
          state.cursor.bound.event_pre_root,
          key,
          value,
        ),
      ],
    },
  )
}

test spend_free_raw_choice_requires_the_prover_signature_positive() {
  spend_open([fixture.fraud_prover_key_hash])
}

test spend_free_raw_choice_missing_signature_refuses() fail {
  spend_open([])
}

test spend_free_raw_choice_wrong_signature_refuses() fail {
  spend_open([hash])
}

fn spend_finish(changed: Bool) {
  let state = complete_spend(True, 1)
  let expected = spend.finish_v1(state)
  let output: Data = if changed {
    ClaimPurposeStateV1 {
      ..expected,
      purpose: ExistingPurpose {
        purpose: PurposeV1 {
          coordinate: PurposeCoordinateV1 { kind: 0, index: 1 },
          script_hash: hash,
          subject: state.target.subject,
        },
      },
    }
  } else {
    expected
  }
  spend_entry.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state),
      },
    ),
    ct.Continue(workers.Finish { input_index: 0, output_index: 0 }),
    fixture.own_out_ref(),
    step_tx(output, [], False),
  )
}

test spend_completed_rank_finish_is_permissionless() {
  spend_finish(False)
}

test spend_finish_cannot_forge_existing_for_key() {
  !spend_finish(True)
}

fn mint_ready() {
  let Pair(observed, opening) = empty_domain(1)
  let state = mint.initial_v1(
    observed,
    opening,
    [],
    fixture.test_certificate_policy_id,
  )
  let Pair(_, checkpoint) = cursor.initial_v1(
    observed.bound.bound,
    5,
    opening,
    [],
    fixture.test_certificate_policy_id,
  )
  let args = workers.ProgressArgsV1 {
    input_index: 0,
    output_index: 0,
    opening,
    checkpoint_bytes: checkpoint,
    budget: 1,
  }
  let next = mint.certify_v1(
    state,
    args,
    [],
    fixture.test_certificate_policy_id,
  )
  let Pair(_, final_checkpoint) = cursor.certify_v1(
    state.work.scan.cursor,
    opening,
    checkpoint,
    1,
    [],
    fixture.test_certificate_policy_id,
  )
  (state, args, next, final_checkpoint)
}

fn mint_grammar(changed: Bool) {
  let (state, args, next, _) = mint_ready()
  let output: Data = if changed {
    mint.StateV1 { ..next, context: flipped(next.context) }
  } else {
    next
  }
  mint_grammar_entry.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state),
      },
    ),
    ct.Continue(args),
    fixture.own_out_ref(),
    step_tx(output, [], False),
  )
}

test mint_grammar_carries_exact_observed_state_without_owner_signature() {
  mint_grammar(False)
}

test mint_grammar_rejects_observed_substitution() {
  !mint_grammar(True)
}

fn mint_finish(changed: Bool) {
  let (_, args, state, checkpoint) = mint_ready()
  let args = start.FinishOpeningArgsV1 {
    input_index: 0,
    output_index: 0,
    opening: args.opening,
    checkpoint_bytes: checkpoint,
  }
  let expected = mint.finish_v1(
    state,
    args,
    [],
    fixture.test_certificate_policy_id,
  )
  let output: Data = if changed {
    ClaimPurposeStateV1 { ..expected, purpose: NotRequired }
  } else {
    expected
  }
  mint_entry.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state),
      },
    ),
    ct.Continue(mint.Finish { args }),
    fixture.own_out_ref(),
    step_tx(output, [], False),
  )
}

test mint_empty_exhaustion_finish_is_permissionless() {
  mint_finish(False)
}

test mint_finish_cannot_drop_impossible_receipt() {
  !mint_finish(True)
}

fn receive_ready() {
  let Pair(observed, opening) = empty_domain(3)
  let state = receive_claim.initial_v1(
    observed,
    opening,
    [],
    fixture.test_certificate_policy_id,
  )
  let Pair(_, checkpoint) = cursor.initial_v1(
    observed.bound.bound,
    2,
    opening,
    [],
    fixture.test_certificate_policy_id,
  )
  let args = workers.ProgressArgsV1 {
    input_index: 0,
    output_index: 0,
    opening,
    checkpoint_bytes: checkpoint,
    budget: 1,
  }
  let next = receive_claim.certify_v1(
    state,
    args,
    [],
    fixture.test_certificate_policy_id,
  )
  let Pair(_, final_checkpoint) = cursor.certify_v1(
    state.scan.cursor,
    opening,
    checkpoint,
    1,
    [],
    fixture.test_certificate_policy_id,
  )
  (state, args, next, final_checkpoint)
}

fn receive_grammar(changed: Bool) {
  let (state, args, next, _) = receive_ready()
  let output: Data = if changed {
    receive_claim.OutputStateV1 { ..next, context: flipped(next.context) }
  } else {
    next
  }
  receive_grammar_entry.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state),
      },
    ),
    ct.Continue(args),
    fixture.own_out_ref(),
    step_tx(output, [], False),
  )
}

test receive_grammar_is_permissionless_and_preserves_the_observation() {
  receive_grammar(False)
}

test receive_grammar_cannot_change_observation() {
  !receive_grammar(True)
}

fn receive_outputs_handover(changed: Bool) {
  let (_, _, state, _) = receive_ready()
  let output: Data = if changed {
    receive_claim.OutputStateV1 { ..state, target_ordinal: 1 }
  } else {
    state
  }
  outputs_entry.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state),
      },
    ),
    ct.Continue(receive_claim.Finish { input_index: 0, output_index: 0 }),
    fixture.own_out_ref(),
    step_tx(output, [], False),
  )
}

test receive_outputs_hands_the_scan_to_its_finish_role() {
  receive_outputs_handover(False)
}

test receive_outputs_handover_cannot_change_the_leaf_target() {
  !receive_outputs_handover(True)
}

fn receive_finish(changed: Bool) {
  let (_, args, state, checkpoint) = receive_ready()
  let args = start.FinishOpeningArgsV1 {
    input_index: 0,
    output_index: 0,
    opening: args.opening,
    checkpoint_bytes: checkpoint,
  }
  let next = receive_claim.finish_outputs_v1(
    state,
    args,
    [],
    fixture.test_certificate_policy_id,
  )
  let output: Data = if changed {
    receive_claim.DeclarationStateV1 {
      ..next,
      scan: receive.DeclarationStateV1 { ..next.scan, target_ordinal: Some(1) },
    }
  } else {
    next
  }
  finish_entry.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state),
      },
    ),
    ct.Continue(args),
    fixture.own_out_ref(),
    step_tx(output, [], False),
  )
}

test receive_finish_authenticates_empty_exhaustion() {
  receive_finish(False)
}

test receive_finish_cannot_change_leaf_target() {
  !receive_finish(True)
}

fn declaration_finish(changed: Bool) {
  let (_, args, state, checkpoint) = receive_ready()
  let state = receive_claim.finish_outputs_v1(
    state,
    start.FinishOpeningArgsV1 {
      input_index: 0,
      output_index: 0,
      opening: args.opening,
      checkpoint_bytes: checkpoint,
    },
    [],
    fixture.test_certificate_policy_id,
  )
  let expected = receive_claim.finish_declaration_v1(state)
  let output: Data = if changed {
    ClaimPurposeStateV1 { ..expected, purpose: NotRequired }
  } else {
    expected
  }
  declaration_entry.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state),
      },
    ),
    ct.Continue(declaration.Finish { input_index: 0, output_index: 0 }),
    fixture.own_out_ref(),
    step_tx(output, [], False),
  )
}

test receive_empty_declaration_finishes_without_owner_signature() {
  declaration_finish(False)
}

test receive_declaration_cannot_drop_impossible_receipt() {
  !declaration_finish(True)
}

fn declaration_scan(signatures: List<ByteArray>) {
  let Pair(observed, _) = empty_domain(3)
  let Pair(output_root, expected_count) = receive.admit_output_v1(
    env.empty_merkle_tree_root,
    0,
    0,
    Some(hash),
    receive.FirstCredential { proof: [] },
  )
  let state = receive_claim.DeclarationStateV1 {
    context: observed,
    scan: receive.DeclarationStateV1 {
      bound: observed.bound.bound,
      source_index: observed.bound.source_index,
      output_root,
      expected_count,
      declared_root: env.empty_merkle_tree_root,
      declared_count: 0,
      previous_credential: #"",
      target_ordinal: Some(0),
      selected: None,
    },
  }
  let entries = [
    receive.DeclaredCredentialV1 {
      script_hash: hash,
      first_output_index: 0,
      proof: [],
    },
  ]
  let output: Data = receive_claim.advance_declaration_v1(state, entries)
  declaration_entry.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state),
      },
    ),
    ct.Continue(declaration.Scan { input_index: 0, output_index: 0, entries }),
    fixture.own_out_ref(),
    step_tx(output, signatures, True),
  )
}

test receive_free_declaration_choice_requires_prover_signature_positive() {
  declaration_scan([fixture.fraud_prover_key_hash])
}

test receive_free_declaration_missing_signature_refuses() fail {
  declaration_scan([])
}

test receive_free_declaration_wrong_signature_refuses() fail {
  declaration_scan([hash])
}
