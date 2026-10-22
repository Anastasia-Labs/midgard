use aiken/builtin
use aiken/collection/list
use aiken/merkle_patricia_forestry as mpf
use aiken/primitive/bytearray
use cardano/address
use cardano/assets
use cardano/transaction.{
  InlineDatum, Input, NoDatum, Output, OutputReference, Spend, Transaction,
  Withdraw,
}
use env
use fraud_proofs/min_ada/step_01 as step_01_validator
use fraud_proofs/min_ada/step_02 as step_02_validator
use fraud_proofs/min_ada/step_02_yields as step_02_yields
use fraud_proofs/min_ada/step_03 as step_03_validator
use fraud_proofs/min_ada/step_04 as step_04_validator
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/forced_submission_fixture as forced_fixture
use midgard/fraud_proof_catalogue
use midgard/fraud_proofs/field_opening_v1 as field_opening
use midgard/fraud_proofs/min_ada/step_01.{
  Args as Step01Args, ForcedSource, MinAdaTx,
}
use midgard/fraud_proofs/min_ada/step_02.{
  Args as Step02Args, Datum as Step02Datum, State, YieldMinAdaStep02V1,
}
use midgard/fraud_proofs/min_ada/step_03.{
  Args as Step03Args, Datum as Step03Datum, MinAdaTxDescriptor, MinAdaTxScan,
}
use midgard/fraud_proofs/min_ada/step_04.{
  Args as Step04Args, Datum as Step04Datum, PredicateAuthenticated,
}
use midgard/fraud_proofs/min_ada/yield as min_ada_yield
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_output}
use midgard/fraud_proofs/native_tx/types.{
  MidgardTransaction, MidgardTransactionBody, MidgardTxOutput, MidgardValue,
  TxIsInvalid,
}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/transaction_output_non_canonical/rule as scan_rule
use midgard/ledger_state
use midgard/native_tx_machine_walk_v1 as field_walk
use midgard/rejection_reason_v1.{
  FeeBelowMinimum, ForcedTxInvalid, ForcedTxValid, OperatorVerdictV1,
  OutputBelowMinAda,
}
use midgard/transition_trace
use midgard/validation_machine/shared as vm_shared

const tx_yield_hash =
  #"abababababababababababababababababababababababababababab"

fn with_authenticated_yield(
  tx: Transaction,
  role: ByteArray,
  script_hash: ByteArray,
  step_datum: Step02Datum,
  spend_redeemer: ct.StepRedeemer<Step02Args>,
) -> Transaction {
  expect [thread_input] = tx.inputs
  let dispatcher_input = Input {
    ..thread_input,
    output: Output { ..thread_input.output, datum: InlineDatum(step_datum) },
  }
  let yield_input = Input {
    output_reference: OutputReference {
      transaction_id: #"adadadadadadadadadadadadadadadadadadadadadadadadadadadadadadadad",
      output_index: 0,
    },
    output: Output {
      address: address.from_script(script_hash),
      value: assets.from_lovelace(2_000_000)
        |> assets.merge(
            assets.from_asset(fixture.test_certificate_policy_id, role, 1),
          ),
      datum: NoDatum,
      reference_script: Some(script_hash),
    },
  }
  let yield_redeemer: Data = 0
  let dispatcher_redeemer: Data = spend_redeemer
  Transaction {
    ..tx,
    inputs: [dispatcher_input],
    reference_inputs: list.concat(tx.reference_inputs, [yield_input]),
    withdrawals: [Pair(address.Script(script_hash), 0)],
    redeemers: list.concat(
      tx.redeemers,
      [
        Pair(Spend(dispatcher_input.output_reference), dispatcher_redeemer),
        Pair(Withdraw(address.Script(script_hash)), yield_redeemer),
      ],
    ),
  }
}

fn output_at_delta(delta: Int) -> MidgardTxOutput {
  let base = fixture.sample_native_output()
  let base_cbor = encode_midgard_tx_output(base)
  let floor = vm_shared.min_ada_lovelace_v1(
    env.coins_per_utxo_byte,
    bytearray.length(base_cbor),
  )
  let value = base.value
  let output = MidgardTxOutput {
    ..base,
    value: MidgardValue { ..value, lovelace: floor + delta },
  }
  // Both the 2-Ada seed and every floor boundary used here have the same
  // canonical uint width. This assertion prevents a self-referential fixture
  // from silently changing the serialized length after deriving its floor.
  expect
    bytearray.length(encode_midgard_tx_output(output)) == bytearray.length(
      base_cbor,
    )
  output
}

fn transaction_with_output(output: MidgardTxOutput) -> MidgardTransaction {
  let base = fixture.valid_native_tx_v1()
  MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, outputs: [output] },
  }
}

fn run_tx_step_01(tx: MidgardTransaction, output_index: Int) -> Bool {
  let block = fixture.native_block_fixture_v1(tx)
  let fault = MinAdaTx { output_index }
  let expected_state: Data = State {
    grammar_checkpoint_hash: #"",
    grammar_complete: False,
    walk_checkpoint_hash: #"",
    direction: 0,
    source_kind: 0,
    bad_tx_id: block.tx_id,
    fault,
  }
  step_01_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(fixture.step_datum_without_state()),
    ct.Continue(
      Step01Args {
        forced_source: None,
        tx_inclusion: Some(fixture.native_inclusion_carriage_v1(block)),
        fault,
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_step_l1_tx_v1(
      block,
      expected_state,
      block.transactions_phas_root,
      block.tx_id,
      block.source_cbor,
    ),
  )
}

test q27_min_ada_step_01_binds_an_accepted_transaction_output() {
  run_tx_step_01(transaction_with_output(output_at_delta(-1)), 0)
}

test q27_min_ada_step_01_rejects_an_invalid_transaction_leaf() fail {
  let tx = transaction_with_output(output_at_delta(-1))
  run_tx_step_01(MidgardTransaction { ..tx, validity: TxIsInvalid }, 0)
}

test q27_min_ada_step_01_rejects_a_negative_output_index() fail {
  run_tx_step_01(transaction_with_output(output_at_delta(-1)), -1)
}

// ## Forced-root binding
//
// A forced leaf binds in either polarity. An accepted leaf (`ForcedTxValid`)
// put every output into the post-block ledger, so a named output below the
// floor is a wrongful acceptance; a rejected leaf must name exactly the
// claimed output in its `OutputBelowMinAda` reason.

const forced_order_key_transaction_id =
  #"7777777777777777777777777777777777777777777777777777777777777777"

fn forced_order_key() -> ledger_state.TxOrderId {
  OutputReference {
    transaction_id: forced_order_key_transaction_id,
    output_index: 0,
  }
}

fn forced_leaf(
  tx: MidgardTransaction,
  verdict: OperatorVerdictV1,
) -> ledger_state.ForcedInclusionTxV1 {
  ledger_state.ForcedInclusionTxV1 {
    tx_id: fixture.native_block_fixture_v1(tx).tx_id,
    submitted_source: forced_fixture.from_native_transaction(tx),
    verdict,
  }
}

fn forced_membership(
  leaf: ledger_state.ForcedInclusionTxV1,
) -> transition_trace.RootMembershipProof<
  ledger_state.TxOrderId,
  ledger_state.ForcedInclusionTxV1,
> {
  let key = forced_order_key()
  let phas_root = mpf.root(
    mpf.insert(
      mpf.empty,
      builtin.serialise_data(key),
      builtin.serialise_data(leaf),
      [],
    ),
  )
  transition_trace.RootMembershipProof {
    domain: transition_trace.ForcedTransactionsV1RootDomain,
    root: transition_trace.commit_counted_root(
      transition_trace.ForcedTransactionsV1RootDomain,
      phas_root,
      1,
    ),
    phas_root,
    count: 1,
    key,
    value: leaf,
    proof: [],
  }
}

fn forced_header(root: ByteArray) -> ledger_state.HeaderV1 {
  let base = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  ledger_state.HeaderV1 {
    ..base.header,
    forced_transactions_root: root,
    forced_transaction_count: 1,
  }
}

fn run_forced_step_01(
  verdict: OperatorVerdictV1,
  direction: Int,
  claimed_index: Int,
  state_direction: Int,
) -> Bool {
  let tx = transaction_with_output(output_at_delta(-1))
  let leaf = forced_leaf(tx, verdict)
  let membership = forced_membership(leaf)
  let header = forced_header(membership.root)
  let header_data: Data = header
  let thread_name = bytearray.concat(
    fixture.fraud_category_id,
    utils.serialise_and_hash_28(header_data),
  )
  expect
    bytearray.length(fixture.fraud_category_id) == fraud_proof_catalogue.id_byte_count
  let fault = MinAdaTx { output_index: claimed_index }
  let expected_state: Data = State {
    grammar_checkpoint_hash: #"",
    grammar_complete: False,
    walk_checkpoint_hash: #"",
    direction: state_direction,
    source_kind: substrate.source_kind_forced,
    bad_tx_id: leaf.tx_id,
    fault,
  }
  step_01_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(fixture.step_datum_without_state()),
    ct.Continue(
      Step01Args {
        forced_source: Some(
          ForcedSource { input_index: 0, output_index: 0, header, membership },
        ),
        tx_inclusion: None,
        fault,
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(thread_name, expected_state),
  )
}

test q27_min_ada_step_01_binds_an_accepted_forced_output() {
  run_forced_step_01(ForcedTxValid, 0, 0, 0)
}

test q27_min_ada_step_01_binds_a_rejected_forced_output() {
  run_forced_step_01(
    ForcedTxInvalid { reason: OutputBelowMinAda { output_index: 0 } },
    1,
    0,
    1,
  )
}

test q27_min_ada_step_01_rejects_an_accepted_forced_leaf_as_rejection() fail {
  run_forced_step_01(ForcedTxValid, 1, 0, 1)
}

test q27_min_ada_step_01_rejects_a_rejected_forced_leaf_as_acceptance() fail {
  run_forced_step_01(
    ForcedTxInvalid { reason: OutputBelowMinAda { output_index: 0 } },
    0,
    0,
    0,
  )
}

test q27_min_ada_step_01_rejects_a_mismatched_forced_reason_index() fail {
  run_forced_step_01(
    ForcedTxInvalid { reason: OutputBelowMinAda { output_index: 1 } },
    1,
    0,
    1,
  )
}

test q27_min_ada_step_01_rejects_an_unrelated_forced_reason() fail {
  run_forced_step_01(ForcedTxInvalid { reason: FeeBelowMinimum }, 1, 0, 1)
}

test q27_min_ada_step_01_rejects_a_negative_accepted_forced_index() fail {
  run_forced_step_01(ForcedTxValid, 0, -1, 0)
}

test q27_min_ada_step_01_rejects_a_forced_state_with_the_wrong_direction() fail {
  run_forced_step_01(ForcedTxValid, 0, 0, 1)
}

fn run_tx_finalize(
  output: MidgardTxOutput,
  carried_output_cbor: ByteArray,
) -> Bool {
  let block = fixture.native_block_fixture_v1(transaction_with_output(output))
  let Pair(view, initial) = field_opening.opened_field_grammar_certification(
    fixture.inline_body_opening(block.compact_cbor, [carried_output_cbor]),
    field_opening.BodyAnchor { tx_id: block.tx_id, source_kind: 0 },
    2,
    [],
    fixture.test_certificate_policy_id,
  )
  let grammar = field_walk.certify_field_grammar(view, initial, 32)
  let state = State {
    grammar_checkpoint_hash: field_walk.field_grammar_checkpoint_hash(grammar),
    grammar_complete: True,
    walk_checkpoint_hash: #"",
    direction: 0,
    source_kind: 0,
    bad_tx_id: block.tx_id,
    fault: MinAdaTx { output_index: 0 },
  }
  let next_state_data: Data = MinAdaTxScan {
    direction: 0,
    scan: scan_rule.initial_scan_v1(
      scan_rule.BoundOutputV1 {
        subject: substrate.VerdictSubjectV1 {
          version: 1,
          direction: 0,
          source_kind: 0,
          transaction_id: block.tx_id,
          source_key: #"",
          rejection_reason: None,
        },
        output_index: 0,
      },
      carried_output_cbor,
    ),
  }
  let base_tx = fixture.continue_mid_step_l1_tx_v1(
    fixture.ct_token_asset_name(block),
    next_state_data,
  )
  let step_datum = ct.StepDatum {
    fraud_prover: fixture.fraud_prover_key_hash,
    data: Some(state),
  }
  let spend_redeemer = ct.Continue(
    Step02Args {
      grammar_checkpoint_bytes: field_walk.encode_field_grammar_checkpoint(
        grammar,
      ),
      walk_checkpoint_bytes: #"",
      input_index: 0,
      output_index: 0,
      yield_to_ref_input_index: list.length(base_tx.reference_inputs),
      outputs_opening: fixture.inline_body_opening(
        block.compact_cbor,
        [carried_output_cbor],
      ),
    },
  )
  let l1_tx = with_authenticated_yield(
    base_tx,
    min_ada_yield.tx_role,
    tx_yield_hash,
    step_datum,
    spend_redeemer,
  )
  and {
    step_02_validator.main.spend(
      fixture.next_step_script_hash,
      fixture.computation_thread_policy_id,
      fixture.test_certificate_policy_id,
      Some(step_datum),
      spend_redeemer,
      fixture.own_out_ref(),
      l1_tx,
    ),
    step_02_yields.tx.withdraw(
      fixture.step_script_hash,
      fixture.test_certificate_policy_id,
      YieldMinAdaStep02V1,
      address.Script(tx_yield_hash),
      l1_tx,
    ),
    run_tx_predicate(
      bytearray.length(carried_output_cbor),
      output.value.lovelace,
    ),
  }
}

test q27_min_ada_tx_convicts_one_lovelace_below_the_exact_floor() {
  let output = output_at_delta(-1)
  run_tx_finalize(output, encode_midgard_tx_output(output))
}

test q27_min_ada_tx_rejects_the_exact_floor() fail {
  let output = output_at_delta(0)
  run_tx_finalize(output, encode_midgard_tx_output(output))
}

test q27_min_ada_tx_rejects_one_lovelace_above_the_floor() fail {
  let output = output_at_delta(1)
  run_tx_finalize(output, encode_midgard_tx_output(output))
}

test q27_min_ada_tx_rejects_a_forged_or_malformed_output_opening() fail {
  run_tx_finalize(output_at_delta(-1), #"ff")
}

fn run_tx_predicate(total_length: Int, lovelace: Int) -> Bool {
  let state = MinAdaTxDescriptor { direction: 0, total_length, lovelace }
  let terminal_state: Data = PredicateAuthenticated
  let asset_name = fixture.ct_token_asset_name(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
  )
  step_03_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state),
      },
    ),
    ct.Continue(Step03Args { window: #"", input_index: 0, output_index: 0 }),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(asset_name, terminal_state),
  )
}

test q27_min_ada_family_binds_the_exact_production_parameter_snapshot() {
  let maximum_total_length = 16_384
  let floor = vm_shared.min_ada_lovelace_v1(
    env.coins_per_utxo_byte,
    maximum_total_length,
  )
  and {
    env.coins_per_utxo_byte == 4_310,
    floor == 71_304_640,
    vm_shared.output_meets_min_ada_v1(
      env.coins_per_utxo_byte,
      maximum_total_length,
      floor,
    ),
    !vm_shared.output_meets_min_ada_v1(
      env.coins_per_utxo_byte,
      maximum_total_length,
      floor - 1,
    ),
    // A mutated zero-rate snapshot would accept the same underfunded output;
    // the family therefore cannot derive the deployment parameter from the
    // prover or transaction and must stay compiled to `env`.
    vm_shared.output_meets_min_ada_v1(0, maximum_total_length, floor - 1),
  }
}

test q27_min_ada_step_04_finalizes_only_an_adjudicated_thread() {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  step_04_validator.main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(PredicateAuthenticated),
      },
    ),
    ct.Continue(
      Step04Args {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(fixture.ct_token_asset_name(block)),
  )
}

test q27_min_ada_steps_preserve_prover_cancellation() {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let asset_name = fixture.ct_token_asset_name(block)
  let step_02_cancel_datum: Option<Step02Datum> = Some(
    ct.StepDatum { fraud_prover: fixture.fraud_prover_key_hash, data: None },
  )
  let step_03_cancel_datum: Option<Step03Datum> = Some(
    ct.StepDatum { fraud_prover: fixture.fraud_prover_key_hash, data: None },
  )
  let step_04_cancel_datum: Option<Step04Datum> = Some(
    ct.StepDatum { fraud_prover: fixture.fraud_prover_key_hash, data: None },
  )
  and {
    step_01_validator.main.spend(
      fixture.next_step_script_hash,
      fixture.computation_thread_policy_id,
      fixture.hub_oracle_script_hash,
      Some(fixture.step_datum_without_state()),
      ct.Cancel { input_index: 0, computation_thread_mint_redeemer_index: 0 },
      fixture.own_out_ref(),
      fixture.cancel_step_l1_tx_v1(asset_name, asset_name, True),
    ),
    step_02_validator.main.spend(
      fixture.next_step_script_hash,
      fixture.computation_thread_policy_id,
      fixture.test_certificate_policy_id,
      step_02_cancel_datum,
      ct.Cancel { input_index: 0, computation_thread_mint_redeemer_index: 0 },
      fixture.own_out_ref(),
      fixture.cancel_step_l1_tx_v1(asset_name, asset_name, True),
    ),
    step_03_validator.main.spend(
      fixture.next_step_script_hash,
      fixture.computation_thread_policy_id,
      step_03_cancel_datum,
      ct.Cancel { input_index: 0, computation_thread_mint_redeemer_index: 0 },
      fixture.own_out_ref(),
      fixture.cancel_step_l1_tx_v1(asset_name, asset_name, True),
    ),
    step_04_validator.main.spend(
      fixture.fraud_proof_policy_id,
      fixture.fraud_proof_token_address(),
      fixture.computation_thread_policy_id,
      step_04_cancel_datum,
      ct.Cancel { input_index: 0, computation_thread_mint_redeemer_index: 0 },
      fixture.own_out_ref(),
      fixture.cancel_step_l1_tx_v1(asset_name, asset_name, True),
    ),
  }
}
