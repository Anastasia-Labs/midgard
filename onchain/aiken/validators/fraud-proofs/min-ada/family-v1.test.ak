use aiken/builtin
use aiken/collection/list
use aiken/crypto.{blake2b_256}
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
use fraud_proofs/min_ada/step_05 as step_05_validator
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proofs/chunked_inclusion_v1.{PublishedProofCarriage}
use midgard/fraud_proofs/common.{
  PublishedChunkMembership, PublishedChunkNonMembership,
  RedeemerCarriedMembership, RedeemerCarriedNonMembership,
}
use midgard/fraud_proofs/min_ada/step_01.{
  Args as Step01Args, MinAdaTx, MinAdaUtxo, PostUtxoMembershipV1,
  PostUtxoStateV1,
}
use midgard/fraud_proofs/min_ada/step_02.{
  Args as Step02Args, Datum as Step02Datum, State, YieldMinAdaStep02V1,
}
use midgard/fraud_proofs/min_ada/step_03.{
  Args as Step03Args, Datum as Step03Datum, MinAdaTxDescriptor,
  MinAdaUtxoDescriptor, State as Step03State,
}
use midgard/fraud_proofs/min_ada/step_04.{
  Args as Step04Args, Datum as Step04Datum, State as Step04State,
}
use midgard/fraud_proofs/min_ada/step_05.{
  Args as Step05Args, Datum as Step05Datum, PredicateAndCulpabilityAuthenticated,
}
use midgard/fraud_proofs/min_ada/yield as min_ada_yield
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_output}
use midgard/fraud_proofs/native_tx/types.{
  MidgardTransaction, MidgardTransactionBody, MidgardTxOutput, MidgardValue,
  TxIsInvalid,
}
use midgard/fraud_proofs/transition_trace/proof.{ledger_outref_key}
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_descriptor_v1
use midgard/ledger_state
use midgard/validation_machine/shared as vm_shared
use pexcludes as pexcludes_validator
use phas as phas_validator

const tx_yield_hash =
  #"abababababababababababababababababababababababababababab"

const utxo_yield_hash =
  #"acacacacacacacacacacacacacacacacacacacacacacacacacacacac"

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

fn block_at_utxos_roots(
  base: fixture.NativeBlockFixtureV1,
  prev_utxos_root: ByteArray,
  utxos_root: ByteArray,
) -> fixture.NativeBlockFixtureV1 {
  let header = ledger_state.HeaderV1 {
    ..base.header,
    prev_utxos_root,
    utxos_root,
  }
  let header_data: Data = header
  fixture.NativeBlockFixtureV1 {
    ..base,
    header,
    header_hash: utils.serialise_and_hash_28(header_data),
  }
}

fn descriptor_material(delta: Int) -> (OutputReference, ByteArray, ByteArray) {
  let output_cbor = encode_midgard_tx_output(output_at_delta(delta))
  expect Some(descriptor) = ledger_output_descriptor_v1.build_v1(0, output_cbor)
  let descriptor_cbor = ledger_output_commitment_v1.encode(descriptor)
  let out_ref = OutputReference {
    transaction_id: #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc",
    output_index: 0,
  }
  let root = mpf.root(
    mpf.insert(mpf.empty, ledger_outref_key(out_ref), descriptor_cbor, []),
  )
  (out_ref, descriptor_cbor, root)
}

/// A well-formed descriptor at the canonical 16,384-byte output ceiling.
/// The post-root family intentionally adjudicates the exact descriptor that
/// the ledger commits; it does not carry or rebuild the full output preimage.
fn maximum_descriptor_material() -> (OutputReference, ByteArray) {
  let output_cbor = encode_midgard_tx_output(output_at_delta(-1))
  expect Some(base) = ledger_output_descriptor_v1.build_v1(0, output_cbor)
  let maximum_total_length = 16_384
  let floor = vm_shared.min_ada_lovelace_v1(
    env.coins_per_utxo_byte,
    maximum_total_length,
  )
  let descriptor = ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
    ..base,
    total_length: maximum_total_length,
    lovelace: floor - 1,
  }
  let out_ref = OutputReference {
    transaction_id: #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc",
    output_index: 0,
  }
  (out_ref, ledger_output_commitment_v1.encode(descriptor))
}

fn run_tx_step_01(tx: MidgardTransaction, output_index: Int) -> Bool {
  let block = fixture.native_block_fixture_v1(tx)
  let fault = MinAdaTx { output_index }
  let expected_state: Data = State {
    bad_tx_id: block.tx_id,
    fault,
    post_utxo: None,
  }
  step_01_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(fixture.step_datum_without_state()),
    ct.Continue(
      Step01Args {
        tx_inclusion: Some(fixture.native_inclusion_carriage_v1(block)),
        post_utxo_membership: None,
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

fn run_post_step_01_with_claim(
  block: fixture.NativeBlockFixtureV1,
  membership: PostUtxoMembershipV1,
  withdrawal_root: ByteArray,
  withdrawal_key: ByteArray,
  withdrawal_value: ByteArray,
) -> Bool {
  let fault = MinAdaUtxo
  let expected_state: Data = State {
    bad_tx_id: membership.out_ref.transaction_id,
    fault,
    post_utxo: Some(
      PostUtxoStateV1 {
        out_ref: membership.out_ref,
        descriptor_cbor: membership.descriptor_cbor,
        post_utxos_root: block.header.utxos_root,
        prev_utxos_root: block.header.prev_utxos_root,
      },
    ),
  }
  step_01_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(fixture.step_datum_without_state()),
    ct.Continue(
      Step01Args {
        tx_inclusion: None,
        post_utxo_membership: Some(membership),
        fault,
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_step_l1_tx_v1(
      block,
      expected_state,
      withdrawal_root,
      withdrawal_key,
      withdrawal_value,
    ),
  )
}

fn post_fixture(
  delta: Int,
) -> (fixture.NativeBlockFixtureV1, PostUtxoMembershipV1) {
  let (out_ref, descriptor_cbor, post_root) = descriptor_material(delta)
  let block = block_at_utxos_roots(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
    env.empty_merkle_tree_root,
    post_root,
  )
  (
    block,
    PostUtxoMembershipV1 {
      input_index: 0,
      output_index: 0,
      hub_ref_input_index: 0,
      state_queue_node_ref_input_index: 1,
      out_ref,
      descriptor_cbor,
    },
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

test q27_min_ada_step_01_binds_a_post_utxo_descriptor() {
  let (block, membership) = post_fixture(-1)
  run_post_step_01_with_claim(
    block,
    membership,
    block.header.utxos_root,
    ledger_outref_key(membership.out_ref),
    membership.descriptor_cbor,
  )
}

test q27_min_ada_step_02_rejects_a_forged_post_value() fail {
  let (block, membership) = post_fixture(-1)
  let (_, other, _) = descriptor_material(0)
  run_post_membership_direct(
    membership.out_ref,
    other,
    block.header.utxos_root,
    block.header.prev_utxos_root,
    [],
  )
}

test q27_min_ada_step_02_rejects_a_forged_post_key() fail {
  let (block, membership) = post_fixture(-1)
  let forged = OutputReference {
    transaction_id: #"dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd",
    output_index: 0,
  }
  run_post_membership_direct(
    forged,
    membership.descriptor_cbor,
    block.header.utxos_root,
    block.header.prev_utxos_root,
    [],
  )
}

test q27_min_ada_step_02_rejects_a_forged_post_root() fail {
  let (block, membership) = post_fixture(-1)
  run_post_membership_direct(
    membership.out_ref,
    membership.descriptor_cbor,
    blake2b_256(#"bad0cafe"),
    block.header.prev_utxos_root,
    [],
  )
}

test q27_min_ada_step_01_binds_a_maximum_post_claim_without_walking_it() {
  let (out_ref, descriptor_cbor) = maximum_descriptor_material()
  let key = ledger_outref_key(out_ref)
  let steps = fixture.adversarial_branch_steps(64)
  let post_root = fixture.adversarial_membership_root(
    key,
    descriptor_cbor,
    steps,
  )
  let membership = PostUtxoMembershipV1 {
    input_index: 0,
    output_index: 0,
    hub_ref_input_index: 0,
    state_queue_node_ref_input_index: 1,
    out_ref,
    descriptor_cbor,
  }
  let block = block_at_utxos_roots(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
    env.empty_merkle_tree_root,
    post_root,
  )
  run_post_step_01_with_claim(
    block,
    membership,
    post_root,
    key,
    descriptor_cbor,
  )
}

test q27_min_ada_step_02_authenticates_maximum_published_post_membership() {
  let (out_ref, descriptor_cbor) = maximum_descriptor_material()
  let key = ledger_outref_key(out_ref)
  let steps = fixture.adversarial_branch_steps(64)
  let post_root = fixture.adversarial_membership_root(
    key,
    descriptor_cbor,
    steps,
  )
  let chunks = fixture.proof_chunk_reference_inputs(steps)
  let carriage = PublishedProofCarriage {
    ordered_chunk_reference_input_indices: [0, 1, 2, 3],
  }
  run_post_membership_published(
    out_ref,
    descriptor_cbor,
    post_root,
    env.empty_merkle_tree_root,
    carriage,
    chunks,
  )
}

test q27_min_ada_step_02_rejects_reordered_published_post_chunks() fail {
  let (out_ref, descriptor_cbor) = maximum_descriptor_material()
  let key = ledger_outref_key(out_ref)
  let steps = fixture.adversarial_branch_steps(64)
  let post_root = fixture.adversarial_membership_root(
    key,
    descriptor_cbor,
    steps,
  )
  let chunks = fixture.proof_chunk_reference_inputs(steps)
  let carriage = PublishedProofCarriage {
    ordered_chunk_reference_input_indices: [0, 2, 1, 3],
  }
  run_post_membership_published(
    out_ref,
    descriptor_cbor,
    post_root,
    env.empty_merkle_tree_root,
    carriage,
    chunks,
  )
}

fn run_tx_finalize(
  output: MidgardTxOutput,
  carried_output_cbor: ByteArray,
) -> Bool {
  run_tx_step_02_with_successors(
    output,
    carried_output_cbor,
    fixture.next_step_script_hash,
    fixture.next_step_script_hash,
  )
}

fn run_tx_step_02_with_successors(
  output: MidgardTxOutput,
  carried_output_cbor: ByteArray,
  step_03_script_hash: ByteArray,
  step_04_script_hash: ByteArray,
) -> Bool {
  let block = fixture.native_block_fixture_v1(transaction_with_output(output))
  let state = State {
    bad_tx_id: block.tx_id,
    fault: MinAdaTx { output_index: 0 },
    post_utxo: None,
  }
  let next_state_data: Data = MinAdaTxDescriptor {
    total_length: bytearray.length(carried_output_cbor),
    lovelace: output.value.lovelace,
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
      input_index: 0,
      output_index: 0,
      yield_to_ref_input_index: list.length(base_tx.reference_inputs),
      outputs_opening: Some(
        fixture.inline_body_opening(block.compact_cbor, [carried_output_cbor]),
      ),
      post_membership: None,
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
      step_03_script_hash,
      step_04_script_hash,
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

fn post_state(
  out_ref: OutputReference,
  descriptor_cbor: ByteArray,
  post_root: ByteArray,
  prev_root: ByteArray,
) -> State {
  State {
    bad_tx_id: out_ref.transaction_id,
    fault: MinAdaUtxo,
    post_utxo: Some(
      PostUtxoStateV1 {
        out_ref,
        descriptor_cbor,
        post_utxos_root: post_root,
        prev_utxos_root: prev_root,
      },
    ),
  }
}

fn run_post_membership_published(
  out_ref: OutputReference,
  descriptor_cbor: ByteArray,
  post_root: ByteArray,
  prev_root: ByteArray,
  carriage: PublishedProofCarriage,
  chunks: List<Input>,
) -> Bool {
  let state = post_state(out_ref, descriptor_cbor, post_root, prev_root)
  let next_state: Data = MinAdaUtxoDescriptor {
    descriptor_cbor,
    out_ref_key: ledger_outref_key(out_ref),
    prev_utxos_root: prev_root,
  }
  let asset_name = fixture.ct_token_asset_name(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
  )
  let base_tx = fixture.continue_mid_step_l1_tx_v1(asset_name, next_state)
  let proof_tx = Transaction { ..base_tx, reference_inputs: chunks }
  let step_datum = ct.StepDatum {
    fraud_prover: fixture.fraud_prover_key_hash,
    data: Some(state),
  }
  let spend_redeemer = ct.Continue(
    Step02Args {
      input_index: 0,
      output_index: 0,
      yield_to_ref_input_index: list.length(chunks),
      outputs_opening: None,
      post_membership: Some(PublishedChunkMembership(carriage)),
    },
  )
  let l1_tx = with_authenticated_yield(
    proof_tx,
    min_ada_yield.utxo_role,
    utxo_yield_hash,
    step_datum,
    spend_redeemer,
  )
  and {
    step_02_validator.main.spend(
      fixture.next_step_script_hash,
      fixture.step_script_hash,
      fixture.computation_thread_policy_id,
      fixture.test_certificate_policy_id,
      Some(step_datum),
      spend_redeemer,
      fixture.own_out_ref(),
      l1_tx,
    ),
    step_02_yields.utxo.withdraw(
      fixture.step_script_hash,
      YieldMinAdaStep02V1,
      address.Script(utxo_yield_hash),
      l1_tx,
    ),
  }
}

fn run_post_membership_direct(
  out_ref: OutputReference,
  descriptor_cbor: ByteArray,
  post_root: ByteArray,
  prev_root: ByteArray,
  proof: mpf.Proof,
) -> Bool {
  let state = post_state(out_ref, descriptor_cbor, post_root, prev_root)
  let next_state: Data = MinAdaUtxoDescriptor {
    descriptor_cbor,
    out_ref_key: ledger_outref_key(out_ref),
    prev_utxos_root: prev_root,
  }
  let asset_name = fixture.ct_token_asset_name(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
  )
  let base_tx = fixture.continue_mid_step_l1_tx_v1(asset_name, next_state)
  let key = ledger_outref_key(out_ref)
  let Pair(_, membership_redeemer) as membership_entry = fixture.phas_withdraw_redeemer_entry(
    post_root,
    key,
    descriptor_cbor,
  )
  let membership_tx = Transaction {
    ..base_tx,
    redeemers: list.concat(base_tx.redeemers, [membership_entry]),
  }
  let step_datum = ct.StepDatum {
    fraud_prover: fixture.fraud_prover_key_hash,
    data: Some(state),
  }
  let spend_redeemer = ct.Continue(
    Step02Args {
      input_index: 0,
      output_index: 0,
      yield_to_ref_input_index: 0,
      outputs_opening: None,
      post_membership: Some(
        RedeemerCarriedMembership {
          membership_proof: proof,
          membership_proof_script_redeemer_index: 0,
        },
      ),
    },
  )
  let l1_tx = with_authenticated_yield(
    membership_tx,
    min_ada_yield.utxo_role,
    utxo_yield_hash,
    step_datum,
    spend_redeemer,
  )
  and {
    step_02_validator.main.spend(
      fixture.next_step_script_hash,
      fixture.step_script_hash,
      fixture.computation_thread_policy_id,
      fixture.test_certificate_policy_id,
      Some(step_datum),
      spend_redeemer,
      fixture.own_out_ref(),
      l1_tx,
    ),
    phas_validator.membership.withdraw(
      membership_redeemer,
      address.Script(env.plutarch_phas_validator_hash),
      l1_tx,
    ),
    step_02_yields.utxo.withdraw(
      fixture.step_script_hash,
      YieldMinAdaStep02V1,
      address.Script(utxo_yield_hash),
      l1_tx,
    ),
  }
}

fn run_tx_predicate(total_length: Int, lovelace: Int) -> Bool {
  let state = MinAdaTxDescriptor { total_length, lovelace }
  let terminal_state: Data = PredicateAndCulpabilityAuthenticated
  let asset_name = fixture.ct_token_asset_name(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
  )
  step_03_validator.main.spend(
    fixture.step_script_hash,
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state),
      },
    ),
    ct.Continue(Step03Args { input_index: 0, output_index: 0 }),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(asset_name, terminal_state),
  )
}

fn run_post_predicate(descriptor_cbor: ByteArray) -> Bool {
  run_post_predicate_at(descriptor_cbor, fixture.next_step_script_hash)
}

fn run_post_predicate_at(
  descriptor_cbor: ByteArray,
  step_04_script_hash: ByteArray,
) -> Bool {
  let (out_ref, _, _) = descriptor_material(-1)
  let state = MinAdaUtxoDescriptor {
    descriptor_cbor,
    out_ref_key: ledger_outref_key(out_ref),
    prev_utxos_root: env.empty_merkle_tree_root,
  }
  let next_state: Data = Step04State {
    out_ref_key: ledger_outref_key(out_ref),
    prev_utxos_root: env.empty_merkle_tree_root,
  }
  let asset_name = fixture.ct_token_asset_name(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
  )
  step_03_validator.main.spend(
    step_04_script_hash,
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state),
      },
    ),
    ct.Continue(Step03Args { input_index: 0, output_index: 0 }),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(asset_name, next_state),
  )
}

fn run_post_predecessor_with_claim(
  prev_root: ByteArray,
  withdrawal_root: ByteArray,
  withdrawal_key: ByteArray,
  proof: mpf.Proof,
) -> Bool {
  run_post_predecessor_with_claim_at(
    prev_root,
    withdrawal_root,
    withdrawal_key,
    proof,
    fixture.next_step_script_hash,
  )
}

fn run_post_predecessor_with_claim_at(
  prev_root: ByteArray,
  withdrawal_root: ByteArray,
  withdrawal_key: ByteArray,
  proof: mpf.Proof,
  step_05_script_hash: ByteArray,
) -> Bool {
  let (out_ref, _, _) = descriptor_material(-1)
  let state = Step04State {
    out_ref_key: ledger_outref_key(out_ref),
    prev_utxos_root: prev_root,
  }
  let asset_name = fixture.ct_token_asset_name(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
  )
  let terminal_state_data: Data = PredicateAndCulpabilityAuthenticated
  let base_tx = fixture.continue_mid_step_l1_tx_v1(
    asset_name,
    terminal_state_data,
  )
  let l1_tx = Transaction {
    ..base_tx,
    redeemers: list.concat(
      base_tx.redeemers,
      [
        fixture.pexcludes_withdraw_redeemer_entry(
          withdrawal_root,
          withdrawal_key,
          proof,
        ),
      ],
    ),
  }
  let proof_data: Data = proof
  let exclusion_redeemer: Data = [
    builtin.b_data(withdrawal_root),
    builtin.b_data(withdrawal_key),
    proof_data,
  ]
  and {
    step_04_validator.main.spend(
      step_05_script_hash,
      fixture.computation_thread_policy_id,
      Some(
        ct.StepDatum {
          fraud_prover: fixture.fraud_prover_key_hash,
          data: Some(state),
        },
      ),
      ct.Continue(
        Step04Args {
          input_index: 0,
          output_index: 0,
          predecessor_non_membership: RedeemerCarriedNonMembership {
            non_membership_proof: proof,
            non_membership_proof_script_redeemer_index: 0,
          },
        },
      ),
      fixture.own_out_ref(),
      l1_tx,
    ),
    pexcludes_validator.exclusion.withdraw(
      exclusion_redeemer,
      address.Script(env.plutarch_pexcludes_validator_hash),
      l1_tx,
    ),
  }
}

fn run_post_predecessor_published(
  out_ref: OutputReference,
  prev_root: ByteArray,
  carriage: PublishedProofCarriage,
  chunks: List<Input>,
) -> Bool {
  let state = Step04State {
    out_ref_key: ledger_outref_key(out_ref),
    prev_utxos_root: prev_root,
  }
  let asset_name = fixture.ct_token_asset_name(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
  )
  let terminal_state_data: Data = PredicateAndCulpabilityAuthenticated
  let base_tx = fixture.continue_mid_step_l1_tx_v1(
    asset_name,
    terminal_state_data,
  )
  let l1_tx = Transaction { ..base_tx, reference_inputs: chunks }
  step_04_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state),
      },
    ),
    ct.Continue(
      Step04Args {
        input_index: 0,
        output_index: 0,
        predecessor_non_membership: PublishedChunkNonMembership(carriage),
      },
    ),
    fixture.own_out_ref(),
    l1_tx,
  )
}

test q27_min_ada_utxo_convicts_a_new_underfunded_post_member() {
  let (out_ref, descriptor, post_root) = descriptor_material(-1)
  and {
    run_post_membership_direct(
      out_ref,
      descriptor,
      post_root,
      env.empty_merkle_tree_root,
      [],
    ),
    run_post_predicate(descriptor),
    run_post_predecessor_with_claim(
      env.empty_merkle_tree_root,
      env.empty_merkle_tree_root,
      ledger_outref_key(out_ref),
      [],
    ),
  }
}

test q27_min_ada_utxo_rejects_the_exact_floor() fail {
  let (_, descriptor, _) = descriptor_material(0)
  run_post_predicate(descriptor)
}

test q27_min_ada_utxo_rejects_an_inherited_underfunded_member() fail {
  let (out_ref, _, post_root) = descriptor_material(-1)
  run_post_predecessor_with_claim(
    post_root,
    post_root,
    ledger_outref_key(out_ref),
    [],
  )
}

test q27_min_ada_utxo_rejects_a_forged_predecessor_root() fail {
  let (out_ref, _, _) = descriptor_material(-1)
  let (foreign_root, _) = fixture.absence_witness()
  run_post_predecessor_with_claim(
    env.empty_merkle_tree_root,
    foreign_root,
    ledger_outref_key(out_ref),
    [],
  )
}

test q27_min_ada_utxo_rejects_a_forged_predecessor_key() fail {
  let forged = OutputReference {
    transaction_id: #"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee",
    output_index: 0,
  }
  run_post_predecessor_with_claim(
    env.empty_merkle_tree_root,
    env.empty_merkle_tree_root,
    ledger_outref_key(forged),
    [],
  )
}

test q27_min_ada_utxo_rejects_malformed_descriptor_bytes() fail {
  run_post_predicate(#"80")
}

test q27_min_ada_utxo_authenticates_maximum_descriptor_predicate() {
  let (_, descriptor_cbor) = maximum_descriptor_material()
  run_post_predicate(descriptor_cbor)
}

test q27_min_ada_utxo_authenticates_maximum_published_predecessor_non_membership() {
  let (out_ref, _) = maximum_descriptor_material()
  let key = ledger_outref_key(out_ref)
  let steps = fixture.adversarial_non_membership_steps(63)
  let prev_root = fixture.adversarial_non_membership_root(key, steps)
  let chunks = fixture.proof_chunk_reference_inputs(steps)
  let carriage = PublishedProofCarriage {
    ordered_chunk_reference_input_indices: [0, 1, 2, 3],
  }
  run_post_predecessor_published(out_ref, prev_root, carriage, chunks)
}

test q27_min_ada_utxo_rejects_reordered_published_predecessor_chunks() fail {
  let (out_ref, _) = maximum_descriptor_material()
  let key = ledger_outref_key(out_ref)
  let steps = fixture.adversarial_non_membership_steps(63)
  let prev_root = fixture.adversarial_non_membership_root(key, steps)
  let chunks = fixture.proof_chunk_reference_inputs(steps)
  let carriage = PublishedProofCarriage {
    ordered_chunk_reference_input_indices: [0, 2, 1, 3],
  }
  run_post_predecessor_published(out_ref, prev_root, carriage, chunks)
}

test q27_min_ada_utxo_rejects_a_forged_published_predecessor_root() fail {
  let (out_ref, _) = maximum_descriptor_material()
  let key = ledger_outref_key(out_ref)
  let steps = fixture.adversarial_non_membership_steps(63)
  let chunks = fixture.proof_chunk_reference_inputs(steps)
  let carriage = PublishedProofCarriage {
    ordered_chunk_reference_input_indices: [0, 1, 2, 3],
  }
  run_post_predecessor_published(
    out_ref,
    blake2b_256(#"feedface"),
    carriage,
    chunks,
  )
}

test q27_min_ada_utxo_rejects_more_than_four_published_chunks() fail {
  let (out_ref, _) = maximum_descriptor_material()
  let key = ledger_outref_key(out_ref)
  let steps = fixture.adversarial_non_membership_steps(63)
  let prev_root = fixture.adversarial_non_membership_root(key, steps)
  expect [first, ..] = fixture.proof_chunk_reference_inputs(steps)
  let chunks = [first, first, first, first, first]
  let carriage = PublishedProofCarriage {
    ordered_chunk_reference_input_indices: [0, 1, 2, 3, 4],
  }
  run_post_predecessor_published(out_ref, prev_root, carriage, chunks)
}

test q27_min_ada_tx_cannot_continue_to_the_utxo_predecessor_step() fail {
  let output = output_at_delta(-1)
  run_tx_step_02_with_successors(
    output,
    encode_midgard_tx_output(output),
    fixture.step_script_hash,
    fixture.next_step_script_hash,
  )
}

test q27_min_ada_utxo_cannot_skip_the_predecessor_step() fail {
  let (_, descriptor, _) = descriptor_material(-1)
  run_post_predicate_at(descriptor, fixture.step_script_hash)
}

test q27_min_ada_predecessor_step_rejects_a_wrong_terminal_successor() fail {
  let (out_ref, _, _) = descriptor_material(-1)
  run_post_predecessor_with_claim_at(
    env.empty_merkle_tree_root,
    env.empty_merkle_tree_root,
    ledger_outref_key(out_ref),
    [],
    fixture.step_script_hash,
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

test q27_min_ada_step_05_finalizes_only_an_adjudicated_thread() {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  step_05_validator.main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(PredicateAndCulpabilityAuthenticated),
      },
    ),
    ct.Continue(
      Step05Args {
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
  let step_05_cancel_datum: Option<Step05Datum> = Some(
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
      fixture.step_script_hash,
      fixture.computation_thread_policy_id,
      step_03_cancel_datum,
      ct.Cancel { input_index: 0, computation_thread_mint_redeemer_index: 0 },
      fixture.own_out_ref(),
      fixture.cancel_step_l1_tx_v1(asset_name, asset_name, True),
    ),
    step_04_validator.main.spend(
      fixture.next_step_script_hash,
      fixture.computation_thread_policy_id,
      step_04_cancel_datum,
      ct.Cancel { input_index: 0, computation_thread_mint_redeemer_index: 0 },
      fixture.own_out_ref(),
      fixture.cancel_step_l1_tx_v1(asset_name, asset_name, True),
    ),
    step_05_validator.main.spend(
      fixture.fraud_proof_policy_id,
      fixture.fraud_proof_token_address(),
      fixture.computation_thread_policy_id,
      step_05_cancel_datum,
      ct.Cancel { input_index: 0, computation_thread_mint_redeemer_index: 0 },
      fixture.own_out_ref(),
      fixture.cancel_step_l1_tx_v1(asset_name, asset_name, True),
    ),
  }
}
