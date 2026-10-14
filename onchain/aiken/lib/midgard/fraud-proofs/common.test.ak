//// Shared computation-thread state-machine safety selectors (GOAL_SPEC.md
//// Q01). These drive the shared `continue` / `finalize` / `cancel` /
//// `pass_native_tx_to_next_step` machinery directly with the native-V1 block
//// fixture, proving lifecycle, token-coupling, reference-input-discipline,
//// and valid-block-rejection properties independently of any single family.

use aiken/cbor
use aiken/collection/list
use cardano/address.{Address, VerificationKey}
use cardano/assets
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction,
}
use env
use midgard/computation_thread as ct
use midgard/fraud_proof
use midgard/fraud_proofs/common
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/compact.{native_tx_compact_view}
use midgard/fraud_proofs/native_tx/types.{
  MidgardTransaction, MidgardTransactionBody,
}
use midgard/ledger_state

const other_policy = #"99999999999999999999999999999999999999999999999999999999"

const forged_root =
  #"deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef"

fn valid_block() -> fixture.NativeBlockFixtureV1 {
  fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
}

/// A second, distinct native-V1 transaction (different fee, hence different
/// tx id and block header).
fn other_block() -> fixture.NativeBlockFixtureV1 {
  let base = fixture.valid_native_tx_v1()
  fixture.native_block_fixture_v1(
    MidgardTransaction {
      ..base,
      body: MidgardTransactionBody { ..base.body, fee: 2_000_000 },
    },
  )
}

fn sample_state() -> Data {
  let state_data: Data = "shared-step-state"
  state_data
}

fn run_continue(l1_tx: Transaction) -> Bool {
  let
      _own_script_hash,
      _asset_name,
      _fraud_prover,
      _m_input_state_data,
      _output_script_hash,
      output_state_data
    <- common.continue(
    fixture.computation_thread_policy_id,
    fixture.step_datum_without_state(),
    0,
    0,
    fixture.own_out_ref(),
    l1_tx.inputs,
    l1_tx.outputs,
  )
  output_state_data == sample_state()
}

// ## 1. Lifecycle: continue

/// Lifecycle positive: an exact thread reproduction (same token, script
/// destination, preserved prover, well-formed step state) continues.
test common_continue_accepts_exact_thread_reproduction() {
  let asset_name = fixture.ct_token_asset_name(valid_block())
  let
      own_script_hash,
      thread_asset_name,
      fraud_prover,
      m_input_state_data,
      output_script_hash,
      output_state_data
    <- common.continue(
    fixture.computation_thread_policy_id,
    fixture.step_datum_without_state(),
    0,
    0,
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(asset_name, sample_state()).inputs,
    fixture.continue_mid_step_l1_tx_v1(asset_name, sample_state()).outputs,
  )
  expect own_script_hash == fixture.step_script_hash
  expect thread_asset_name == asset_name
  expect fraud_prover == fixture.fraud_prover_key_hash
  expect None = m_input_state_data
  expect output_script_hash == fixture.next_step_script_hash
  output_state_data == sample_state()
}

/// Lifecycle/no-skip negative: a mid-step transition cannot emit the terminal
/// fraud-proof marker in place of the thread token.
test common_continue_rejects_fraud_proof_marker_output() fail {
  let asset_name = fixture.ct_token_asset_name(valid_block())
  let marker_value = assets.from_lovelace(2_000_000)
    |> assets.add(fixture.fraud_proof_policy_id, asset_name, 1)
  run_continue(
    Transaction {
      ..fixture.continue_mid_step_l1_tx_v1(asset_name, sample_state()),
      outputs: [fixture.step_output_with_value(marker_value, sample_state())],
    },
  )
}

/// Lifecycle negative: the fraud prover recorded at init cannot be swapped
/// mid-thread.
test common_continue_rejects_prover_swap() fail {
  let asset_name = fixture.ct_token_asset_name(valid_block())
  let swapped_datum: ct.StepDatum<Data> = ct.StepDatum {
    fraud_prover: other_policy,
    data: Some(sample_state()),
  }
  let swapped_datum_data: Data = swapped_datum
  run_continue(
    Transaction {
      ..fixture.continue_mid_step_l1_tx_v1(asset_name, sample_state()),
      outputs: [
        Output {
          address: address.from_script(fixture.next_step_script_hash),
          value: fixture.ct_token_value(asset_name),
          datum: InlineDatum(swapped_datum_data),
          reference_script: None,
        },
      ],
    },
  )
}

/// Public valid evidence does not authorize taking the thread's ADA.
test common_continue_rejects_redirected_thread_ada() fail {
  let asset_name = fixture.ct_token_asset_name(valid_block())
  let funded = fixture.ct_token_value(asset_name)
    |> assets.add(#"", #"", 5_000_000)
  let tx = fixture.continue_mid_step_l1_tx_v1(asset_name, sample_state())
  run_continue(
    Transaction { ..tx, inputs: [fixture.step_input_with_value(funded)] },
  )
}

/// A separate wallet may fund a larger datum's ledger minimum.
test common_continue_accepts_separately_funded_topup() {
  let asset_name = fixture.ct_token_asset_name(valid_block())
  let funded = fixture.ct_token_value(asset_name)
    |> assets.add(#"", #"", 5_000_000)
  let tx = fixture.continue_mid_step_l1_tx_v1(asset_name, sample_state())
  run_continue(
    Transaction {
      ..tx,
      outputs: [fixture.step_output_with_value(funded, sample_state())],
    },
  )
}

// ## 3. Token coupling: state and token are inseparable

/// Token-coupling negative: a thread-state UTxO without its thread token is
/// unusable.
test common_continue_rejects_stateless_thread_utxo() fail {
  let asset_name = fixture.ct_token_asset_name(valid_block())
  run_continue(
    Transaction {
      ..fixture.continue_mid_step_l1_tx_v1(asset_name, sample_state()),
      inputs: [fixture.step_input_with_value(assets.from_lovelace(2_000_000))],
    },
  )
}

/// Token-coupling negative: a token under a foreign policy cannot animate a
/// thread step.
test common_continue_rejects_foreign_policy_token() fail {
  let asset_name = fixture.ct_token_asset_name(valid_block())
  let foreign_value = assets.from_lovelace(2_000_000)
    |> assets.add(other_policy, asset_name, 1)
  run_continue(
    Transaction {
      ..fixture.continue_mid_step_l1_tx_v1(asset_name, sample_state()),
      inputs: [fixture.step_input_with_value(foreign_value)],
    },
  )
}

/// Token-coupling negative: the thread token cannot be dropped on the way to
/// the next step.
test common_continue_rejects_thread_token_drop() fail {
  let asset_name = fixture.ct_token_asset_name(valid_block())
  run_continue(
    Transaction {
      ..fixture.continue_mid_step_l1_tx_v1(asset_name, sample_state()),
      outputs: [
        fixture.step_output_with_value(
          assets.from_lovelace(2_000_000),
          sample_state(),
        ),
      ],
    },
  )
}

/// Token-coupling negative: smuggling any additional token next to the thread
/// token rejects (double-satisfaction guard).
test common_continue_rejects_token_split() fail {
  let asset_name = fixture.ct_token_asset_name(valid_block())
  let padded_value = fixture.ct_token_value(asset_name)
    |> assets.add(other_policy, "extra", 1)
  run_continue(
    Transaction {
      ..fixture.continue_mid_step_l1_tx_v1(asset_name, sample_state()),
      outputs: [fixture.step_output_with_value(padded_value, sample_state())],
    },
  )
}

// ## 1. Lifecycle: finalize

fn run_finalize(l1_tx: Transaction) -> Bool {
  let terminal_datum: ct.StepDatum<Data> = ct.StepDatum {
    fraud_prover: fixture.fraud_prover_key_hash,
    data: Some(sample_state()),
  }
  let _own_script_hash, _asset_name, _fraud_prover, m_input_state_data <- common.finalize(
    fixture.computation_thread_policy_id,
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    terminal_datum,
    0,
    0,
    0,
    fixture.own_out_ref(),
    l1_tx.inputs,
    l1_tx.outputs,
    l1_tx.redeemers,
  )
  expect Some(state) = m_input_state_data
  state == sample_state()
}

/// Lifecycle positive: a terminal step finalizes into the immutable
/// fraud-proof record (fraud-proof token + bare prover datum) with the
/// fraud-proof mint invoked.
test common_finalize_accepts_terminal_fraud_proof_record() {
  let asset_name = fixture.ct_token_asset_name(valid_block())
  run_finalize(fixture.finalize_step_l1_tx_v1(asset_name))
}

/// Finalization must not move the retained thread funding to an attacker.
test common_finalize_rejects_redirected_thread_ada() fail {
  let asset_name = fixture.ct_token_asset_name(valid_block())
  let funded = fixture.ct_token_value(asset_name)
    |> assets.add(#"", #"", 5_000_000)
  let tx = fixture.finalize_step_l1_tx_v1(asset_name)
  run_finalize(
    Transaction { ..tx, inputs: [fixture.step_input_with_value(funded)] },
  )
}

/// Lifecycle negative: finalization without invoking the fraud-proof minting
/// policy (which alone couples the thread-token burn) rejects.
test common_finalize_rejects_missing_fraud_proof_mint() fail {
  let asset_name = fixture.ct_token_asset_name(valid_block())
  run_finalize(
    Transaction { ..fixture.finalize_step_l1_tx_v1(asset_name), redeemers: [] },
  )
}

/// Lifecycle negative: the terminal record must be exactly the bare
/// fraud-proof datum; step state cannot be smuggled past finalization.
test common_finalize_rejects_step_state_smuggling() fail {
  let asset_name = fixture.ct_token_asset_name(valid_block())
  let smuggled_datum: ct.StepDatum<Data> = ct.StepDatum {
    fraud_prover: fixture.fraud_prover_key_hash,
    data: Some(sample_state()),
  }
  let smuggled_datum_data: Data = smuggled_datum
  let record_value = assets.from_lovelace(2_000_000)
    |> assets.add(fixture.fraud_proof_policy_id, asset_name, 1)
  run_finalize(
    Transaction {
      ..fixture.finalize_step_l1_tx_v1(asset_name),
      outputs: [
        Output {
          address: fixture.fraud_proof_token_address(),
          value: record_value,
          datum: InlineDatum(smuggled_datum_data),
          reference_script: None,
        },
      ],
    },
  )
}

/// Token-coupling negative: the terminal record must carry the fraud-proof
/// token, not the (to-be-burned) thread token.
test common_finalize_rejects_thread_marker_output() fail {
  let asset_name = fixture.ct_token_asset_name(valid_block())
  let fraud_proof_datum: Data = fraud_proof.Datum {
    fraud_prover: fixture.fraud_prover_key_hash,
  }
  run_finalize(
    Transaction {
      ..fixture.finalize_step_l1_tx_v1(asset_name),
      outputs: [
        Output {
          address: fixture.fraud_proof_token_address(),
          value: fixture.ct_token_value(asset_name),
          datum: InlineDatum(fraud_proof_datum),
          reference_script: None,
        },
      ],
    },
  )
}

// ## 1. Lifecycle: cancel

/// Lifecycle positive: the fraud prover may cancel their thread, burning the
/// thread token.
test common_cancel_accepts_prover_signed_burn() {
  let asset_name = fixture.ct_token_asset_name(valid_block())
  common.cancel(
    fixture.computation_thread_policy_id,
    Some(fixture.step_datum_without_state()),
    0,
    fixture.own_out_ref(),
    0,
    fixture.cancel_step_l1_tx_v1(asset_name, asset_name, True),
  )
}

/// Lifecycle negative: nobody but the fraud prover can cancel a thread.
test common_cancel_rejects_unsigned_cancellation() fail {
  let asset_name = fixture.ct_token_asset_name(valid_block())
  common.cancel(
    fixture.computation_thread_policy_id,
    Some(fixture.step_datum_without_state()),
    0,
    fixture.own_out_ref(),
    0,
    fixture.cancel_step_l1_tx_v1(asset_name, asset_name, False),
  )
}

/// Token-coupling negative: the cancellation burn must name exactly the
/// thread token held by the spent step UTxO.
test common_cancel_rejects_foreign_token_burn() fail {
  let asset_name = fixture.ct_token_asset_name(valid_block())
  common.cancel(
    fixture.computation_thread_policy_id,
    Some(fixture.step_datum_without_state()),
    0,
    fixture.own_out_ref(),
    0,
    fixture.cancel_step_l1_tx_v1(
      asset_name,
      fixture.ct_token_asset_name(other_block()),
      True,
    ),
  )
}

// ## 5/6. Shared native binding: reference-input discipline and
// valid-block rejection

fn run_native_binding(
  block: fixture.NativeBlockFixtureV1,
  args: common.NativeTxInclusionArgs,
  l1_tx: Transaction,
) -> Bool {
  let
      _own_script_hash,
      _asset_name,
      _fraud_prover,
      _m_input_state_data,
      _output_script_hash,
      output_state_data,
      header,
      native_tx_id,
      native_tx_view
    <- common.pass_native_tx_to_next_step(
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(fixture.step_datum_without_state()),
    args,
    fixture.own_out_ref(),
    l1_tx,
  )
  expect header == block.header
  expect native_tx_id == block.tx_id
  expect native_tx_view.tx_compact == native_tx_compact_view(block.compact)
  output_state_data == sample_state()
}

/// Valid-block positive: only data genuinely committed under the header's
/// counted `transactions_root` flows through the shared binding.
test common_native_binding_accepts_committed_transaction() {
  let block = valid_block()
  run_native_binding(
    block,
    fixture.native_inclusion_args_v1(block),
    fixture.continue_step_l1_tx_v1(
      block,
      sample_state(),
      block.transactions_phas_root,
      block.tx_id,
      block.source_cbor,
    ),
  )
}

/// Valid-block rejection (shared layer): a raw transactions root that the
/// valid header's counted root does not commit cannot enter the machinery.
test common_native_binding_rejects_forged_raw_root() fail {
  let block = valid_block()
  let args = common.NativeTxInclusionArgs {
    ..fixture.native_inclusion_args_v1(block),
    transactions_phas_root: forged_root,
  }
  run_native_binding(
    block,
    args,
    fixture.continue_step_l1_tx_v1(
      block,
      sample_state(),
      forged_root,
      block.tx_id,
      block.source_cbor,
    ),
  )
}

/// Valid-block rejection (shared layer): a transaction id that is not the
/// canonical id of the presented native CBOR cannot bind.
test common_native_binding_rejects_mismatched_tx_id() fail {
  let block = valid_block()
  let foreign_id = other_block().tx_id
  let args = common.NativeTxInclusionArgs {
    ..fixture.native_inclusion_args_v1(block),
    native_tx_id: foreign_id,
  }
  run_native_binding(
    block,
    args,
    fixture.continue_step_l1_tx_v1(
      block,
      sample_state(),
      block.transactions_phas_root,
      foreign_id,
      block.source_cbor,
    ),
  )
}

/// Reference-input discipline: the thread token is bound to one block; a
/// state-queue node for a different block rejects.
test common_native_binding_rejects_foreign_block_thread() fail {
  let block = valid_block()
  let foreign = other_block()
  let base_tx = fixture.continue_step_l1_tx_v1(
    block,
    sample_state(),
    block.transactions_phas_root,
    block.tx_id,
    block.source_cbor,
  )
  run_native_binding(
    block,
    fixture.native_inclusion_args_v1(block),
    Transaction {
      ..base_tx,
      reference_inputs: [
        fixture.hub_reference_input(),
        fixture.state_queue_reference_input(foreign),
      ],
    },
  )
}

/// Reference-input discipline: a state-queue node authenticated by a foreign
/// policy (not the hub-registered state-queue policy) rejects.
test common_native_binding_rejects_substituted_state_queue_node() fail {
  let block = valid_block()
  let base_tx = fixture.continue_step_l1_tx_v1(
    block,
    sample_state(),
    block.transactions_phas_root,
    block.tx_id,
    block.source_cbor,
  )
  run_native_binding(
    block,
    fixture.native_inclusion_args_v1(block),
    Transaction {
      ..base_tx,
      reference_inputs: [
        fixture.hub_reference_input(),
        fixture.state_queue_reference_input_with_policy(block, other_policy),
      ],
    },
  )
}

/// Reference-input discipline: a hub oracle whose authenticating NFT is not
/// the hub policy's rejects.
test common_native_binding_rejects_substituted_hub_oracle() fail {
  let block = valid_block()
  let base_tx = fixture.continue_step_l1_tx_v1(
    block,
    sample_state(),
    block.transactions_phas_root,
    block.tx_id,
    block.source_cbor,
  )
  run_native_binding(
    block,
    fixture.native_inclusion_args_v1(block),
    Transaction {
      ..base_tx,
      reference_inputs: [
        fixture.hub_reference_input_with_nft_policy(other_policy),
        fixture.state_queue_reference_input(block),
      ],
    },
  )
}

fn decoded_source(
  block: fixture.NativeBlockFixtureV1,
) -> ledger_state.L2TransactionSourceV1 {
  expect Some(source_data) = cbor.deserialise(block.source_cbor)
  expect source: ledger_state.L2TransactionSourceV1 = source_data
  source
}

fn run_committed_source(source_cbor: ByteArray) -> Bool {
  let base = valid_block()
  let block = fixture.committed_leaf_block_fixture_v1(
    base,
    base.tx_id,
    source_cbor,
  )
  run_native_binding(
    block,
    fixture.native_inclusion_args_v1(block),
    fixture.continue_step_l1_tx_v1(
      block,
      sample_state(),
      block.transactions_phas_root,
      block.tx_id,
      block.source_cbor,
    ),
  )
}

test common_native_binding_rejects_a_source_value_under_a_foreign_key() fail {
  let base = valid_block()
  let block = fixture.committed_leaf_block_fixture_v1(
    base,
    other_block().tx_id,
    base.source_cbor,
  )
  run_native_binding(
    block,
    fixture.native_inclusion_args_v1(block),
    fixture.continue_step_l1_tx_v1(
      block,
      sample_state(),
      block.transactions_phas_root,
      block.tx_id,
      block.source_cbor,
    ),
  )
}

test common_native_binding_rejects_forged_source_witness_bytes() fail {
  let block = valid_block()
  let source = decoded_source(block)
  run_committed_source(
    cbor.serialise(
      ledger_state.L2TransactionSourceV1 {
        ..source,
        source: ledger_state.NativeTxProofSourceV1 {
          ..source.source,
          witness_set_compact_cbor: #"80",
        },
      },
    ),
  )
}

test common_native_binding_rejects_forged_source_field_lengths() fail {
  let block = valid_block()
  let source = decoded_source(block)
  run_committed_source(
    cbor.serialise(
      ledger_state.L2TransactionSourceV1 {
        ..source,
        source: ledger_state.NativeTxProofSourceV1 {
          ..source.source,
          field_preimage_lengths_cbor: #"80",
        },
      },
    ),
  )
}

test common_native_binding_rejects_malformed_source_data() fail {
  run_committed_source(#"ff")
}

// ## 6. Witness-set binding at the shared layer
//
// Retired by #575. The two selectors that lived here exercised
// `common.verify_native_tx_witness_set`, which is gone: the derivation it ran is
// now a precondition inside `authenticated_field_view`, against a
// `witness_set_hash` the *thread* anchored rather than one the redeemer's own
// compact bytes carried. The equivalent assertions live where the guard does —
// `midgard/fraud_proofs/field_opening_v1.test`'s
// `forged_witness_set_and_compact_is_refused_under_the_anchor` and the
// per-family both-sides forgery selectors — and they are strictly stronger,
// because the helper here could only ever catch a witness set forged on one
// side.

// ## Validation-trace value rule (pd-design 3.5, 6.5)
//
// `continue_validation_trace` lets a lone thread input pay its own fee, up to
// `env.max_dispute_move_fee_lovelace`; `finalize_validation_trace` bounds the
// fraud-proof output and returns the rest to the prover. Each negative below
// breaks exactly one conjunct of a transaction the matching positive accepts.

const thread_lovelace = 100_000_000

fn thread_value(asset_name: ByteArray, lovelace: Int) {
  assets.from_lovelace(lovelace)
    |> assets.add(fixture.computation_thread_policy_id, asset_name, 1)
}

/// A single-input move whose thread pays `paid` lovelace of a `fee` fee.
fn fee_paying_move(paid: Int, fee: Int) -> Transaction {
  let asset_name = fixture.ct_token_asset_name(valid_block())
  let tx = fixture.continue_mid_step_l1_tx_v1(asset_name, sample_state())
  Transaction {
    ..tx,
    inputs: [
      fixture.step_input_with_value(thread_value(asset_name, thread_lovelace)),
    ],
    outputs: [
      fixture.step_output_with_value(
        thread_value(asset_name, thread_lovelace - paid),
        sample_state(),
      ),
    ],
    fee,
  }
}

fn wallet_input() -> Input {
  Input {
    output_reference: OutputReference {
      transaction_id: #"0000000000000000000000000000000000000000000000000000000000000077",
      output_index: 3,
    },
    output: Output {
      address: address.from_verification_key(fixture.fraud_prover_key_hash),
      value: assets.from_lovelace(10_000_000),
      datum: transaction.NoDatum,
      reference_script: None,
    },
  }
}

fn with_wallet_input(tx: Transaction) -> Transaction {
  Transaction { ..tx, inputs: list.concat(tx.inputs, [wallet_input()]) }
}

fn signed_by_prover(tx: Transaction) -> Transaction {
  Transaction { ..tx, extra_signatories: [fixture.fraud_prover_key_hash] }
}

fn run_trace_continue(l1_tx: Transaction, mover: common.Mover) -> Bool {
  let
      _own_script_hash,
      _asset_name,
      _fraud_prover,
      _m_input_state_data,
      _output_script_hash,
      output_state_data
    <- common.continue_validation_trace(
    fixture.computation_thread_policy_id,
    fixture.step_datum_without_state(),
    0,
    0,
    fixture.own_out_ref(),
    l1_tx,
    mover,
  )
  output_state_data == sample_state()
}

test trace_continue_accepts_single_input_fee_within_cap() {
  run_trace_continue(
    fee_paying_move(
      env.max_dispute_move_fee_lovelace,
      env.max_dispute_move_fee_lovelace,
    ),
    common.Anyone,
  )
}

test trace_continue_refuses_more_than_the_fee() fail {
  run_trace_continue(
    fee_paying_move(
      env.max_dispute_move_fee_lovelace,
      env.max_dispute_move_fee_lovelace - 1,
    ),
    common.Anyone,
  )
}

test trace_continue_refuses_more_than_the_cap() fail {
  run_trace_continue(
    fee_paying_move(
      env.max_dispute_move_fee_lovelace + 1,
      env.max_dispute_move_fee_lovelace + 1,
    ),
    common.Anyone,
  )
}

test trace_continue_accepts_two_input_move_preserving_the_thread() {
  run_trace_continue(
    with_wallet_input(fee_paying_move(0, 300_000)),
    common.Anyone,
  )
}

test trace_continue_refuses_two_input_skim() fail {
  run_trace_continue(
    with_wallet_input(fee_paying_move(1, 300_000)),
    common.Anyone,
  )
}

/// The plain `continue` keeps `out >= in`: the same fee-paying move fails it.
test plain_continue_refuses_the_fee_paying_move() fail {
  run_continue(fee_paying_move(1, 300_000))
}

test trace_continue_accepts_prover_signed_prover_only_move() {
  run_trace_continue(
    signed_by_prover(fee_paying_move(200_000, 300_000)),
    common.ProverOnly,
  )
}

test trace_continue_refuses_unsigned_prover_only_move() fail {
  run_trace_continue(fee_paying_move(200_000, 300_000), common.ProverOnly)
}

test trace_continue_refuses_prover_only_move_signed_by_another_key() fail {
  run_trace_continue(
    Transaction {
      ..fee_paying_move(200_000, 300_000),
      extra_signatories: [other_policy],
    },
    common.ProverOnly,
  )
}

// ### Finalize

fn prover_remainder(lovelace: Int, tag: OutputReference) -> Output {
  let tag_data: Data = tag
  Output {
    address: Address {
      payment_credential: VerificationKey(fixture.fraud_prover_key_hash),
      stake_credential: None,
    },
    value: assets.from_lovelace(lovelace),
    datum: InlineDatum(tag_data),
    reference_script: None,
  }
}

/// A single-input finalize: the fraud-proof output holds `fp`, the thread pays
/// `paid` of a `fee` fee, and the remainder output carries the rest.
fn trace_finalize_tx(fp: Int, paid: Int, fee: Int) -> Transaction {
  let asset_name = fixture.ct_token_asset_name(valid_block())
  let tx = fixture.finalize_step_l1_tx_v1(asset_name)
  expect [record] = tx.outputs
  Transaction {
    ..tx,
    inputs: [
      fixture.step_input_with_value(thread_value(asset_name, thread_lovelace)),
    ],
    outputs: [
      Output {
        ..record,
        value: assets.from_lovelace(fp)
          |> assets.add(fixture.fraud_proof_policy_id, asset_name, 1),
      },
      prover_remainder(thread_lovelace - fp - paid, fixture.own_out_ref()),
    ],
    fee,
  }
}

fn with_outputs(tx: Transaction, outputs: List<Output>) -> Transaction {
  Transaction { ..tx, outputs }
}

fn run_trace_finalize(l1_tx: Transaction) -> Bool {
  run_trace_finalize_at(l1_tx, 0, 0, fixture.own_out_ref())
}

fn run_trace_finalize_at(
  l1_tx: Transaction,
  input_index: Int,
  output_index: Int,
  own_out_ref: OutputReference,
) -> Bool {
  let terminal_datum: ct.StepDatum<Data> = ct.StepDatum {
    fraud_prover: fixture.fraud_prover_key_hash,
    data: Some(sample_state()),
  }
  let _own_script_hash, _asset_name, _fraud_prover, m_input_state_data <- common.finalize_validation_trace(
    fixture.computation_thread_policy_id,
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    terminal_datum,
    input_index,
    output_index,
    0,
    own_out_ref,
    l1_tx,
  )
  expect Some(state) = m_input_state_data
  state == sample_state()
}

test trace_finalize_accepts_lower_bound_with_remainder() {
  run_trace_finalize(
    trace_finalize_tx(
      env.fraud_proof_output_lovelace,
      env.max_dispute_move_fee_lovelace,
      env.max_dispute_move_fee_lovelace,
    ),
  )
}

test trace_finalize_accepts_upper_bound_with_remainder() {
  run_trace_finalize(
    trace_finalize_tx(
      env.fraud_proof_output_lovelace + env.max_dispute_move_fee_lovelace,
      300_000,
      300_000,
    ),
  )
}

test trace_finalize_refuses_fraud_proof_output_below_range() fail {
  run_trace_finalize(
    trace_finalize_tx(env.fraud_proof_output_lovelace - 1, 300_000, 300_000),
  )
}

test trace_finalize_refuses_fraud_proof_output_above_range() fail {
  run_trace_finalize(
    trace_finalize_tx(
      env.fraud_proof_output_lovelace + env.max_dispute_move_fee_lovelace + 1,
      300_000,
      300_000,
    ),
  )
}

test trace_finalize_refuses_remainder_short_of_the_fee() fail {
  run_trace_finalize(
    trace_finalize_tx(env.fraud_proof_output_lovelace, 300_001, 300_000),
  )
}

test trace_finalize_refuses_remainder_short_of_the_cap() fail {
  run_trace_finalize(
    trace_finalize_tx(
      env.fraud_proof_output_lovelace,
      env.max_dispute_move_fee_lovelace + 1,
      env.max_dispute_move_fee_lovelace + 1,
    ),
  )
}

test trace_finalize_refuses_missing_remainder() fail {
  let tx = trace_finalize_tx(env.fraud_proof_output_lovelace, 300_000, 300_000)
  expect [record, _] = tx.outputs
  run_trace_finalize(with_outputs(tx, [record]))
}

test trace_finalize_refuses_remainder_with_another_tag() fail {
  let tx = trace_finalize_tx(env.fraud_proof_output_lovelace, 300_000, 300_000)
  expect [record, remainder] = tx.outputs
  let other_tag = OutputReference { ..fixture.own_out_ref(), output_index: 1 }
  run_trace_finalize(
    with_outputs(
      tx,
      [record, prover_remainder(assets.lovelace_of(remainder.value), other_tag)],
    ),
  )
}

test trace_finalize_refuses_remainder_to_another_key() fail {
  let tx = trace_finalize_tx(env.fraud_proof_output_lovelace, 300_000, 300_000)
  expect [record, remainder] = tx.outputs
  run_trace_finalize(
    with_outputs(
      tx,
      [
        record,
        Output {
          ..remainder,
          address: address.from_verification_key(other_policy),
        },
      ],
    ),
  )
}

test trace_finalize_refuses_remainder_with_a_stake_credential() fail {
  let tx = trace_finalize_tx(env.fraud_proof_output_lovelace, 300_000, 300_000)
  expect [record, remainder] = tx.outputs
  run_trace_finalize(
    with_outputs(
      tx,
      [
        record,
        Output {
          ..remainder,
          address: address.from_verification_key(fixture.fraud_prover_key_hash)
            |> address.with_delegation_key(other_policy),
        },
      ],
    ),
  )
}

test trace_finalize_accepts_two_input_remainder_of_the_whole_rest() {
  run_trace_finalize(
    with_wallet_input(
      trace_finalize_tx(env.fraud_proof_output_lovelace, 0, 300_000),
    ),
  )
}

test trace_finalize_refuses_two_input_remainder_that_pays_the_fee() fail {
  run_trace_finalize(
    with_wallet_input(
      trace_finalize_tx(env.fraud_proof_output_lovelace, 1, 300_000),
    ),
  )
}

// ### Two threads finalizing in one transaction
//
// Two threads on one header carry the same thread asset name. Each thread's
// remainder is told apart only by its `InlineDatum(own_out_ref)` tag, so one
// remainder output cannot serve both.

fn second_thread_out_ref() -> OutputReference {
  OutputReference { ..fixture.own_out_ref(), output_index: 1 }
}

/// Thread A at input 0 and thread B at input 1, fraud-proof outputs 0 and 1,
/// followed by `remainders`.
fn two_thread_finalize_tx(remainders: List<Output>) -> Transaction {
  let tx = trace_finalize_tx(env.fraud_proof_output_lovelace, 300_000, 300_000)
  expect [thread_a] = tx.inputs
  expect [record, _] = tx.outputs
  let thread_b = Input { ..thread_a, output_reference: second_thread_out_ref() }
  Transaction {
    ..tx,
    inputs: [thread_a, thread_b],
    outputs: list.concat([record, record], remainders),
  }
}

fn whole_rest() -> Int {
  thread_lovelace - env.fraud_proof_output_lovelace
}

test trace_finalize_accepts_two_threads_with_their_own_remainders() {
  let tx = two_thread_finalize_tx(
    [
      prover_remainder(whole_rest(), fixture.own_out_ref()),
      prover_remainder(whole_rest(), second_thread_out_ref()),
    ],
  )
  and {
    run_trace_finalize_at(tx, 0, 0, fixture.own_out_ref()),
    run_trace_finalize_at(tx, 1, 1, second_thread_out_ref()),
  }
}

/// One remainder holding both threads' rest, tagged for thread A, does not
/// return thread B's.
test trace_finalize_refuses_a_remainder_shared_between_threads() fail {
  let tx = two_thread_finalize_tx(
    [prover_remainder(2 * whole_rest(), fixture.own_out_ref())],
  )
  run_trace_finalize_at(tx, 1, 1, second_thread_out_ref())
}
