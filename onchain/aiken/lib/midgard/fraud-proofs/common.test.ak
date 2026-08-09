//// Shared computation-thread state-machine safety selectors (GOAL_SPEC.md
//// Q01). These drive the shared `continue` / `finalize` / `cancel` /
//// `pass_native_tx_to_next_step` machinery directly with the native-V1 block
//// fixture, proving lifecycle, token-coupling, reference-input-discipline,
//// and valid-block-rejection properties independently of any single family.

use cardano/address
use cardano/assets
use cardano/transaction.{InlineDatum, Output, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proof
use midgard/fraud_proofs/common
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/types.{
  MidgardTransaction, MidgardTransactionBody,
}

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
  expect native_tx_view.tx_compact == block.compact
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
      block.compact_cbor,
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
      block.compact_cbor,
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
      block.compact_cbor,
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
    block.compact_cbor,
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
    block.compact_cbor,
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
    block.compact_cbor,
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
