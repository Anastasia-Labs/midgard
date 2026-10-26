//// Q35 network-id step 01: counted-root binding and acceptance gate.

use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{
  NativeTxInclusionCarriage, cancel, continue,
  pass_native_tx_to_next_step_carried,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/types.{
  MidgardTransaction, MidgardTransactionBody, TxIsInvalid,
}
use midgard/fraud_proofs/network_id/step_01.{
  Args, Datum, ForcedDispatchV1, ForcedNetworkIdMismatch, NetworkIdFaultV1,
  OutputNetwork, SpendRedeemer, TransactionNetwork,
}
use midgard/fraud_proofs/network_id/step_02 as step_02

validator main(
  step_02_validator_script_hash: ScriptHash,
  forced_step_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  hub_oracle: ScriptHash,
  expected_network_id: Int,
) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          computation_thread_token_policy_id,
          datum,
          input_index,
          own_out_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(Args { tx_inclusion, forced_source, fault }) ->
        when forced_source is {
          // A forced leaf of either verdict: the forced door authenticates the
          // leaf and pairs the claimed fault with the verdict it contradicts.
          Some(source) -> {
            expect None = tx_inclusion
            expect
              when fault is {
                OutputNetwork { output_index } -> output_index >= 0
                _ -> True
              }
            pass_forced_claim(
              forced_step_validator_script_hash,
              computation_thread_token_policy_id,
              datum,
              source,
              fault,
              own_out_ref,
              tx,
            )
          }
          None ->
            when fault is {
              TransactionNetwork -> {
                expect Some(inclusion) = tx_inclusion
                pass_transaction_claim(
                  step_02_validator_script_hash,
                  computation_thread_token_policy_id,
                  hub_oracle,
                  expected_network_id,
                  datum,
                  inclusion,
                  fault,
                  own_out_ref,
                  tx,
                )
              }
              OutputNetwork { output_index } -> {
                expect output_index >= 0
                expect Some(inclusion) = tx_inclusion
                pass_transaction_claim(
                  step_02_validator_script_hash,
                  computation_thread_token_policy_id,
                  hub_oracle,
                  expected_network_id,
                  datum,
                  inclusion,
                  fault,
                  own_out_ref,
                  tx,
                )
              }
              ForcedNetworkIdMismatch -> False
            }
        }
    }
  }

  else(_) {
    fail
  }
}

fn pass_forced_claim(
  forced_step_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  datum: Option<Datum>,
  source: ForcedDispatchV1,
  fault: NetworkIdFaultV1,
  own_out_ref: OutputReference,
  tx: Transaction,
) -> Bool {
  let Transaction { inputs, outputs, .. } = tx
  expect Some(step_datum) = datum
  let _, _, _, input_state, output_script_hash, output_state_data <- continue(
    computation_thread_token_policy_id,
    step_datum,
    source.input_index,
    source.output_index,
    own_out_ref,
    inputs,
    outputs,
    tx.extra_signatories,
  )
  // The shared `continue` helper only admits a populated successor state, so
  // the forced door receives the claimed fault and re-authenticates it.
  let handoff_state: Data = fault
  and {
    input_state == None,
    output_script_hash == forced_step_validator_script_hash,
    output_state_data == handoff_state,
  }
}

fn pass_transaction_claim(
  step_02_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  hub_oracle: ScriptHash,
  expected_network_id: Int,
  datum: Option<Datum>,
  tx_inclusion: NativeTxInclusionCarriage,
  fault: NetworkIdFaultV1,
  own_out_ref: OutputReference,
  tx: Transaction,
) -> Bool {
  let
      _own_script_hash,
      _computation_thread_token_asset_name,
      _fraud_prover,
      m_input_state_data,
      output_script_hash,
      output_state_data,
      _header,
      bad_tx_id,
      bad_tx_view
    <- pass_native_tx_to_next_step_carried(
    computation_thread_token_policy_id,
    hub_oracle,
    datum,
    tx_inclusion,
    own_out_ref,
    tx,
  )

  expect None = m_input_state_data
  // Network rules only affect an accepted transaction. A rejected leaf is an
  // honest no-op and must never produce a permanent proof token.
  expect bad_tx_view.tx_compact.validity_code == Some(0)
  expect output_script_hash == step_02_validator_script_hash
  let expected_output_state: Data = step_02.State {
    bad_tx_id,
    committed_tx_network_id: bad_tx_view.tx_compact.body.network_id,
    expected_network_id,
    fault,
    forced_source_key: None,
  }
  expect output_state_data == expected_output_state
  True
}

fn with_tx_network(network_id: Int) -> MidgardTransaction {
  let base = fixture.valid_native_tx_v1()
  MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, network_id },
  }
}

fn run_step_01(
  bad_tx: MidgardTransaction,
  expected_network_id: Int,
  fault: NetworkIdFaultV1,
) -> Bool {
  let block = fixture.native_block_fixture_v1(bad_tx)
  let expected_state: Data = step_02.State {
    bad_tx_id: block.tx_id,
    committed_tx_network_id: block.compact.body.network_id,
    expected_network_id,
    fault,
    forced_source_key: None,
  }
  main.spend(
    fixture.next_step_script_hash,
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    expected_network_id,
    Some(fixture.step_datum_without_state()),
    ct.Continue(
      Args {
        tx_inclusion: Some(fixture.native_inclusion_carriage_v1(block)),
        forced_source: None,
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

test network_id_step_01_binds_an_accepted_transaction_claim() {
  run_step_01(with_tx_network(1), 0, TransactionNetwork)
}

test network_id_step_01_binds_an_accepted_output_claim() {
  run_step_01(with_tx_network(0), 0, OutputNetwork { output_index: 0 })
}

test network_id_step_01_rejects_an_honestly_rejected_transaction() fail {
  let bad = with_tx_network(1)
  run_step_01(
    MidgardTransaction { ..bad, validity: TxIsInvalid },
    0,
    TransactionNetwork,
  )
}

test network_id_step_01_rejects_a_negative_output_index() fail {
  run_step_01(with_tx_network(0), 0, OutputNetwork { output_index: -1 })
}

// ## Forced dispatch arm
//
// `ForcedNetworkIdMismatch` is the only arm that hands the thread to the
// forced door instead of the terminal step, and it carries no evidence of its
// own: the whole arm is the routing decision plus the handoff marker the
// forced step re-authenticates.

fn forced_dispatch_asset_name() -> ByteArray {
  fixture.ct_token_asset_name(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
  )
}

fn run_forced_dispatch(
  forced_step_validator_script_hash: ScriptHash,
  input_state: Option<Data>,
  tx_inclusion: Option<NativeTxInclusionCarriage>,
  handoff_state: Data,
) -> Bool {
  run_forced_fault_dispatch(
    forced_step_validator_script_hash,
    input_state,
    tx_inclusion,
    ForcedNetworkIdMismatch,
    handoff_state,
  )
}

fn run_forced_fault_dispatch(
  forced_step_validator_script_hash: ScriptHash,
  input_state: Option<Data>,
  tx_inclusion: Option<NativeTxInclusionCarriage>,
  fault: NetworkIdFaultV1,
  handoff_state: Data,
) -> Bool {
  let asset_name = forced_dispatch_asset_name()
  main.spend(
    fixture.step_script_hash,
    forced_step_validator_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    0,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: input_state,
      },
    ),
    ct.Continue(
      Args {
        tx_inclusion,
        forced_source: Some(
          ForcedDispatchV1 { input_index: 0, output_index: 0 },
        ),
        fault,
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(asset_name, handoff_state),
  )
}

fn forced_handoff_state() -> Data {
  let state: Data = ForcedNetworkIdMismatch
  state
}

test network_id_step_01_dispatches_a_forced_claim() {
  run_forced_dispatch(
    fixture.next_step_script_hash,
    None,
    None,
    forced_handoff_state(),
  )
}

test network_id_step_01_rejects_a_populated_state_on_forced_dispatch() fail {
  let carried: Data = ForcedNetworkIdMismatch
  run_forced_dispatch(
    fixture.next_step_script_hash,
    Some(carried),
    None,
    forced_handoff_state(),
  )
}

test network_id_step_01_rejects_an_empty_forced_handoff_state() fail {
  let empty: Option<Int> = None
  let empty_state: Data = empty
  run_forced_dispatch(fixture.next_step_script_hash, None, None, empty_state)
}

test network_id_step_01_rejects_a_foreign_forced_handoff_state() fail {
  let foreign: Data = TransactionNetwork
  run_forced_dispatch(fixture.next_step_script_hash, None, None, foreign)
}

test network_id_step_01_rejects_a_wrong_successor_on_forced_dispatch() fail {
  run_forced_dispatch(
    fixture.step_script_hash,
    None,
    None,
    forced_handoff_state(),
  )
}

test network_id_step_01_rejects_tx_inclusion_on_forced_dispatch() fail {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  run_forced_dispatch(
    fixture.next_step_script_hash,
    None,
    Some(fixture.native_inclusion_carriage_v1(block)),
    forced_handoff_state(),
  )
}

fn run_accepted_forced_dispatch(
  fault: NetworkIdFaultV1,
  handoff: NetworkIdFaultV1,
) -> Bool {
  let handoff_state: Data = handoff
  run_forced_fault_dispatch(
    fixture.next_step_script_hash,
    None,
    None,
    fault,
    handoff_state,
  )
}

// An accepted forced transaction reaches the same body and output claims as
// an accepted normal one; the forced door pairs them with the leaf verdict.
test network_id_step_01_dispatches_an_accepted_forced_transaction_claim() {
  run_accepted_forced_dispatch(TransactionNetwork, TransactionNetwork)
}

test network_id_step_01_dispatches_an_accepted_forced_output_claim() {
  run_accepted_forced_dispatch(
    OutputNetwork { output_index: 2 },
    OutputNetwork { output_index: 2 },
  )
}

test network_id_step_01_rejects_a_negative_forced_output_index() fail {
  run_accepted_forced_dispatch(
    OutputNetwork { output_index: -1 },
    OutputNetwork { output_index: -1 },
  )
}

test network_id_step_01_rejects_a_forced_handoff_naming_another_output() fail {
  run_accepted_forced_dispatch(
    OutputNetwork { output_index: 1 },
    OutputNetwork { output_index: 2 },
  )
}

test network_id_step_01_rejects_tx_inclusion_on_accepted_forced_dispatch() fail {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let handoff_state: Data = TransactionNetwork
  run_forced_fault_dispatch(
    fixture.next_step_script_hash,
    None,
    Some(fixture.native_inclusion_carriage_v1(block)),
    TransactionNetwork,
    handoff_state,
  )
}
