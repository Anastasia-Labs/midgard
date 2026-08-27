//// Cross-block duplicate-event — step 02/final.
////
//// Authenticates the confirmed settlement node, opens the same event identity
//// in the same root domain under a different header, and finalizes the fraud
//// proof against the live challenged header.

use cardano/address.{Address}
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{Input, OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/cross_block_duplicate_event/step_01
use midgard/fraud_proofs/cross_block_duplicate_event/step_02.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/ledger_state

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
      ct.Continue(Args {
        input_index,
        output_index,
        fraud_proof_mint_redeemer_index,
        settlement_ref_input_index,
        settled_event,
      }) -> {
        let Transaction { inputs, outputs, reference_inputs, redeemers, .. } = tx
        expect Some(step_datum) = datum
        let
            _own_script_hash,
            computation_thread_token_asset_name,
            _fraud_prover,
            m_input_state_data
          <- finalize(
          computation_thread_token_policy_id,
          fraud_proof_token_policy_id,
          fraud_proof_token_address,
          step_datum,
          input_index,
          output_index,
          fraud_proof_mint_redeemer_index,
          own_out_ref,
          inputs,
          outputs,
          redeemers,
        )
        expect Some(state) = m_input_state_data
        expect
          step_01.fraud_category_is_cross_block_duplicate_event_v1(
            computation_thread_token_asset_name,
          )
        expect
          state.challenged_header_hash == step_01.challenged_header_hash_of_v1(
            computation_thread_token_asset_name,
          )
        expect _settled_header_hash = step_02.verify_confirmed_duplicate_v1(
          state,
          settlement_ref_input_index,
          settled_event,
          reference_inputs,
        )
        True
      }
    }
  }

  else(_) {
    fail
  }
}

fn state_for(block: step_01.DuplicateEventBlockV1) -> State {
  State {
    challenged_header_hash: block.block.header_hash,
    settlement_policy_id: step_02.fixture_settlement_policy_v1(),
    event_kind: step_01.committed_event_kind_v1(block.committed_event),
    event_key: step_01.committed_event_key_v1(block.committed_event),
  }
}

fn run_step_02_with_input(
  state: State,
  settled_event: step_01.CommittedDuplicateEventProofV1,
  settlement_input: Input,
  thread_asset_name: AssetName,
) -> Bool {
  main.spend(
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
      Args {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
        settlement_ref_input_index: 0,
        settled_event,
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_with_reference_inputs_v1(
      thread_asset_name,
      [settlement_input],
    ),
  )
}

fn run_step_02(
  challenged: step_01.DuplicateEventBlockV1,
  settled: step_01.DuplicateEventBlockV1,
  settled_header_hash: ledger_state.HeaderHash,
  settled_event: step_01.CommittedDuplicateEventProofV1,
) -> Bool {
  run_step_02_with_input(
    state_for(challenged),
    settled_event,
    step_02.settlement_reference_input_v1(
      settled,
      settled_header_hash,
      step_02.fixture_settlement_policy_v1(),
    ),
    step_01.block_thread_asset_name_v1(challenged),
  )
}

test cross_block_duplicate_event_step_02_convicts_confirmed_deposit_duplicate() {
  let challenged = step_01.duplicate_deposit_block_v1()
  let settled = step_01.duplicate_deposit_block_v1()
  run_step_02(
    challenged,
    settled,
    step_02.historical_header_hash_v1(settled),
    settled.committed_event,
  )
}

test cross_block_duplicate_event_step_02_convicts_confirmed_withdrawal_duplicate() {
  let challenged = step_01.duplicate_withdrawal_block_v1()
  let settled = step_01.duplicate_withdrawal_block_v1()
  run_step_02(
    challenged,
    settled,
    step_02.historical_header_hash_v1(settled),
    settled.committed_event,
  )
}

/// Honest/single-application case: confirmed history contains another event,
/// not the event the challenged block applies.
test cross_block_duplicate_event_step_02_rejects_honest_different_event() fail {
  let challenged = step_01.duplicate_deposit_block_v1()
  let settled = step_01.deposit_event_block_v1(
    step_01.foreign_event_id_v1(),
    step_01.duplicated_deposit_info_v1(),
  )
  run_step_02(
    challenged,
    settled,
    step_02.historical_header_hash_v1(settled),
    settled.committed_event,
  )
}

test cross_block_duplicate_event_step_02_rejects_cross_domain_event() fail {
  let challenged = step_01.duplicate_deposit_block_v1()
  let settled = step_01.duplicate_withdrawal_block_v1()
  run_step_02(
    challenged,
    settled,
    step_02.historical_header_hash_v1(settled),
    settled.committed_event,
  )
}

test cross_block_duplicate_event_step_02_rejects_same_header() fail {
  let challenged = step_01.duplicate_deposit_block_v1()
  run_step_02(
    challenged,
    challenged,
    challenged.block.header_hash,
    challenged.committed_event,
  )
}

/// A removed live ancestor never spawned a settlement NFT. A copy of its roots
/// under any other policy is unauthenticated and cannot stand in for history.
test cross_block_duplicate_event_step_02_rejects_removed_ancestor_copy() fail {
  let challenged = step_01.duplicate_withdrawal_block_v1()
  let settled = step_01.duplicate_withdrawal_block_v1()
  run_step_02_with_input(
    state_for(challenged),
    settled.committed_event,
    step_02.settlement_reference_input_v1(
      settled,
      step_02.historical_header_hash_v1(settled),
      #"74747474747474747474747474747474747474747474747474747474",
    ),
    step_01.block_thread_asset_name_v1(challenged),
  )
}

test cross_block_duplicate_event_step_02_rejects_forged_historical_root() fail {
  let challenged = step_01.duplicate_deposit_block_v1()
  let settled = step_01.duplicate_deposit_block_v1()
  let foreign = step_01.deposit_event_block_v1(
    step_01.foreign_event_id_v1(),
    step_01.duplicated_deposit_info_v1(),
  )
  run_step_02(
    challenged,
    settled,
    step_02.historical_header_hash_v1(settled),
    foreign.committed_event,
  )
}

fn run_cancel(signed_by_prover: Bool) -> Bool {
  let block = step_01.duplicate_deposit_block_v1()
  let state = state_for(block)
  let asset_name = step_01.block_thread_asset_name_v1(block)
  main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state),
      },
    ),
    ct.Cancel { input_index: 0, computation_thread_mint_redeemer_index: 0 },
    fixture.own_out_ref(),
    fixture.cancel_step_l1_tx_v1(asset_name, asset_name, signed_by_prover),
  )
}

test cross_block_duplicate_event_step_02_accepts_prover_cancellation() {
  run_cancel(True)
}

test cross_block_duplicate_event_step_02_rejects_unsigned_cancellation() fail {
  run_cancel(False)
}
