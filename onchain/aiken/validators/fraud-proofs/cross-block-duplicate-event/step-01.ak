//// Cross-block duplicate-event — step 01.
////
//// Opens the duplicated deposit/withdrawal leaf under the challenged live
//// header's canonical counted root and carries only authenticated facts to the
//// settlement-history step.

use aiken/builtin
use aiken/crypto.{ScriptHash}
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/cross_block_duplicate_event/step_01.{
  Args, Datum, SpendRedeemer,
}
use midgard/fraud_proofs/cross_block_duplicate_event/step_02
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/ledger_state
use midgard/transition_trace

validator main(
  step_02_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  hub_oracle: ScriptHash,
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
        hub_ref_input_index,
        state_queue_node_ref_input_index,
        committed_event,
      }) -> {
        let Transaction { inputs, outputs, reference_inputs, .. } = tx
        expect Some(step_datum) = datum
        let
            _own_script_hash,
            computation_thread_token_asset_name,
            _fraud_prover,
            _m_input_state_data,
            output_script_hash,
            output_state_data
          <- continue(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          inputs,
          outputs,
        )

        let header, header_hash, hub_datum <- step_01.verify_challenged_header_v1(
          computation_thread_token_asset_name,
          hub_oracle,
          hub_ref_input_index,
          state_queue_node_ref_input_index,
          reference_inputs,
        )
        expect
          step_01.verify_committed_event_membership_v1(header, committed_event)

        expect output_script_hash == step_02_validator_script_hash
        let expected_output_state: Data = step_02.State {
          challenged_header_hash: header_hash,
          settlement_policy_id: hub_datum.settlement,
          event_kind: step_01.committed_event_kind_v1(committed_event),
          event_key: step_01.committed_event_key_v1(committed_event),
        }
        expect output_state_data == expected_output_state
        True
      }
    }
  }

  else(_) {
    fail
  }
}

fn run_step_01(
  block: step_01.DuplicateEventBlockV1,
  committed_event: step_01.CommittedDuplicateEventProofV1,
  thread_asset_name: AssetName,
) -> Bool {
  let expected_output_state: Data = step_02.State {
    challenged_header_hash: block.block.header_hash,
    settlement_policy_id: step_02.fixture_settlement_policy_v1(),
    event_kind: step_01.committed_event_kind_v1(committed_event),
    event_key: step_01.committed_event_key_v1(committed_event),
  }
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(fixture.step_datum_without_state()),
    ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        hub_ref_input_index: 0,
        state_queue_node_ref_input_index: 1,
        committed_event,
      },
    ),
    fixture.own_out_ref(),
    step_01.continue_step_l1_tx_v1(
      thread_asset_name,
      expected_output_state,
      step_01.block_reference_inputs_v1(block),
    ),
  )
}

test cross_block_duplicate_event_step_01_accepts_deposit_and_withdrawal_membership() {
  let deposit = step_01.duplicate_deposit_block_v1()
  let withdrawal = step_01.duplicate_withdrawal_block_v1()
  and {
    run_step_01(
      deposit,
      deposit.committed_event,
      step_01.block_thread_asset_name_v1(deposit),
    ),
    run_step_01(
      withdrawal,
      withdrawal.committed_event,
      step_01.block_thread_asset_name_v1(withdrawal),
    ),
  }
}

test cross_block_duplicate_event_step_01_rejects_forged_source_root() fail {
  let challenged = step_01.duplicate_deposit_block_v1()
  let forged = step_01.deposit_event_block_v1(
    step_01.foreign_event_id_v1(),
    step_01.duplicated_deposit_info_v1(),
  )
  run_step_01(
    challenged,
    forged.committed_event,
    step_01.block_thread_asset_name_v1(challenged),
  )
}

test cross_block_duplicate_event_step_01_rejects_wrong_count() fail {
  let block = step_01.duplicate_deposit_block_v1()
  expect step_01.CommittedDuplicateDepositV1 { membership } = block.committed_event
  run_step_01(
    block,
    step_01.CommittedDuplicateDepositV1 {
      membership: transition_trace.RootMembershipProof {
        ..membership,
        count: membership.count + 1,
      },
    },
    step_01.block_thread_asset_name_v1(block),
  )
}

const transition_trace_category_id: ByteArray = #"00000004"

fn foreign_category_thread_asset_name(
  challenged_header_hash: ledger_state.HeaderHash,
) -> AssetName {
  transition_trace_category_id
    |> builtin.append_bytearray(challenged_header_hash)
}

test cross_block_duplicate_event_step_01_rejects_wrong_category() fail {
  let block = step_01.duplicate_withdrawal_block_v1()
  run_step_01(
    block,
    block.committed_event,
    foreign_category_thread_asset_name(block.block.header_hash),
  )
}

fn run_cancel(signed_by_prover: Bool) -> Bool {
  let block = step_01.duplicate_deposit_block_v1()
  let asset_name = step_01.block_thread_asset_name_v1(block)
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(fixture.step_datum_without_state()),
    ct.Cancel { input_index: 0, computation_thread_mint_redeemer_index: 0 },
    fixture.own_out_ref(),
    fixture.cancel_step_l1_tx_v1(asset_name, asset_name, signed_by_prover),
  )
}

test cross_block_duplicate_event_step_01_accepts_prover_cancellation() {
  run_cancel(True)
}

test cross_block_duplicate_event_step_01_rejects_unsigned_cancellation() fail {
  run_cancel(False)
}
