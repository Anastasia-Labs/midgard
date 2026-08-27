use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/fabricated_withdrawal/step_01 as source
use midgard/fraud_proofs/withdrawal_mistag/step_01
use midgard/hub_oracle as hub
use midgard/ledger_state
use midgard/state_queue

validator main(
  step_02_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  hub_oracle: ScriptHash,
) {
  spend(
    datum: Option<step_01.Datum>,
    redeemer: step_01.SpendRedeemer,
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
      ct.Continue(args) -> {
        let Transaction { inputs, outputs, reference_inputs, .. } = tx
        expect Some(step_datum) = datum
        let _, asset_name, _, _, output_script_hash, output_state_data <- continue(
          computation_thread_token_policy_id,
          step_datum,
          args.input_index,
          args.output_index,
          own_out_ref,
          inputs,
          outputs,
        )
        expect step_01.category_is_withdrawal_mistag_v1(asset_name)
        let hub_datum = hub.get_datum(
          reference_inputs,
          hub_oracle,
          args.hub_ref_input_index,
        )
        let header, header_hash <- state_queue.get_block_datum_v1(
          reference_inputs,
          hub_datum.state_queue,
          args.state_queue_node_ref_input_index,
        )
        expect header_hash == step_01.challenged_header_hash_of_v1(asset_name)
        expect
          source.verify_committed_withdrawal_membership_v1(
            header,
            args.committed_withdrawal,
          )
        expect output_script_hash == step_02_validator_script_hash
        let expected: Data = step_01.State {
          challenged_header_hash: header_hash,
          withdrawal_id: args.committed_withdrawal.key,
          withdrawal_info_hash: utils.serialise_and_hash_32(
            args.committed_withdrawal.value,
          ),
          claimed_valid: args.committed_withdrawal.value.validity == ledger_state.WithdrawalIsValid,
          event_to_step_root: header.event_to_step_root,
          total_event_count: header.total_event_count,
          transition_trace_root: header.transition_trace_root,
          transition_step_count: header.transition_step_count,
        }
        expect output_state_data == expected
        True
      }
    }
  }

  else(_) {
    fail
  }
}
