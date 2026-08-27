use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/withdrawal_mistag/step_02

validator main(
  step_03_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
) {
  spend(
    datum: Option<step_02.Datum>,
    redeemer: step_02.SpendRedeemer,
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
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum
        let _, _, _, input_state_data, output_script_hash, output_state_data <- continue(
          computation_thread_token_policy_id,
          step_datum,
          args.input_index,
          args.output_index,
          own_out_ref,
          inputs,
          outputs,
        )
        expect Some(input_state) = input_state_data
        expect
          utils.serialise_and_hash_32(args.withdrawal_info) == input_state.withdrawal_info_hash
        expect
          step_02.trace_coordinate_is_exact_v1(
            input_state,
            args.event_to_step,
            args.transition_step,
          )
        expect output_script_hash == step_03_validator_script_hash
        let expected: Data = step_02.State {
          challenged_header_hash: input_state.challenged_header_hash,
          withdrawal_id: input_state.withdrawal_id,
          withdrawal_info_hash: input_state.withdrawal_info_hash,
          claimed_valid: input_state.claimed_valid,
          pre_utxos_root: args.transition_step.value.pre_utxos_root,
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
