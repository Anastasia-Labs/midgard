use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{
  WitnessAnchor, opened_field_view, script_witnesses_field_index,
}
use midgard/fraud_proofs/witness_script_decoding/rule.{authenticate_item_v1}
use midgard/fraud_proofs/witness_script_decoding/step_02.{
  Args, Datum, SpendRedeemer,
}
use midgard/native_tx_field_access_v1.{field_item_at}

validator main(
  step_03_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  field_preimage_certificate_policy_id: PolicyId,
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
      ct.Continue(Args { input_index, output_index, opening }) -> {
        let Transaction { inputs, outputs, reference_inputs, .. } = tx
        expect Some(step_datum) = datum
        let
            _own_script_hash,
            _thread_asset_name,
            _fraud_prover,
            input_state,
            next_script_hash,
            next_state
          <- continue(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          inputs,
          outputs,
        )
        expect Some(bound) = input_state
        let view = opened_field_view(
          opening,
          WitnessAnchor {
            tx_id: bound.subject.transaction_id,
            witness_set_hash: bound.witness_set_hash,
          },
          script_witnesses_field_index,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
        let item = field_item_at(view, bound.script_index)
        let expected: Data = authenticate_item_v1(
          bound,
          item,
          step_03_validator_script_hash,
        )
        and {
          next_script_hash == step_03_validator_script_hash,
          next_state == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}
