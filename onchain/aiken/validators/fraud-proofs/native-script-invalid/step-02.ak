use aiken/crypto.{ScriptHash, blake2b_256}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{
  WitnessAnchor, opened_field_view, script_witnesses_field_index,
}
use midgard/fraud_proofs/native_script_invalid/step_02.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/native_script_invalid/step_03
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_versioned_script_at, encode_midgard_versioned_script,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardVersionedScript, NativeCardanoScript,
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
      ct.Continue(Args {
        input_index,
        output_index,
        script_index,
        script_tx_wits_opening,
      }) -> {
        let Transaction { inputs, outputs, reference_inputs, .. } = tx
        expect Some(step_datum) = datum
        let
            _own_script_hash,
            _thread_asset_name,
            _fraud_prover,
            m_input_state_data,
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
        expect
            Some(State {
              bad_tx_id,
              bad_tx_witness_set_hash,
              validity_interval_start,
              validity_interval_end,
            })
          = m_input_state_data
        let field_view = opened_field_view(
          script_tx_wits_opening,
          WitnessAnchor {
            tx_id: bad_tx_id,
            witness_set_hash: bad_tx_witness_set_hash,
          },
          script_witnesses_field_index,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
        let script_item_cbor = field_item_at(field_view, script_index)
        let Pair(offset, script) = decode_midgard_versioned_script_at(
          script_item_cbor,
          0,
        )
        expect offset == bytearray.length(script_item_cbor)
        expect encode_midgard_versioned_script(script) == script_item_cbor
        expect MidgardVersionedScript { language: NativeCardanoScript, .. } = script
        expect output_script_hash == step_03_validator_script_hash
        let expected_output_state: Data = step_03.State {
          bad_tx_id,
          bad_tx_witness_set_hash,
          script_item_hash: blake2b_256(script_item_cbor),
          validity_interval_start,
          validity_interval_end,
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
