use aiken/crypto.{ScriptHash, blake2b_256}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/bounded_item_v1
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/execution_native_script_invalid/rule.{
  source_item_coordinate,
}
use midgard/fraud_proofs/execution_native_script_invalid/step_03.{
  Args, Datum, SpendRedeemer,
}
use midgard/fraud_proofs/execution_native_script_invalid/step_04
use midgard/fraud_proofs/native_tx/compact.{decode_native_tx_compact_v1}
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_versioned_script_at, encode_midgard_versioned_script,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardVersionedScript, NativeCardanoScript,
}

validator main(
  step_04_validator_script_hash: ScriptHash,
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
      ct.Continue(Args { input_index, output_index, script_item_cbor }) -> {
        let Transaction { inputs, outputs, .. } = tx
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
        expect Some(source) = m_input_state_data
        let compact = decode_native_tx_compact_v1(source.compact_cbor)
        let direction = source.bound.subject.direction
        let execution_index = source.bound.execution_index
        let source_index = source.source_index
        let origin_kind = source.origin_kind
        let item_commitment = source.item_commitment
        let bad_tx_id = source.bound.subject.transaction_id
        let bad_tx_witness_set_hash = compact.witness_set_hash
        let validity_interval_start = compact.body.validity_interval_start
        let validity_interval_end = compact.body.validity_interval_end
        let Pair(field_index, item_index) = source_item_coordinate(source)
        expect
          bounded_item_v1.from_bytes(field_index, item_index, script_item_cbor) == item_commitment
        let Pair(offset, script) = decode_midgard_versioned_script_at(
          script_item_cbor,
          0,
        )
        expect offset == bytearray.length(script_item_cbor)
        expect encode_midgard_versioned_script(script) == script_item_cbor
        expect MidgardVersionedScript { language: NativeCardanoScript, .. } = script
        expect output_script_hash == step_04_validator_script_hash
        let expected_output_state: Data = step_04.State {
          direction,
          execution_index,
          source_index,
          origin_kind,
          item_commitment,
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
