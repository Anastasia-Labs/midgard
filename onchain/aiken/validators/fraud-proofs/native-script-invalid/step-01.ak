use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{
  cancel, continue, pass_native_tx_to_next_step_carried,
}
use midgard/fraud_proofs/native_script_invalid/step_01.{
  AcceptedSource, Args, Datum, ForcedSource, SpendRedeemer,
}
use midgard/fraud_proofs/native_script_invalid/step_02
use midgard/fraud_proofs/native_tx/compact.{
  verify_forced_tx_compact_cbor_v1, verify_native_tx_compact_cbor_v1,
}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/rejection_reason_v1.{WitnessNativeScriptFalse}

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
      ct.Continue(Args{source: AcceptedSource { inclusion: carriage }}) -> {
        let
            _own_script_hash,
            _thread_asset_name,
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
          carriage,
          own_out_ref,
          tx,
        )
        expect None = m_input_state_data
        expect bad_tx_view.tx_compact.validity_code == Some(0)
        expect output_script_hash == step_02_validator_script_hash
        let expected_output_state: Data = step_02.State {
          grammar_checkpoint_hash: #"",
          grammar_complete: False,
          script_checkpoint_hash: #"",
          subject: substrate.bind_accepted_subject_v1(bad_tx_view),
          bad_tx_id,
          bad_tx_witness_set_hash: bad_tx_view.tx_compact.witness_set_hash,
          validity_interval_start: bad_tx_view.tx_compact.body.validity_interval_start,
          validity_interval_end: bad_tx_view.tx_compact.body.validity_interval_end,
        }
        expect output_state_data == expected_output_state
        True
      }
      ct.Continue(Args{source: ForcedSource {
        input_index,
        output_index,
        header,
        membership,
        direction,
      }}) -> {
        expect Some(step_datum) = datum
        let _, name, _, prior, output_hash, output_data <- continue(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          tx.inputs,
          tx.outputs,
        )
        expect None = prior
        let subject = substrate.bind_forced_subject_to_thread_v1(
          name,
          header,
          membership,
          direction,
        )
        expect subject.direction == substrate.direction_wrongful_rejection
        expect Some(WitnessNativeScriptFalse { script_index }) = subject.rejection_reason
        expect script_index >= 0
        let verified = verify_forced_tx_compact_cbor_v1(
          subject.transaction_id,
          membership.value.submitted_source.compact_cbor,
        )
        let expected: Data = step_02.State {
          grammar_checkpoint_hash: #"",
          grammar_complete: False,
          script_checkpoint_hash: #"",
          subject,
          bad_tx_id: verified.tx_id,
          bad_tx_witness_set_hash: verified.tx_compact.witness_set_hash,
          validity_interval_start: verified.tx_compact.body.validity_interval_start,
          validity_interval_end: verified.tx_compact.body.validity_interval_end,
        }
        and {
          output_hash == step_02_validator_script_hash,
          output_data == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}
