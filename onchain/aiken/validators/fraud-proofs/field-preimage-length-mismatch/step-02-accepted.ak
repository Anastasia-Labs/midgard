use aiken/cbor
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_preimage_length_mismatch/rule.{
  authenticated_length_evidence_v1,
}
use midgard/fraud_proofs/field_preimage_length_mismatch/step_02.{
  AuthenticateAccepted, BoundSource, Datum, SpendRedeemer,
}
use midgard/fraud_proofs/field_preimage_length_mismatch/step_03
use midgard/fraud_proofs/native_tx/compact.{verify_native_tx_proof_source_v1}
use midgard/ledger_state.{L2TransactionSourceV1}

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
      ct.Continue(AuthenticateAccepted { input_index, output_index, claim }) -> {
        let Transaction { inputs, outputs, reference_inputs, .. } = tx
        expect Some(step_datum) = datum
        let _, _, _, input_state, output_script_hash, output_state_data <- continue(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          inputs,
          outputs,
        )
        expect output_script_hash == step_03_validator_script_hash
        expect Some(BoundSource { subject, source_cbor }) = input_state
        expect Some(source_data) = cbor.deserialise(source_cbor)
        expect source: L2TransactionSourceV1 = source_data
        expect cbor.serialise(source) == source_cbor
        expect source.tx_id == subject.transaction_id
        let Pair(verified, _) = verify_native_tx_proof_source_v1(
          source.tx_id,
          source.source.compact_cbor,
          source.source.witness_set_compact_cbor,
          source.source.field_preimage_lengths_cbor,
        )
        let evidence = authenticated_length_evidence_v1(
          verified,
          source.source.field_preimage_lengths_cbor,
          claim,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
        let expected: Data = step_03.State {
          subject,
          field_index: evidence.field_index,
          declared_length: evidence.declared_length,
          actual_length: evidence.actual_length,
        }
        output_state_data == expected
      }
      ct.Continue(_) -> fail
    }
  }

  else(_) {
    fail
  }
}
