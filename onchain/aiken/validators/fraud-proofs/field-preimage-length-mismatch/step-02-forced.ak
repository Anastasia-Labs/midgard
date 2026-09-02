use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_preimage_length_mismatch/rule.{
  authenticated_length_evidence_v1,
}
use midgard/fraud_proofs/field_preimage_length_mismatch/step_02.{
  AuthenticateForced, Datum, PendingForced, SpendRedeemer,
}
use midgard/fraud_proofs/field_preimage_length_mismatch/step_03
use midgard/fraud_proofs/native_tx/compact.{verify_native_tx_proof_source_v1}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/rejection_reason_v1.{FieldPreimageLengthMismatch}

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
      ct.Continue(AuthenticateForced {
        input_index,
        output_index,
        header,
        membership,
        claim,
      }) -> {
        let Transaction { inputs, outputs, reference_inputs, .. } = tx
        expect Some(step_datum) = datum
        let
            _,
            thread_asset_name,
            _,
            input_state,
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
        expect output_script_hash == step_03_validator_script_hash
        expect Some(PendingForced { direction }) = input_state
        let subject = substrate.bind_forced_subject_to_thread_v1(
          thread_asset_name,
          header,
          membership,
          direction,
        )
        let leaf = membership.value
        let Pair(verified, _) = verify_native_tx_proof_source_v1(
          leaf.tx_id,
          leaf.source.compact_cbor,
          leaf.source.witness_set_compact_cbor,
          leaf.source.field_preimage_lengths_cbor,
        )
        let evidence = authenticated_length_evidence_v1(
          verified,
          leaf.source.field_preimage_lengths_cbor,
          claim,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
        if direction == substrate.direction_wrongful_rejection {
          expect FieldPreimageLengthMismatch { field_index } = substrate.rejection_reason_of_v1(
            subject,
          )
          expect field_index == evidence.field_index
        } else {
          expect direction == substrate.direction_wrongful_acceptance
        }
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
