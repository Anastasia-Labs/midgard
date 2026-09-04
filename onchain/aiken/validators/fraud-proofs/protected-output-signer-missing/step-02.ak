use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{BodyAnchor, opened_field_view}
use midgard/fraud_proofs/native_tx/components.{decode_midgard_tx_output_cbor}
use midgard/fraud_proofs/native_tx/types.{PubKeyCredential}
use midgard/fraud_proofs/protected_output_signer_missing/rule.{
  ProtectedCredentialV1, outputs_field_index,
}
use midgard/fraud_proofs/protected_output_signer_missing/step_02.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/native_tx_field_access_v1.{field_item_at}

validator main(
  step_03_hash: ScriptHash,
  computation_thread_policy: PolicyId,
  certificate_policy: PolicyId,
) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          computation_thread_policy,
          datum,
          input_index,
          own_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(Args { input_index, output_index, opening }) -> {
        let Transaction { inputs, outputs, reference_inputs, .. } = tx
        expect Some(step_datum) = datum
        let _own, _name, _prover, prior, next_hash, next_data <- continue(
          computation_thread_policy,
          step_datum,
          input_index,
          output_index,
          own_ref,
          inputs,
          outputs,
        )
        expect Some(State { bound, witness_set_hash }) = prior
        let view = opened_field_view(
          opening,
          BodyAnchor { tx_id: bound.subject.transaction_id },
          outputs_field_index,
          reference_inputs,
          certificate_policy,
        )
        let output = decode_midgard_tx_output_cbor(
          field_item_at(view, bound.output_index),
        )
        expect output.address.protected
        expect PubKeyCredential(payment_credential) = output.address.payment_credential
        let protected = ProtectedCredentialV1 {
          subject: bound.subject,
          transaction_id: bound.subject.transaction_id,
          witness_set_hash,
          output_index: bound.output_index,
          payment_credential,
        }
        let expected: Data = protected
        and {
          next_hash == step_03_hash,
          next_data == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}
