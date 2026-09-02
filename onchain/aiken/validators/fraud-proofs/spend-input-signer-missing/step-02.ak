use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue, verify_membership_carried}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, opened_field_view, spend_inputs_field_index,
}
use midgard/fraud_proofs/native_tx/types.{PubKeyCredential}
use midgard/fraud_proofs/spend_input_signer_missing/rule.{
  authenticate_credential_v1,
}
use midgard/fraud_proofs/spend_input_signer_missing/step_02.{
  Args, Datum, SpendRedeemer,
}
use midgard/fraud_proofs/transition_trace/proof.{ledger_outref_key}
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_v1
use midgard/native_tx_machine_walk_v1.{spend_input_at}

validator main(
  step_03_hash: ScriptHash,
  thread_policy: PolicyId,
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
          thread_policy,
          datum,
          input_index,
          own_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(Args {
        input_index,
        output_index,
        spend_inputs_opening,
        descriptor_cbor,
        membership,
      }) -> {
        let Transaction { inputs, outputs, reference_inputs, redeemers, .. } = tx
        expect Some(step_datum) = datum
        let _, _, _, state, next_hash, next_state <- continue(
          thread_policy,
          step_datum,
          input_index,
          output_index,
          own_ref,
          inputs,
          outputs,
        )
        expect Some(bound) = state
        let view = opened_field_view(
          spend_inputs_opening,
          BodyAnchor { tx_id: bound.subject.transaction_id },
          spend_inputs_field_index,
          reference_inputs,
          certificate_policy,
        )
        let selected = spend_input_at(view, bound.input_index)
        let out_ref = OutputReference {
          transaction_id: selected.tx_id,
          output_index: selected.output_index,
        }
        expect
          verify_membership_carried(
            membership,
            bound.prior_root,
            ledger_outref_key(out_ref),
            descriptor_cbor,
            reference_inputs,
            redeemers,
          )
        let descriptor = ledger_output_commitment_v1.decode(descriptor_cbor)
        expect descriptor.output_index == out_ref.output_index
        expect Some(address) = ledger_output_v1.decode_canonical_address_bytes(
          descriptor.address,
        )
        expect PubKeyCredential(payment_credential) = address.payment_credential
        let authenticated = authenticate_credential_v1(
          bound,
          payment_credential,
        )
        let expected: Data = authenticated
        and {
          next_hash == step_03_hash,
          next_state == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}
