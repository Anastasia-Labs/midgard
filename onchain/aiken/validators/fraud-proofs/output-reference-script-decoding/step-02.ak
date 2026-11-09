use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, opened_certified_field_item,
}
use midgard/fraud_proofs/output_reference_script_decoding/rule.{
  OutputDescriptorStateV1, initial_output_scan_v1, outputs_field_index,
}
use midgard/fraud_proofs/output_reference_script_decoding/step_02.{
  Args, Datum, SpendRedeemer,
}

fn output_state(state: OutputDescriptorStateV1) -> Data {
  let value: Data = state
  value
}

validator main(
  step_03_hash: ScriptHash,
  computation_thread_policy: PolicyId,
  certificate_policy: PolicyId,
  grammar_certificate_policy: PolicyId,
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
      ct.Continue(Args {
        input_index,
        output_index,
        opening,
        grammar_ref_input_index,
      }) -> {
        let Transaction { inputs, outputs, reference_inputs, .. } = tx
        expect Some(step_datum) = datum
        let _own, _name, _prover, input_state, next_hash, next_state <- continue(
          computation_thread_policy,
          step_datum,
          input_index,
          output_index,
          own_ref,
          inputs,
          outputs,
          tx.extra_signatories,
        )
        expect Some(bound) = input_state
        let state = initial_output_scan_v1(
          bound,
          opened_certified_field_item(
            opening,
            BodyAnchor {
              tx_id: bound.subject.transaction_id,
              source_kind: bound.subject.source_kind,
            },
            outputs_field_index,
            bound.output_index,
            grammar_ref_input_index,
            reference_inputs,
            certificate_policy,
            grammar_certificate_policy,
          ),
        )
        and {
          next_hash == step_03_hash,
          next_state == output_state(state),
        }
      }
    }
  }

  else(_) {
    fail
  }
}
