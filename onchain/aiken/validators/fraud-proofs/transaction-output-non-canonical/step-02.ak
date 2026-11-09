use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, opened_certified_field_item,
}
use midgard/fraud_proofs/transaction_output_non_canonical/rule.{
  OutputScanStateV1, initial_scan_v1, outputs_field_index,
}
use midgard/fraud_proofs/transaction_output_non_canonical/step_02.{
  Args, Datum, SpendRedeemer,
}

fn output_state(state: OutputScanStateV1) -> Data {
  let value: Data = state
  value
}

validator main(
  step_03_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  field_preimage_certificate_policy_id: PolicyId,
  field_grammar_certificate_policy_id: PolicyId,
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
        opening,
        grammar_ref_input_index,
      }) -> {
        let Transaction { inputs, outputs, reference_inputs, .. } = tx
        expect Some(step_datum) = datum
        let _own, _name, _prover, input_state, next_hash, next_state <- continue(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          inputs,
          outputs,
          tx.extra_signatories,
        )
        expect Some(bound) = input_state
        let state = initial_scan_v1(
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
            field_preimage_certificate_policy_id,
            field_grammar_certificate_policy_id,
          ),
        )
        and {
          next_hash == step_03_validator_script_hash,
          next_state == output_state(state),
        }
      }
    }
  }

  else(_) {
    fail
  }
}
