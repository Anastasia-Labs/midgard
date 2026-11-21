use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, opened_field_view, reference_inputs_field_index,
  spend_inputs_field_index,
}
use midgard/fraud_proofs/resolved_output_non_canonical/rule.{
  absent_input_verdict_v1, authenticate_out_ref_v1, outside_input_fields_v1,
}
use midgard/fraud_proofs/resolved_output_non_canonical/step_02.{
  Args, Datum, SpendRedeemer,
}
use midgard/native_tx_machine_walk_v1.{spend_input_at, spend_input_count}

validator main(
  step_03_hash: ScriptHash,
  step_05_hash: ScriptHash,
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
      ct.Continue(Args { input_index, output_index, opening }) -> {
        let Transaction { inputs, outputs, reference_inputs, .. } = tx
        expect Some(step_datum) = datum
        let _, _, _, state, next_hash, next_state <- continue(
          thread_policy,
          step_datum,
          input_index,
          output_index,
          own_ref,
          inputs,
          outputs,
          tx.extra_signatories,
        )
        expect Some(bound) = state
        // A coordinate that names no input of the transaction closes with the
        // direct verdict. Only a rejection binds one (`bind_input_v1`).
        when opening is {
          None -> {
            let absent: Data = absent_input_verdict_v1(bound)
            and {
              outside_input_fields_v1(bound),
              next_hash == step_05_hash,
              next_state == absent,
            }
          }
          Some(opening) -> {
            let field_index = if bound.source_kind == 0 {
              spend_inputs_field_index
            } else {
              reference_inputs_field_index
            }
            let view = opened_field_view(
              opening,
              BodyAnchor {
                tx_id: bound.subject.transaction_id,
                source_kind: bound.subject.source_kind,
              },
              field_index,
              reference_inputs,
              certificate_policy,
            )
            if bound.input_index >= spend_input_count(view) {
              let absent: Data = absent_input_verdict_v1(bound)
              and {
                next_hash == step_05_hash,
                next_state == absent,
              }
            } else {
              let selected = spend_input_at(view, bound.input_index)
              let authenticated = authenticate_out_ref_v1(
                bound,
                OutputReference {
                  transaction_id: selected.tx_id,
                  output_index: selected.output_index,
                },
              )
              let expected: Data = authenticated
              and {
                next_hash == step_03_hash,
                next_state == expected,
              }
            }
          }
        }
      }
    }
  }

  else(_) {
    fail
  }
}
