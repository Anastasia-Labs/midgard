use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, opened_certified_field_item, opened_certified_field_item_count,
}
use midgard/fraud_proofs/output_reference_script_decoding/rule.{
  OutputDescriptorStateV1, absent_output_state_v1, initial_output_scan_v1,
  outputs_field_index,
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
  step_06_hash: ScriptHash,
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
        coordinate_absent,
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
        let anchor = BodyAnchor {
          tx_id: bound.subject.transaction_id,
          source_kind: bound.subject.source_kind,
        }
        if coordinate_absent {
          // Canonical validation decodes no reference script at a position
          // field 2 lacks, so the scan closes with no fault and goes straight
          // to the terminal, which convicts only a rejection. The count is
          // the authenticated one.
          expect or {
              bound.output_index < 0,
              bound.output_index >= opened_certified_field_item_count(
                opening,
                anchor,
                outputs_field_index,
                grammar_ref_input_index,
                reference_inputs,
                certificate_policy,
                grammar_certificate_policy,
              ),
            }
          let value: Data = absent_output_state_v1(bound, step_06_hash)
          and {
            next_hash == step_06_hash,
            next_state == value,
          }
        } else {
          let state = initial_output_scan_v1(
            bound,
            opened_certified_field_item(
              opening,
              anchor,
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
  }

  else(_) {
    fail
  }
}
