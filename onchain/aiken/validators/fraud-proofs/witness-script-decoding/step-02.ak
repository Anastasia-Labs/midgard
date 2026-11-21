use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{
  WitnessAnchor, opened_certified_field_item, opened_certified_field_item_count,
  script_witnesses_field_index,
}
use midgard/fraud_proofs/witness_script_decoding/rule.{
  absent_script_state_v1, authenticate_item_v1,
}
use midgard/fraud_proofs/witness_script_decoding/step_02.{
  Args, Datum, SpendRedeemer,
}
use midgard/script_decode_v1.{record_at_v1}

validator main(
  step_03_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  field_preimage_certificate_policy_id: PolicyId,
  script_decode_policy_id: PolicyId,
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
        record_reference_input_index,
        coordinate_absent,
        grammar_ref_input_index,
      }) -> {
        let Transaction { inputs, outputs, reference_inputs, .. } = tx
        expect Some(step_datum) = datum
        let
            _own_script_hash,
            _thread_asset_name,
            _fraud_prover,
            input_state,
            next_script_hash,
            next_state
          <- continue(
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
        let anchor = WitnessAnchor {
          tx_id: bound.subject.transaction_id,
          witness_set_hash: bound.witness_set_hash,
          source_kind: bound.subject.source_kind,
        }
        let expected: Data = if coordinate_absent {
          // Canonical validation decodes no script at a position field 6
          // lacks, so the state closes with no fault: it convicts only a
          // rejection at the terminal. The count is the authenticated one.
          expect or {
              bound.script_index < 0,
              bound.script_index >= opened_certified_field_item_count(
                opening,
                anchor,
                script_witnesses_field_index,
                grammar_ref_input_index,
                reference_inputs,
                field_preimage_certificate_policy_id,
                field_grammar_certificate_policy_id,
              ),
            }
          absent_script_state_v1(bound, step_03_validator_script_hash)
        } else {
          let item = opened_certified_field_item(
            opening,
            anchor,
            script_witnesses_field_index,
            bound.script_index,
            grammar_ref_input_index,
            reference_inputs,
            field_preimage_certificate_policy_id,
            field_grammar_certificate_policy_id,
          )
          authenticate_item_v1(
            bound,
            item,
            step_03_validator_script_hash,
            record_at_v1(
              reference_inputs,
              record_reference_input_index,
              script_decode_policy_id,
            ),
          )
        }
        and {
          next_script_hash == step_03_validator_script_hash,
          next_state == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}
