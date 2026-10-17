use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, opened_committed_preimage,
}
use midgard/fraud_proofs/mint_item_non_canonical/field_scan
use midgard/fraud_proofs/mint_item_non_canonical/rule.{
  BoundItemV1, initial_scan_v1, mint_field_index,
}
use midgard/fraud_proofs/mint_item_non_canonical/step_02.{
  Args, Datum, SpendRedeemer,
}

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
      ct.Continue(Args { input_index, output_index, opening }) -> {
        let Transaction { inputs, outputs, reference_inputs, .. } = tx
        expect Some(step_datum) = datum
        let own_hash, _name, _prover, input_state, next_hash, next_state <- continue(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          inputs,
          outputs,
        )
        expect Some(bound) = input_state
        let field = opened_committed_preimage(
          opening,
          BodyAnchor {
            tx_id: bound.subject.transaction_id,
            source_kind: bound.subject.source_kind,
          },
          mint_field_index,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
        let Pair(cursor, complete) = field_scan.advance(
          field,
          bound.item_index,
          bound.field_cursor,
        )
        if complete {
          let expected: Data = initial_scan_v1(
            bound,
            bytearray.slice(
              field,
              cursor.item_offset,
              cursor.item_offset + cursor.item_length - 1,
            ),
            cursor.previous_policy,
          )
          and {
            next_hash == step_03_validator_script_hash,
            next_state == expected,
          }
        } else {
          let expected: Data = BoundItemV1 {
            ..bound,
            field_cursor: Some(cursor),
          }
          and {
            next_hash == own_hash,
            next_state == expected,
          }
        }
      }
    }
  }

  else(_) {
    fail
  }
}
