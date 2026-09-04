use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{BodyAnchor, opened_field_walk}
use midgard/fraud_proofs/observer_order_invalid/rule.{
  initial_scan_v1, observer_field_index,
}
use midgard/fraud_proofs/observer_order_invalid/state.{Bound}
use midgard/fraud_proofs/observer_order_invalid/step_02.{
  Authenticate, Datum, Reserved, SpendRedeemer,
}
use midgard/native_tx_field_access_v1.{field_count_requires_certification}
use midgard/native_tx_machine_walk_v1.{field_walk_checkpoint_hash}

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
      ct.Continue(Authenticate { input_index, output_index, opening }) -> {
        let Transaction { inputs, outputs, reference_inputs, .. } = tx
        expect Some(step_datum) = datum
        let _, _, _, input_state, output_hash, output_state <- continue(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          inputs,
          outputs,
        )
        expect Some(Bound { bound }) = input_state
        let Pair(view, start) = opened_field_walk(
          opening,
          BodyAnchor { tx_id: bound.subject.transaction_id },
          observer_field_index,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
        // Fields 3/4 have a fixed 30-byte stride, so even certified carriage
        // derives the count and walk position without a provisional grammar.
        expect !field_count_requires_certification(view)
        let expected: Data = initial_scan_v1(
          bound,
          field_walk_checkpoint_hash(start),
        )
        and {
          output_hash == step_03_validator_script_hash,
          output_state == expected,
        }
      }
      ct.Continue(Reserved) -> fail
    }
  }

  else(_) {
    fail
  }
}
