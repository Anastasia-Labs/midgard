use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{
  WitnessAnchor, address_witnesses_field_index, opened_field_walk,
}
use midgard/fraud_proofs/spend_input_signer_missing/rule.{WitnessScanV1}
use midgard/fraud_proofs/spend_input_signer_missing/step_03.{
  Args, Datum, SpendRedeemer,
}
use midgard/native_tx_machine_walk_v1.{field_walk_checkpoint_hash}

validator main(
  step_04_hash: ScriptHash,
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
      ct.Continue(Args { input_index, output_index, witnesses_opening }) -> {
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
        )
        expect Some(authenticated) = state
        let Pair(_, checkpoint) = opened_field_walk(
          witnesses_opening,
          WitnessAnchor {
            tx_id: authenticated.transaction_id,
            witness_set_hash: authenticated.witness_set_hash,
          },
          address_witnesses_field_index,
          reference_inputs,
          certificate_policy,
        )
        let expected: Data = WitnessScanV1 {
          authenticated,
          checkpoint_hash: field_walk_checkpoint_hash(checkpoint),
        }
        and {
          next_hash == step_04_hash,
          next_state == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}
