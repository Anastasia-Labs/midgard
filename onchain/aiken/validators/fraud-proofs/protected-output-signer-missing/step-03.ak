use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{WitnessAnchor, opened_field_walk}
use midgard/fraud_proofs/protected_output_signer_missing/rule.{
  WitnessScanV1, address_witnesses_field_index,
}
use midgard/fraud_proofs/protected_output_signer_missing/step_03.{
  Args, Datum, SpendRedeemer,
}
use midgard/native_tx_machine_walk_v1.{field_walk_checkpoint_hash}

validator main(
  step_04_hash: ScriptHash,
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
        expect Some(protected) = prior
        let Pair(_view, checkpoint) = opened_field_walk(
          opening,
          WitnessAnchor {
            tx_id: protected.transaction_id,
            witness_set_hash: protected.witness_set_hash,
          },
          address_witnesses_field_index,
          reference_inputs,
          certificate_policy,
        )
        let expected: Data = WitnessScanV1 {
          protected,
          checkpoint_hash: field_walk_checkpoint_hash(checkpoint),
          signer_present: False,
        }
        and {
          next_hash == step_04_hash,
          next_data == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}
