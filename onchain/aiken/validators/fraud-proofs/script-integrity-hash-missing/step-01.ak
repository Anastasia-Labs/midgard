//// `scriptIntegrityHashMissing` step 01: authenticate the verdict source.

use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{
  cancel, continue, pass_native_tx_to_next_step_carried,
}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/script_integrity_hash_missing/state.{
  BoundAccepted, PendingForced,
}
use midgard/fraud_proofs/script_integrity_hash_missing/step_01.{
  BindAccepted, Datum, RecordForced, SpendRedeemer,
}

validator main(
  step_02_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  hub_oracle: ScriptHash,
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
      ct.Continue(BindAccepted { carriage }) -> {
        let
            _,
            _,
            _,
            input_state,
            output_script_hash,
            output_state_data,
            _,
            _,
            verified
          <- pass_native_tx_to_next_step_carried(
          computation_thread_token_policy_id,
          hub_oracle,
          datum,
          carriage,
          own_out_ref,
          tx,
        )
        expect None = input_state
        expect output_script_hash == step_02_validator_script_hash
        let subject = substrate.bind_accepted_subject_v1(verified)
        let expected: Data = BoundAccepted {
          subject,
          witness_set_hash: verified.tx_compact.witness_set_hash,
        }
        expect output_state_data == expected
        True
      }
      ct.Continue(RecordForced { direction, input_index, output_index }) -> {
        expect or {
            direction == substrate.direction_wrongful_acceptance,
            direction == substrate.direction_wrongful_rejection,
          }
        expect Some(step_datum) = datum
        let Transaction { inputs, outputs, .. } = tx
        let _, _, _, input_state, output_script_hash, output_state_data <- continue(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          inputs,
          outputs,
        )
        expect None = input_state
        expect output_script_hash == step_02_validator_script_hash
        let expected: Data = PendingForced { direction }
        expect output_state_data == expected
        True
      }
    }
  }

  else(_) {
    fail
  }
}
