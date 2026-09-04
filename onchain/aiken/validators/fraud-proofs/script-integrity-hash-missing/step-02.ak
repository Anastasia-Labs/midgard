use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/native_tx/compact.{verify_native_tx_proof_source_v1}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/script_integrity_hash_missing/state.{
  BoundAccepted, PendingForced, SubjectStateV1,
}
use midgard/fraud_proofs/script_integrity_hash_missing/step_02.{
  Args, Datum, SpendRedeemer,
}
use midgard/rejection_reason_v1.{ScriptIntegrityHashMissing}

validator main(
  step_03_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
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
      ct.Continue(args) -> {
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum
        let
            _,
            thread_asset_name,
            _,
            input_state,
            output_script_hash,
            output_state_data
          <- continue(
          computation_thread_token_policy_id,
          step_datum,
          args.input_index,
          args.output_index,
          own_out_ref,
          inputs,
          outputs,
        )
        expect Some(state) = input_state
        expect output_script_hash == step_03_validator_script_hash
        let Pair(subject, witness_set_hash) = when state is {
          BoundAccepted { subject, witness_set_hash } -> {
            expect None = args.forced_membership
            Pair(subject, witness_set_hash)
          }
          PendingForced { direction } -> {
            expect Some(membership) = args.forced_membership
            let subject = substrate.bind_forced_subject_to_thread_v1(
              thread_asset_name,
              args.header,
              membership,
              direction,
            )
            let leaf = membership.value
            let Pair(verified, _) = verify_native_tx_proof_source_v1(
              leaf.tx_id,
              leaf.source.compact_cbor,
              leaf.source.witness_set_compact_cbor,
              leaf.source.field_preimage_lengths_cbor,
            )
            Pair(subject, verified.tx_compact.witness_set_hash)
          }
        }
        expect ScriptIntegrityHashMissing = if subject.direction == substrate.direction_wrongful_rejection {
          substrate.bind_exact_rejection_reason_v1(
            subject,
            ScriptIntegrityHashMissing,
          )
        } else {
          ScriptIntegrityHashMissing
        }
        let expected: Data = SubjectStateV1 { subject, witness_set_hash }
        expect output_state_data == expected
        True
      }
    }
  }

  else(_) {
    fail
  }
}
