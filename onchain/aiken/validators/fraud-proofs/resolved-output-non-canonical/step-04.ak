use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/resolved_output_non_canonical/rule.{
  CanonicalVerdictV1, ContinueDatum, FinishDatum, advance_reconstruction_v1,
  finalize_canonical_v1, open_datum_v1,
}
use midgard/fraud_proofs/resolved_output_non_canonical/step_03.{
  ReconstructionV1,
}
use midgard/fraud_proofs/resolved_output_non_canonical/step_04.{
  Advance, Args, Datum, FinalizeCanonical, SpendRedeemer,
}
use midgard/ledger_output_commitment_v1

validator main(
  step_04_datum_hash: ScriptHash,
  step_05_hash: ScriptHash,
  thread_policy: PolicyId,
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
      ct.Continue(Args { input_index, output_index, action }) -> {
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum
        let own_hash, _, _, state, next_hash, next_state <- continue(
          thread_policy,
          step_datum,
          input_index,
          output_index,
          own_ref,
          inputs,
          outputs,
        )
        expect Some(ReconstructionV1 { subject, descriptor_cbor, control }) = state
        let descriptor = ledger_output_commitment_v1.decode_canonical_schema(
          descriptor_cbor,
        )
        let emit = fn(hash: ScriptHash, value: Data) {
          and {
            next_hash == hash,
            next_state == value,
          }
        }
        when action is {
          Advance { chunk_proof, next_chunk_proof } ->
            when
              advance_reconstruction_v1(
                descriptor,
                control,
                chunk_proof,
                next_chunk_proof,
              )
            is {
              Some(next_control) ->
                emit(
                  own_hash,
                  ReconstructionV1 {
                    subject,
                    descriptor_cbor,
                    control: next_control,
                  },
                )
              None ->
                emit(
                  step_05_hash,
                  CanonicalVerdictV1 { subject, output_is_non_canonical: True },
                )
            }
          FinalizeCanonical ->
            when
              open_datum_v1(
                subject,
                descriptor,
                finalize_canonical_v1(descriptor, control),
              )
            is {
              ContinueDatum { state: datum_state } ->
                emit(step_04_datum_hash, datum_state)
              FinishDatum { verdict } -> emit(step_05_hash, verdict)
            }
        }
      }
    }
  }

  else(_) {
    fail
  }
}
