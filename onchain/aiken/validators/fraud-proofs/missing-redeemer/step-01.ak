use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{
  cancel, continue, pass_native_tx_to_next_step_carried,
}
use midgard/fraud_proofs/missing_redeemer/rule.{bind_purpose_v1}
use midgard/fraud_proofs/missing_redeemer/step_01.{
  AcceptedSource, Args, Datum, ForcedSource, SpendRedeemer,
}
use midgard/fraud_proofs/native_tx/compact.{verify_native_tx_proof_source_v1}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate

validator main(
  step_02_hash: ScriptHash,
  computation_thread_policy: PolicyId,
  hub_oracle: ScriptHash,
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
        source: AcceptedSource { inclusion },
        purpose_kind,
        purpose_index,
      }) -> {
        let _, _, _, _, next_hash, next_state, header, _, verified <- pass_native_tx_to_next_step_carried(
          computation_thread_policy,
          hub_oracle,
          datum,
          inclusion,
          own_ref,
          tx,
        )
        let expected: Data = bind_purpose_v1(
          substrate.bind_accepted_subject_v1(verified),
          verified.tx_compact.witness_set_hash,
          header.validation_traces_root,
          header.validation_trace_count,
          purpose_kind,
          purpose_index,
        )
        and {
          next_hash == step_02_hash,
          next_state == expected,
        }
      }
      ct.Continue(Args {
        source: ForcedSource {
          input_index,
          output_index,
          header,
          membership,
          direction,
        },
        purpose_kind,
        purpose_index,
      }) -> {
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum
        let _, thread_name, _, _, next_hash, next_state <- continue(
          computation_thread_policy,
          step_datum,
          input_index,
          output_index,
          own_ref,
          inputs,
          outputs,
        )
        let subject = substrate.bind_forced_subject_to_thread_v1(
          thread_name,
          header,
          membership,
          direction,
        )
        let Pair(verified, _) = verify_native_tx_proof_source_v1(
          membership.value.tx_id,
          membership.value.source.compact_cbor,
          membership.value.source.witness_set_compact_cbor,
          membership.value.source.field_preimage_lengths_cbor,
        )
        let expected: Data = bind_purpose_v1(
          subject,
          verified.tx_compact.witness_set_hash,
          header.validation_traces_root,
          header.validation_trace_count,
          purpose_kind,
          purpose_index,
        )
        and {
          next_hash == step_02_hash,
          next_state == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}
