use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{
  cancel, continue, pass_native_tx_to_next_step_carried,
}
use midgard/fraud_proofs/direct_script_derivation/types.{SourceBoundV1} as direct_types
use midgard/fraud_proofs/native_tx/compact.{
  verify_forced_tx_proof_source_v1, verify_native_tx_proof_source_v1,
}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/source_carriage_v1
use midgard/fraud_proofs/unused_redeemer/rule.{bind_redeemer_v1}
use midgard/fraud_proofs/unused_redeemer/step_01.{
  AcceptedSource, Args, Datum, ForcedSource, SpendRedeemer,
}

validator main(
  step_02_hash: ScriptHash,
  thread_policy: PolicyId,
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
          thread_policy,
          datum,
          input_index,
          own_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(Args { source: AcceptedSource { inclusion }, redeemer_index }) -> {
        let
            _,
            subject_thread_name,
            _,
            _,
            output_hash,
            output_state,
            header,
            _,
            verified
          <- pass_native_tx_to_next_step_carried(
          thread_policy,
          hub_oracle,
          datum,
          inclusion,
          own_ref,
          tx,
        )
        let expected: Data = SourceBoundV1 {
          subject: substrate.open_subject_v1(
            subject_thread_name,
            substrate.AcceptedSource { header, tx_inclusion: verified },
          ),
          header,
          compact_cbor: source_carriage_v1.carriage_compact_cbor_v1(inclusion),
        }
        and {
          output_hash == step_02_hash,
          output_state == expected,
        }
      }
      ct.Continue(Args {
        source: ForcedSource { input_index, output_index, header, membership },
        redeemer_index,
      }) -> {
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum
        let _, thread_name, _, _, output_hash, output_state <- continue(
          thread_policy,
          step_datum,
          input_index,
          output_index,
          own_ref,
          inputs,
          outputs,
        )
        let subject = substrate.open_subject_v1(
          thread_name,
          substrate.ForcedSource { header, membership },
        )
        let leaf = membership.value
        let Pair(verified, _) = verify_forced_tx_proof_source_v1(
          leaf.tx_id,
          leaf.submitted_source.compact_cbor,
          leaf.submitted_source.witness_set_compact_cbor,
          leaf.submitted_source.field_preimage_lengths_cbor,
        )
        expect verified.tx_id == subject.transaction_id
        let expected: Data = SourceBoundV1 {
          subject,
          header,
          compact_cbor: membership.value.submitted_source.compact_cbor,
        }
        and {
          output_hash == step_02_hash,
          output_state == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}
