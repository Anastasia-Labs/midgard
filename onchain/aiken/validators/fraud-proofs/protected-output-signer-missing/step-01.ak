use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{
  cancel, continue, pass_native_tx_to_next_step_carried,
}
use midgard/fraud_proofs/native_tx/compact.{verify_native_tx_proof_source_v1}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/protected_output_signer_missing/rule.{bind_output_v1}
use midgard/fraud_proofs/protected_output_signer_missing/step_01.{
  AcceptedSource, Args, Datum, ForcedSource, SpendRedeemer,
}
use midgard/fraud_proofs/protected_output_signer_missing/step_02

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
      ct.Continue(Args { source: AcceptedSource { inclusion }, output_index }) -> {
        let
            _own,
            _name,
            _prover,
            _prior,
            next_hash,
            next_data,
            _header,
            _id,
            verified
          <- pass_native_tx_to_next_step_carried(
          computation_thread_policy,
          hub_oracle,
          datum,
          inclusion,
          own_ref,
          tx,
        )
        let bound = bind_output_v1(
          substrate.bind_accepted_subject_v1(verified),
          output_index,
        )
        let expected: Data = step_02.State {
          bound,
          witness_set_hash: verified.tx_compact.witness_set_hash,
        }
        and {
          next_hash == step_02_hash,
          next_data == expected,
        }
      }
      ct.Continue(Args {
        source: ForcedSource {
          input_index,
          output_index: next_index,
          header,
          membership,
          direction,
        },
        output_index,
      }) -> {
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum
        let _own, name, _prover, _prior, next_hash, next_data <- continue(
          computation_thread_policy,
          step_datum,
          input_index,
          next_index,
          own_ref,
          inputs,
          outputs,
        )
        let subject = substrate.bind_forced_subject_to_thread_v1(
          name,
          header,
          membership,
          direction,
        )
        let Pair(verified, _witness_set) = verify_native_tx_proof_source_v1(
          membership.value.tx_id,
          membership.value.source.compact_cbor,
          membership.value.source.witness_set_compact_cbor,
          membership.value.source.field_preimage_lengths_cbor,
        )
        let expected: Data = step_02.State {
          bound: bind_output_v1(subject, output_index),
          witness_set_hash: verified.tx_compact.witness_set_hash,
        }
        and {
          next_hash == step_02_hash,
          next_data == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}
