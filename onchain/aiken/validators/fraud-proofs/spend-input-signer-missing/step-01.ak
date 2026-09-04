use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{
  cancel, continue, pass_native_tx_to_next_step_carried,
}
use midgard/fraud_proofs/native_tx/compact.{decode_native_tx_compact_v1}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/spend_input_signer_missing/rule.{bind_spend_input_v1}
use midgard/fraud_proofs/spend_input_signer_missing/step_01.{
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
      ct.Continue(Args { source: AcceptedSource { inclusion }, input_index }) -> {
        let _, _, _, _, next_hash, next_state, header, _, verified <- pass_native_tx_to_next_step_carried(
          thread_policy,
          hub_oracle,
          datum,
          inclusion,
          own_ref,
          tx,
        )
        let state = bind_spend_input_v1(
          substrate.bind_accepted_subject_v1(verified),
          input_index,
          header.prev_utxos_root,
          verified.tx_compact.witness_set_hash,
        )
        let expected: Data = state
        and {
          next_hash == step_02_hash,
          next_state == expected,
        }
      }
      ct.Continue(Args {
        source: ForcedSource {
          input_index: own_input_index,
          output_index,
          header,
          membership,
          direction,
        },
        input_index,
      }) -> {
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum
        let _, thread_name, _, _, next_hash, next_state <- continue(
          thread_policy,
          step_datum,
          own_input_index,
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
        let state = bind_spend_input_v1(
          subject,
          input_index,
          header.prev_utxos_root,
          decode_native_tx_compact_v1(membership.value.source.compact_cbor).witness_set_hash,
        )
        let expected: Data = state
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
