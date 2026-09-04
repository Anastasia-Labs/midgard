use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{
  cancel, continue, pass_native_tx_to_next_step_carried,
}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/resolved_output_non_canonical/rule.{bind_input_v1}
use midgard/fraud_proofs/resolved_output_non_canonical/step_01.{
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
      ct.Continue(Args {
        source: AcceptedSource { inclusion },
        source_kind,
        input_index,
      }) -> {
        let _, _, _, _, next_hash, next_state, header, _, verified <- pass_native_tx_to_next_step_carried(
          thread_policy,
          hub_oracle,
          datum,
          inclusion,
          own_ref,
          tx,
        )
        let state = bind_input_v1(
          substrate.bind_accepted_subject_v1(verified),
          source_kind,
          input_index,
          header.prev_utxos_root,
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
        source_kind,
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
        let state = bind_input_v1(
          subject,
          source_kind,
          input_index,
          header.prev_utxos_root,
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
