use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{
  cancel, continue, pass_native_tx_to_next_step_carried,
}
use midgard/fraud_proofs/output_reference_script_decoding/rule.{
  BoundOutputV1, bind_output_v1,
}
use midgard/fraud_proofs/output_reference_script_decoding/step_01.{
  AcceptedSource, Args, Datum, ForcedSource, SpendRedeemer,
}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate

fn output_state(bound: BoundOutputV1) -> Data {
  let value: Data = bound
  value
}

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
            _state,
            next_hash,
            next_state,
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
        and {
          next_hash == step_02_hash,
          next_state == output_state(bound),
        }
      }
      ct.Continue(Args {
        source: ForcedSource {
          input_index,
          output_index: next_output_index,
          header,
          membership,
          direction,
        },
        output_index,
      }) -> {
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum
        let _own, name, _prover, _state, next_hash, next_state <- continue(
          computation_thread_policy,
          step_datum,
          input_index,
          next_output_index,
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
        let bound = bind_output_v1(subject, output_index)
        and {
          next_hash == step_02_hash,
          next_state == output_state(bound),
        }
      }
    }
  }

  else(_) {
    fail
  }
}
