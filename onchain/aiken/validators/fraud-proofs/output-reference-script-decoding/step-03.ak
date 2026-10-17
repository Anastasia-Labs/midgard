use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/output_reference_script_decoding/rule.{
  advance_output_scan_v1, output_canonical, output_scanning,
}
use midgard/fraud_proofs/output_reference_script_decoding/step_03.{
  Args, Datum, SpendRedeemer,
}

validator main(step_04_hash: ScriptHash, computation_thread_policy: PolicyId) {
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
      ct.Continue(Args { input_index, output_index, window }) -> {
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum
        let own_hash, _name, _prover, input_state, next_hash, next_state <- continue(
          computation_thread_policy,
          step_datum,
          input_index,
          output_index,
          own_ref,
          inputs,
          outputs,
        )
        expect Some(state) = input_state
        let advanced = advance_output_scan_v1(state, window)
        let expected_hash = if advanced.outcome == output_canonical {
          step_04_hash
        } else {
          own_hash
        }
        let expected: Data = advanced
        and {
          advanced.outcome == output_scanning || advanced.outcome == output_canonical,
          next_hash == expected_hash,
          next_state == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}
