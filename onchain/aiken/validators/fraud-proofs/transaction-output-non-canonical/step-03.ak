use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/transaction_output_non_canonical/rule.{
  OutputScanStateV1, advance_scan_v1, open_datum_v1, outcome_scanning,
}
use midgard/fraud_proofs/transaction_output_non_canonical/step_03.{
  Args, Datum, SpendRedeemer,
}

fn output_state(state: OutputScanStateV1) -> Data {
  let value: Data = state
  value
}

validator main(
  step_03_datum_validator_script_hash: ScriptHash,
  step_04_validator_script_hash: ScriptHash,
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
      ct.Continue(Args { input_index, output_index, window }) -> {
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum
        let own_hash, _name, _prover, input_state, next_hash, next_state <- continue(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          inputs,
          outputs,
          tx.extra_signatories,
        )
        expect Some(state) = input_state
        let next = advance_scan_v1(state, window)
        if next.outcome == outcome_scanning {
          and {
            next_hash == own_hash,
            next_state == output_state(next),
          }
        } else {
          when open_datum_v1(next) is {
            Some(datum) -> {
              let value: Data = datum
              and {
                next_hash == step_03_datum_validator_script_hash,
                next_state == value,
              }
            }
            None -> and {
                next_hash == step_04_validator_script_hash,
                next_state == output_state(next),
              }
          }
        }
      }
    }
  }

  else(_) {
    fail
  }
}
