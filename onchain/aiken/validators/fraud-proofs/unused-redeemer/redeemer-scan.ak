use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/unused_redeemer/walk

validator main(
  claim_start_hash: ScriptHash,
  terminal_hash: ScriptHash,
  thread_policy: PolicyId,
  certificate_policy: PolicyId,
) {
  spend(
    datum: Option<walk.WalkDatum>,
    redeemer: walk.ScanRedeemer,
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
      ct.Continue(action) -> {
        let Pair(input_index, output_index) = walk.scan_indices_v1(action)
        expect Some(step_datum) = datum
        let own_hash, _, _, input_state, output_hash, output_state <- continue(
          thread_policy,
          step_datum,
          input_index,
          output_index,
          own_ref,
          tx.inputs,
          tx.outputs,
          tx.extra_signatories,
        )
        expect Some(state) = input_state
        when action is {
          walk.Scan { args } -> {
            let expected: Data = walk.advance_v1(
              state,
              args,
              tx.reference_inputs,
              certificate_policy,
            )
            and {
              output_hash == own_hash,
              output_state == expected,
            }
          }
          walk.Finish { .. } ->
            if state.reached && !state.repeated {
              let expected: Data = walk.claim_context_v1(state)
              and {
                output_hash == claim_start_hash,
                output_state == expected,
              }
            } else {
              let expected: Data = walk.final_state_v1(state)
              and {
                output_hash == terminal_hash,
                output_state == expected,
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
