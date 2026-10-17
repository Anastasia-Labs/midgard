use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/missing_redeemer/claim_receive as claim

validator main(
  declaration_hash: ScriptHash,
  thread_policy: PolicyId,
  certificate_policy: PolicyId,
) {
  spend(
    datum: Option<claim.OutputDatum>,
    redeemer: claim.OutputRedeemer,
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
        let Pair(input_index, output_index) = claim.output_indices_v1(action)
        expect Some(step_datum) = datum
        let own_hash, _, _, input_state, output_hash, output_state <- continue(
          thread_policy,
          step_datum,
          input_index,
          output_index,
          own_ref,
          tx.inputs,
          tx.outputs,
        )
        expect Some(state) = input_state
        when action is {
          claim.Scan { opening, checkpoint_bytes, witnesses, .. } -> {
            let expected: Data = claim.advance_outputs_v1(
              state,
              opening,
              checkpoint_bytes,
              witnesses,
              tx.reference_inputs,
              certificate_policy,
            )
            and {
              output_hash == own_hash,
              output_state == expected,
            }
          }
          claim.Finish { args } -> {
            let expected: Data = claim.finish_outputs_v1(
              state,
              args,
              tx.reference_inputs,
              certificate_policy,
            )
            and {
              output_hash == declaration_hash,
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
