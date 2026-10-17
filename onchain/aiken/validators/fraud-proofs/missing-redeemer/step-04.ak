use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/missing_redeemer/claim
use midgard/fraud_proofs/missing_redeemer/rule
use midgard/fraud_proofs/missing_redeemer/step_04 as door

validator main(
  claim_start_hash: ScriptHash,
  computation_thread_policy: PolicyId,
  field_preimage_certificate_policy: PolicyId,
) {
  spend(
    datum: Option<door.Datum>,
    redeemer: door.SpendRedeemer,
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
      ct.Continue(action) -> {
        let Pair(input_index, output_index) = when action is {
          door.Scan { args } -> Pair(args.input_index, args.output_index)
          door.Finish { input_index, output_index } ->
            Pair(input_index, output_index)
        }
        expect Some(step_datum) = datum
        let own_hash, _, _, input_state, output_hash, output_state <- continue(
          computation_thread_policy,
          step_datum,
          input_index,
          output_index,
          own_ref,
          tx.inputs,
          tx.outputs,
        )
        expect Some(state) = input_state
        when action is {
          door.Scan { args } -> {
            let expected: Data = rule.advance_v1(
              state,
              args,
              tx.reference_inputs,
              field_preimage_certificate_policy,
            )
            and {
              output_hash == own_hash,
              output_state == expected,
            }
          }
          door.Finish { .. } -> {
            let expected: Data = claim.observed_context_v1(
              rule.decision_v1(state),
            )
            and {
              output_hash == claim_start_hash,
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
