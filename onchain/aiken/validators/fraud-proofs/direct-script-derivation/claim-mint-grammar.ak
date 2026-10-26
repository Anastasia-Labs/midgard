use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/direct_script_derivation/claim_mint as claim
use midgard/fraud_proofs/direct_script_derivation/cursor

validator main(
  scan_hash: ScriptHash,
  thread_policy: PolicyId,
  certificate_policy: PolicyId,
) {
  spend(
    datum: Option<claim.Datum>,
    redeemer: claim.GrammarRedeemer,
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
      ct.Continue(args) -> {
        let Pair(input_index, output_index) = Pair(
          args.input_index,
          args.output_index,
        )
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
        let advanced = claim.certify_v1(
          state,
          args,
          tx.reference_inputs,
          certificate_policy,
        )
        let expected: Data = advanced
        and {
          output_hash == if advanced.work.scan.cursor.phase == cursor.Walking {
            scan_hash
          } else {
            own_hash
          },
          output_state == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}
