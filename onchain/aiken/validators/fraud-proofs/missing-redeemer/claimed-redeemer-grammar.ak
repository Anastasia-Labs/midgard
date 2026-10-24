use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/missing_redeemer/adjudicate

validator main(
  claimed_redeemer_scan_hash: ScriptHash,
  thread_policy: PolicyId,
  certificate_policy: PolicyId,
) {
  spend(
    datum: Option<adjudicate.ClaimedScanDatum>,
    redeemer: adjudicate.ClaimedGrammarRedeemer,
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
        expect Some(step_datum) = datum
        let own_hash, _, _, input_state, output_hash, output_state <- continue(
          thread_policy,
          step_datum,
          args.input_index,
          args.output_index,
          own_ref,
          tx.inputs,
          tx.outputs,
        )
        expect Some(state) = input_state
        let advanced = adjudicate.claimed_certify_v1(
          state,
          args,
          tx.reference_inputs,
          certificate_policy,
        )
        let expected: Data = advanced
        and {
          output_hash == if advanced.scan.scan.cursor.phase == cursor.Walking {
            claimed_redeemer_scan_hash
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
