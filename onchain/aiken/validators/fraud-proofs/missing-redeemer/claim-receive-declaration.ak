use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/direct_script_derivation/receive_declaration as declaration
use midgard/fraud_proofs/missing_redeemer/claim_receive as claim

validator main(claim_adjudicate_hash: ScriptHash, thread_policy: PolicyId) {
  spend(
    datum: Option<claim.DeclarationDatum>,
    redeemer: claim.DeclarationRedeemer,
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
        let Pair(input_index, output_index) = declaration.indices(action)
        expect Some(step_datum) = datum
        let own_hash, _, fraud_prover, input_state, output_hash, output_state <- continue(
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
          declaration.Scan { entries, .. } -> {
            // Declaration leaves are a free choice until final root equality.
            expect list.has(tx.extra_signatories, fraud_prover)
            let expected: Data = claim.advance_declaration_v1(state, entries)
            and {
              output_hash == own_hash,
              output_state == expected,
            }
          }
          declaration.Finish { .. } -> {
            let expected: Data = claim.finish_declaration_v1(state)
            and {
              output_hash == claim_adjudicate_hash,
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
