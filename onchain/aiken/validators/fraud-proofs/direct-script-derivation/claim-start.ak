use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/direct_script_derivation/claim_mint
use midgard/fraud_proofs/direct_script_derivation/claim_receive
use midgard/fraud_proofs/direct_script_derivation/claim_start as claim

validator main(
  claim_adjudicate_hash: ScriptHash,
  spend_start_hash: ScriptHash,
  mint_grammar_hash: ScriptHash,
  receive_grammar_hash: ScriptHash,
  thread_policy: PolicyId,
  certificate_policy: PolicyId,
) {
  spend(
    datum: Option<claim.StartDatum>,
    redeemer: claim.StartRedeemer,
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
        let Pair(input_index, output_index) = claim.start_indices_v1(action)
        expect Some(step_datum) = datum
        let _, _, _, input_state, output_hash, output_state <- continue(
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
        let Pair(next_hash, expected) = when action is {
          claim.Complete { .. } -> {
            let next: Data = claim.complete_v1(state)
            Pair(claim_adjudicate_hash, next)
          }
          claim.Spend { .. } -> {
            let next: Data = state
            Pair(spend_start_hash, next)
          }
          claim.SpendOutOfRange { opening, .. } -> {
            let next: Data = claim.spend_out_of_range_v1(
              state,
              opening,
              tx.reference_inputs,
              certificate_policy,
            )
            Pair(claim_adjudicate_hash, next)
          }
          claim.Observe { opening, .. } -> {
            let next: Data = claim.observe_v1(
              state,
              opening,
              tx.reference_inputs,
              certificate_policy,
            )
            Pair(claim_adjudicate_hash, next)
          }
          claim.Mint { opening, .. } -> {
            let next: Data = claim_mint.initial_v1(
              state,
              opening,
              tx.reference_inputs,
              certificate_policy,
            )
            Pair(mint_grammar_hash, next)
          }
          claim.Receive { opening, .. } -> {
            let next: Data = claim_receive.initial_v1(
              state,
              opening,
              tx.reference_inputs,
              certificate_policy,
            )
            Pair(receive_grammar_hash, next)
          }
        }
        and {
          output_hash == next_hash,
          output_state == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}
