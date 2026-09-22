use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/value_not_preserved/step_01.{
  fraud_category_is_value_not_preserved_v1,
}
use midgard/fraud_proofs/value_not_preserved/union.{
  AssetDeltaWitness, FoldState, terminal_claim_holds,
}
use midgard/fraud_proofs/value_not_preserved/union_state.{Complete}

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
  witness: Option<AssetDeltaWitness>,
}

validator main(
  fraud_proof_token_policy_id: PolicyId,
  fraud_proof_token_address: Address,
  computation_thread_token_policy_id: PolicyId,
) {
  spend(
    datum: Option<ct.StepDatum<FoldState>>,
    redeemer: ct.StepRedeemer<Args>,
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
      ct.Continue(args) -> {
        expect Some(step_datum) = datum
        let _, thread_name, _, input_state <- finalize(
          computation_thread_token_policy_id,
          fraud_proof_token_policy_id,
          fraud_proof_token_address,
          step_datum,
          args.input_index,
          args.output_index,
          args.fraud_proof_mint_redeemer_index,
          own_out_ref,
          tx.inputs,
          tx.outputs,
          tx.redeemers,
        )
        expect Some(state) = input_state
        expect Complete = state.continuation
        expect fraud_category_is_value_not_preserved_v1(thread_name)
        terminal_claim_holds(state.balance, args.witness)
      }
    }
  }

  else(_) {
    fail
  }
}
