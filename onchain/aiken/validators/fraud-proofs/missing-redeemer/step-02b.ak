use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/missing_redeemer/rule.{
  authenticate_stage_ten_selection_v1,
}
use midgard/fraud_proofs/missing_redeemer/step_02b.{Datum, SpendRedeemer}
use midgard/fraud_proofs/missing_redeemer/step_03.{Ready}

validator main(step_03_hash: ScriptHash, computation_thread_policy: PolicyId) {
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
      ct.Continue(args) -> {
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum
        let _, _, _, input_state, next_hash, next_state <- continue(
          computation_thread_policy,
          step_datum,
          args.input_index,
          args.output_index,
          own_ref,
          inputs,
          outputs,
        )
        expect Some(stage_ten) = input_state
        let expected: Data = Ready {
          authenticated: authenticate_stage_ten_selection_v1(
            stage_ten,
            args.absolute_purpose_index,
            args.purpose_siblings,
            args.source_origin_kind,
            args.source_key,
            args.source_language_tag,
            args.source_script_hash,
            args.source_total_length,
            args.source_item_commitment,
            args.source_siblings,
          ),
        }
        and {
          next_hash == step_03_hash,
          next_state == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}
