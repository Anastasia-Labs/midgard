use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/unused_redeemer/rule.{
  ReverseScanV1, SelectionOpeningV1, decision_v1, maximum_scan_batch,
  reverse_scan_complete_v1, scan_selection_v1,
}
use midgard/fraud_proofs/unused_redeemer/step_05.{Datum, SpendRedeemer}

validator main(step_06_hash: ScriptHash, thread_policy: PolicyId) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
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
        expect and {
            args.item_budget > 0,
            args.item_budget <= maximum_scan_batch,
          }
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum
        let own_hash, _, _, input_state, output_hash, output_state <- continue(
          thread_policy,
          step_datum,
          args.input_index,
          args.output_index,
          own_ref,
          inputs,
          outputs,
        )
        expect Some(state) = input_state
        let scanned = advance(state, args.openings, args.item_budget)
        if reverse_scan_complete_v1(scanned) {
          let expected: Data = decision_v1(scanned)
          and {
            output_hash == step_06_hash,
            output_state == expected,
          }
        } else {
          expect list.length(args.openings) == args.item_budget
          let expected: Data = scanned
          and {
            output_hash == own_hash,
            output_state == expected,
          }
        }
      }
    }
  }

  else(_) {
    fail
  }
}

fn advance(
  state: ReverseScanV1,
  openings: List<SelectionOpeningV1>,
  budget: Int,
) -> ReverseScanV1 {
  if budget == 0 || reverse_scan_complete_v1(state) {
    state
  } else {
    expect [opening, ..rest] = openings
    advance(scan_selection_v1(state, opening), rest, budget - 1)
  }
}
