use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, FieldOpeningV1, opened_field_view,
}
use midgard/fraud_proofs/value_not_preserved/union as conservation
use midgard/fraud_proofs/value_not_preserved/union.{FoldState}
use midgard/fraud_proofs/value_not_preserved/union_state.{
  FieldGrammar, Inputs, SelectedInput,
}
use midgard/native_tx_machine_walk_v1.{spend_input_at, spend_input_count}

pub type Args {
  input_index: Int,
  output_index: Int,
  opening: FieldOpeningV1,
}

validator main(
  input_value_hash: ByteArray,
  field_grammar_hash: ByteArray,
  field_certificate_policy_id: PolicyId,
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
        let own_hash, _thread_name, _, input_state, output_hash, output_state <- continue(
          computation_thread_token_policy_id,
          step_datum,
          args.input_index,
          args.output_index,
          own_out_ref,
          tx.inputs,
          tx.outputs,
        )
        expect Some(state) = input_state
        expect cursor: Inputs = state.continuation
        let view = opened_field_view(
          args.opening,
          BodyAnchor {
            tx_id: state.balance.transaction_id,
            source_kind: if state.balance.claim == conservation.ForcedConservation {
              1
            } else {
              0
            },
          },
          0,
          tx.reference_inputs,
          field_certificate_policy_id,
        )
        let count = spend_input_count(view)
        expect cursor.cursor >= 0 && cursor.cursor <= count
        if cursor.cursor < count {
          let selected: Data = SelectedInput {
            input: spend_input_at(view, cursor.cursor),
            cursor: cursor.cursor,
            selector_hash: own_hash,
          }
          let expected: Data = FoldState { ..state, continuation: selected }
          output_hash == input_value_hash && output_state == expected
        } else {
          let grammar: Data = FieldGrammar {
            field_index: 2,
            checkpoint_hash: None,
          }
          let expected: Data = FoldState { ..state, continuation: grammar }
          output_hash == field_grammar_hash && output_state == expected
        }
      }
    }
  }

  else(_) {
    fail
  }
}
