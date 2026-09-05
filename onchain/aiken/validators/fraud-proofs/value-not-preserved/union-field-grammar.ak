use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, FieldOpeningV1, opened_certified_field_walk_from_grammar,
  opened_field_grammar_certification, resume_opened_field_grammar_certification,
}
use midgard/fraud_proofs/value_not_preserved/union.{FoldState}
use midgard/fraud_proofs/value_not_preserved/union_state.{
  FieldCursor, FieldGrammar, MintCursor,
}
use midgard/native_tx_machine_walk_v1.{
  certify_field_grammar, encode_field_grammar_checkpoint,
  field_grammar_checkpoint_hash, field_grammar_is_complete,
  field_walk_checkpoint_hash,
}

pub type Args {
  input_index: Int,
  output_index: Int,
  opening: FieldOpeningV1,
  checkpoint_bytes: ByteArray,
}

validator main(
  outputs_hash: ByteArray,
  mint_hash: ByteArray,
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
        expect grammar: FieldGrammar = state.continuation
        expect grammar.field_index == 2 || grammar.field_index == 5
        let anchor = BodyAnchor { tx_id: state.balance.transaction_id }
        let Pair(view, checkpoint) = when grammar.checkpoint_hash is {
          None -> {
            expect args.checkpoint_bytes == #""
            opened_field_grammar_certification(
              args.opening,
              anchor,
              grammar.field_index,
              tx.reference_inputs,
              field_certificate_policy_id,
            )
          }
          Some(hash) ->
            resume_opened_field_grammar_certification(
              args.opening,
              anchor,
              grammar.field_index,
              hash,
              args.checkpoint_bytes,
              tx.reference_inputs,
              field_certificate_policy_id,
            )
        }
        let advanced = certify_field_grammar(view, checkpoint, 16)
        let hash = field_grammar_checkpoint_hash(advanced)
        if field_grammar_is_complete(advanced) {
          let Pair(_, start) = opened_certified_field_walk_from_grammar(
            args.opening,
            anchor,
            grammar.field_index,
            hash,
            encode_field_grammar_checkpoint(advanced),
            tx.reference_inputs,
            field_certificate_policy_id,
          )
          let cursor = FieldCursor {
            field_index: grammar.field_index,
            checkpoint_hash: field_walk_checkpoint_hash(start),
            grammar_script_hash: own_hash,
          }
          if grammar.field_index == 2 {
            let context: Data = cursor
            let expected: Data = FoldState { ..state, continuation: context }
            output_hash == outputs_hash && output_state == expected
          } else {
            let context: Data = MintCursor {
              field: cursor,
              previous_policy: None,
              active: None,
            }
            let expected: Data = FoldState { ..state, continuation: context }
            output_hash == mint_hash && output_state == expected
          }
        } else {
          let context: Data = FieldGrammar {
            ..grammar,
            checkpoint_hash: Some(hash),
          }
          let expected: Data = FoldState { ..state, continuation: context }
          output_hash == own_hash && output_state == expected
        }
      }
    }
  }

  else(_) {
    fail
  }
}
