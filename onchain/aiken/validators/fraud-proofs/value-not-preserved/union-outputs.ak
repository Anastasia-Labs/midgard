use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, FieldOpeningV1, resume_opened_field_walk,
}
use midgard/fraud_proofs/value_not_preserved/union.{FoldState}
use midgard/fraud_proofs/value_not_preserved/union_field_chunks.{
  authenticated_chunk_hashes,
}
use midgard/fraud_proofs/value_not_preserved/union_state.{
  FieldCursor, FieldGrammar, OutputItem, OutputScan,
}
use midgard/ledger_output_scan_v1
use midgard/native_tx_field_access_v1.{field_total_length}
use midgard/native_tx_machine_walk_v1.{
  field_walk_checkpoint_hash, walk_is_complete, walk_next_extent,
  walk_next_item_index,
}

pub type Args {
  input_index: Int,
  output_index: Int,
  opening: FieldOpeningV1,
  checkpoint_bytes: ByteArray,
}

validator main(
  output_scan_hash: ByteArray,
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
        expect cursor: FieldCursor = state.continuation
        expect cursor.field_index == 2
        let Pair(view, checkpoint) = resume_opened_field_walk(
          args.opening,
          BodyAnchor { tx_id: state.balance.transaction_id },
          2,
          cursor.checkpoint_hash,
          args.checkpoint_bytes,
          tx.reference_inputs,
          field_certificate_policy_id,
        )
        if walk_is_complete(checkpoint) {
          let context: Data = FieldGrammar {
            field_index: 5,
            checkpoint_hash: None,
          }
          let expected: Data = FoldState { ..state, continuation: context }
          output_hash == cursor.grammar_script_hash && output_state == expected
        } else {
          let Pair(Pair(offset, length), advanced) = walk_next_extent(
            view,
            checkpoint,
          )
          let item = OutputItem {
            field_total_length: field_total_length(view),
            field_chunk_hashes: authenticated_chunk_hashes(view),
            index: walk_next_item_index(checkpoint),
            offset,
            length,
            checkpoint_hash: cursor.checkpoint_hash,
            next_checkpoint_hash: field_walk_checkpoint_hash(advanced),
            selector_hash: own_hash,
            grammar_script_hash: cursor.grammar_script_hash,
          }
          let context: Data = OutputScan {
            item,
            control: ledger_output_scan_v1.initial_control_v1(),
          }
          let expected: Data = FoldState { ..state, continuation: context }
          output_hash == output_scan_hash && output_state == expected
        }
      }
    }
  }

  else(_) {
    fail
  }
}
