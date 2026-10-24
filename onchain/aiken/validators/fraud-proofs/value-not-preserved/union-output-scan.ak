use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/value_not_preserved/union.{BalanceState, FoldState}
use midgard/fraud_proofs/value_not_preserved/union_field_chunks.{ChunkCarriage} as chunks
use midgard/fraud_proofs/value_not_preserved/union_output_scan
use midgard/fraud_proofs/value_not_preserved/union_state.{
  AssetCursor, FieldCursor, OutputScan,
}
use midgard/ledger_output_scan_v1
use midgard/validation_merkle_v1

pub type Args {
  input_index: Int,
  output_index: Int,
  carriage: ChunkCarriage,
  budget: Int,
}

validator main(
  union_assets_hash: ByteArray,
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
        expect scan: OutputScan = state.continuation
        expect args.budget > 0 && args.budget <= 4
        let view = chunks.open(
          args.carriage,
          tx.reference_inputs,
          scan.item.field_chunk_hashes,
          scan.item.field_total_length,
        )
        let control = union_output_scan.advance(
          view,
          scan.item.offset,
          scan.item.length,
          scan.control,
          args.budget,
        )
        if ledger_output_scan_v1.terminal_is_exact_v1(control, scan.item.length) {
          let next: Data = FieldCursor {
            field_index: 2,
            checkpoint_hash: scan.item.next_checkpoint_hash,
            grammar_script_hash: scan.item.grammar_script_hash,
          }
          let context: Data = AssetCursor {
            count: control.asset_count,
            frontier_commitment: validation_merkle_v1.frontier_commitment(
              control.asset_count,
              control.asset_peaks,
            ),
            cursor: 0,
            quantity_sign: -1,
            next_script_hash: scan.item.selector_hash,
            next_continuation: next,
          }
          let expected: Data = FoldState {
            balance: BalanceState {
              ..state.balance,
              lovelace_delta: state.balance.lovelace_delta - control.lovelace,
            },
            continuation: context,
          }
          output_hash == union_assets_hash && output_state == expected
        } else {
          let context: Data = OutputScan { ..scan, control }
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
