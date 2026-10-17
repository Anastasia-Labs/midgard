use aiken/merkle_patricia_forestry as mpf
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_input}
use midgard/fraud_proofs/value_not_preserved/union.{BalanceState, FoldState}
use midgard/fraud_proofs/value_not_preserved/union_state.{
  AssetCursor, Inputs, SelectedInput,
}
use midgard/ledger_output_commitment_v1
use midgard/mpf_proof_v1

pub type Args {
  input_index: Int,
  output_index: Int,
  descriptor_cbor: ByteArray,
  proof: mpf.Proof,
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
        let _own_hash, _thread_name, _, input_state, output_hash, output_state
          <- continue(
          computation_thread_token_policy_id,
          step_datum,
          args.input_index,
          args.output_index,
          own_out_ref,
          tx.inputs,
          tx.outputs,
        )
        expect Some(state) = input_state
        expect selected: SelectedInput = state.continuation
        expect
          mpf_proof_v1.has(
            state.balance.pre_utxos_root,
            encode_midgard_tx_input(selected.input),
            args.descriptor_cbor,
            args.proof,
          )
        let descriptor = ledger_output_commitment_v1.decode(
          args.descriptor_cbor,
        )
        let next: Data = Inputs { cursor: selected.cursor + 1 }
        let cursor: Data = AssetCursor {
          count: descriptor.asset_count,
          frontier_commitment: descriptor.asset_frontier_commitment,
          cursor: 0,
          quantity_sign: 1,
          next_script_hash: selected.selector_hash,
          next_continuation: next,
        }
        let expected: Data = FoldState {
          balance: BalanceState {
            ..state.balance,
            lovelace_delta: state.balance.lovelace_delta + descriptor.lovelace,
          },
          continuation: cursor,
        }
        output_hash == union_assets_hash && output_state == expected
      }
    }
  }

  else(_) {
    fail
  }
}
