use aiken/builtin
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/cek_context_chain as chain
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/redeemer_item_proof_v1 as item
use midgard/script_proof_v1
use midgard/validation_machine/machine_types.{CekContextControlV1}
use midgard/validation_machine/shared.{purpose_kind_to_redeemer_tag}
use midgard/validation_merkle_v1

validator main(
  settle_script_hash: ScriptHash,
  computation_thread_policy_id: PolicyId,
) {
  spend(
    datum: Option<chain.StagedDatum>,
    redeemer: chain.ContextRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          computation_thread_policy_id,
          datum,
          input_index,
          own_out_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(action) -> {
        expect Some(step) = datum
        expect Some(staged) = step.data
        let context = staged.context
        let native = staged.native
        expect
          context.stage == 0 && context.redeemer_context_control_hash == #""
        expect chain.auxiliary_is_bound(staged.bound, action.auxiliary)
        expect
            Pair(9,
            [
              index_data, count_data, length_data, commitment_data,
              siblings_data,
            ])
          = builtin.un_constr_data(action.auxiliary)
        expect index: Int = index_data
        expect count: Int = count_data
        expect length: Int = length_data
        expect commitment: ByteArray = commitment_data
        expect siblings: List<ByteArray> = siblings_data
        let leaf = script_proof_v1.redeemer_item_leaf_hash(index, commitment)
        expect count == native.redeemer_count && leaf == context.redeemer_leaf
        expect
          validation_merkle_v1.verify_membership(
            native.redeemer_count,
            native.redeemer_peaks,
            index,
            leaf,
            siblings,
          )
        let control = item.initial_control_v1(
          item.mode_descriptor,
          index,
          count,
          length,
          commitment,
          purpose_kind_to_redeemer_tag(context.purpose_kind),
          context.purpose_index,
        )
        let next_context = CekContextControlV1 {
          ..context,
          redeemer_context_control_hash: item.hash_control_v1(control),
        }
        let staged_data: Data = staged
        let next: Data = chain.ContextVerified {
          staged: staged_data,
          successor: chain.ContinueContext { next: next_context },
        }
        continue(
          computation_thread_policy_id,
          step,
          action.input_index,
          action.output_index,
          own_out_ref,
          tx.inputs,
          tx.outputs,
          fn(_, _, _, _, hash, state) {
            hash == settle_script_hash && state == next
          },
        )
      }
    }
  }

  else(_) {
    fail
  }
}
