use aiken/builtin
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/cek_context_chain as chain
use midgard/cek_context_finalization as finalization
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/script_proof_v1
use midgard/validation_machine/machine_types.{CekRedeemerContextControlV1}
use midgard/validation_machine/witness as witness
use midgard/validation_merkle_v1

validator main(
  summary_script_hashes: List<ScriptHash>,
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
        expect staged.context.stage == 10
        expect chain.auxiliary_is_bound(staged.bound, action.auxiliary)
        let context = staged.context
        let native = staged.native
        let midgard = context.language_tag == 128
        let redeemers = if !midgard && context.purpose_kind == 0 {
          expect
              Pair(20,
              [
                redeemers_data, index_data, key_data, descriptor_data,
                siblings_data,
              ])
            = builtin.un_constr_data(action.auxiliary)
          expect index: Int = index_data
          expect key: ByteArray = key_data
          expect descriptor: ByteArray = descriptor_data
          expect siblings: List<ByteArray> = siblings_data
          expect index == context.purpose_index && key == context.subject
          expect
            validation_merkle_v1.verify_membership(
              native.resolved_input_count,
              native.resolved_item_peaks,
              index,
              script_proof_v1.resolved_context_item_leaf_hash(
                0,
                index,
                key,
                descriptor,
              ),
              siblings,
            )
          expect result: CekRedeemerContextControlV1 = redeemers_data
          result
        } else {
          expect Pair(19, [redeemers_data]) = builtin.un_constr_data(
            action.auxiliary,
          )
          expect result: CekRedeemerContextControlV1 = redeemers_data
          result
        }
        expect and {
            witness.cek_redeemer_context_control_is_well_formed(
              native.redeemer_count,
              redeemers,
            ),
            witness.hash_cek_redeemer_context_control_v1(redeemers) == context.redeemer_context_control_hash,
            redeemers.cursor == native.redeemer_count,
            redeemers.active_scan_hash == #"",
            bytearray.length(redeemers.current_redeemer.root) == 32,
          }
        let route = if midgard {
          4
        } else {
          context.purpose_kind
        }
        expect Some(next_hash) = list.at(summary_script_hashes, route)
        let staged_data: Data = staged
        let next: Data = finalization.FinalizationPending {
          staged: staged_data,
          redeemer_items: redeemers.map_items,
          redeemer: redeemers.current_redeemer,
        }
        continue(
          computation_thread_policy_id,
          step,
          action.input_index,
          action.output_index,
          own_out_ref,
          tx.inputs,
          tx.outputs,
          fn(_, _, _, _, hash, state) { hash == next_hash && state == next },
        )
      }
    }
  }

  else(_) {
    fail
  }
}
