use aiken/builtin
use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/cek_context_chain as chain
use midgard/cek_context_item.{ItemValueResult}
use midgard/cek_context_redeemer as redeemers
use midgard/cek_data_v1
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/redeemer_item_proof_v1 as item
use midgard/script_sources_redeemer_normalization_v1.{
  CekContextItemStepVerified,
}
use midgard/validation_machine/machine_types.{CekRedeemerContextControlV1}
use midgard/validation_machine/witness.{empty_data_summary_v1}

type VerifiedDatum =
  ct.StepDatum<ItemValueResult>

validator main(
  settle_script_hash: ScriptHash,
  computation_thread_policy_id: PolicyId,
) {
  spend(
    datum: Option<VerifiedDatum>,
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
        expect Some(result) = step.data
        expect verified: CekContextItemStepVerified = result.verified
        expect staged: chain.ContextStaged = verified.staged
        expect chain.auxiliary_is_bound(staged.bound, action.auxiliary)
        let context = staged.context
        let next_item = verified.next
        expect context.stage == 9
        expect Pair(18, [redeemers_data,
              control_data, _]) = builtin.un_constr_data(action.auxiliary)
        expect Pair(0, [current_data]) = builtin.un_constr_data(redeemers_data)
        expect current: CekRedeemerContextControlV1 = current_data
        expect old_item: item.RedeemerItemProofControlV1 = control_data
        if next_item.mode == item.mode_descriptor {
          expect old_item.expected_purpose_tag == 6
          expect context.language_tag == 3
          expect current.active_purpose == empty_data_summary_v1()
        } else {
          expect bytearray.length(current.active_purpose.root) == 32
        }
        expect
          next_item.stage == item.stage_terminal && next_item.mode != item.mode_descriptor
        let summary = result.summary
        let next_redeemers = CekRedeemerContextControlV1 {
          cursor: current.cursor + 1,
          map_items: cek_data_v1.prepend_data_pair_summary_v1(
            current.active_purpose,
            summary,
            current.map_items,
          ),
          active_scan_hash: #"",
          active_redeemer_leaf: #"",
          active_purpose: empty_data_summary_v1(),
          current_redeemer: if current.active_redeemer_leaf == context.redeemer_leaf {
            summary
          } else {
            current.current_redeemer
          },
        }
        let next_context = redeemers.context_successor(staged, next_redeemers)
        let next: Data = chain.ContextVerified {
          staged: verified.staged,
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
