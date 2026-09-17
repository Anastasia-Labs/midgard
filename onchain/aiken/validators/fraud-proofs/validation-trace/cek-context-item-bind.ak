use aiken/builtin
use aiken/cbor
use aiken/crypto.{ScriptHash, blake2b_256}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/cek_context_chain as chain
use midgard/cek_context_item.{ItemRedeemer}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/redeemer_item_proof_v1 as item
use midgard/script_proof_v1
use midgard/script_sources_redeemer_normalization_v1.{CekContextItemStepPending}
use midgard/validation_machine/machine_types.{CekRedeemerContextControlV1}
use midgard/validation_machine/shared.{purpose_kind_to_redeemer_tag}
use midgard/validation_machine/witness.{
  cek_redeemer_context_control_is_well_formed,
  hash_cek_redeemer_context_control_v1,
}

validator main(
  item_entry_script_hash: ScriptHash,
  computation_thread_policy_id: PolicyId,
) {
  spend(
    datum: Option<chain.StagedDatum>,
    redeemer: ItemRedeemer,
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
        expect chain.auxiliary_is_bound(staged.bound, action.auxiliary)
        expect Pair(18, [redeemers_data,
              control_data,
              witness_data]) = builtin.un_constr_data(action.auxiliary)
        expect control: item.RedeemerItemProofControlV1 = control_data
        let leaf = script_proof_v1.redeemer_item_leaf_hash(
          control.item_index,
          control.item_commitment,
        )
        expect control.item_count == native.redeemer_count
        let expected_hash = if context.stage == 0 {
          expect Pair(1, []) = builtin.un_constr_data(redeemers_data)
          expect control.mode == item.mode_descriptor
          expect
            control.expected_purpose_tag == purpose_kind_to_redeemer_tag(
              context.purpose_kind,
            )
          expect control.expected_pointer_index == context.purpose_index
          expect leaf == context.redeemer_leaf
          context.redeemer_context_control_hash
        } else {
          expect context.stage == 9
          expect Pair(0, [current_data]) = builtin.un_constr_data(
            redeemers_data,
          )
          expect current: CekRedeemerContextControlV1 = current_data
          expect
            cek_redeemer_context_control_is_well_formed(
              native.redeemer_count,
              current,
            )
          expect
            hash_cek_redeemer_context_control_v1(current) == context.redeemer_context_control_hash
          expect
            control.item_index == native.redeemer_count - current.cursor - 1
          expect leaf == current.active_redeemer_leaf
          current.active_scan_hash
        }
        expect item.hash_control_v1(control) == expected_hash
        let staged_data: Data = staged
        let next: Data = CekContextItemStepPending {
          staged: staged_data,
          item_control: control,
          item_witness_hash: blake2b_256(cbor.serialise(witness_data)),
          claimed_next: action.claimed_next,
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
            hash == item_entry_script_hash && state == next
          },
        )
      }
    }
  }

  else(_) {
    fail
  }
}
