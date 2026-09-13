use aiken/builtin
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/cek_context_chain as chain
use midgard/cek_context_redeemer.{RedeemerSelectionPending}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/script_proof_v1
use midgard/validation_machine/machine_types.{CekRedeemerContextControlV1}
use midgard/validation_machine/witness.{
  cek_redeemer_context_control_is_well_formed,
  hash_cek_redeemer_context_control_v1,
}
use midgard/validation_merkle_v1

validator main(
  initialize_script_hash: ScriptHash,
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
        let native = staged.native
        expect staged.context.stage == 9
        expect chain.auxiliary_is_bound(staged.bound, action.auxiliary)
        expect
            Pair(17,
            [
              control_data, index_data, count_data, length_data, commitment_data,
              siblings_data, purpose_frontier_data, purpose_kind_data,
              purpose_index_data, script_hash_data, subject_data,
              purpose_siblings_data,
            ])
          = builtin.un_constr_data(action.auxiliary)
        expect control: CekRedeemerContextControlV1 = control_data
        expect index: Int = index_data
        expect count: Int = count_data
        expect length: Int = length_data
        expect commitment: ByteArray = commitment_data
        expect siblings: List<ByteArray> = siblings_data
        expect purpose_frontier: Int = purpose_frontier_data
        expect purpose_kind: Int = purpose_kind_data
        expect purpose_index: Int = purpose_index_data
        expect script_hash: ByteArray = script_hash_data
        expect subject: ByteArray = subject_data
        expect purpose_siblings: List<ByteArray> = purpose_siblings_data
        expect
          cek_redeemer_context_control_is_well_formed(
            native.redeemer_count,
            control,
          )
        expect
          hash_cek_redeemer_context_control_v1(control) == staged.context.redeemer_context_control_hash
        expect control.active_scan_hash == #""
        expect
          index == native.redeemer_count - control.cursor - 1 && count == native.redeemer_count
        let leaf = script_proof_v1.redeemer_item_leaf_hash(index, commitment)
        expect
          validation_merkle_v1.verify_membership(
            native.redeemer_count,
            native.redeemer_peaks,
            index,
            leaf,
            siblings,
          )
        expect purpose_frontier >= 0 && purpose_frontier < native.purpose_count
        expect
          validation_merkle_v1.verify_membership(
            native.purpose_count,
            native.purpose_peaks,
            purpose_frontier,
            script_proof_v1.purpose_leaf_hash(
              purpose_kind,
              purpose_index,
              script_hash,
              subject,
            ),
            purpose_siblings,
          )
        let staged_data: Data = staged
        let next: Data = RedeemerSelectionPending {
          staged: staged_data,
          control,
          item_index: index,
          item_count: count,
          total_length: length,
          item_commitment: commitment,
          redeemer_leaf: leaf,
          purpose_kind,
          purpose_index,
          script_hash,
          subject,
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
            hash == initialize_script_hash && state == next
          },
        )
      }
    }
  }

  else(_) {
    fail
  }
}
