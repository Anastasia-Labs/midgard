use aiken/builtin
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/bounded_item_v1
use midgard/cek_context_chain as chain
use midgard/cek_context_redeemer.{RedeemerSelectionPending}
use midgard/cek_context_step
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/redeemer_item_proof_v1
use midgard/script_proof_v1
use midgard/validation_machine/machine_types.{
  CekContextControlV1, CekRedeemerContextControlV1,
}
use midgard/validation_machine/witness.{hash_cek_redeemer_context_control_v1}
use midgard/validation_merkle_v1

// The redeemer fold walks the execution frontier downwards from the
// control's purpose bound. The execution leaf at `purpose_bound - 1` was
// appended with that purpose's own leaf, source and redeemer item, so it
// admits exactly one step: a select (tag 17) when it names a redeemer, a
// skip (tag 40) when it is a native execution. Nothing else in the witness
// reaches the successor except `total_length`, which the item commitment
// fixes.
validator main(
  initialize_script_hash: ScriptHash,
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
        expect staged.context.stage == 9
        expect chain.auxiliary_is_bound(staged.bound, action.auxiliary)
        let Pair(tag, fields) = builtin.un_constr_data(action.auxiliary)
        expect control: CekRedeemerContextControlV1 = builtin.head_list(fields)
        expect
          hash_cek_redeemer_context_control_v1(control) == staged.context.redeemer_context_control_hash
        expect control.active_scan_hash == #""
        let frontier = control.purpose_bound - 1
        let (execution_count, execution_peaks) = cek_context_step.execution_frontier(
          staged.bound.control,
        )
        let staged_data: Data = staged
        let (target, next) = if tag == 17 {
          expect
              [
                _, index_data, length_data, commitment_data, purpose_kind_data,
                purpose_index_data, script_hash_data, subject_data,
                language_tag_data, source_leaf_data, siblings_data,
                item_frontier_root_data,
              ]
            = fields
          expect index: Int = index_data
          expect length: Int = length_data
          expect commitment: ByteArray = commitment_data
          expect purpose_kind: Int = purpose_kind_data
          expect purpose_index: Int = purpose_index_data
          expect script_hash: ByteArray = script_hash_data
          expect subject: ByteArray = subject_data
          expect language_tag: Int = language_tag_data
          expect source_leaf: ByteArray = source_leaf_data
          expect siblings: List<ByteArray> = siblings_data
          expect item_frontier_root: ByteArray = item_frontier_root_data
          let leaf = script_proof_v1.redeemer_item_leaf_hash_unchecked(
            index,
            commitment,
          )
          // [execution-leaf]
          expect
            validation_merkle_v1.verify_membership(
              execution_count,
              execution_peaks,
              frontier,
              script_proof_v1.execution_leaf_hash_unchecked(
                language_tag,
                script_proof_v1.purpose_leaf_hash_unchecked(
                  purpose_kind,
                  purpose_index,
                  script_hash,
                  subject,
                ),
                source_leaf,
                leaf,
              ),
              siblings,
            )
          // [item-length]
          expect
            bounded_item_v1.commitment_from_frontier_root(
              redeemer_item_proof_v1.redeemer_field_index,
              index,
              length,
              item_frontier_root,
            ) == commitment
          let pending: Data = RedeemerSelectionPending {
            staged: staged_data,
            control,
            item_index: index,
            item_count: staged.native.redeemer_count,
            total_length: length,
            item_commitment: commitment,
            redeemer_leaf: leaf,
            purpose_frontier: frontier,
            purpose_kind,
            purpose_index,
            script_hash,
            subject,
          }
          (initialize_script_hash, pending)
        } else {
          expect tag == 40
          expect [_,
                purpose_leaf_data, source_leaf_data, siblings_data] = fields
          expect purpose_leaf: ByteArray = purpose_leaf_data
          expect source_leaf: ByteArray = source_leaf_data
          expect siblings: List<ByteArray> = siblings_data
          // [native-execution]
          expect
            validation_merkle_v1.verify_membership(
              execution_count,
              execution_peaks,
              frontier,
              script_proof_v1.execution_leaf_hash_unchecked(
                0,
                purpose_leaf,
                source_leaf,
                #"",
              ),
              siblings,
            )
          // A native execution takes no redeemer, so the cursor stays below
          // the redeemer count and the fold stays at stage 9.
          let verified: Data = chain.ContextVerified {
            staged: staged_data,
            successor: chain.ContinueContext {
              next: CekContextControlV1 {
                ..staged.context,
                redeemer_context_control_hash: hash_cek_redeemer_context_control_v1(
                  CekRedeemerContextControlV1 {
                    ..control,
                    purpose_bound: frontier,
                  },
                ),
              },
            },
          }
          (settle_script_hash, verified)
        }
        continue(
          computation_thread_policy_id,
          step,
          action.input_index,
          action.output_index,
          own_out_ref,
          tx.inputs,
          tx.outputs,
          fn(_, _, _, _, hash, state) { hash == target && state == next },
        )
      }
    }
  }

  else(_) {
    fail
  }
}
