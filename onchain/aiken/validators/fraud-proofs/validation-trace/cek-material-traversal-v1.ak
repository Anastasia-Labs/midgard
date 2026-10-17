use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/cek_material_traversal as material
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/state_queue_yield.{require_authenticated_zero_yield}
use midgard/validation_resolution_v1.{winning_resolution}

validator main(
  award_script_hash: ScriptHash,
  computation_thread_policy_id: PolicyId,
  reference_script_auth_policy_id: PolicyId,
) {
  spend(
    datum: Option<material.Datum>,
    redeemer: material.SpendRedeemer,
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
        expect Some(state) = step.data
        let next = material.advance(state, action)
        let role = if action.task.kind >= 0 && action.task.kind <= 3 {
          material.program_role
        } else {
          material.data_role
        }
        expect _yield = require_authenticated_zero_yield(
          tx.reference_inputs,
          tx.withdrawals,
          tx.redeemers,
          reference_script_auth_policy_id,
          role,
          action.yield_reference_input_index,
        )
        continue(
          computation_thread_policy_id,
          step,
          action.input_index,
          action.output_index,
          own_out_ref,
          tx.inputs,
          tx.outputs,
          fn(input_hash, _, _, _, output_hash, output_state) {
            if next.pending_root == material.empty_stack {
              let award: Data = winning_resolution()
              and {
                next.node_count == next.expected_node_count,
                next.byte_length == next.expected_byte_length,
                output_hash == award_script_hash,
                output_state == award,
              }
            } else {
              let expected: Data = next
              output_hash == input_hash && output_state == expected
            }
          },
        )
      }
    }
  }

  else(_) {
    fail
  }
}
