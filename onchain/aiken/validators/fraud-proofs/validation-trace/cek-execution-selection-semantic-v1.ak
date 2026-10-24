use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/cek_selection_yield as selection
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{Anyone, cancel, continue_validation_trace}
use midgard/state_queue_yield.{require_authenticated_zero_yield}
use midgard/validation_machine/witness.{
  cek_control_is_execution_selection_v1, cek_witness_control_v1,
  cek_witness_is_well_formed_v1,
}
use midgard/validation_resolution_v1 as resolution
use midgard/validation_semantic_v1.{Datum}
use midgard/validation_trace_v1.{Cek}

pub type ActionV1 =
  selection.Action

pub type SpendRedeemer =
  selection.SpendRedeemer

fn invoke(
  policy: PolicyId,
  role: ByteArray,
  index: Int,
  tx: Transaction,
) -> Bool {
  expect _hash = require_authenticated_zero_yield(
    tx.reference_inputs,
    tx.withdrawals,
    tx.redeemers,
    policy,
    role,
    index,
  )
  True
}

fn required_yields(
  action: selection.Action,
  policy: PolicyId,
  tx: Transaction,
) -> Bool {
  expect [authenticate, successor] = action.yield_ref_input_indices
  and {
    invoke(policy, selection.authenticate_role, authenticate, tx),
    invoke(policy, selection.successor_role, successor, tx),
  }
}

validator main(
  award_script_hash: ScriptHash,
  computation_thread_policy_id: PolicyId,
  reference_script_auth_policy_id: PolicyId,
) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
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
        expect Some(ct.StepDatum { data: Some(state), .. }) = datum
        let cek = cek_witness_control_v1(action.transition)
        expect Some(step_datum) = datum
        let transition_data: Data = action.transition
        let expected: Data = resolution.winning_resolution()
        continue_validation_trace(
          computation_thread_policy_id,
          step_datum,
          action.input_index,
          action.output_index,
          own_out_ref,
          tx,
          // The award successor is fixed by the thread state and evidence.
          Anyone,
          fn(_, _, _, _, output_hash, output_state) {
            and {
              resolution.prepared_resolution_is_well_formed(state),
              state.resolution.pre_state.phase == Cek,
              resolution.hash_one_step_evidence(
                transition_data,
                action.auxiliary,
              ) == state.evidence_hash,
              cek_witness_is_well_formed_v1(
                state.resolution.pre_state,
                action.transition,
                cek,
              ),
              cek_control_is_execution_selection_v1(cek),
              cek.execution_cpu_limit == 0,
              cek.execution_memory_limit == 0,
              cek.program_script_hash == #"",
              required_yields(action, reference_script_auth_policy_id, tx),
              output_hash == award_script_hash,
              output_state == expected,
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
