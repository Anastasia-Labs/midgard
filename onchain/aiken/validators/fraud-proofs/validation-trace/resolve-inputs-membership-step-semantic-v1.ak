use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel}
use midgard/ledger_output_proof_v1.{LedgerOutputProofWitnessV1}
use midgard/validation_machine_v1.{
  LedgerOutputProofStepWitness, ValidationOneStepWitnessV1,
  verify_resolve_inputs_membership_step_semantics_v1,
}
use midgard/validation_semantic_v1.{Datum, continue_winning}
use midgard/validation_trace_v1.{ResolveInputs}

pub type ActionV1 {
  VerifyMembershipStep {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    proof_witness: LedgerOutputProofWitnessV1,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<ActionV1>

validator main(
  award_script_hash: ScriptHash,
  computation_thread_policy_id: PolicyId,
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
      ct.Continue(VerifyMembershipStep {
        input_index,
        output_index,
        transition,
        proof_witness,
      }) -> {
        let auxiliary =
          LedgerOutputProofStepWitness { witness: proof_witness }
        let auxiliary_data: Data = auxiliary
        continue_winning(
          ResolveInputs,
          award_script_hash,
          computation_thread_policy_id,
          datum,
          input_index,
          output_index,
          transition,
          auxiliary_data,
          verify_resolve_inputs_membership_step_semantics_v1(
            datum |> semantic_pre_state,
            transition,
            proof_witness,
          ),
          own_out_ref,
          tx,
        )
      }
    }
  }

  else(_) {
    fail
  }
}

fn semantic_pre_state(datum: Option<Datum>) {
  expect Some(ct.StepDatum { data: Some(state), .. }) = datum
  state.resolution.pre_state
}
