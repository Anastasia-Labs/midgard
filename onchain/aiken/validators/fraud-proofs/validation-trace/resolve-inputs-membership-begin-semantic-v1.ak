use aiken/crypto.{ScriptHash}
use aiken/merkle_patricia_forestry.{Proof}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel}
use midgard/validation_machine_v1.{
  ScheduledLedgerMembershipWitness, SignerSetProofV1, ValidationOneStepWitnessV1,
  verify_resolve_inputs_membership_begin_semantics_v1,
}
use midgard/validation_semantic_v1.{Datum, continue_winning}
use midgard/validation_trace_v1.{ResolveInputs}

pub type ActionV1 {
  VerifyMembershipBegin {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    source_kind: Int,
    key: ByteArray,
    next_schedule_hash: ByteArray,
    descriptor_cbor: ByteArray,
    proof: Proof,
    signer_proof: SignerSetProofV1,
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
      ct.Continue(VerifyMembershipBegin {
        input_index,
        output_index,
        transition,
        source_kind,
        key,
        next_schedule_hash,
        descriptor_cbor,
        proof,
        signer_proof,
      }) -> {
        let auxiliary = ScheduledLedgerMembershipWitness {
          source_kind,
          key,
          next_schedule_hash,
          value: descriptor_cbor,
          proof,
          signer_proof,
        }
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
          verify_resolve_inputs_membership_begin_semantics_v1(
            datum |> semantic_pre_state,
            transition,
            source_kind,
            key,
            next_schedule_hash,
            descriptor_cbor,
            proof,
            signer_proof,
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
