use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel}
use midgard/script_sources_descriptor as descriptor
use midgard/validation_machine/machine_types.{
  RedeemerScanBeginWitness, ValidationOneStepWitnessV1,
}
use midgard/validation_machine/script_sources_late_stages as semantics
use midgard/validation_semantic_v1.{Datum, continue_winning}
use midgard/validation_semantic_yield_v1 as yielding
use midgard/validation_trace_v1.{ScriptSources}

pub type ActionV1 {
  VerifyRedeemerItemStep {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    claim: descriptor.DescriptorStepClaim,
    yield_to_ref_input_index: Int,
  }
  VerifyRedeemerScanBegin {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    item_index: Int,
    item_count: Int,
    total_length: Int,
    item_commitment: ByteArray,
    siblings: List<ByteArray>,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<ActionV1>

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
      ct.Continue(VerifyRedeemerItemStep {
        input_index,
        output_index,
        transition,
        claim,
        yield_to_ref_input_index,
      }) -> {
        expect Some(ct.StepDatum { data: Some(state), .. }) = datum
        let h = yielding.require_semantic_yield_v1(
          tx,
          reference_script_auth_policy_id,
          "V1VtSsRedeemerItemStepYield",
          yield_to_ref_input_index,
        )
        let auxiliary: Data = descriptor.auxiliary(claim)
        continue_winning(
          ScriptSources,
          award_script_hash,
          computation_thread_policy_id,
          datum,
          input_index,
          output_index,
          transition,
          auxiliary,
          bytearray.length(h) == 28 && semantics.verify_descriptor_used(
            state.resolution.pre_state,
            transition,
            claim,
          ),
          own_out_ref,
          tx,
        )
      }
      ct.Continue(VerifyRedeemerScanBegin {
        input_index,
        output_index,
        transition,
        item_index,
        item_count,
        total_length,
        item_commitment,
        siblings,
      }) -> {
        expect Some(ct.StepDatum { data: Some(state), .. }) = datum
        let auxiliary: Data = RedeemerScanBeginWitness {
          item_index,
          item_count,
          total_length,
          item_commitment,
          siblings,
        }
        continue_winning(
          ScriptSources,
          award_script_hash,
          computation_thread_policy_id,
          datum,
          input_index,
          output_index,
          transition,
          auxiliary,
          semantics.verify_descriptor_begin_twelve(
            state.resolution.pre_state,
            transition,
            item_index,
            item_count,
            total_length,
            item_commitment,
            siblings,
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
