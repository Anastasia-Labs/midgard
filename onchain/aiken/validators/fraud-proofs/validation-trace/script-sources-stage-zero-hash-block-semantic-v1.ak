use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/bounded_item_v1.{ChunkProofV1}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel}
use midgard/validation_machine_v1.{
  ScriptSourceHashBlockWitness, ValidationOneStepWitnessV1,
  verify_script_sources_stage_zero_hash_block_semantics_v1,
}
use midgard/validation_semantic_v1.{Datum, continue_winning}
use midgard/validation_trace_v1.{ScriptSources}

pub type ActionV1 {
  VerifyHashBlock {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    chunk_proof: ChunkProofV1,
    next_chunk_proof: Option<ChunkProofV1>,
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
      ct.Continue(VerifyHashBlock {
        input_index,
        output_index,
        transition,
        chunk_proof,
        next_chunk_proof,
      }) -> {
        let auxiliary = ScriptSourceHashBlockWitness {
          chunk_proof,
          next_chunk_proof,
        }
        let auxiliary_data: Data = auxiliary
        continue_winning(
          ScriptSources,
          award_script_hash,
          computation_thread_policy_id,
          datum,
          input_index,
          output_index,
          transition,
          auxiliary_data,
          verify_script_sources_stage_zero_hash_block_semantics_v1(
            datum |> semantic_pre_state,
            transition,
            chunk_proof,
            next_chunk_proof,
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
