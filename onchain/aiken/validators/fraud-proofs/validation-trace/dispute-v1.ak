use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proof_catalogue
use midgard/fraud_proofs/common.{ProverOnly, cancel, continue_validation_trace}
use midgard/hub_oracle as hub
use midgard/state_queue
use midgard/validation_claim_v1
use midgard/validation_dispute_v1
use midgard/validation_game_v1
use midgard/validation_trace_v1

pub type Datum =
  ct.StepDatum<Data>

pub type OpenActionV1 {
  Open {
    input_index: Int,
    output_index: Int,
    hub_ref_input_index: Int,
    state_queue_node_ref_input_index: Int,
    claim: validation_claim_v1.ValidationClaimWitnessV1,
    challenger_descriptor: validation_trace_v1.ValidationTraceDescriptorV1,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<OpenActionV1>

/// First step of the V1 validation dispute. Expensive block/claim
/// authentication happens once here; midpoint responses execute at the
/// smaller game validator.
validator main(
  source_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  hub_oracle: PolicyId,
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
          computation_thread_token_policy_id,
          datum,
          input_index,
          own_out_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(Open {
        input_index,
        output_index,
        hub_ref_input_index,
        state_queue_node_ref_input_index,
        claim,
        challenger_descriptor,
      }) -> {
        let Transaction { reference_inputs, .. } = tx
        let hub.Datum { state_queue: state_queue_policy, .. } = hub.get_datum(
          reference_inputs,
          hub_oracle,
          hub_ref_input_index,
        )
        expect Some(step_datum) = datum
        continue_validation_trace(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          tx,
          ProverOnly,
          fn(
            _input_script_hash,
            computation_thread_token_asset_name,
            _fraud_prover,
            input_state,
            output_script_hash,
            output_state,
          ) {
            expect None = input_state
            state_queue.get_state_queue_node(
              reference_inputs,
              state_queue_policy,
              state_queue_node_ref_input_index,
              fn(node, header_hash) {
                let header = state_queue.decode_header_view(node.header)
                let expected_state: Data = validation_game_v1.PendingValidationClaimV1 {
                  challenged_header_hash: header_hash,
                  challenged_header: header,
                  claim,
                  challenger_descriptor,
                  block_end: node.block_time,
                }
                and {
                  output_script_hash == source_validator_script_hash,
                  header_hash == bytearray.drop(
                    computation_thread_token_asset_name,
                    fraud_proof_catalogue.id_byte_count,
                  ),
                  validation_claim_v1.committed_claim_structure_is_valid(
                    header,
                    claim,
                  ),
                  validation_dispute_v1.descriptors_can_dispute(
                    claim.descriptor_membership.value,
                    challenger_descriptor,
                  ),
                  output_state == expected_state,
                }
              },
            )
          },
        )
      }
    }
  }

  else(_) {
    fail
  }
}
