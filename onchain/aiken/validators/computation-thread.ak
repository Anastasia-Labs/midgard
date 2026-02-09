use aiken/collection/list
use aiken/primitive/bytearray
use aiken_design_patterns/linked_list/ordered as ordered
use aiken_design_patterns/linked_list/unordered as unordered
use cardano/assets.{PolicyId, flatten, quantity_of}
use cardano/transaction.{InlineDatum, Input, Output, Transaction}
use midgard/common/utils.{has_signed, is_output_to_sc}
use midgard/computation_thread.{
  BurnForCancellation, Init, MintRedeemer, StepDatum, Success,
}
use midgard/fraud_proof_catalogue as fpc
use midgard/hub_oracle as hub
use midgard/ledger_state

validator mint(
  fraud_proof_catalogue_script_hash: ByteArray,
  hub_oracle_script_hash: PolicyId,
) {
  mint(redeemer: MintRedeemer, policy_id: PolicyId, self: Transaction) {
    when redeemer is {
      Init {
        fraud_proof_set_node_ref_input_index,
        hub_oracle_ref_input_index,
        frauded_state_queue_node_ref_input_index,
        output_to_init_step_index,
        fraud_prover_hash,
        fraud_proof_catalogue_asset_name,
        state_queue_asset_name,
      } -> {
        expect Some(fraud_proof_catalogue_node_input) =
          list.at(self.reference_inputs, fraud_proof_set_node_ref_input_index)
        expect Some(frauded_state_queue_node_input) =
          list.at(
            self.reference_inputs,
            frauded_state_queue_node_ref_input_index,
          )
        expect Some(output_to_init_step) =
          list.at(self.outputs, output_to_init_step_index)

        let hub.Datum { state_queue: state_queue_policy_id, .. } =
          hub.get_datum(
            self.reference_inputs,
            hub_oracle_script_hash,
            hub_oracle_ref_input_index,
          )

        expect InlineDatum(fraud_proof_catalogue_node_datum) =
          fraud_proof_catalogue_node_input.output.datum
        expect parsed_fraud_proof_catalogue_node_datum: ordered.NodeDatum =
          fraud_proof_catalogue_node_datum
        expect parsed_fraud_proof_catalogue_node_app_data: fpc.Datum =
          parsed_fraud_proof_catalogue_node_datum.data
        expect ordered.Key { key: parsed_fraud_proof_catalogue_node_datum_key } =
          parsed_fraud_proof_catalogue_node_datum.key
        expect InlineDatum(frauded_state_queue_node_datum) =
          frauded_state_queue_node_input.output.datum
        expect parsed_frauded_state_queue_node_datum: unordered.NodeDatum =
          frauded_state_queue_node_datum
        expect parsed_frauded_state_queue_node_app_data: ledger_state.Header =
          parsed_frauded_state_queue_node_datum.data

        let token_to_mint_asset_name =
          bytearray.concat(
            parsed_fraud_proof_catalogue_node_datum_key,
            // TODO: `Header` does not contain the block hash. Which one should
            // change?
            // parsed_frauded_state_queue_node_app_data.block_hash,
            parsed_frauded_state_queue_node_app_data.utxos_root,
          )
        and {
          quantity_of(self.mint, policy_id, token_to_mint_asset_name) == 1,
          utils.quantity_of_policy_id(self.mint, policy_id) == 1,
          validate_fraud_proof_catalogue_node_input(
            fraud_proof_catalogue_node_input,
            fraud_proof_catalogue_script_hash,
            fraud_proof_catalogue_asset_name,
          ),
          validate_frauded_state_queue_node_input(
            frauded_state_queue_node_input,
            state_queue_policy_id,
            state_queue_asset_name,
          ),
          validate_output_to_initial_fraud_proof_step(
            output_to_init_step,
            fraud_prover_hash,
            parsed_fraud_proof_catalogue_node_app_data.init_step_script_hash,
            policy_id,
            token_to_mint_asset_name,
          ),
          fraud_prover_hash |> has_signed(self.extra_signatories),
        }
      }
      Success { burning_token_asset_name } -> {
        // 1. Specified token must be burned. The rest of validations
        //    (including the coupling of this endpoint with minting of a
        //    `fraud_proof` token) are done in each fraud proof's last step.
        expect quantity_of(self.mint, policy_id, burning_token_asset_name) == -1

        // Done.
        True
      }
      BurnForCancellation { burning_token_asset_name } -> {
        // 1. Transaction must burn the specified token, without minting or
        //    burning any other tokens.
        let expected_mint =
          assets.from_asset(policy_id, burning_token_asset_name, -1)
        expect expected_mint == self.mint

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}

fn validate_fraud_proof_catalogue_node_input(
  fraud_proof_catalogue_node_input: Input,
  fraud_proof_catalogue_script_hash: ByteArray,
  fraud_proof_asset_name: ByteArray,
) {
  quantity_of(
    fraud_proof_catalogue_node_input.output.value,
    fraud_proof_catalogue_script_hash,
    fraud_proof_asset_name,
  ) == 1
}

fn validate_frauded_state_queue_node_input(
  frauded_state_queue_node_input: Input,
  state_queue_script_hash: ByteArray,
  state_queue_asset_name: ByteArray,
) {
  quantity_of(
    frauded_state_queue_node_input.output.value,
    state_queue_script_hash,
    state_queue_asset_name,
  ) == 1
}

fn validate_output_to_initial_fraud_proof_step(
  output_to_init_step: Output,
  fraud_prover_hash: ByteArray,
  init_step_script_hash: ByteArray,
  own_hash: ByteArray,
  token_asset_name: ByteArray,
) {
  expect InlineDatum(outputDatum) = output_to_init_step.datum
  expect parsedDatum: StepDatum<Data> = outputDatum

  let newDatum = StepDatum { fraud_prover: fraud_prover_hash, data: None }

  let correctValue = and {
      quantity_of(output_to_init_step.value, own_hash, token_asset_name) == 1,
      list.length(flatten(output_to_init_step.value)) == 2,
    }
  and {
    is_output_to_sc(output_to_init_step, init_step_script_hash),
    parsedDatum == newDatum,
    correctValue,
  }
}
