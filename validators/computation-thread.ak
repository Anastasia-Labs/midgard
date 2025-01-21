use aiken/collection/list
use aiken/primitive/bytearray
use aiken_design_patterns/linked_list/ordered.{Key, NodeDatum}
use cardano/assets.{PolicyId, flatten, quantity_of}
use cardano/transaction.{InlineDatum, Input, Output, Transaction}
use midgard/common/utils.{is_output_to_sc}
use midgard/computation_thread as ct
use midgard/computation_thread.{StepDatum}
use midgard/fraud_proof/catalogue as fpc
use midgard/hub_oracle
use midgard/ledger_state
use midgard/state_queue

validator computation_thread_policy(
  fraud_proof_catalogue_script_hash: ByteArray,
  hub_oracle_script_hash: PolicyId,
) {
  mint(redeemer: ct.Redeemer, policy_id: PolicyId, self: Transaction) {
    when redeemer is {
      ct.Mint(
        fraud_proof_set_node_ref_input_index,
        hub_oracle_ref_input_index,
        frauded_state_queue_node_ref_input_index,
        output_to_init_step_index,
        fraud_prover_hash,
        fraud_proof_catalogue_asset_name,
        hub_oracle_asset_name,
        state_queue_asset_name,
      ) -> {
        expect Some(fraud_proof_catalogue_node_input) =
          list.at(self.reference_inputs, fraud_proof_set_node_ref_input_index)
        expect Some(hub_oracle_input) =
          list.at(self.reference_inputs, hub_oracle_ref_input_index)
        expect Some(frauded_state_queue_node_input) =
          list.at(
            self.reference_inputs,
            frauded_state_queue_node_ref_input_index,
          )
        expect Some(output_to_init_step) =
          list.at(self.outputs, output_to_init_step_index)

        expect InlineDatum(hub_oracle_datum) = hub_oracle_input.output.datum
        expect parsed_hub_oracle_datum: hub_oracle.Datum = hub_oracle_datum
        let state_queue_policy_id = parsed_hub_oracle_datum.state_queue

        expect InlineDatum(fraud_proof_catalogue_node_datum) =
          fraud_proof_catalogue_node_input.output.datum
        expect parsed_fraud_proof_catalogue_node_datum: NodeDatum =
          fraud_proof_catalogue_node_datum
        expect parsed_fraud_proof_catalogue_node_app_data: fpc.Datum =
          parsed_fraud_proof_catalogue_node_datum.data
        expect Key(parsed_fraud_proof_catalogue_node_datum_key) =
          parsed_fraud_proof_catalogue_node_datum.key
        expect InlineDatum(frauded_state_queue_node_datum) =
          frauded_state_queue_node_input.output.datum
        expect parsed_frauded_state_queue_node_datum: state_queue.Datum =
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
          validate_hub_oracle_input(
            hub_oracle_input,
            hub_oracle_script_hash,
            hub_oracle_asset_name,
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
        }
      }
      ct.Success(token_to_burn_asset_name) -> and {
          quantity_of(self.mint, policy_id, token_to_burn_asset_name) == -1,
          utils.quantity_of_policy_id(self.mint, policy_id) == 1,
        }
      ct.Cancel(token_to_burn_asset_name) -> and {
          quantity_of(self.mint, policy_id, token_to_burn_asset_name) == -1,
          utils.quantity_of_policy_id(self.mint, policy_id) == 1,
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

fn validate_hub_oracle_input(
  hub_oracle_input: Input,
  hub_oracle_script_hash: ByteArray,
  hub_oracle_asset_name: ByteArray,
) {
  quantity_of(
    hub_oracle_input.output.value,
    hub_oracle_script_hash,
    hub_oracle_asset_name,
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
  expect parsedDatum: StepDatum = outputDatum

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
