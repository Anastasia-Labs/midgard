use aiken/collection/list
use cardano/assets.{PolicyId, flatten, from_asset, quantity_of}
use cardano/transaction.{InlineDatum, Output, OutputReference, Transaction}
use midgard/computation_thread.{StepDatum}
use midgard/fraud_proof/double_spend.{validate_output_to_fraud_prover}

pub type DoubleSpendStepLastState {
  data: Void,
}

// TODO
pub type Proof =
  Void

pub type DoubleSpendStepLastRedeemer {
  CancelLast
  ContinueLast {
    frauded_state_queue_node_ref_input_index: Int,
    unverified_frauded_state_queue_tx1_hash: ByteArray,
    unverified_frauded_state_queue_tx1_info_hash: ByteArray,
    unverified_frauded_state_queue_tx1_to_root_proof_path: Proof,
  }
}

validator double_spend_s_last(ct_token_policy_id: PolicyId) {
  spend(
    maybe_datum: Option<StepDatum>,
    redeemer: DoubleSpendStepLastRedeemer,
    input: OutputReference,
    self: Transaction,
  ) {
    expect Some(step_datum) = maybe_datum
    when redeemer is {
      CancelLast -> {
        expect Some(own_input) =
          list.find(
            self.inputs,
            fn(a_input) { a_input.output_reference == input },
          )
        expect
          quantity_of(output.value, ct_token_policy_id, ct_token_asset_name) == 1

        expect lovelace == lovelace_of(output.value)
        expect Some(output_to_fraud_prover) = list.at(self.outputs, 0)
        and {
          self.mint == from_asset(
            policy_id: ct_token_policy_id,
            asset_name: ct_token_asset_name,
            quantity: -1,
          ),
          validate_output_to_fraud_prover(
            output_to_fraud_prover,
            step_datum.fraud_prover,
          ),
          quantity_of(output_to_fraud_prover.value, "", "") == lovelace,
        }
      }

      // --------- PURPOSE - LastContinue Redeemer-------------------------------
      // (1) Mint the fraud token, which will implicitly burn the computation thread token with the Success redeemer. Let output state be that transaction output
      // (2) Fraud prover field must match between the output state and the input datum
      // (3) Output state datum type must be StepDatum(Void)
      ContinueLast { .. } -> {
        // (1.1) Mint the fraud token, which will implicitly burn the computation thread token with the Success redeemer. Let output state be that transaction output
        // todo ct_token_burnt,
        // todo fraud_proof_token_minted,

        // (2.1) The fraud prover field must match between the output state and the input datum
        expect Some(output_to_finish) = list.at(self.outputs, 0)
        expect InlineDatum(output_to_finish_datum) = output_to_finish.datum
        expect parsed_output_to_finish_datum: StepDatum = output_to_finish_datum
        // (3.1) Define the state transition        
        let step_last_state = DoubleSpendStepLastState { data: Void }
        let step_last_datum =
          StepDatum {
            fraud_prover: parsed_output_to_finish_datum.fraud_prover,
            data: step_last_state,
          }

        and {
          // todo ct_token_burnt(),
          // todo fraud_proof_token_minted(),
          parsed_output_to_finish_datum.fraud_prover == step_datum.fraud_prover,
          // (2.2) fraud prover field matches
          step_last_datum == parsed_output_to_finish_datum,
        }
      }
    }
    // (3.2) state transition verified
  }

  else(_) {
    fail
  }
}
