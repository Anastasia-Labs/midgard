use aiken/collection/list
use cardano/address.{Address, Script}
use cardano/assets.{PolicyId, flatten, quantity_of}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction,
}
use midgard/common/utils
use midgard/computation_thread.{StepDatum}
use midgard/fraud_proof.{Datum, MintRedeemer, SpendRedeemer}
use midgard/hub_oracle as hub

/// The spending validator of a fraud proof token never allows it to be spent.
/// Midgard fraud proof tokens last forever.
validator spend {
  spend(
    _datumOpt: Option<Data>,
    _redeemer: SpendRedeemer,
    _input: OutputReference,
    _self: Transaction,
  ) {
    False
  }

  else(_) {
    fail
  }
}

/// Midgard fraud proof tokens last forever.
validator mint(
  computation_thread_script_hash: ByteArray,
  hub_oracle_script_hash: PolicyId,
) {
  mint(redeemer: MintRedeemer, policy_id: PolicyId, self: Transaction) {
    let hub.Datum { fraud_proof_addr: fraud_proof_spend_addr, .. } =
      hub.get_datum(
        self.reference_inputs,
        hub_oracle_script_hash,
        redeemer.hub_oracle_ref_input_index,
      )

    expect Some(fraud_proof_last_step_input) =
      list.at(self.inputs, redeemer.fraud_proof_last_step_input_index)
    expect Some(output_with_fraud_proof) =
      list.at(self.outputs, redeemer.output_with_fraud_proof_index)

    expect InlineDatum(fraud_proof_last_step_datum) =
      fraud_proof_last_step_input.output.datum
    expect parsed_fraud_proof_last_step_datum: StepDatum =
      fraud_proof_last_step_datum

    and {
      validate_fraud_proof_last_step_input(
        fraud_proof_last_step_input,
        computation_thread_script_hash,
        redeemer.computation_thread_token_asset_name,
      ),
      quantity_of(
        self.mint,
        computation_thread_script_hash,
        redeemer.computation_thread_token_asset_name,
      ) == -1,
      quantity_of(
        self.mint,
        policy_id,
        redeemer.computation_thread_token_asset_name,
      ) == 1,
      utils.quantity_of_policy_id(self.mint, policy_id) == 1,
      validate_output_to_fraud_proof_addr(
        output_with_fraud_proof,
        fraud_proof_spend_addr,
        parsed_fraud_proof_last_step_datum.fraud_prover,
        policy_id,
        redeemer.computation_thread_token_asset_name,
      ),
    }
  }

  else(_) {
    fail
  }
}

fn validate_fraud_proof_last_step_input(
  fraud_proof_last_step_input: Input,
  computation_thread_script_hash: ByteArray,
  computation_thread_token_asset_name: ByteArray,
) {
  quantity_of(
    fraud_proof_last_step_input.output.value,
    computation_thread_script_hash,
    computation_thread_token_asset_name,
  ) == 1
}

fn validate_output_to_fraud_proof_addr(
  output_with_fraud_proof: Output,
  fraud_proof_spend_addr: Address,
  fraud_prover_hash: ByteArray,
  own_hash: ByteArray,
  token_asset_name: ByteArray,
) {
  expect Script(script_hash) = fraud_proof_spend_addr.payment_credential

  expect InlineDatum(outputDatum) = output_with_fraud_proof.datum
  expect parsedDatum: Datum = outputDatum
  let newDatum = Datum { fraud_prover: fraud_prover_hash }

  let correctValue = and {
      quantity_of(output_with_fraud_proof.value, own_hash, token_asset_name) == 1,
      list.length(flatten(output_with_fraud_proof.value)) == 2,
    }
  and {
    utils.is_output_to_sc(output_with_fraud_proof, script_hash),
    parsedDatum == newDatum,
    correctValue,
  }
}
