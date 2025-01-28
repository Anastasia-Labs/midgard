use aiken/builtin
// imports 
// midgard-merkle is used as a library to verify merkle inclusion proofs 
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{ScriptHash, blake2b_256}
use cardano/address.{Address, Script, VerificationKey}
use cardano/assets.{PolicyId, Value, flatten, from_asset, quantity_of}
use cardano/transaction.{InlineDatum, Output, OutputReference, Transaction}
use midgard/common/ledger_state.{MidgardTx, MidgardTxBody}
use midgard/fraud_proof/non_existing_input/step_02.{
  Cancel, Continue, InvalidInputSpendStep1State, InvalidInputSpendStep2State,
  Step2redeemer, StepDatum,
}

validator main(
  ct_token_policy_id: PolicyId,
  next_step_spending_validator: ScriptHash,
  _stake_validator: ScriptHash,
) {
  spend(
    maybe_datum: Option<StepDatum>,
    redeemer: Step2redeemer,
    input: OutputReference,
    self: Transaction,
  ) {
    expect Some(step_datum) = maybe_datum
    when redeemer is {
      Cancel { index_output } -> {
        expect Some(own_input) =
          list.find(
            self.inputs,
            fn(a_input) { a_input.output_reference == input },
          )
        //let's find the assetname fraud token
        expect
          quantity_of(output.value, ct_token_policy_id, ct_token_asset_name) == 1

        //*****************/
        //1) the fraud token should be burnt
        expect
          self.mint == from_asset(
            policy_id: ct_token_policy_id,
            asset_name: ct_token_asset_name,
            quantity: -1,
          )

        //*****************/
        //let's find the amount of ADA inside the input of the fraud proof to be cancelled
        expect lovelace == lovelace_of(output.value)
        //let's find the output that goes to the fraud prover
        expect Some(output_to_fraud_prover) =
          list.at(self.outputs, index_output)

        //*****************/
        //2)verify that the output is going to fraud prover address
        expect
          validate_output_to_fraud_prover(
            output_to_fraud_prover,
            step_datum.fraud_prover,
          )

        //*****************/
        //3)send the ada back to the prover
        expect lovelace_of(output_to_fraud_prover.value) == lovelace
        True
      }
      // I want to pass the transaction, verify that the hash is equal to the tx I found previously, and verify that the 
      Continue { bad_input, input_utxos, index_output, tx_body, tx } -> {
        //we get the previous datum and find the tx_hash that is the one we want to check
        expect InvalidInputSpendStep1State { tx_hash, .. } = step_datum.data

        //1) The list of inputs is part of the transactio
        expect bytes_input_list: ByteArray = input_utxos
        expect blake2b_256(bytes_input_list) == tx_body.spend_inputs

        //2) The passed input list contains the bad list
        expect list.has(builtin.un_list_data(input_utxos), bad_input)

        //3) Is the midgard body part of the Midgard tx that we are passing?
        expect blake2b_256(cbor.serialise(tx_body)) == tx.body

        //4) Is the midgard tx passing hashed the same one we got in the previous step
        expect blake2b_256(cbor.serialise(tx)) == tx_hash

        expect Some(own_input) =
          list.find(
            self.inputs,
            fn(a_input) { a_input.output_reference == input },
          )

        //Let's find the assetname
        expect
          quantity_of(output.value, ct_token_policy_id, ct_token_asset_name) == 1

        //let's get the new datum
        expect Some(output_state) = list.at(self.outputs, index_output)
        expect InlineDatum(datum_output) = output_state.datum
        expect datum_parsed: StepDatum = datum_output
        //5) Is the fraud token kept
        expect
          quantity_of(
            output_state.value,
            ct_token_policy_id,
            ct_token_asset_name,
          ) == 1

        expect parsed_bad_input: OutputReference = bad_input
        let step2_state =
          InvalidInputSpendStep2State { tx_hash, input: parsed_bad_input }
        let step_2_datum =
          StepDatum { fraud_prover: step_datum.fraud_prover, data: step2_state }
        //6) Does the new step store the right datum?
        expect step_2_datum == datum_parsed
        //7) Does the new step goes to the right validator?
        expect Script(hash) = output_state.address.payment_credential
        expect hash == next_step_spending_validator
        True
      }
    }
  }

  else(_) {
    fail
  }
}

pub fn ct_token_burnt(value: Value, policy: PolicyId, assetname: ByteArray) {
  quantity_of(value, policy, assetname) == -1
}

pub fn validate_output_to_fraud_prover(
  output_to_fraud_prover: Output,
  prover_hash: ByteArray,
) {
  when output_to_fraud_prover.address.payment_credential is {
    VerificationKey(hash) -> hash == prover_hash
    _ -> False
  }
}
