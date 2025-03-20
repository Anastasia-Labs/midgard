// imports 
// midgard-merkle is used as a library to verify merkle inclusion proofs 
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Address, Script, VerificationKey}
use cardano/assets.{PolicyId, Value, flatten, from_asset, quantity_of}
use cardano/transaction.{InlineDatum, Output, OutputReference, Transaction}

// Invalid Input Reference fraud proof: A transaction tx in a proposed block contains an input referencing a nonexistent output.

// tx refers to entire tx that contain:
// Input UTxOs it's trying to spend
// Output UTxOs it's creating

//STEP 2
//The utxo specified is part of the tx 

pub type StepDatum {
  fraud_prover: ByteArray,
  data: Data,
}

//TODO This should be importend from a library, it's the state queue Datum
pub type StateDatum {
  block_hash: ByteArray,
  block_roots: BlockRoots,
}

pub type BlockRoots {
  block_withdraw_root: ByteArray,
  block_tx_root: ByteArray,
  block_deposit_root: ByteArray,
}

pub type MembershipProofInputs {
  root: ByteArray,
  tx_hash: ByteArray,
  tx_info_hash: ByteArray,
  proof_path: ByteArray,
}

pub type InvalidInputSpendStep1State {
  tx_hash: ByteArray,
  proof_path: ByteArray,
}

pub type InvalidInputSpendStep2State {
  tx_hash: ByteArray,
  input: OutputReference,
}

//verify that given the array of inputs I get the hash that is actually written inside the Block
//the input is part of it

pub type Step2redeemer {
  Cancel { index_output: Int }
  Continue {
    frauded_input: OutputReference,
    // TODO: Should this field be here?
    frauded_state_queue_tx_hash: ByteArray,
    input_utxos: List<OutputReference>,
    index_output: Int,
  }
}

validator not_existing_input(
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
        expect Some((_, ct_token_asset_name, _)) =
          list.at(flatten(own_input.output.value), 1)

        expect Some((_, _, lovelace)) =
          list.at(flatten(own_input.output.value), 0)

        expect Some(output_to_fraud_prover) =
          list.at(self.outputs, index_output)
        //The conditions are
        //- Burn the fraud token
        //- verify that the output is going to fraud prover address
        //- send the ada back to the prover
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

      Continue {
        frauded_input,
        frauded_state_queue_tx_hash,
        input_utxos,
        index_output,
      } -> {
        //we get the previous datum and find the tx_hash that is the one we want to check
        expect InvalidInputSpendStep1State { tx_hash, .. } = step_datum.data
        let tx_evaluated = cbor.serialise(input_utxos)

        //let's find the input invalid inside 
        let flag_input_present = list.has(input_utxos, frauded_input)

        //let's see if the input utxos given hashed gives this hash
        expect Some(own_input) =
          list.find(
            self.inputs,
            fn(a_input) { a_input.output_reference == input },
          )

        //Let's find the assetname
        expect Some((_, ct_token_asset_name, _)) =
          list.at(flatten(own_input.output.value), 1)

        //let's get the new datum
        expect Some(output_state) = list.at(self.outputs, index_output)
        expect InlineDatum(datum_output) = output_state.datum
        expect datum_parsed: StepDatum = datum_output

        expect Script(hash) = output_state.address.payment_credential
        let step2_state =
          InvalidInputSpendStep2State {
            tx_hash: frauded_state_queue_tx_hash,
            // TODO: Is `own_input` correct?
            input: own_input.output_reference,
          }
        let step_2_datum =
          StepDatum { fraud_prover: step_datum.fraud_prover, data: step2_state }
        and {
          tx_hash == tx_evaluated,
          flag_input_present,
          step_2_datum == datum_parsed,
          quantity_of(
            output_state.value,
            ct_token_policy_id,
            ct_token_asset_name,
          ) == 1,
          hash == next_step_spending_validator,
        }
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
