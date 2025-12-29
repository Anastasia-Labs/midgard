use aiken/builtin
use aiken/collection/dict.{Dict}
use aiken/collection/list
use aiken/crypto.{ScriptHash, VerificationKeyHash}
use cardano/address.{Address, Script}
use cardano/assets.{PolicyId}
use cardano/transaction.{Input, Output, OutputReference, Transaction}
use midgard/common/constants
use midgard/common/types.{H32, MerkleRoot} as midgard_types
use midgard/common/utils.{verify_hash_32}
use midgard/computation_thread.{StepDatum}
use midgard/fraud_proof/common/types.{MembershipProofInputs}
use midgard/fraud_proof/common/utils.{
  get_singleton_ct_token, get_tx_root_validate_block_hash, handle_cancel,
  validate_final_step_output, validate_membership_proof, validate_step_output,
} as fraud_proof_utils
use midgard/fraud_proof/missing_native_script/step_01.{
  Cancel, Continue, StepRedeemer as Step1Redeemer, StepState as Step1State,
}
use midgard/ledger_state.{
  MidgardScript, MidgardTxBodyCompact, MidgardTxCompact,
  MidgardTxWitnessSetCompact, Plutus, Timelock,
}

validator main(
  // Script hash of next step (backwards compilation order of step validators required)
  step_02_validator_script_hash: ScriptHash,
  // Fraud proof token policy
  fraud_proof_token_policy: PolicyId,
  // Script hash of stake validator (forwarded merkle proof logic)
  plutarch_midgard_merkle_validator_inclusion: ScriptHash,
  // Computation thread token policy
  ct_token_policy_id: PolicyId,
  // State queue token policy
  state_queue_policy_id: PolicyId,
) {
  spend(
    datum: Option<StepDatum>,
    redeemer: Step1Redeemer,
    input: OutputReference,
    tx: Transaction,
  ) {
    expect Some(step_datum) = datum
    expect Some(own_input) =
      list.find(tx.inputs, fn(a_input) { a_input.output_reference == input })

    when redeemer is {
      Cancel { index_output } -> {
        expect
          handle_cancel(
            own_input,
            index_output,
            step_datum,
            ct_token_policy_id,
            tx,
          )
        True
      }
      Continue {
        bad_state_queue_node_ref_input_index,
        bad_tx_hash,
        bad_tx_info_hash,
        bad_tx_to_root_proof_path,
        bad_tx,
        bad_tx_body,
        bad_tx_wits,
        bad_output_index,
        bad_output_as_data,
        bad_output_membership_proof,
        script_wit_to_delete_key,
        script_wit_to_delete_value,
        script_wits_delete_proof,
        script_wits_new_root,
        index_output,
        computation_thread_redeemer_index,
      } -> {
        let ct_token_asset_name =
          get_singleton_ct_token(own_input, ct_token_policy_id)
        let extracted_tx_root =
          get_tx_root_validate_block_hash(
            tx,
            bad_state_queue_node_ref_input_index,
            ct_token_asset_name,
            state_queue_policy_id,
          )

        // -------- Missing native script: validation-specific logic START --------
        // Membership proof for tx
        expect
          validate_membership_proof(
            tx,
            plutarch_midgard_merkle_validator_inclusion,
            MembershipProofInputs {
              tx_root: extracted_tx_root,
              tx_hash: bad_tx_hash,
              tx_info_hash: bad_tx_info_hash,
              tx_to_root_proof_path: bad_tx_to_root_proof_path,
            },
          )

        verify_hash_32(bad_tx, bad_tx_hash)
        verify_hash_32(bad_tx_body, bad_tx.body)
        verify_hash_32(bad_tx_wits, bad_tx.wits)

        //Prove output u belongs to outputs(tx)
        expect
          utils.plutarch_phas(
            bad_tx_body.outputs,
            bad_output_index,
            bad_output_as_data,
            bad_output_membership_proof,
            tx.redeemers,
          )
        let bad_output_address: Address =
          output_address_coercer(bad_output_as_data)
        let bad_output_script_hash = getScriptHash(bad_output_address)

        let script_wits_old_root = get_script_tx_wits_merkle_root(bad_tx_wits)

        //If you try to delete the bad output from the merkletree, it means it existed!
        expect
          bad_output_script_hash != when script_wit_to_delete_value is {
            Timelock(script) -> script
            Plutus(script) -> script
          }

        //Prove (script_hash(address(u)), s) doesn't belong to script_tx_wits(tx)
        //starting the recursion that will continue in next fraud proof step
        expect
          utils.plutarch_pdelete(
            script_wits_old_root,
            script_wit_to_delete_key,
            script_wit_to_delete_value,
            script_wits_delete_proof,
            tx.redeemers,
          ) == script_wits_new_root
        // -------- Missing native script: validation-specific logic END --------
        if script_wits_new_root == constants.empty_merkle_tree_root {
          //Base step
          expect
            validate_final_step_output(
              tx,
              index_output,
              fraud_proof_token_policy,
              ct_token_asset_name,
              step_datum,
              computation_thread_redeemer_index,
            )
        } else {
          //Recursion step
          let step_1_state =
            Step1State {
              bad_output_script_hash,
              current_root: script_wits_new_root,
            }
          let expected_output_datum =
            StepDatum {
              fraud_prover: step_datum.fraud_prover,
              data: step_1_state,
            }

          expect
            validate_step_output(
              tx,
              index_output,
              step_02_validator_script_hash,
              ct_token_policy_id,
              ct_token_asset_name,
              step_datum,
              expected_output_datum,
            )
        }

        True
      }
    }
  }

  else(_) {
    fail
  }
}

fn getScriptHash(address: Address) -> ByteArray {
  when address.payment_credential is {
    Script(hash) -> hash
    _ -> fail @"Expected a script credential"
  }
}

pub fn get_script_tx_wits_merkle_root(
  bad_tx_wits_as_data: Data<MidgardTxWitnessSetCompact>,
) -> MerkleRoot<ScriptHash, MidgardScript> {
  //It's the second field and it's a ByteArray
  bad_tx_wits_as_data
    |> builtin.unconstr_fields
    |> builtin.tail_list
    |> builtin.head_list
    |> builtin.un_b_data
}

pub fn output_address_coercer(output_as_data: Data<Output>) -> Address {
  // Unsafely destructuring `output_as_data`, assuming it is structured as 
  // an `Output`. Since further down we are validating the presence of 
  // this UTxO in the latest ledger, we don't need to worry about the 
  // correctness of its structure here. 
  expect [output_address_data, ..] = builtin.unconstr_fields(output_as_data)

  utils.address_coercer(output_address_data)
}
