use aiken/builtin
use aiken/collection/dict.{Dict}
use aiken/collection/list
use aiken/crypto.{ScriptHash, VerificationKeyHash}
use cardano/address.{Address, Script}
use cardano/assets.{PolicyId}
use cardano/transaction.{Input, OutputReference, Transaction}
use midgard/common/types.{H32} as midgard_types
use midgard/common/utils.{verify_hash_32}
use midgard/computation_thread.{StepDatum}
use midgard/fraud_proof/common/types.{MembershipProofInputs}
use midgard/fraud_proof/common/utils.{
  get_singleton_ct_token, get_tx_root_validate_block_hash, handle_cancel,
  validate_final_step_output, validate_membership_proof,
} as fraud_proof_utils
use midgard/fraud_proof/missing_native_script/step.{
  Cancel, Continue, StepRedeemer as Step1Redeemer,
}
use midgard/ledger_state.{
  MidgardScript, MidgardTxBodyCompact, MidgardTxCompact,
  MidgardTxWitnessSetCompact,
}

validator main(
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
      // Step 1: Membership proof for tx1
      Continue {
        bad_state_queue_node_ref_input_index,
        bad_tx_hash,
        bad_tx_info_hash,
        bad_tx_to_root_proof_path,
        bad_tx,
        bad_tx_body,
        bad_tx_outputs_as_data,
        bad_tx_wits_as_data,
        script_tx_wits_as_data,
        bad_output_index,
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

        verify_hash_32(bad_tx_outputs_as_data, bad_tx_body.outputs)
        verify_hash_32(bad_tx_wits_as_data, bad_tx.wits)
        verify_hash_32(
          script_tx_wits_as_data,
          get_script_tx_wits_hash_32(bad_tx_wits_as_data),
        )
        let script_tx_wits =
          resolve_script_tx_wits_from_data(script_tx_wits_as_data)
        let bad_output_address =
          get_bad_output_address(bad_tx_outputs_as_data, bad_output_index)

        // Output script must be missing in the witnesses
        expect
          dict.has_key(script_tx_wits, getScriptHash(bad_output_address)) == False
        // -------- Missing native script: validation-specific logic END --------
        expect
          validate_final_step_output(
            tx,
            index_output,
            fraud_proof_token_policy,
            ct_token_asset_name,
            step_datum,
            computation_thread_redeemer_index,
          )

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

fn get_bad_output_address(outputs_as_data, index) -> Address {
  let data_list: List<Data> = builtin.un_list_data(outputs_as_data)
  expect Some(output_as_data) = list.at(data_list, index)
  let output_address_as_data =
    output_as_data |> builtin.unconstr_fields |> builtin.head_list
  expect address: Address = output_address_as_data
  address
}

pub fn get_script_tx_wits_hash_32(
  bad_tx_wits_as_data: Data<MidgardTxWitnessSetCompact>,
) -> H32<Dict<ScriptHash, MidgardScript>> {
  //It's the second field and it's a ByteArray
  bad_tx_wits_as_data
    |> builtin.unconstr_fields
    |> builtin.tail_list
    |> builtin.head_list
    |> builtin.un_b_data
}

pub fn resolve_script_tx_wits_from_data(
  script_tx_wits_as_data: Data<Dict<ScriptHash, MidgardScript>>,
) -> Dict<ScriptHash, MidgardScript> {
  // Return the resolved hash
  expect coerced: Pairs<VerificationKeyHash, MidgardScript> =
    script_tx_wits_as_data

  // Fails if not in ascending order
  coerced |> dict.from_ascending_pairs
}
