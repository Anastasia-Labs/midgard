use aiken/builtin
use aiken/collection/dict.{Dict}
use aiken/collection/list
use aiken/crypto.{ScriptHash, Signature, VerificationKey, VerificationKeyHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{Input, OutputReference, Transaction}
use midgard/common/types.{MerkleRoot} as midgard_types
use midgard/common/utils.{verify_hash_32}
use midgard/computation_thread.{StepDatum}
use midgard/fraud_proofs/common.{
  ExclusionProof, MembershipProofInputs, get_singleton_ct_token,
  get_tx_root_validate_block_hash, handle_cancel, validate_exclusion_proof,
  validate_final_step_output, validate_membership_proof,
} as fraud_proof_utils
use midgard/fraud_proofs/missing_signature/step.{
  Cancel, Continue, StepRedeemer as Step1Redeemer,
}
use midgard/ledger_state.{MidgardTxWitnessSetCompact}

validator main(
  // Fraud proof token policy
  fraud_proof_token_policy: PolicyId,
  // Script hash of stake validator (forwarded merkle proof logic)
  plutarch_midgard_merkle_validator_inclusion: ScriptHash,
  // Script hash of stake validator (forwarded merkle proof logic)
  plutarch_midgard_merkle_validator_exclusion: ScriptHash,
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
        bad_tx_required_signer_hashes,
        bad_tx_wits_as_data,
        verification_key,
        signature,
        signature_exclusion_proof,
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

        // -------- Missing signature: validation-specific logic START --------
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
        verify_hash_32(bad_tx_wits_as_data, bad_tx.wits)

        expect
          validate_exclusion_proof(
            tx,
            plutarch_midgard_merkle_validator_exclusion,
            ExclusionProof {
              merkle_root: get_addr_tx_wits_merkle_tree_root(
                bad_tx_wits_as_data,
              ),
              excluded_object: (verification_key, signature),
              excluded_key: verification_key,
              object_to_root_proof_path: signature_exclusion_proof,
            },
          )

        // //Prove that (verification_key, signature) belongs to addr_tx_wits(tx)
        // expect
        //   utils.plutarch_phas(
        //     get_addr_tx_wits_merkle_tree_root(bad_tx_wits_as_data),
        //     bad_required_signer_verification_key_hash,
        //     bad_required_signer_verification_as_data,
        //     bad_required_signer_verification_membership_proof,
        //     tx.redeemers,
        //   )
        // let (verification_key, signature): (VerificationKey, Signature) =
        //   addr_tx_wit_coercer(bad_addr_tx_wit_as_data)
        // verify_hash_32(
        //   bad_tx_addr_tx_wits_as_data,
        //   get_addr_tx_wits_hash_32(bad_tx_wits_as_data),
        // )
        // verify_hash_32(
        //   bad_tx_required_signer_hashes,
        //   bad_tx_body.required_signer_hashes,
        // )
        // let bad_tx_addr_tx_wits =
        //   resolve_address_tx_wits_from_data(bad_tx_addr_tx_wits_as_data)
        // expect Some(bad_required_signer_verification_key_hash) =
        //   list.at(bad_tx_required_signer_hashes, bad_required_signer_hash_index)
        // //The signer key hash must not be present in the address transaction witnesses
        // expect
        //   dict.has_key(
        //     bad_tx_addr_tx_wits,
        //     bad_required_signer_verification_key_hash,
        //   ) == False
        // -------- Missing signature: validation-specific logic END --------
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

pub fn get_addr_tx_wits_merkle_tree_root(
  bad_tx_wits_as_data: Data<MidgardTxWitnessSetCompact>,
) -> MerkleRoot<VerificationKeyHash, (VerificationKey, Signature)> {
  //It's the first field and it's a ByteArray
  bad_tx_wits_as_data
    |> builtin.unconstr_fields
    |> builtin.head_list
    |> builtin.un_b_data
}

pub fn resolve_address_tx_wits_from_data(
  address_tx_wits_as_data: Data<
    Dict<VerificationKeyHash, (VerificationKey, Signature)>,
  >,
) -> Dict<VerificationKeyHash, (VerificationKey, Signature)> {
  // Return the resolved hash
  expect coerced: Pairs<VerificationKeyHash, (VerificationKey, Signature)> =
    address_tx_wits_as_data

  // Fails if not in ascending order
  coerced |> dict.from_ascending_pairs
}
