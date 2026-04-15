use aiken/builtin
use aiken/collection/dict.{Dict}
use aiken/crypto.{ScriptHash, Signature, VerificationKey, VerificationKeyHash}
use cardano/address.{Address, Script}
use cardano/assets.{PolicyId}
use cardano/transaction.{Output, OutputReference, Transaction}
use midgard/common/types.{MerkleRoot} as midgard_types
use midgard/common/utils.{verify_hash_32}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, pass_tx_to_next_step}
use midgard/fraud_proofs/missing_native_script/step_02.{
  Args, Datum, SpendRedeemer,
}
use midgard/fraud_proofs/missing_native_script/step_03_recursive as step_03
use midgard/ledger_state.{MidgardScript, MidgardTxWitnessSetCompact}

validator main(
  step_03_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  hub_oracle: ScriptHash,
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
      ct.Continue(Args {
        input_index,
        output_index,
        bad_output_index,
        bad_output,
        bad_output_membership_proof,
        missing_native_script_bytes,
        inclusion_proof_script_withdraw_redeemer_index,
      }) -> {
        let Transaction { inputs, outputs, redeemers, .. } = tx

        expect Some(step_datum) = datum

        // 1. Specified input must be authentic and properly reproduced.
        let
          _own_script_hash,
          _computation_thread_token_asset_name,
          _fraud_prover,
          m_input_state_data,
          output_script_hash,
          output_state_data,
        <-
          continue(
            computation_thread_token_policy_id,
            step_datum,
            input_index,
            output_index,
            own_out_ref,
            inputs,
            outputs,
          )

        expect Some(State { bad_tx_outputs_root, bad_tx_script_wits }) =
          m_input_state_data

        // 3. Prove output u belongs to outputs(tx)
        expect
          utils.plutarch_phas(
            merkle_root: bad_tx_outputs_root,
            key: bad_output_index,
            value: bad_output,
            membership_proof: bad_output_membership_proof,
            redeemers: redeemers,
            withdraw_redeemer_index: inclusion_proof_script_withdraw_redeemer_index,
          )

        // 4. The specified output must be at a script address.
        expect Script(bad_output_script_hash) = bad_output.address

        // 5. The missing script must be a Native script.
        expect
          bad_output_script_hash == ledger_state.hash_midgard_script(
            Timelock(missing_native_script_bytes),
          )

        // 6. The produced UTxO must go to the next step with a proper state.
        expect output_script_hash == step_03_validator_script_hash
        let expected_output_state: Data =
          step_03.State {
            expected_missing_script_hash: bad_output_script_hash,
            verified_script_wits_root: bad_tx_script_wits,
          }
        expect output_state_data == expected_output_state

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}

pub fn get_addr_tx_wits_merkle_tree_root(
  bad_tx_wits: MidgardTxWitnessSetCompact,
) -> MerkleRoot<VerificationKeyHash, (VerificationKey, Signature)> {
  //It's the first field and it's a ByteArray
  bad_tx_wits
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

pub fn get_script_hash(address: Address) -> ByteArray {
  when address.payment_credential is {
    Script(hash) -> hash
    _ -> fail @"Expected a script credential"
  }
}
