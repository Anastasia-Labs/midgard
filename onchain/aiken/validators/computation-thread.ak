use aiken/builtin
use aiken/collection/list
use aiken/primitive/bytearray
use cardano/assets.{PolicyId, flatten, quantity_of}
use cardano/transaction.{InlineDatum, Input, Output, Transaction}
use midgard/common/utils.{has_signed, is_output_to_sc}
use midgard/computation_thread.{
  BurnForCancellation, Init, MintRedeemer, StepDatum, Success,
}
use midgard/fraud_proof_catalogue as catalogue
use midgard/hub_oracle as hub
use midgard/ledger_state
use midgard/state_queue

validator mint(
  fraud_proof_catalogue_script_hash: ByteArray,
  hub_oracle_script_hash: PolicyId,
) {
  mint(redeemer: MintRedeemer, own_policy_id: PolicyId, self: Transaction) {
    when redeemer is {
      Init {
        first_step_output_index,
        fraud_category_id,
        fraud_category,
        fraud_category_membership_proof,
        fraud_proof_catalogue_ref_input_index,
        inclusion_proof_script_redeemer_index,
        hub_oracle_ref_input_index,
        fraudulent_block_ref_input_index,
      } -> {
        let Transaction {
          outputs,
          reference_inputs,
          redeemers,
          extra_signatories,
          ..
        } = self

        // 1. Transaction must reference the fraud proof category's authentic
        //    utxo.
        let catalogue_merkle_root =
          reference_inputs
            |> catalogue.get_datum(
                fraud_proof_catalogue_script_hash,
                fraud_proof_catalogue_ref_input_index,
              )

        // 2. The provided fraud category (i.e. spending script hash for the
        //    first step of the desired fraud proof) must be a member of the
        //    Merkle structure which its root is attached to the referenced
        //    fraud proof category utxo.
        expect
          utils.plutarch_phas(
            merkle_root: catalogue_merkle_root,
            key: builtin.un_b_data(fraud_category_id),
            value: builtin.un_b_data(fraud_category),
            membership_proof: fraud_category_membership_proof,
            redeemers: redeemers,
            withdraw_redeemer_index: inclusion_proof_script_redeemer_index,
          )

        // 3. Transaction must retrieve policy ID of the state queue from the
        //    authentic hub oracle utxo.
        let hub.Datum { state_queue: state_queue_policy_id, .. } =
          hub.get_datum(
            reference_inputs,
            hub_oracle_script_hash,
            hub_oracle_ref_input_index,
          )

        // 4. Transaction must reference the fraudulent block. Let its key be
        //    `fraudulent_header_hash`.
        let
          _fraudulent_header,
          fraudulent_header_hash,
        <-
          state_queue.get_block_datum(
            reference_inputs,
            state_queue_policy_id,
            fraudulent_block_ref_input_index,
          )

        // 5. Output at the specified index must exist and:
        //    - Go to an address which its payment part is a script
        //    - Have an inline datum attached
        //    - Have no reference script attached
        //    Let this output be `first_step_output`.
        expect Some(Output {
          address: Address {
            payment_credential: Script(first_step_output_script_hash),
            ..
          },
          value: first_step_output_value,
          datum: InlineDatum(first_step_output_datum_data),
          reference_script: None,
        }) = list.at(outputs, first_step_output_index)

        // 6. The script hash of the destination address must be equal to the
        //    proven fraud category.
        expect first_step_output_script_hash == fraud_category

        // 7. `first_step_output` must have only 1 NFT such that:
        //    - Its policy ID is that of the current script (computation thread)
        //    - Its asset name is the concatenation of fraud category's ID, and
        //      `fraudulent_header_hash`
        let expected_asset_name =
          fraud_category_id |> bytearray.concat(fraudulent_header_hash)
        let expected_mint_triplet = (own_policy_id, expected_asset_name, 1)
        expect
          utils.get_single_asset_from_value_apart_from_ada(
            first_step_output_value,
          ) == expected_mint_triplet

        // 8. The datum attached to `first_step_output` must carry the fraud
        //    prover, without any additional data.
        expect StepDatum { fraud_prover, data: None }: StepDatum<Data> =
          first_step_output_datum_data

        // 9. Transaction must be signed by the specified fraud prover.
        expect fraud_prover |> has_signed(extra_signatories)

        // 10. The NFT in `first_step_output` must be the only asset minted in
        //     the transaction.
        expect utils.get_single_asset_from_value(mint) == expected_mint_triplet

        // Done.
        True
      }
      Success { burning_token_asset_name } -> {
        // 1. Specified token must be burned. The rest of validations
        //    (including the coupling of this endpoint with minting of a
        //    `fraud_proof` token) are done in each fraud proof's last step.
        expect
          quantity_of(self.mint, own_policy_id, burning_token_asset_name) == -1

        // Done.
        True
      }
      BurnForCancellation { burning_token_asset_name } -> {
        // 1. Transaction must burn the specified token, without minting or
        //    burning any other tokens.
        let expected_mint =
          assets.from_asset(own_policy_id, burning_token_asset_name, -1)
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
