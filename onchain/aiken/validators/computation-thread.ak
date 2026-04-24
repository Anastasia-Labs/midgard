use aiken/builtin
use aiken/collection/list
use aiken/primitive/bytearray
use cardano/address.{Address, Script}
use cardano/assets.{PolicyId}
use cardano/transaction.{InlineDatum, Output, Transaction}
use midgard/common/utils
use midgard/computation_thread.{
  BurnForCancellation, Init, MintRedeemer, StepDatum, Success,
}
use midgard/fraud_proof_catalogue as catalogue
use midgard/hub_oracle as hub
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
          mint,
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
        expect fraud_prover |> utils.has_signed(extra_signatories)

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
          assets.quantity_of(self.mint, own_policy_id, burning_token_asset_name) == -1

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
