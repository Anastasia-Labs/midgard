use aiken/collection/list
use cardano/assets.{PolicyId}
use cardano/transaction.{
  Input, Mint, Output, OutputReference, Transaction, resolve_input,
}
use midgard/common/utils
use midgard/da_attestation.{
  AddSignatures, ApplyToStateQueue, BurnForStateQueue, Datum, Init, MintRedeemer,
  SpendRedeemer, asset_name, get_attestation_input_datum,
  get_authenticated_state_queue_policy_id, validate_add_signatures,
  validate_init_output,
}
use midgard/da_params_governor
use midgard/state_queue

validator main(
  da_params_policy_id: PolicyId,
  reference_script_auth_policy_id: PolicyId,
) {
  mint(redeemer: MintRedeemer, own_policy_id: PolicyId, tx: Transaction) {
    when redeemer is {
      Init {
        output_index,
        da_params_ref_input_index,
        state_queue_ref_input_index,
        state_queue_mint_ref_script_input_index,
      } -> {
        // 1. The referenced data availability params must be authentic.
        let params =
          da_params_governor.get_da_params(
            tx.reference_inputs,
            da_params_policy_id,
            da_params_ref_input_index,
          )

        // 2. The referenced state queue mint script must be authenticated by
        //    the reference-script authorization NFT.
        let state_queue_policy_id =
          get_authenticated_state_queue_policy_id(
            tx.reference_inputs,
            reference_script_auth_policy_id,
            state_queue_mint_ref_script_input_index,
          )

        // 3. The referenced state queue node must match a block with no
        //    attached data availability attestation.
        let expected_header_hash =
          state_queue.get_state_queue_node(
            tx.reference_inputs,
            state_queue_policy_id,
            state_queue_ref_input_index,
            fn(state_queue_node, header_hash) {
              expect
                state_queue_node.da_attestation == state_queue.no_da_attestation
              header_hash
            },
          )

        // 4. The attestation output must initialize attestation for the
        //    expected header with the current params committee hash and
        //    threshold.
        let attestation_asset =
          tx.outputs
            |> list.expect_at(output_index)
            |> validate_init_output(own_policy_id, params, expected_header_hash)

        // 5. Exactly one header-specific attestation NFT must be minted.
        expect utils.validate_mint(tx.mint, own_policy_id, attestation_asset, 1)

        True
      }
      ApplyToStateQueue {
        da_attestation_input_index,
        state_queue_input_index,
        state_queue_output_index,
        state_queue_mint_ref_script_input_index,
      } -> {
        // 1. The attestation input must be authentic for this policy.
        let attestation_datum =
          get_attestation_input_datum(
            tx.inputs,
            da_attestation_input_index,
            own_policy_id,
          )

        // 2. The attestation threshold must have been met.
        expect
          attestation_datum.attestation_count >= attestation_datum.da_threshold

        // 3. The header-specific attestation NFT must be burnt.
        let attestation_asset = asset_name(attestation_datum.header_hash)
        expect
          utils.validate_mint(tx.mint, own_policy_id, attestation_asset, -1)

        // 4. The referenced state queue mint script must be authenticated by
        //    the reference-script authorization NFT.
        let state_queue_policy_id =
          get_authenticated_state_queue_policy_id(
            tx.reference_inputs,
            reference_script_auth_policy_id,
            state_queue_mint_ref_script_input_index,
          )

        // 5. The state queue node must attach this attestation to the same
        //    header.
        state_queue.validate_da_attestation_attachment(
          tx.inputs,
          tx.outputs,
          state_queue_policy_id,
          state_queue_input_index,
          state_queue_output_index,
          attestation_datum.header_hash,
          own_policy_id,
        )
      }
    }
  }

  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      AddSignatures { output_index, da_params_ref_input_index, signatures } ->
        validate_add_signatures(
          datum,
          output_index,
          da_params_policy_id,
          da_params_ref_input_index,
          signatures,
          own_ref,
          tx,
        )
      BurnForStateQueue { mint_redeemer_index } -> {
        // 1. The attestation spend must correspond to this policy.
        let Output { address: input_address, .. } =
          tx.inputs |> resolve_input(own_ref)
        let own_policy_id = utils.expect_script_hash(input_address)

        // 2. The matching mint redeemer must burn this attestation for state
        //    queue attachment.
        let mint_redeemer_data =
          tx.redeemers
            |> utils.get_redeemer_at(Mint(own_policy_id), mint_redeemer_index)
        expect ApplyToStateQueue { da_attestation_input_index, .. }: MintRedeemer =
          mint_redeemer_data

        // 3. The mint redeemer's attestation input index must point to this
        //    spent UTxO.
        let Input { output_reference, .. } =
          tx.inputs |> list.expect_at(da_attestation_input_index)
        expect output_reference == own_ref

        True
      }
    }
  }

  else(_) {
    fail
  }
}
