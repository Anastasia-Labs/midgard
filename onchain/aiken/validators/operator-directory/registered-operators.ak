use aiken/collection/list
use aiken/crypto.{VerificationKeyHash}
use aiken_design_patterns/linked_list
use cardano/assets.{PolicyId, Value}
use cardano/transaction.{
  Input, Mint, Output, OutputReference, Redeemer, Transaction,
}
use midgard/common/utils
use midgard/hub_oracle as hub
use midgard/operator_directory.{deinit, init, operator_is_not_a_member}
use midgard/operator_directory/active_operators
use midgard/operator_directory/registered_operators.{
  ActivateOperator, Datum, Deinit, DeregisterOperator, DuplicateIsActive,
  DuplicateIsRegistered, DuplicateIsRetired, Init, MintRedeemer, NodeData,
  RegisterOperator, RemoveDuplicateSlashBond, finalize_linked_list,
  root_asset_name,
}
use midgard/operator_directory/retired_operators
use midgard/protocol_parameters.{
  registration_duration, required_bond, slashing_penalty,
}

validator spend(registered_operators_mint_script_hash: ByteArray) {
  spend(
    _m_datum: Option<Data>,
    _redeemer: Redeemer,
    _own_out_ref: OutputReference,
    self: Transaction,
  ) {
    linked_list.spend_for_adding_or_removing_an_element(
      registered_operators_mint_script_hash,
      self.mint,
    )
  }

  else(_) {
    fail
  }
}

validator mint(
  retired_operators_mint_script_hash: PolicyId,
  hub_oracle_script_hash: PolicyId,
) {
  mint(redeemer: MintRedeemer, own_policy_id: PolicyId, self: Transaction) {
    when redeemer is {
      Init { output_index } ->
        init(
          hub_oracle_script_hash,
          own_policy_id,
          root_asset_name,
          output_index,
          self,
        )
      Deinit { input_index } ->
        deinit(
          hub_oracle_script_hash,
          own_policy_id,
          root_asset_name,
          input_index,
          self,
        )
      RegisterOperator {
        registering_operator,
        root_input_index,
        root_output_index,
        registered_node_output_index,
        hub_oracle_ref_input_index,
        active_operators_element_ref_input_index,
        retired_operators_element_ref_input_index,
      } -> {
        let Transaction {
          inputs,
          outputs,
          reference_inputs,
          mint,
          validity_range,
          extra_signatories,
          ..
        } = self

        // 1. Transaction must be signed by the registering operator.
        expect registering_operator |> utils.has_signed(extra_signatories)

        // 2. Transaction must refer to hub oracle's authentic UTxO.
        let hub.Datum {
          active_operators: active_operators_mint_script_hash,
          ..
        } =
          hub.get_datum(
            reference_inputs,
            hub_oracle_script_hash,
            hub_oracle_ref_input_index,
          )

        // 3. Registering operator must not be present in either the active
        //    or the retired sets.
        expect and {
            registering_operator
              |> operator_is_not_a_member(
                  reference_inputs: reference_inputs,
                  element_ref_input_index: active_operators_element_ref_input_index,
                )
              |> active_operators.finalize_linked_list(
                  active_operators_mint_script_hash,
                ),
            registering_operator
              |> operator_is_not_a_member(
                  reference_inputs: reference_inputs,
                  element_ref_input_index: retired_operators_element_ref_input_index,
                )
              |> retired_operators.finalize_linked_list(
                  retired_operators_mint_script_hash,
                ),
          }

        let valid_to =
          utils.get_inclusive_upper_bound_of_interval(validity_range)

        // 4. Linked list prepend input and outputs must exist at specified
        //    indices.
        expect Some(root_input) = inputs |> list.at(root_input_index)
        expect Some(root_output) = outputs |> list.at(root_output_index)
        expect Some(registered_node_output) =
          outputs |> list.at(registered_node_output_index)

        // 5. Prepending to the head of the linked list must be proper.
        {
          let
            _root_lovelace_change,
            _root_data,
            registered_node_lovelace,
            registered_node_key,
            registered_node_data,
            _registered_node_link,
          <-
            linked_list.prepend_unordered(
              root_input,
              root_output,
              registered_node_output,
              mint,
            )

          // 6. The Lovelace in the registered node must equal the
          //    `required_bond` Midgard parameter.
          expect registered_node_lovelace == required_bond

          // 7. The key of registered node match the `registering_operator`
          //    redeemer argument.
          expect registered_node_key == registering_operator

          // 8.  The `activation_time` field of registered node must equal the
          //     sum of the Midgard `registration_duration` protocol parameter
          //     and the inclusive upper bound of the transaction's validity
          //     interval.
          expect NodeData { activation_time } = registered_node_data
          expect activation_time == valid_to + registration_duration

          // Done.
          True
        }
          |> finalize_linked_list(own_policy_id)
      }
      ActivateOperator {
        activating_operator,
        anchor_element_input_index,
        removed_node_input_index,
        anchor_element_output_index,
        hub_oracle_ref_input_index,
        retired_operators_element_ref_input_index,
        active_operators_redeemer_index,
      } -> {
        let Transaction {
          inputs,
          outputs,
          reference_inputs,
          mint,
          redeemers,
          validity_range,
          ..
        } = self

        // 1. Transaction must refer to hub oracle's authentic UTxO.
        let hub.Datum {
          active_operators: active_operators_mint_script_hash,
          ..
        } =
          hub.get_datum(
            reference_inputs,
            hub_oracle_script_hash,
            hub_oracle_ref_input_index,
          )

        // 2. `ActivateOperator` minting redeemer of the active operators script
        //    must be invoked with a matching operator pub key hash.
        expect active_operators.ActivateOperator { new_active_operator_key, .. } =
          redeemers
            |> utils.get_redeemer_at(
                expected_purpose: Mint(active_operators_mint_script_hash),
                redeemer_index: active_operators_redeemer_index,
              )
        expect new_active_operator_key == activating_operator

        // 3. Activating operator must not have been retired.
        expect
          activating_operator
            |> operator_is_not_a_member(
                reference_inputs: reference_inputs,
                element_ref_input_index: retired_operators_element_ref_input_index,
              )
            |> retired_operators.finalize_linked_list(
                retired_operators_mint_script_hash,
              )

        // 4. Removal of the activating node from the registered operator set
        //    must be valid.
        let removed_node_data <-
          remove_operator_and_get_node_data(
            own_policy_id,
            activating_operator,
            anchor_element_input_index,
            removed_node_input_index,
            anchor_element_output_index,
            inputs,
            outputs,
            mint,
          )

        // 5.  The `activation_time` field of the removed node must lie before
        //     the lower bound of the transaction's validity range.
        let valid_from =
          utils.get_inclusive_lower_bound_of_interval(validity_range)
        expect NodeData { activation_time } = removed_node_data
        expect valid_from >= activation_time

        // Done.
        True
      }
      DeregisterOperator {
        deregistering_operator,
        anchor_element_input_index,
        removed_node_input_index,
        anchor_element_output_index,
      } -> {
        let Transaction { inputs, outputs, mint, extra_signatories, .. } = self

        // 1. Transaction must be signed by the deregistering operator.
        expect deregistering_operator |> utils.has_signed(extra_signatories)

        // 2. Removal of the deregistering node from the registered operator set
        //    must be valid.
        let _removed_node_data <-
          remove_operator_and_get_node_data(
            own_policy_id,
            deregistering_operator,
            anchor_element_input_index,
            removed_node_input_index,
            anchor_element_output_index,
            inputs,
            outputs,
            mint,
          )

        // Done.
        True
      }
      RemoveDuplicateSlashBond {
        duplicate_operator,
        anchor_element_input_index,
        removed_node_input_index,
        anchor_element_output_index,
        duplicate_node_ref_input_index,
        duplicate_operator_status,
      } -> {
        let Transaction { inputs, outputs, reference_inputs, mint, fee, .. } =
          self

        // 1. `slashing_penalty` must be paid via the transaction fee. The rest
        //    of the bond is assumed to be going to the signer's desired
        //    destination.
        expect fee >= slashing_penalty

        // 2. Removal of the duplicate operator node from the registered
        //    operator set must be valid.
        let _removed_node_data <-
          remove_operator_and_get_node_data(
            own_policy_id,
            duplicate_operator,
            anchor_element_input_index,
            removed_node_input_index,
            anchor_element_output_index,
            inputs,
            outputs,
            mint,
          )

        // 3. A reference input must exist at the specified index.
        expect Some(Input { output: duplicate_node_ref_utxo, .. }) =
          reference_inputs |> list.at(duplicate_node_ref_input_index)

        // Depending on where the existing duplicate operator resides, it must
        // be shown that the referenced input is a member of an associated
        // operator set, with a matching operator pub key hash.
        when duplicate_operator_status is {
          DuplicateIsRegistered ->
            // 4a. Referenced element must be a node in the registered
            //     operators set with a key that matches the removed node.
            {
              let
                _node_lovelace,
                m_key,
                _node_data,
                _node_link,
              <- linked_list.get_element_info(duplicate_node_ref_utxo)
              expect Some(registered_operator) = m_key
              expect registered_operator == duplicate_operator

              // Done.
              True
            }
              |> finalize_linked_list(own_policy_id)
          DuplicateIsActive { hub_oracle_ref_input_index } -> {
            // 4b. Transaction must refer to hub oracle's authentic UTxO.
            let hub_datum =
              hub.get_datum(
                reference_inputs,
                hub_oracle_script_hash,
                hub_oracle_ref_input_index,
              )

            // 5b. Referenced element must be a node in the active operators
            //     set with a key that matches the removed node.
            {
              let
                _node_lovelace,
                m_key,
                _node_data,
                _node_link,
              <- linked_list.get_element_info(duplicate_node_ref_utxo)
              expect Some(active_operator) = m_key
              expect active_operator == duplicate_operator

              // Done.
              True
            }
              |> active_operators.finalize_linked_list(
                  hub_datum.active_operators,
                )
          }
          DuplicateIsRetired ->
            // 4c. Referenced element must be a node in the retired operators
            //     set, with a key that matches the removed node.
            {
              let
                _node_lovelace,
                m_key,
                _node_data,
                _node_link,
              <- linked_list.get_element_info(duplicate_node_ref_utxo)
              expect Some(retired_operator) = m_key
              expect retired_operator == duplicate_operator

              // Done.
              True
            }
              |> retired_operators.finalize_linked_list(
                  retired_operators_mint_script_hash,
                )
        }
      }
    }
  }

  else(_) {
    fail
  }
}

fn remove_operator_and_get_node_data(
  own_policy_id: PolicyId,
  removed_operator: VerificationKeyHash,
  anchor_element_input_index: Int,
  removed_node_input_index: Int,
  anchor_element_output_index: Int,
  inputs: List<Input>,
  outputs: List<Output>,
  mint: Value,
  removed_node_data_validation: fn(Data) -> Bool,
) -> Bool {
  // 1. Linked list inputs and output for removal must exist at specified
  //    indices.
  expect Some(anchor_element_input) =
    inputs |> list.at(anchor_element_input_index)
  expect Some(removed_node_input) = inputs |> list.at(removed_node_input_index)
  expect Some(anchor_element_output) =
    outputs |> list.at(anchor_element_output_index)

  // 2. Removal of the operator node from the registered operator set must be
  //    valid.
  {
    let
      anchor_lovelace_change,
      _m_anchor_key,
      _anchor_data,
      _removed_node_lovelace,
      removed_node_key,
      removed_node_data,
      _removed_node_link,
    <-
      linked_list.remove(
        anchor_element_input,
        removed_node_input,
        anchor_element_output,
        mint,
      )

    // 3. ADA bond of the anchor node (or min required ADA if anchor is root)
    //    must be preserved.
    expect anchor_lovelace_change >= 0

    // 4. The key of removed node must match the `activating_operator` redeemer
    //    argument.
    expect removed_node_key == removed_operator

    // 5.  The custom validation must pass.
    expect removed_node_data_validation(removed_node_data)

    // Done.
    True
  }
    |> finalize_linked_list(own_policy_id)
}
