use aiken/collection/list
use aiken/crypto.{VerificationKeyHash}
use aiken/interval
use aiken_design_patterns/linked_list
use cardano/assets.{PolicyId, Value}
use cardano/transaction.{
  Input, Mint, Output, OutputReference, Redeemer, Transaction,
}
use midgard/common/types.{PosixTime}
use midgard/common/utils
use midgard/hub_oracle as hub
use midgard/operator_directory.{
  deinit, init, operator_is_not_a_member, slash_non_active_fraudulent_operator,
}
use midgard/operator_directory/active_operators
use midgard/operator_directory/registered_operators.{
  ActivateOperator, Datum, Deinit, DeregisterOperator, DuplicateIsActive,
  DuplicateIsRegistered, DuplicateIsRetired, Init, MintRedeemer, NewOperator,
  NodeData, RegisterOperator, ReturningOperator, SlashDuplicateOperator,
  SlashFraudulentOperator, activation_time_to_node_key, finalize_linked_list,
  node_key_to_activation_time, root_asset_name,
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
        operator_origin,
      } -> {
        // retired_operators_element_ref_input_index,
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

        // 3. Registering operator must not be present in the active operators
        //    set.
        expect
          registering_operator
            |> operator_is_not_a_member(
                reference_inputs: reference_inputs,
                element_ref_input_index: active_operators_element_ref_input_index,
              )
            |> active_operators.finalize_linked_list(
                active_operators_mint_script_hash,
              )

        // 4. Linked list prepend input and outputs must exist at specified
        //    indices.
        expect Some(root_input) = inputs |> list.at(root_input_index)
        expect Some(root_output) = outputs |> list.at(root_output_index)
        expect Some(registered_node_output) =
          outputs |> list.at(registered_node_output_index)

        // 5. Prepending to the head of the linked list must be proper.
        {
          let
            _anchor_lovelace_change,
            m_anchor_key,
            _anchor_data,
            registered_node_lovelace,
            registered_node_key,
            registered_node_data,
            _registered_node_link,
          <-
            linked_list.insert_descending(
              root_input,
              root_output,
              registered_node_output,
              mint,
            )

          // 6. Anchor element must be root.
          expect m_anchor_key == None

          // 7. The Lovelace in the registered node must equal the
          //    `required_bond` Midgard parameter.
          expect registered_node_lovelace >= required_bond

          // 8. Let `activation_time` be equal to the sum of the Midgard
          //    `registration_duration` protocol parameter and the inclusive
          //    upper bound of the transaction's validity interval.
          let activation_time =
            registration_duration + utils.get_inclusive_upper_bound_of_interval(
              validity_range,
            )

          // 9. The key of registered node must match the big-endian encoding of
          //    `activation_time`
          expect
            registered_node_key == activation_time_to_node_key(activation_time)

          when operator_origin is {
            ReturningOperator { retired_operators_redeemer_index } -> {
              // 10a. `ReregisterOperator` redeemer of the retired operators set
              //      must be invoked with a matching operator key.
              expect retired_operators.ReregisterOperator {
                retired_operator_key,
                retired_operator_bond_unlock_time,
                ..
              } =
                self.redeemers
                  |> utils.get_redeemer_at(
                      expected_purpose: Mint(retired_operators_mint_script_hash),
                      redeemer_index: retired_operators_redeemer_index,
                    )
              expect registering_operator == retired_operator_key

              // 11a. The returning operators bond unlock time must be carried
              //      over.
              let expected_node_data: Data =
                NodeData {
                  operator: registering_operator,
                  bond_unlock_time: retired_operator_bond_unlock_time,
                }
              expect registered_node_data == expected_node_data

              // Done.
              True
            }
            NewOperator { retired_operators_element_ref_input_index } -> {
              // 10b. Registering operator must not be present in the retired
              //      operators set.
              expect
                registering_operator
                  |> operator_is_not_a_member(
                      reference_inputs: reference_inputs,
                      element_ref_input_index: retired_operators_element_ref_input_index,
                    )
                  |> retired_operators.finalize_linked_list(
                      retired_operators_mint_script_hash,
                    )

              // 11b. Registered node's data must carry the registering
              //      operator, with no bond unlock time.
              let expected_node_data: Data =
                NodeData {
                  operator: registering_operator,
                  bond_unlock_time: None,
                }
              expect registered_node_data == expected_node_data

              // Done.
              True
            }
          }
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
        expect active_operators.ActivateOperator {
          new_active_operator_key,
          new_active_operator_bond_unlock_time,
          ..
        } =
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
        let
          removed_node_key,
          bond_unlock_time,
        <-
          remove_operator_and_get_node_key_and_bond_unlock_time(
            own_policy_id,
            activating_operator,
            anchor_element_input_index,
            removed_node_input_index,
            anchor_element_output_index,
            inputs,
            outputs,
            mint,
          )

        // 5. The bond unlock time retrieved from the removed registered node
        //    must be carried over to the active operators set.
        expect new_active_operator_bond_unlock_time == bond_unlock_time

        // 6.  The `activation_time` of the removed node (retrieved from its
        //     key) must be in the past.
        let activation_time = node_key_to_activation_time(removed_node_key)
        expect validity_range |> interval.is_entirely_after(activation_time)

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

        // 2. Removal of the deregistering operator's node from the registered
        //    operator set must be valid.
        let
          _removed_node_key,
          m_bond_unlock_time,
        <-
          remove_operator_and_get_node_key_and_bond_unlock_time(
            own_policy_id,
            deregistering_operator,
            anchor_element_input_index,
            removed_node_input_index,
            anchor_element_output_index,
            inputs,
            outputs,
            mint,
          )

        // 3. If the operator has prior obligations, they all must be matured.
        expect
          when m_bond_unlock_time is {
            Some(bond_unlock_time) ->
              self.validity_range
                |> interval.is_entirely_after(bond_unlock_time)
            None -> True
          }

        // Done.
        True
      }
      SlashDuplicateOperator {
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

        // 2. Removal of the duplicate operator's node from the registered
        //    operator set must be valid.
        let
          _removed_node_key,
          _bond_unlock_time,
        <-
          remove_operator_and_get_node_key_and_bond_unlock_time(
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
            // 4a. Referenced element must be a node in the registered operators
            //     set with an operator key that matches the removed operator.
            {
              let
                _node_lovelace,
                _m_key,
                node_data,
                _node_link,
              <- linked_list.get_element_info(duplicate_node_ref_utxo)
              expect NodeData { operator: node_operator, .. } = node_data
              expect node_operator == duplicate_operator

              // Done.
              True
            }
              |> finalize_linked_list(own_policy_id)
          DuplicateIsActive { hub_oracle_ref_input_index } -> {
            // 4b. Since the active operators policy ID is not a parameter here,
            //     the transaction must refer to hub oracle's authentic UTxO for
            //     retrieval of the policy ID.
            let hub_datum =
              hub.get_datum(
                reference_inputs,
                hub_oracle_script_hash,
                hub_oracle_ref_input_index,
              )

            // 5b. Referenced element must be a node in the active operators
            //     set, with a key that matches the removed node.
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
      SlashFraudulentOperator { slashing_arguments } ->
        slash_non_active_fraudulent_operator(
          hub_oracle_script_hash,
          slashing_arguments,
          self,
        )
          |> finalize_linked_list(own_policy_id)
    }
  }

  else(_) {
    fail
  }
}

fn remove_operator_and_get_node_key_and_bond_unlock_time(
  own_policy_id: PolicyId,
  removed_operator: VerificationKeyHash,
  anchor_element_input_index: Int,
  removed_node_input_index: Int,
  anchor_element_output_index: Int,
  inputs: List<Input>,
  outputs: List<Output>,
  mint: Value,
  custom_validator: fn(linked_list.NodeKey, Option<PosixTime>) -> Bool,
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

    // 4. The operator of removed node must match the provided
    //    `removed_operator`.
    expect NodeData { operator, bond_unlock_time } = removed_node_data
    expect removed_operator == operator

    // 5.  The custom validation must pass.
    expect custom_validator(removed_node_key, bond_unlock_time)

    // Done.
    True
  }
    |> finalize_linked_list(own_policy_id)
}
