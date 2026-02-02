use aiken/collection/list
use aiken/interval.{Finite, Interval}
use aiken_design_patterns/linked_list
use cardano/assets.{
  PolicyId, ada_asset_name, ada_policy_id, quantity_of, tokens,
}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction,
}
use midgard/hub_oracle
use midgard/operator_directories/common.{deinit, init, operator_is_not_a_member}
use midgard/operator_directories/registered.{
  Activate, Active, Datum, Deinit, Deregister, Init, MintRedeemer, Register,
  Registered, RemoveDuplicateSlashBond, Retired, finalize_linked_list,
}
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
                  element_input_index: active_operators_element_ref_input_index,
                )
              |> active.finalize_linked_list(active_operators_mint_script_hash),
            registering_operator
              |> operator_is_not_a_member(
                  reference_inputs: reference_inputs,
                  element_input_index: retired_operators_element_ref_input_index,
                )
              |> retired.finalize_linked_list(
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

        // 5. The Lovelace in the registered node must equal the `required_bond`
        //    Midgard parameter.
        expect
          quantity_of(
            registered_node_output.value,
            ada_policy_id,
            ada_asset_name,
          ) == required_bond

        let linked_list_eval = {
          // 6. Prepending to the head of the linked list must be proper.
          let
            _root_data,
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
        linked_list_eval |> finalize_linked_list(own_policy_id)
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
        expect active.ActivateOperator { new_active_operator_key, .. } =
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
                element_input_index: retired_operators_element_ref_input_index,
              )
            |> retired.finalize_linked_list(retired_operators_script_hash)

        let valid_from =
          utils.get_inclusive_lower_bound_of_interval(validity_range)

        // 4. Linked list inputs and output for removal must exist at specified
        //    indices.
        expect Some(anchor_element_input) =
          inputs |> list.at(anchor_element_input_index)
        expect Some(removed_node_input) =
          inputs |> list.at(removed_node_input_index)
        expect Some(anchor_element_output) =
          outputs |> list.at(anchor_element_output_index)

        let linked_list_eval = {
          // 5. Removal of the activating node from the registered operator set
          //    must be valid.
          let
            _m_anchor_key,
            _anchor_data,
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

          // 6. The key of removed node must match the `activating_operator`
          //    redeemer argument.
          expect removed_node_key == activating_operator

          // 7.  The `activation_time` field of the removed node must lie before
          //     the lower bound of the transaction's validity range..
          expect NodeData { activation_time } = removed_node_data
          expect valid_from >= activation_time

          // Done.
          True
        }
        linked_list_eval |> finalize_linked_list(own_policy_id)
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

        // 2. Linked list inputs and output for removal must exist at specified
        //    indices.
        expect Some(anchor_element_input) =
          inputs |> list.at(anchor_element_input_index)
        expect Some(removed_node_input) =
          inputs |> list.at(removed_node_input_index)
        expect Some(anchor_element_output) =
          outputs |> list.at(anchor_element_output_index)

        let linked_list_eval = {
          // 3. Removal of the deregistering operator's node from linked list
          //    must be valid.
          let
            _m_anchor_key,
            _anchor_data,
            removed_node_key,
            _removed_node_data,
            _removed_node_link,
          <-
            linked_list.remove(
              anchor_element_input,
              removed_node_input,
              anchor_element_output,
              mint,
            )

          // 4. The key of the removed node must match the
          //    `deregistering_operator` redeemer argument.
          expect removed_node_key == deregistering_operator

          // Done.
          True
        }
      }
      RemoveDuplicateSlashBond {
        duplicate_node_key,
        hub_oracle_ref_input_index,
        duplicate_node_ref_input_index,
        duplicate_node_ref_input_asset_name,
        removed_node_input_index,
        anchor_node_input_index,
        witness_status,
      } -> {
        expect Some(hub_oracle_ref_input) =
          list.at(self.reference_inputs, hub_oracle_ref_input_index)
        expect InlineDatum(hub_oracle_datum) = hub_oracle_ref_input.output.datum
        expect parsed_hub_oracle_datum: hub_oracle.Datum = hub_oracle_datum
        and {
          unordered.remove(
            Key(duplicate_node_key),
            removed_node_input_index,
            anchor_node_input_index,
            node_in_outputs,
            node_outputs,
            node_mint,
          ),
          self.fee >= slashing_penalty,
          when witness_status is {
            Registered -> {
              expect Some(duplicate_node_ref_input) =
                list.at(self.reference_inputs, duplicate_node_ref_input_index)
              and {
                quantity_of(
                  duplicate_node_ref_input.output.value,
                  policy_id,
                  duplicate_node_ref_input_asset_name,
                ) == 1,
                unordered.prove_is_member(
                  parsed_hub_oracle_datum.registered_operators,
                  Key(duplicate_node_key),
                  duplicate_node_ref_input.output,
                ),
              }
            }
            Active -> {
              expect Some(duplicate_node_ref_input) =
                list.at(self.reference_inputs, duplicate_node_ref_input_index)
              and {
                quantity_of(
                  duplicate_node_ref_input.output.value,
                  parsed_hub_oracle_datum.active_operators,
                  duplicate_node_ref_input_asset_name,
                ) == 1,
                unordered.prove_is_member(
                  parsed_hub_oracle_datum.active_operators,
                  Key(duplicate_node_key),
                  duplicate_node_ref_input.output,
                ),
              }
            }
            Retired -> {
              expect Some(duplicate_node_ref_input) =
                list.at(self.reference_inputs, duplicate_node_ref_input_index)
              and {
                quantity_of(
                  duplicate_node_ref_input.output.value,
                  parsed_hub_oracle_datum.retired_operators,
                  duplicate_node_ref_input_asset_name,
                ) == 1,
                unordered.prove_is_member(
                  parsed_hub_oracle_datum.retired_operators,
                  Key(duplicate_node_key),
                  duplicate_node_ref_input.output,
                ),
              }
            }
          },
        }
      }
    }
  }

  else(_) {
    fail
  }
}
