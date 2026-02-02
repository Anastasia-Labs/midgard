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
      Register {
        key_to_prepend,
        hub_oracle_ref_input_index,
        active_operator_ref_input_index,
        active_operator_asset_name,
        retired_operator_ref_input_index,
        retired_operator_asset_name,
        prepended_node_output_index,
        anchor_node_output_index,
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
        let hub.Datum { retired_operators: retired_operators_script_hash, .. } =
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
              |> active.finalize_linked_list(active_operators_script_hash),
            registering_operator
              |> operator_is_not_a_member(
                  reference_inputs: reference_inputs,
                  element_input_index: retired_operators_element_ref_input_index,
                )
              |> retired.finalize_linked_list(retired_operators_script_hash),
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

          // 6. The key of registered node match the `registering_operator`
          //    redeemer argument.
          expect registered_node_key == registering_operator

          // 7.  The `activation_time` field of registered node must equal the
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
      Activate {
        operator_to_activate,
        hub_oracle_ref_input_index,
        retired_operator_ref_input_index,
        retired_operator_asset_name,
        removed_node_input_index,
        anchor_node_input_index,
        active_operators_inserted_node_output_index,
        active_operators_anchor_node_output_index,
      } -> {
        expect Finite(valid_from) = self.validity_range.lower_bound.bound_type
        expect Some(hub_oracle_ref_input) =
          list.at(self.reference_inputs, hub_oracle_ref_input_index)
        expect InlineDatum(hub_oracle_datum) = hub_oracle_ref_input.output.datum
        expect parsed_hub_oracle_datum: hub_oracle.Datum = hub_oracle_datum
        expect Some(retired_operator_ref_input) =
          list.at(self.reference_inputs, retired_operator_ref_input_index)
        expect Some(removed_node_input) =
          list.at(self.inputs, removed_node_input_index)
        expect InlineDatum(removed_node_datum) = removed_node_input.output.datum
        expect parsed_removed_node_datum: NodeDatum = removed_node_datum
        expect registered_operator_data: Datum = parsed_removed_node_datum.data
        let active_operators_node_in_outputs =
          get_node_inputs(self.inputs, parsed_hub_oracle_datum.active_operators)
        let active_operators_node_outputs =
          get_node_outputs(
            self.outputs,
            parsed_hub_oracle_datum.active_operators,
          )
        let active_operators_node_mint =
          tokens(self.mint, parsed_hub_oracle_datum.active_operators)

        and {
          unordered.remove(
            Key(operator_to_activate),
            removed_node_input_index,
            anchor_node_input_index,
            node_in_outputs,
            node_outputs,
            node_mint,
          ),
          valid_from >= registered_operator_data.activation_time,
          quantity_of(
            hub_oracle_ref_input.output.value,
            hub_oracle_script_hash,
            hub.asset_name,
          ) == 1,
          ordered.insert(
            ordered.Key(operator_to_activate),
            active_operators_inserted_node_output_index,
            active_operators_anchor_node_output_index,
            active_operators_node_in_outputs,
            active_operators_node_outputs,
            active_operators_node_mint,
          ),
          quantity_of(
            retired_operator_ref_input.output.value,
            parsed_hub_oracle_datum.retired_operators,
            retired_operator_asset_name,
          ) == 1,
          ordered.prove_is_not_member(
            parsed_hub_oracle_datum.retired_operators,
            ordered.Key(operator_to_activate),
            retired_operator_ref_input.output,
          ),
        }
      }
      Deregister {
        node_to_deregister_key,
        removed_node_input_index,
        anchor_node_input_index,
      } -> and {
          unordered.remove(
            Key(node_to_deregister_key),
            removed_node_input_index,
            anchor_node_input_index,
            node_in_outputs,
            node_outputs,
            node_mint,
          ),
          list.has(self.extra_signatories, node_to_deregister_key),
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
