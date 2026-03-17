use aiken/builtin
use aiken/collection/list
use aiken/collection/dict.{Dict}
use aiken/interval.{Finite, Interval}
use aiken_design_patterns/linked_list/ordered
use aiken_design_patterns/linked_list/unordered.{
  Key, NodeDatum, NodeKey, get_node_inputs, get_node_outputs,
}
use cardano/assets.{PolicyId, quantity_of, tokens}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction,
}
use midgard/active_operators.{Datum as ActiveOperatorDatum}
use midgard/hub_oracle
use midgard/protocol_parameters.{registration_duration}
use midgard/registered_operators.{
  Activate, Active, Datum, Deinit, Deregister, Init, MintRedeemer, Register,
  Registered, RemoveDuplicateSlashBond, Retired,
}

validator spend(registered_operators_mint_script_hash: ByteArray) {
  spend(
    _datum: Option<Data>,
    _redeemer: Data,
    _input: OutputReference,
    self: Transaction,
  ) {
    unordered.list_state_transition(
      tokens(self.mint, registered_operators_mint_script_hash),
    )
  }

  else(_) {
    fail
  }
}

validator mint(
  required_bond: Int,
  slashing_penalty: Int,
  hub_oracle_script_hash: PolicyId,
  hub_oracle_asset_name: ByteArray,
) {
  mint(redeemer: MintRedeemer, policy_id: PolicyId, self: Transaction) {
    let node_in_outputs = get_node_inputs(self.inputs, policy_id)
    let node_outputs = get_node_outputs(self.outputs, policy_id)
    let node_mint = tokens(self.mint, policy_id)
    let has_hub_oracle_mint =
      quantity_of(self.mint, hub_oracle_script_hash, hub_oracle_asset_name) == 1
    let has_hub_oracle_ref_input =
      list.any(
        self.reference_inputs,
        fn(ref_input) {
          quantity_of(
            ref_input.output.value,
            hub_oracle_script_hash,
            hub_oracle_asset_name,
          ) == 1
        },
      )
    when redeemer is {
      Init -> and {
          or {
            has_hub_oracle_mint,
            has_hub_oracle_ref_input,
          },
          unordered.init(node_outputs, node_mint, policy_id),
        }
      Deinit -> and {
          quantity_of(self.mint, hub_oracle_script_hash, hub_oracle_asset_name) == -1,
          unordered.deinit(node_in_outputs, node_mint, policy_id),
        }
      Register {
        key_to_prepend,
        hub_oracle_ref_input_index,
        active_operator_ref_input_index,
        active_operator_asset_name,
        retired_operator_ref_input_index,
        retired_operator_asset_name,
        prepended_node_output_index,
        anchor_node_output_index,
      } -> {
        expect Finite(valid_to) = self.validity_range.upper_bound.bound_type
        expect Some(hub_oracle_ref_input) =
          list.at(self.reference_inputs, hub_oracle_ref_input_index)
        expect InlineDatum(hub_oracle_datum) = hub_oracle_ref_input.output.datum
        expect parsed_hub_oracle_datum: hub_oracle.Datum = hub_oracle_datum
        expect Some(active_operator_ref_input) =
          list.at(self.reference_inputs, active_operator_ref_input_index)
        expect Some(retired_operator_ref_input) =
          list.at(self.reference_inputs, retired_operator_ref_input_index)
        expect Some(registered_node_output) =
          list.at(node_outputs, prepended_node_output_index)
        expect InlineDatum(registered_operator_datum) =
          registered_node_output.datum
        expect parsed_registered_operator_datum: NodeDatum =
          registered_operator_datum
        let registered_operator_data_constr =
          builtin.un_constr_data(parsed_registered_operator_datum.data)
        expect Some(registered_operator_activation_time_data) =
          list.at(registered_operator_data_constr.2nd, 0)
        expect registered_operator_activation_time: Int =
          registered_operator_activation_time_data
        and {
          register_prepend_transition(
            Key(key_to_prepend),
            prepended_node_output_index,
            anchor_node_output_index,
            node_in_outputs,
            node_outputs,
            node_mint,
          ),
          list.has(self.extra_signatories, key_to_prepend),
          registered_operator_activation_time == valid_to + registration_duration,
          quantity_of(registered_node_output.value, "", "") == required_bond,
          quantity_of(
            hub_oracle_ref_input.output.value,
            hub_oracle_script_hash,
            hub_oracle_asset_name,
          ) == 1,
          quantity_of(
            active_operator_ref_input.output.value,
            parsed_hub_oracle_datum.active_operators,
            active_operator_asset_name,
          ) == 1,
          ordered.prove_is_not_member(
            parsed_hub_oracle_datum.active_operators,
            ordered.Key(key_to_prepend),
            active_operator_ref_input.output,
          ),
          quantity_of(
            retired_operator_ref_input.output.value,
            parsed_hub_oracle_datum.retired_operators,
            retired_operator_asset_name,
          ) == 1,
          ordered.prove_is_not_member(
            parsed_hub_oracle_datum.retired_operators,
            ordered.Key(key_to_prepend),
            retired_operator_ref_input.output,
          ),
        }
      }

      Activate {
        node_to_activate_key,
        hub_oracle_ref_input_index,
        retired_operator_ref_input_index,
        retired_operator_asset_name,
        removed_node_input_index,
        anchor_node_input_index,
        active_operators_inserted_node_output_index,
        active_operators_anchor_node_output_index,
      } -> {
        trace @"RO.activate.entry"
        trace @"RO.activate.expect.valid_from.start"
        expect Finite(valid_from) = self.validity_range.lower_bound.bound_type
        trace @"RO.activate.expect.valid_from.ok"
        trace @"RO.activate.expect.hub_oracle_ref_input.start"
        expect Some(hub_oracle_ref_input) =
          list.at(self.reference_inputs, hub_oracle_ref_input_index)
        trace @"RO.activate.expect.hub_oracle_ref_input.ok"
        trace @"RO.activate.expect.hub_oracle_inline_datum.start"
        expect InlineDatum(hub_oracle_datum) = hub_oracle_ref_input.output.datum
        trace @"RO.activate.expect.hub_oracle_inline_datum.ok"
        trace @"RO.activate.expect.hub_oracle_datum_decode.start"
        expect parsed_hub_oracle_datum: hub_oracle.Datum = hub_oracle_datum
        trace @"RO.activate.expect.hub_oracle_datum_decode.ok"
        trace @"RO.activate.expect.retired_ref_input.start"
        expect Some(retired_operator_ref_input) =
          list.at(self.reference_inputs, retired_operator_ref_input_index)
        trace @"RO.activate.expect.retired_ref_input.ok"
        trace @"RO.activate.expect.removed_node_input.start"
        expect Some(removed_node_input) =
          list.at(node_in_outputs, removed_node_input_index)
        trace @"RO.activate.expect.removed_node_input.ok"
        trace @"RO.activate.expect.removed_node_inline_datum.start"
        expect InlineDatum(removed_node_datum) = removed_node_input.datum
        trace @"RO.activate.expect.removed_node_inline_datum.ok"
        trace @"RO.activate.expect.removed_node_decode.start"
        expect parsed_removed_node_datum: NodeDatum = removed_node_datum
        trace @"RO.activate.expect.removed_node_decode.ok"
        trace @"RO.activate.expect.registered_datum_decode.start"
        expect registered_operator_data: Datum = parsed_removed_node_datum.data
        trace @"RO.activate.expect.registered_datum_decode.ok"
        let active_operators_node_in_outputs =
          get_node_inputs(self.inputs, parsed_hub_oracle_datum.active_operators)
        let active_operators_node_outputs =
          get_node_outputs(
            self.outputs,
            parsed_hub_oracle_datum.active_operators,
          )
        let active_operators_node_mint =
          tokens(self.mint, parsed_hub_oracle_datum.active_operators)
        trace @"RO.activate.prepare.active_operator_sets.ok"

        trace @"RO.activate.check.unordered_remove.start"
        let check_unordered_remove =
          unordered.remove(
            Key(node_to_activate_key),
            removed_node_input_index,
            anchor_node_input_index,
            node_in_outputs,
            node_outputs,
            node_mint,
          )
        trace @"RO.activate.check.unordered_remove.end"

        trace @"RO.activate.check.empty_active_list.start"
        let active_list_was_empty =
          when active_operators_node_in_outputs is {
            [active_anchor_input] -> {
              expect InlineDatum(active_anchor_datum) = active_anchor_input.datum
              expect parsed_active_anchor_datum: NodeDatum = active_anchor_datum
              and {
                parsed_active_anchor_datum.key == NodeKey.Empty,
                parsed_active_anchor_datum.link == NodeKey.Empty,
              }
            }
            _ -> False
          }
        trace @"RO.activate.check.empty_active_list.end"

        trace @"RO.activate.check.active_anchor_output.start"
        expect Some(active_anchor_output) =
          list.at(
            active_operators_node_outputs,
            active_operators_anchor_node_output_index,
          )
        expect InlineDatum(active_anchor_output_datum) =
          active_anchor_output.datum
        expect parsed_active_anchor_output_datum: NodeDatum =
          active_anchor_output_datum
        expect _: ActiveOperatorDatum = parsed_active_anchor_output_datum.data
        trace @"RO.activate.check.active_anchor_output.end"

        trace @"RO.activate.check.active_inserted_output.start"
        expect Some(active_inserted_output) =
          list.at(
            active_operators_node_outputs,
            active_operators_inserted_node_output_index,
          )
        expect InlineDatum(active_inserted_output_datum) =
          active_inserted_output.datum
        expect parsed_active_inserted_output_datum: NodeDatum =
          active_inserted_output_datum
        expect _: ActiveOperatorDatum = parsed_active_inserted_output_datum.data
        trace @"RO.activate.check.active_inserted_output.end"

        // Registration delay is waived only when activating into an empty
        // active-operators list via the head node (key=None,next=None -> key=None,*).
        let bypass_activation_window =
          and {
            active_list_was_empty,
            parsed_active_anchor_output_datum.key == NodeKey.Empty,
          }

        let check_activation_window =
          or {
            bypass_activation_window,
            valid_from >= registered_operator_data.activation_time,
          }

        let check_hub_oracle_ref_value =
          quantity_of(
            hub_oracle_ref_input.output.value,
            hub_oracle_script_hash,
            hub_oracle_asset_name,
          ) == 1

        trace @"RO.activate.check.ordered_insert.start"
        let check_ordered_insert =
          ordered.insert(
            ordered.Key(node_to_activate_key),
            active_operators_inserted_node_output_index,
            active_operators_anchor_node_output_index,
            active_operators_node_in_outputs,
            active_operators_node_outputs,
            active_operators_node_mint,
          )
        trace @"RO.activate.check.ordered_insert.end"

        let check_retired_ref_value =
          quantity_of(
            retired_operator_ref_input.output.value,
            parsed_hub_oracle_datum.retired_operators,
            retired_operator_asset_name,
          ) == 1

        trace @"RO.activate.check.retired_not_member.start"
        let check_retired_not_member =
          ordered.prove_is_not_member(
            parsed_hub_oracle_datum.retired_operators,
            ordered.Key(node_to_activate_key),
            retired_operator_ref_input.output,
          )
        trace @"RO.activate.check.retired_not_member.end"

        and {
          check_unordered_remove,
          check_activation_window,
          check_hub_oracle_ref_value,
          check_ordered_insert,
          check_retired_ref_value,
          check_retired_not_member,
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

fn register_prepend_transition(
  key_to_prepend: NodeKey,
  prepended_node_output_index: Int,
  anchor_node_output_index: Int,
  node_in_outputs: List<Output>,
  node_outputs: List<Output>,
  node_mint: Dict<ByteArray, Int>,
) -> Bool {
  unordered.prepend_unsafe(
    key_to_prepend,
    prepended_node_output_index,
    anchor_node_output_index,
    node_in_outputs,
    node_outputs,
    node_mint,
  )
}
