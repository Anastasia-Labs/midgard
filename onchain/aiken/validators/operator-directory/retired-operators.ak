use aiken/collection/list
use aiken/interval.{Finite, Interval}
use aiken_design_patterns/linked_list
use cardano/assets.{PolicyId, quantity_of, tokens}
use cardano/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, Transaction,
}
use midgard/hub_oracle as hub
use midgard/operator_directory.{deinit, init}
use midgard/operator_directory/active_operators
use midgard/operator_directory/retired_operators.{
  Deinit, Init, MintRedeemer, RecoverOperatorBond, RemoveOperatorBadSettlement,
  RemoveOperatorBadState, RetireOperator, finalize_linked_list, root_asset_name,
}
use midgard/protocol_parameters.{slashing_penalty}
use midgard/settlement
use midgard/state_queue

validator spend(retired_operators_mint_script_hash: PolicyId) {
  spend(
    _m_datum: Option<Data>,
    _redeemer: Redeemer,
    _own_out_ref: OutputReference,
    self: Transaction,
  ) {
    linked_list.spend_for_adding_or_removing_an_element(
      retired_operators_mint_script_hash,
      self.mint,
    )
  }

  else(_) {
    fail
  }
}

validator mint(
  hub_oracle_script_hash: PolicyId,
  hub_oracle_asset_name: ByteArray,
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
      RetireOperator {
        new_retired_operator_key,
        hub_oracle_ref_input_index,
        retired_operator_appended_node_output_index,
        retired_operator_anchor_node_output_index,
        active_operators_redeemer_index,
      } -> {
        expect Some(hub_oracle_ref_input) =
          list.at(self.reference_inputs, hub_oracle_ref_input_index)
        expect InlineDatum(hub_oracle_datum) = hub_oracle_ref_input.output.datum
        expect parsed_hub_oracle_datum: hub.Datum = hub_oracle_datum
        expect Some(active_operators_redeemer_pair) =
          list.at(self.redeemers, active_operators_redeemer_index)
        expect active_operators.RetireOperator { active_operator_key, .. } =
          active_operators_redeemer_pair.2nd

        and {
          quantity_of(
            hub_oracle_ref_input.output.value,
            hub_oracle_script_hash,
            hub_oracle_asset_name,
          ) == 1,
          ordered.append(
            Key(new_retired_operator_key),
            retired_operator_appended_node_output_index,
            retired_operator_anchor_node_output_index,
            node_in_outputs,
            node_outputs,
            node_mint,
          ),
          active_operators_redeemer_pair.1st == Mint(
            parsed_hub_oracle_datum.active_operators,
          ),
          active_operator_key == new_retired_operator_key,
        }
      }
      RecoverOperatorBond {
        retired_operator_key,
        removed_node_input_index,
        anchor_node_input_index,
      } -> {
        expect Finite(valid_from) = self.validity_range.lower_bound.bound_type
        expect Some(retired_node_input) =
          list.at(self.inputs, removed_node_input_index)
        expect InlineDatum(retired_operator_datum) =
          retired_node_input.output.datum
        expect parsed_retired_operator_datum: NodeDatum = retired_operator_datum
        expect retired_operator_data: Datum = parsed_retired_operator_datum.data
        and {
          ordered.remove(
            Key(retired_operator_key),
            removed_node_input_index,
            anchor_node_input_index,
            node_in_outputs,
            node_outputs,
            node_mint,
          ),
          when retired_operator_data.bond_unlock_time is {
            None -> True
            Some(bond_unlock_time) -> valid_from >= bond_unlock_time
          },
        }
      }
      RemoveOperatorBadState {
        slashed_retired_operator_key,
        hub_oracle_ref_input_index,
        retired_operator_slashed_node_input_index,
        retired_operator_anchor_node_input_index,
        state_queue_redeemer_index,
      } -> {
        expect Some(hub_oracle_ref_input) =
          list.at(self.reference_inputs, hub_oracle_ref_input_index)
        expect InlineDatum(hub_oracle_datum) = hub_oracle_ref_input.output.datum
        expect parsed_hub_oracle_datum: hub.Datum = hub_oracle_datum
        expect Some(state_queue_redeemer_pair) =
          list.at(self.redeemers, state_queue_redeemer_index)
        expect state_queue.RemoveFraudulentBlockHeader {
          fraudulent_operator,
          ..
        } = state_queue_redeemer_pair.2nd
        and {
          quantity_of(
            hub_oracle_ref_input.output.value,
            hub_oracle_script_hash,
            hub_oracle_asset_name,
          ) == 1,
          ordered.remove(
            Key(slashed_retired_operator_key),
            retired_operator_slashed_node_input_index,
            retired_operator_anchor_node_input_index,
            node_in_outputs,
            node_outputs,
            node_mint,
          ),
          self.fee >= slashing_penalty,
          state_queue_redeemer_pair.1st == Mint(
            parsed_hub_oracle_datum.state_queue,
          ),
          fraudulent_operator == slashed_retired_operator_key,
        }
      }
      RemoveOperatorBadSettlement {
        slashed_retired_operator_key,
        hub_oracle_ref_input_index,
        retired_operator_slashed_node_input_index,
        retired_operator_anchor_node_input_index,
        settlement_input_index,
        settlement_redeemer_index,
      } -> {
        let Transaction { inputs, reference_inputs, redeemers, fee, .. } = self
        let hub.Datum { settlement_addr, .. } =
          hub.get_datum(
            reference_inputs,
            hub_oracle_script_hash,
            hub_oracle_ref_input_index,
          )
        expect settlement.DisproveResolutionClaim { operator, .. } =
          utils.get_spending_redeemer_data_at(
            settlement_addr,
            settlement_input_index,
            settlement_redeemer_index,
            inputs,
            redeemers,
          )
        // TODO fix performance:
        and {
          ordered.remove(
            Key(slashed_retired_operator_key),
            retired_operator_slashed_node_input_index,
            retired_operator_anchor_node_input_index,
            node_in_outputs,
            node_outputs,
            node_mint,
          ),
          fee >= slashing_penalty,
          operator == slashed_retired_operator_key,
        }
      }
    }
  }

  else(_) {
    fail
  }
}
