use aiken/collection/list
use aiken/interval.{Finite, Interval}
use aiken_design_patterns/linked_list/ordered.{
  Key, NodeDatum, get_node_inputs, get_node_outputs,
}
use cardano/assets.{PolicyId, quantity_of, tokens}
use cardano/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, Transaction,
}
use midgard/active_operators
use midgard/hub_oracle
use midgard/retired_operators.{
  Datum as RetiredOperatorDatum, Deinit, Init, MintRedeemer, RecoverOperatorBond,
  RemoveOperatorBadSettlement, RemoveOperatorBadState, RetireOperator,
}
use midgard/settlement
use midgard/state_queue

validator spend(retired_operators_mint_script_hash: ByteArray) {
  spend(
    _datumOpt: Option<Data>,
    _redeemer: Data,
    _input: OutputReference,
    self: Transaction,
  ) {
    ordered.list_state_transition(
      tokens(self.mint, retired_operators_mint_script_hash),
    )
  }

  else(_) {
    fail
  }
}

validator mint(
  slashing_penalty: Int,
  hub_oracle_script_hash: PolicyId,
  hub_oracle_asset_name: ByteArray,
) {
  mint(redeemer: MintRedeemer, policy_id: PolicyId, self: Transaction) {
    let node_in_outputs = get_node_inputs(self.inputs, policy_id)
    let node_outputs = get_node_outputs(self.outputs, policy_id)
    let node_mint = tokens(self.mint, policy_id)
    when redeemer is {
      Init -> and {
          quantity_of(self.mint, hub_oracle_script_hash, hub_oracle_asset_name) == 1,
          ordered.init(node_outputs, node_mint, policy_id),
        }
      Deinit -> and {
          quantity_of(self.mint, hub_oracle_script_hash, hub_oracle_asset_name) == -1,
          ordered.deinit(node_in_outputs, node_mint, policy_id),
        }
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
        expect parsed_hub_oracle_datum: hub_oracle.Datum = hub_oracle_datum
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
        expect retired_operator_data: RetiredOperatorDatum =
          parsed_retired_operator_datum.data
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
        expect parsed_hub_oracle_datum: hub_oracle.Datum = hub_oracle_datum
        expect Some(state_queue_redeemer_pair) =
          list.at(self.redeemers, state_queue_redeemer_index)
        expect state_queue.CutOffFraudulentBlockHeader {
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
