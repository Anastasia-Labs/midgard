use aiken/collection/list
use aiken/interval.{Finite, Interval}
use aiken_design_patterns/linked_list
use cardano/assets.{PolicyId, Value, quantity_of, tokens}
use cardano/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, Transaction,
}
use midgard/active_operators.{
  ActivateOperator, Datum, Deinit, Init, ListStateTransition, MintRedeemer,
  RemoveOperatorBadSettlement, RemoveOperatorBadState, RetireOperator,
  SpendRedeemer, UpdateBondHoldNewSettlement, UpdateBondHoldNewState,
  finalize_linked_list,
}
use midgard/common/utils
use midgard/hub_oracle as hub
use midgard/protocol_parameters.{maturity_duration}
use midgard/registered_operators
use midgard/retired_operators.{Datum as RetiredOperatorData}
use midgard/settlement
use midgard/state_queue

validator spend(
  active_operators_mint_script_hash: ByteArray,
  hub_oracle_script_hash: PolicyId,
) {
  spend(
    datumOpt: Option<NodeDatum>,
    redeemer: SpendRedeemer,
    input_out_ref: OutputReference,
    self: Transaction,
  ) {
    when redeemer is {
      ListStateTransition ->
        linked_list.spend_for_adding_or_removing_an_element(
          active_operators_mint_script_hash,
          self.mint,
        )
      UpdateBondHoldNewState {
        active_node_output_index,
        hub_oracle_ref_input_index,
        state_queue_input_index,
        state_queue_redeemer_index,
      } -> {
        let Transaction {
          inputs,
          outputs,
          reference_inputs,
          redeemers,
          mint,
          validity_range,
          ..
        } = self

        // 1. The new bond unlock time must be equal to the transaction's
        //    validity range upper bound plus the maturity duration.
        //    TODO: Does inclusivity of the upper bound matter here?
        let valid_to =
          utils.get_inclusive_upper_bound_of_interval(validity_range)
        let new_bond_unlock_time = valid_to + maturity_duration

        let linked_list_eval = {
          // 2. An element from the active operators directory must be spent and
          //    reproduced without affecting the linked list structure.
          let
            m_operator,
            _active_node_input_data,
            active_node_output_data,
          <-
            linked_list.spend_for_updating_elements_data(
              element_input_index: active_node_input_index,
              continued_element_output_index: active_node_output_index,
              element_input_outref: input_out_ref,
              inputs: inputs,
              outputs: outputs,
              tx_mint: mint,
            )

          // 3. The spent element must be a node (i.e. not root).
          expect Some(operator) = m_operator

          // 4. The reproduced node must properly store the new bond unlock
          //    time.
          expect NodeData { bond_unlock_time: m_output_bond_unlock_time } =
            active_node_output_data
          expect m_output_bond_unlock_time == Some(new_bond_unlock_time)

          // 5. Transaction must spend a UTxO from state queue with the Commit
          //    Block Header redeemer. The operator's signature should be
          //    validated there.
          //
          //    TODO: Potential optimization opportunities, i.e. using builtin
          //    functions. This requires implicitly imposing the position of
          //    `operator` field in the redeemer to be at index 0.
          expect state_queue.CommitBlockHeader {
            operator: state_queue_redeemer_operator,
            ..
          } =
            state_queue.get_spending_redeemer_data(
              hub_oracle_script_hash,
              hub_oracle_ref_input_index,
              state_queue_input_index,
              state_queue_redeemer_index,
              inputs,
              reference_inputs,
              redeemers,
            )

          // 6. The operator specified in state queue's redeemer must match the
          //    key of the spending active operator node.
          expect state_queue_redeemer_operator == operator

          // Done.
          True
        }
        linked_list_eval
          |> finalize_linked_list(active_operators_mint_script_hash)
      }
      UpdateBondHoldNewSettlement {
        active_node_input_index,
        active_node_output_index,
        hub_oracle_ref_input_index,
        settlement_input_index,
        settlement_redeemer_index,
        new_bond_unlock_time,
      } -> {
        let Transaction {
          inputs,
          outputs,
          reference_inputs,
          redeemers,
          mint,
          validity_range,
          ..
        } = self

        // 1. The new bond unlock time must be equal to the transaction's
        //    validity range upper bound plus the maturity duration.
        //    TODO: Does inclusivity of the upper bound matter here?
        let valid_to =
          utils.get_inclusive_upper_bound_of_interval(validity_range)
        expect valid_to + maturity_duration == new_bond_unlock_time

        let linked_list_eval = {
          // 2. An element from the active operators directory must be spent and
          //    reproduced without affecting the linked list structure.
          let
            m_operator,
            _active_node_input_data,
            active_node_output_data,
          <-
            linked_list.spend_for_updating_elements_data(
              element_input_index: active_node_input_index,
              continued_element_output_index: active_node_output_index,
              element_input_outref: input_out_ref,
              inputs: inputs,
              outputs: outputs,
              tx_mint: mint,
            )

          // 3. The spent element must be a node (i.e. not root).
          expect Some(operator) = m_operator

          // 4. The reproduced node must properly store the new bond unlock
          //    time.
          expect NodeData { bond_unlock_time: m_output_bond_unlock_time } =
            active_node_output_data
          expect m_output_bond_unlock_time == Some(new_bond_unlock_time)

          // 5. Transaction must spend a UTxO from settlement with the Attach
          //    Resolution Claim redeemer. The operator's signature should be
          //    validated there.
          //
          //    TODO: Potential optimization opportunities, i.e. using builtin
          //    functions. This requires implicitly imposing the position of
          //    `operator` field in the redeemer to be at index 0.
          expect settlement.AttachResolutionClaim {
            operator: settlement_redeemer_operator,
            ..
          } =
            settlement.get_spending_redeemer_data(
              hub_oracle_script_hash,
              hub_oracle_ref_input_index,
              settlement_input_index,
              settlement_redeemer_index,
              inputs,
              reference_inputs,
              redeemers,
            )

          // 6. The operator specified in settlement's redeemer must match the
          //    key of the spending active operator node.
          expect settlement_redeemer_operator == operator

          // Done.
          True
        }
        linked_list_eval
          |> finalize_linked_list(active_operators_mint_script_hash)
      }
    }
  }

  else(_) {
    fail
  }
}

validator mint(slashing_penalty: Int, hub_oracle_script_hash: PolicyId) {
  mint(redeemer: MintRedeemer, policy_id: PolicyId, self: Transaction) {
    let node_in_outputs = get_node_inputs(self.inputs, policy_id)
    let node_outputs = get_node_outputs(self.outputs, policy_id)
    let node_mint = tokens(self.mint, policy_id)
    when redeemer is {
      Init -> and {
          quantity_of(self.mint, hub_oracle_script_hash, hub.asset_name) == 1,
          ordered.init(node_outputs, node_mint, policy_id),
        }
      Deinit -> and {
          quantity_of(self.mint, hub_oracle_script_hash, hub.asset_name) == -1,
          ordered.deinit(node_in_outputs, node_mint, policy_id),
        }
      ActivateOperator {
        new_active_operator_key,
        hub_oracle_ref_input_index,
        active_operator_appended_node_output_index,
        active_operator_anchor_node_output_index,
        registered_operators_redeemer_index,
      } -> {
        expect Some(active_node_output) =
          list.at(node_outputs, active_operator_appended_node_output_index)
        expect InlineDatum(active_operator_datum) = active_node_output.datum
        expect parsed_active_operator_datum: NodeDatum = active_operator_datum
        expect active_operator_data: Datum = parsed_active_operator_datum.data
        expect Some(hub_oracle_ref_input) =
          list.at(self.reference_inputs, hub_oracle_ref_input_index)
        expect InlineDatum(hub_oracle_datum) = hub_oracle_ref_input.output.datum
        expect parsed_hub_oracle_datum: hub.Datum = hub_oracle_datum
        expect Some(registered_operators_redeemer_pair) =
          list.at(self.redeemers, registered_operators_redeemer_index)
        expect registered_operators.Activate { node_to_activate_key, .. } =
          registered_operators_redeemer_pair.2nd

        and {
          quantity_of(
            hub_oracle_ref_input.output.value,
            hub_oracle_script_hash,
            hub.asset_name,
          ) == 1,
          ordered.append(
            Key(new_active_operator_key),
            active_operator_appended_node_output_index,
            active_operator_anchor_node_output_index,
            node_in_outputs,
            node_outputs,
            node_mint,
          ),
          active_operator_data.bond_unlock_time == None,
          registered_operators_redeemer_pair.1st == Mint(
            parsed_hub_oracle_datum.registered_operators,
          ),
          node_to_activate_key == new_active_operator_key,
        }
      }
      RemoveOperatorBadState {
        slashed_active_operator_key,
        hub_oracle_ref_input_index,
        active_operator_slashed_node_input_index,
        active_operator_anchor_node_input_index,
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
            hub.asset_name,
          ) == 1,
          ordered.remove(
            Key(slashed_active_operator_key),
            active_operator_slashed_node_input_index,
            active_operator_anchor_node_input_index,
            node_in_outputs,
            node_outputs,
            node_mint,
          ),
          self.fee >= slashing_penalty,
          state_queue_redeemer_pair.1st == Mint(
            parsed_hub_oracle_datum.state_queue,
          ),
          fraudulent_operator == slashed_active_operator_key,
        }
      }
      RemoveOperatorBadSettlement {
        slashed_active_operator_key,
        hub_oracle_ref_input_index,
        active_operator_slashed_node_input_index,
        active_operator_anchor_node_input_index,
        settlement_input_index,
        settlement_redeemer_index,
      } -> {
        let Transaction { inputs, reference_inputs, redeemers, fee, .. } = self
        //
        let settlement_spending_redeemer_data =
          settlement.get_spending_redeemer_data(
            hub_oracle_script_hash,
            hub_oracle_ref_input_index,
            settlement_input_index,
            settlement_redeemer_index,
            inputs,
            reference_inputs,
            redeemers,
          )
        // TODO: Potential optimization opportunities, i.e. using builtin
        // functions. This requires implicitly imposing the position of
        // `operator` field in the redeemer to be at index 0.
        expect settlement.DisproveResolutionClaim { operator, .. } =
          settlement_spending_redeemer_data
        //
        and {
          ordered.remove(
            Key(slashed_active_operator_key),
            active_operator_slashed_node_input_index,
            active_operator_anchor_node_input_index,
            node_in_outputs,
            node_outputs,
            node_mint,
          ),
          fee >= slashing_penalty,
          operator == slashed_active_operator_key,
        }
      }
      RetireOperator {
        active_operator_key,
        hub_oracle_ref_input_index,
        active_operator_removed_node_input_index,
        active_operator_anchor_node_input_index,
        retired_operator_inserted_node_output_index,
        retired_operators_redeemer_index,
      } -> {
        expect Some(active_node_input) =
          list.at(self.inputs, active_operator_removed_node_input_index)
        expect InlineDatum(active_operator_datum) =
          active_node_input.output.datum
        expect parsed_active_operator_datum: NodeDatum = active_operator_datum
        expect active_operator_data: Datum = parsed_active_operator_datum.data
        expect Some(retired_node_output) =
          list.at(self.outputs, retired_operator_inserted_node_output_index)
        expect InlineDatum(retired_operator_datum) = retired_node_output.datum
        expect parsed_retired_operator_datum: NodeDatum = retired_operator_datum
        expect retired_operator_data: RetiredOperatorData =
          parsed_retired_operator_datum.data

        expect Some(hub_oracle_ref_input) =
          list.at(self.reference_inputs, hub_oracle_ref_input_index)
        expect InlineDatum(hub_oracle_datum) = hub_oracle_ref_input.output.datum
        expect parsed_hub_oracle_datum: hub.Datum = hub_oracle_datum
        expect Some(retired_operators_redeemer_pair) =
          list.at(self.redeemers, retired_operators_redeemer_index)
        expect retired_operators.RetireOperator { new_retired_operator_key, .. } =
          retired_operators_redeemer_pair.2nd
        and {
          quantity_of(
            hub_oracle_ref_input.output.value,
            hub_oracle_script_hash,
            hub.asset_name,
          ) == 1,
          ordered.remove(
            Key(active_operator_key),
            active_operator_removed_node_input_index,
            active_operator_anchor_node_input_index,
            node_in_outputs,
            node_outputs,
            node_mint,
          ),
          retired_operators_redeemer_pair.1st == Mint(
            parsed_hub_oracle_datum.retired_operators,
          ),
          new_retired_operator_key == active_operator_key,
          active_operator_data.bond_unlock_time == retired_operator_data.bond_unlock_time,
        }
      }
    }
  }

  else(_) {
    fail
  }
}

fn no_active_operators_minted(mint: Value, policy_id: PolicyId) {
  utils.quantity_of_policy_id(mint, policy_id) == 0
}
