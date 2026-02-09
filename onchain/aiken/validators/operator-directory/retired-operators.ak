use aiken/collection/list
use aiken/interval.{Finite, Interval}
use aiken_design_patterns/linked_list
use cardano/assets.{PolicyId, quantity_of, tokens}
use cardano/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, Redeemer, Transaction,
}
use midgard/hub_oracle as hub
use midgard/operator_directory.{
  deinit, init, validate_operator_transfer_and_get_inserted_data,
}
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

validator mint(hub_oracle_script_hash: PolicyId) {
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
        bond_unlock_time,
        hub_oracle_ref_input_index,
        retired_operator_anchor_element_input_index,
        retired_operator_anchor_element_output_index,
        retired_operator_inserted_node_output_index,
        active_operators_redeemer_index,
      } -> {
        let Transaction {
          inputs,
          outputs,
          reference_inputs,
          mint,
          redeemers,
          ..
        } = tx

        // 1. The active operators policy ID must come from reference authentic
        //    hub oracle UTxO.
        let hub_datum =
          hub.get_datum(
            self.reference_inputs,
            hub_oracle_script_hash,
            hub_oracle_ref_input_index,
          )

        let linked_list_eval = {
          // 2. Generic validations for operator transfer must pass.
          let inserted_node_data <-
            validate_operator_transfer_and_get_inserted_data(
              new_retired_operator_key,
              hub_datum.active_operators,
              active_operators_redeemer_index,
              fn(active_operators_redeemer_data) -> VerificationKeyHash {
                expect active_operators.RetireOperator {
                  active_operator_key,
                  ..
                } = active_operators_redeemer_data
                active_operator_key
              },
              retired_operator_anchor_element_input_index,
              retired_operator_anchor_element_output_index,
              retired_operator_inserted_node_output_index,
              inputs,
              outputs,
              mint,
              redeemers,
            )

          // 3. Newly retired operator's bond unlock time must be preserved from
          //    the active operators set. This equality between the redeemer
          //    argument and active operators datum is checked by the
          //    `active_operators.RetireOperator` redeemer.
          let expected_inserted_node_data: Data = NodeData { bond_unlock_time }
          expect inserted_node_data == expected_inserted_node_data

          // Done.
          True
        }
        linked_list_eval |> finalize_linked_list(own_policy_id)
      }
      RecoverOperatorBond {
        retired_operator_key,
        retired_operator_anchor_element_input_index,
        retired_operator_removed_node_input_index,
        retired_operator_anchor_element_output_index,
      } -> {
        let Transaction {
          inputs,
          outputs,
          mint,
          redeemers,
          validity_range,
          extra_signatories,
          ..
        } = self

        // 1. The retired operator must give consent to the transaction to show
        //    destination of the ADA bond is approved.
        expect retired_operator_key |> utils.has_signed(extra_signatories)

        // TODO: Is ensuring inclusivity of significance here?
        let valid_from =
          utils.get_inclusive_lower_bound_of_interval(validity_range)

        // 2. Specified inputs and output for removal of the retired operator's
        //    node from the linked list must exist at their indices.
        expect Some(retired_operator_anchor_element_input) =
          inputs |> list.at(retired_operator_anchor_element_input_index)
        expect Some(retired_operator_removed_node_input) =
          inputs |> list.at(retired_operator_removed_node_input_index)
        expect Some(retired_operator_anchor_element_output) =
          outputs |> list.at(retired_operator_anchor_element_output_index)

        let linked_list_eval = {
          // 3. Removal of the retiried operator's node from the linked list
          //    must be valid.
          let
            anchor_lovelace_change,
            _m_anchor_key,
            _anchor_data,
            _retired_operator_lovelace,
            retired_operator_node_key,
            retired_operator_node_data,
            _retired_operator_node_link,
          <-
            linked_list.remove(
              retired_operator_anchor_element_input,
              retired_operator_removed_node_input,
              retired_operator_anchor_element_output,
              mint,
            )
          // 4. ADA of the removing node's anchor element must be preserved.
          expect anchor_lovelace_change >= 0

          // 5. The key of the removing node must match the retired operator.
          expect retired_operator_key == retired_operator_node_key

          // 6. The retirement of the operator must be mature. In other words,
          //    lower bound of the transaction must be later than retired
          //    operator's commitment time.
          expect NodeData{bond_unlock_time: Some(bond_unlock_time)} =
            retired_operator_node_data
          expect valid_from >= bond_unlock_time

          // Done.
          True
        }
        linked_list_eval |> finalize_linked_list(own_policy_id)
      }
      RemoveOperatorBadState {
        slashed_retired_operator_key,
        hub_oracle_ref_input_index,
        retired_operator_anchor_element_input_index,
        retired_operator_slashed_node_input_index,
        retired_operator_anchor_element_output_index,
        state_queue_redeemer_index,
      } -> {
        let Transaction {
          inputs,
          outputs,
          reference_inputs,
          mint,
          redeemers,
          fee,
          ..
        } = self

        let linked_list_eval = {
          let hub_datum <-
            slash_operator_and_get_hub_datum(
              hub_oracle_script_hash,
              slashed_retired_operator_key,
              hub_oracle_ref_input_index,
              retired_operator_anchor_element_input_index,
              retired_operator_slashed_node_input_index,
              retired_operator_anchor_element_output_index,
              inputs,
              outputs,
              reference_inputs,
              mint,
              fee,
            )

          // 2. State queue minting script must be invoked via the 
          //    `RemoveFraudulentBlockHeader` redeemer, and the operator
          //    specified there must agree with the
          //    `slashed_retired_operator_key` redeemer argument here.
          let fraudulent_operator_from_state_queue_mint_redeemer =
            state_queue.get_fraudulent_operator_of_removing_block(
              hub_datum,
              state_queue_redeemer_index,
              redeemers,
            )
          expect
            fraudulent_operator_from_state_queue_mint_redeemer == slashed_retired_operator_key

          // Done.
          True
        }
        linked_list_eval |> finalize_linked_list(own_policy_id)
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
