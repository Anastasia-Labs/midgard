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
  finalize_linked_list, root_asset_name,
}
use midgard/common/utils
use midgard/hub_oracle as hub
use midgard/protocol_parameters.{maturity_duration, slashing_penalty}
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

          // 5. Transaction must have the hub oracle's UTxO referenced.
          let hub.Datum { state_queue_addr, .. } =
            hub.get_datum(
              reference_inputs,
              hub_oracle_script_hash,
              hub_oracle_ref_input_index,
            )

          // 6. Transaction must spend a UTxO from state queue with the Commit
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
            utils.get_spending_redeemer_data_at(
              state_queue_addr,
              state_queue_input_index,
              state_queue_redeemer_index,
              inputs,
              redeemers,
            )

          // 7. The operator specified in state queue's redeemer must match the
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

          // 5. Transaction must have the hub oracle's UTxO referenced.
          let hub.Datum { settlement_addr, .. } =
            hub.get_datum(
              reference_inputs,
              hub_oracle_script_hash,
              hub_oracle_ref_input_index,
            )

          // 6. Transaction must spend a UTxO from settlement with the Attach
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
            utils.get_spending_redeemer_data_at(
              settlement_addr,
              settlement_input_index,
              settlement_redeemer_index,
              inputs,
              redeemers,
            )

          // 7. The operator specified in settlement's redeemer must match the
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

validator mint(hub_oracle_script_hash: PolicyId) {
  mint(redeemer: MintRedeemer, own_policy_id: PolicyId, self: Transaction) {
    when redeemer is {
      Init { output_index } -> {
        let Transaction { outputs, mint, .. } = self

        // 1. An output must exist at specified index.
        expect Some(root_output) = outputs |> list.get(output_index)

        // 2. Linked list must be initiated with a void root data, and
        //    validation that a hub oracle NFT is getting minted.
        linked_list.init(
          nonce_validated: quantity_of(
            mint,
            hub_oracle_script_hash,
            hub.asset_name,
          ) == 1,
          produced_element_output: root_output,
          tx_mint: mint,
          root_data_validator: fn(root_data: Data) -> Bool { root_data == Void },
        )
          |> linked_list.run_root_with(own_policy_id, root_asset_name)
      }
      Deinit { input_index } -> {
        let Transaction { inputs, mint, .. } = self

        // 1. An input must exist at specified index.
        expect Some(own_input) = inputs |> list.at(input_index)

        // 2. Hub oracle NFT must be getting burnt.
        expect quantity_of(mint, hub_oracle_script_hash, hub.asset_name)

        // 3. Linked list de-initialization must be valid. No other validations
        //    required.
        expect
          deinit(
            root_input: own_input,
            tx_mint: mint,
            root_data_validator: fn(_) -> Bool { True },
          )
            |> linked_list.run_root_with(
                own_policy_id,
                confirmed_state_asset_name,
              )

        // Done.
        True
      }
      ActivateOperator {
        new_active_operator_key,
        hub_oracle_ref_input_index,
        active_operator_anchor_element_input_index,
        active_operator_anchor_element_output_index,
        active_operator_inserted_node_output_index,
        registered_operators_redeemer_index,
      } -> {
        let Transaction {
          inputs,
          outputs,
          reference_inputs,
          mint,
          redeemers,
          ..
        } = self

        // 1. Transaction must refer to the authentic hub oracle UTxO to extract
        //    the minting policy of the registered operators directory.
        let hub.Datum {
          registered_operators: registered_operators_policy_id,
          ..
        } =
          hub.get_datum(
            reference_inputs,
            hub_oracle_script_hash,
            hub_oracle_ref_input_index,
          )

        // 2. The Activate minting redeemer of the registered operators
        //    directory must be invoked.
        expect registered_operators.Activate { operator_to_activate, .. } =
          redeemers
            |> utils.get_redeemer_at(
                expected_purpose: Mint(registered_operators_policy_id),
                redeemer_index: registered_operators_redeemer_index,
              )

        // 3. The operator indicated in the registered operators minting
        //    redeemer must match the one specified in the redeemer here.
        expect operator_to_activate == new_active_operator_key

        // 4. Specified input and outpus related to the linked list must exist.
        expect Some(active_operator_anchor_element_input) =
          inputs |> list.at(active_operator_anchor_element_input_index)
        expect Some(active_operator_anchor_element_output) =
          outputs |> list.at(active_operator_anchor_element_output_index)
        expect Some(active_operator_inserted_node) =
          outputs |> list.at(active_operator_inserted_node_output_index)

        let linked_list_eval = {
          // 5. Ordered insertion in the linked list must be proper.
          let
            _m_anchor_key,
            _anchor_data,
            inserted_node_key,
            inserted_node_data,
          <-
            linked_list.insert_ordered(
              active_operator_anchor_element_input,
              active_operator_anchor_element_output,
              active_operator_inserted_node,
              mint,
            )

          // 6. The newly inserted node must carry the same operator as the one
          //    provided via the redeemer as its key.
          expect inserted_node_key == new_active_operator_key

          // 7. Newly activated operators must not have any bond unlock times
          //    specified.
          let expected_inserted_node_data: Data =
            NodeData { bond_unlock_time: None }
          expect inserted_node_data == expected_inserted_node_data

          // Done.
          True
        }
        linked_list_eval |> finalize_linked_list(own_policy_id)
      }
      RemoveOperatorBadState {
        slashed_active_operator_key,
        hub_oracle_ref_input_index,
        active_operator_slashed_node_input_index,
        active_operator_anchor_node_input_index,
        state_queue_redeemer_index,
      } -> {
        let Transaction {
          inputs,
          outputs,
          reference_inputs,
          mint,
          redeemers,
          ..
        } = self

        // 1. Transaction must refer to the authentic hub oracle UTxO to extract
        //    the minting policy of the state queue.
        let hub.Datum { state_queue: state_queue_policy_id, .. } =
          hub.get_datum(
            reference_inputs,
            hub_oracle_script_hash,
            hub_oracle_ref_input_index,
          )

        // 2. State queue minting script must be invoked via the 
        //    `CutOffFraudulentBlockHeader`redeemer, and the operator specified
        //    there must agree with the `slashed_active_operator_key` redeemer
        //    argument here.
        expect state_queue.CutOffFraudulentBlockHeader {
          fraudulent_operator,
          ..
        } =
          redeemers
            |> utils.get_redeemer_at(
                expected_purpose: Mint(state_queue_policy_id),
                redeemer_index: state_queue_redeemer_index,
              )
        expect fraudulent_operator == slashed_active_operator_key

        expect Some(hub_oracle_ref_input) =
          list.at(self.reference_inputs, hub_oracle_ref_input_index)
        expect InlineDatum(hub_oracle_datum) = hub_oracle_ref_input.output.datum
        expect parsed_hub_oracle_datum: hub.Datum = hub_oracle_datum
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
        let hub.Datum { settlement_addr, .. } =
          hub.get_datum(
            reference_inputs,
            hub_oracle_script_hash,
            hub_oracle_ref_input_index,
          )
        // TODO: Potential optimization opportunities, i.e. using builtin
        // functions. This requires implicitly imposing the position of
        // `operator` field in the redeemer to be at index 0.
        expect settlement.DisproveResolutionClaim { operator, .. } =
          utils.get_spending_redeemer_data_at(
            settlement_addr,
            settlement_input_index,
            settlement_redeemer_index,
            inputs,
            redeemers,
          )
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
