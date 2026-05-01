use aiken/collection/list
use aiken/crypto.{VerificationKeyHash}
use aiken_design_patterns/linked_list
use cardano/assets.{PolicyId, Value}
use cardano/transaction.{
  Input, Mint, Output, OutputReference, Redeemer, ScriptPurpose, Transaction,
  ValidityRange,
}
use midgard/common/utils
use midgard/hub_oracle as hub
use midgard/operator_directory.{
  deinit, init, slash_fraudulent_operator_and_get_info,
  validate_operator_transfer_and_get_inserted_data,
}
use midgard/operator_directory/active_operators.{
  ActivateOperator, Datum, Deinit, Init, ListStateTransition, MintRedeemer,
  NodeData, OperatorRemovalSchedulerSync, RetireOperator, ShowOperatorIsInactive,
  ShowSchedulerIsAdvancing, SlashOperator, SpendRedeemer, StrikeForInactivity,
  UpdateBondHoldNewSettlement, UpdateBondHoldNewState, finalize_linked_list,
  root_asset_name,
}
use midgard/operator_directory/registered_operators
use midgard/operator_directory/retired_operators
use midgard/protocol_parameters.{
  inactivity_slashing_penalty, maturity_duration, max_inactivity_strikes,
}
use midgard/scheduler
use midgard/settlement
use midgard/state_queue

validator spend(
  active_operators_mint_script_hash: PolicyId,
  hub_oracle_script_hash: PolicyId,
) {
  spend(
    _datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    self: Transaction,
  ) {
    when redeemer is {
      ListStateTransition ->
        linked_list.spend_for_adding_or_removing_an_element(
          active_operators_mint_script_hash,
          self.mint,
        )
      UpdateBondHoldNewState {
        active_operator,
        active_node_input_index,
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

        // 1. Common validations for spending and reproducing an active operator
        //    node must pass.
        let
          _new_bond_unlock_time,
          hub_datum,
        <-
          spend_for_updating_bond_unlock_time(
            active_operator,
            active_operators_mint_script_hash,
            hub_oracle_script_hash,
            active_node_input_index,
            active_node_output_index,
            hub_oracle_ref_input_index,
            own_out_ref,
            inputs,
            outputs,
            reference_inputs,
            mint,
            validity_range,
          )

        // 2. Transaction must spend a UTxO from state queue with the Commit
        //    Block Header redeemer. The operator's signature should be
        //    validated there.
        expect state_queue.CommitBlockHeader {
          operator: state_queue_redeemer_operator,
          ..
        } =
          utils.get_spending_redeemer_data_at(
            hub_datum.state_queue_addr,
            state_queue_input_index,
            state_queue_redeemer_index,
            inputs,
            redeemers,
          )

        // 3. The operator specified in state queue's redeemer must match the
        //    key of the spending active operator node.
        expect state_queue_redeemer_operator == active_operator

        // Done.
        True
      }
      UpdateBondHoldNewSettlement {
        active_operator,
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

        // 1. Common validations for spending and reproducing an active operator
        //    node must pass.
        let
          computed_new_bond_unlock_time,
          hub_datum,
        <-
          spend_for_updating_bond_unlock_time(
            active_operator,
            active_operators_mint_script_hash,
            hub_oracle_script_hash,
            active_node_input_index,
            active_node_output_index,
            hub_oracle_ref_input_index,
            own_out_ref,
            inputs,
            outputs,
            reference_inputs,
            mint,
            validity_range,
          )

        // 2. The new bond unlock time in the redeemer must be valid for other
        //    contracts to rely on.
        expect computed_new_bond_unlock_time == new_bond_unlock_time

        // 3. Transaction must spend a UTxO from settlement with the Attach
        //    Resolution Claim redeemer. The operator's signature should be
        //    validated there.
        expect settlement.AttachResolutionClaim {
          operator: settlement_redeemer_operator,
          ..
        } =
          utils.get_spending_redeemer_data_at(
            hub_datum.settlement_addr,
            settlement_input_index,
            settlement_redeemer_index,
            inputs,
            redeemers,
          )

        // 5. The operator specified in settlement's redeemer must match the
        //    key of the spending active operator node.
        expect settlement_redeemer_operator == active_operator

        // Done.
        True
      }
      StrikeForInactivity {
        active_node_input_index,
        active_node_output_index,
        operator,
        active_node_link: active_node_link_from_redeemer,
        scheduler_input_index,
        scheduler_redeemer_index,
        hub_oracle_ref_input_index,
      } -> {
        let Transaction {
          inputs,
          outputs,
          mint,
          reference_inputs,
          redeemers,
          ..
        } = self

        // Validate the scheduler UTxO is updated accordingly.
        // 1. The authentic hub oracle UTxO must be provided as a reference
        //    input.
        let hub_datum =
          hub.get_datum(
            reference_inputs,
            hub_oracle_script_hash,
            hub_oracle_ref_input_index,
          )

        // 2. The indices specified in the spending redeemer of the scheduler
        //    script must match with the indices in the redeemer here.
        expect scheduler.SpendRedeemer {
          scheduler_input_index: scheduler_input_index_from_scheduler_redeemer,
          advancing_approach: scheduler_advancing_approach,
          ..
        } =
          utils.get_spending_redeemer_data_at(
            hub_datum.scheduler_addr,
            scheduler_input_index,
            scheduler_redeemer_index,
            inputs,
            redeemers,
          )
        expect
          when scheduler_advancing_approach is {
            scheduler.RewindDueToSkippedOperator {
              skipped_operator_node_input_index,
              ..
            } |
            scheduler.GoToNextDueToSkippedOperator {
              skipped_operator_node_input_index,
              ..
            } ->
              // TODO: One of these two might suffice.
              and {
                scheduler_input_index == scheduler_input_index_from_scheduler_redeemer,
                active_node_input_index == skipped_operator_node_input_index,
              }
            _ -> False
          }

        // Validate inactive operator's strike count is incremented properly.
        // 3. Spending and reproducing the linked list node must be proper.
        {
          let
            lovelace_change,
            m_operator,
            active_node_input_data,
            active_node_output_data,
            correct_active_node_link,
          <-
            linked_list.spend_for_updating_elements_data(
              element_input_index: active_node_input_index,
              continued_element_output_index: active_node_output_index,
              element_input_outref: own_out_ref,
              inputs: inputs,
              outputs: outputs,
              tx_mint: mint,
            )

          // 4. ADA of the node must be preserved.
          expect lovelace_change >= 0

          // 5. The indicated operator in redeemer must match the key of the
          //    spent node.
          expect Some(operator) == m_operator

          // 6. The active node's link specified in the redeemer must be
          //    correct for other contracts to use.
          expect active_node_link_from_redeemer == correct_active_node_link

          // 7. The underlying data must only have its strike counts
          //    incremented.
          expect NodeData {
            inactivity_strikes: input_strikes,
            bond_unlock_time: input_bond_unlock_time,
          } = active_node_input_data
          let new_inactivity_strikes = input_strikes + 1
          let expected_output_node_data: Data =
            NodeData {
              inactivity_strikes: new_inactivity_strikes,
              bond_unlock_time: input_bond_unlock_time,
            }
          expect active_node_output_data == expected_output_node_data

          // 8. The new strikes count must not exceed the maximum allowed. This
          //    is there to cover one specific edge case: If the appointed
          //    operator is the only member of the active operators set,
          //    inactivity causes the scheduler to rewind back to the same
          //    operator. Allowing this to happen indefinitely, gives an
          //    attacker the opportunity to constantly spend the operator's node
          //    UTxO and prevent the retirement transaction to go through.
          //
          //    With this check in place, the operator's UTxO cannot be spent
          //    for strikes any further, and the node will be free for transfer
          //    to the retired set.
          expect new_inactivity_strikes <= max_inactivity_strikes

          // Done.
          True
        }
          |> finalize_linked_list(active_operators_mint_script_hash)
      }
    }
  }

  else(_) {
    fail
  }
}

validator mint(
  hub_oracle_script_hash: PolicyId,
  registered_operators_policy_id: PolicyId,
  retired_operators_policy_id: PolicyId,
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
      ActivateOperator {
        new_active_operator_key,
        new_active_operator_bond_unlock_time,
        active_operator_anchor_element_input_index,
        active_operator_anchor_element_output_index,
        active_operator_inserted_node_output_index,
        registered_operators_redeemer_index,
      } -> {
        let Transaction { inputs, outputs, mint, redeemers, .. } = self

        // 1. Generic validations for operator transfer must pass.
        {
          let inserted_node_data <-
            validate_operator_transfer_and_get_inserted_data(
              new_active_operator_key,
              registered_operators_policy_id,
              registered_operators_redeemer_index,
              fn(registered_operators_redeemer_data) -> (
                VerificationKeyHash,
                Bool,
              ) {
                expect registered_operators.ActivateOperator {
                  activating_operator,
                  ..
                } = registered_operators_redeemer_data
                (activating_operator, False)
              },
              active_operator_anchor_element_input_index,
              active_operator_anchor_element_output_index,
              active_operator_inserted_node_output_index,
              inputs,
              outputs,
              mint,
              redeemers,
            )

          // 2. Newly activated operator must have the bond unlock time which
          //    comes from the registered operators node.
          let expected_inserted_node_data: Data =
            NodeData {
              bond_unlock_time: new_active_operator_bond_unlock_time,
              inactivity_strikes: 0,
            }
          expect inserted_node_data == expected_inserted_node_data

          // Done.
          True
        }
          |> finalize_linked_list(own_policy_id)
      }
      RetireOperator {
        active_operator_key,
        hub_oracle_ref_input_index,
        active_operator_anchor_element_input_index,
        active_operator_removed_node_input_index,
        active_operator_anchor_element_output_index,
        retired_operators_redeemer_index,
        penalize_for_inactivity,
        operator_removal_scheduler_sync,
      } -> {
        let Transaction {
          inputs,
          outputs,
          mint,
          reference_inputs,
          redeemers,
          extra_signatories,
          ..
        } = self

        // 1. Retired operators directory must insert a new node via the
        //    `RetireOperator` redeemer, with a matching operator key.
        expect retired_operators.RetireOperator {
          new_retired_operator_key,
          bond_unlock_time: retired_operators_redeemer_bond_unlock_time,
          ..
        } =
          redeemers
            |> utils.get_redeemer_at(
                expected_purpose: Mint(retired_operators_policy_id),
                redeemer_index: retired_operators_redeemer_index,
              )
        expect active_operator_key == new_retired_operator_key

        // 2. Transaction must refer to the authentic hub oracle UTxO.
        let hub_datum =
          hub.get_datum(
            reference_inputs,
            hub_oracle_script_hash,
            hub_oracle_ref_input_index,
          )

        // 3. Specified inputs and output for removal of the active operator's
        //    node from the linked list must exist at their indices.
        expect Some(active_operator_anchor_element_input) =
          inputs |> list.at(active_operator_anchor_element_input_index)
        expect Some(active_operator_removed_node_input) =
          inputs |> list.at(active_operator_removed_node_input_index)
        expect Some(active_operator_anchor_element_output) =
          outputs |> list.at(active_operator_anchor_element_output_index)

        // 4. Linked list's removal logic must be valid for the specified
        //    inputs and output.
        {
          let
            anchor_lovelace_change,
            m_anchor_key,
            _anchor_data,
            _active_operator_lovelace,
            active_operator_node_key,
            active_operator_node_data,
            active_operator_node_link,
          <-
            linked_list.remove(
              active_operator_anchor_element_input,
              active_operator_removed_node_input,
              active_operator_anchor_element_output,
              mint,
            )

          // 5. Anchor element's ADA must be preserved.
          expect anchor_lovelace_change >= 0

          // 6. Key of the removing node must match the operator key specified
          //    via the redeemer.
          expect active_operator_key == active_operator_node_key

          // 7. Ensure scheduler datum is getting updated if needed, or show
          //    that it doesn't need to.
          expect
            operator_removal_scheduler_sync
              |> validate_scheduler_syncs_with_operator_removal(
                  active_operator_key,
                  m_anchor_key,
                  active_operator_node_link,
                  hub_datum,
                  inputs,
                  reference_inputs,
                  redeemers,
                )

          expect NodeData {
            inactivity_strikes,
            bond_unlock_time: removing_nodes_bond_unlock_time,
          } = active_operator_node_data

          // 8. The `bond_unlock_time` redeemer argument from the retired
          //    operators directory must match the removing node's datum. This
          //    allows the minting of the retired operator's node to be able to
          //    rely on its correctness.
          expect
            removing_nodes_bond_unlock_time == retired_operators_redeemer_bond_unlock_time

          if penalize_for_inactivity {
            // 9a. Since retired operators minting script relies on the penalty
            //     flag here, its correctness must be verified.
            expect inactivity_strikes >= max_inactivity_strikes

            // 10a. With enough strikes, anyone can partially slash the
            //      operator,
            expect self.fee >= inactivity_slashing_penalty

            // Done.
            True
          } else {
            // 9b. The penalty flag must be valid.
            expect inactivity_strikes < max_inactivity_strikes

            // 10b. The retiring operator must consent to the transaction.
            expect active_operator_key |> utils.has_signed(extra_signatories)

            // Done.
            True
          }
        }
          |> finalize_linked_list(own_policy_id)
      }
      SlashOperator { slashing_arguments, operator_removal_scheduler_sync } ->
        {
          let
            slashed_operator_from_redeemer,
            m_anchor_key,
            slashed_operator_node_key,
            _slashed_operator_node_data,
            slashed_operator_node_link,
            hub_datum,
            inputs,
            reference_inputs,
            redeemers,
          <-
            slash_fraudulent_operator_and_get_info(
              hub_oracle_script_hash,
              slashing_arguments,
              self,
            )

          // 1. The operator key indicated in the redeemer must match the key of
          //    the removed node from the active operators set.
          expect slashed_operator_node_key == slashed_operator_from_redeemer

          // 2. If operator is currently active, validate scheduler datum is
          //    getting updated. Otherwise, show the currently appointed
          //    operator is different from the slashing operator.
          expect
            operator_removal_scheduler_sync
              |> validate_scheduler_syncs_with_operator_removal(
                  slashed_operator_from_redeemer,
                  m_anchor_key,
                  slashed_operator_node_link,
                  hub_datum,
                  inputs,
                  reference_inputs,
                  redeemers,
                )

          // Done.
          True
        }
          |> finalize_linked_list(own_policy_id)
    }
  }

  else(_) {
    fail
  }
}

fn spend_for_updating_bond_unlock_time(
  active_operator: VerificationKeyHash,
  active_operators_mint_script_hash: ByteArray,
  hub_oracle_script_hash: PolicyId,
  active_node_input_index: Int,
  active_node_output_index: Int,
  hub_oracle_ref_input_index: Int,
  own_out_ref: OutputReference,
  inputs: List<Input>,
  outputs: List<Output>,
  reference_inputs: List<Input>,
  mint: Value,
  validity_range: ValidityRange,
  return: fn(Int, hub.Datum) -> Bool,
) -> Bool {
  // 1. The new bond unlock time must be equal to the transaction's
  //    validity range upper bound plus the maturity duration.
  //    TODO: Does inclusivity of the upper bound matter here?
  let valid_to = utils.get_inclusive_upper_bound_of_interval(validity_range)
  let new_bond_unlock_time = valid_to + maturity_duration

  // 2. An element from the active operators directory must be spent and
  //    reproduced without affecting the linked list structure.
  {
    let
      lovelace_change,
      m_operator,
      active_node_input_data,
      active_node_output_data,
      _active_node_link,
    <-
      linked_list.spend_for_updating_elements_data(
        element_input_index: active_node_input_index,
        continued_element_output_index: active_node_output_index,
        element_input_outref: own_out_ref,
        inputs: inputs,
        outputs: outputs,
        tx_mint: mint,
      )

    // 3. Node's ADA bond must be preserved.
    expect lovelace_change >= 0

    // 4. The spent element must be a node (i.e. not root) such that its key
    //    matches the operator pub key hash specified in the redeemer. This
    //    allows other contracts to rely on the key in the spending redeemer.
    expect Some(active_operator) == m_operator

    // 5. The reproduced node must preserve inactivity strike count and
    //    properly store the new bond unlock time.
    expect NodeData { inactivity_strikes, .. } = active_node_input_data
    let expected_active_node_output_data: Data =
      NodeData {
        bond_unlock_time: Some(new_bond_unlock_time),
        inactivity_strikes,
      }
    expect active_node_output_data == expected_active_node_output_data

    // 6. Transaction must have the hub oracle's UTxO referenced.
    let hub_datum =
      hub.get_datum(
        reference_inputs,
        hub_oracle_script_hash,
        hub_oracle_ref_input_index,
      )

    return(new_bond_unlock_time, hub_datum)
  }
    |> finalize_linked_list(active_operators_mint_script_hash)
}

/// If the subject operator is currently appointed active, this function
/// validates scheduler UTxO is spent. Otherwise, shows that the currently
/// appointed operator is different from the subject operator.
fn validate_scheduler_syncs_with_operator_removal(
  operator_removal_scheduler_sync: OperatorRemovalSchedulerSync,
  subject_operator: VerificationKeyHash,
  subject_operators_anchor_key: Option<linked_list.NodeKey>,
  subject_operators_link: linked_list.Link,
  hub_datum: hub.Datum,
  inputs: List<Input>,
  reference_inputs: List<Input>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
) -> Bool {
  when operator_removal_scheduler_sync is {
    ShowOperatorIsInactive { scheduler_ref_input_index } -> {
      let scheduler_datum =
        reference_inputs
          |> scheduler.get_datum(hub_datum.scheduler, scheduler_ref_input_index)
      when scheduler_datum is {
        scheduler.NoActiveOperators -> True
        scheduler.ActiveOperator { operator, .. } ->
          operator != subject_operator
      }
    }
    ShowSchedulerIsAdvancing {
      scheduler_input_index,
      scheduler_redeemer_index,
      removing_operators_anchor_element_key,
      removing_operator_is_the_last_member,
    } -> {
      // Redeemer arguments must be validated for scheduler to use.
      expect
        removing_operators_anchor_element_key == subject_operators_anchor_key

      let (scheduler_datum_data, scheduler_redeemer_data) =
        utils.get_inline_datum_and_spending_redeemer_data_at(
          hub_datum.scheduler_addr,
          scheduler_input_index,
          scheduler_redeemer_index,
          inputs,
          redeemers,
        )
      expect scheduler.ActiveOperator { operator, .. } = scheduler_datum_data
      expect scheduler.SpendRedeemer {
        scheduler_input_index: input_index_from_scheduler_redeemer,
        advancing_approach,
        ..
      } = scheduler_redeemer_data
      expect operator == subject_operator
      expect scheduler_input_index == input_index_from_scheduler_redeemer
      expect
        when advancing_approach is {
          scheduler.GoToNextDueToOperatorRemoval { .. } |
          scheduler.RewindDueToOperatorRemoval { .. } -> True
          _ -> False
        }
      when subject_operators_link is {
        None -> removing_operator_is_the_last_member == True
        Some(_) -> removing_operator_is_the_last_member == False
      }
    }
  }
}
