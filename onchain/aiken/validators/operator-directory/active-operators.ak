use aiken/collection/list
use aiken/crypto.{VerificationKeyHash}
use aiken/interval.{Finite, Interval}
use aiken_design_patterns/linked_list
use cardano/assets.{PolicyId, Value, quantity_of, tokens}
use cardano/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, Transaction,
}
use midgard/common/utils
use midgard/hub_oracle as hub
use midgard/operator_directory.{
  deinit, init, validate_operator_transfer_and_get_incoming_data,
}
use midgard/operator_directory/active_operators.{
  ActivateOperator, Datum, Deinit, Init, ListStateTransition, MintRedeemer,
  RemoveOperatorBadSettlement, RemoveOperatorBadState, RetireOperator,
  SpendRedeemer, UpdateBondHoldNewSettlement, UpdateBondHoldNewState,
  finalize_linked_list, root_asset_name,
}
use midgard/operator_directory/registered_operators
use midgard/operator_directory/retired_operators
use midgard/protocol_parameters.{maturity_duration, slashing_penalty}
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
        active_node_input_index,
        active_node_output_index,
        hub_oracle_ref_input_index,
        state_queue_input_index,
        state_queue_redeemer_index,
      } -> {
        // 1. Common validations for spending and reproducing an active operator
        //    node must pass.
        let
          _new_bond_unlock_time,
          active_operator_key,
          hub_datum,
        <-
          spend_for_updating_bond_unlock_time(
            active_operators_mint_script_hash,
            hub_oracle_script_hash,
            active_node_input_index,
            active_node_output_index,
            hub_oracle_ref_input_index,
            own_out_ref,
            self,
          )

        // 2. Transaction must spend a UTxO from state queue with the Commit
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
            hub_datum.state_queue_addr,
            state_queue_input_index,
            state_queue_redeemer_index,
            inputs,
            redeemers,
          )

        // 3. The operator specified in state queue's redeemer must match the
        //    key of the spending active operator node.
        expect state_queue_redeemer_operator == active_operator_key

        // Done.
        True
      }
      UpdateBondHoldNewSettlement {
        active_node_input_index,
        active_node_output_index,
        hub_oracle_ref_input_index,
        settlement_input_index,
        settlement_redeemer_index,
        new_bond_unlock_time,
      } -> {
        // 1. Common validations for spending and reproducing an active operator
        //    node must pass.
        let
          computed_new_bond_unlock_time,
          active_operator_key,
          hub_datum,
        <-
          spend_for_updating_bond_unlock_time(
            active_operators_mint_script_hash,
            hub_oracle_script_hash,
            active_node_input_index,
            active_node_output_index,
            hub_oracle_ref_input_index,
            own_out_ref,
            self,
          )

        // 2. The new bond unlock time in the redeemer must be valid for other
        //    contracts to rely on.
        expect computed_new_bond_unlock_time == new_bond_unlock_time

        // 3. Transaction must spend a UTxO from settlement with the Attach
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
            hub_datum.settlement_addr,
            settlement_input_index,
            settlement_redeemer_index,
            inputs,
            redeemers,
          )

        // 4. The operator specified in settlement's redeemer must match the
        //    key of the spending active operator node.
        expect settlement_redeemer_operator == active_operator_key

        // Done.
        True
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
        active_operator_anchor_element_input_index,
        active_operator_anchor_element_output_index,
        active_operator_inserted_node_output_index,
        registered_operators_redeemer_index,
      } -> {
        let Transaction { inputs, outputs, mint, redeemers, .. } = tx

        let linked_list_eval = {
          // 1. Generic validations for operator transfer must pass.
          let inserted_node_data <-
            validate_operator_transfer_and_get_inserted_data(
              new_active_operator_key,
              registered_operators_policy_id,
              registered_operators_redeemer_index,
              fn(registered_operators_redeemer_data) -> VerificationKeyHash {
                expect registered_operators.ActivateOperator {
                  activating_operator,
                  ..
                } = registered_operators_redeemer_data
                activating_operator
              },
              active_operator_anchor_element_input_index,
              active_operator_anchor_element_output_index,
              active_operator_inserted_node_output_index,
              inputs,
              outputs,
              mint,
              redeemers,
            )

          // 2. Newly activated operator must not have any bond unlock time
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
        active_operator_anchor_element_input_index,
        active_operator_slashed_node_input_index,
        active_operator_anchor_element_output_index,
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
        // 1. Fraudulent active operator must be properly removed from the
        //    linked list.
        let hub_datum <-
          burn_for_operator_removal_and_get_hub_datum(
            own_policy_id,
            hub_oracle_script_hash,
            slashed_active_operator_key,
            hub_oracle_ref_input_index,
            active_operator_anchor_element_input_index,
            active_operator_slashed_node_input_index,
            active_operator_anchor_element_output_index,
            inputs,
            outputs,
            reference_inputs,
            mint,
            fee,
          )

        // 2. State queue minting script must be invoked via the 
        //    `RemoveFraudulentBlockHeader` redeemer, and the operator specified
        //    there must agree with the `slashed_active_operator_key` redeemer
        //    argument here.
        expect state_queue.RemoveFraudulentBlockHeader {
          fraudulent_operator,
          ..
        } =
          redeemers
            |> utils.get_redeemer_at(
                expected_purpose: Mint(hub_datum.state_queue),
                redeemer_index: state_queue_redeemer_index,
              )
        expect fraudulent_operator == slashed_active_operator_key

        // Done.
        True
      }
      RemoveOperatorBadSettlement {
        slashed_active_operator_key,
        hub_oracle_ref_input_index,
        active_operator_anchor_element_input_index,
        active_operator_slashed_node_input_index,
        active_operator_anchor_element_output_index,
        settlement_input_index,
        settlement_redeemer_index,
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
        // 1. Fraudulent active operator must be properly removed from the
        //    linked list.
        let hub_datum <-
          burn_for_operator_removal_and_get_hub_datum(
            own_policy_id,
            hub_oracle_script_hash,
            slashed_active_operator_key,
            hub_oracle_ref_input_index,
            active_operator_anchor_element_input_index,
            active_operator_slashed_node_input_index,
            active_operator_anchor_element_output_index,
            inputs,
            outputs,
            reference_inputs,
            mint,
            fee,
          )

        // 2. A settlement UTxO must be getting spent via the
        //    `DisproveResolutionClaim` redeemer.
        //
        //    Let `operator` be the redeemer argument provided to the settlement
        //    contract.
        //
        // TODO: Potential optimization opportunities, i.e. using builtin
        // functions. This requires implicitly imposing the position of
        // `operator` field in the redeemer to be at index 0.
        expect settlement.DisproveResolutionClaim { operator, .. } =
          utils.get_spending_redeemer_data_at(
            hub_datum.settlement_addr,
            settlement_input_index,
            settlement_redeemer_index,
            inputs,
            redeemers,
          )

        // 3. `operator` must match the removing active operator node's key.
        expect operator == slashed_active_operator_key

        // Done.
        True
      }
      RetireOperator {
        active_operator_key,
        active_operator_anchor_element_input_index,
        active_operator_removed_node_input_index,
        active_operator_anchor_element_output_index,
        retired_operators_redeemer_index,
      } -> {
        let Transaction {
          inputs,
          outputs,
          reference_inputs,
          mint,
          redeemers,
          extra_signatories,
          ..
        } = self

        // 1. The retiring operator must consent to the transaction.
        expect active_operator_key |> utils.has_signed(extra_signatories)

        // 2. Retired operators directory must insert a new node via the
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

        // 3. Specified inputs and output for removal of the active operator's
        //    node from the linked list must exist at their indices.
        expect Some(active_operator_anchor_element_input) =
          inputs |> list.at(active_operator_anchor_element_input_index)
        expect Some(active_operator_removed_node_input) =
          inputs |> list.at(active_operator_removed_node_input_index)
        expect Some(active_operator_anchor_element_output) =
          outputs |> list.at(active_operator_anchor_element_output_index)

        let linked_list_eval = {
          // 4. Linked list's removal logic must be valid for the specified
          //    inputs and output.
          let
            anchor_lovelace_change,
            _m_anchor_key,
            _anchor_data,
            _active_operator_lovelace,
            active_operator_node_key,
            active_operator_node_data,
            _active_operator_node_link,
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

          // 7. The `bond_unlock_time` redeemer argument from the retired
          //    operators directory must match the removing node's datum. This
          //    allows the minting of the retired operator's node to be able to
          //    rely on its correctness.
          expect NodeData { bond_unlock_time } = active_operator_node_data
          expect bond_unlock_time == retired_operators_redeemer_bond_unlock_time

          // Done.
          True
        }
        linked_list_eval |> finalize_linked_list(own_policy_id)
      }
    }
  }

  else(_) {
    fail
  }
}

fn spend_for_updating_bond_unlock_time(
  active_operators_mint_script_hash: ByteArray,
  hub_oracle_script_hash: PolicyId,
  active_node_input_index: Int,
  active_node_output_index: Int,
  hub_oracle_ref_input_index: Int,
  own_out_ref: OutputReference,
  tx: Transaction,
  return: fn(Int, VerificationKeyHash, hub.Datum) -> Bool,
) -> Bool {
  let Transaction {
    inputs,
    outputs,
    reference_inputs,
    redeemers,
    mint,
    validity_range,
    ..
  } = tx

  // 1. The new bond unlock time must be equal to the transaction's
  //    validity range upper bound plus the maturity duration.
  //    TODO: Does inclusivity of the upper bound matter here?
  let valid_to = utils.get_inclusive_upper_bound_of_interval(validity_range)
  let new_bond_unlock_time = valid_to + maturity_duration

  let linked_list_eval = {
    // 2. An element from the active operators directory must be spent and
    //    reproduced without affecting the linked list structure.
    let
      lovelace_change,
      m_operator,
      _active_node_input_data,
      active_node_output_data,
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

    // 4. The spent element must be a node (i.e. not root).
    expect Some(operator) = m_operator

    // 5. The reproduced node must properly store the new bond unlock
    //    time.
    expect NodeData { bond_unlock_time: m_output_bond_unlock_time } =
      active_node_output_data
    expect m_output_bond_unlock_time == Some(new_bond_unlock_time)

    // 6. Transaction must have the hub oracle's UTxO referenced.
    let hub_datum =
      hub.get_datum(
        reference_inputs,
        hub_oracle_script_hash,
        hub_oracle_ref_input_index,
      )

    return(new_bond_unlock_time, operator, hub_datum)
  }
  linked_list_eval
    |> finalize_linked_list(active_operators_mint_script_hash)
}

fn burn_for_operator_removal_and_get_hub_datum(
  active_operators_mint_script_hash: ByteArray,
  hub_oracle_script_hash: PolicyId,
  slashed_active_operator_key: VerificationKeyHash,
  hub_oracle_ref_input_index: Int,
  active_operator_anchor_element_input_index: Int,
  active_operator_slashed_node_input_index: Int,
  active_operator_anchor_element_output_index: Int,
  inputs: List<Input>,
  outputs: List<Output>,
  reference_inputs: List<Input>,
  mint: Value,
  fee: Int,
  return: fn(hub.Datum) -> Bool,
) -> Bool {
  // 1. Transaction must refer to the authentic hub oracle UTxO to extract the
  //    minting policy of the state queue.
  let hub_datum =
    hub.get_datum(
      reference_inputs,
      hub_oracle_script_hash,
      hub_oracle_ref_input_index,
    )

  // 2. Specified inputs and output for removal of the active operator's node
  //    from the linked list must exist at their indices.
  expect Some(active_operator_anchor_element_input) =
    inputs |> list.at(active_operator_anchor_element_input_index)
  expect Some(active_operator_slashed_node_input) =
    inputs |> list.at(active_operator_slashed_node_input_index)
  expect Some(active_operator_anchor_element_output) =
    outputs |> list.at(active_operator_anchor_element_output_index)

  let linked_list_eval = {
    // 3. Linked list's removal logic must be valid for the specified inputs and
    //    output.
    let
      anchor_lovelace_change,
      _m_anchor_key,
      _anchor_data,
      _active_operator_slashed_node_lovelace,
      active_operator_slashed_node_key,
      _active_operator_slashed_node_data,
      _active_operator_slashed_node_link,
    <-
      linked_list.remove(
        active_operator_anchor_element_input,
        active_operator_slashed_node_input,
        active_operator_anchor_element_output,
        mint,
      )

    // 4. ADA of anchor element must be preserved.
    expect anchor_lovelace_change >= 0

    // 5. Key of the removing node must be that of the fraudulent operator's
    //    specified via the redeemer.
    expect active_operator_slashed_node_key == slashed_active_operator_key

    // 6. Slashing penalty must be paid to the Cardano treasury via the
    //    transaction fee. Fraud prover is assumed to be the signer of the
    //    transaction, and therefore will receive the remaining bond.
    expect fee >= slashing_penalty

    // Done.
    return(hub_datum)
  }
  linked_list_eval |> finalize_linked_list(own_policy_id)
}
