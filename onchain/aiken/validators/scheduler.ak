use aiken/collection/list
use aiken/crypto.{VerificationKeyHash}
use aiken/interval
use aiken/primitive/bytearray
use aiken_design_patterns/linked_list
use aiken_design_patterns/singular_utxo_indexer
use cardano/assets.{PolicyId}
use cardano/transaction.{
  Input, Output, OutputReference, Transaction, ValidityRange,
}
use midgard/common/utils
use midgard/hub_oracle as hub
use midgard/operator_directory/active_operators
use midgard/operator_directory/registered_operators
use midgard/scheduler.{
  Advance, Datum, Deinit, Init, MintRedeemer, Rewind, SpendRedeemer, asset_name,
  shift_end_time,
}

validator mint(hub_oracle_script_hash: PolicyId) {
  mint(redeemer: MintRedeemer, own_policy_id: PolicyId, self: Transaction) {
    let Transaction { mint, .. } = self
    when redeemer is {
      Init -> and {
          Pair(hub.asset_name, 1) == utils.get_singleton_asset_with_policy(
            mint,
            hub_oracle_script_hash,
          ),
          Pair(asset_name, 1) == utils.get_singleton_asset_with_policy(
            mint,
            own_policy_id,
          ),
        }
      Deinit -> and {
          Pair(hub.asset_name, -1) == utils.get_singleton_asset_with_policy(
            mint,
            hub_oracle_script_hash,
          ),
          Pair(asset_name, -1) == utils.get_singleton_asset_with_policy(
            mint,
            own_policy_id,
          ),
        }
    }
  }

  else(_) {
    fail
  }
}

validator spend(
  registered_operators_policy_id: PolicyId,
  active_operators_policy_id: PolicyId,
  scheduler_policy_id: PolicyId,
) {
  spend(
    m_datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    self: Transaction,
  ) {
    when redeemer is {
      Advance {
        scheduler_input_index,
        scheduler_output_index,
        active_node_ref_input_index,
      } -> {
        let Transaction {
          inputs,
          outputs,
          reference_inputs,
          extra_signatories,
          validity_range,
          ..
        } = self

        // 1. Apart from validating proper reproduction of the authentic input,
        //    the consent of the current operator must be verified if their
        //    shift has not yet ended.
        let
          input_operator,
          output_operator,
        <-
          validate_end_of_shift_and_get_operators(
            scheduler_policy_id,
            scheduler_input_index,
            scheduler_output_index,
            m_datum,
            own_out_ref,
            inputs,
            outputs,
            extra_signatories,
            validity_range,
          )

        // 2. The authentic reference input of the new active operator node must
        //    exist at the specified index.
        expect Some(active_node_ref_input) =
          list.at(reference_inputs, active_node_ref_input_index)
        {
          let
            _active_node_lovelace,
            m_active_node_key,
            _active_node_data,
            active_node_link,
          <- linked_list.get_element_info(active_node_ref_input.output)

          // 3. The referenced active operator element has to be a node, and its
          //    key must match the new active operator.
          expect Some(output_operator) == m_active_node_key

          // 4. If the new active operator node has a linked operator, the key
          //    of the linked operator must match or exceed the previous
          //    operator's key.
          expect
            when active_node_link is {
              None -> True
              Some(active_node_linked_operator) ->
                bytearray.compare(input_operator, active_node_linked_operator) != Greater
            }

          // Done.
          True
        }
          |> active_operators.finalize_linked_list(active_operators_policy_id)
      }
      Rewind {
        scheduler_input_index,
        scheduler_output_index,
        active_node_ref_input_index,
        active_root_ref_input_index,
        registered_element_ref_input_index,
      } -> {
        let Transaction {
          inputs,
          outputs,
          reference_inputs,
          extra_signatories,
          validity_range,
          ..
        } = self

        // 1. Apart from validating proper reproduction of the authentic input,
        //    the consent of the current operator must be verified if their
        //    shift has not yet ended.
        let
          input_operator,
          output_operator,
        <-
          validate_end_of_shift_and_get_operators(
            scheduler_policy_id,
            scheduler_input_index,
            scheduler_output_index,
            m_datum,
            own_out_ref,
            inputs,
            outputs,
            extra_signatories,
            validity_range,
          )

        // 2. Active and registered operatorelements must exist at the specified
        //    indices.
        expect Some(active_node_ref_input) =
          reference_inputs |> list.at(active_node_ref_input_index)
        expect Some(active_root_ref_input) =
          reference_inputs |> list.at(active_root_ref_input_index)
        expect Some(registered_element_ref_input) =
          reference_inputs |> list.at(registered_element_ref_input_index)

        let last_operator_node_linked_list_eval = {
          let
            _active_node_lovelace,
            m_active_node_key,
            _active_node_data,
            active_node_link,
          <- linked_list.get_element_info(active_node_ref_input.output)

          // 3. The referenced active operator element has to be a node, and its
          //    key must match the new active operator.
          expect Some(output_operator) == m_active_node_key

          // 4. The node has to be the last in the list.
          None == active_node_link
        }
        expect
          last_operator_node_linked_list_eval
            |> active_operators.finalize_linked_list(active_operators_policy_id)

        let active_operators_root_linked_list_eval = {
          let
            _active_root_lovelace,
            m_active_root_key,
            _active_root_data,
            active_root_link,
          <- linked_list.get_element_info(active_root_ref_input.output)

          // 5. The referenced element has to be root.
          expect None == m_active_root_key

          // TODO: Handle the case of empty active operators linked list
          //       properly. Related to
          //       [#296](https://github.com/anastasia-labs/midgard/issues/296)
          //       and
          //       [#391](https://github.com/anastasia-labs/midgard/issues/391).
          when active_root_link is {
            None -> False
            Some(active_root_linked_operator) ->
              bytearray.compare(input_operator, active_root_linked_operator) != Greater
          }
        }
        expect
          active_operators_root_linked_list_eval
            |> active_operators.finalize_linked_list(active_operators_policy_id)

        let registered_operators_linked_list_eval = {
          let
            _registered_element_lovelace,
            m_registered_element_key,
            registered_element_data,
            registered_element_link,
          <- linked_list.get_element_info(registered_element_ref_input.output)

          // 6. If the referenced registered operator element is a node, its
          //    activation time must not have reached yet.
          expect
            when m_registered_element_key is {
              None -> True
              Some(_) -> {
                expect registered_operators.NodeData { activation_time } =
                  registered_element_data
                validity_range |> interval.is_entirely_before(activation_time)
              }
            }

          // 7. The referenced registered operator element has to be at the very
          //    end of the list.
          None == registered_element_link
        }
        expect
          registered_operators_linked_list_eval
            |> registered_operators.finalize_linked_list(
                registered_operators_policy_id,
              )

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}

fn validate_end_of_shift_and_get_operators(
  scheduler_policy_id: PolicyId,
  scheduler_input_index: Int,
  scheduler_output_index: Int,
  m_datum: Option<Datum>,
  own_out_ref: OutputReference,
  inputs: List<Input>,
  outputs: List<Output>,
  extra_signatories: List<VerificationKeyHash>,
  validity_range: ValidityRange,
  return: fn(VerificationKeyHash, VerificationKeyHash) -> Bool,
) -> Bool {
  // 1. Proper datum must be attached to the scheduler input.
  expect Some(Datum { operator: input_operator, start_time: input_start_time }) =
    m_datum

  // 2. Output and the correct input must exist at the specified indices.
  let
    input,
    output,
  <-
    singular_utxo_indexer.one_to_one(
      input_index: scheduler_input_index,
      output_index: scheduler_output_index,
      own_ref: own_out_ref,
      inputs: inputs,
      outputs: outputs,
      // Double satisfaction is prevented by requiring the same NFT present in
      // both the input and the output.
      double_satisfaction_prevented: True,
    )

  // 3. Both input and output must have the scheduler NFT, and the output
  //    datum must be properly formed.
  expect Datum { operator: output_operator, start_time: output_start_time } =
    utils.authenticate_input_output_and_get_output_datum_data(
      input,
      output,
      scheduler_policy_id,
    )

  // 4. The start time of the new shift must be the end time of the
  //    previous shift.
  expect output_start_time == shift_end_time(input_start_time)

  // 5. Either the start time of the new shift is in the past, or it's
  //    contained within the transaction's validity range, in which case
  //    the previous operator (who is currently active) must consent to
  //    advancing the schedule.
  //
  //    TODO: See [#416](https://github.com/anastasia-labs/midgard/issues/416).
  expect or {
      validity_range |> interval.is_entirely_after(output_start_time),
      and {
        input_operator |> utils.has_signed(extra_signatories),
        {
          let (_inclusive_lower_bound, inclusive_upper_bound) =
            utils.get_inclusive_bounds_of_a_short_validity_range(validity_range)
          output_start_time < inclusive_upper_bound
        },
      },
    }

  return(input_operator, output_operator)
}
