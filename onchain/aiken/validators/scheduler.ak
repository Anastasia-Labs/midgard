use aiken/collection/list
use aiken/crypto.{ScriptHash, VerificationKeyHash}
use aiken/interval
use aiken/math.{max}
use aiken/primitive/bytearray
use aiken_design_patterns/linked_list
use aiken_design_patterns/singular_utxo_indexer
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{
  Input, Output, OutputReference, Redeemer, ScriptPurpose, Transaction,
  ValidityRange,
}
use midgard/common/types.{PosixTime}
use midgard/common/utils
use midgard/hub_oracle as hub
use midgard/ledger_state
use midgard/operator_directory/active_operators
use midgard/operator_directory/registered_operators
use midgard/protocol_parameters.{operator_inactivity_timeout}
use midgard/scheduler.{
  ActiveOperator, AdvancingApproach, AppointFirstOperator, Datum, Deinit,
  GoToNextDueToEndOfShift, GoToNextDueToOperatorRemoval,
  GoToNextDueToSkippedOperator, Init, MintRedeemer, NoActiveOperators,
  OperatorRetirement, OperatorSlashing, RewindDueToEndOfShift,
  RewindDueToOperatorRemoval, RewindDueToSkippedOperator, SpendRedeemer,
  asset_name, shift_end_time,
}
use midgard/state_queue

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
  active_operators_address: Address,
  active_operators_policy_id: PolicyId,
  scheduler_policy_id: PolicyId,
  hub_oracle_script_hash: PolicyId,
) {
  spend(
    m_datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    self: Transaction,
  ) {
    let SpendRedeemer {
      scheduler_input_index,
      scheduler_output_index,
      advancing_approach,
    } = redeemer

    let Transaction {
      inputs,
      outputs,
      reference_inputs,
      extra_signatories,
      validity_range,
      ..
    } = self

    expect Some(datum) = m_datum

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
        // Double satisfaction is prevented by requiring the same NFT present
        // in both the input and the output.
        double_satisfaction_prevented: True,
      )

    // 3. Both input and output must have the scheduler NFT, and the output must
    //    have an inline datum.
    let output_datum_data =
      utils.authenticate_input_output_and_get_output_datum_data(
        input,
        output,
        scheduler_policy_id,
      )

    when advancing_approach is {
      RewindDueToEndOfShift {
        active_operators_root_ref_input_index,
        active_operators_last_node_ref_input_index,
        registered_element_ref_input_index,
      } -> {
        // 4a. Input and output datums must be valid, and time conditions and
        //     potentially next operator's consent must be met.
        let (previous_shifts_operator, new_shifts_operator) =
          validate_end_of_shift_and_get_operators(
            datum,
            output_datum_data,
            validity_range,
            extra_signatories,
          )

        // 5a. The active operators root UTxO must point to the previous
        //     operator.
        expect
          validate_root_is_reached(
            active_operators_policy_id,
            previous_shifts_operator,
            active_operators_root_ref_input_index,
            reference_inputs,
          )

        // 6a. The last node in the active operators set must have a key that
        //     matches the new shift's operator.
        expect
          validate_operator_is_the_last_active_node(
            active_operators_policy_id,
            active_operators_last_node_ref_input_index,
            new_shifts_operator,
            reference_inputs,
          )

        // 7a. Registered operator element must exist at the specified index.
        expect Some(registered_element_ref_input) =
          reference_inputs |> list.at(registered_element_ref_input_index)

        {
          let
            _registered_element_lovelace,
            m_registered_element_key,
            _registered_element_data,
            registered_element_link,
          <- linked_list.get_element_info(registered_element_ref_input.output)

          // 8a. If the referenced registered operator element is a node, its
          //     activation time must not have been reached yet.
          expect
            when m_registered_element_key is {
              None -> True
              Some(registered_node_key) -> {
                let activation_time =
                  registered_operators.node_key_to_activation_time(
                    registered_node_key,
                  )
                validity_range |> interval.is_entirely_before(activation_time)
              }
            }

          // 9a. The referenced registered operator element has to be at the
          //     very end of the list to ensure it has the earliest activation
          //     time.
          expect None == registered_element_link

          // Done.
          True
        }
          |> registered_operators.finalize_linked_list(
              registered_operators_policy_id,
            )
      }
      GoToNextDueToEndOfShift { new_shifts_operator_node_ref_input_index } -> {
        // 4b. Input and output datums must be valid, and time conditions and
        //     potentially next operator's consent must be met.
        let (previous_shifts_operator, new_shifts_operator) =
          validate_end_of_shift_and_get_operators(
            datum,
            output_datum_data,
            validity_range,
            extra_signatories,
          )

        // 5b. The referenced node in the active operators set must have a key
        //     that matches the new shift's operator, and a link that points to
        //     the previous operator.
        expect
          validate_new_shifts_node_is_next(
            active_operators_policy_id,
            new_shifts_operator_node_ref_input_index,
            previous_shifts_operator,
            new_shifts_operator,
            reference_inputs,
          )

        // Done.
        True
      }
      RewindDueToSkippedOperator {
        active_operators_root_ref_input_index,
        skipped_operator_node_input_index,
        active_operators_spend_redeemer_index,
        state_queue_ref_input_index,
        m_active_operators_last_node_ref_input_index,
      } -> {
        // 4c. Input and output datums must both have an appointed operator, and
        //     the new shift's start time must follow the previous one
        //     correctly.
        let
          previous_shifts_operator,
          new_shifts_operator,
          previous_shifts_start_time,
          new_shifts_start_time,
        <- validate_active_datums_and_get_info(input_datum, output_datum_data)

        // 5c. The referenced active operators root UTxO must point to the
        //     previous operator.
        expect
          validate_root_is_reached(
            active_operators_policy_id,
            previous_shifts_operator,
            active_operators_root_ref_input_index,
            reference_inputs,
          )

        // 6c. Inactive operator (i.e. previous shift's operator) must be
        //     receiving a strike, and the last element in the state queue must
        //     be old enough for the operator to be considered inactive.
        let inactive_operators_link =
          validate_operator_inactivity_and_get_its_link(
            active_operators_address,
            hub_oracle_script_hash,
            scheduler_input_index,
            skipped_operator_node_input_index,
            active_operators_spend_redeemer_index,
            state_queue_ref_input_index,
            hub_oracle_ref_input_index,
            previous_shifts_operator,
            previous_shifts_start_time,
            reference_inputs,
            redeemers,
            validity_range,
          )

        // 7c. The new shift must start some time in a close future.
        expect
          validate_new_shifts_start_time_after_skipping(
            new_shifts_start_time,
            validity_range,
          )

        expect
          when m_active_operators_last_node_ref_input_index is {
            Some(active_operators_last_node_ref_input_index) -> {
              // 8c. Input and output operators must not be identical.
              //     TODO: Is this check needed? I.e., is it possible to refer
              //           to a UTxO that is spent in the same transaction?
              expect previous_shifts_operator != new_shifts_operator

              // 9c. The last node in the active operators set must have a key
              //     that matches the new shift's operator from output datum.
              validate_operator_is_the_last_active_node(
                active_operators_policy_id,
                active_operators_last_node_ref_input_index,
                new_shifts_operator,
                reference_inputs,
              )
            }
            None -> {
              // 8c'. Without an additional reference input index for the last
              //      operator in the active set, it is expected that the
              //      inactive operator is the only member of the set.
              expect previous_shifts_operator == new_shifts_operator
              None == inactive_operators_link
            }
          }

        // Done.
        True
      }
      GoToNextDueToSkippedOperator {
        new_shifts_operator_node_ref_input_index,
        skipped_operator_node_input_index,
        active_operators_spend_redeemer_index,
      } -> {
        // 4d. Input and output datums must both have an appointed operator, and
        //     the new shift's start time must follow the previous one
        //     correctly.
        let
          previous_shifts_operator,
          new_shifts_operator,
          previous_shifts_start_time,
          new_shifts_start_time,
        <- validate_active_datums_and_get_info(input_datum, output_datum_data)

        // 5d. Inactive operator (i.e. previous shift's operator) must be
        //     receiving a strike, and the last element in the state queue must
        //     be old enough for the operator to be considered inactive.
        expect _inactive_operators_link =
          validate_operator_inactivity_and_get_its_link(
            active_operators_address,
            hub_oracle_script_hash,
            scheduler_input_index,
            skipped_operator_node_input_index,
            active_operators_spend_redeemer_index,
            state_queue_ref_input_index,
            hub_oracle_ref_input_index,
            previous_shifts_operator,
            previous_shifts_start_time,
            reference_inputs,
            redeemers,
            validity_range,
          )

        // 6d. The new shift must start some time in a close future.
        expect
          validate_new_shifts_start_time_after_skipping(
            new_shifts_start_time,
            validity_range,
          )

        // 7d. The referenced new shift's operator's node in the active set must
        //     have a key that matches the new shift's operator in the output
        //     datum, and a link that points to the previous operator.
        expect
          validate_new_shifts_node_is_next(
            active_operators_policy_id,
            new_shifts_operator_node_ref_input_index,
            previous_shifts_operator,
            new_shifts_operator,
            reference_inputs,
          )

        // Done.
        True
      }
      RewindDueToOperatorRemoval {
        active_operators_last_node_ref_input_index,
        active_operators_mint_redeemer_index,
        m_active_operators_last_node_ref_input_index,
        removal_reason,
      } -> {
        // 4e. An active operator must be appointed.
        expect ActiveOperator {
          operator: previous_shifts_operator,
          start_time: previous_shifts_start_time,
        } = datum

        // 5e. The previous shift's operator must be getting removed from the
        //     active operators set, where its mint redeemer provides some info
        //     for this endpoint to use.
        let
          removing_operators_anchor_element_key,
          last_operator_is_removed,
        <-
          validate_active_operator_removal(
            active_operators_policy_id,
            active_operators_mint_redeemer_index,
            removal_reason,
            previous_shifts_operator,
            redeemers,
          )

        if last_operator_is_removed {
          // 6e. Ensure correctness of redeemer (TODO: might not be needed).
          expect None == m_active_operators_last_node_ref_input_index

          // 7e. Output scheduler datum must declare unavailability of any
          //    active operators.
          expect NoActiveOperators = output_datum_data

          // Done.
          True
        } else {
          expect ActiveOperator {
            operator: new_shifts_operator,
            start_time: new_shifts_start_time,
          } = output_datum_data

          // TODO: This might not be needed.
          expect new_shifts_operator != previous_shifts_operator

          // 6e'. Reference input index for last node in the active operators
          //      set must be provided in redeemer.
          expect Some(active_operators_last_node_ref_input_index) =
            m_active_operators_last_node_ref_input_index

          // 7e'. The last node in the active operators set must have a key
          //      that matches the new shift's operator.
          expect
            validate_operator_is_the_last_active_node(
              active_operators_policy_id,
              active_operators_last_node_ref_input_index,
              new_shifts_operator,
              reference_inputs,
            )

          // Done.
          True
        }
      }
      GoToNextDueToOperatorRemoval { .. } -> todo
      AppointFirstOperator { new_shifts_operator_node_ref_input_index } -> todo
    }
  }

  else(_) {
    fail
  }
}

fn validate_active_datums_and_get_info(
  input_datum: Datum,
  output_datum_data: Data,
  return: fn(VerificationKeyHash, VerificationKeyHash, PosixTime, PosixTime) ->
    Bool,
) -> Bool {
  // 1. An operator must already be appointed active.
  expect ActiveOperator {
    operator: previous_shifts_operator,
    start_time: previous_shifts_start_time,
  } = input_datum

  // 2. Output datum must specify an appointed operator.
  expect ActiveOperator {
    operator: new_shifts_operator,
    start_time: new_shifts_start_time,
  } = output_datum_data

  // 3. The start time of the new shift must be the end time of the previous
  //    shift.
  expect new_shifts_start_time == shift_end_time(previous_shifts_start_time)

  return(
    previous_shifts_operator,
    new_shifts_operator,
    previous_shifts_start_time,
    new_shifts_start_time,
  )
}

fn validate_end_of_shift_and_get_operators(
  input_datum: Datum,
  output_datum_data: Data,
  validity_range: ValidityRange,
  extra_signatories: List<VerificationKeyHash>,
) -> (VerificationKeyHash, VerificationKeyHash) {
  // 1. Input and output datums must both have an appointed operator, and the
  //    new shift's start time must follow the previous one correctly.
  let
    previous_shifts_operator,
    new_shifts_operator,
    previous_shifts_start_time,
    new_shifts_start_time,
  <- validate_active_datums_and_get_info(input_datum, output_datum_data)

  // 2. If the next shift's start time is in the past, it means the next shift's
  //    operator is most likely inactive, so anyone can first advance the
  //    scheduler, and immediately give an inactivity strike while again
  //    advancing to the next operator. Otherwise, the transaction's (short)
  //    validity range must contain the next shift's start time, and requires
  //    consent of the next operator to advance. This ensures readiness/liveness
  //    of the next operator to take over.
  expect or {
      validity_range |> interval.is_entirely_after(new_shifts_start_time),
      and {
        next_shifts_operator |> utils.has_signed(extra_signatories),
        {
          let (_inclusive_lower_bound, inclusive_upper_bound) =
            utils.get_inclusive_bounds_of_a_short_validity_range(validity_range)
          new_shifts_start_time <= inclusive_upper_bound
        },
      },
    }

  (previous_shifts_operator, new_shifts_operator)
}

fn validate_new_shifts_start_time_after_skipping(
  new_shifts_start_time: PosixTime,
  validity_range: ValidityRange,
) -> Bool {
  let (_inclusive_lower_bound, inclusive_upper_bound) =
    utils.get_inclusive_bounds_of_a_short_validity_range(validity_range)
  new_shifts_start_time == inclusive_upper_bound
}

fn validate_operator_inactivity_and_get_its_link(
  active_operators_address: Address,
  hub_oracle_script_hash: ScriptHash,
  //
  scheduler_input_index: Int,
  active_operators_input_index: Int,
  active_operators_redeemer_index: Int,
  state_queue_ref_input_index: Int,
  hub_oracle_ref_input_index: Int,
  //
  inactive_operator: VerificationKeyHash,
  inactive_operators_shift_start_time: PosixTime,
  //
  reference_inputs: List<Input>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  validity_range: ValidityRange,
) -> linked_list.Link {
  // 1. A matching spending redeemer of the active operators set must exist for
  //    incrementing the strike counter of the inactive operator.
  expect active_operators.StrikeForInactivity { operator, active_node_link, .. } =
    utils.get_spending_redeemer_data_at(
      active_operators_address,
      active_operators_input_index,
      active_operators_redeemer_index,
      inputs,
      redeemers,
    )
  expect inactive_operator == operator

  // 2. Authentic hub oracle UTxO must be referred to.
  let hub_datum =
    hub.get_datum(
      reference_inputs,
      hub_oracle_script_hash,
      hub_oracle_ref_input_index,
    )

  // 3. Specified state queue element UTxO must exist in reference inputs.
  expect Some(state_queue_ref_input) =
    reference_inputs |> list.at(state_queue_ref_input_index)

  // 4. The referred state queue UTxO must be authentic.
  expect
    {
      let
        _state_queue_lovelace,
        m_header_hash,
        confirmed_state_or_header_data,
        state_queue_link,
      <- linked_list.get_element_info(state_queue_ref_input.output)

      // 5. The element must be last in the state queue.
      expect None == state_queue_link

      // 6. Let `inactivity_threshold` be the sum of either the upper bound of
      //    the latest block commitment, or inactive operator's shift start time
      //    (whichever is biggest), and `operator_inactivity_timeout` protocol
      //    parameter. The reason for considering inactive operator's shift
      //    start time here is to allow a grace period for the case of a
      //    singular operator becoming inactive. Without this in place, the
      //    operator could be striked multiple times to the point of slashing
      //    for one instance of inactivity, as skipping rewinds the scheduler
      //    back to the same operator.
      let last_state_queue_elements_end_time =
        when m_header_hash is {
          None -> {
            expect ledger_state.ConfirmedState { end_time, .. } =
              confirmed_state_or_header_data
            end_time
          }
          Some(_) -> {
            expect ledger_state.Header { end_time, .. } =
              confirmed_state_or_header_data
            end_time
          }
        }
      let inactivity_threshold =
        max(
          last_state_queue_elements_end_time,
          inactive_operators_shift_start_time,
        ) + operator_inactivity_timeout

      // 7. The validity range of the transaction must be entirely after
      //    `inactivity_threshold`.
      expect
        validity_range
          |> interval.is_entirely_after(inactivity_threshold)

      // Done.
      True
    }
      |> state_queue.finalize_linked_list(hub_datum.state_queue)

  active_node_link
}

fn validate_root_is_reached(
  active_operators_policy_id: PolicyId,
  previous_shifts_operator: VerificationKeyHash,
  active_operators_root_ref_input_index: Int,
  reference_inputs: List<Input>,
) -> Bool {
  // 1. Element UTxO of active operators set must exist at the specified index.
  expect Some(active_root_ref_input) =
    reference_inputs |> list.at(active_operators_root_ref_input_index)

  {
    let
      _active_root_lovelace,
      m_active_root_key,
      _active_root_data,
      active_root_link,
    <- linked_list.get_element_info(active_root_ref_input.output)

    // 2. The referenced element must be root.
    expect None == m_active_root_key

    // 3. The root must point to a node with a key that matches the
    //    previous operator.
    Some(previous_shifts_operator) == active_root_link
  }
    |> active_operators.finalize_linked_list(active_operators_policy_id)
}

fn validate_operator_is_the_last_active_node(
  active_operators_policy_id: PolicyId,
  active_operators_last_node_ref_input_index: Int,
  new_shifts_operator: VerificationKeyHash,
  reference_inputs: List<Input>,
) -> Bool {
  // 1. The specified UTxO must exist in the reference inputs list.
  expect Some(active_node_ref_input) =
    reference_inputs
      |> list.at(active_operators_last_node_ref_input_index)

  {
    let
      _active_node_lovelace,
      m_active_node_key,
      _active_node_data,
      active_node_link,
    <- linked_list.get_element_info(active_node_ref_input.output)

    // 2. The referenced element must be a node with a key that matches the
    //    the next shift's operator.
    expect Some(new_shifts_operator) == m_active_node_key

    // 3. The node must be the last in the list.
    None == active_node_link
  }
    |> active_operators.finalize_linked_list(active_operators_policy_id)
}

fn validate_new_shifts_node_is_next(
  active_operators_policy_id: PolicyId,
  new_shifts_operator_node_ref_input_index: Int,
  previous_shifts_operator: VerificationKeyHash,
  new_shifts_operator: VerificationKeyHash,
  reference_inputs: List<Input>,
) -> Bool {
  // 1. Active operator element must exist at the specified index.
  expect Some(active_node_ref_input) =
    reference_inputs |> list.at(new_shifts_operator_node_ref_input_index)

  // The referenced node in the active operators set must have a key that
  // matches the new shift's operator, and a link that points to the
  // previous operator.
  {
    let
      _active_node_lovelace,
      m_active_node_key,
      _active_node_data,
      active_node_link,
    <- linked_list.get_element_info(active_node_ref_input.output)

    // 2. The referenced element must be a node with a key that matches
    //    the next shift's operator.
    expect Some(new_shifts_operator) == m_active_node_key

    // 3. The node must be anchor to the previous operator's node.
    expect Some(previous_shifts_operator) == active_node_link
  }
    |> active_operators.finalize_linked_list(active_operators_policy_id)
}

/// Validates active operators mint script is invoked, and from its redeemer
/// retrieves the removing operator's anchor key (or `None` if its anchor is a
/// root element), and a flag that indicates whether the removing node has a
/// link to another node (that is, if it's `False`, it means after removal only
/// the root element remains in the active set).
fn validate_active_operator_removal(
  active_operators_policy_id: PolicyId,
  active_operators_mint_redeemer_index: Int,
  removal_reason: OperatorRemovalReason,
  previous_shifts_operator: VerificationKeyHash,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  return: fn(Option<linked_list.NodeKey>, Bool) -> Bool,
) -> Bool {
  // 1. Minting script of active operators set must be invoked.
  let active_operators_mint_redeemer_data =
    redeemers
      |> utils.get_redeemer_at(
          expected_purpose: Mint(active_operators_policy_id),
          redeemer_index: active_operators_mint_redeemer_index,
        )

  when removal_reason is {
    OperatorRetirement -> {
      expect active_operators.RetireOperator {
        active_operator_key,
        operator_removal_scheduler_sync: operator_directory.ShowSchedulerIsAdvancing {
          scheduler_input_index: scheduler_input_index_from_active_ops_redeemer,
          removing_operators_anchor_element_key,
          removing_operator_is_the_last_member,
          ..
        },
        ..
      } = active_operators_mint_redeemer_data
      // 2a. Operator in the spent scheduler UTxO must match the retiring
      //     operator from active operators redeemer.
      expect previous_shifts_operator == active_operator_key

      return(
        removing_operators_anchor_element_key,
        removing_operator_is_the_last_member,
      )
    }
    OperatorSlashing -> {
      expect active_operators.SlashOperator{slashing_arguments: operator_directory.SlashingArguments {
        slashed_operator,
        operator_removal_scheduler_sync: operator_directory.ShowSchedulerIsAdvancing {
          scheduler_input_index: scheduler_input_index_from_active_ops_redeemer,
          removing_operators_anchor_element_key,
          removing_operator_is_the_last_member,
          ..
        },
        ..
      }} = active_operators_mint_redeemer_data

      // 2b. Operator in the spent scheduler UTxO must match the slashed
      //     operator from active operators redeemer.
      expect previous_shifts_operator == slashed_operator

      return(
        removing_operators_anchor_element_key,
        removing_operator_is_the_last_member,
      )
    }
  }
}
