use aiken/builtin
use aiken/collection/list
use aiken/crypto
use aiken_design_patterns/linked_list
use cardano/address.{Address}
use cardano/assets.{Lovelace, PolicyId, quantity_of}
use cardano/transaction.{
  Input, Mint, OutputReference, Redeemer, ScriptPurpose, Transaction,
}
use env
use midgard/common/utils
use midgard/da_attestation_types as da_attestation
use midgard/fraud_proof
use midgard/hub_oracle as hub
use midgard/ledger_state.{
  ConfirmedState, HeaderV1, block_maturity_duration_v1,
  confirmed_state_next_header_protocol_version_v1, genesis_confirmed_state_v1,
  header_v1_is_valid, protocol_version_v1,
}
use midgard/operator_directory.{SlashOperatorForBadState}
use midgard/operator_directory/active_operators
use midgard/operator_directory/retired_operators
use midgard/scheduler
use midgard/settlement
use midgard/state_queue.{
  AttachDaAttestation, CommitBlockHeader, Deinit, HeaderView, InitV1,
  LinkedListMutation, MergeToConfirmedStateV1, MintRedeemer,
  OperatorAlreadySlashed, RemoveFraudulentBlockHeader,
  RemoveFraudulentBlocksLink, RemoveLastFraudulentBlock, SlashActiveOperator,
  SlashRetiredOperator, SpendRedeemer, StateQueueNode,
  commit_bound_header_time_is_valid, confirmed_state_asset_name,
  decode_header_view, finalize_linked_list, no_da_attestation,
}

/// Pure CommitBlockHeader structural seam. The full validator authenticates
/// the linked-list output before reaching this predicate.
pub fn commit_block_header_output_is_valid_v1(header: HeaderV1) -> Bool {
  and {
    header.start_time < header.end_time,
    header_v1_is_valid(header),
  }
}

/// The scheduler reference and active-operator spend are authenticated by
/// their respective scripts before their keys reach this pure equality check.
pub fn commit_block_header_operator_is_legitimate_v1(
  header_operator: ByteArray,
  redeemer_operator: ByteArray,
  scheduler_operator: ByteArray,
  active_operator: ByteArray,
) -> Bool {
  and {
    header_operator == redeemer_operator,
    scheduler_operator == redeemer_operator,
    active_operator == redeemer_operator,
  }
}

/// Exact carry-over required when appending after another queued block.
pub fn commit_block_header_carries_previous_block_v1(
  output_header: HeaderV1,
  previous_header_hash: ByteArray,
  previous_header: HeaderV1,
) -> Bool {
  and {
    output_header.prev_header_hash == previous_header_hash,
    output_header.prev_utxos_root == previous_header.utxos_root,
    output_header.start_time == previous_header.end_time,
    output_header.protocol_version == previous_header.protocol_version,
  }
}

/// Exact carry-over required when the linked-list anchor is the confirmed
/// state. `confirmed_state_next_header_protocol_version_v1` distinguishes the
/// sole genesis sentinel from authenticated ordinary V1 states.
pub fn commit_block_header_carries_confirmed_state_v1(
  output_header: HeaderV1,
  confirmed_state: ConfirmedState,
) -> Bool {
  when confirmed_state_next_header_protocol_version_v1(confirmed_state) is {
    Some(next_protocol_version) -> and {
        output_header.prev_header_hash == confirmed_state.header_hash,
        output_header.prev_utxos_root == confirmed_state.utxo_root,
        output_header.start_time == confirmed_state.end_time,
        output_header.protocol_version == next_protocol_version,
      }
    None -> False
  }
}

pub type MergeCommitments {
  withdrawals_root: ByteArray,
  forced_transactions_root: ByteArray,
  transactions_root: ByteArray,
  deposits_root: ByteArray,
  transition_trace_root: ByteArray,
  event_to_step_root: ByteArray,
  validation_traces_root: ByteArray,
  withdrawal_count: Int,
  forced_transaction_count: Int,
  l2_transaction_count: Int,
  deposit_count: Int,
  total_event_count: Int,
  transition_step_count: Int,
  validation_trace_count: Int,
}

pub fn merge_commitments_match_header(
  commitments: MergeCommitments,
  header: HeaderView,
) -> Bool {
  and {
    commitments.withdrawals_root == header.withdrawals_root,
    commitments.forced_transactions_root == header.forced_transactions_root,
    commitments.transactions_root == header.transactions_root,
    commitments.deposits_root == header.deposits_root,
    commitments.transition_trace_root == header.transition_trace_root,
    commitments.event_to_step_root == header.event_to_step_root,
    commitments.validation_traces_root == header.validation_traces_root,
    commitments.withdrawal_count == header.withdrawal_count,
    commitments.forced_transaction_count == header.forced_transaction_count,
    commitments.l2_transaction_count == header.l2_transaction_count,
    commitments.deposit_count == header.deposit_count,
    commitments.total_event_count == header.total_event_count,
    commitments.transition_step_count == header.transition_step_count,
    commitments.validation_trace_count == header.validation_trace_count,
  }
}

pub fn merge_settlement_binding_matches_header(
  header: HeaderView,
  header_node_key: ByteArray,
  m_settlement_id: Option<ByteArray>,
) -> Bool {
  let carries_l2_material = header_carries_l2_material(header)
  if carries_l2_material {
    m_settlement_id == Some(header_node_key)
  } else {
    m_settlement_id == None
  }
}

pub fn merge_settlement_id_at_route(
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  settlement_script_hash: PolicyId,
  m_settlement_redeemer_index: Option<Int>,
) -> Option<ByteArray> {
  when m_settlement_redeemer_index is {
    None -> None
    Some(settlement_redeemer_index) -> {
      let settlement_minting_redeemer_data = redeemers
        |> utils.get_redeemer_at(
            Mint(settlement_script_hash),
            settlement_redeemer_index,
          )
      expect settlement.Spawn { settlement_id, .. }: settlement.MintRedeemer = settlement_minting_redeemer_data
      Some(settlement_id)
    }
  }
}

fn header_carries_l2_material(header: HeaderView) -> Bool {
  or {
    header.transactions_root != env.empty_merkle_tree_root,
    header.deposits_root != env.empty_merkle_tree_root,
    header.withdrawals_root != env.empty_merkle_tree_root,
    header.forced_transactions_root != env.empty_merkle_tree_root,
  }
}

fn merge_to_confirmed_state(
  own_policy_id: PolicyId,
  da_attestation_policy_id: PolicyId,
  settlement_script_hash: PolicyId,
  self: Transaction,
  expected_protocol_version: Int,
  header_node_key: ByteArray,
  confirmed_state_input_outref: OutputReference,
  confirmed_state_output_index: Int,
  m_settlement_redeemer_index: Option<Int>,
  commitments: MergeCommitments,
) -> Bool {
  let Transaction { inputs, outputs, mint, redeemers, validity_range, .. } = self
  expect Some(continued_confirmed_state_output) = outputs
    |> list.at(confirmed_state_output_index)

  {
    let
        _confirmed_state_input,
        _confirmed_state_lovelace_change,
        input_confirmed_state_data,
        _input_header_node,
        _input_header_lovelace,
        input_header_node_key,
        input_header_node_data,
        _input_header_link,
        output_confirmed_state_data
      <- linked_list.fold_from_root(
      anchor_root_input_outref: confirmed_state_input_outref,
      continued_anchor_root_output: continued_confirmed_state_output,
      inputs: inputs,
      tx_mint: mint,
    )

    expect input_header_node_key == header_node_key
    expect
        StateQueueNode { header: input_header_data, da_attestation }: StateQueueNode
      = input_header_node_data
    expect da_attestation == da_attestation_policy_id

    let input_header = decode_header_view(input_header_data)
    expect input_header.protocol_version == expected_protocol_version
    expect header_v1_is_valid(input_header)
    expect merge_commitments_match_header(commitments, input_header)

    let inclusive_tx_lower_bound = utils.get_inclusive_lower_bound_of_interval(
      validity_range,
    )
    expect
      inclusive_tx_lower_bound >= input_header.end_time + block_maturity_duration_v1

    expect
        ConfirmedState {
          start_time: input_confirmed_state_start_time,
          header_hash: input_confirmed_state_header_hash,
          prev_header_hash: input_confirmed_state_prev_header_hash,
          utxo_root: input_confirmed_state_utxo_root,
          end_time: input_confirmed_state_end_time,
          protocol_version: input_confirmed_state_protocol_version,
        }
      = input_confirmed_state_data
    expect
      Some(expected_protocol_version) == confirmed_state_next_header_protocol_version_v1(
        ConfirmedState {
          header_hash: input_confirmed_state_header_hash,
          prev_header_hash: input_confirmed_state_prev_header_hash,
          utxo_root: input_confirmed_state_utxo_root,
          start_time: input_confirmed_state_start_time,
          end_time: input_confirmed_state_end_time,
          protocol_version: input_confirmed_state_protocol_version,
        },
      )
    let expected_output_confirmed_state_data: Data = ConfirmedState {
      header_hash: header_node_key,
      prev_header_hash: input_confirmed_state_header_hash,
      utxo_root: input_header.utxos_root,
      start_time: input_confirmed_state_start_time,
      end_time: input_header.end_time,
      protocol_version: input_header.protocol_version,
    }
    expect output_confirmed_state_data == expected_output_confirmed_state_data

    if header_carries_l2_material(input_header) {
      merge_settlement_binding_matches_header(
        input_header,
        header_node_key,
        merge_settlement_id_at_route(
          redeemers,
          settlement_script_hash,
          m_settlement_redeemer_index,
        ),
      )
    } else {
      and {
        m_settlement_redeemer_index == None,
        merge_settlement_binding_matches_header(
          input_header,
          header_node_key,
          None,
        ),
      }
    }
  }
    |> finalize_linked_list(own_policy_id)
}

validator spend(
  state_queue_mint_script_hash: ByteArray,
  da_attestation_policy_id: PolicyId,
) {
  spend(
    _datum: Option<Data>,
    redeemer: SpendRedeemer,
    own_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      LinkedListMutation ->
        linked_list.spend_for_adding_or_removing_an_element(
          state_queue_mint_script_hash,
          tx.mint,
        )
      AttachDaAttestation {
        state_queue_input_index,
        da_attestation_mint_redeemer_index,
      } -> {
        expect Some(Input { output_reference, .. }) = tx.inputs
          |> list.at(state_queue_input_index)
        expect output_reference == own_ref

        let mint_redeemer_data = tx.redeemers
          |> utils.get_redeemer_at(
              Mint(da_attestation_policy_id),
              da_attestation_mint_redeemer_index,
            )
        expect
            da_attestation.ApplyToStateQueue {
              state_queue_input_index: redeemer_state_queue_input_index,
              ..
            }: da_attestation.MintRedeemer
          = mint_redeemer_data
        redeemer_state_queue_input_index == state_queue_input_index
      }
    }
  }

  else(_) {
    fail
  }
}

validator mint(
  hub_oracle_script_hash: PolicyId,
  active_operators_script_hash: PolicyId,
  active_operators_addr: Address,
  retired_operators_script_hash: PolicyId,
  scheduler_script_hash: PolicyId,
  fraud_proof_script_hash: PolicyId,
  settlement_script_hash: PolicyId,
  da_attestation_policy_id: PolicyId,
) {
  mint(redeemer: MintRedeemer, own_policy_id: PolicyId, self: Transaction) {
    when redeemer is {
      InitV1 { output_index } -> {
        let Transaction { outputs, mint, validity_range, .. } = self

        expect Some(root_output) = outputs |> list.at(output_index)

        let (_current_time_lower, current_time_upper) = utils.get_inclusive_bounds_of_a_short_validity_range(
          validity_range,
        )

        expect Some(expected_genesis_state) = genesis_confirmed_state_v1(
          current_time_upper,
        )
        let expected_root_data: Data = expected_genesis_state

        expect
          linked_list.init(
            nonce_validated: quantity_of(
              mint,
              hub_oracle_script_hash,
              hub.asset_name,
            ) == 1,
            produced_element_output: root_output,
            tx_mint: mint,
            root_validator: fn(_, _root_lovelace: Lovelace, root_data: Data) -> Bool {
              root_data == expected_root_data
            },
          )
            |> linked_list.run_root_with(
                own_policy_id,
                confirmed_state_asset_name,
              )

        True
      }
      Deinit -> {
        let Transaction { inputs, mint, .. } = self

        // 1. Hub oracle NFT must be getting burnt.
        expect quantity_of(mint, hub_oracle_script_hash, hub.asset_name) == -1

        // 2. Linked list de-initialization must be valid. No other validations
        //    required.
        expect
          linked_list.deinit(
            inputs: inputs,
            tx_mint: mint,
            root_validator: fn(_, _, _) -> Bool { True },
          )
            |> linked_list.run_root_with(
                own_policy_id,
                confirmed_state_asset_name,
              )

        // Done.
        True
      }
      CommitBlockHeader {
        new_block_output_index,
        continued_latest_block_output_index,
        operator,
        scheduler_ref_input_index,
        active_operators_input_index,
        active_operators_redeemer_index,
      } -> {
        let
            Transaction {
              inputs,
              outputs,
              reference_inputs,
              mint,
              extra_signatories,
              redeemers,
              validity_range,
              ..
            }
          = self
        // Commit the block header to the state queue, with the operator's
        // content:
        // 1. Let `operator` be a redeemer argument indicating the pub key hash
        //    of the operator committing the block header.
        // 2. The transaction must be signed by `operator`.
        expect operator |> utils.has_signed(extra_signatories)

        expect Some(continued_anchor_element_output) = outputs
          |> list.at(continued_latest_block_output_index)
        expect Some(new_block_output) = outputs
          |> list.at(new_block_output_index)

        // 3. Indicated outputs must properly adhere to unordered linked list
        //    append requirements.
        {
          let
              _anchor_input,
              _anchor_lovelace_change,
              m_anchor_key,
              anchor_data,
              _output_header_lovelace,
              output_header_key,
              output_node_data
            <- linked_list.append_unordered(
            continued_anchor_element_output: continued_anchor_element_output,
            new_node_output: new_block_output,
            inputs: inputs,
            tx_mint: mint,
          )

          expect
              StateQueueNode { header: output_header_data, da_attestation }: StateQueueNode
            = output_node_data
          expect da_attestation == no_da_attestation

          // 4. The new block in state queue must have the hash of the header
          //    as its linked list key.
          let output_header_hash = crypto.blake2b_224(
            builtin.serialise_data(output_header_data),
          )
          expect output_header_key == output_header_hash

          // 5. New element's data must be `HeaderV1` with a valid time range.
          let output_header = decode_header_view(output_header_data)
          let output_operator_vkey = output_header.operator_vkey
          expect commit_block_header_output_is_valid_v1(output_header)
          expect
            commit_bound_header_time_is_valid(
              output_header.start_time,
              output_header.end_time,
              validity_range,
            )

          // Verify that it is the operator's turn to commit according to the
          // scheduler:
          // 7. The transaction must include a reference input with the
          //    scheduler NFT. Let that input's datum be the `scheduler_state`.
          expect scheduler.ActiveOperator { operator: scheduler_operator, .. } = scheduler.get_datum(
            reference_inputs: reference_inputs,
            scheduler_policy_id: scheduler_script_hash,
            scheduler_ref_input_index: scheduler_ref_input_index,
          )

          // Update the operator's timestamp in the active operators set:
          // 9. The transaction must include an input, spent via the
          //    `UpdateBondHoldNewState` redeemer, of an `active_operators`
          //    node with a key matching the `operator`.
          expect active_operators.UpdateBondHoldNewState { active_operator, .. }
            = utils.get_spending_redeemer_data_at(
            active_operators_addr,
            active_operators_input_index,
            active_operators_redeemer_index,
            inputs,
            redeemers,
          )
          // 6, 8, 9. The header, authenticated scheduler state, and
          // authenticated active-operator spend must all name the redeemer
          // operator.
          expect
            commit_block_header_operator_is_legitimate_v1(
              output_operator_vkey,
              operator,
              scheduler_operator,
              active_operator,
            )

          // To validate the output header, we must take two routes depending on
          // whether the anchor element is the root (i.e. confirmed state), or a
          // node (i.e. latest block in the queue).
          when m_anchor_key is {
            Some(header_hash) -> {
              // 10a. New block must properly carry over data from previous
              //      block.
              expect
                  StateQueueNode { header: anchor_header_data, .. }: StateQueueNode
                = anchor_data
              let anchor_header = decode_header_view(anchor_header_data)
              expect
                commit_block_header_carries_previous_block_v1(
                  output_header,
                  header_hash,
                  anchor_header,
                )

              // Done.
              True
            }
            None -> {
              // 10b. New block must properly carry over data from confirmed
              //      state.
              expect
                  ConfirmedState {
                    header_hash,
                    prev_header_hash,
                    utxo_root,
                    start_time,
                    end_time,
                    protocol_version,
                  }
                = anchor_data
              expect
                commit_block_header_carries_confirmed_state_v1(
                  output_header,
                  ConfirmedState {
                    header_hash,
                    prev_header_hash,
                    utxo_root,
                    start_time,
                    end_time,
                    protocol_version,
                  },
                )

              // Done.
              True
            }
          }
        }
          |> finalize_linked_list(own_policy_id)
      }
      RemoveFraudulentBlockHeader {
        fraudulent_operator,
        fraudulent_blocks_header_hash,
        slashing_approach,
        fraud_proof_ref_input_index,
        block_removal_approach,
      } -> {
        let
            Transaction {
              inputs,
              outputs,
              reference_inputs,
              mint,
              redeemers,
              ..
            }
          = self

        // Slash the fraudulent operator:
        // 1. Let `slashing_approach` be a sum type indicating whether the
        //    fraudulent operator is active, retired, or already slashed.
        expect
          when slashing_approach is {
            SlashActiveOperator { active_operators_redeemer_index } -> {
              // 2a. If the operator is active:
              //     The transaction must Remove a node from the
              //     `active_operators` set via the `SlashOperatorBadState`
              //     redeemer. The `slashed_operator` argument provided to that
              //     redeemer must match `fraudulent_operator`.
              let slashing_reason_from_active_operators = operator_directory.cross_validate_slashing_reason(
                fraudulent_operator,
                active_operators_script_hash,
                active_operators_redeemer_index,
                fn(redeemer_data: Data) {
                  expect
                      active_operators.SlashOperator { slashing_arguments, .. }
                    = redeemer_data
                  slashing_arguments
                },
                redeemers,
              )
              expect SlashOperatorForBadState { .. } = slashing_reason_from_active_operators
              True
            }
            SlashRetiredOperator { retired_operators_redeemer_index } -> {
              // 2b. If the operator is retired:
              //     The transaction must Remove a node from the
              //     `retired_operators` set via the `SlashOperatorBadState`
              //     redeemer. The `slashed_operator` argument provided to that
              //     redeemer must match `fraudulent_operator`.
              let slashing_reason_from_retired_operators = operator_directory.cross_validate_slashing_reason(
                fraudulent_operator,
                retired_operators_script_hash,
                retired_operators_redeemer_index,
                fn(redeemer_data: Data) {
                  expect retired_operators.SlashOperator { slashing_arguments }
                    = redeemer_data
                  slashing_arguments
                },
                redeemers,
              )
              expect SlashOperatorForBadState { .. } = slashing_reason_from_retired_operators
              True
            }
            OperatorAlreadySlashed {
              active_operators_element_ref_input_index,
              retired_operators_element_ref_input_index,
            } ->
              // 2c. If the operator has already been slashed:
              //     The transaction must refer to elements from both the active
              //     and retired operators directories (which are ordered linked
              //     lists), and show that neither lists include the fraudulent
              //     operator.
              and {
                fraudulent_operator
                  |> operator_directory.operator_is_not_a_member(
                      reference_inputs: reference_inputs,
                      element_ref_input_index: active_operators_element_ref_input_index,
                    )
                  |> active_operators.finalize_linked_list(
                      active_operators_script_hash,
                    ),
                fraudulent_operator
                  |> operator_directory.operator_is_not_a_member(
                      reference_inputs: reference_inputs,
                      element_ref_input_index: retired_operators_element_ref_input_index,
                    )
                  |> retired_operators.finalize_linked_list(
                      nft_policy_id: retired_operators_script_hash,
                    ),
              }
          }

        // Remove fraudulent block or its link from the state queue:
        // 3. Let `block_removal_approach` be a redeemer argument that specifies
        //    whether the fraudulent block itself can be removed:
        expect
          when block_removal_approach is {
            RemoveFraudulentBlocksLink {
              fraudulent_node_input_outref,
              fraudulent_node_output_index,
            } -> {
              // 4a. If the fraudulent block has a link, its successor node must
              //     be removed first.
              //
              //     > [!NOTE]
              //     > The successor node is also considered fraudulent because
              //     > of getting appended to a fraudulent block, so it does not
              //     > need to have an associated fraud proof token.
              expect Some(fraudulent_node_output) = outputs
                |> list.at(fraudulent_node_output_index)
              let
                  _fraudulent_header_input,
                  _fraudulent_header_lovelace_change,
                  m_fraudulent_header_hash,
                  _fraudulent_header_data,
                  _removed_block_input,
                  _removed_node_lovelace,
                  _removed_node_header_hash,
                  removed_node_header_data,
                  _removed_node_link
                <- linked_list.remove(
                anchor_input_outref: fraudulent_node_input_outref,
                continued_anchor_element_output: fraudulent_node_output,
                inputs: inputs,
                tx_mint: mint,
              )
              expect
                Some(fraudulent_blocks_header_hash) == m_fraudulent_header_hash
              expect
                  StateQueueNode { header: removed_node_header, .. }: StateQueueNode
                = removed_node_header_data
              let removed_header_view = decode_header_view(removed_node_header)
              removed_header_view.operator_vkey == fraudulent_operator
            }
            RemoveLastFraudulentBlock {
              anchor_element_input_outref,
              anchor_element_output_index,
            } -> {
              // 4b. If the fraudulent block has no links, it can be removed
              //     from the very end of the state queue.
              expect Some(anchor_element_output) = outputs
                |> list.at(anchor_element_output_index)
              let
                  _anchor_input,
                  _anchor_lovelace_change,
                  _m_anchor_key,
                  _anchor_data,
                  _removed_node_input,
                  _removed_node_lovelace,
                  removed_node_header_hash,
                  removed_node_header_data,
                  removed_node_link
                <- linked_list.remove(
                anchor_input_outref: anchor_element_input_outref,
                continued_anchor_element_output: anchor_element_output,
                inputs: inputs,
                tx_mint: mint,
              )
              expect fraudulent_blocks_header_hash == removed_node_header_hash
              expect
                  StateQueueNode { header: removed_node_header, .. }: StateQueueNode
                = removed_node_header_data
              let removed_header_view = decode_header_view(removed_node_header)
              expect removed_header_view.operator_vkey == fraudulent_operator
              removed_node_link == None
            }
          }
            |> finalize_linked_list(own_policy_id)

        // Verify that fraud has been proved for the specified block header:
        // 5. The transaction must include a reference input holding a
        //    `fraud_proof` token.
        let referenced_fraudulent_header_hash = reference_inputs
          |> fraud_proof.get_proven_fraudulent_blocks_header_hash(
              fraud_proof_script_hash,
              fraud_proof_ref_input_index,
            )
        expect
          referenced_fraudulent_header_hash == fraudulent_blocks_header_hash

        // Done.
        True
      }
      MergeToConfirmedStateV1 {
        header_node_key,
        confirmed_state_input_outref,
        confirmed_state_output_index,
        m_settlement_redeemer_index,
        merged_block_withdrawals_root,
        merged_block_forced_transactions_root,
        merged_block_transactions_root,
        merged_block_deposits_root,
        merged_block_transition_trace_root,
        merged_block_event_to_step_root,
        merged_block_validation_traces_root,
        merged_block_withdrawal_count,
        merged_block_forced_transaction_count,
        merged_block_l2_transaction_count,
        merged_block_deposit_count,
        merged_block_total_event_count,
        merged_block_transition_step_count,
        merged_block_validation_trace_count,
      } ->
        merge_to_confirmed_state(
          own_policy_id,
          da_attestation_policy_id,
          settlement_script_hash,
          self,
          protocol_version_v1,
          header_node_key,
          confirmed_state_input_outref,
          confirmed_state_output_index,
          m_settlement_redeemer_index,
          MergeCommitments {
            withdrawals_root: merged_block_withdrawals_root,
            forced_transactions_root: merged_block_forced_transactions_root,
            transactions_root: merged_block_transactions_root,
            deposits_root: merged_block_deposits_root,
            transition_trace_root: merged_block_transition_trace_root,
            event_to_step_root: merged_block_event_to_step_root,
            validation_traces_root: merged_block_validation_traces_root,
            withdrawal_count: merged_block_withdrawal_count,
            forced_transaction_count: merged_block_forced_transaction_count,
            l2_transaction_count: merged_block_l2_transaction_count,
            deposit_count: merged_block_deposit_count,
            total_event_count: merged_block_total_event_count,
            transition_step_count: merged_block_transition_step_count,
            validation_trace_count: merged_block_validation_trace_count,
          },
        )
    }
  }

  // TODO: Add redeemers for escape hatch blocks

  else(_) {
    fail
  }
}

const q49_header_hash_a =
  #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const q49_header_hash_b =
  #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const q49_genesis_header_hash =
  #"00000000000000000000000000000000000000000000000000000000"

const q49_operator = #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccc"

const q49_other_operator =
  #"dddddddddddddddddddddddddddddddddddddddddddddddddddddddd"

const q49_utxo_root_a =
  #"1111111111111111111111111111111111111111111111111111111111111111"

const q49_utxo_root_b =
  #"2222222222222222222222222222222222222222222222222222222222222222"

fn q49_header_fixture() -> HeaderV1 {
  HeaderV1 {
    prev_utxos_root: q49_utxo_root_a,
    utxos_root: q49_utxo_root_b,
    withdrawals_root: env.empty_merkle_tree_root,
    forced_transactions_root: env.empty_merkle_tree_root,
    transactions_root: env.empty_merkle_tree_root,
    deposits_root: env.empty_merkle_tree_root,
    transition_trace_root: env.empty_merkle_tree_root,
    event_to_step_root: env.empty_merkle_tree_root,
    validation_traces_root: env.empty_merkle_tree_root,
    withdrawal_count: 0,
    forced_transaction_count: 0,
    l2_transaction_count: 0,
    deposit_count: 0,
    total_event_count: 0,
    transition_step_count: 0,
    validation_trace_count: 0,
    start_time: 100,
    end_time: 200,
    block_slot: 0,
    expected_network_id: 0,
    min_fee_a: 0,
    min_fee_b: 0,
    prev_header_hash: q49_header_hash_a,
    operator_vkey: q49_operator,
    protocol_version: protocol_version_v1,
  }
}

test q49_l295_header_shape_control_and_interval_mutations() {
  let control = q49_header_fixture()
  and {
    commit_block_header_output_is_valid_v1(control),
    !commit_block_header_output_is_valid_v1(
      HeaderV1 { ..control, start_time: control.end_time },
    ),
    !commit_block_header_output_is_valid_v1(
      HeaderV1 { ..control, start_time: control.end_time + 1 },
    ),
    !commit_block_header_output_is_valid_v1(
      HeaderV1 { ..control, protocol_version: 0 },
    ),
    !commit_block_header_output_is_valid_v1(
      HeaderV1 { ..control, protocol_version: 2 },
    ),
  }
}

test q49_l295_scheduled_operator_control_and_independent_mutations() {
  and {
    commit_block_header_operator_is_legitimate_v1(
      q49_operator,
      q49_operator,
      q49_operator,
      q49_operator,
    ),
    !commit_block_header_operator_is_legitimate_v1(
      q49_other_operator,
      q49_operator,
      q49_operator,
      q49_operator,
    ),
    !commit_block_header_operator_is_legitimate_v1(
      q49_operator,
      q49_operator,
      q49_other_operator,
      q49_operator,
    ),
    !commit_block_header_operator_is_legitimate_v1(
      q49_operator,
      q49_operator,
      q49_operator,
      q49_other_operator,
    ),
    !commit_block_header_operator_is_legitimate_v1(
      q49_operator,
      q49_other_operator,
      q49_operator,
      q49_operator,
    ),
  }
}

test q49_l295_previous_header_control_and_independent_mutations() {
  let previous = q49_header_fixture()
  let output = HeaderV1 {
    ..q49_header_fixture(),
    prev_header_hash: q49_header_hash_b,
    prev_utxos_root: previous.utxos_root,
    start_time: previous.end_time,
    end_time: previous.end_time + 100,
  }
  and {
    commit_block_header_carries_previous_block_v1(
      output,
      q49_header_hash_b,
      previous,
    ),
    !commit_block_header_carries_previous_block_v1(
      HeaderV1 { ..output, prev_header_hash: q49_header_hash_a },
      q49_header_hash_b,
      previous,
    ),
    !commit_block_header_carries_previous_block_v1(
      HeaderV1 { ..output, prev_utxos_root: q49_utxo_root_a },
      q49_header_hash_b,
      previous,
    ),
    !commit_block_header_carries_previous_block_v1(
      HeaderV1 { ..output, start_time: previous.end_time - 1 },
      q49_header_hash_b,
      previous,
    ),
    !commit_block_header_carries_previous_block_v1(
      HeaderV1 { ..output, start_time: previous.end_time + 1 },
      q49_header_hash_b,
      previous,
    ),
    !commit_block_header_carries_previous_block_v1(
      HeaderV1 { ..output, protocol_version: 2 },
      q49_header_hash_b,
      previous,
    ),
  }
}

test q49_l295_confirmed_genesis_control_and_independent_mutations() {
  expect Some(genesis) = genesis_confirmed_state_v1(100)
  let output = HeaderV1 {
    ..q49_header_fixture(),
    prev_header_hash: q49_genesis_header_hash,
    prev_utxos_root: env.empty_merkle_tree_root,
  }
  let malformed_header_hash = ConfirmedState {
    ..genesis,
    header_hash: q49_header_hash_a,
  }
  let malformed_prev_hash = ConfirmedState {
    ..genesis,
    prev_header_hash: q49_header_hash_a,
  }
  let malformed_utxo_root = ConfirmedState {
    ..genesis,
    utxo_root: q49_utxo_root_a,
  }
  let malformed_interval = ConfirmedState {
    ..genesis,
    end_time: genesis.start_time + 1,
  }
  and {
    commit_block_header_carries_confirmed_state_v1(output, genesis),
    !commit_block_header_carries_confirmed_state_v1(
      HeaderV1 { ..output, prev_header_hash: q49_header_hash_b },
      genesis,
    ),
    !commit_block_header_carries_confirmed_state_v1(
      HeaderV1 { ..output, prev_utxos_root: q49_utxo_root_b },
      genesis,
    ),
    !commit_block_header_carries_confirmed_state_v1(
      HeaderV1 { ..output, start_time: genesis.end_time - 1 },
      genesis,
    ),
    !commit_block_header_carries_confirmed_state_v1(
      HeaderV1 { ..output, start_time: genesis.end_time + 1 },
      genesis,
    ),
    !commit_block_header_carries_confirmed_state_v1(
      HeaderV1 { ..output, protocol_version: 2 },
      genesis,
    ),
    !commit_block_header_carries_confirmed_state_v1(
      output,
      malformed_header_hash,
    ),
    !commit_block_header_carries_confirmed_state_v1(output, malformed_prev_hash),
    !commit_block_header_carries_confirmed_state_v1(output, malformed_utxo_root),
    !commit_block_header_carries_confirmed_state_v1(output, malformed_interval),
    !commit_block_header_carries_confirmed_state_v1(
      output,
      ConfirmedState { ..genesis, protocol_version: protocol_version_v1 },
    ),
  }
}

test q49_l295_confirmed_ordinary_control_and_independent_mutations() {
  let ordinary = ConfirmedState {
    header_hash: q49_header_hash_b,
    prev_header_hash: q49_header_hash_a,
    utxo_root: q49_utxo_root_a,
    start_time: 0,
    end_time: 100,
    protocol_version: protocol_version_v1,
  }
  let output = HeaderV1 {
    ..q49_header_fixture(),
    prev_header_hash: q49_header_hash_b,
  }
  and {
    commit_block_header_carries_confirmed_state_v1(output, ordinary),
    !commit_block_header_carries_confirmed_state_v1(
      output,
      ConfirmedState { ..ordinary, header_hash: q49_header_hash_a },
    ),
    !commit_block_header_carries_confirmed_state_v1(
      output,
      ConfirmedState { ..ordinary, header_hash: q49_genesis_header_hash },
    ),
    !commit_block_header_carries_confirmed_state_v1(
      output,
      ConfirmedState { ..ordinary, start_time: -1 },
    ),
    !commit_block_header_carries_confirmed_state_v1(
      output,
      ConfirmedState { ..ordinary, start_time: ordinary.end_time + 1 },
    ),
    !commit_block_header_carries_confirmed_state_v1(
      output,
      ConfirmedState { ..ordinary, protocol_version: 0 },
    ),
    !commit_block_header_carries_confirmed_state_v1(
      output,
      ConfirmedState { ..ordinary, protocol_version: 2 },
    ),
  }
}
