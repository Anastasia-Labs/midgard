use aiken/builtin
use aiken/collection/list
use aiken/crypto
use aiken_design_patterns/linked_list
use cardano/address.{Address}
use cardano/assets.{Lovelace, PolicyId, quantity_of}
use cardano/transaction.{Input, Mint, OutputReference, Transaction}
use env
use midgard/common/utils
use midgard/da_attestation_types as da_attestation
use midgard/fraud_proof
use midgard/hub_oracle as hub
use midgard/ledger_state.{
  ConfirmedState, block_maturity_duration_v1,
  confirmed_state_next_header_protocol_version_v1, genesis_confirmed_state_v1,
  max_deposit_count_v1, max_forced_transaction_count_v1,
  max_l2_transaction_count_v1, max_total_event_count_v1,
  max_transition_step_count_v1, max_validation_trace_count_v1,
  max_withdrawal_count_v1, protocol_version_v1,
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

fn root_matches_count(root: ByteArray, count: Int) -> Bool {
  if count == 0 {
    root == env.empty_merkle_tree_root
  } else {
    root != env.empty_merkle_tree_root
  }
}

fn header_view_is_valid(header: HeaderView) -> Bool {
  let counts_are_valid = and {
    header.protocol_version == protocol_version_v1,
    header.withdrawal_count <= max_withdrawal_count_v1,
    header.forced_transaction_count <= max_forced_transaction_count_v1,
    header.l2_transaction_count <= max_l2_transaction_count_v1,
    header.deposit_count <= max_deposit_count_v1,
    header.total_event_count <= max_total_event_count_v1,
    header.transition_step_count <= max_transition_step_count_v1,
    header.validation_trace_count <= max_validation_trace_count_v1,
    header.validation_trace_count == header.forced_transaction_count + header.l2_transaction_count,
    root_matches_count(
      header.validation_traces_root,
      header.validation_trace_count,
    ),
  }
  and {
    header.withdrawal_count >= 0,
    header.forced_transaction_count >= 0,
    header.l2_transaction_count >= 0,
    header.deposit_count >= 0,
    header.total_event_count >= 0,
    header.transition_step_count >= 0,
    header.validation_trace_count >= 0,
    header.total_event_count == header.withdrawal_count + header.forced_transaction_count + header.l2_transaction_count + header.deposit_count,
    header.transition_step_count == header.total_event_count,
    root_matches_count(header.withdrawals_root, header.withdrawal_count),
    root_matches_count(
      header.forced_transactions_root,
      header.forced_transaction_count,
    ),
    root_matches_count(header.transactions_root, header.l2_transaction_count),
    root_matches_count(header.deposits_root, header.deposit_count),
    root_matches_count(header.transition_trace_root, header.total_event_count),
    root_matches_count(header.event_to_step_root, header.total_event_count),
    counts_are_valid,
  }
}

type MergeCommitments {
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
    expect StateQueueNode { header: input_header_data, da_attestation } = input_header_node_data
    expect da_attestation == da_attestation_policy_id

    let input_header = decode_header_view(input_header_data)
    expect input_header.protocol_version == expected_protocol_version
    expect header_view_is_valid(input_header)
    expect commitments.withdrawals_root == input_header.withdrawals_root
    expect
      commitments.forced_transactions_root == input_header.forced_transactions_root
    expect commitments.transactions_root == input_header.transactions_root
    expect commitments.deposits_root == input_header.deposits_root
    expect
      commitments.transition_trace_root == input_header.transition_trace_root
    expect commitments.event_to_step_root == input_header.event_to_step_root
    expect
      commitments.validation_traces_root == input_header.validation_traces_root
    expect commitments.withdrawal_count == input_header.withdrawal_count
    expect
      commitments.forced_transaction_count == input_header.forced_transaction_count
    expect commitments.l2_transaction_count == input_header.l2_transaction_count
    expect commitments.deposit_count == input_header.deposit_count
    expect commitments.total_event_count == input_header.total_event_count
    expect
      commitments.transition_step_count == input_header.transition_step_count
    expect
      commitments.validation_trace_count == input_header.validation_trace_count

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

    if or {
      input_header.transactions_root != env.empty_merkle_tree_root,
      input_header.deposits_root != env.empty_merkle_tree_root,
      input_header.withdrawals_root != env.empty_merkle_tree_root,
      input_header.forced_transactions_root != env.empty_merkle_tree_root,
    } {
      expect Some(settlement_redeemer_index) = m_settlement_redeemer_index
      let settlement_minting_redeemer_data = redeemers
        |> utils.get_redeemer_at(
            Mint(settlement_script_hash),
            settlement_redeemer_index,
          )
      expect settlement.Spawn { settlement_id, .. }: settlement.MintRedeemer = settlement_minting_redeemer_data
      expect settlement_id == header_node_key
      True
    } else {
      expect None == m_settlement_redeemer_index
      True
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

          expect StateQueueNode { header: output_header_data, da_attestation } = output_node_data
          expect da_attestation == no_da_attestation

          // 4. The new block in state queue must have the hash of the header
          //    as its linked list key.
          let output_header_hash = crypto.blake2b_224(
            builtin.serialise_data(output_header_data),
          )
          expect output_header_key == output_header_hash

          // 5. New element's data must be `HeaderV1` with a valid time range.
          let output_header = decode_header_view(output_header_data)
          let output_prev_utxos_root = output_header.prev_utxos_root
          let output_start_time = output_header.start_time
          let output_end_time = output_header.end_time
          let output_prev_header_hash = output_header.prev_header_hash
          let output_operator_vkey = output_header.operator_vkey
          let output_protocol_version = output_header.protocol_version
          expect
            commit_bound_header_time_is_valid(
              output_start_time,
              output_end_time,
              validity_range,
            )
          expect header_view_is_valid(output_header)

          // 6. The operator pub key hash from the redeemer must match the one
          //    in the new header.
          expect output_operator_vkey == operator

          // Verify that it is the operator's turn to commit according to the
          // scheduler:
          // 7. The transaction must include a reference input with the
          //    scheduler NFT. Let that input's datum be the `scheduler_state`.
          expect scheduler.ActiveOperator { operator: scheduler_operator, .. } = scheduler.get_datum(
            reference_inputs: reference_inputs,
            scheduler_policy_id: scheduler_script_hash,
            scheduler_ref_input_index: scheduler_ref_input_index,
          )

          // 8. The `operator` field of `scheduler_state` must match `operator`
          //    from redeemer.
          expect operator == scheduler_operator

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
          expect operator == active_operator

          // To validate the output header, we must take two routes depending on
          // whether the anchor element is the root (i.e. confirmed state), or a
          // node (i.e. latest block in the queue).
          when m_anchor_key is {
            Some(header_hash) -> {
              // 10a. New block must properly carry over data from previous
              //      block.
              expect StateQueueNode { header: anchor_header_data, .. } = anchor_data
              let anchor_header = decode_header_view(anchor_header_data)
              expect output_prev_header_hash == header_hash
              expect output_prev_utxos_root == anchor_header.utxos_root
              expect output_start_time == anchor_header.end_time
              expect output_protocol_version == anchor_header.protocol_version

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
              expect output_prev_header_hash == header_hash
              expect output_prev_utxos_root == utxo_root
              expect output_start_time == end_time
              expect
                Some(output_protocol_version) == confirmed_state_next_header_protocol_version_v1(
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
              expect StateQueueNode { header: removed_node_header, .. } = removed_node_header_data
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
              expect StateQueueNode { header: removed_node_header, .. } = removed_node_header_data
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
