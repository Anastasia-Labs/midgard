use aiken/collection/list
use aiken/crypto
use aiken/primitive/bytearray
use aiken_design_patterns/linked_list
use aiken_design_patterns/validity_range_normalization as vrn
use cardano/assets.{PolicyId, quantity_of}
use cardano/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, Spend, Transaction,
}
use midgard/active_operators
use midgard/common/types.{PosixTime}
use midgard/common/utils
use midgard/hub_oracle as hub
use midgard/ledger_state.{
  ConfirmedState, Header, genesis_header_hash, genesis_protocol_version,
  genesis_utxo_root,
}
use midgard/operator_directories/active as active_operators
use midgard/operator_directories/common as operator_directory
use midgard/operator_directories/registered as registered_operators
use midgard/operator_directories/retired as retired_operators
use midgard/protocol_parameters.{maturity_duration}
use midgard/scheduler
use midgard/settlement
use midgard/state_queue.{
  CommitBlockHeader, Deinit, Init, MergeToConfirmedState, MintRedeemer,
  RemoveFraudulentBlockHeader, block_asset_name_prefix,
  block_asset_name_prefix_length, confirmed_state_asset_name,
}

const five_minutes: PosixTime = 5 * 60 * 1000

validator spend(state_queue_mint_script_hash: ByteArray) {
  spend(
    _datum: Option<Data>,
    _redeemer: Data,
    _own_ref: OutputReference,
    tx: Transaction,
  ) {
    linked_list.spend_for_adding_or_removing_an_element(
      state_queue_mint_script_hash,
      tx.mint,
    )
  }

  else(_) {
    fail
  }
}

validator mint(
  hub_oracle_script_hash: PolicyId,
  active_operators_script_hash: PolicyId,
  retired_operators_script_hash: PolicyId,
  scheduler_script_hash: PolicyId,
  fraud_proof_script_hash: PolicyId,
  settlement_script_hash: PolicyId,
) {
  mint(redeemer: MintRedeemer, own_policy_id: PolicyId, self: Transaction) {
    when redeemer is {
      Init { output_index } -> {
        let Transaction { outputs, mint, validity_range, .. } = self

        expect Some(root_output) = outputs |> list.get(output_index)

        expect vrn.ClosedRange {
          lower: current_time_lower,
          upper: current_time_upper,
        } = vrn.normalize_time_range(validity_range)

        // 1. Estimate current time by taking the upper bound of a time range
        //    that is shorter than 5 minutes.
        expect current_time_upper - current_time_lower <= five_minutes

        // 2. The produced root UTxO must have a `ConfirmedState` data with zero
        //    hashes, and an empty ledger (i.e. `utxo_root` must be that of an
        //    empty Merkle trie).
        let expected_root_data: Data =
          ConfirmedState {
            header_hash: genesis_header_hash,
            prev_header_hash: genesis_header_hash,
            utxo_root: genesis_utxo_root,
            start_time: current_time_upper,
            end_time: current_time_upper,
            protocol_version: genesis_protocol_version,
          }

        // 3. Linked list must be initiated with the expected root data, and
        //    validation that a hub oracle NFT is getting minted.
        expect
          linked_list.init(
            nonce_validated: quantity_of(
              mint,
              hub_oracle_script_hash,
              hub.asset_name,
            ) == 1,
            produced_element_output: root_output,
            tx_mint: mint,
            root_data_validator: fn(root_data: Data) -> Bool {
              root_data == expected_root_data
            },
          )
            |> linked_list.run_root_with(
                own_policy_id,
                confirmed_state_asset_name,
              )

        // Done.
        True
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
      CommitBlockHeader {
        latest_block_input_index,
        new_block_output_index,
        continued_latest_block_output_index,
        operator,
        scheduler_ref_input_index,
        active_operators_input_index,
        active_operators_redeemer_index,
      } -> {
        let Transaction {
          inputs,
          outputs,
          reference_inputs,
          extra_signatories,
          redeemers,
          validity_range,
          ..
        } = self
        // Commit the block header to the state queue, with the operator's
        // content:
        // 1. Let `operator` be a redeemer argument indicating the pub key hash
        //    of the operator committing the block header.
        // 2. The transaction must be signed by `operator`.
        expect operator |> has_signed(extra_signatories)

        expect Some(anchor_element_input) =
          inputs |> list.at(latest_block_input_index)
        expect Some(continued_anchor_element_output) =
          outputs |> list.at(continued_latest_block_output_index)
        expect Some(new_block_output) =
          inputs |> list.at(new_block_output_index)

        let linked_list_eval = {
          // 3. Indicated inputs and output must properly adhere to unordered
          //    linked list append requirements.
          let
            m_anchor_key,
            anchor_data,
            output_header_key,
            output_header_data,
          <-
            linked_list.append_unordered(
              anchor_element_input,
              continued_anchor_element_output,
              new_block_output,
              mint,
            )

          // 4. The new block in state queue must have the hash of the header as
          //    its linked list key.
          let output_header_hash =
            crypto.blake2b_224(builtin.un_b_data(output_header_data))
          expect output_header_key == output_header_hash

          // 5. New element's data must be `Header`.
          expect Header {
            prev_utxos_root: output_prev_utxos_root,
            start_time: output_start_time,
            end_time: output_end_time,
            prev_header_hash: output_prev_header_hash,
            operator_vkey: output_operator_vkey,
            protocol_version: output_protocol_version,
            ..
          } = output_header_data

          // 6. The `end_time` of `header_node` must match the transaction's
          //    time-validity upper bound.
          //
          //    > [!NOTE]
          //    > We are assuming inclusive lower bound and exclusive upper
          //    > bound in the block. TODO: This needs clarification in specs.
          let inclusive_tx_upper_bound =
            utils.get_inclusive_upper_bound_of_interval(validity_range)
          expect output_end_time == inclusive_tx_upper_bound + 1

          // 7. The operator pub key hash from the redeemer must match the one
          //    in the new header.
          expect output_operator_vkey == operator

          // Verify that it is the operator's turn to commit according to the
          // scheduler:
          // 8. The transaction must include a reference input with the
          //    scheduler NFT. Let that input's datum be the `scheduler_state`.
          let scheduler.Datum {
            operator: scheduler_operator,
            start_time: scheduler_start_time,
          } =
            scheduler.get_datum(
              reference_inputs: reference_inputs,
              scheduler_policy_id: scheduler_script_hash,
              scheduler_ref_input_index: scheduler_ref_input_index,
            )

          // 9. The `operator` field of `scheduler_state` must match `operator`
          //    from redeemer.
          expect operator == scheduler_operator

          // 10. The `end_time` of `header_node` must be within the shift
          //     interval of `scheduler_state`.
          expect output_end_time >= scheduler_start_time
          expect
            output_end_time <= scheduler.shift_end_time(scheduler_state_time)

          // Update the operator's timestamp in the active operators set:
          // 11. The transaction must include an input, spent via the Update
          //     Bond Hold New State redeemer, of an `active_operators` node
          //     with a key matching the `operator`.
          //
          //     Here we are indirectly validating the active operators spending
          //     script is invoked by checking the presence of the linked list
          //     NFT.
          expect Some(Input {
            output_reference: active_node_outref,
            output: active_node_output,
          }) = inputs |> list.at(active_operators_input_index)
          let active_operators_linked_list_eval =
            linked_list.get_element_info(
              active_node_output,
              fn(m_active_node_key, _, _) -> Bool {
                Some(operator) == m_active_node_key
              },
            )
          expect
            active_operators_linked_list_eval
              |> linked_list.finalize_linked_list(active_operators_script_hash)
          //
          expect active_operators.UpdateBondHoldNewState { .. } =
            redeemers
              |> utils.get_redeemer_at(
                  Spend(active_node_outref),
                  active_operators_redeemer_index,
                )

          // To validate the output header, we must take two routes depending on
          // whether the anchor element is the root (i.e. confirmed state), or a
          // node (i.e. latest block in the queue).
          when m_anchor_key is {
            Some(header_hash) -> {
              // 12a. New block must properly carry over data from previous
              //      block.
              expect Header { utxos_root, end_time, protocol_version, .. } =
                anchor_data
              expect output_prev_header_hash == header_hash
              expect output_prev_utxos_root == utxo_root
              expect output_start_time == end_time
              expect output_protocol_version == protocol_version

              // Done.
              True
            }
            None -> {
              // 12b. New block must properly carry over data from confirmed
              //      state.
              expect ConfirmedState {
                header_hash,
                utxo_root,
                end_time,
                protocol_version,
                ..
              } = anchor_data
              expect output_prev_header_hash == header_hash
              expect output_prev_utxos_root == utxo_root
              expect output_start_time == end_time
              expect output_protocol_version == protocol_version

              // Done.
              True
            }
          }
        }
        linked_list_eval |> finalize_linked_list(own_policy_id)
      }
      MergeToConfirmedState {
        header_node_key,
        header_node_input_index,
        confirmed_state_input_index,
        confirmed_state_output_index,
        m_settlement_redeemer_index,
      } -> {
        let Transaction { inputs, outputs, mint, redeemers, validity_range, .. } =
          self

        // 1. Inputs and output of linked list must exist in the specified
        //    indices.
        expect Some(confirmed_state_input) =
          list.at(inputs, confirmed_state_input_index)
        expect Some(header_node_input) =
          list.at(inputs, header_node_input_index)
        expect Some(continued_confirmed_state_output) =
          list.at(outputs, confirmed_state_output_index)

        let linked_list_eval = {
          // 2. Specified inputs and output must properly adhere to the fold and
          //    burn logic of the linked list.
          let
            input_confirmed_state_data,
            input_header_node_key,
            input_header_node_data,
            _input_header_link,
            output_confirmed_state_data,
          <-
            linked_list.fold_from_root(
              confirmed_state_input,
              header_node_input,
              confirmed_state_output,
              mint,
            )

          // 3. The key of the header node specified in the redeemer must
          //    properly associate with the input picked using
          //    `header_node_input_index`.
          expect input_header_node_key == header_node_key

          // 4. `header_node` must be mature---the lower bound of the
          //    transaction validity interval meets or exceeds the sum of the
          //    `end_time` field of `header_node` and the Midgard
          //    `maturity_duration` protocol parameter.
          let inclusive_tx_lower_bound =
            utils.get_inclusive_lower_bound_of_interval(validity_range)
          expect
            inclusive_tx_lower_bound >= header_input_end_time + maturity_duration

          // 5. a. `header_hash` of the input confirmed state must be preserved
          //       under `prev_header_hash` of the updated confirmed state.
          //    b. `start_time` of the input confirmed state must also carry
          //       over.
          // 6. From the merging header node, these values must be preserved in
          //    the new confirmed state:
          //    a. `utxos_root` must be the new confirmed ledger.
          //    b. `end_time` must replace confirmed state's.
          //    c. `protocol_version` should also be that of the new block's
          //       (TODO?).
          //    d. Key (i.e. header hash) must also be saved under
          //       `header_hash`.
          expect ConfirmedState {
            start_time: input_confirmed_state_start_time,
            header_hash: input_confirmed_state_header_hash,
            ..
          } = input_confirmed_state_data
          expect Header {
            utxos_root: input_header_utxos_root,
            end_time: input_header_end_time,
            protocol_version: input_header_protocol_version,
            transactions_root: input_header_transactions_root,
            deposits_root: input_header_deposits_root,
            withdrawals_root: input_header_withdrawals_root,
            ..
          } = header_node_data
          let expected_output_confirmed_state_data: Data =
            ConfirmedState {
              header_hash: header_node_key,
              prev_header_hash: input_confirmed_state_header_hash,
              utxo_root: input_header_utxos_root,
              start_time: input_confirmed_state_start_time,
              end_time: input_header_end_time,
              protocol_version: input_header_protocol_version,
            }
          expect
            output_confirmed_state_data == expected_output_confirmed_state_data

          // 7. If either the `deposits_root`, or `withdrawals_root`, or
          //    `transactions_root` of `header_node` is _not_ the MPT root hash
          //    of the empty set, a `settlement` utxo must be produced via the
          //    Spawn redeemer. The redeemer must mention `header_node` by input
          //    index.
          //
          //    > [!NOTE]
          //    > We trust the redeemer provided to settlement because we expect
          //    > it to check the equality between its `settlement_id` and
          //    > `header_node_key` here from the redeemer here.
          if or {
            input_header_transactions_root != empty_merkle_tree_root,
            input_header_deposits_root != empty_merkle_tree_root,
            input_header_withdrawals_root != empty_merkle_tree_root,
          } {
            // 8a. The redeemer index for settlement minting policy must be
            //     provided.
            expect Some(settlement_redeemer_index) = m_settlement_redeemer_index
            let settlement_minting_redeemer_data =
              redeemers
                |> utils.get_redeemer_at(
                    Mint(settlement_script_hash),
                    settlement_redeemer_index,
                  )
            expect settlement.Spawn { settlement_id, .. }: settlement.MintRedeemer =
              settlement_minting_redeemer_data
            //
            expect settlement_id == header_node_key

            // Done.
            True
          } else {
            // 8b. No mint redeemer index must be provided. Settlement contract
            //     uses this argument to disallow minting empty settlement
            //     utxos.
            expect None == m_settlement_redeemer_index

            // Done.
            True
          }
        }
        linked_list_eval |> finalize_linked_list(own_policy_id)
      }
      RemoveFraudulentBlockHeader {
        fraudulent_operator,
        fraudulent_blocks_header_hash,
        slashing_approach,
        fraudulent_node_input_index,
        fraud_proof_ref_input_index,
        block_removal_approach,
      } -> {
        let Transaction {
          inputs,
          outputs,
          reference_inputs,
          mint,
          redeemers,
          ..
        } = self

        // Slash the fraudulent operator:
        // 1. Let `slashing_approach` be a sum type indicating whether the
        //    fraudulent operator is active, retired, or already slashed.
        expect
          when slashing_approach is {
            SlashActiveOperator { active_operators_redeemer_index } -> {
              // 2a. If the operator is active:
              //     The transaction must Remove a node from the
              //     `active_operators` set via the `RemoveOperatorBadState`
              //     redeemer. The `slashed_operator` argument provided to that
              //     redeemer must match `fraudulent_operator`.
              let active_operators_redeemer_data =
                utils.get_redeemer_at(
                  redeemers: redeemers,
                  expected_purpose: Mint(active_operators_script_hash),
                  redeemer_index: active_operators_redeemer_index,
                )
              expect active_operators.RemoveOperatorBadState {
                slashed_active_operator_key,
                ..
              } = active_operators_redeemer_data
              //
              fraudulent_operator == slashed_active_operator_key
            }
            SlashRetiredOperator { retired_operators_redeemer_index } -> {
              // 2b. If the operator is retired:
              //     The transaction must Remove a node from the
              //     `retired_operators` set via the `RemoveOperatorBadState`
              //     redeemer. The `slashed_operator` argument provided to that
              //     redeemer must match `fraudulent_operator`.
              let retired_operators_redeemer_data =
                utils.get_redeemer_at(
                  redeemers: redeemers,
                  expected_purpose: Mint(active_operators_script_hash),
                  redeemer_index: operators_redeemer_index,
                )
              expect retired_operators.RemoveOperatorBadState {
                slashed_retired_operator_key,
                ..
              } = retired_operators_redeemer_data
              //
              fraudulent_operator == slashed_retired_operator_key
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
                      element_input_index: active_operators_element_ref_input_index,
                    )
                  |> active_operators.finalize_linked_list(
                      active_operators_script_hash,
                    ),
                fraudulent_operator
                  |> operator_directory.operator_is_not_a_member(
                      reference_inputs: reference_inputs,
                      element_input_index: retired_operators_element_ref_input_index,
                    )
                  |> retired_operators.finalize_linked_list(
                      nft_policy_id: retired_operators_script_hash,
                    ),
              }
          }

        expect Some(fraudulent_node_input) =
          inputs |> list.at(fraudulent_node_input_index)

        // Remove fraudulent block or its link from the state queue:
        // 3. Let `block_removal_approach` be a redeemer argument that specifies
        //    whether the fraudulent block itself can be removed:
        when block_removal_approach is {
          RemoveFraudulentBlocksLink {
            fraudulent_node_output_index,
            removed_block_input_index,
          } -> {
            // 4a. If the fraudulent block has a link, its successor node must
            //     be removed first.
            //
            //     > [!NOTE]
            //     > The successor node is also considered fraudulent because of
            //     > getting appended to a fraudulent block, so it does not need
            //     > to have an associated fraud proof token.
            expect Some(removed_block_input) =
              inputs |> list.at(removed_block_input_index)
            expect Some(fraudulent_node_output) =
              outputs |> list.at(fraudulent_node_output_index)
            let linked_list_eval = {
              let
                m_fraudulent_header_hash,
                _fraudulent_header_data,
                removed_node_header_hash,
                removed_node_header_data,
                _removed_node_link,
              <-
                linked_list.remove(
                  fraudulent_node_input,
                  removed_block_input,
                  fraudulent_node_output,
                  mint,
                )
              expect
                Some(fraudulent_blocks_header_hash) == m_fraudulent_header_hash
              expect Header { operator_vkey, .. } = removed_node_header_data
              operator_vkey == fraudulent_operator
            }
          }
          RemoveLastFraudulentBlock {
            anchor_element_input_index,
            anchor_element_output_index,
          } -> {
            // 4b. If the fraudulent block has no links, it can be removed from
            //     the very end of the state queue.
            expect Some(anchor_element_input) =
              inputs |> list.at(anchor_element_input_index)
            expect Some(anchor_element_output) =
              outputs |> list.at(anchor_element_output_index)
            let linked_list_eval = {
              let
                _m_anchor_key,
                _anchor_data,
                removed_node_header_hash,
                removed_node_header_data,
                removed_node_link,
              <-
                linked_list.remove(
                  anchor_element_input,
                  fraudulent_node_input,
                  anchor_element_output,
                  mint,
                )
              expect fraudulent_blocks_header_hash == removed_node_header_hash
              expect Header { operator_vkey, .. } = removed_node_header_data
              expect operator_vkey == fraudulent_operator
              removed_node_link == None
            }
          }
        }

        // Verify that fraud has been proved for the specified block header:
        // 5. The transaction must include a reference input holding a
        //    `fraud_proof` token.
        let referenced_fraudulent_header_hash =
          reference_inputs
            |> fraud_proof.get_proven_fraudulent_blocks_header_hash(
                fraud_proof_script_hash,
                fraud_proof_ref_input_index,
              )
        expect
          referenced_fraudulent_header_hash == fraudulent_blocks_header_hash

        // Done.
        True
      }
    }
  }

  // TODO: Add redeemers for escape hatch blocks

  else(_) {
    fail
  }
}
