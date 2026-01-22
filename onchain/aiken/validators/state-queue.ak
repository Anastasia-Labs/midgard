use aiken/collection/list
use aiken/crypto
use aiken/interval.{Finite, Interval}
use aiken/primitive/bytearray
use aiken_design_patterns/linked_list
use aiken_design_patterns/validity_range_normalization as vrn
use cardano/assets.{PolicyId, quantity_of, tokens}
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
use midgard/protocol_parameters.{maturity_duration, shift_duration}
use midgard/registered_operators
use midgard/retired_operators
use midgard/scheduler
use midgard/settlement
use midgard/state_queue.{
  CommitBlockHeader, Deinit, Init, MergeToConfirmedState, MintRedeemer,
  RemoveFraudulentBlockHeader, block_asset_name_prefix,
  block_asset_name_prefix_length, confirmed_state_asset_name,
}

const five_minutes: PosixTime = 5 * 60 * 1000

validator spend(fraud_proof_catalog_mint_script_hash: ByteArray) {
  spend(
    _datum: Option<Data>,
    _redeemer: Data,
    _input: OutputReference,
    self: Transaction,
  ) {
    let tokens = tokens(self.mint, fraud_proof_catalog_mint_script_hash)

    unordered.list_state_transition(tokens)
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
          //    linked list requirements.
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
          expect output_end_time == inclusive_tx_upper_bound - 1

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
              |> linked_list.run_element_with(
                  active_operators_script_hash,
                  active_operators.root_asset_name,
                  active_operators.node_asset_name_prefix,
                  active_operators.node_asset_name_prefix_length,
                )
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
        settlement_redeemer_index,
      } -> {
        let Transaction { inputs, outputs, mint, redeemers, validity_range, .. } =
          self
        expect Some(confirmed_state_input) =
          list.at(inputs, confirmed_state_input_index)
        expect Some(header_node_input) =
          list.at(inputs, header_node_input_index)
        expect Some(continued_confirmed_state_output) =
          list.at(outputs, confirmed_state_output_index)

        let linked_list_eval = {
          // 1. Specified inputs and output must properly adhere to the fold and
          //    burn logic of the linked list.
          let
            input_confirmed_state_data,
            input_header_node_key,
            input_header_node_data,
            output_confirmed_state_data,
          <-
            linked_list.fold_and_burn(
              anchor_root_input: confirmed_state_input,
              folding_node_input: header_node_input,
              continued_anchor_root_output: confirmed_state_output,
              tx_mint: mint,
            )

          // 2. The key of the header node specified in the redeemer must
          //    properly associate with the input picked using
          //    `header_node_input_index`.
          expect input_header_node_key == header_node_key

          // 3. `header_node` must be mature---the lower bound of the
          //    transaction validity interval meets or exceeds the sum of the
          //    `end_time` field of `header_node` and the Midgard
          //    `maturity_duration` protocol parameter.
          expect
            when vrn.normalize_time_range(validity_range) is {
              vrn.ClosedRange { lower, .. } | vrn.ToPosInf { lower } ->
                lower >= header_input_end_time + maturity_duration
              _ -> fail @"Unexpected validity range"
            }

          // 4. a. `header_hash` of the input confirmed state must be preserved
          //       under `prev_header_hash` of the updated confirmed state.
          //    b. `start_time` of the input confirmed state must also carry
          //       over.
          // 5. From the merging header node, these values must be preserved in
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

          // 6. If either the `deposits_root`, or `withdrawals_root`, or
          //    `transactions_root` of `header_node` is _not_ the MPT root hash
          //    of the empty set, a `settlement` utxo must be produced via the
          //    Spawn redeemer. The redeemer must mention `header_node` by input
          //    index.
          //
          //    > [!NOTE]
          //    > We trust the redeemer provided to settlement because we expect
          //    > it to check them.
          expect and {
              input_header_transactions_root != empty_merkle_tree_root,
              input_header_deposits_root != empty_merkle_tree_root,
              input_header_withdrawals_root != empty_merkle_tree_root,
            } || {
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
            }

          // Done.
          True
        }
        linked_list_eval |> finalize_linked_list(own_policy_id)
      }
      RemoveFraudulentBlockHeader {
        removed_node_input_index,
        predecessor_node_input_index,
        predecessor_node_output_index,
        operators_redeemer_index,
        fraud_proof_ref_input_index,
        fraudulent_blocks_header_hash,
        fraudulent_operator,
        operator_is_active,
      } -> {
        let Transaction {
          inputs,
          outputs,
          reference_inputs,
          mint,
          redeemers,
          ..
        } = self

        // Remove the fraudulent block header:
        // 1. Let `fraudulent_operator` be a redeemer argument indicating the
        //    operator who committed the fraudulent block header.
        // 2. The transaction must Remove a node from the `state_queue`.
        //    Let `removed_node` be the removed node and `predecessor_node` be
        //    its anchor element.
        // 3. `fraudulent_operator` must match the operator retrieved from the
        //    spending UTxO from either the active operators directory, or the
        //    retired directory.
        expect Some(removed_node_input) =
          list.at(inputs, removed_node_input_index)
        expect Some(predecessor_node_input) =
          list.at(inputs, predecessor_node_input_index)
        expect Some(predecessor_node_output) =
          list.at(outputs, predecessor_node_output_index)

        let linked_list_eval = {
          // TODO: This should be fixed. The generic element removal does not
          //       work here as the list needs to be cut off (i.e. the tail must
          //       be disposed).
          let
            _m_predecessor_node_key,
            _predecessor_node_data,
            removed_node_key,
            removed_node_data,
          <-
            linked_list.remove(
              predecessor_node_input,
              removed_node_input,
              predecessor_node_output,
              mint,
            )

          // Slash the fraudulent operator:
          // 4. Let `operator_is_active` be a redeemer argument indicating
          //    whether the fraudulent operator is active or retired.
          expect
            if operator_is_active {
              // 5a. If `operator_is_active` is true:
              //     The transaction must Remove a node from the
              //     `active_operators` set via the Remove Operator Bad State
              //     redeemer. The `slashed_operator` argument provided to that
              //     redeemer must match `fraudulent_operator`.
              let active_operators_redeemer_data =
                utils.get_redeemer_at(
                  redeemers: redeemers,
                  expected_purpose: Mint(active_operators_script_hash),
                  redeemer_index: operators_redeemer_index,
                )
              expect active_operators.RemoveOperatorBadState {
                slashed_active_operator_key,
                ..
              } = active_operators_redeemer_data
              //
              fraudulent_operator == slashed_active_operator_key
            } else {
              // 5b. Otherwise:
              //     The transaction must Remove a node from the
              //     `retired_operators` set via the Remove Operator Bad State
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

          // Verify that fraud has been proved for the removed node or its
          // predecessor:
          // 6. The transaction must include a reference input holding a
          //    `fraud_proof` token.
          let referenced_fraudulent_header_hash =
            fraud_proof.get_proven_fraudulent_blocks_header_hash(
              reference_inputs,
              fraud_proof_script_hash,
              fraud_proof_ref_input_index,
            )
          expect
            referenced_fraudulent_header_hash == fraudulent_blocks_header_hash

          // 7. The removed block's key (i.e. its header hash) must match the
          //    one retrieved from the proven fraud token.
          expect removed_node_key == referenced_fraudulent_header_hash

          // Done
          True
        }
        linked_list_eval |> finalize_linked_list(policy_id)
      }
    }
  }

  // TODO: Add redeemers for escape hatch blocks

  else(_) {
    fail
  }
}
