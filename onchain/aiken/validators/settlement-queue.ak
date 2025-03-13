use aiken/builtin
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use aiken/interval.{Finite, Interval}
use aiken_design_patterns/linked_list/ordered
use aiken_design_patterns/linked_list/unordered.{
  Key, NodeDatum, get_node_inputs, get_node_outputs, list_state_transition,
}
use aiken_design_patterns/merkelized_validator
use aiken_design_patterns/singular_utxo_indexer
use cardano/assets.{PolicyId}
use cardano/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, Spend, Transaction,
}
use midgard/active_operators
use midgard/common/utils
use midgard/hub_oracle as hub
use midgard/ledger_state
use midgard/settlement_queue.{
  ClaimAsResolved, ConcludeSettlement, Datum, Deinit, Init, ListStateTransition,
  MintRedeemer, NodeData, RemoveSettlement, RevokeResolvedFlag, SpawnSettlement,
  SpendRedeemer, UpdateResolvedFlag, settlement_resolution_duration,
}
use midgard/state_queue
use midgard/user_events

validator spend(
  hub_oracle: ScriptHash,
  settlement_policy_id: ScriptHash,
  plutarch_phas_validator_hash: ScriptHash,
) {
  spend(
    m_datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    expect Some(node_datum) = m_datum

    expect NodeData {
      deposits_root,
      withdrawals_root,
      time_of_claimed_as_resolved,
      operator,
      ..
    } as node_data = node_datum.data

    when redeemer is {
      ListStateTransition ->
        list_state_transition(assets.tokens(tx.mint, settlement_policy_id))
      UpdateResolvedFlag {
        node_input_index,
        node_output_index,
        hub_ref_input_index,
        active_operators_node_input_index,
        active_operators_redeemer_index,
        purpose,
      } -> {
        let Transaction {
          inputs,
          outputs,
          reference_inputs,
          redeemers,
          extra_signatories,
          ..
        } = tx
        let hub.Datum {
          deposit: deposit_script_hash,
          withdrawal: withdrawal_script_hash,
          active_operators_addr,
          active_operators: active_operators_policy_id,
          ..
        } = hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

        // Using the singular one-to-one indexer design pattern.
        singular_utxo_indexer.spend(
          fn(in_ix, input, spend_purpose, out_ix, output) {
            // 1. Prevent double satisfaction (TODO: this is probably not
            //    needed).
            expect in_ix == out_ix

            let Input {
              output: Output { address: input_address, value: input_value, .. },
              ..
            } = input

            expect Output {
              address: cont_output_address,
              value: cont_output_value,
              datum: InlineDatum(cont_output_datum_data),
              ..
            } = output

            // 2. Make sure the UTxO is reproduced at own address.
            expect input_address == cont_output_address

            // 3. Spent value must be reproduced unchanged. TODO: This is
            //    relying on some additional ADA being available (i.e. to be
            //    paid to the active operator).
            expect input_value == cont_output_value

            // 4. The UTxO must not have been subject to fraudulent activity
            //    before.
            expect Some(operator_vkey) = operator

            when spend_purpose is {
              ClaimAsResolved -> {
                // 5. Operator must have signed this claim.
                expect operator_vkey |> utils.has_signed(extra_signatories)

                // 6. The UTxO must not have been claimed as resolved prior to
                //    this transaction.
                expect time_of_claimed_as_resolved == None

                // 7. Expect a spend redeemer at the specified redeemer index.
                expect Some(
                  Pair(Spend(active_operators_spend_purpose_out_ref),
                  active_operators_spend_redeemer_data),
                ) = list.at(redeemers, active_operators_redeemer_index)

                // 8. Found redeemer data must have the same structure as active
                //    operator's update commitment time redeemer.
                expect active_operators.UpdateCommitmentTime {
                  active_node_output_index,
                  ..
                }: active_operators.SpendRedeemer =
                  active_operators_spend_redeemer_data

                // Grab the active operators contract's input UTxO to validate
                // its address, and also to make sure the output reference here
                // matches the one from the `redeemers`.
                expect Some(Input {
                  output: Output { address: active_operators_node_address, .. },
                  output_reference: active_operators_out_ref,
                }) = list.at(inputs, active_operators_node_input_index)

                // 9. The picked active operators contract redeemer must point
                //    to the same input found based on the settlement queue's
                //    redeemer.
                expect
                  active_operators_out_ref == active_operators_spend_purpose_out_ref

                // 10. Provided input index of an active operator node must come
                //     from the address found from the hub oracle.
                expect active_operators_node_address == active_operators_addr

                // Grab the continued output of the active operators contract.
                expect Some(Output {
                  datum: InlineDatum(cont_active_operators_node_datum_data),
                  ..
                }) = list.at(outputs, active_node_output_index)

                // TODO: Update the `UpdateCommitmentTime` redeemer to carry
                // this key, instead of reading it from the output datum.
                expect ordered.NodeDatum {
                  key: ordered.Key(active_operators_node_key),
                  data: active_operators_data,
                  ..
                }: ordered.NodeDatum = cont_active_operators_node_datum_data

                // 11. The key of the node must be the same as current
                //     settlement queue's recorded operator validation key hash.
                expect active_operators_node_key == operator_vkey

                // 12. Active operator's updated datum is expected to record the
                //     new commitment time.
                expect active_operators.Datum{commitment_time: Some(
                  new_commitment_time,
                )}: active_operators.Datum = active_operators_data

                // 13. The only change in the updated datum must be the addition
                //     of the new commitment time as the moment of claim.
                let expected_cont_output_datum_data: Data =
                  NodeDatum {
                    ..node_datum,
                    data: NodeData {
                      ..node_data,
                      time_of_claimed_as_resolved: Some(new_commitment_time),
                    },
                  }
                cont_output_datum_data == expected_cont_output_datum_data
              }
              RevokeResolvedFlag {
                unresolved_event_ref_input_index,
                unresolved_event_asset_name,
                event_type,
                membership_proof,
              } -> {
                let (event_script, events_root) =
                  when event_type is {
                    settlement_queue.Deposit ->
                      (deposit_script_hash, deposits_root)
                    settlement_queue.Withdrawal ->
                      (withdrawal_script_hash, withdrawals_root)
                  }

                // 2. Find the specified (via its token name) event UTxO as a
                //    ref. input at given index. Its NFT must have the same
                //    policy as its address script hash.
                expect Input {
                  output: Output { datum: InlineDatum(event_datum_data), .. },
                  ..
                } =
                  utils.get_authentic_input_of(
                    reference_inputs,
                    event_script,
                    unresolved_event_asset_name,
                    unresolved_event_ref_input_index,
                  )

                // Unsafe coercion is fine here as we have already validated the
                // authenticity of the UTxO.
                let Pair(event_key, event_value) =
                  user_events.unsafe_to_key_value_pair(event_datum_data)

                // 3. The specified deposit event must be present in the stored
                //    tree of the settlement datum that's been flagged as fully
                //    resolved by its operator.
                expect
                  merkelized_validator.generic_delegated_validation(
                    staking_validator: plutarch_phas_validator_hash,
                    withdraw_redeemer_validator: fn(
                      membership_validator_redeemer: Data,
                    ) -> Bool {
                      expect [
                        input_root_data, input_key_data, input_value_data,
                        input_proof_data,
                        ..
                      ] = builtin.unconstr_fields(membership_validator_redeemer)
                      let mpf_root_data: Data = events_root
                      let proof_data: Data = membership_proof
                      and {
                        mpf_root_data == input_root_data,
                        event_key == input_key_data,
                        event_value == input_value_data,
                        proof_data == input_proof_data,
                      }
                    },
                    redeemers: redeemers,
                  )

                // 4. Expect a mint redeemer at the specified redeemer index.
                expect Some(
                  Pair(Mint(active_operators_mint_purpose_policy_id),
                  active_operators_mint_redeemer_data),
                ) = list.at(redeemers, active_operators_redeemer_index)

                // 5. The policy ID found via the redeemer must match the one
                //    found in the hub oracle.
                expect
                  active_operators_mint_purpose_policy_id == active_operators_policy_id

                // 6. Found redeemer data must have the same structure as active
                //    operator's bond slash redeemer.
                expect active_operators.RemoveOperatorSlashBond {
                  slashed_active_operator_key,
                  ..
                }: active_operators.MintRedeemer =
                  active_operators_mint_redeemer_data

                // 7. Operator vkey found in the slash bond redeemer must match
                //    the one stored in the spending settlement UTxO.
                expect slashed_active_operator_key == operator_vkey

                // 8. The only change in the datum should be removal of the
                //    claim, and also removal of the operator.
                let expected_cont_output_datum_data: Data =
                  NodeDatum {
                    ..node_datum,
                    data: NodeData {
                      ..node_data,
                      time_of_claimed_as_resolved: None,
                      operator: None,
                    },
                  }
                cont_output_datum_data == expected_cont_output_datum_data
              }
            }
          },
          purpose,
          node_input_index,
          node_output_index,
          own_out_ref,
          tx,
        )
      }
      ConcludeSettlement ->
        list_state_transition(assets.tokens(tx.mint, settlement_policy_id))
    }
  }

  else(_) {
    fail
  }
}

validator mint(hub_oracle: ScriptHash) {
  mint(redeemer: MintRedeemer, own_policy: PolicyId, tx: Transaction) {
    let Transaction {
      inputs,
      reference_inputs,
      outputs,
      mint,
      redeemers,
      extra_signatories,
      validity_range,
      ..
    } = tx

    let node_in_outputs = get_node_inputs(inputs, own_policy)
    let node_outputs = get_node_outputs(outputs, own_policy)
    let node_mint = assets.tokens(mint, own_policy)

    when redeemer is {
      Init -> and {
          assets.quantity_of(mint, hub_oracle, hub.asset_name) == 1,
          unordered.init(node_outputs, node_mint, own_policy),
        }
      Deinit -> and {
          assets.quantity_of(mint, hub_oracle, hub.asset_name) == -1,
          unordered.deinit(node_in_outputs, node_mint, own_policy),
        }
      SpawnSettlement {
        anchor_node_output_index,
        node_output_index,
        state_queue_merge_redeemer_index,
        hub_ref_input_index,
      } -> {
        let hub.Datum { state_queue, state_queue_addr, .. } =
          hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

        // 1. Ensure merge endpoint of state queue is present in the tx.
        expect Some(state_queue_redeemer_pair) =
          redeemers |> list.at(state_queue_merge_redeemer_index)
        expect state_queue_redeemer_pair.1st == Mint(state_queue)
        expect state_queue.MergeToConfirmedState {
          header_node_input_index,
          confirmed_state_node_output_index,
          ..
        }: state_queue.MintRedeemer = state_queue_redeemer_pair.2nd

        // 2. Grab the header hash of the new confirmed state to use as the
        //    asset name of the settlement authentication NFTs.
        expect Some(Output {
          datum: InlineDatum(confirmed_state_datum_data),
          ..
        }) = outputs |> list.at(confirmed_state_node_output_index)
        expect ledger_state.ConfirmedState {
          header_hash: confirmed_state_header_hash,
          ..
        } = confirmed_state_datum_data

        // 3. Validate appending to the end of settlement queue.
        expect
          unordered.append_unsafe(
            Key(confirmed_state_header_hash),
            node_output_index,
            anchor_node_output_index,
            node_in_outputs,
            node_outputs,
            node_mint,
          )

        // 4. Fetch the state commitment UTxO that's being merged into the
        //    confirmed state.
        expect Input {
          output: Output { datum: InlineDatum(block_datum_data), .. },
          ..
        } =
          utils.get_authentic_input_with_policy_at_address(
            inputs,
            state_queue_addr,
            state_queue,
            state_queue.block_asset_name,
            header_node_input_index,
          )
        expect block_datum: state_queue.Datum = block_datum_data
        expect ledger_state.Header {
          operator_vkey,
          deposits_root,
          withdrawals_root,
          start_time,
          end_time,
          ..
        } = block_datum.data

        // 5. Validate the produced output datum.
        expect Some(Output {
          datum: InlineDatum(settlement_output_datum_data),
          ..
        }) = list.at(node_outputs, node_output_index)
        expect parsed_settlement_output_datum: Datum =
          settlement_output_datum_data
        let expected_produced_node_data: Data =
          NodeData {
            deposits_root,
            withdrawals_root,
            start_time,
            end_time,
            time_of_claimed_as_resolved: None,
            operator: Some(operator_vkey),
          }
        parsed_settlement_output_datum.data == expected_produced_node_data
      }
      RemoveSettlement {
        key_to_remove,
        removed_node_input_index,
        anchor_node_input_index,
      } -> {
        expect Finite(valid_from) = validity_range.lower_bound.bound_type
        expect Some(Output { datum: InlineDatum(removed_node_datum_data), .. }) =
          node_in_outputs |> list.at(removed_node_input_index)
        expect parsed_removed_node_datum: Datum = removed_node_datum_data
        expect NodeData {
          time_of_claimed_as_resolved: Some(
            unwrapped_time_of_claimed_as_resolved,
          ),
          operator: Some(operator_vkey),
          ..
        }: NodeData = parsed_removed_node_datum.data
        and {
          unordered.remove(
            Key(key_to_remove),
            removed_node_input_index,
            anchor_node_input_index,
            node_in_outputs,
            node_outputs,
            node_mint,
          ),
          operator_vkey |> utils.has_signed(extra_signatories),
          unwrapped_time_of_claimed_as_resolved + settlement_resolution_duration <= valid_from,
        }
      }
    }
  }

  else(_) {
    fail
  }
}
