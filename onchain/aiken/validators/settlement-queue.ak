use aiken/collection/list
use aiken/crypto.{ScriptHash}
use aiken/interval.{Finite, Interval}
use aiken_design_patterns/linked_list/unordered.{
  Key, NodeDatum, get_node_inputs, get_node_outputs, list_state_transition,
}
use cardano/assets.{PolicyId}
use cardano/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, Spend, Transaction,
}
use midgard/active_operators
use midgard/common/utils
use midgard/hub_oracle as hub
use midgard/ledger_state
use midgard/registered_operators
use midgard/retired_operators
use midgard/scheduler.{Datum as SchedulerDatum, scheduler_asset_name}
use midgard/settlement_queue.{
  AppendSettlementNode, AttachResolutionClaim, Datum, Deinit,
  DisproveResolutionClaim, Init, ListStateTransition, MintRedeemer, NodeData,
  RemoveSettlementNode, ResolutionClaim, SpendRedeemer,
}
use midgard/state_queue

validator spend(hub_oracle: ScriptHash, settlement_policy_id: ScriptHash) {
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
      tx_orders_root,
      resolution_claim,
      ..
    } as input_node_data = node_datum.data

    when redeemer is {
      ListStateTransition ->
        list_state_transition(assets.tokens(tx.mint, settlement_policy_id))
      AttachResolutionClaim {
        node_input_index,
        node_output_index,
        hub_ref_input_index,
        active_operators_node_input_index,
        active_operators_redeemer_index,
        operator,
        scheduler_ref_input_index,
      } -> {
        // 1. The spent input must be a settlement node without a resolution claim.
        expect resolution_claim == None

        // 2. The spent input must be reproduced as a settlement node with a resolution claim.
        let Output { address: input_address, value: input_value, .. } =
          utils.get_own_input_at(tx.inputs, own_out_ref, node_input_index)
        //
        expect Some(Output {
          address: cont_output_address,
          value: cont_output_value,
          datum: InlineDatum(cont_output_datum),
          ..
        }) = tx.outputs |> list.at(node_output_index)
        //
        expect NodeData {
          resolution_claim: Some(ResolutionClaim {
            resolution_time,
            operator: resolution_operator,
          }) as output_resolution_claim,
          ..
        } as output_node_data = cont_output_datum
        //
        expect input_address == cont_output_address
        expect input_value == cont_output_value
        // TODO: inefficient but implicitly safe
        expect
          output_node_data == NodeData {
            ..input_node_data,
            resolution_claim: output_resolution_claim,
          }

        // 3. The transaction must be signed by the resolution claim's operator.
        expect operator == resolution_operator
        expect operator |> utils.has_signed(tx.extra_signatories)

        // 4. The transaction must include the Midgard hub oracle NFT in a reference input.
        // 5. hub_oracle.{active_operators, scheduler}
        let hub.Datum { active_operators_addr, scheduler, .. } =
          hub.get_datum(tx.reference_inputs, hub_oracle, hub_ref_input_index)

        // 6. The transaction must include an input (`operator_node`), spent via the Update Bond
        //    Hold New Settlement redeemer, of an `active_operators` node with a key matching
        //    the resolution claim's operator.
        expect Some(
          Pair(Spend(active_operators_spend_purpose_out_ref),
          active_operators_spend_redeemer_data),
        ) = list.at(tx.redeemers, active_operators_redeemer_index)
        //
        expect active_operators.UpdateBondHoldNewSettlement {
          new_bond_unlock_time,
          ..
        }: active_operators.SpendRedeemer = active_operators_spend_redeemer_data
        //
        expect Some(Input {
          output: Output { address: active_operators_node_address, .. },
          output_reference: active_operators_out_ref,
        }) = list.at(tx.inputs, active_operators_node_input_index)
        //
        expect and {
            active_operators_out_ref == active_operators_spend_purpose_out_ref,
            active_operators_node_address == active_operators_addr,
          }

        // 7. The `bond_unlock_time` of the `operator_node` must match the `resolution_time`
        //    of the resolution claim.
        expect resolution_time == new_bond_unlock_time

        // 8. The transaction must include the `scheduler` utxo as a reference input,
        //    indicating that the current operator matches the resolution claim's operator.
        expect Input {
          output: Output { datum: InlineDatum(scheduler_state_datum), .. },
          ..
        } =
          utils.get_authentic_input_with_nft_at(
            tx.reference_inputs,
            scheduler,
            scheduler_asset_name,
            scheduler_ref_input_index,
          )
        expect parsed_scheduler_state_datum: SchedulerDatum =
          scheduler_state_datum
        expect operator == parsed_scheduler_state_datum.operator

        // Done. 
        True
      }
      DisproveResolutionClaim {
        node_input_index,
        node_output_index,
        hub_ref_input_index,
        operators_redeemer_index,
        operator,
        operator_status,
        unresolved_event_ref_input_index,
        unresolved_event_asset_name,
        event_type,
        membership_proof,
      } -> {
        // 1. The spent input must be a settlement node with a resolution claim.
        expect Some(ResolutionClaim {
          resolution_time,
          operator: resolution_operator,
        }) = resolution_claim
        expect operator == resolution_operator

        // 2. The spent input must be reproduced as a settlement node without a resolution claim.
        let Output { address: input_address, value: input_value, .. } =
          utils.get_own_input_at(tx.inputs, own_out_ref, node_input_index)
        //
        expect Some(Output {
          address: cont_output_address,
          value: cont_output_value,
          datum: InlineDatum(cont_output_datum),
          ..
        }) = tx.outputs |> list.at(node_output_index)
        //
        expect NodeData { .. } as output_node_data = cont_output_datum
        //
        expect input_address == cont_output_address
        expect input_value == cont_output_value
        // TODO: inefficient but implicitly safe
        expect
          output_node_data == NodeData {
            ..input_node_data,
            resolution_claim: None,
          }

        // 3. The transaction must include the Midgard hub oracle NFT in a reference input.
        // 4. hub_oracle.{active_operators, deposit, withdrawal}
        let hub.Datum {
          active_operators,
          deposit: deposit_script_hash,
          withdrawal: withdrawal_script_hash,
          tx_order: tx_order_script_hash,
          retired_operators,
          ..
        } = hub.get_datum(tx.reference_inputs, hub_oracle, hub_ref_input_index)

        // 5. The transaction must include either a deposit, a withdrawal order,
        //    or an L2 transaction order as a reference input. Let that
        //    reference input be `unprocessed_event`.
        let (event_script, events_root) =
          when event_type is {
            settlement_queue.Deposit -> (deposit_script_hash, deposits_root)
            settlement_queue.Withdrawal ->
              (withdrawal_script_hash, withdrawals_root)
            settlement_queue.TxOrder -> (tx_order_script_hash, tx_orders_root)
          }
        expect Input {
          output: Output { datum: InlineDatum(event_datum_data), .. },
          ..
        } =
          utils.get_authentic_input_with_nft_at(
            tx.reference_inputs,
            event_script,
            unresolved_event_asset_name,
            unresolved_event_ref_input_index,
          )
        // Unsafe coercion is fine here as we have already validated the
        // authenticity of the UTxO.
        let Pair(event_key, event_value) =
          ledger_state.unsafe_event_to_key_value_pair(event_datum_data)

        // 6. A valid membership proof must be provided, proving that `unprocessed_event` is a
        //    member of the corresponding tree in the settlement node.
        expect
          utils.plutarch_phas(
            mpf_root: events_root,
            key: event_key,
            value: event_value,
            membership_proof: membership_proof,
            redeemers: tx.redeemers,
          )

        // 7. The transaction's time-validity upper bound must be earlier than
        //    the resolution claim's resolution time.
        expect Finite(valid_to) = tx.validity_range.upper_bound.bound_type
        expect valid_to < resolution_time

        // 8. Let `operator_status` be a redeemer argument indicating whether `operator`
        //    is active or retired.
        // TODO: define a sub-type that is only active or retired. (Not registered)
        when operator_status is {
          registered_operators.Active -> {
            // 9. The transaction must remove a node from the active operators list
            //    via the Remove Operator Bad Settlement redeemer.
            expect Some(
              Pair(Mint(active_operators_mint_purpose_policy_id),
              active_operators_mint_redeemer_data),
            ) = list.at(tx.redeemers, operators_redeemer_index)
            //
            expect active_operators_mint_purpose_policy_id == active_operators
            //
            expect active_operators.RemoveOperatorBadSettlement {
              slashed_active_operator_key,
              ..
            }: active_operators.MintRedeemer =
              active_operators_mint_redeemer_data
            //
            expect slashed_active_operator_key == operator
            //
            True
          }
          registered_operators.Retired -> {
            // 10. The transaction must remove a node from the retired operators list
            //     via the Remove Operator Bad Settlement redeemer.
            expect Some(
              Pair(Mint(retired_operators_mint_purpose_policy_id),
              retired_operators_mint_redeemer_data),
            ) = list.at(tx.redeemers, operators_redeemer_index)
            //
            expect retired_operators_mint_purpose_policy_id == retired_operators
            //
            expect retired_operators.RemoveOperatorBadSettlement {
              slashed_retired_operator_key,
              ..
            }: retired_operators.MintRedeemer =
              retired_operators_mint_redeemer_data
            //
            expect slashed_retired_operator_key == operator
            //
            True
          }
          _ -> fail @"Bad operator status"
        }
      }
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
      AppendSettlementNode {
        key_to_add,
        anchor_node_output_index,
        node_output_index,
        state_queue_merge_redeemer_index,
        hub_ref_input_index,
      } -> {
        // 1. The transaction must include the Midgard hub oracle NFT in a reference input.
        // 2. hub_oracle.{state_queue}
        let hub.Datum { state_queue, .. } =
          hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

        // 3. The transaction must Remove a `state_queue` node via the Merge To Confirmed
        //    State redeemer. Let `merged_block` be the block being merged to the confirmed
        //    state, and let `header_hash` be its header-hash key.
        //
        // Note: we trust the redeemer arguments provided to state_queue
        // because we expect it to check them.
        expect Some(state_queue_redeemer_pair) =
          redeemers |> list.at(state_queue_merge_redeemer_index)
        expect state_queue_redeemer_pair.1st == Mint(state_queue)
        expect state_queue.MergeToConfirmedState {
          header_node_key,
          header_node_input_index,
          ..
        }: state_queue.MintRedeemer = state_queue_redeemer_pair.2nd
        // We verify this redeemer argument so that other scripts can use it:
        expect header_node_key == key_to_add
        //
        expect Some(merged_block) = list.at(tx.inputs, header_node_input_index)
        expect Input {
          output: Output { datum: InlineDatum(block_datum_data), .. },
          ..
        } = merged_block
        //
        expect block_datum: state_queue.Datum = block_datum_data
        expect ledger_state.Header {
          deposits_root,
          withdrawals_root,
          tx_orders_root,
          start_time,
          end_time,
          ..
        } = block_datum.data

        // 4. The transaction must Append a node to the settlement queue with a key matching
        // `header_hash`. Let `new_settlement_node` be the node being appended.
        expect
          unordered.append_unsafe(
            Key(key_to_add),
            node_output_index,
            anchor_node_output_index,
            node_in_outputs,
            node_outputs,
            node_mint,
          )
        //
        expect Some(Output {
          datum: InlineDatum(settlement_output_datum_data),
          ..
        }) = list.at(node_outputs, node_output_index)

        // 5. `merged_block` and `new_settlement_node` must match on all these fields:
        //    `deposits_root`, `withdrawals_root`, `tx_orders_root`, `start_time`,
        //    `end_time`.
        // 6. The `resolution_claim` of `new_settlement_node` must be empty.
        expect parsed_settlement_output_datum: Datum =
          settlement_output_datum_data
        let expected_produced_node_data: Data =
          NodeData {
            deposits_root,
            withdrawals_root,
            tx_orders_root,
            start_time,
            end_time,
            resolution_claim: None,
          }
        expect
          parsed_settlement_output_datum.data == expected_produced_node_data

        // Done.
        True
      }
      RemoveSettlementNode {
        key_to_remove,
        removed_node_input_index,
        anchor_node_input_index,
      } -> {
        // 1. The transaction must Remove a node from the `settlement_queue`.
        //    Let `removed_settlement_node` be that node.
        expect
          unordered.remove(
            Key(key_to_remove),
            removed_node_input_index,
            anchor_node_input_index,
            node_in_outputs,
            node_outputs,
            node_mint,
          )
        //
        expect Some(Output { datum: InlineDatum(removed_node_datum_data), .. }) =
          node_in_outputs |> list.at(removed_node_input_index)

        // 2. The `resolution_claim` of `removed_settlement_node` must not be empty.
        expect parsed_removed_node_datum: Datum = removed_node_datum_data
        expect NodeData {
          resolution_claim: Some(ResolutionClaim {
            resolution_time: unwrapped_time_of_claimed_as_resolved,
            operator: operator_vkey,
          }),
          ..
        }: NodeData = parsed_removed_node_datum.data

        // 3. The transaction must be signed by the `operator` of `resolution_claim`.
        expect operator_vkey |> utils.has_signed(extra_signatories)

        // 4. The transaction's time-validity lower bound must match or exceed the
        //    `resolution_time` of the `resolution_claim`.
        expect Finite(valid_from) = validity_range.lower_bound.bound_type
        expect unwrapped_time_of_claimed_as_resolved <= valid_from

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}
