use aiken/collection/list
use aiken/crypto.{ScriptHash}
use aiken_design_patterns/singular_utxo_indexer
use cardano/assets.{PolicyId}
use cardano/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, Spend, Transaction,
}
use midgard/common/utils
use midgard/hub_oracle as hub
use midgard/ledger_state
use midgard/operator_directory/active_operators
use midgard/operator_directory/retired_operators
use midgard/scheduler
use midgard/settlement.{
  AttachResolutionClaim, Datum, DisproveResolutionClaim, MintRedeemer, Remove,
  ResolutionClaim, Resolve, Spawn, SpendRedeemer,
}
use midgard/state_queue

validator spend(hub_oracle: ScriptHash, settlement_policy_id: ScriptHash) {
  spend(
    m_datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    expect Some(datum) = m_datum

    let Datum {
      deposits_root,
      withdrawals_root,
      transactions_root,
      resolution_claim,
    } = datum

    when redeemer is {
      AttachResolutionClaim {
        settlement_input_index,
        settlement_output_index,
        hub_ref_input_index,
        active_operators_node_input_index,
        active_operators_redeemer_index,
        operator,
        scheduler_ref_input_index,
      } -> {
        let Transaction {
          inputs,
          reference_inputs,
          outputs,
          extra_signatories,
          redeemers,
          ..
        } = tx

        // 1. The spent input must not have a resolution claim.
        expect resolution_claim == None

        // 2. The transaction must be signed by the resolution claim's operator.
        expect operator |> utils.has_signed(extra_signatories)

        // 3. Specified input must be authentic, and reproduced properly at the
        //    indicated output index.
        let
          input,
          output,
        <-
          singular_utxo_indexer.one_to_one(
            input_index: settlement_input_index,
            output_index: settlement_output_index,
            own_ref: own_out_ref,
            inputs: inputs,
            outputs: outputs,
            // Double satisfaction prevented by requiring the input NFT to be
            // reproduced at the output.
            double_satisfaction_prevented: True,
          )

        // 4. The transaction must include the Midgard hub oracle NFT in a
        //    reference input.
        // 5. hub_oracle.{active_operators, scheduler}
        let hub.Datum { active_operators_addr, scheduler, .. } =
          hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

        // 6. The transaction must include an input (`operator_node`), spent via
        //    the Update Bond Hold New Settlement redeemer, of an
        //    `active_operators` node with a key matching the resolution claim's
        //    operator.
        expect active_operators.UpdateBondHoldNewSettlement {
          new_bond_unlock_time,
          ..
        } =
          utils.get_spending_redeemer_data_at(
            script_address: active_operators_addr,
            input_index: active_operators_node_input_index,
            redeemer_index: active_operators_redeemer_index,
            inputs: inputs,
            redeemers: redeemers,
          )

        // 7. The `bond_unlock_time` of the `operator_node` must match the
        //    `resolution_time` of the resolution claim, which lies in the
        //    future by `maturity_duration` milliseconds.
        let cont_output_datum_data =
          authenticate_input_output_and_get_output_datum_data(
            input,
            output,
            settlement_policy_id,
          )
        let expected_output_datum_data: Data =
          Datum {
            ..datum,
            resolution_claim: Some(
              ResolutionClaim {
                resolution_time: new_bond_unlock_time,
                operator,
              },
            ),
          }
        expect cont_output_datum_data == expected_output_datum_data

        // 8. The transaction must include the `scheduler` utxo as a reference
        //    input, indicating that the current operator matches the resolution
        //    claim's operator.
        expect parsed_scheduler_state_datum: scheduler.Datum =
          utils.get_authentic_input_datum_with_nft_at(
            reference_inputs,
            scheduler,
            scheduler.asset_name,
            scheduler_ref_input_index,
          )
        expect operator == parsed_scheduler_state_datum.operator

        // Done. 
        True
      }
      DisproveResolutionClaim {
        settlement_input_index,
        settlement_output_index,
        hub_ref_input_index,
        operators_redeemer_index,
        operator,
        operator_is_active,
        unresolved_event_ref_input_index,
        unresolved_event_asset_name,
        event_type,
        membership_proof,
        inclusion_proof_script_withdraw_redeemer_index,
      } -> {
        let Transaction {
          inputs,
          reference_inputs,
          outputs,
          validity_range,
          redeemers,
          ..
        } = tx

        // 1. The spent input must have a resolution claim.
        expect Some(ResolutionClaim {
          resolution_time,
          operator: resolution_operator,
        }) = resolution_claim
        expect operator == resolution_operator

        // 2. Specified input must be authentic, and reproduced properly at the
        //    indicated output index.
        let
          input,
          output,
        <-
          singular_utxo_indexer.one_to_one(
            input_index: settlement_input_index,
            output_index: settlement_output_index,
            own_ref: own_out_ref,
            inputs: inputs,
            outputs: outputs,
            // Double satisfaction prevented by requiring the input NFT to be
            // reproduced at the output.
            double_satisfaction_prevented: True,
          )
        let cont_output_datum_data =
          authenticate_input_output_and_get_output_datum_data(
            input,
            output,
            settlement_policy_id,
          )

        // 3. The spent input must be reproduced without a resolution claim.
        let expected_datum_data: Data =
          Datum { ..datum, resolution_claim: None }
        expect cont_output_datum_data == expected_datum_data

        // 4. The transaction must include the Midgard hub oracle NFT in a
        //    reference input.
        // 5. hub_oracle.{active_operators, retired_operators, deposit, withdrawal, tx_order}
        let hub.Datum {
          active_operators,
          retired_operators,
          deposit: deposit_script_hash,
          withdrawal: withdrawal_script_hash,
          tx_order: tx_order_script_hash,
          ..
        } = hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

        // 6. The transaction must include either a deposit, a withdrawal order,
        //    or an L2 transaction order as a reference input. The validity of
        //    withdrawal or transaction order in the corresponding tree root can
        //    be different than the one coming from the reference input.
        //    Inclusion of the event in the tree is checked using the validity
        //    provided via the redeemer.
        expect
          settlement.valid_event_inclusion(
            event_type,
            deposit_script_hash,
            withdrawal_script_hash,
            tx_order_script_hash,
            deposits_root,
            withdrawals_root,
            transactions_root,
            membership_proof,
            unresolved_event_asset_name,
            unresolved_event_ref_input_index,
            reference_inputs,
            redeemers,
            inclusion_proof_script_withdraw_redeemer_index,
          )

        // 7. The transaction's time-validity inclusive upper bound must be
        //    earlier than the resolution claim's resolution time.
        let valid_to =
          utils.get_inclusive_upper_bound_of_interval(validity_range)
        expect valid_to < resolution_time

        // 8. Let `operator_is_active` be a redeemer argument indicating whether
        //    `operator` is active or retired.
        if operator_is_active {
          // 9a. The transaction must remove a node from the active operators
          //     list via the Remove Operator Bad Settlement redeemer.
          let active_operators_mint_redeemer_data =
            redeemers
              |> utils.get_redeemer_at(
                  Mint(active_operators),
                  operators_redeemer_index,
                )
          expect active_operators.RemoveOperatorBadSettlement {
            slashed_active_operator_key,
            ..
          }: active_operators.MintRedeemer = active_operators_mint_redeemer_data
          expect slashed_active_operator_key == operator

          // Done.
          True
        } else {
          // 9b. The transaction must remove a node from the retired operators
          //     list via the Remove Operator Bad Settlement redeemer.
          let retired_operators_mint_redeemer_data =
            redeemers
              |> utils.get_redeemer_at(
                  Mint(retired_operators),
                  operators_redeemer_index,
                )
          expect retired_operators.RemoveOperatorBadSettlement {
            slashed_retired_operator_key,
            ..
          }: retired_operators.MintRedeemer =
            retired_operators_mint_redeemer_data
          expect slashed_retired_operator_key == operator

          // Done.
          True
        }
      }
      Resolve { settlement_id } -> {
        // 1. The specified settlement NFT must be burnt.
        expect
          assets.quantity_of(tx.mint, settlement_policy_id, settlement_id) == -1

        // Done.
        True
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

    when redeemer is {
      Spawn {
        settlement_id,
        output_index,
        state_queue_merge_redeemer_index,
        hub_ref_input_index,
      } -> {
        // 1. The transaction must include the Midgard hub oracle NFT in a
        //    reference input.
        // 2. hub_oracle.{state_queue}
        let hub.Datum { state_queue, settlement_addr, .. } =
          hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

        // 3. The transaction must fold a `state_queue` node via the Merge To
        //    Confirmed State redeemer. Let `merged_block` be the block being
        //    merged to the confirmed state, and let `header_hash` be its
        //    header-hash key.
        //
        //    > [!NOTE]
        //    > We trust the redeemer arguments provided to state queue because
        //    > we expect it to check them.
        let state_queue_redeemer_data =
          redeemers
            |> utils.get_redeemer_at(
                Mint(state_queue),
                state_queue_merge_redeemer_index,
              )
        //
        expect state_queue.MergeToConfirmedState {
          header_node_key,
          header_node_input_index,
          m_settlement_redeemer_index,
          ..
        }: state_queue.MintRedeemer = state_queue_redeemer_data
        // We verify this redeemer argument so that other scripts can use it:
        expect header_node_key == settlement_id
        // Provision of redeemer index in state queue's redeemer shows that at
        // least one of the trees is non-empty. Without this check, this mint
        // endpoint would pass even if all trees were empty.
        //
        // Another solution would be checking the absence of settlement minting
        // by the state queue policy, which would be more expensive.
        expect Some(_) == m_settlement_redeemer_index
        //
        expect Some(merged_block) = list.at(inputs, header_node_input_index)
        expect Input {
          output: Output { datum: InlineDatum(block_datum_data), .. },
          ..
        } = merged_block
        //
        expect block_datum: state_queue.Datum = block_datum_data
        expect ledger_state.Header {
          deposits_root,
          withdrawals_root,
          transactions_root,
          ..
        } = block_datum.data

        // 4. The transaction must produce an authentic settlement utxo with an
        //    NFT which its asset name matches `header_hash`. Let
        //    `new_settlement` be the new settlement utxo.
        let settlement_output_datum_data =
          outputs
            |> utils.get_authentic_output_datum_at_address_with_nft_at(
                expected_address: settlement_addr,
                nft_policy_id: own_policy,
                nft_asset_name: settlement_id,
                output_index: output_index,
              )

        // 5. `merged_block` and `new_settlement` must match on all these
        //    fields: `deposits_root`, `withdrawals_root`, `transactions_root`.
        // 6. The `resolution_claim` of `new_settlement` must be empty.
        let expected_produced_settlement_datum: Data =
          Datum {
            deposits_root,
            withdrawals_root,
            transactions_root,
            resolution_claim: None,
          }
        expect
          settlement_output_datum_data == expected_produced_settlement_datum

        // 7. Only 1 of the new NFT must be minted.
        expect assets.quantity_of(mint, own_policy, settlement_id) == 1

        // Done.
        True
      }
      Remove { settlement_id, input_index, spend_redeemer_index } -> {
        // 1. The transaction must spend the specified settlement utxo. Let
        //    `removed_settlement` be that utxo.
        expect Input {
          output: Output { datum: InlineDatum(input_datum_data), .. },
          output_reference: input_outref,
        } =
          inputs
            |> utils.get_authentic_input_with_nft_at(
                own_policy,
                settlement_id,
                input_index,
              )

        let settlement_spending_redeemer_data =
          redeemers
            |> utils.get_redeemer_at(Spend(input_outref), spend_redeemer_index)

        // 2. The redeemer used for spending the `removed_settlement` must be
        //    `Resolve`. This also validates the specified NFT is getting burnt.
        expect Resolve { settlement_id: settlement_id_from_spend_redeemer }: SpendRedeemer =
          settlement_spending_redeemer_data

        // 3. The `settlement_id` included in the spending redeemer must match
        //    the ID provided in the burning redeemer.
        expect settlement_id == settlement_id_from_spend_redeemer

        // 4. The `resolution_claim` of `removed_settlement` must not be empty.
        expect Datum {
          resolution_claim: Some(ResolutionClaim {
            resolution_time: unwrapped_time_of_claimed_as_resolved,
            operator: operator_vkey,
          }),
          ..
        } = input_datum_data

        // 5. The transaction must be signed by the `operator` of
        //    `resolution_claim`.
        expect operator_vkey |> utils.has_signed(extra_signatories)

        // 6. The transaction's time-validity lower bound must match or exceed
        //    the `resolution_time` of the `resolution_claim`.
        //    TODO: Is ensuring inclusivity of significance here?
        let valid_from =
          utils.get_inclusive_lower_bound_of_interval(validity_range)
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

fn authenticate_input_output_and_get_output_datum_data(
  input: Input,
  output: Output,
  settlement_policy_id: PolicyId,
) -> Data {
  expect Input {
    output: Output { address: input_address, value: input_value, .. },
    ..
  } = input

  expect Output {
    address: output_address,
    value: output_value,
    datum: InlineDatum(output_datum_data),
    reference_script: None,
  } = output

  expect input_address == output_address

  expect (input_nft_symbol, _, 1) as input_nft_triplet =
    utils.get_single_asset_from_value_apart_from_ada(input_value)

  expect input_nft_symbol == settlement_policy_id

  let output_nft_triplet =
    utils.get_single_asset_from_value_apart_from_ada(output_value)

  expect input_nft_triplet == output_nft_triplet

  output_datum_data
}
