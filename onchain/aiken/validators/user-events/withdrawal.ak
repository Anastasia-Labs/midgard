use aiken/builtin
use aiken/crypto.{ScriptHash}
use aiken_design_patterns/linked_list/unordered
use aiken_design_patterns/singular_utxo_indexer
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, Transaction,
}
use env
use midgard/common/types.{ValuePairs}
use midgard/common/utils.{plutarch_phas}
use midgard/escrow
use midgard/hub_oracle as hub
use midgard/ledger_state
use midgard/settlement_queue
use midgard/state_queue
use midgard/user_events
use midgard/user_events/withdrawal.{
  AuthenticateWithdrawal, BurnWithdrawalNFT, Datum, MintRedeemer, Refund,
  SpendRedeemer, TransferToEscrow,
}

validator spend(hub_oracle: ScriptHash) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    let Transaction { reference_inputs, redeemers, .. } = tx

    let SpendRedeemer {
      input_index,
      output_index,
      hub_ref_input_index,
      settlement_ref_input_index,
      burn_redeemer_index,
      escrow_mint_redeemer_index,
      purpose,
    } = redeemer

    let hub.Datum {
      withdrawal: withdrawal_policy_id,
      settlement_queue: settlement_queue_policy_id,
      escrow: escrow_policy_id,
      escrow_addr,
      ..
    } = hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

    let parsed_settlement_node_datum =
      settlement_queue.get_datum(
        reference_inputs,
        settlement_queue_policy_id,
        settlement_ref_input_index,
      )

    expect settlement_queue.NodeData {
      withdrawals_root,
      start_time: settlement_start_time,
      end_time: settlement_end_time,
      ..
    } = parsed_settlement_node_datum.data

    expect Some(withdrawal_datum) = datum
    let user_events.Datum {
      event: withdrawal_event,
      inclusion_time,
      refund_address,
      refund_datum,
      ..
    } = withdrawal_datum

    // 1. Spending a user event UTxO must also be accompanied by the burning of
    //    its NFT (and consequently deregistration of its witness staking
    //    script).
    expect user_events.BurnEventNFT { nonce_asset_name: burn_asset_name, .. }: user_events.MintRedeemer =
      utils.get_redeemer_at(
        redeemers,
        Mint(withdrawal_policy_id),
        burn_redeemer_index,
      )

    singular_utxo_indexer.spend(
      fn(_in_ix, input, spend_purpose, _out_ix, output) {
        let Input { output: Output { value: own_value, .. }, .. } = input

        // 2. The produced UTxO must not have any scripts attached to it.
        expect Output {
          address: output_address,
          value: output_value,
          datum: output_datum,
          reference_script: None,
        } = output

        when spend_purpose is {
          TransferToEscrow { membership_proof } -> {
            // 3. Output value must be the same as the input value, without the
            //    withdrawal NFT, and with the escrow NFT added.
            expect
              output_value == (
                own_value
                  |> assets.add(withdrawal_policy_id, burn_asset_name, -1)
                  |> assets.add(escrow_policy_id, burn_asset_name, 1)
              )

            // 4. TODO: Anticipating what the escrow mint redeemer should look
            //          like.
            expect escrow.MintRedeemer { withdrawal_utxo_out_ref }: escrow.MintRedeemer =
              utils.get_redeemer_at(
                redeemers,
                Mint(escrow_policy_id),
                escrow_mint_redeemer_index,
              )
            expect withdrawal_utxo_out_ref == own_out_ref

            let Pair(withdrawal_event_id, withdrawal_event_info) =
              ledger_state.unsafe_event_to_key_value_pair(withdrawal_event)

            // 5. The referenced settlement node's withdrawal tree must contain
            //    the subject withdrawal event. No need to check for the
            //    inclusion time of the withdrawal.
            expect
              plutarch_phas(
                withdrawals_root,
                withdrawal_event_id,
                withdrawal_event_info,
                membership_proof,
                redeemers,
              )

            expect ledger_state.WithdrawalInfo {
              l2_value,
              l1_address,
              l1_datum,
              ..
            } = withdrawal_event_info

            let expected_output_datum: Data =
              escrow.Datum { l2_value, l1_address, l1_datum }

            and {
              // 6. Assert proper datum is attached to the escrow UTxO.
              output_datum == InlineDatum(expected_output_datum),
              // 7. Withdrawal must go to the escrow contract.
              output_address == escrow_addr,
            }
          }
          Refund { refund_approach } ->
            // 3. Numerous validations for the refund logic.
            user_events.validate_refund(
              own_value,
              withdrawal_policy_id,
              burn_asset_name,
              output_address,
              output_value,
              output_datum,
              refund_address,
              refund_datum,
              refund_approach,
              withdrawals_root,
              ledger_state.unsafe_event_to_id_data(withdrawal_event),
              settlement_queue_policy_id,
              parsed_settlement_node_datum.link,
              settlement_start_time,
              settlement_end_time,
              inclusion_time,
              reference_inputs,
              redeemers,
            )
        }
      },
      purpose,
      input_index,
      output_index,
      own_out_ref,
      tx,
    )
  }

  else(_) {
    fail
  }
}

validator mint(hub_oracle: ScriptHash) {
  mint(redeemer: MintRedeemer, own_policy: PolicyId, tx: Transaction) {
    when redeemer is {
      AuthenticateWithdrawal {
        nonce_input_index,
        event_output_index,
        hub_ref_input_index,
        witness_registration_redeemer_index,
        latest_block_ref_input_index,
        l2_output_data,
        membership_proof,
      } -> {
        let Transaction {
          inputs,
          outputs,
          reference_inputs,
          validity_range,
          mint,
          redeemers,
          ..
        } = tx

        // Unsafely destructuring `l2_output_data`, assuming it is structured as
        // an `Output`. Since further down we are validating the presence of
        // this UTxO in the latest ledger, we don't need to worry about the
        // correctness of its structure here.
        expect [l2_output_address_data,
          l2_output_value_data, ..] = builtin.unconstr_fields(l2_output_data)

        // Coerce the first data under `l2_output_data` into an `Address`.
        expect l2_output_address: Address = l2_output_address_data

        // 1. Subject UTxO must be sitting at the outbox contract on L2.
        expect l2_output_address == env.l2_outbox_address

        // Grab the hub oracle datum.
        let hub.Datum { state_queue: state_queue_policy, withdrawal_addr, .. } =
          hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

        // 2. Perform numerous validations and grant access to:
        //    - Serialized and hashed nonce based on the output reference of a
        //      specified input UTxO
        //    - The output value of the UTxO at the withdrawal address for the
        //      minting (not burning) endpoint
        //    - Withdrawal info
        let
          l1_id,
          output_withdrawal_order_value,
          withdrawal_info_data,
        <-
          user_events.authenticate_event(
            withdrawal_addr,
            nonce_input_index,
            event_output_index,
            witness_registration_redeemer_index,
            own_policy,
            inputs,
            outputs,
            validity_range,
            mint,
            redeemers,
          )

        expect ledger_state.WithdrawalInfo { l2_outref, l2_value, .. } =
          withdrawal_info_data

        // Grab the state queue datum.
        expect Input {
          output: Output { datum: InlineDatum(latest_block_datum_data), .. },
          ..
        } =
          utils.get_authentic_input_with_policy_at(
            reference_inputs,
            state_queue_policy,
            latest_block_ref_input_index,
          )

        // Coerce the second data under `l2_output_data` into a `ValuePairs`.
        // `Value` is not allowed as its an opaque type.
        expect l2_output_value: ValuePairs = l2_output_value_data

        // 3. The `l2_value` stated in `WithdrawalInfo` must be the same as the
        //    value from the `l2_output_data` coming from the redeemer.
        expect l2_output_value == l2_value

        expect parsed_header_datum: state_queue.Datum = latest_block_datum_data
        // 4. The provided block must be the latest in queue.
        expect parsed_header_datum.link == unordered.Empty
        expect ledger_state.Header { utxos_root, .. } = parsed_header_datum.data

        // 5. The latest block's ledger must contain the UTxO identified by
        //    `l2_outref` from `WithdrawalInfo`.
        expect
          utils.plutarch_phas(
            mpf_root: utxos_root,
            key: l2_outref,
            value: l2_output_data,
            membership_proof: membership_proof,
            redeemers: redeemers,
          )

        let output_withdrawal_order_nft_triplet =
          utils.get_single_asset_from_value_apart_from_ada(
            output_withdrawal_order_value,
          )

        // 6. The output value of the UTxO at the withdrawal address must
        //    contain only ADA and the authentication NFT.
        output_withdrawal_order_nft_triplet == (own_policy, l1_id, 1)
      }
      BurnWithdrawalNFT {
        nonce_asset_name,
        witness_unregistration_redeemer_index,
      } -> {
        let Transaction { mint, redeemers, .. } = tx
        user_events.validate_event_nft_burn(
          nonce_asset_name,
          witness_unregistration_redeemer_index,
          own_policy,
          mint,
          redeemers,
        )
      }
    }
  }

  else(_) {
    fail
  }
}
