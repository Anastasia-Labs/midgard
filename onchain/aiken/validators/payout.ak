use aiken/collection/dict
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{
  Input, Mint, NoDatum, Output, OutputReference, Spend, Transaction,
}
use midgard/common/utils
use midgard/hub_oracle as hub
use midgard/ledger_state
use midgard/payout.{
  AddFunds, BurnPayout, ConcludeWithdrawal, Datum, MintPayout, MintRedeemer,
  SpendRedeemer,
}
use midgard/settlement
use midgard/user_events
use midgard/user_events/withdrawal

validator mint(withdrawal_addr: Address) {
  mint(redeemer: MintRedeemer, own_policy: PolicyId, tx: Transaction) {
    let Transaction { inputs, redeemers, mint, .. } = tx

    when redeemer is {
      // Minting: must be coupled with InitializePayout in the withdrawal
      // spending validator, ensuring a payout NFT is only ever created for a
      // confirmed, valid withdrawal.
      MintPayout { withdrawal_input_index, withdrawal_spend_redeemer_index } -> {
        let Transaction { inputs, outputs, redeemers, .. } = tx

        // 1. Its spending redeemer must be InitializePayout. The withdrawal
        //    spending validator already validates all other minting conditions
        //    (asset name, output value, destination, etc.).
        expect withdrawal.SpendRedeemer {
          purpose: withdrawal.InitializePayout,
          ..
        } =
          utils.get_spending_redeemer_data_at(
            script_address: withdrawal_addr,
            input_index: withdrawal_input_index,
            redeemer_index: withdrawal_spend_redeemer_index,
            inputs: inputs,
            redeemers: redeemers,
          )

        // 2. Exactly one payout NFT minted with quantity 1.
        //    The withdrawal validator ensures the asset name matches the
        //    burnt withdrawal NFT.
        expect [Pair(_, 1)] = assets.tokens(mint, own_policy) |> dict.to_pairs

        True
      }

      // Burning: must be coupled with ConcludeWithdrawal in the payout
      // spending validator, ensuring the NFT can only be destroyed when
      // the accumulator is fully funded and funds are released to L1.
      BurnPayout {
        payout_asset_name,
        payout_input_index,
        payout_spend_redeemer_index,
      } -> {
        // 1. Exactly one payout NFT burned.
        expect assets.quantity_of(mint, own_policy, payout_asset_name) == -1

        // 2. The payout UTxO must be spent with the ConcludeWithdrawal
        //    redeemer, preventing burns outside of the conclusion endpoint.
        let
          payout_input,
          _,
        <-
          utils.get_authentic_input_and_asset_name_with_policy_at(
            inputs,
            own_policy,
            payout_input_index,
          )

        expect ConcludeWithdrawal { .. }: SpendRedeemer =
          utils.get_redeemer_at(
            redeemers,
            Spend(payout_input.output_reference),
            payout_spend_redeemer_index,
          )

        True
      }
    }
  }

  else(_) {
    fail
  }
}

validator spend(payout_policy_id: PolicyId, reserve_addr: Address) {
  spend(
    m_datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    let Transaction { inputs, outputs, reference_inputs, redeemers, .. } = tx

    expect Some(Datum { l2_value }) = m_datum

    when redeemer is {
      // ── AddFunds ───────────────────────────────────────────────────────────
      // Spends exactly one reserve UTxO to top up the payout accumulator.
      // Partial accumulation is allowed (combined < target is fine).
      // Any excess beyond what was deposited must return to the reserve.
      AddFunds {
        payout_input_index,
        payout_output_index,
        reserve_input_index,
        reserve_change_output_index,
        hub_ref_input_index,
      } -> {
        // 1. Authenticate payout input and bind its asset name.
        let
          payout_input,
          payout_asset_name,
        <-
          utils.get_authentic_input_and_asset_name_with_policy_at(
            inputs,
            payout_policy_id,
            payout_input_index,
          )

        // This spending validator must be executing for this UTxO.
        expect payout_input.output_reference == own_out_ref

        let Input {
          output: Output {
            address: payout_in_address,
            value: payout_in_value,
            ..
          },
          ..
        } = payout_input

        // 2. Reserve input must come from reserve_addr. Enforce exactly one
        //    reserve input per transaction to prevent draining multiple UTxOs.
        expect [Input { output: Output { value: reserve_in_value, .. }, .. }] =
          inputs |> list.filter(fn(i) { i.output.address == reserve_addr })

        // 3. Payout output: same address (prevents theft) and same datum, NFT
        //    preserved, no reference script attached.
        expect Some(Output {
          address: payout_out_address,
          value: payout_out_value,
          datum: payout_out_datum,
          reference_script: None,
        }) = outputs |> list.at(payout_output_index)

        expect payout_out_address == payout_in_address
        expect payout_out_datum == m_datum
        expect
          assets.quantity_of(
            payout_out_value,
            payout_policy_id,
            payout_asset_name,
          ) == 1

        // 4. Monotonic accumulation: payout output must have at least as much
        //    lovelace as the input (never loses value during accumulation).
        let payout_in_lovelace = assets.lovelace_of(payout_in_value)
        let payout_out_lovelace = assets.lovelace_of(payout_out_value)
        let reserve_in_lovelace = assets.lovelace_of(reserve_in_value)

        expect payout_out_lovelace >= payout_in_lovelace

        // 5. If the payout output received less than the full combined value,
        //    the excess must be returned to reserve_addr — not kept by the
        //    operator. excess = (payout_in + reserve_in) - payout_out.
        let excess_lovelace =
          payout_in_lovelace + reserve_in_lovelace - payout_out_lovelace

        // 6. TODO: Currently incorrect, but this check prevents the accumulator
        //          to grow larger than the target.
        expect payout_out_lovelace <= assets.lovelace_of(l2_value)

        when reserve_change_output_index is {
          None -> {
            // No change claimed: payout absorbed all the combined lovelace.
            expect excess_lovelace == 0
            True
          }
          Some(change_idx) -> {
            expect Some(Output {
              address: change_address,
              value: change_value,
              ..
            }) = outputs |> list.at(change_idx)

            and {
              // Change must go back to the reserve, not to the operator.
              change_address == reserve_addr,
              // Change must cover the full excess.
              assets.lovelace_of(change_value) >= excess_lovelace,
            }
          }
        }
      }

      // ── ConcludeWithdrawal ────────────────────────────────────────────────
      // Burns the payout NFT and releases the accumulated funds to the L1
      // recipient. Payout parameters are recovered trustlessly from the
      // settlement tree — no datum needed on the payout UTxO.
      ConcludeWithdrawal {
        payout_input_index,
        l1_output_index,
        hub_ref_input_index,
        settlement_ref_input_index,
        burn_redeemer_index,
        withdrawal_out_ref,
        withdrawal_event_info,
        membership_proof,
        inclusion_proof_script_withdraw_redeemer_index,
      } -> {
        let hub.Datum {
          payout: payout_policy_id,
          settlement: settlement_policy_id,
          ..
        } = hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

        // 1. Authenticate payout input and get its asset name.
        let
          payout_input,
          payout_asset_name,
        <-
          utils.get_authentic_input_and_asset_name_with_policy_at(
            inputs,
            payout_policy_id,
            payout_input_index,
          )

        expect payout_input.output_reference == own_out_ref

        let Input { output: Output { value: payout_in_value, .. }, .. } =
          payout_input

        // 2. Link this payout to its withdrawal: the payout NFT asset name
        //    must equal blake2b_256(cbor(withdrawal_out_ref)). This proves
        //    which withdrawal this accumulator belongs to without needing the
        //    (already-spent) withdrawal UTxO as a reference.
        expect
          user_events.out_ref_to_nonce(withdrawal_out_ref) == payout_asset_name

        // 3. Recover withdrawals_root from the settlement reference UTxO.
        let settlement.Datum { withdrawals_root, .. } =
          settlement.get_datum(
            reference_inputs,
            settlement_policy_id,
            settlement_ref_input_index,
          )

        // 4. Validate the withdrawal event info against the settlement tree.
        //    The Merkle proof confirms that (withdrawal_out_ref, withdrawal_event_info)
        //    is a member of the withdrawals tree. Cannot be forged.
        let withdrawal_event_key: Data = withdrawal_out_ref

        expect
          utils.plutarch_phas(
            withdrawals_root,
            withdrawal_event_key,
            withdrawal_event_info,
            membership_proof,
            redeemers,
            inclusion_proof_script_withdraw_redeemer_index,
          )

        // 5. Extract payout parameters from the verified event info.
        //    Also verify the withdrawal was marked valid — only valid
        //    withdrawals can have had their payout NFT minted.
        expect ledger_state.WithdrawalInfo {
          body: ledger_state.WithdrawalBody {
            l2_value,
            l1_address,
            l1_datum,
            ..
          },
          validity: ledger_state.WithdrawalIsValid,
          ..
        }: ledger_state.WithdrawalInfo = withdrawal_event_info

        // 6. Fully funded check: the payout accumulator must hold at least
        //    l2_value for each token (ADA and any other assets).
        expect
          utils.compare_authentic_value_with_target(
            payout_in_value,
            payout_policy_id,
            payout_asset_name,
            l2_value,
            fn(actual, target) { actual >= target },
          )

        // 7. L1 output: must go to l1_address with l1_datum and carry exactly
        //    l2_value. The payout NFT is burned and must NOT appear here.
        expect Some(Output {
          address: l1_out_address,
          value: l1_out_value,
          datum: l1_out_datum,
          reference_script: None,
        }) = outputs |> list.at(l1_output_index)

        expect l1_out_address == l1_address
        expect l1_out_datum == l1_datum

        // l1 output must carry exactly l2_value — no extra tokens, no NFT.
        expect
          assets.match(
            l1_out_value,
            l2_value,
            fn(_, lovelace) { lovelace == 0 },
          )

        // 8. Payout NFT must be burned via the BurnPayout mint redeemer,
        //    which itself is coupled back to this ConcludeWithdrawal redeemer.
        expect BurnPayout { payout_asset_name: burn_asset_name, .. }: MintRedeemer =
          utils.get_redeemer_at(
            redeemers,
            Mint(payout_policy_id),
            burn_redeemer_index,
          )
        expect burn_asset_name == payout_asset_name

        True
      }
    }
  }

  else(_) {
    fail
  }
}
