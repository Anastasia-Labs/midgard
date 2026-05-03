use aiken/collection/dict
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{
  Input, Mint, NoDatum, Output, OutputReference, Spend, Transaction,
}
use midgard/common/utils
use midgard/hub_oracle as hub
use midgard/ledger_state
use midgard/payout
use midgard/settlement
use midgard/user_events
use midgard/user_events/withdrawal as withdrawal_lib

// ── Minting policy ──────────────────────────────────────────────────────────

validator mint(_hub_oracle: ScriptHash) {
  mint(redeemer: payout.MintRedeemer, own_policy: PolicyId, tx: Transaction) {
    let Transaction { inputs, redeemers, mint, .. } = tx

    when redeemer is {
      // Minting: must be coupled with InitializePayout in the withdrawal
      // spending validator, ensuring a payout NFT is only ever created for a
      // confirmed, valid withdrawal.
      payout.MintPayout {
        withdrawal_utxo_out_ref,
        withdrawal_spend_redeemer_index,
      } -> {
        // 1. The withdrawal UTxO must be consumed in this transaction.
        expect
          list.any(
            inputs,
            fn(i) { i.output_reference == withdrawal_utxo_out_ref },
          )

        // 2. Its spending redeemer must be InitializePayout. The withdrawal
        //    spending validator already validates all other minting conditions
        //    (asset name, output value, destination, etc.).
        expect withdrawal_lib.SpendRedeemer {
          purpose: withdrawal_lib.InitializePayout,
          ..
        }: withdrawal_lib.SpendRedeemer =
          utils.get_redeemer_at(
            redeemers,
            Spend(withdrawal_utxo_out_ref),
            withdrawal_spend_redeemer_index,
          )

        // 3. Exactly one payout NFT minted with quantity 1.
        //    The withdrawal validator ensures the asset name matches the
        //    burnt withdrawal NFT.
        expect [Pair(_, 1)] = assets.tokens(mint, own_policy) |> dict.to_pairs

        True
      }

      // Burning: must be coupled with ConcludeWithdrawal in the payout
      // spending validator, ensuring the NFT can only be destroyed when
      // the accumulator is fully funded and funds are released to L1.
      payout.BurnPayout {
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

        expect payout.ConcludeWithdrawal { .. }: payout.SpendRedeemer =
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

// ── Spending validator ───────────────────────────────────────────────────────

validator spend(hub_oracle: ScriptHash) {
  spend(
    // Payout UTxOs always hold NoDatum. datum must be None.
    datum: Option<Data>,
    redeemer: payout.SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    let Transaction { inputs, outputs, reference_inputs, redeemers, .. } = tx

    // Invariant: payout UTxOs must never carry a datum.
    // Enforced at creation (withdrawal.ak) and at continuation (AddFunds).
    expect datum == None

    when redeemer is {
      // ── AddFunds ───────────────────────────────────────────────────────────
      // Spends exactly one reserve UTxO to top up the payout accumulator.
      // Partial accumulation is allowed (combined < target is fine).
      // Any excess beyond what was deposited must return to the reserve.
      payout.AddFunds {
        payout_input_index,
        payout_output_index,
        reserve_input_index,
        reserve_change_output_index,
        hub_ref_input_index,
      } -> {
        let hub.Datum { payout: payout_policy_id, reserve_addr, .. } =
          hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

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
            datum: payout_in_datum,
            ..
          },
          ..
        } = payout_input

        // Invariant check on input.
        expect payout_in_datum == NoDatum

        // 2. Reserve input must come from reserve_addr. Enforce exactly one
        //    reserve input per transaction to prevent draining multiple UTxOs.
        expect Some(Input {
          output: Output {
            address: reserve_in_address,
            value: reserve_in_value,
            ..
          },
          ..
        }) = inputs |> list.at(reserve_input_index)

        expect reserve_in_address == reserve_addr

        let reserve_input_count =
          list.count(inputs, fn(i) { i.output.address == reserve_addr })
        expect reserve_input_count == 1

        // 3. Payout output: same address (prevents theft), NoDatum (prevents
        //    injection), NFT preserved, no reference script attached.
        expect Some(Output {
          address: payout_out_address,
          value: payout_out_value,
          datum: payout_out_datum,
          reference_script: None,
        }) = outputs |> list.at(payout_output_index)

        expect payout_out_address == payout_in_address
        expect payout_out_datum == NoDatum
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
      payout.ConcludeWithdrawal {
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
        expect payout.BurnPayout { payout_asset_name: burn_asset_name, .. }: payout.MintRedeemer =
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
