use aiken/collection/dict
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Address}
use cardano/assets.{AssetName, PolicyId}
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
        expect Pair(_, 1) =
          utils.get_singleton_asset_with_policy(mint, own_policy)

        // Done.
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
        expect
          Pair(payout_asset_name, -1) == utils.get_singleton_asset_with_policy(
            mint,
            own_policy,
          )

        // 2. The payout UTxO must be spent with the ConcludeWithdrawal
        //    redeemer, preventing burns outside of the conclusion endpoint.
        let
          payout_input,
          payout_input_nft_asset_name,
        <-
          utils.get_authentic_input_and_asset_name_with_policy_at(
            inputs,
            own_policy,
            payout_input_index,
          )

        // 3. Validate correctness of the asset name name in redeemer.
        expect payout_asset_name == payout_input_nft_asset_name

        expect ConcludeWithdrawal { .. } =
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
        m_reserve_change_output_index,
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

        when m_reserve_change_output_index is {
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
        let hub.Datum { settlement: settlement_policy_id, .. } =
          hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

        let
          payout_input,
          output,
        <-
          singular_utxo_indexer.one_to_one(
            input_index,
            output_index,
            own_out_ref,
            inputs,
            outputs,
            // Double satisfaction prevented via the burn endpoint as it allows
            // only one token of its policy to be burnt.
            True,
          )

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

validator add_funds(payout_policy_id: PolicyId, reserve_addr: Address) {
  withdraw(
    redeemer: AddFundsRedeemer,
    own_credential: Credential,
    tx: Transaction,
  ) {
    let Transaction { inputs, outputs, redeemer, .. } = tx

    expect Script(own_script_hash) = own_credential

    let AddFundsRedeemer {
      payout_input_index,
      payout_output_index,
      reserve_input_index,
      m_reserve_change_output_index,
      payout_input_outref,
      payout_input_redeemer_index,
    } = redeemer

    let
      payout_input,
      payout_output,
    <-
      singular_utxo_indexer.one_to_one(
        payout_input_index,
        payout_output_index,
        payout_input_outref,
        inputs,
        outputs,
        // Double satisfaction prevented by TODO
        True,
      )

    let
      payout_asset_name,
      payout_input_datum_data,
    <-
      authenticate_input_output_and_get_info(
        payout_input,
        payout_output,
        payout_policy_id,
      )

    expect AddFunds {
      payout_input_index: p0,
      payout_output_index: p1,
      reserve_input_index: r0,
      m_reserve_change_output_index: r1,
      ..
    } =
      redeemers
        |> utils.get_redeemer_at(
            Spend(payout_input_outref),
            payout_input_redeemer_index,
          )

    expect and {
        p0 == payout_input_index,
        p1 == payout_output_index,
        r0 == reserve_input_index,
        r1 == m_reserve_change_output_index,
      }

    expect (Spend(payout_input_outref_in_redeemer), payout_spend_redeemer_data) =
      redeemers |> list.at(payout_input_redeemer_index)

    expect payout_input_outref_in_redeemer == payout_input_outref

    let
      payout_input,
      payout_cont_output,
    <-
      singular_utxo_indexer.one_to_one(
        payout_input_index,
        payout_output_index,
        payout_input_outref,
        inputs,
        outputs,
        // Double satisfaction prevented by TODO
        True,
      )

    todo
  }

  else(_) {
    fail
  }
}

fn authenticate_input_output_and_get_info(
  input: Input,
  output: Output,
  authentication_policy_id: PolicyId,
  return: fn(AssetName, Data) -> Bool,
) -> Bool {
  let Input {
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

  expect (input_nft_symbol, input_nft_asset_name, 1) as input_nft_triplet =
    get_single_asset_from_value_apart_from_ada(input_value)

  expect input_nft_symbol == authentication_policy_id

  let output_nft_triplet =
    get_single_asset_from_value_apart_from_ada(output_value)

  expect input_nft_triplet == output_nft_triplet

  return(input_nft_asset_name, output_datum_data)
}

pub type AssetToAdd {
  Lovelace { m_reserve_change_output_index: Option<Int> }
  Asset {
    policy_id: PolicyId,
    asset_name: AssetName,
    policy_id_index_in_target: Int,
    asset_name_index_in_target: Int,
    reserve_change_output_index: Int,
  }
}

pub fn validate_value_accumulation_with_change(
  asset_to_add: AssetToAdd,
  p0: Value,
  r0: Value,
  p1: Value,
  pt: ValuePairs,
  payout_policy_id: PolicyId,
  payout_asset_name: AssetName,
  outputs: List<Output>,
) -> Bool {
  // Remove auth NFT.
  let p0 = p0 |> assets.add(payout_policy_id, payout_asset_name, -1)
  let p1 = p1 |> assets.add(payout_policy_id, payout_asset_name, -1)

  // Separate Lovelace from the rest of assets.
  let (p0_lovelace, p0_assets) = p0 |> get_lovelace_and_assets
  let (r0_lovelace, r0_assets) = r0 |> get_lovelace_and_assets
  let (p1_lovelace, p1_assets) = p1 |> get_lovelace_and_assets

  // NOTE that we are assuming it is already verified that the first entry in
  // the target `ValuePairs` is ADA.
  let Pair(_, pt_lovelace) = builtin.head_list(pt).2nd |> builtin.head_list
  let pt_assets = pt |> builtin.tail_list

  if p0_lovelace == pt_lovelace {
    // If target ADA had already been reached, this endpoint must have been
    // invoked for addition of another asset.
    expect Asset {
      policy_id,
      asset_name,
      policy_id_index_in_target,
      asset_name_index_in_target,
      reserve_change_output_index,
    } = asset_to_add

    expect Some(Output {
      address: reserve_output_address,
      value: r1,
      datum: NoDatum,
      reference_script: None,
    }) = outputs |> list.at(reserve_change_output_index)

    let (r1_lovelace, r1_assets) = r1 |> get_lovelace_and_assets

    // Payout lovelace must not change
    expect p1_lovelace == p0_lovelace

    // Reserve lovelace must not change
    expect r1_lovelace == r0_lovelace

    // Verify accumulation of the given asset
    validate_transfer_of_given_asset_with_change(
      p0_assets,
      r0_assets,
      p1_assets,
      r1_assets,
      pt_assets,
      policy_id,
      asset_name,
      policy_id_index_in_target,
      asset_name_index_in_target,
    )
  } else {
    // Output lovelace must not exceed the target
    expect p1_lovelace <= pt_lovelace

    // Output lovelace must either grow or remain unchanged
    expect p1_lovelace >= p0_lovelace

    when asset_to_add is {
      Lovelace { m_reserve_change_output_index } ->
        when m_reserve_change_output_index is {
          None -> {
            // Input reserve must contain only ADA, and all its quantity must be
            // absorbed by the payout accumulator.
            expect r0_assets == dict.empty
            expect r0_lovelace == p1_lovelace - p0_lovelace
            True
          }
          Some(reserve_change_output_index) -> {
            expect Some(Output {
              address: reserve_output_address,
              value: r1,
              datum: NoDatum,
              reference_script: None,
            }) = outputs |> list.at(reserve_change_output_index)

            let (r1_lovelace, r1_assets) = r1 |> get_lovelace_and_assets

            // The change in reserve lovelace must match the change in payout
            // lovelace.
            expect r0_lovelace - r1_lovelace == p1_lovelace - p0_lovelace

            // Other assets of reserve must remain unchanged.
            let r0_assets_data: Data = r0_assets
            let r1_assets_data: Data = r1_assets
            expect r0_assets_data == r1_assets_data
            True
          }
        }
      Asset {
        policy_id,
        asset_name,
        policy_id_index_in_target,
        asset_name_index_in_target,
        reserve_change_output_index,
      } -> {
        expect Some(Output {
          address: reserve_output_address,
          value: r1,
          datum: NoDatum,
          reference_script: None,
        }) = outputs |> list.at(reserve_change_output_index)

        let (r1_lovelace, r1_assets) = r1 |> get_lovelace_and_assets

        // Verify accumulation of the given asset.
        validate_transfer_of_given_asset_with_change(
          p0_assets,
          r0_assets,
          p1_assets,
          r1_assets,
          pt_assets,
          policy_id,
          asset_name,
          policy_id_index_in_target,
          asset_name_index_in_target,
        )
      }
    }
  }
}

/// p0: Assets of input payout
/// r0: Assets of input reserve
/// p1: Assets of output payout
/// r1: Assets of output reserve
/// pt: Target assets
fn validate_transfer_of_given_asset_with_change(
  p0_assets: Dict<PolicyId, Dict<AssetName, Int>>,
  r0_assets: Dict<PolicyId, Dict<AssetName, Int>>,
  p1_assets: Dict<PolicyId, Dict<AssetName, Int>>,
  r1_assets: Dict<PolicyId, Dict<AssetName, Int>>,
  pt_assets: ValuePairs,
  policy_id_to_add: PolicyId,
  asset_name_to_add: AssetName,
  policy_id_index_in_target: Int,
  asset_name_index_in_target: Int,
) -> Bool {
  // 1. Provided indices for extracting the subject asset must be valid.
  let Pair(target_policy_id, target_asset_pairs) =
    pt_assets |> list.expect_at(policy_id_index_in_target)
  expect target_policy_id == policy_id_to_add
  let Pair(target_asset_name, pt_qty) =
    target_asset_pairs |> list.expect_at(asset_name_index_in_target)
  expect target_asset_name == asset_name_to_add

  let p0_qty = p0_assets |> get_token_qty(policy_id_to_add, asset_name_to_add)
  let r0_qty =
    r0_assets |> get_token_qty_strict(policy_id_to_add, asset_name_to_add)
  let p1_qty =
    p1_assets |> get_token_qty_strict(policy_id_to_add, asset_name_to_add)
  let r1_qty = r1_assets |> get_token_qty(policy_id_to_add, asset_name_to_add)

  // 2. Token's quantity must not exceed the target.
  expect p1_qty <= pt_qty

  // 3. Token's quantity must grow.
  expect p1_qty > p0_qty

  // 4. The change in reserve's token quantity must match the change in
  //    payout's.
  expect r0_qty - r1_qty == p1_qty - p0_qty

  // Done.
  True
}

fn get_lovelace_and_assets(
  v: Value,
) -> (Lovelace, Dict<PolicyId, Dict<AssetName, Int>>) {
  let (v_lovelace, v_assets) =
    v |> assets.to_dict |> dict.expect_pop(ada_policy_id)
  (builtin.head_list(dict.to_pairs(v_lovelace)).2nd, v_assets)
}

fn get_token_qty_strict(
  assets: Dict<PolicyId, Dict<AssetName, Int>>,
  policy_id: PolicyId,
  asset_name: AssetName,
) -> Int {
  assets |> dict.expect_get(policy_id) |> dict.expect_get(asset_name)
}

fn get_token_qty(
  assets: Dict<PolicyId, Dict<AssetName, Int>>,
  policy_id: PolicyId,
  asset_name: AssetName,
) -> Int {
  let m_tokens = assets |> dict.get(policy_id)
  when m_tokens is {
    None -> 0
    Some(tokens) -> {
      let m_qty = tokens |> dict.get(asset_name)
      when m_qty is {
        None -> 0
        Some(qty) -> qty
      }
    }
  }
}
