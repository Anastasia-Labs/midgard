use aiken/collection/dict
use aiken/collection/dict/strategy as dict_strategy
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
  AddFunds, AddFundsRedeemer, AssetToAdd, BurnPayout, ConcludeWithdrawal, Datum,
  MintPayout, MintRedeemer, SpendRedeemer, separate_ada_and_remove_auth_nft,
}
use midgard/reserve
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

validator spend(payout_policy_id: PolicyId) {
  spend(
    m_datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    let Transaction { inputs, outputs, redeemer, .. } = tx

    expect Some(Datum { l2_value }) = m_datum

    when redeemer is {
      AddFunds {
        accumulation_args,
        payout_input_outref,
        payout_input_redeemer_index,
      } -> {
        let AccumulationArgs {
          payout_input_index,
          payout_output_index,
          asset_to_add,
          payout_nft_asset_name,
        } = accumulation_args

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
            // Double satisfaction prevented by validating the same NFT is
            // present in the output.
            True,
          )

        expect
          validate_asset_accumulation(
            payout_policy_id,
            payout_nft_asset_name,
            asset_to_add,
            payout_input.output.value,
            payout_output.value,
            l2_value,
            accumulation_args,
            inputs,
            outputs,
          )
      }
      ConcludeWithdrawal { .. } -> todo
    }
  }

  else(_) {
    fail
  }
}

fn validate_asset_accumulation(
  payout_policy_id: PolicyId,
  payout_asset_name: AssetName,
  asset_to_add: AssetToAdd,
  p0: Value,
  p1: Value,
  pt: ValuePairs,
  accumulation_args_data: Data,
  inputs: List<Input>,
  outputs: List<Output>,
) -> Bool {
  // 1. Separate Lovelace from the rest of assets. `p0` and `p1` must carry 1 of
  //    the given auth NFT.
  let (p0_lovelace, p0_assets) =
    p0 |> separate_ada_and_remove_auth_nft(payout_policy_id, payout_asset_name)
  let (p1_lovelace, p1_assets) =
    p1 |> separate_ada_and_remove_auth_nft(payout_policy_id, payout_asset_name)

  // NOTE that we are assuming it is already verified that the first entry in
  // the target `ValuePairs` is ADA. This validation is currently happening at
  // minting of the withdrawal order UTxO (TODO?).
  let Pair(_, pt_lovelace) = builtin.head_list(pt).2nd |> builtin.head_list
  let pt_assets = pt |> builtin.tail_list

  // 2. Output lovelace must not exceed the target.
  expect p1_lovelace <= pt_lovelace

  // 3. Output lovelace must either grow or remain unchanged.
  expect p1_lovelace >= p0_lovelace

  when asset_to_add is {
    Lovelace {
      reserve_addr,
      reserve_input_index,
      reserve_redeemer_index,
      m_reserve_change_output_index,
    } -> {
      // 4a. Target Lovelace must not have been reached.
      expect p0_lovelace < pt_lovelace

      // 5a. Specified UTxO from reserve must be getting spent with a matching
      //     set of redeemer arguments.
      let (r0_lovelace, r0_assets) =
        get_reserve_input_lovelace_and_assets(
          reserve_addr,
          reserve_input_index,
          reserve_redeemer_index,
          accumulation_args_data,
          inputs,
        )

      when m_reserve_change_output_index is {
        None -> {
          // 6a. When redeemer specifies no change must go back to the reserve,
          //     input reserve must contain only ADA, and all its quantity must
          //     be absorbed by the payout accumulator.
          expect r0_assets == dict.empty
          expect r0_lovelace == p1_lovelace - p0_lovelace

          // Done.
          True
        }
        Some(reserve_change_output_index) -> {
          let (r1_lovelace, r1_assets) =
            get_reserve_change_output_lovelace_and_assets(
              reserve_addr,
              reserve_change_output_index,
              outputs,
            )

          // 6a'. The change in reserve lovelace must match the change in
          //      payout Lovelace.
          expect r0_lovelace - r1_lovelace == p1_lovelace - p0_lovelace

          // 7a'. Other assets of reserve must remain unchanged.
          let r0_assets_data: Data = r0_assets
          let r1_assets_data: Data = r1_assets
          expect r0_assets_data == r1_assets_data

          // Done.
          True
        }
      }
    }
    CardanoAsset {
      policy_id,
      asset_name,
      policy_id_index_in_target,
      asset_name_index_in_target,
      reserve_addr,
      reserve_input_index,
      reserve_redeemer_index,
      reserve_change_output_index,
    } -> {
      // 4b. Specified UTxO from reserve must be getting spent with a matching
      //     set of redeemer arguments.
      let (r0_lovelace, r0_assets) =
        get_reserve_input_lovelace_and_assets(
          reserve_addr,
          reserve_input_index,
          reserve_redeemer_index,
          accumulation_args_data,
          inputs,
        )

      // 5b. A change output is expected to be going back to the reserve.
      let (r1_lovelace, r1_assets) =
        get_reserve_change_output_lovelace_and_assets(
          reserve_addr,
          reserve_change_output_index,
          outputs,
        )

      // 6b. The change in reserve lovelace must match the change in payout
      //     Lovelace.
      expect r0_lovelace - r1_lovelace == p1_lovelace - p0_lovelace

      // 7b. Transfer of the specified asset from reserve to the payout
      //     accumulator must be valid.
      expect
        validate_transfer_of_given_asset_with_change(
          p0_assets,
          r0_assets,
          p1_assets,
          r1_assets,
          pt_assets,
          policy_id_index_in_target,
          asset_name_index_in_target,
        )

      // Done.
      True
    }
    MidgardAsset { .. } -> todo
  }
}

/// TODO: Here the assumption is that resreve UTxOs do not carry any beacon
///       tokens.
fn get_reserve_input_lovelace_and_assets(
  reserve_addr: Address,
  reserve_input_index: Int,
  reserve_redeemer_index: Int,
  accumulation_args_data: Data,
  inputs: List<Input>,
) -> (Lovelace, Dict<PolicyId, Dict<AssetName, Int>>) {
  expect Some(Input {
    output_reference: reserve_input_outref,
    output: Output {
      address: reserve_input_address,
      value: reserve_input_value,
      ..
    },
  }) = inputs |> list.at(reserve_input_index)
  expect
    reserve_input_address.payment_credential == reserve_addr.payment_credential
  let reserve_spend_redeemer_data =
    redeemers
      |> utils.get_redeemer_at(
          Spend(reserve_input_outref),
          reserve_redeemer_index,
        )
  expect accumulation_args_data == reserve_spend_redeemer_data
  reserve_input_value |> utils.get_lovelace_and_assets
}

/// p0: Non-ADA assets of input payout (without auth NFT)
/// r0: Non-ADA assets of input reserve
/// p1: Non-ADA assets of output payout (without auth NFT)
/// r1: Non-ADA assets of output reserve
/// pt: Target non-ADA assets
///
/// This function assumes all assets are stripped of their ADA. Since it is
/// expected for the target value to have ADA at its head, the given policy ID
/// index cannot be 0 (as it gets decremented to compensate the removal of ADA).
fn validate_transfer_of_given_asset_with_change(
  p0_assets: Dict<PolicyId, Dict<AssetName, Int>>,
  r0_assets: Dict<PolicyId, Dict<AssetName, Int>>,
  p1_assets: Dict<PolicyId, Dict<AssetName, Int>>,
  r1_assets: Dict<PolicyId, Dict<AssetName, Int>>,
  pt_assets: ValuePairs,
  policy_id_index_in_target: Int,
  asset_name_index_in_target: Int,
) -> Bool {
  // 1. Provided indices for extracting the subject asset must yield values.
  //    We are decrementing the policy ID's index since the assumption is that
  //    Lovelace at head was removed.
  let Pair(policy_id, target_asset_pairs) =
    pt_assets |> list.expect_at(policy_id_index_in_target - 1)
  let Pair(asset_name, pt_qty) =
    target_asset_pairs |> list.expect_at(asset_name_index_in_target)

  let p0_qty = p0_assets |> get_token_qty(policy_id, asset_name)
  let p1_qty = p1_assets |> get_token_qty_strict(policy_id, asset_name)

  let (p0_target_qty, p0_names_without_target, p0_policies_without_target) =
    p0_assets |> pop_target_asset(policy_id, asset_name)
  let (p1_target_qty, p1_names_without_target, p1_policies_without_target) =
    p1_assets |> pop_target_asset_strict(policy_id, asset_name)

  let (r0_target_qty, r0_names_without_target, r0_policies_without_target) =
    r0_assets |> pop_target_asset_strict(policy_id, asset_name)
  let (r1_target_qty, r1_names_without_target, r1_policies_without_target) =
    r1_assets |> pop_target_asset(policy_id, asset_name)

  // 2. Token's quantity must not exceed the target.
  expect p1_target_qty <= pt_target_qty

  // 3. Token's quantity must grow.
  expect p1_target_qty > p0_target_qty

  // 4. The change in reserve's token quantity must match the change in
  //    payout's.
  expect r0_target_qty - r1_target_qty == p1_target_qty - p0_target_qty

  // Done.
  True
}

/// This is essentially the same as `dict.expect_pop`, but goes one level
/// deeper to correspond to `Value` dicts. Therefore returns 3 values:
/// 1. Quantity of the popped asset.
/// 2. Dict carrying the rest of the asset name - quantity pairs (as `Data`).
/// 3. Dict carrying the rest of the policy IDs and their asset name dicts (as
///    `Data`).
fn pop_target_asset_strict(
  r: Dict<PolicyId, Dict<AssetName, Int>>,
  policy_id: PolicyId,
  asset_name: AssetName,
) -> (
  Int,
  Data<Dict<AssetName, Int>>,
  Data<Dict<PolicyId, Dict<AssetName, Int>>>,
) {
  let (r_target_names_qty_dict, r_without_target_policy) =
    r_assets |> dict.expect_pop(policy_id)
  let (r_target_qty, r_without_target_asset) =
    r_without_target_policy |> dict.expect_pop(asset_name)
  (r_target_qty, r_without_target_asset, r_without_target_policy)
}

fn pop_target_asset(
  r: Dict<PolicyId, Dict<AssetName, Int>>,
  policy_id: PolicyId,
  asset_name: AssetName,
) -> (
  Int,
  Data<Dict<AssetName, Int>>,
  Data<Dict<PolicyId, Dict<AssetName, Int>>>,
) {
  let (m_r_target_names_qty_dict, r_without_target_policy) =
    r_assets |> dict.pop(policy_id)
  when m_r_target_names_qty_dict is {
    None -> (0, Dict.empty, r)
    Some(r_target_names_qty_dict) -> {
      let (m_r_target_qty, r_without_target_asset) =
        r_without_target_policy |> dict.pop(asset_name)
      when m_r_target_qty is {
        None -> (0, r_target_names_qty_dict, r_without_target_policy)
        Some(r_target_qty) ->
          (r_target_qty, r_without_target_asset, r_without_target_policy)
      }
    }
  }
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

fn get_reserve_change_output_lovelace_and_assets(
  reserve_addr: Address,
  reserve_change_output_index: Int,
  outputs: List<Output>,
) -> (Lovelace, Dict<PolicyId, Dict<AssetName, Int>>) {
  // 1. Change output must go to the Midgard reserve with no datum and reference
  //    script attached.
  expect Some(Output {
    address: reserve_output_address,
    value: r1,
    datum: NoDatum,
    reference_script: None,
  }) = outputs |> list.at(reserve_change_output_index)
  expect reserve_output_address == reserve_addr

  r1 |> get_lovelace_and_assets
}

/// Re-implementation of `assets.difference` for `Dict`s. Behavior is
/// `v0 - v1`.
fn difference_between_value_dicts(
  v0: Dict<PolicyId, Dict<AssetName, Int>>,
  v1: Dict<PolicyId, Dict<AssetName, Int>>,
) -> Value {
  Value(
    dict.difference_with(
      v0,
      v1,
      fn(_, a0, a1, keep, discard) {
        let result = difference_between_asset_name_dicts(a0, a1)
        if dict.empty == result {
          discard()
        } else {
          keep(result)
        }
      },
      fn(_k, right, keep, _discard) {
        keep(dict.map(right, fn(_, qty) { -qty }))
      },
    ),
  )
}

/// Re-implementation of `assets.difference` for asset-name quantity dicts.
/// Behavior is `a0 - a1`.
fn difference_between_asset_name_dicts(
  a0: Dict<AssetName, Int>,
  a1: Dict<AssetName, Int>,
) -> Dict<AssetName, Int> {
  dict.difference_with(
    a0,
    a1,
    dict_strategy.difference_if_non_zero(),
    dict_strategy.negate(),
  )
}
