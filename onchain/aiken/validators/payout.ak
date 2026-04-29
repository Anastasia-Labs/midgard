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

validator spend(payout_policy_id: PolicyId, reserve_addr: Address) {
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
        reserve_redeemer_index,
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
            // Double satisfaction prevented by validating the same NFT is present
            // in the output.
            True,
          )

        expect
          validate_asset_accumulation(
            reserve_addr,
            payout_policy_id,
            asset_to_add,
            payout_input.output.value,
            payout_output.value,
            l2_value,
            payout_nft_asset_name,
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
  reserve_addr: Address,
  payout_policy_id: PolicyId,
  asset_to_add: AssetToAdd,
  p0: Value,
  p1: Value,
  pt: ValuePairs,
  payout_asset_name: AssetName,
  accumulation_args_data: Data,
  inputs: List<Input>,
  outputs: List<Output>,
) -> Bool {
  // Separate Lovelace from the rest of assets.
  let (p0_lovelace, p0_assets) = p0 |> separate_ada_and_remove_auth_nft
  let (p1_lovelace, p1_assets) = p1 |> separate_ada_and_remove_auth_nft

  // NOTE that we are assuming it is already verified that the first entry in
  // the target `ValuePairs` is ADA.
  let Pair(_, pt_lovelace) = builtin.head_list(pt).2nd |> builtin.head_list
  let pt_assets = pt |> builtin.tail_list

  if p0_lovelace == pt_lovelace {
    // 1a. If target ADA had already been reached, this endpoint must have been
    //     invoked for addition of another asset.
    expect Asset {
      policy_id,
      asset_name,
      policy_id_index_in_target,
      asset_name_index_in_target,
      reserve_input_index,
      reserve_change_output_index,
    } = asset_to_add

    // 2a. Specified UTxO from reserve must be getting spent with a matching set
    //     of redeemer arguments.
    let (r0_lovelace, r0_assets) =
      get_reserve_input_lovelace_and_assets(
        reserve_addr,
        reserve_input_index,
        reserve_redeemer_index,
        accumulation_args_data,
        inputs,
      )

    // 3a. Change output must go to the Midgard reserve with no datum and ref.
    //     script attached.
    let (r1_lovelace, r1_assets) =
      get_reserve_change_output_lovelace_and_assets(
        reserve_addr,
        reserve_change_output_index,
        outputs,
      )

    // 4a. Since we are only adding another token, payout Lovelace must not
    //     change.
    expect p1_lovelace == p0_lovelace

    // 5a. ADA of the reserve UTxO must also be preserved.
    expect r1_lovelace == r0_lovelace

    // 6a. Transfer of the specified asset from reserve to the payout
    //     accumulator must be valid.
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
    // 1b. Output lovelace must not exceed the target.
    expect p1_lovelace <= pt_lovelace

    // 2b. Output lovelace must either grow or remain unchanged.
    expect p1_lovelace >= p0_lovelace

    when asset_to_add is {
      Lovelace { reserve_input_index, m_reserve_change_output_index } -> {
        // 3b. Specified UTxO from reserve must be getting spent with a matching
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
            // 4b. When redeemer specifies no change must go back to the
            //     reserve, input reserve must contain only ADA, and all its
            //     quantity must be absorbed by the payout accumulator.
            expect r0_assets == dict.empty
            expect r0_lovelace == p1_lovelace - p0_lovelace
            True
          }
          Some(reserve_change_output_index) -> {
            let (r1_lovelace, r1_assets) =
              get_reserve_change_output_lovelace_and_assets(
                reserve_addr,
                reserve_change_output_index,
                outputs,
              )

            // 4b'. The change in reserve lovelace must match the change in
            //      payout Lovelace.
            expect r0_lovelace - r1_lovelace == p1_lovelace - p0_lovelace

            // 5b'. Other assets of reserve must remain unchanged.
            let r0_assets_data: Data = r0_assets
            let r1_assets_data: Data = r1_assets
            expect r0_assets_data == r1_assets_data
            True
          }
        }
      }
      Asset {
        policy_id,
        asset_name,
        policy_id_index_in_target,
        asset_name_index_in_target,
        reserve_input_index,
        reserve_change_output_index,
      } -> {
        // 3c. Specified UTxO from reserve must be getting spent with a matching
        //     set of redeemer arguments.
        let (r0_lovelace, r0_assets) =
          get_reserve_input_lovelace_and_assets(
            reserve_addr,
            reserve_input_index,
            reserve_redeemer_index,
            accumulation_args_data,
            inputs,
          )

        // 4c. A change output is expected to be going back to the reserve.
        let (r1_lovelace, r1_assets) =
          get_reserve_change_output_lovelace_and_assets(
            reserve_addr,
            reserve_change_output_index,
            outputs,
          )

        // 5c. Transfer of the specified asset from reserve to the payout
        //     accumulator must be valid.
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
/// This function assumes all assets except `pt` are stripped of their ADA.
/// Therefore no validations are performed to prevent `policy_id_to_add` from
/// being the same as ADA.
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
