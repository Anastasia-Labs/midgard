use aiken/collection/dict
use aiken/collection/dict/strategy as dict_strategy
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use aiken/primitive/int
use cardano/address.{Address}
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{
  Input, Mint, NoDatum, Output, OutputReference, Spend, Transaction,
}
use midgard/common/utils
use midgard/hub_oracle as hub
use midgard/ledger_state
use midgard/payout.{
  AddCardanoAssets, AddFunds, AddLovelace, AddMidgardAssets, AssetsToAdd,
  BurnPayout, ConcludeWithdrawal, Datum, MintPayout, MintRedeemer, SpendRedeemer,
  separate_ada_and_remove_auth_nft,
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
        expect Input {
          output_reference: payout_input_outref,
          output: Output {
            address: payout_input_address,
            value: payout_input_value,
            datum: InlineDatum(payout_input_datum_data),
            ..
          },
        } = inputs |> list.expect_at(payout_input_index)
        expect
          payout_input_value
            |> assets.has_nft_strict(own_policy, payout_asset_name)

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
    expect Some(input_datum) = m_datum

    when redeemer is {
      AddFunds {
        accumulation_args,
        payout_input_outref,
        payout_input_redeemer_index,
      } -> {
        let Datum { l2_value, .. } = input_datum

        let Transaction { inputs, outputs, redeemer, .. } = tx

        let AccumulationArgs {
          payout_input_index,
          payout_output_index,
          assets_to_add,
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

        let Input {
          output: Output {
            address: payout_input_address,
            value: payout_output_value,
            ..
          },
          ..
        } = payout_input

        expect Output {
          address: payout_output_address,
          value: payout_output_value,
          datum: InlineDatum(output_datum_data),
          reference_script: None,
        } = payout_output

        // 1. Payout UTxO must be reproduced at the same address.
        expect payout_input_address == payout_output_address

        // 2. Datum must not change.
        let input_datum_data: Data = input_datum
        expect output_datum_data == input_datum_data

        // 3. Specified assets must be included in the singular reserve UTxO
        //    (which its address is also specified in the redeemer) and they
        //    must all go to the reproduced payout UTxO.
        expect
          validate_asset_accumulation(
            payout_policy_id,
            payout_nft_asset_name,
            assets_to_add,
            payout_input_value,
            payout_output_value,
            l2_value,
            accumulation_args,
            inputs,
            outputs,
          )

        // Done.
        True
      }
      ConcludeWithdrawal { input_index, burn_redeemer_index } -> {
        let Transaction { inputs, redeemers, .. } = tx

        // 1. The minting policy of the payout script must be invoked via the
        //    `BurnPayout` redeemer, with a matching payout input index. Let
        //    this index be `input_index`.
        expect BurnPayout { payout_input_index, .. } =
          redeemers
            |> utils.get_redeemer_at(
                Mint(payout_policy_id),
                burn_redeemer_index,
              )
        expect input_index == payout_input_index

        // 2. The input corresponding to `input_index` must have a matching
        //    output reference from the script info.
        let Input { output_reference, .. } =
          inputs |> list.expect_at(input_index)
        expect output_reference == own_out_ref

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}

fn validate_asset_accumulation(
  payout_policy_id: PolicyId,
  payout_asset_name: AssetName,
  assets_to_add: AssetsToAdd,
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

  // NOTE that we are assuming it is already verified that it's properly formed.
  // Meaning:
  // - The head is `Pair(ada_policy_id, [Pair(ada_asset_name, pt_lovelace)])`
  // - All entries have quantities greater than 0.
  // - Both inner and outer pairs are in ascending order lexicographically.
  // This guarantee is currently happening at minting of the withdrawal order
  // UTxO (TODO?).
  let Pair(_, pt_lovelace) = builtin.head_list(pt).2nd |> builtin.head_list
  let pt_assets = pt |> builtin.tail_list

  // 2. Output lovelace must not exceed the target.
  expect p1_lovelace <= pt_lovelace

  // 3. Output lovelace must either grow or remain unchanged.
  expect p1_lovelace >= p0_lovelace

  when assets_to_add is {
    AddLovelace {
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
    AddCardanoAssets {
      assets,
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
        validate_transfer_of_given_assets_with_change(
          p0_assets,
          r0_assets,
          p1_assets,
          r1_assets,
          pt_assets,
          assets,
        )

      // Done.
      True
    }
    AddMidgardAssets { .. } -> todo
  }
}

/// Validates addition of `assets_to_add` to `p0_assets` eqauls `p1_assets`.
/// Assumes `assets_to_add` is validated to only carry positive quantities.
fn validate_addition_reaches(
  p0_assets: Dict<PolicyId, Dict<AssetName, Int>>,
  assets_to_add: Dict<PolicyId, Dict<AssetName, Int>>,
  p1_assets: Dict<PolicyId, Dict<AssetName, Int>>,
) -> Bool {
  p1_assets == dict.union_with(p0_assets, assets_to_add, dict_strategy.sum)
}

/// p0: Non-ADA assets of input payout (without auth NFT)
/// r0: Non-ADA assets of input reserve
/// p1: Non-ADA assets of output payout (without auth NFT)
/// r1: Non-ADA assets of output reserve
/// pt: Target non-ADA assets
///
/// This function assumes all assets are stripped of their ADA.
fn validate_transfer_of_given_assets_with_change(
  p0_assets: Dict<PolicyId, Dict<AssetName, Int>>,
  r0_assets: Dict<PolicyId, Dict<AssetName, Int>>,
  p1_assets: Dict<PolicyId, Dict<AssetName, Int>>,
  r1_assets: Dict<PolicyId, Dict<AssetName, Int>>,
  pt_assets: ValuePairs,
  assets_to_add_pairs: ValuePairs,
) -> Bool {
  expect utils.all_value_pairs_quantities_are_positive(assets_to_add_pairs)
  let assets_to_add =
    utils.unsafe_value_pairs_to_value(assets_to_add_pairs) |> assets.to_dict

  expect p0_assets |> validate_addition_reaches(assets_to_add, p1_assets)
  expect r1_assets |> validate_addition_reaches(assets_to_add, r0_assets)

  let pt_assets_dict =
    utils.unsafe_value_pairs_to_value(pt_assets) |> assets.to_dict
  expect pt_assets_dict |> dict.expect_contains(p1_assets, int.compare)

  // Done.
  True
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
    output: Output { address: reserve_input_address, value: r0, .. },
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
  r0 |> utils.get_lovelace_and_assets
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

  r1 |> utils.get_lovelace_and_assets
}
