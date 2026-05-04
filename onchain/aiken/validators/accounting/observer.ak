use aiken/builtin
use aiken/collection/dict.{Dict}
use aiken/collection/dict/strategy as dict_strategy
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use aiken/primitive/int
use aiken_design_patterns/singular_utxo_indexer
use cardano/address.{Address, Credential}
use cardano/assets.{AssetName, Lovelace, PolicyId, Value}
use cardano/transaction.{InlineDatum, Input, NoDatum, Output, Transaction}
use midgard/accounting/observer.{
  AddCardanoAssets, AddLovelace, AddMidgardAssets, AssetsToAdd, WithdrawRedeemer,
  separate_ada_and_remove_auth_nft,
}
use midgard/accounting/payout
use midgard/common/types.{ValuePairs}
use midgard/common/utils
use midgard/hub_oracle as hub

validator withdraw(hub_oracle_script_hash: ScriptHash) {
  withdraw(redeemer: WithdrawRedeemer, _own_cred: Credential, tx: Transaction) {
    let Transaction { inputs, outputs, reference_inputs, .. } = tx

    let WithdrawRedeemer {
      payout_input_outref,
      payout_input_index,
      payout_output_index,
      payout_nft_asset_name,
      assets_to_add,
      hub_oracle_ref_input_index,
    } = redeemer

    let hub.Datum { payout: payout_policy_id, reserve_addr, .. } =
      hub.get_datum(
        reference_inputs,
        hub_oracle_script_hash,
        hub_oracle_ref_input_index,
      )

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

    expect Input {
      output: Output {
        address: payout_input_address,
        value: payout_input_value,
        datum: InlineDatum(payout_input_datum_data),
        ..
      },
      ..
    } = payout_input

    expect payout.Datum { l2_value, .. } = payout_input_datum_data

    expect Output {
      address: payout_output_address,
      value: payout_output_value,
      datum: InlineDatum(output_datum_data),
      reference_script: None,
    } = payout_output

    // 1. Payout UTxO must be reproduced at the same address.
    expect payout_input_address == payout_output_address

    // 2. Datum must not change.
    expect output_datum_data == payout_input_datum_data

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
        reserve_addr,
        inputs,
        outputs,
      )

    // Done.
    True
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
  reserve_addr: Address,
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
    AddLovelace { m_reserve_change_output_index } -> {
      // 4a. Target Lovelace must not have been reached.
      expect p0_lovelace < pt_lovelace

      // 5a. Specified UTxO from reserve must be getting spent with a matching
      //     set of redeemer arguments.
      let (r0_lovelace, r0_assets) =
        get_reserve_input_lovelace_and_assets(reserve_addr, inputs)

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
    AddCardanoAssets { assets, reserve_change_output_index } -> {
      // 4b. Specified UTxO from reserve must be getting spent with a matching
      //     set of redeemer arguments.
      let (r0_lovelace, r0_assets) =
        get_reserve_input_lovelace_and_assets(reserve_addr, inputs)

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
  p1_assets == dict.union_with(
    p0_assets,
    assets_to_add,
    fn(_policy_id, left_assets, right_assets, keep, _discard) {
      keep(dict.union_with(left_assets, right_assets, dict_strategy.sum()))
    },
  )
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
  pt_assets_dict
    |> dict.expect_contains(
        p1_assets,
        fn(superset_assets, subset_assets) {
          dict.expect_contains(superset_assets, subset_assets, int.compare)
          Equal
        },
      )

  // Done.
  True
}

/// TODO: Here the assumption is that resreve UTxOs do not carry any beacon
///       tokens.
fn get_reserve_input_lovelace_and_assets(
  reserve_addr: Address,
  inputs: List<Input>,
) -> (Lovelace, Dict<PolicyId, Dict<AssetName, Int>>) {
  // 1. There must be exactly 1 UTxO spent from `reserve_addr`.
  expect [Input { output: Output { value: r0, .. }, .. }] =
    inputs |> list.filter(fn(i) { i.output.address == reserve_addr })
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
