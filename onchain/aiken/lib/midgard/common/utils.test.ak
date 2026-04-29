use cardano/assets.{Lovelace, PolicyId, Value, ada_asset_name, ada_policy_id}
use cardano/fuzz as cardano_fuzz
use midgard/common/utils

const target_policy_id = #"01"

const target_asset_name = #"a0"

const secondary_policy_id = #"02"

const secondary_asset_name_a = #"0a"

const secondary_asset_name_b = #"0b"

const additional_asset_name = #"b0"

const mismatched_asset_name = #"ff"

const baseline_lovelace = 2_000_000

const larger_lovelace = 5_000_000

const alternate_lovelace = 3_000_000

const singleton_quantity = 1

const invalid_quantity = 2

const secondary_quantity_a = 7

const secondary_quantity_b = 11

test zip_foldl() {
  let list_a = [1, 2, 3]
  let list_b = [1, 2, 3, 4]
  let final_list =
    utils.zip_foldl(
      list_a,
      list_b,
      [],
      fn(a, b, acc) {
        let new_list = [(a, b), ..acc]
        new_list
      },
    )
  final_list == [(3, 3), (2, 2), (1, 1)]
}

test zip_foldr() {
  let list_a = [1, 2, 3]
  let list_b = [1, 2, 3, 4]
  let final_list =
    utils.zip_foldr(
      list_a,
      list_b,
      [],
      fn(a, b, acc) {
        let new_list = [(a, b), ..acc]
        new_list
      },
    )
  final_list == [(1, 1), (2, 2), (3, 3)]
}

test prop_get_lovelace_and_assets_matches_without_lovelace(
  v via cardano_fuzz.value(assets.from_lovelace(baseline_lovelace)),
) {
  let (lovelace, v_assets) = utils.get_lovelace_and_assets(v)

  and {
    lovelace == baseline_lovelace,
    v_assets == ( v |> assets.without_lovelace |> assets.to_dict ),
  }
}

test prop_get_lovelace_and_assets_lovelace_only(v via cardano_fuzz.lovelace()) {
  let (lovelace, v_assets) = utils.get_lovelace_and_assets(v)

  and {
    lovelace == assets.lovelace_of(v),
    v_assets == dict.empty,
  }
}
