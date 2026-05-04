use aiken/collection/dict
use aiken/fuzz
use cardano/assets.{Lovelace, Value, ada_asset_name, ada_policy_id}
use cardano/fuzz as cardano_fuzz
use midgard/accounting/observer.{separate_ada_and_remove_auth_nft}

const target_policy_id = #"01"

const target_asset_name = #"a0"

const secondary_policy_id = #"02"

const secondary_asset_name_a = #"0a"

const additional_asset_name = #"b0"

const mismatched_asset_name = #"ff"

const baseline_lovelace = 2_000_000

const singleton_quantity = 1

const invalid_quantity = 2

const secondary_quantity_a = 7

fn value_with_target_asset(lovelace: Lovelace) -> Value {
  assets.from_asset_list(
    [
      Pair(ada_policy_id, [Pair(ada_asset_name, lovelace)]),
      Pair(target_policy_id, [Pair(target_asset_name, singleton_quantity)]),
    ],
  )
}

fn value_with_mismatched_target_asset(lovelace: Lovelace) -> Value {
  assets.from_asset_list(
    [
      Pair(ada_policy_id, [Pair(ada_asset_name, lovelace)]),
      Pair(target_policy_id, [Pair(mismatched_asset_name, singleton_quantity)]),
      Pair(
        secondary_policy_id,
        [Pair(secondary_asset_name_a, secondary_quantity_a)],
      ),
    ],
  )
}

fn value_with_invalid_target_quantity(lovelace: Lovelace) -> Value {
  assets.from_asset_list(
    [
      Pair(ada_policy_id, [Pair(ada_asset_name, lovelace)]),
      Pair(target_policy_id, [Pair(target_asset_name, invalid_quantity)]),
      Pair(
        secondary_policy_id,
        [Pair(secondary_asset_name_a, secondary_quantity_a)],
      ),
    ],
  )
}

fn value_with_extra_target_asset(lovelace: Lovelace) -> Value {
  assets.from_asset_list(
    [
      Pair(ada_policy_id, [Pair(ada_asset_name, lovelace)]),
      Pair(
        target_policy_id,
        [
          Pair(target_asset_name, singleton_quantity),
          Pair(additional_asset_name, singleton_quantity),
        ],
      ),
      Pair(
        secondary_policy_id,
        [Pair(secondary_asset_name_a, secondary_quantity_a)],
      ),
    ],
  )
}

test success__separate_ada_and_remove_auth_nft__removes_target_from_generated_value(
  v via cardano_fuzz.value(value_with_target_asset(baseline_lovelace)),
) {
  let (lovelace, assets_without_nft) =
    separate_ada_and_remove_auth_nft(v, target_policy_id, target_asset_name)
  let (_, expected_assets) =
    v
      |> assets.without_lovelace
      |> assets.to_dict
      |> dict.expect_pop(target_policy_id)

  and {
    lovelace == baseline_lovelace,
    assets_without_nft == expected_assets,
    !dict.has_key(assets_without_nft, ada_policy_id),
    !dict.has_key(assets_without_nft, target_policy_id),
  }
}

test fail__separate_ada_and_remove_auth_nft__missing_target(
  v via cardano_fuzz.value(
    assets.from_asset_list(
      [
        Pair(ada_policy_id, [Pair(ada_asset_name, baseline_lovelace)]),
        Pair(
          secondary_policy_id,
          [Pair(secondary_asset_name_a, secondary_quantity_a)],
        ),
      ],
    ),
  ),
) fail {
  let (lovelace, assets_without_nft) =
    separate_ada_and_remove_auth_nft(v, target_policy_id, target_asset_name)

  or {
    lovelace == baseline_lovelace,
    assets_without_nft == dict.empty,
  }
}

test fail__separate_ada_and_remove_auth_nft__mismatched_target_asset(
  v via cardano_fuzz.value(
    value_with_mismatched_target_asset(baseline_lovelace),
  ),
) fail {
  let (lovelace, assets_without_nft) =
    separate_ada_and_remove_auth_nft(v, target_policy_id, target_asset_name)

  or {
    lovelace == baseline_lovelace,
    assets_without_nft == dict.empty,
  }
}

test fail__separate_ada_and_remove_auth_nft__invalid_target_quantity(
  v via cardano_fuzz.value(
    value_with_invalid_target_quantity(baseline_lovelace),
  ),
) fail {
  let (lovelace, assets_without_nft) =
    separate_ada_and_remove_auth_nft(v, target_policy_id, target_asset_name)

  or {
    lovelace == baseline_lovelace,
    assets_without_nft == dict.empty,
  }
}

test fail__separate_ada_and_remove_auth_nft__extra_target_asset(
  v via cardano_fuzz.value(value_with_extra_target_asset(baseline_lovelace)),
) fail {
  let (lovelace, assets_without_nft) =
    separate_ada_and_remove_auth_nft(v, target_policy_id, target_asset_name)

  or {
    lovelace == baseline_lovelace,
    assets_without_nft == dict.empty,
  }
}

test success__separate_ada_and_remove_auth_nft__preserves_generated_lovelace(
  lovelace via fuzz.int_at_least(1),
) {
  let v = value_with_target_asset(lovelace)
  let (found_lovelace, _) =
    separate_ada_and_remove_auth_nft(v, target_policy_id, target_asset_name)

  found_lovelace == lovelace
}
