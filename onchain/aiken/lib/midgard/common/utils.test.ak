use aiken/collection/dict
use cardano/assets.{AssetName, PolicyId, Value}
use cardano/fuzz as cardano_fuzz
use midgard/common/types.{ValuePairs}
use midgard/common/utils

const baseline_lovelace = 2_000_000

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

test success__get_lovelace_and_assets(
  v via cardano_fuzz.value(assets.from_lovelace(baseline_lovelace)),
) {
  let (lovelace, v_assets) = utils.get_lovelace_and_assets(v)

  and {
    lovelace == baseline_lovelace,
    v_assets == ( v |> assets.without_lovelace |> assets.to_dict ),
  }
}

test success__get_lovelace_and_assets__lovelace_only(
  v via cardano_fuzz.lovelace(),
) {
  let (lovelace, v_assets) = utils.get_lovelace_and_assets(v)

  and {
    lovelace == assets.lovelace_of(v),
    v_assets == dict.empty,
  }
}

// COMMENTED OUT: Waiting for Aiken's next release.
// 
// const asset_name_fuzzer = fn(_p: PolicyId) { cardano_fuzz.asset_name() }
//
// const quantity_fuzzer = fn(_p: PolicyId, _a: AssetName) { fuzz.int_at_least(1) }
// 
// test success__all_value_pairs_quantities_are_positive(
//   v via cardano_fuzz.value_with(
//     assets.from_lovelace(baseline_lovelace),
//     fuzz.list(cardano_fuzz.policy_id()),
//     asset_name_fuzzer,
//     quantity_fuzzer,
//   ),
// ) {
//   all_value_pairs_quantities_are_positive(v |> value_to_value_pairs)
// }

test roundtrip__value_to_pairs_to_value(
  v0 via cardano_fuzz.value(assets.from_lovelace(baseline_lovelace)),
) {
  let v_pairs = utils.value_to_value_pairs(v0)
  let v1 = v_pairs |> utils.value_pairs_to_value_strict(baseline_lovelace)
  let v2 = v_pairs |> utils.unsafe_value_pairs_to_value
  let v_pairs2 = v1 |> utils.value_to_value_pairs
  let v3 = v_pairs2 |> utils.unsafe_value_pairs_to_value
  and {
    v0 == v1,
    v0 == v2,
    v0 == v3,
  }
}

const test_auth_policy: PolicyId =
  #"44444444444444444444444444444444444444444444444444444444"

const test_auth_asset: AssetName = #"aa"

const test_target_policy: PolicyId =
  #"55555555555555555555555555555555555555555555555555555555"

fn test_authentic_value(lovelace: Int) -> Value {
  assets.from_lovelace(lovelace)
    |> assets.add(test_auth_policy, test_auth_asset, 1)
}

fn test_target(lovelace: Int) -> ValuePairs {
  [Pair(assets.ada_policy_id, [Pair(assets.ada_asset_name, lovelace)])]
}

fn test_multi_asset_target(lovelace: Int) -> ValuePairs {
  [
    Pair(assets.ada_policy_id, [Pair(assets.ada_asset_name, lovelace)]),
    Pair(test_target_policy, [Pair(#"01", 1)]),
  ]
}

test success__authentic_value_is_bounded_by_target__underfunded_value() {
  utils.authentic_value_is_bounded_by_target(
    test_authentic_value(2_000_000),
    test_auth_policy,
    test_auth_asset,
    test_target(3_000_000),
  )
}

test success__authentic_value_is_bounded_by_target__ada_only_value_for_multi_asset_target() {
  utils.authentic_value_is_bounded_by_target(
    test_authentic_value(2_000_000),
    test_auth_policy,
    test_auth_asset,
    test_multi_asset_target(3_000_000),
  )
}

test fail__authentic_value_is_bounded_by_target__overfunded_lovelace() fail {
  utils.authentic_value_is_bounded_by_target(
    test_authentic_value(4_000_000),
    test_auth_policy,
    test_auth_asset,
    test_target(3_000_000),
  )
}

test fail__authentic_value_is_bounded_by_target__overfunded_asset() fail {
  utils.authentic_value_is_bounded_by_target(
    test_authentic_value(2_000_000)
      |> assets.add(test_target_policy, #"01", 2),
    test_auth_policy,
    test_auth_asset,
    test_multi_asset_target(3_000_000),
  )
}

test fail__authentic_value_is_bounded_by_target__unexpected_asset() fail {
  utils.authentic_value_is_bounded_by_target(
    test_authentic_value(2_000_000)
      |> assets.add(test_target_policy, #"01", 1),
    test_auth_policy,
    test_auth_asset,
    test_target(3_000_000),
  )
}
