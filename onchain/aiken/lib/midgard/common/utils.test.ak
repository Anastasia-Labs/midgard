use aiken/collection/dict
use cardano/assets
use cardano/fuzz as cardano_fuzz
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
