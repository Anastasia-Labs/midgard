use cardano/assets
use midgard/common/utils

test canonical_value_accepts_empty_value() {
  utils.is_canonical_value([]) == assets.zero
}

test canonical_value_preserves_datum_data() {
  let xs = [
    Pair(#"", [Pair(#"", 12_000_000)]),
    Pair(#"11", [Pair("alpha", 5), Pair("beta", 7)]),
    Pair(#"22", [Pair(#"", 1)]),
  ]
  let original: Data = xs
  let checked: Data = utils.is_canonical_value(xs)
  checked == original
}

test canonical_value_accepts_signed_quantities() {
  utils.is_canonical_value([Pair(#"11", [Pair("alpha", -5)])]) == assets.from_asset(
    #"11",
    "alpha",
    -5,
  )
}

test canonical_value_rejects_policy_reordering() fail {
  utils.is_canonical_value(
    [Pair(#"22", [Pair("alpha", 1)]), Pair(#"11", [Pair("beta", 1)])],
  ) == assets.zero
}

test canonical_value_rejects_ada_after_native_policy() fail {
  utils.is_canonical_value(
    [Pair(#"11", [Pair("alpha", 1)]), Pair(#"", [Pair(#"", 1_000_000)])],
  ) == assets.zero
}

test canonical_value_rejects_duplicate_policies() fail {
  utils.is_canonical_value(
    [Pair(#"11", [Pair("alpha", 1)]), Pair(#"11", [Pair("beta", 1)])],
  ) == assets.zero
}

test canonical_value_rejects_length_first_token_order() fail {
  utils.is_canonical_value([Pair(#"11", [Pair("beta", 7), Pair("alpha", 5)])]) == assets.zero
}

test canonical_value_rejects_duplicate_asset_names() fail {
  utils.is_canonical_value([Pair(#"11", [Pair("alpha", 1), Pair("alpha", 2)])]) == assets.zero
}

test canonical_value_rejects_zero_quantity() fail {
  utils.is_canonical_value([Pair(#"11", [Pair("alpha", 0)])]) == assets.zero
}

test canonical_value_rejects_empty_asset_map() fail {
  utils.is_canonical_value([Pair(#"11", [])]) == assets.zero
}
