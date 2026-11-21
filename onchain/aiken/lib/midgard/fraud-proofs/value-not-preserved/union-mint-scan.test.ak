use midgard/fraud_proofs/value_not_preserved/union_mint_scan

test mint_policy_header_authenticates_count_and_policy() {
  let Pair(policy, Pair(consumed, count)) = union_mint_scan.policy_header(
    #"82581caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa14001",
    None,
  )
  policy == #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" && consumed == 32 && count == 1
}

test mint_asset_preserves_signed_burn() {
  union_mint_scan.asset(#"416124", None) == Pair(#"61", Pair(-5, 3))
}

test mint_asset_rejects_zero_quantity() fail {
  union_mint_scan.asset(#"416100", None) == Pair(#"", Pair(0, 0))
}

test mint_asset_rejects_duplicate_name() fail {
  union_mint_scan.asset(#"416101", Some(#"61")) == Pair(#"", Pair(0, 0))
}

test mint_policy_rejects_duplicate_policy() fail {
  union_mint_scan.policy_header(
    #"82581caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa14001",
    Some(#"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"),
  ) == Pair(#"", Pair(0, 0))
}

test mint_asset_rejects_noncanonical_integer() fail {
  union_mint_scan.asset(#"41611801", None) == Pair(#"", Pair(0, 0))
}
