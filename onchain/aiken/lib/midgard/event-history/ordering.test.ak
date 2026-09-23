use cardano/address
use cardano/assets
use cardano/transaction.{InlineDatum, Output}
use midgard/event_history/ordering.{Key, Link, Root}

const policy = #"11111111111111111111111111111111111111111111111111111111"

const other_policy = #"22222222222222222222222222222222222222222222222222222222"

const zero = #"0000000000000000000000000000000000000000000000000000000000000000"

const middle =
  #"8080808080808080808080808080808080808080808080808080808080808080"

const maximum =
  #"ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"

fn link(position: ordering.Position, next: Option<ByteArray>) -> Link<Data> {
  let payload: Data = 42
  Link { position, next, protected_until: 300, payload }
}

fn output(node: Link<Data>) -> Output {
  let data: Data = node
  Output {
    address: address.from_script(policy),
    value: assets.from_lovelace(3_000_000)
      |> assets.add(policy, ordering.token_name(node.position), 1),
    datum: InlineDatum(data),
    reference_script: None,
  }
}

test empty_root_covers_both_extreme_keys() {
  let root = link(Root, None)
  and {
    ordering.contains_gap(root, zero),
    ordering.contains_gap(root, maximum),
  }
}

test protection_time_has_a_funded_maximum_encoding_width() {
  let root = link(Root, None)
  and {
    ordering.valid_link(Link { ..root, protected_until: ordering.max_protection_time }),
    !ordering.valid_link(Link { ..root, protected_until: ordering.max_protection_time + 1 }),
  }
}

test root_token_name_does_not_steal_zero_key() {
  and {
    ordering.token_name(Root) == #"",
    ordering.token_name(Key(zero)) == zero,
    ordering.token_name(Key(maximum)) == maximum,
  }
}

test first_and_tail_gaps_exclude_existing_keys() {
  and {
    ordering.contains_gap(link(Root, Some(middle)), zero),
    !ordering.contains_gap(link(Root, Some(middle)), middle),
    !ordering.contains_gap(link(Root, Some(middle)), maximum),
    ordering.contains_gap(link(Key(middle), None), maximum),
    !ordering.contains_gap(link(Key(middle), None), middle),
    !ordering.contains_gap(link(Key(middle), None), zero),
  }
}

test bounded_gap_is_strict_on_both_sides() {
  let predecessor = link(Key(zero), Some(maximum))
  and {
    ordering.contains_gap(predecessor, middle),
    !ordering.contains_gap(predecessor, zero),
    !ordering.contains_gap(predecessor, maximum),
  }
}

test short_and_prefixed_keys_do_not_enter_list() {
  and {
    !ordering.valid_key(#"01"),
    !ordering.valid_key(
      #"01ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
    ),
    !ordering.contains_gap(link(Root, None), #""),
    !ordering.valid_link(link(Key(#"01"), None)),
    !ordering.valid_link(link(Root, Some(#"01"))),
  }
}

test reversed_and_self_links_reject() {
  and {
    !ordering.valid_link(link(Key(maximum), Some(zero))),
    !ordering.valid_link(link(Key(middle), Some(middle))),
  }
}

test authentic_root_and_full_width_nodes_decode() {
  let root = link(Root, Some(zero))
  let first = link(Key(zero), Some(maximum))
  let last = link(Key(maximum), None)
  and {
    ordering.authenticate(output(root), policy, address.from_script(policy)) == root,
    ordering.authenticate(output(first), policy, address.from_script(policy)) == first,
    ordering.authenticate(output(last), policy, address.from_script(policy)) == last,
  }
}

test wrong_deployment_rejects() fail {
  expect _ = ordering.authenticate(
    output(link(Root, None)),
    other_policy,
    address.from_script(policy),
  )
  True
}

test wrong_address_rejects() fail {
  expect _ = ordering.authenticate(
    output(link(Root, None)),
    policy,
    address.from_script(other_policy),
  )
  True
}

test missing_authentication_rejects() fail {
  let original = output(link(Root, None))
  expect _ = ordering.authenticate(
    Output { ..original, value: assets.from_lovelace(3_000_000) },
    policy,
    original.address,
  )
  True
}

test incorrect_quantity_rejects() fail {
  let original = output(link(Key(middle), None))
  expect _ = ordering.authenticate(
    Output {
      ..original,
      value: original.value |> assets.add(policy, middle, 1),
    },
    policy,
    original.address,
  )
  True
}

test root_role_cannot_reinterpret_node() fail {
  let original = output(link(Key(middle), None))
  let data: Data = link(Root, None)
  expect _ = ordering.authenticate(
    Output { ..original, datum: InlineDatum(data) },
    policy,
    original.address,
  )
  True
}

test one_output_cannot_authenticate_two_nodes() fail {
  let original = output(link(Root, None))
  expect _ = ordering.authenticate(
    Output {
      ..original,
      value: original.value |> assets.add(policy, middle, 1),
    },
    policy,
    original.address,
  )
  True
}

test inline_datum_is_not_authority_without_matching_key() fail {
  let original = output(link(Key(middle), None))
  let data: Data = link(Key(maximum), None)
  expect _ = ordering.authenticate(
    Output { ..original, datum: InlineDatum(data) },
    policy,
    original.address,
  )
  True
}

test reference_script_cannot_be_smuggled_into_continuation() fail {
  let original = output(link(Root, None))
  expect _ = ordering.authenticate(
    Output { ..original, reference_script: Some(other_policy) },
    policy,
    original.address,
  )
  True
}

test pointer_update_preserves_actual_deposit_assets() {
  let original = output(link(Key(zero), Some(maximum)))
  let before = Output {
    ..original,
    value: original.value |> assets.add(other_policy, #"010203", 27),
  }
  let changed = output(link(Key(zero), Some(middle)))
  let after = Output { ..changed, value: before.value }
  and {
    ordering.preserves_predecessor(
      before,
      after,
      policy,
      original.address,
      Some(middle),
      300,
    ),
    !ordering.preserves_predecessor(
      before,
      Output {
        ..after,
        value: after.value |> assets.add(other_policy, #"010203", -1),
      },
      policy,
      original.address,
      Some(middle),
      300,
    ),
    !ordering.preserves_predecessor(
      before,
      Output { ..after, value: after.value |> assets.add(#"", #"", -1) },
      policy,
      original.address,
      Some(middle),
      300,
    ),
  }
}

test pointer_update_cannot_change_immutable_payload() {
  let original = output(link(Key(zero), Some(maximum)))
  let payload: Data = 43
  let changed = output(Link { ..link(Key(zero), Some(middle)), payload })
  !ordering.preserves_predecessor(
    original,
    changed,
    policy,
    original.address,
    Some(middle),
    300,
  )
}
