use cardano/address
use cardano/assets
use cardano/transaction.{InlineDatum, Output}
use midgard/event_history/node_role.{Filler, Order, RootContent}
use midgard/event_history/ordering.{Key, Link, Root}

const policy = #"11111111111111111111111111111111111111111111111111111111"

const owner = #"22222222222222222222222222222222222222222222222222222222"

const target =
  #"8080808080808080808080808080808080808080808080808080808080808080"

const low = #"0000000000000000000000000000000000000000000000000000000000000000"

const high = #"ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"

fn output(
  position: ordering.Position,
  next: Option<ByteArray>,
  content: node_role.Content<Data>,
) -> Output {
  let payload: Data = content
  let raw: Data = Link { position, next, protected_until: 300, payload }
  Output {
    address: address.from_script(policy),
    value: assets.from_lovelace(3_000_000)
      |> assets.add(policy, ordering.token_name(position), 1),
    datum: InlineDatum(raw),
    reference_script: None,
  }
}

fn absent(witness: Output) -> Bool {
  node_role.authenticated_absence(
    witness,
    policy,
    address.from_script(policy),
    target,
  )
}

test empty_root_proves_arbitrary_key_absence() {
  absent(output(Root, None, RootContent))
}

test exact_filler_proves_order_absence() {
  absent(output(Key(target), Some(high), Filler { refund_key: owner }))
}

test exact_order_cannot_prove_absence() {
  let facts: Data = 42
  !absent(output(Key(target), None, Order { facts }))
}

test filler_in_a_different_gap_does_not_prove_absence() {
  !absent(output(Key(high), None, Filler { refund_key: owner }))
}

test order_predecessor_can_prove_strict_gap_absence() {
  let facts: Data = 42
  absent(output(Key(low), Some(high), Order { facts }))
}

test root_pointing_at_target_cannot_prove_absence() {
  !absent(output(Root, Some(target), RootContent))
}

test node_cannot_claim_the_root_role() fail {
  expect _ = absent(output(Key(target), None, RootContent))
  True
}

test root_cannot_claim_an_event_role() fail {
  let facts: Data = 42
  expect _ = absent(output(Root, None, Order { facts }))
  True
}
