use cardano/address
use cardano/assets
use cardano/transaction.{InlineDatum, NoDatum, Output, OutputReference}
use midgard/event_history/funding
use midgard/event_history/ordering.{Key, Link}
use midgard/ledger_state.{WithdrawalBody}
use midgard/payout

const policy = #"11111111111111111111111111111111111111111111111111111111"

const owner = #"22222222222222222222222222222222222222222222222222222222"

const other = #"33333333333333333333333333333333333333333333333333333333"

const key = #"4444444444444444444444444444444444444444444444444444444444444444"

fn filler_value() -> assets.Value {
  assets.from_lovelace(4_000_000) |> assets.add(policy, key, 1)
}

fn refund() -> Output {
  Output {
    address: address.from_verification_key(owner),
    value: assets.from_lovelace(4_000_000),
    datum: NoDatum,
    reference_script: None,
  }
}

test admission_funds_future_pointer_and_timestamp_at_the_exact_floor() {
  let payload: Data = 42
  let node = Link { position: Key(key), next: None, protected_until: 0, payload }
  let worst: Data = Link { ..node, next: Some(key), protected_until: ordering.max_protection_time }
  let floor = funding.minimum_output_lovelace(filler_value(), InlineDatum(worst), False)
  and {
    funding.node_is_funded(assets.from_lovelace(floor) |> assets.add(policy, key, 1), node),
    !funding.node_is_funded(assets.from_lovelace(floor - 1) |> assets.add(policy, key, 1), node),
  }
}

test deposit_structural_ada_cannot_subsidize_its_reserve_output() {
  let floor = funding.minimum_output_lovelace(assets.from_lovelace(4_000_000), NoDatum, False)
  and {
    funding.deposit_is_funded(assets.from_lovelace(floor)),
    !funding.deposit_is_funded(assets.from_lovelace(floor - 1)),
    !funding.deposit_is_funded(assets.from_lovelace(0)),
  }
}

fn withdrawal_body() -> WithdrawalBody {
  WithdrawalBody {
    l2_outref: OutputReference { transaction_id: key, output_index: 0 },
    l2_owner: owner,
    l2_value: [Pair(#"", [Pair(#"", 20_000_000)])],
    l1_address: address.from_verification_key(owner),
    l1_datum: NoDatum,
  }
}

test withdrawal_funding_checks_initial_payout_refund_and_target() {
  let body = withdrawal_body()
  let datum: Data = payout.Datum { l2_value: body.l2_value, l1_address: body.l1_address, l1_datum: body.l1_datum }
  let floor = funding.minimum_output_lovelace(assets.from_lovelace(4_000_000), InlineDatum(datum), True)
  and {
    funding.withdrawal_is_funded(assets.from_lovelace(floor), body, NoDatum),
    !funding.withdrawal_is_funded(assets.from_lovelace(floor - 1), body, NoDatum),
    !funding.withdrawal_is_funded(assets.from_lovelace(20_000_001), body, NoDatum),
    !funding.withdrawal_is_funded(assets.from_lovelace(1), WithdrawalBody { ..body, l2_value: [Pair(#"", [Pair(#"", 1)])] }, NoDatum),
  }
}

test bounded_filler_refund_needs_no_owner_signer() {
  funding.pays_filler_refund(filler_value(), policy, key, owner, refund())
}

test filler_cannot_introduce_native_assets_into_funding() {
  !funding.valid_filler_value(
    filler_value() |> assets.add(other, #"01", 1),
    policy,
    key,
  )
}

test filler_refund_cannot_deduct_fees() {
  !funding.pays_filler_refund(
    filler_value(),
    policy,
    key,
    owner,
    Output { ..refund(), value: assets.from_lovelace(3_999_999) },
  )
}

test filler_refund_cannot_divert_funds() {
  !funding.pays_filler_refund(
    filler_value(),
    policy,
    key,
    owner,
    Output { ..refund(), address: address.from_verification_key(other) },
  )
}

test filler_creator_cannot_impose_a_refund_datum_or_script() {
  let datum: Data = 42
  and {
    !funding.pays_filler_refund(
      filler_value(),
      policy,
      key,
      owner,
      Output { ..refund(), datum: InlineDatum(datum) },
    ),
    !funding.pays_filler_refund(
      filler_value(),
      policy,
      key,
      owner,
      Output { ..refund(), reference_script: Some(other) },
    ),
    !funding.pays_filler_refund(
      filler_value(),
      policy,
      key,
      owner,
      Output { ..refund(), address: address.from_script(owner) },
    ),
  }
}

test original_assets_exclude_structural_token_and_declared_node_funding() {
  let deposited = assets.from_lovelace(7_000_000)
    |> assets.add(other, #"010203", 27)
  let order = deposited
    |> assets.add(#"", #"", 3_000_000)
    |> assets.add(policy, key, 1)
  funding.order_assets(order, policy, key, 3_000_000) == deposited
}

test negative_funding_cannot_inflate_deposit_assets() fail {
  expect _ = funding.order_assets(filler_value(), policy, key, -1)
  True
}

test funding_cannot_exceed_actual_lovelace() fail {
  expect _ = funding.order_assets(filler_value(), policy, key, 4_000_001)
  True
}

test multiple_structural_tokens_cannot_be_projected() fail {
  expect _ = funding.order_assets(
    filler_value() |> assets.add(policy, key, 1),
    policy,
    key,
    3_000_000,
  )
  True
}
