use aiken/builtin
use aiken/collection/list
use aiken/interval
use cardano/address
use cardano/assets
use cardano/transaction.{
  InlineDatum, Input, NoDatum, Output, OutputReference, Transaction, placeholder,
}
use env
use midgard/event_history/admission as semantics
use midgard/event_history/node_role.{Filler, Order, RootContent}
use midgard/event_history/order_facts.{
  Deposit, DepositPayload, Facts, Inline, Payload,
}
use midgard/event_history/ordering.{Key, Link, Root}
use midgard/ledger_state.{DepositEvent, DepositInfo}
use midgard/user_events

const policy = #"11111111111111111111111111111111111111111111111111111111"

const retention = #"22222222222222222222222222222222222222222222222222222222"

const owner = #"33333333333333333333333333333333333333333333333333333333"

const tx_id =
  #"4444444444444444444444444444444444444444444444444444444444444444"

const nonce = OutputReference { transaction_id: tx_id, output_index: 0 }

fn payload() -> Payload {
  DepositPayload {
    event: DepositEvent {
      id: nonce,
      info: DepositInfo {
        l2_address: address.from_verification_key(owner),
        l2_network_id: 0,
        l2_datum: None,
      },
    },
  }
}

fn facts() -> Facts {
  Facts {
    event_id: nonce,
    inclusion_time: 400 + env.event_wait_duration,
    location: Inline { payload: payload() },
    structural_lovelace: 3_000_000,
    structural_refund_key: owner,
  }
}

fn order_output(
  facts: Facts,
  next: Option<ByteArray>,
  protected_until: Int,
) -> Output {
  let raw_facts: Data = facts
  let content: Data = Order { facts: raw_facts }
  let raw: Data = Link {
    position: Key(user_events.out_ref_to_nonce(nonce)),
    next,
    protected_until,
    payload: content,
  }
  Output {
    address: address.from_script(policy),
    value: assets.from_lovelace(10_000_000)
      |> assets.add(policy, user_events.out_ref_to_nonce(nonce), 1),
    datum: InlineDatum(raw),
    reference_script: None,
  }
}

fn admission(facts: Facts) -> Transaction {
  Transaction {
    ..placeholder,
    inputs: [
      Input {
        output_reference: nonce,
        output: Output {
          address: address.from_verification_key(owner),
          value: assets.from_lovelace(20_000_000),
          datum: NoDatum,
          reference_script: None,
        },
      },
    ],
    outputs: [order_output(facts, None, 600)],
    validity_range: interval.between(300, 400),
  }
}

fn params() -> semantics.Parameters {
  semantics.Parameters {
    policy,
    address: address.from_script(policy),
    retention_address: address.from_script(retention),
    kind: Deposit,
    protection_duration: 200,
    inline_limit: 1000,
    max_payload_bytes: 15000,
    max_payload_nodes: 1024,
  }
}

fn filler(next: Option<ByteArray>, protection: Int) -> Output {
  let role: Data = Filler { refund_key: owner }
  let raw: Data = Link {
    position: Key(user_events.out_ref_to_nonce(nonce)),
    next,
    protected_until: protection,
    payload: role,
  }
  Output {
    ..order_output(facts(), next, protection),
    datum: InlineDatum(raw),
    value: assets.from_lovelace(5_000_000)
      |> assets.add(policy, user_events.out_ref_to_nonce(nonce), 1),
  }
}

fn root(next: Option<ByteArray>, protection: Int) -> Output {
  let role: node_role.Content<Data> = RootContent
  let raw: Data = Link {
    position: Root,
    next,
    protected_until: protection,
    payload: role,
  }
  Output {
    ..filler(None, 0),
    datum: InlineDatum(raw),
    value: assets.from_lovelace(3_000_000) |> assets.add(policy, #"", 1),
  }
}

fn history_input(output: Output) -> Input {
  Input {
    output_reference: OutputReference { transaction_id: tx_id, output_index: 2 },
    output,
  }
}

fn refund() -> Output {
  Output {
    address: address.from_verification_key(owner),
    value: assets.from_lovelace(5_000_000),
    datum: NoDatum,
    reference_script: None,
  }
}

fn promotion() -> Transaction {
  let tx = admission(facts())
  Transaction {
    ..tx,
    inputs: [history_input(filler(None, 200)), ..tx.inputs],
    outputs: [order_output(facts(), None, 600), refund()],
  }
}

fn promote(tx: Transaction) -> Bool {
  semantics.promote_filler(tx, params(), 0, 0, 1, 1, None)
}

test equal_key_promotion_requires_no_filler_owner_signature() {
  let tx = promotion()
  and {
    tx.extra_signatories == [],
    promote(tx),
  }
}

test promotion_cannot_keep_filler_funding() fail {
  let tx = promotion()
  promote(
    Transaction {
      ..tx,
      outputs: [
        order_output(facts(), None, 600),
        Output { ..refund(), value: assets.from_lovelace(4_999_999) },
      ],
    },
  )
}

test promotion_cannot_redirect_filler_funding() fail {
  promote(
    Transaction {
      ..promotion(),
      outputs: [
        order_output(facts(), None, 600),
        Output { ..refund(), address: address.from_verification_key(retention) },
      ],
    },
  )
}

test promotion_cannot_overdeclare_structural_funding() fail {
  promote(
    Transaction {
      ..promotion(),
      outputs: [
        order_output(
          Facts { ..facts(), structural_lovelace: 10_000_001 },
          None,
          600,
        ),
        refund(),
      ],
    },
  )
}

test promotion_cannot_backdate() fail {
  promote(
    Transaction {
      ..promotion(),
      outputs: [
        order_output(Facts { ..facts(), inclusion_time: 1 }, None, 600),
        refund(),
      ],
    },
  )
}

test promotion_cannot_rewrite_an_existing_order() fail {
  let tx = promotion()
  expect [_, nonce_input] = tx.inputs
  promote(
    Transaction {
      ..tx,
      inputs: [history_input(order_output(facts(), None, 200)), nonce_input],
    },
  )
}

test promotion_cannot_spend_an_extra_authenticated_node() fail {
  let tx = promotion()
  promote(
    Transaction {
      ..tx,
      inputs: [
        Input {
          output_reference: OutputReference {
            transaction_id: tx_id,
            output_index: 3,
          },
          output: root(None, 0),
        },
        ..tx.inputs
      ],
    },
  )
}

fn insertion() -> Transaction {
  let tx = admission(facts())
  let key = user_events.out_ref_to_nonce(nonce)
  Transaction {
    ..tx,
    inputs: [history_input(root(None, 200)), ..tx.inputs],
    outputs: [root(Some(key), 600), order_output(facts(), None, 600)],
    mint: assets.from_asset(policy, key, 1),
  }
}

fn insert(tx: Transaction) -> Bool {
  semantics.insert_order(tx, params(), 0, 0, 1, 1, None)
}

test insert_order_preserves_root_funds() {
  insert(insertion())
}

test insert_order_cannot_spend_protected_predecessor() fail {
  let tx = insertion()
  expect [_, nonce_input] = tx.inputs
  insert(
    Transaction { ..tx, inputs: [history_input(root(None, 301)), nonce_input] },
  )
}

test insert_order_cannot_skip_mint() fail {
  insert(Transaction { ..insertion(), mint: assets.zero })
}

test insert_order_cannot_claim_filler_as_event() fail {
  let tx = insertion()
  expect [continued, _] = tx.outputs
  insert(Transaction { ..tx, outputs: [continued, filler(None, 600)] })
}

/// A deposit payload around `datum`: the payload adds 12 nodes of its own, so
/// a 1,012-node datum puts it exactly at the 1,024-node bound.
fn datum_facts(datum: Data) -> Facts {
  expect DepositPayload { event } = payload()
  let info = DepositInfo { ..event.info, l2_datum: Some(datum) }
  Facts {
    ..facts(),
    location: Inline {
      payload: DepositPayload { event: DepositEvent { ..event, info } },
    },
  }
}

const at_bound = 1012

fn nested_lists(nodes: Int) -> Data {
  if nodes <= 1 {
    builtin.list_data([])
  } else {
    builtin.list_data([nested_lists(nodes - 1)])
  }
}

fn nested_constructors(nodes: Int) -> Data {
  if nodes <= 1 {
    builtin.constr_data(0, [])
  } else {
    builtin.constr_data(0, [nested_constructors(nodes - 1)])
  }
}

fn flat_list(nodes: Int) -> Data {
  builtin.list_data(list.repeat(builtin.i_data(0), nodes - 1))
}

/// A list around one map of two-node entries, and an integer for odd counts.
fn wide_map(nodes: Int) -> Data {
  let map = builtin.map_data(
    list.repeat(Pair(builtin.i_data(0), builtin.i_data(0)), ( nodes - 2 ) / 2),
  )
  builtin.list_data(
    if nodes % 2 == 0 {
      [map]
    } else {
      [map, builtin.i_data(0)]
    },
  )
}

/// A list of 13-byte strings: the most payload bytes at the node bound.
fn long_bytes(nodes: Int) -> Data {
  builtin.list_data(
    list.repeat(builtin.b_data(#"ababababababababababababab"), nodes - 1),
  )
}

/// The deployed node bound, with an inline limit that holds every shape.
fn bounded() -> semantics.Parameters {
  semantics.Parameters { ..params(), inline_limit: 15000 }
}

/// An Order around `datum`, funded for the largest inline payload.
fn datum_order(datum: Data) -> Output {
  let output = order_output(datum_facts(datum), None, 600)
  Output {
    ..output,
    value: assets.merge(output.value, assets.from_lovelace(90_000_000)),
  }
}

fn insert_datum(datum: Data) -> Bool {
  let tx = insertion()
  expect [continued, _] = tx.outputs
  semantics.insert_order(
    Transaction { ..tx, outputs: [continued, datum_order(datum)] },
    bounded(),
    0,
    0,
    1,
    1,
    None,
  )
}

fn promote_datum(datum: Data) -> Bool {
  let tx = promotion()
  expect [_, refund_output] = tx.outputs
  semantics.promote_filler(
    Transaction { ..tx, outputs: [datum_order(datum), refund_output] },
    bounded(),
    0,
    0,
    1,
    1,
    None,
  )
}

test insert_order_admits_nested_lists_at_the_node_bound() {
  insert_datum(nested_lists(at_bound))
}

test insert_order_admits_nested_constructors_at_the_node_bound() {
  insert_datum(nested_constructors(at_bound))
}

test insert_order_admits_a_flat_list_at_the_node_bound() {
  insert_datum(flat_list(at_bound))
}

test insert_order_admits_a_wide_map_at_the_node_bound() {
  insert_datum(wide_map(at_bound))
}

test insert_order_admits_long_bytes_at_the_node_bound() {
  insert_datum(long_bytes(at_bound))
}

test insert_order_refuses_nested_lists_past_the_node_bound() fail {
  insert_datum(nested_lists(at_bound + 1))
}

test insert_order_refuses_nested_constructors_past_the_node_bound() fail {
  insert_datum(nested_constructors(at_bound + 1))
}

test insert_order_refuses_a_flat_list_past_the_node_bound() fail {
  insert_datum(flat_list(at_bound + 1))
}

test insert_order_refuses_a_wide_map_past_the_node_bound() fail {
  insert_datum(wide_map(at_bound + 1))
}

test insert_order_refuses_long_bytes_past_the_node_bound() fail {
  insert_datum(long_bytes(at_bound + 1))
}

test promotion_admits_nested_lists_at_the_node_bound() {
  promote_datum(nested_lists(at_bound))
}

test promotion_admits_nested_constructors_at_the_node_bound() {
  promote_datum(nested_constructors(at_bound))
}

test promotion_admits_a_flat_list_at_the_node_bound() {
  promote_datum(flat_list(at_bound))
}

test promotion_admits_a_wide_map_at_the_node_bound() {
  promote_datum(wide_map(at_bound))
}

test promotion_admits_long_bytes_at_the_node_bound() {
  promote_datum(long_bytes(at_bound))
}

test promotion_refuses_nested_lists_past_the_node_bound() fail {
  promote_datum(nested_lists(at_bound + 1))
}

test promotion_refuses_nested_constructors_past_the_node_bound() fail {
  promote_datum(nested_constructors(at_bound + 1))
}

test promotion_refuses_a_flat_list_past_the_node_bound() fail {
  promote_datum(flat_list(at_bound + 1))
}

test promotion_refuses_a_wide_map_past_the_node_bound() fail {
  promote_datum(wide_map(at_bound + 1))
}

test promotion_refuses_long_bytes_past_the_node_bound() fail {
  promote_datum(long_bytes(at_bound + 1))
}

/// A filler insertion cannot write Order content, so `admit` is skipped only
/// for nodes that carry no event facts.
test insert_filler_cannot_write_an_order() fail {
  let tx = insertion()
  semantics.insert_filler(tx, params(), 0, 0, 1)
}

test insert_filler_is_permissionless() {
  let tx = insertion()
  expect [continued, _] = tx.outputs
  semantics.insert_filler(
    Transaction { ..tx, outputs: [continued, filler(None, 600)] },
    params(),
    0,
    0,
    1,
  )
}

fn reclamation() -> Transaction {
  let key = user_events.out_ref_to_nonce(nonce)
  Transaction {
    ..placeholder,
    inputs: [
      history_input(root(Some(key), 200)),
      Input {
        output_reference: OutputReference {
          transaction_id: tx_id,
          output_index: 3,
        },
        output: filler(None, 200),
      },
    ],
    outputs: [root(None, 600), refund()],
    mint: assets.from_asset(policy, key, -1),
    validity_range: interval.between(300, 400),
    extra_signatories: [owner],
  }
}

fn reclaim(tx: Transaction) -> Bool {
  semantics.reclaim_filler(tx, params(), 0, 1, 0, 1)
}

test filler_reclaim_returns_all_funding_with_owner_approval() {
  reclaim(reclamation())
}

test standalone_filler_reclaim_requires_owner() fail {
  reclaim(Transaction { ..reclamation(), extra_signatories: [] })
}

test standalone_filler_reclaim_cannot_unlink_order() fail {
  let tx = reclamation()
  expect [predecessor, target] = tx.inputs
  reclaim(
    Transaction {
      ..tx,
      inputs: [
        predecessor,
        Input { ..target, output: order_output(facts(), None, 200) },
      ],
    },
  )
}
