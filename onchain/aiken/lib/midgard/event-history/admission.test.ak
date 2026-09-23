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
