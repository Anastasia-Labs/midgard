use aiken/interval
use cardano/address
use cardano/assets
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction, placeholder,
}
use midgard/event_history/mutation
use midgard/event_history/ordering.{Key, Link, Root}

const policy = #"11111111111111111111111111111111111111111111111111111111"

const tx_id =
  #"2222222222222222222222222222222222222222222222222222222222222222"

const low = #"0000000000000000000000000000000000000000000000000000000000000000"

const middle =
  #"8080808080808080808080808080808080808080808080808080808080808080"

const high = #"ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"

fn node(
  position: ordering.Position,
  next: Option<ByteArray>,
  protection: Int,
) -> Link<Data> {
  let payload: Data = 42
  Link { position, next, protected_until: protection, payload }
}

fn output(link: Link<Data>) -> Output {
  let raw: Data = link
  Output {
    address: address.from_script(policy),
    value: assets.from_lovelace(3_000_000)
      |> assets.add(policy, ordering.token_name(link.position), 1),
    datum: InlineDatum(raw),
    reference_script: None,
  }
}

fn input(link: Link<Data>, index: Int) -> Input {
  Input {
    output_reference: OutputReference {
      transaction_id: tx_id,
      output_index: index,
    },
    output: output(link),
  }
}

fn initialization() -> Transaction {
  Transaction {
    ..placeholder,
    inputs: [
      Input {
        output_reference: OutputReference {
          transaction_id: tx_id,
          output_index: 999,
        },
        output: Output {
          ..output(node(Root, None, 0)),
          address: address.from_verification_key(policy),
          value: assets.from_lovelace(10_000_000),
        },
      },
    ],
    outputs: [output(node(Root, None, 600))],
    mint: assets.from_asset(policy, #"", 1),
    validity_range: interval.between(300, 400),
  }
}

fn initialize(tx: Transaction) -> Bool {
  expect _ = mutation.initialize(
    tx,
    policy,
    address.from_script(policy),
    200,
    OutputReference { transaction_id: tx_id, output_index: 999 },
    0,
    0,
  )
  True
}

test initialize_exactly_one_empty_authenticated_root() {
  initialize(initialization())
}

test initialization_cannot_choose_another_nonce() fail {
  let tx = initialization()
  expect [nonce] = tx.inputs
  initialize(
    Transaction {
      ..tx,
      inputs: [
        Input {
          ..nonce,
          output_reference: OutputReference {
            transaction_id: tx_id,
            output_index: 998,
          },
        },
      ],
    },
  )
}

test initialization_cannot_mint_two_roots() fail {
  initialize(
    Transaction { ..initialization(), mint: assets.from_asset(policy, #"", 2) },
  )
}

test initialization_cannot_point_at_a_detached_node() fail {
  initialize(
    Transaction {
      ..initialization(),
      outputs: [output(node(Root, Some(high), 600))],
    },
  )
}

test initialization_cannot_mint_a_detached_node() fail {
  let tx = initialization()
  initialize(
    Transaction { ..tx, mint: tx.mint |> assets.add(policy, middle, 1) },
  )
}

fn insertion() -> Transaction {
  Transaction {
    ..placeholder,
    inputs: [input(node(Root, Some(high), 300), 0)],
    outputs: [
      output(node(Root, Some(middle), 600)),
      output(node(Key(middle), Some(high), 600)),
    ],
    mint: assets.from_asset(policy, middle, 1),
    validity_range: interval.between(300, 400),
  }
}

fn insert(tx: Transaction) -> Bool {
  expect _ = mutation.insert(
    tx,
    policy,
    address.from_script(policy),
    200,
    0,
    0,
    1,
  )
  True
}

test insertion_preserves_adjacency_and_authentication() {
  insert(insertion())
}

test insertion_does_not_require_root_for_a_nonfirst_gap() {
  let tx = insertion()
  insert(
    Transaction {
      ..tx,
      inputs: [input(node(Key(low), Some(high), 300), 0)],
      outputs: [
        output(node(Key(low), Some(middle), 600)),
        output(node(Key(middle), Some(high), 600)),
      ],
    },
  )
}

test disconnected_insertion_rejects() fail {
  let tx = insertion()
  insert(
    Transaction {
      ..tx,
      outputs: [
        output(node(Root, Some(high), 600)),
        output(node(Key(middle), Some(high), 600)),
      ],
    },
  )
}

test insertion_cannot_skip_successor() fail {
  let tx = insertion()
  insert(
    Transaction {
      ..tx,
      outputs: [
        output(node(Root, Some(middle), 600)),
        output(node(Key(middle), None, 600)),
      ],
    },
  )
}

test insertion_cannot_claim_equal_key_gap() fail {
  let tx = insertion()
  insert(
    Transaction { ..tx, inputs: [input(node(Key(middle), Some(high), 300), 0)] },
  )
}

test insertion_cannot_remove_predecessor_funds() fail {
  let tx = insertion()
  let pred = output(node(Root, Some(middle), 600))
  insert(
    Transaction {
      ..tx,
      outputs: [
        Output { ..pred, value: pred.value |> assets.add(#"", #"", -1) },
        output(node(Key(middle), Some(high), 600)),
      ],
    },
  )
}

test insertion_cannot_satisfy_another_input() fail {
  let tx = insertion()
  insert(
    Transaction {
      ..tx,
      inputs: [
        input(node(Root, Some(high), 300), 0),
        input(node(Key(high), None, 300), 1),
      ],
    },
  )
}

test insertion_cannot_mint_an_extra_detached_node() fail {
  let tx = insertion()
  insert(Transaction { ..tx, mint: tx.mint |> assets.add(policy, low, 1) })
}

test insertion_requires_the_expected_mint() fail {
  insert(Transaction { ..insertion(), mint: assets.zero })
}

test insertion_during_protected_capture_rejects() fail {
  insert(
    Transaction { ..insertion(), validity_range: interval.between(299, 400) },
  )
}

test insertion_must_protect_new_node() fail {
  let tx = insertion()
  insert(
    Transaction {
      ..tx,
      outputs: [
        output(node(Root, Some(middle), 600)),
        output(node(Key(middle), Some(high), 599)),
      ],
    },
  )
}

test insertion_must_protect_rewritten_predecessor() fail {
  let tx = insertion()
  insert(
    Transaction {
      ..tx,
      outputs: [
        output(node(Root, Some(middle), 599)),
        output(node(Key(middle), Some(high), 600)),
      ],
    },
  )
}

fn replacement() -> Transaction {
  Transaction {
    ..placeholder,
    inputs: [input(node(Key(middle), Some(high), 300), 1)],
    outputs: [output(node(Key(middle), Some(high), 600))],
    validity_range: interval.between(300, 400),
  }
}

fn replace(tx: Transaction) -> Bool {
  expect _ = mutation.replace(
    tx,
    policy,
    address.from_script(policy),
    200,
    0,
    0,
  )
  True
}

test replacement_needs_no_predecessor_input() {
  replace(replacement())
}

test replacement_cannot_change_key() fail {
  replace(
    Transaction {
      ..replacement(),
      outputs: [output(node(Key(low), Some(high), 600))],
    },
  )
}

test replacement_cannot_change_successor() fail {
  replace(
    Transaction {
      ..replacement(),
      outputs: [output(node(Key(middle), None, 600))],
    },
  )
}

test root_cannot_be_promoted_to_an_order() fail {
  let tx = replacement()
  replace(
    Transaction {
      ..tx,
      inputs: [input(node(Root, Some(high), 300), 0)],
      outputs: [output(node(Root, Some(high), 600))],
    },
  )
}

test replacement_cannot_carry_extra_policy_mint() fail {
  replace(
    Transaction { ..replacement(), mint: assets.from_asset(policy, low, 1) },
  )
}

test replacement_must_respect_current_capture_protection() fail {
  replace(
    Transaction { ..replacement(), validity_range: interval.between(299, 400) },
  )
}

fn removal() -> Transaction {
  Transaction {
    ..placeholder,
    inputs: [
      input(node(Root, Some(middle), 300), 0),
      input(node(Key(middle), Some(high), 300), 1),
    ],
    outputs: [output(node(Root, Some(high), 600))],
    mint: assets.from_asset(policy, middle, -1),
    validity_range: interval.between(300, 400),
  }
}

fn remove(tx: Transaction) -> Bool {
  expect _ = mutation.remove(
    tx,
    policy,
    address.from_script(policy),
    200,
    0,
    1,
    0,
  )
  True
}

test removal_reconnects_predecessor_to_successor() {
  remove(removal())
}

test removal_cannot_unlink_a_detached_node() fail {
  let tx = removal()
  remove(
    Transaction {
      ..tx,
      inputs: [
        input(node(Root, Some(high), 300), 0),
        input(node(Key(middle), Some(high), 300), 1),
      ],
    },
  )
}

test removal_cannot_drop_the_successor() fail {
  remove(Transaction { ..removal(), outputs: [output(node(Root, None, 600))] })
}

test removal_requires_exact_node_burn() fail {
  remove(Transaction { ..removal(), mint: assets.zero })
}

test removal_cannot_consume_an_unaccounted_order() fail {
  let tx = removal()
  remove(
    Transaction {
      ..tx,
      inputs: [
        input(node(Root, Some(middle), 300), 0),
        input(node(Key(middle), Some(high), 300), 1),
        input(node(Key(high), None, 300), 2),
      ],
    },
  )
}

test removal_cannot_spend_a_protected_order() fail {
  let tx = removal()
  remove(
    Transaction {
      ..tx,
      inputs: [
        input(node(Root, Some(middle), 300), 0),
        input(node(Key(middle), Some(high), 301), 1),
      ],
    },
  )
}

test removed_node_and_predecessor_must_be_distinct() fail {
  expect _ = mutation.remove(
    removal(),
    policy,
    address.from_script(policy),
    200,
    0,
    0,
    0,
  )
  True
}
