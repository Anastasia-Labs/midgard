//// The active-operators spend for a defense's bond hold, driven through the
//// deployed validator: the operator signs, the node keeps its lovelace and
//// strikes, and the hold only grows. The case validator fixes the new hold in
//// the same transaction.

use aiken/interval
use aiken/primitive/bytearray
use aiken_design_patterns/linked_list
use cardano/address
use cardano/assets
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction,
}
use midgard/operator_directory/active_operators.{
  NodeData, UpdateBondHoldNewDefense,
}
use operator_directory/active_operators as active_operators_validator

const active_policy =
  #"a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1"

const active_script =
  #"b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1"

const hub_policy = #"c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0"

const operator = #"e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3"

const other_key = #"e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2"

const node_lovelace = 50_000_000_000

const old_hold = 5_000_000

const node_out_ref =
  OutputReference {
    transaction_id: #"2222222222222222222222222222222222222222222222222222222222222222",
    output_index: 3,
  }

fn node(hold: Option<Int>, strikes: Int, lovelace: Int) -> Output {
  let datum: active_operators.Datum = linked_list.Element {
    data: linked_list.Node(
      NodeData { bond_unlock_time: hold, inactivity_strikes: strikes },
    ),
    link: None,
  }
  Output {
    address: address.from_script(active_script),
    value: assets.from_lovelace(lovelace)
      |> assets.add(
          active_policy,
          bytearray.concat(active_operators.node_asset_name_prefix, operator),
          1,
        ),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn hold_tx(
  old: Option<Int>,
  continued: Output,
  signers: List<ByteArray>,
) -> Transaction {
  Transaction {
    ..transaction.placeholder,
    inputs: [
      Input {
        output_reference: node_out_ref,
        output: node(old, 1, node_lovelace),
      },
    ],
    outputs: [continued],
    validity_range: interval.between(1_000, 2_000),
    extra_signatories: signers,
  }
}

fn holds_for(key: ByteArray, tx: Transaction) -> Bool {
  active_operators_validator.spend.spend(
    active_policy,
    hub_policy,
    None,
    UpdateBondHoldNewDefense {
      active_operator: key,
      active_node_input_index: 0,
      active_node_output_index: 0,
    },
    node_out_ref,
    tx,
  )
}

fn holds(tx: Transaction) -> Bool {
  holds_for(operator, tx)
}

test defense_hold_sets_a_first_hold() {
  holds(hold_tx(None, node(Some(old_hold), 1, node_lovelace), [operator]))
}

test defense_hold_raises_a_hold() {
  holds(
    hold_tx(
      Some(old_hold),
      node(Some(old_hold + 1), 1, node_lovelace),
      [operator],
    ),
  )
}

test defense_hold_may_keep_a_hold() {
  holds(
    hold_tx(Some(old_hold), node(Some(old_hold), 1, node_lovelace), [operator]),
  )
}

test defense_hold_shortening_a_hold_is_refused() fail {
  holds(
    hold_tx(
      Some(old_hold),
      node(Some(old_hold - 1), 1, node_lovelace),
      [operator],
    ),
  )
}

test defense_hold_clearing_a_hold_is_refused() fail {
  holds(hold_tx(Some(old_hold), node(None, 1, node_lovelace), [operator]))
}

test defense_hold_left_unset_is_refused() fail {
  holds(hold_tx(None, node(None, 1, node_lovelace), [operator]))
}

test defense_hold_unsigned_is_refused() fail {
  holds(hold_tx(None, node(Some(old_hold), 1, node_lovelace), [other_key]))
}

test defense_hold_for_another_operator_is_refused() fail {
  holds_for(
    other_key,
    hold_tx(None, node(Some(old_hold), 1, node_lovelace), [operator, other_key]),
  )
}

test defense_hold_taking_lovelace_from_the_node_is_refused() fail {
  holds(hold_tx(None, node(Some(old_hold), 1, node_lovelace - 1), [operator]))
}

test defense_hold_adding_lovelace_to_the_node_is_refused() fail {
  holds(hold_tx(None, node(Some(old_hold), 1, node_lovelace + 1), [operator]))
}

test defense_hold_changing_the_strikes_is_refused() fail {
  holds(hold_tx(None, node(Some(old_hold), 0, node_lovelace), [operator]))
}
