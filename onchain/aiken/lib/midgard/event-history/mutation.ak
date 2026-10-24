//// Structural mutations for one authenticated history list. Each operation
//// covers all inputs at the list address, with exact policy minting. The
//// event-specific observer must additionally validate node roles, admission,
//// original assets, inline/external carriage, filler refunds and retirement.
//// These functions deliberately do not authorize those semantic transitions.

use aiken/collection/dict
use aiken/collection/list
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{Input, Output, OutputReference, Transaction}
use midgard/common/utils
use midgard/event_history/horizon
use midgard/event_history/ordering.{Key, Link, Root}

fn input_at(tx: Transaction, index: Int) -> Input {
  expect Some(input) = list.at(tx.inputs, index)
  input
}

fn output_at(tx: Transaction, index: Int) -> Output {
  expect Some(output) = list.at(tx.outputs, index)
  output
}

/// No single observer invocation can be reused to spend another list input.
/// Order is irrelevant; references must be distinct and the exact set of
/// consumed inputs at this deployment address.
fn exact_inputs(
  tx: Transaction,
  address: Address,
  refs: List<OutputReference>,
) -> Bool {
  let actual = tx.inputs
    |> list.filter(fn(input) { input.output.address == address })
    |> list.map(fn(input) { input.output_reference })
  and {
    list.length(actual) == list.length(refs),
    list.length(list.unique(refs)) == list.length(refs),
    list.all(refs, fn(reference) { list.has(actual, reference) }),
  }
}

fn protection(tx: Transaction, duration: Int) -> (Int, Int, Int) {
  let (lower, upper) = utils.get_inclusive_bounds_of_a_short_validity_range(
    tx.validity_range,
  )
  // Deployment application fixes and validates the positive duration.
  expect lower >= 0
  (lower, upper, upper + duration)
}

/// One-shot initialization creates exactly the empty root. The one-shot is a
/// deployment parameter shared with initialization, not a redeemer-selected
/// nonce. Subsequent calls cannot consume it again. Root payload and funding
/// are checked by the event-specific observer.
pub fn initialize(
  tx: Transaction,
  policy: PolicyId,
  address: Address,
  duration: Int,
  one_shot: OutputReference,
  nonce_input_index: Int,
  root_output_index: Int,
) -> Link<Data> {
  expect input_at(tx, nonce_input_index).output_reference == one_shot
  expect exact_inputs(tx, address, [])
  let root = ordering.authenticate(
    output_at(tx, root_output_index),
    policy,
    address,
  )
  let (_, _, protected_until) = protection(tx, duration)
  expect root.position == Root
  expect root.next == None
  expect root.protected_until == protected_until
  let mint = tx.mint |> assets.tokens(policy) |> dict.to_pairs
  expect mint == [Pair(#"", 1)]
  root
}

/// Returns the authenticated inserted node for event/filler admission checks.
pub fn insert(
  tx: Transaction,
  policy: PolicyId,
  address: Address,
  duration: Int,
  predecessor_input_index: Int,
  predecessor_output_index: Int,
  new_output_index: Int,
) -> Link<Data> {
  let input = input_at(tx, predecessor_input_index)
  let continued = output_at(tx, predecessor_output_index)
  let added = output_at(tx, new_output_index)
  let predecessor = ordering.authenticate(input.output, policy, address)
  let node = ordering.authenticate(added, policy, address)
  expect Key(key) = node.position
  let (lower, upper, protected_until) = protection(tx, duration)
  expect horizon.mutation_permitted(predecessor.protected_until, lower, upper)
  expect exact_inputs(tx, address, [input.output_reference])
  expect predecessor_output_index != new_output_index
  let mint = tx.mint |> assets.tokens(policy) |> dict.to_pairs
  expect mint == [Pair(key, 1)]
  expect ordering.contains_gap(predecessor, key)
  expect node.next == predecessor.next
  expect node.protected_until == protected_until
  expect
    ordering.preserves_predecessor(
      input.output,
      continued,
      policy,
      address,
      Some(key),
      protected_until,
    )
  node
}

/// Equal-key replacement preserves adjacency without spending a predecessor.
/// Caller MUST prove the input is Filler and the output is a genuine Order,
/// and pay the exact bounded filler refund without owner authorization.
pub fn replace(
  tx: Transaction,
  policy: PolicyId,
  address: Address,
  duration: Int,
  input_index: Int,
  output_index: Int,
) -> (Link<Data>, Link<Data>) {
  let input = input_at(tx, input_index)
  let output = output_at(tx, output_index)
  let before = ordering.authenticate(input.output, policy, address)
  let after = ordering.authenticate(output, policy, address)
  expect Key(_) = before.position
  let (lower, upper, protected_until) = protection(tx, duration)
  expect horizon.mutation_permitted(before.protected_until, lower, upper)
  expect exact_inputs(tx, address, [input.output_reference])
  expect dict.is_empty(assets.tokens(tx.mint, policy))
  expect after.position == before.position
  expect after.next == before.next
  expect after.protected_until == protected_until
  (before, after)
}

/// Returns the removed authenticated node. Caller MUST authorize its semantic
/// retirement/refund and independently authenticate the finalized frontier for
/// Orders. No root burn, detached removal or unrelated input is admitted here.
pub fn remove(
  tx: Transaction,
  policy: PolicyId,
  address: Address,
  duration: Int,
  predecessor_input_index: Int,
  removed_input_index: Int,
  predecessor_output_index: Int,
) -> Link<Data> {
  let predecessor_input = input_at(tx, predecessor_input_index)
  let removed_input = input_at(tx, removed_input_index)
  let continued = output_at(tx, predecessor_output_index)
  let predecessor = ordering.authenticate(
    predecessor_input.output,
    policy,
    address,
  )
  let removed = ordering.authenticate(removed_input.output, policy, address)
  expect Key(key) = removed.position
  let (lower, upper, protected_until) = protection(tx, duration)
  expect horizon.mutation_permitted(predecessor.protected_until, lower, upper)
  expect horizon.mutation_permitted(removed.protected_until, lower, upper)
  expect
    exact_inputs(
      tx,
      address,
      [predecessor_input.output_reference, removed_input.output_reference],
    )
  expect predecessor.next == Some(key)
  let mint = tx.mint |> assets.tokens(policy) |> dict.to_pairs
  expect mint == [Pair(key, -1)]
  expect
    ordering.preserves_predecessor(
      predecessor_input.output,
      continued,
      policy,
      address,
      removed.next,
      protected_until,
    )
  removed
}
