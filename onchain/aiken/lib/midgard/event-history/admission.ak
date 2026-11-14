//// Semantic admission composes the structural list transaction with role,
//// nonce, timing, payload and funding checks. One invocation covers the exact
//// consumed list-input set; a future observer must not combine invocations.

use aiken/collection/list
use aiken/primitive/bytearray
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/event_history/funding
use midgard/event_history/mutation
use midgard/event_history/node_role.{Filler, RootContent}
use midgard/event_history/order_facts.{Kind}
use midgard/event_history/ordering.{Key}

pub type Parameters {
  policy: PolicyId,
  address: Address,
  retention_address: Address,
  kind: Kind,
  protection_duration: Int,
  inline_limit: Int,
  max_payload_bytes: Int,
  max_payload_nodes: Int,
}

pub fn initialize(
  tx: Transaction,
  params: Parameters,
  one_shot: OutputReference,
  nonce_input_index: Int,
  root_output_index: Int,
) -> Bool {
  let node = mutation.initialize(
    tx,
    params.policy,
    params.address,
    params.protection_duration,
    one_shot,
    nonce_input_index,
    root_output_index,
  )
  expect role: node_role.Content<Data> = node.payload
  expect Some(output) = list.at(tx.outputs, root_output_index)
  and {
    role == RootContent,
    funding.valid_filler_value(output.value, params.policy, #""),
    funding.node_is_funded(output.value, node),
  }
}

pub fn insert_filler(
  tx: Transaction,
  params: Parameters,
  predecessor_input_index: Int,
  predecessor_output_index: Int,
  filler_output_index: Int,
) -> Bool {
  let node = mutation.insert(
    tx,
    params.policy,
    params.address,
    params.protection_duration,
    predecessor_input_index,
    predecessor_output_index,
    filler_output_index,
  )
  expect Key(key) = node.position
  expect Filler { refund_key }: node_role.Content<Data> = node.payload
  expect Some(output) = list.at(tx.outputs, filler_output_index)
  and {
    bytearray.length(refund_key) == 28,
    funding.valid_filler_value(output.value, params.policy, key),
    funding.node_is_funded(output.value, node),
  }
}

pub fn insert_order(
  tx: Transaction,
  params: Parameters,
  predecessor_input_index: Int,
  predecessor_output_index: Int,
  order_output_index: Int,
  nonce_input_index: Int,
  external_reference_index: Option<Int>,
) -> Bool {
  let node = mutation.insert(
    tx,
    params.policy,
    params.address,
    params.protection_duration,
    predecessor_input_index,
    predecessor_output_index,
    order_output_index,
  )
  let (facts, _, _) = order_facts.admit(
    params.kind,
    tx,
    params.policy,
    params.address,
    params.retention_address,
    params.inline_limit,
    params.max_payload_bytes,
    params.max_payload_nodes,
    order_output_index,
    nonce_input_index,
    external_reference_index,
  )
  // Force both structural and semantic validation even though they read the
  // same output; an unused lazy result would omit its validation entirely.
  order_facts.from_node(node) == facts
}

pub fn promote_filler(
  tx: Transaction,
  params: Parameters,
  filler_input_index: Int,
  order_output_index: Int,
  refund_output_index: Int,
  nonce_input_index: Int,
  external_reference_index: Option<Int>,
) -> Bool {
  let (before, after) = mutation.replace(
    tx,
    params.policy,
    params.address,
    params.protection_duration,
    filler_input_index,
    order_output_index,
  )
  expect Key(key) = before.position
  expect Filler { refund_key }: node_role.Content<Data> = before.payload
  expect Some(input) = list.at(tx.inputs, filler_input_index)
  expect Some(refund) = list.at(tx.outputs, refund_output_index)
  expect refund_output_index != order_output_index
  let (facts, _, _) = order_facts.admit(
    params.kind,
    tx,
    params.policy,
    params.address,
    params.retention_address,
    params.inline_limit,
    params.max_payload_bytes,
    params.max_payload_nodes,
    order_output_index,
    nonce_input_index,
    external_reference_index,
  )
  and {
    order_facts.from_node(after) == facts,
    funding.pays_filler_refund(
      input.output.value,
      params.policy,
      key,
      refund_key,
      refund,
    ),
  }
}

/// Standalone filler removal requires its recorded key's approval and returns
/// all funding. Promotion above deliberately has no corresponding signer gate.
pub fn reclaim_filler(
  tx: Transaction,
  params: Parameters,
  predecessor_input_index: Int,
  filler_input_index: Int,
  predecessor_output_index: Int,
  refund_output_index: Int,
) -> Bool {
  let node = mutation.remove(
    tx,
    params.policy,
    params.address,
    params.protection_duration,
    predecessor_input_index,
    filler_input_index,
    predecessor_output_index,
  )
  expect Key(key) = node.position
  expect Filler { refund_key }: node_role.Content<Data> = node.payload
  expect Some(input) = list.at(tx.inputs, filler_input_index)
  expect Some(refund) = list.at(tx.outputs, refund_output_index)
  and {
    predecessor_output_index != refund_output_index,
    list.has(tx.extra_signatories, refund_key),
    funding.pays_filler_refund(
      input.output.value,
      params.policy,
      key,
      refund_key,
      refund,
    ),
  }
}
