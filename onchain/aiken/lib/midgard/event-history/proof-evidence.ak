//// Current authenticated list evidence for a pending header. A missing Order
//// does not mean its nonce never existed: authorized retirement also leaves
//// absence. Capture after the accused interval and before earliest merge makes
//// both cases evidence that a later header could not legitimately include it.

use aiken/collection/list
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/utils
use midgard/event_history/horizon
use midgard/event_history/node_role
use midgard/event_history/order_facts.{Commitment, Kind}
use midgard/user_events

pub type Parameters {
  retention_address: Address,
  inline_limit: Int,
  max_payload_bytes: Int,
  max_payload_nodes: Int,
}

pub fn absence(
  tx: Transaction,
  policy: PolicyId,
  list_address: Address,
  target: OutputReference,
  header_end: Int,
  history_reference_index: Int,
) -> Bool {
  let (lower, upper) = utils.get_inclusive_bounds_of_a_short_validity_range(
    tx.validity_range,
  )
  expect horizon.capture_permitted(header_end, lower, upper)
  expect Some(input) = list.at(tx.reference_inputs, history_reference_index)
  expect
    !list.any(
      tx.inputs,
      fn(spent) { spent.output_reference == input.output_reference },
    )
  node_role.authenticated_absence(
    input.output,
    policy,
    list_address,
    user_events.out_ref_to_nonce(target),
  )
}

pub fn presence(
  tx: Transaction,
  policy: PolicyId,
  list_address: Address,
  kind: Kind,
  target: OutputReference,
  header_end: Int,
  params: Parameters,
  event_reference_index: Int,
  external_reference_index: Option<Int>,
) -> Commitment {
  order_facts.capture(
    tx,
    policy,
    list_address,
    params.retention_address,
    kind,
    target,
    header_end,
    params.inline_limit,
    params.max_payload_bytes,
    params.max_payload_nodes,
    event_reference_index,
    external_reference_index,
  )
}
