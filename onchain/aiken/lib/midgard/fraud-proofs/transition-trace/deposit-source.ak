use aiken/cbor
use midgard/event_history/order_facts
use midgard/fraud_proofs/transition_trace/final_yield.{Args, State}
use midgard/ledger_state

pub type Source {
  commitment: order_facts.Commitment,
  event: ledger_state.DepositEvent,
  value: Data,
}

/// The first projection stage captured these facts from the authenticated list.
/// Subsequent stages need neither the original out-ref nor any current node.
pub fn open(state: State, args: Args) -> Source {
  expect Some(data) = cbor.deserialise(state.deposit_source_cbor)
  expect commitment: order_facts.Commitment = data
  expect commitment.kind == order_facts.Deposit
  expect Some(raw_opening) = args.deposit_opening
  expect opening: order_facts.Opening = raw_opening
  expect order_facts.opens(commitment, opening.payload, opening.original_assets)
  expect order_facts.DepositPayload { event } = opening.payload
  // The complete hash was captured from a canonical L1 Value. Keep that exact
  // Data here: rebuilding and validating every token at every four-asset step
  // exceeds the maximum-Value reopening budget. Consumers inspect only the
  // authenticated entries they use; this is not an unchecked external Value.
  Source { commitment, event, value: opening.original_assets }
}
