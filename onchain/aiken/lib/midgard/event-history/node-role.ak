//// Role decoding is separate from ordering: a structural token authenticates
//// a list node, but only an Order can authenticate an event. The immutable
//// order facts remain typed by the event-specific admission/proof consumer.

use aiken/crypto.{VerificationKeyHash}
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{Output}
use midgard/event_history/ordering.{Key, Root}

pub type Content<facts> {
  RootContent
  Filler { refund_key: VerificationKeyHash }
  Order { facts: facts }
}

/// Proves absence of an Order, not that an ID never existed. Fraud-proof
/// callers must additionally enforce capture horizon, admission no-backdating
/// and the finalized-retirement invariant. Reclamation needs current absence
/// plus exact owner authorization and has no historical claim.
pub fn authenticated_absence(
  output: Output,
  policy: PolicyId,
  address: Address,
  target: ByteArray,
) -> Bool {
  expect ordering.valid_key(target)
  let link = ordering.authenticate(output, policy, address)
  expect content: Content<Data> = link.payload
  // Keep this Boolean guard in the calling scope. A decoded value whose use
  // only occurs on the equal-key branch does not force its helper's checks on
  // the gap branch under Aiken's lazy bindings.
  expect
    when link.position is {
      Root -> content == RootContent
      Key(_) -> content != RootContent
    }
  if ordering.contains_gap(link, target) {
    True
  } else {
    when content is {
      Filler { .. } -> link.position == Key(target)
      _ -> False
    }
  }
}
