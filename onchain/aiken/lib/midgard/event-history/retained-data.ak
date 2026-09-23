//// Authentication of prepublished event data. The retention address must be
//// pinned to the deployment AND event kind by the caller's applied script.
//// Publication is not admission. The datum contains no admission timestamp
//// or future publication output reference.

use aiken/cbor
use aiken/collection/list
use aiken/crypto.{VerificationKeyHash, blake2b_256}
use cardano/address.{Address, Credential, Script, VerificationKey}
use cardano/transaction.{InlineDatum, Input, Output}
use midgard/event_history/data_budget
use midgard/event_history/ordering

pub type EventData {
  event_key: ByteArray,
  event_payload: Data,
  reclaim_auth: Credential,
}

pub fn datum_hash(data: EventData) -> ByteArray {
  cbor.serialise(data) |> blake2b_256
}

/// Only the actual reference-input set can provide this evidence. A redeemer
/// opening or newly produced output is never an alternative publication path.
/// Copies with the same complete datum at the correct address are equivalent.
pub fn authenticate(
  reference_inputs: List<Input>,
  spent_inputs: List<Input>,
  index: Int,
  retention_address: Address,
  event_key: ByteArray,
  expected_hash: ByteArray,
  max_payload_nodes: Int,
) -> EventData {
  expect ordering.valid_key(event_key)
  expect Some(input) = list.at(reference_inputs, index)
  expect
    !list.any(
      spent_inputs,
      fn(spent) { spent.output_reference == input.output_reference },
    )
  expect Output { address, datum: InlineDatum(raw), reference_script: None, .. }
    = input.output
  expect address == retention_address
  expect data: EventData = raw
  expect data.event_key == event_key
  expect data_budget.within_nodes(data.event_payload, max_payload_nodes)
  expect datum_hash(data) == expected_hash
  data
}

/// Owner authorization is only one conjunct of reclamation. The retention
/// spending validator must ALSO authenticate order absence in its pinned list.
/// For scripts, the ledger executes the exact rewarding purpose identified by
/// this zero withdrawal; a spending redeemer or supplied script is insufficient.
pub fn owner_authorized(
  credential: Credential,
  extra_signatories: List<VerificationKeyHash>,
  withdrawals: Pairs<Credential, Int>,
) -> Bool {
  when credential is {
    VerificationKey(key) -> list.has(extra_signatories, key)
    Script(_) -> list.has(withdrawals, Pair(credential, 0))
  }
}
