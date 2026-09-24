//// Immutable event facts. Admission time is outside the prepublished payload,
//// so a retry may change its validity bounds without republishing user data.
//// Withdrawal validity in the submitted payload is the initial marker; an L2
//// operator's later classification is never substituted into these facts.

use aiken/cbor
use aiken/collection/list
use aiken/crypto.{VerificationKeyHash, blake2b_256}
use aiken/primitive/bytearray
use cardano/address.{Address}
use cardano/assets.{PolicyId, Value}
use cardano/transaction.{Datum, Input, OutputReference, Transaction}
use env
use midgard/common/utils
use midgard/event_history/data_budget
use midgard/event_history/funding
use midgard/event_history/horizon
use midgard/event_history/node_role.{Order}
use midgard/event_history/ordering.{Key, Link}
use midgard/event_history/retained_data
use midgard/ledger_state
use midgard/user_events

pub type Kind {
  Deposit
  Withdrawal
}

pub type Payload {
  DepositPayload { event: ledger_state.DepositEvent }
  WithdrawalPayload {
    event: ledger_state.WithdrawalEvent,
    refund_address: Address,
    refund_datum: Datum,
  }
}

pub type Location {
  Inline { payload: Payload }
  External { storage_datum_hash: ByteArray }
}

pub type Facts {
  event_id: OutputReference,
  inclusion_time: Int,
  location: Location,
  structural_lovelace: Int,
  structural_refund_key: VerificationKeyHash,
}

pub fn from_node(node: Link<Data>) -> Facts {
  expect Key(key) = node.position
  expect Order { facts: raw }: node_role.Content<Data> = node.payload
  expect facts: Facts = raw
  expect user_events.out_ref_to_nonce(facts.event_id) == key
  facts
}

pub fn payload_id(payload: Payload) -> OutputReference {
  when payload is {
    DepositPayload { event } -> event.id
    WithdrawalPayload { event, .. } -> event.id
  }
}

/// The inline limit is supplied by the measured deployment recipe. It bounds
/// serialized Plutus Data, not JSON, and is not inferred from initial tx fit.
pub fn open_payload(
  facts: Facts,
  tx: Transaction,
  retention_address: Address,
  inline_limit: Int,
  max_payload_bytes: Int,
  max_payload_nodes: Int,
  external_reference_index: Option<Int>,
) -> Payload {
  let payload = when facts.location is {
    Inline { payload } -> {
      expect external_reference_index == None
      expect data_budget.within_nodes(payload, max_payload_nodes)
      expect bytearray.length(cbor.serialise(payload)) <= inline_limit
      payload
    }
    External { storage_datum_hash } -> {
      expect Some(index) = external_reference_index
      let data = retained_data.authenticate(
        tx.reference_inputs,
        tx.inputs,
        index,
        retention_address,
        user_events.out_ref_to_nonce(facts.event_id),
        storage_datum_hash,
        max_payload_nodes,
      )
      expect payload: Payload = data.event_payload
      payload
    }
  }
  expect bytearray.length(cbor.serialise(payload)) <= max_payload_bytes
  expect payload_id(payload) == facts.event_id
  payload
}

/// Called after structural insertion/promotion has authenticated the output.
/// Actual assets are derived from that output, never from a claimed payload.
pub fn admit(
  kind: Kind,
  tx: Transaction,
  policy: PolicyId,
  list_address: Address,
  retention_address: Address,
  inline_limit: Int,
  max_payload_bytes: Int,
  max_payload_nodes: Int,
  order_output_index: Int,
  nonce_input_index: Int,
  external_reference_index: Option<Int>,
) -> (Facts, Payload, Value) {
  expect Some(output) = list.at(tx.outputs, order_output_index)
  let node = ordering.authenticate(output, policy, list_address)
  let facts = from_node(node)
  expect funding.node_is_funded(output.value, node)
  expect Some(Input { output_reference, .. }) = list.at(
    tx.inputs,
    nonce_input_index,
  )
  expect facts.event_id == output_reference
  let upper = utils.get_inclusive_upper_bound_of_interval(tx.validity_range)
  expect facts.inclusion_time == upper + env.event_wait_duration
  expect bytearray.length(facts.structural_refund_key) == 28
  let original_assets = funding.order_assets(
    output.value,
    policy,
    user_events.out_ref_to_nonce(facts.event_id),
    facts.structural_lovelace,
  )
  let payload = open_payload(
    facts,
    tx,
    retention_address,
    inline_limit,
    max_payload_bytes,
    max_payload_nodes,
    external_reference_index,
  )
  expect
    when kind is {
      Deposit -> {
        expect DepositPayload { .. } = payload
        // Preserve the existing deposit limit, including ADA when present.
        and {
          list.length(assets.flatten(original_assets)) <= env.max_tokens_allowed_in_deposits,
          funding.deposit_is_funded(original_assets),
        }
      }
      Withdrawal -> {
        expect WithdrawalPayload { event, refund_datum, .. } = payload
        let expected_value = assets.from_lovelace(
          assets.lovelace_of(output.value),
        )
          |> assets.add(policy, user_events.out_ref_to_nonce(facts.event_id), 1)
        and {
          facts.structural_lovelace == 0,
          output.value == expected_value,
          event.info.validity == ledger_state.WithdrawalIsValid,
          funding.withdrawal_is_funded(
            original_assets,
            event.info.body,
            refund_datum,
          ),
        }
      }
    }
  (facts, payload, original_assets)
}

/// A stage retains these separately typed commitments after authenticating a
/// current Order. Subsequent pointer rewrites need not remain referenceable.
/// The caller also binds its challenged header/leaf and capture time. This is
/// not a replacement for the L2 event-leaf or complete storage-datum encoding.
pub type Commitment {
  policy: PolicyId,
  kind: Kind,
  event_id: OutputReference,
  inclusion_time: Int,
  payload_hash: ByteArray,
  original_assets_hash: ByteArray,
}

pub fn commitment(
  policy: PolicyId,
  kind: Kind,
  facts: Facts,
  payload: Payload,
  original_assets: Value,
) -> Commitment {
  Commitment {
    policy,
    kind,
    event_id: facts.event_id,
    inclusion_time: facts.inclusion_time,
    payload_hash: cbor.serialise(payload) |> blake2b_256,
    original_assets_hash: cbor.serialise(original_assets) |> blake2b_256,
  }
}

pub fn matches_kind(payload: Payload, kind: Kind) -> Bool {
  when payload is {
    DepositPayload { .. } -> kind == Deposit
    WithdrawalPayload { .. } -> kind == Withdrawal
  }
}

/// An operator classifies a submitted withdrawal in the L2 leaf. That verdict
/// is checked by the validity proof family, not treated as fabricated content.
pub fn withdrawal_info_matches(
  payload: Payload,
  committed: ledger_state.WithdrawalInfo,
) -> Bool {
  expect WithdrawalPayload { event, .. } = payload
  and {
    event.info.body == committed.body,
    event.info.signature == committed.signature,
  }
}

/// Initial presence capture authenticates the current output and actual Value.
/// A pointer-only continuation may change this output reference afterward.
pub fn capture(
  tx: Transaction,
  policy: PolicyId,
  list_address: Address,
  retention_address: Address,
  kind: Kind,
  target: OutputReference,
  authenticated_header_end: Int,
  inline_limit: Int,
  max_payload_bytes: Int,
  max_payload_nodes: Int,
  node_reference_index: Int,
  external_reference_index: Option<Int>,
) -> Commitment {
  let (retained, _, _) = capture_open(
    tx,
    policy,
    list_address,
    retention_address,
    kind,
    target,
    authenticated_header_end,
    inline_limit,
    max_payload_bytes,
    max_payload_nodes,
    node_reference_index,
    external_reference_index,
  )
  retained
}

/// Timing predicates need authenticated metadata/content, but not an original
/// Value commitment. Keep the shared L1 authentication here so those scripts
/// do not carry deposit projection/accounting code they never use.
pub fn capture_payload(
  tx: Transaction,
  policy: PolicyId,
  list_address: Address,
  retention_address: Address,
  kind: Kind,
  target: OutputReference,
  authenticated_header_end: Int,
  inline_limit: Int,
  max_payload_bytes: Int,
  max_payload_nodes: Int,
  node_reference_index: Int,
  external_reference_index: Option<Int>,
) -> (Facts, Payload, Value) {
  let (lower, upper) = utils.get_inclusive_bounds_of_a_short_validity_range(
    tx.validity_range,
  )
  expect horizon.capture_permitted(authenticated_header_end, lower, upper)
  expect Some(input) = list.at(tx.reference_inputs, node_reference_index)
  expect
    !list.any(
      tx.inputs,
      fn(spent) { spent.output_reference == input.output_reference },
    )
  let node = ordering.authenticate(input.output, policy, list_address)
  let facts = from_node(node)
  expect facts.event_id == target
  let payload = open_payload(
    facts,
    tx,
    retention_address,
    inline_limit,
    max_payload_bytes,
    max_payload_nodes,
    external_reference_index,
  )
  expect matches_kind(payload, kind)
  (facts, payload, input.output.value)
}

/// Capture and return the exact immutable opening while the node and optional
/// retained datum are authenticated. Staged transition proofs keep the commitment
/// and reopen these facts after pointer changes, retirement or L1 data reclamation.
pub fn capture_open(
  tx: Transaction,
  policy: PolicyId,
  list_address: Address,
  retention_address: Address,
  kind: Kind,
  target: OutputReference,
  authenticated_header_end: Int,
  inline_limit: Int,
  max_payload_bytes: Int,
  max_payload_nodes: Int,
  node_reference_index: Int,
  external_reference_index: Option<Int>,
) -> (Commitment, Payload, Value) {
  let (facts, payload, locked_value) = capture_payload(
    tx,
    policy,
    list_address,
    retention_address,
    kind,
    target,
    authenticated_header_end,
    inline_limit,
    max_payload_bytes,
    max_payload_nodes,
    node_reference_index,
    external_reference_index,
  )
  let original_assets = funding.order_assets(
    locked_value,
    policy,
    user_events.out_ref_to_nonce(target),
    facts.structural_lovelace,
  )
  (
    commitment(policy, kind, facts, payload, original_assets),
    payload,
    original_assets,
  )
}

/// Later stages reopen only authenticated immutable facts. No spent node or
/// storage output reference is carried into this API. Original assets are raw
/// Data at public boundaries because Value is opaque. Exact hash equality with
/// the Value captured from L1 authenticates its encoding without reconstruction.
pub fn opens(
  retained: Commitment,
  payload: Payload,
  original_assets: Data,
) -> Bool {
  and {
    matches_kind(payload, retained.kind),
    payload_id(payload) == retained.event_id,
    ( cbor.serialise(payload) |> blake2b_256 ) == retained.payload_hash,
    ( cbor.serialise(original_assets) |> blake2b_256 ) == retained.original_assets_hash,
  }
}

/// Immutable material carried by a later stage, authenticated by Commitment.
pub type Opening {
  payload: Payload,
  original_assets: Data,
}

/// Reconstruct only the Value whose complete canonical encoding was captured
/// from L1. Structural ADA and the list NFT have already been removed at capture;
/// callers must project this original Value without subtracting another token.
/// Public redeemers use Data because the standard-library Value type is opaque.
pub fn reopen(
  retained: Commitment,
  payload: Payload,
  original_assets: Data,
) -> Value {
  expect opens(retained, payload, original_assets)
  expect pairs: Pairs<PolicyId, Pairs<ByteArray, Int>> = original_assets
  assets.from_asset_list(pairs)
}
