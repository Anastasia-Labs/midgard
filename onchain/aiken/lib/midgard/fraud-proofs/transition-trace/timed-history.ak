//// Timing predicates over authenticated deposit/withdrawal history. Mutable
//// UTxO pointers belong to the current transaction witness, not to a routed
//// semantic proof. Callers authenticate the header envelope and hub parameters,
//// then mark the challenged queue node when issuing the terminal fraud token.

use aiken/cbor
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/event_history/horizon
use midgard/event_history/order_facts.{Deposit, Withdrawal}
use midgard/event_history/proof_evidence
use midgard/ledger_state
use midgard/transition_trace

pub type Parameters {
  policy: PolicyId,
  list_address: Address,
  data: proof_evidence.Parameters,
}

/// Resolved from the actual final transaction. Neither reference is an event
/// identity; permissionless predecessor/filler operations may replace them.
pub type Reference {
  order_index: Int,
  external_data_index: Option<Int>,
}

fn capture(
  tx: Transaction,
  parameters: Parameters,
  reference: Reference,
  kind: order_facts.Kind,
  target: OutputReference,
  header_end: Int,
) -> (order_facts.Facts, order_facts.Payload) {
  let (facts, payload, _) = order_facts.capture_payload(
    tx,
    parameters.policy,
    parameters.list_address,
    parameters.data.retention_address,
    kind,
    target,
    header_end,
    parameters.data.inline_limit,
    parameters.data.max_payload_bytes,
    parameters.data.max_payload_nodes,
    reference.order_index,
    reference.external_data_index,
  )
  (facts, payload)
}

pub fn omitted(
  tx: Transaction,
  parameters: Parameters,
  reference: Reference,
  kind: order_facts.Kind,
  header: ledger_state.HeaderV1,
  source: transition_trace.RootNonMembershipProof<OutputReference>,
) -> Bool {
  let (facts, _) = capture(
    tx,
    parameters,
    reference,
    kind,
    source.key,
    header.end_time,
  )
  let (domain, root, count) = when kind is {
    Deposit ->
      (
        transition_trace.DepositsRootDomain,
        header.deposits_root,
        header.deposit_count,
      )
    Withdrawal ->
      (
        transition_trace.WithdrawalsRootDomain,
        header.withdrawals_root,
        header.withdrawal_count,
      )
  }
  and {
    horizon.eligible(facts.inclusion_time, header.start_time, header.end_time),
    transition_trace.verify_root_non_membership_with_key_bytes(
      source,
      domain,
      root,
      count,
      cbor.serialise(source.key),
    ),
  }
}

pub fn out_of_window_deposit(
  tx: Transaction,
  parameters: Parameters,
  reference: Reference,
  header: ledger_state.HeaderV1,
  source: transition_trace.RootMembershipProof<
    ledger_state.DepositId,
    ledger_state.DepositInfo,
  >,
) -> Bool {
  let (facts, payload) = capture(
    tx,
    parameters,
    reference,
    Deposit,
    source.key,
    header.end_time,
  )
  expect order_facts.DepositPayload { event } = payload
  and {
    !horizon.eligible(facts.inclusion_time, header.start_time, header.end_time),
    source.value == event.info,
    transition_trace.verify_root_membership_with_bytes(
      source,
      transition_trace.DepositsRootDomain,
      header.deposits_root,
      header.deposit_count,
      cbor.serialise(source.key),
      cbor.serialise(source.value),
    ),
  }
}

pub fn out_of_window_withdrawal(
  tx: Transaction,
  parameters: Parameters,
  reference: Reference,
  header: ledger_state.HeaderV1,
  source: transition_trace.RootMembershipProof<
    ledger_state.WithdrawalId,
    ledger_state.WithdrawalInfo,
  >,
) -> Bool {
  let (facts, payload) = capture(
    tx,
    parameters,
    reference,
    Withdrawal,
    source.key,
    header.end_time,
  )
  and {
    !horizon.eligible(facts.inclusion_time, header.start_time, header.end_time),
    // The submitted body/signature are immutable. The L2 operator's verdict
    // remains part of the authenticated leaf, but is not L1 submitted content.
    order_facts.withdrawal_info_matches(payload, source.value),
    transition_trace.verify_root_membership_with_bytes(
      source,
      transition_trace.WithdrawalsRootDomain,
      header.withdrawals_root,
      header.withdrawal_count,
      cbor.serialise(source.key),
      cbor.serialise(source.value),
    ),
  }
}
