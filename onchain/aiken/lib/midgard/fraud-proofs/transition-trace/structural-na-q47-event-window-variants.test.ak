//// ## Q47 structural-N/A executable disposition (GOAL_SPEC.md §9.1)
////
//// Q47 (omitted / out-of-window deposit, withdrawal and forced-event
//// variants) is structurally N/A as a standalone proof family: the six
//// violation variants are already constructors of the shared
//// `transitionTrace` family — `OmittedDueL1EventWitness` and
//// `OutOfWindowSourceEventWitness` in
//// `lib/midgard/fraud-proofs/transition-trace/proof.ak`. §9.1 requires that a
//// structural N/A be discharged by an *adversarial executable* proof of that
//// fact, never by prose.
////
//// `proof.test.ak` (Q21-owned, not edited here) already covers the
//// deposit-omitted, deposit-out-of-window and forced-order variants plus the
//// authenticated-end/excluded-start/published-after publication boundaries.
//// The measured residual breadth gap is the **withdrawal** domain:
//// `OmittedDueWithdrawal` and `OutOfWindowWithdrawal` had zero selectors
//// anywhere in the tree. This module closes that gap and adds the adjacent
//// in-window valid negatives plus wrong-domain and wrong-event-id controls,
//// so all six variants are executably reachable through the shared
//// `transitionTrace` lifecycle.
////
//// Every fixture here is re-declared locally: `proof.test.ak` is a distinct
//// Aiken module whose helpers are private, so nothing can be reused from it.
//// This module declares no validator, no catalogue category and no builder —
//// that absence is itself part of the structural claim.

use aiken/cbor
use aiken/merkle_patricia_forestry as mpf
use aiken/primitive/bytearray
use cardano/address.{Address}
use cardano/assets
use cardano/transaction.{InlineDatum, Input, NoDatum, Output, OutputReference}
use env
use midgard/common/utils
use midgard/fraud_proofs/native_tx/compact as native_compact
use midgard/fraud_proofs/native_tx/transaction as native_transaction
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/fraud_proofs/transition_trace/proof
use midgard/hub_oracle
use midgard/ledger_state
use midgard/transition_trace
use midgard/user_events
use midgard/user_events/tx_order_v1 as tx_order
use midgard/user_events/withdrawal

// ## Local fixtures

const h28_a = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const h28_b = #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const h32_a =
  #"1111111111111111111111111111111111111111111111111111111111111111"

const h32_b =
  #"2222222222222222222222222222222222222222222222222222222222222222"

const h64_a =
  #"33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333"

const fraud_category_id = #"00000004"

const zero_hash =
  #"0000000000000000000000000000000000000000000000000000000000000000"

const event_asset_name = #"01"

fn out_ref(ix: Int) -> OutputReference {
  OutputReference { transaction_id: h32_a, output_index: ix }
}

fn dummy_address() -> Address {
  address.from_script(h28_a)
}

fn dummy_hub_datum() -> hub_oracle.Datum {
  hub_oracle.Datum {
    registered_operators: h28_a,
    active_operators: h28_a,
    retired_operators: h28_a,
    scheduler: h28_a,
    state_queue: h28_a,
    fraud_proof_catalogue: h28_a,
    fraud_proof: h28_a,
    deposit: h28_a,
    withdrawal: h28_a,
    tx_order: h28_a,
    settlement: h28_a,
    payout: h28_a,
    registered_operators_addr: dummy_address(),
    active_operators_addr: dummy_address(),
    retired_operators_addr: dummy_address(),
    scheduler_addr: dummy_address(),
    state_queue_addr: dummy_address(),
    fraud_proof_catalogue_addr: dummy_address(),
    fraud_proof_addr: dummy_address(),
    deposit_addr: dummy_address(),
    withdrawal_addr: dummy_address(),
    tx_order_addr: dummy_address(),
    settlement_addr: dummy_address(),
    reserve_addr: dummy_address(),
    payout_addr: dummy_address(),
    reserve_observer: h28_a,
  }
}

/// Publication window is `(start_time, end_time]`: `start_time` is excluded
/// and `end_time` is included (`timed_l1_event_is_due` in proof.ak).
fn base_header() -> ledger_state.HeaderV1 {
  ledger_state.HeaderV1 {
    prev_utxos_root: h32_a,
    utxos_root: h32_b,
    withdrawals_root: env.empty_merkle_tree_root,
    forced_transactions_root: env.empty_merkle_tree_root,
    transactions_root: env.empty_merkle_tree_root,
    deposits_root: env.empty_merkle_tree_root,
    transition_trace_root: env.empty_merkle_tree_root,
    event_to_step_root: env.empty_merkle_tree_root,
    validation_traces_root: env.empty_merkle_tree_root,
    withdrawal_count: 0,
    forced_transaction_count: 0,
    l2_transaction_count: 0,
    deposit_count: 0,
    total_event_count: 0,
    transition_step_count: 0,
    validation_trace_count: 0,
    start_time: 10,
    end_time: 20,
    block_slot: 0,
    expected_network_id: 0,
    min_fee_a: 0,
    min_fee_b: 0,
    prev_header_hash: h28_b,
    operator_vkey: h28_a,
    protocol_version: ledger_state.protocol_version_v1,
  }
}

fn prove(
  header: ledger_state.HeaderV1,
  fault: proof.TransitionFault,
) -> proof.TransitionFaultProof {
  let header_data: Data = header
  proof.TransitionFaultProof {
    challenged_header_hash: utils.serialise_and_hash_28(header_data),
    header,
    fault,
  }
}

fn asset_name_for(header: ledger_state.HeaderV1) -> ByteArray {
  let header_data: Data = header
  fraud_category_id
    |> bytearray.concat(utils.serialise_and_hash_28(header_data))
}

fn empty_non_membership(
  domain: transition_trace.RootDomain,
  key: key,
) -> transition_trace.RootNonMembershipProof<key> {
  transition_trace.RootNonMembershipProof {
    domain,
    root: env.empty_merkle_tree_root,
    phas_root: env.empty_merkle_tree_root,
    count: 0,
    key,
    proof: [],
  }
}

// ### Withdrawal fixtures

fn withdrawal_info(
  validity: ledger_state.WithdrawalValidity,
) -> ledger_state.WithdrawalInfo {
  ledger_state.WithdrawalInfo {
    body: ledger_state.WithdrawalBody {
      l2_outref: out_ref(9),
      l2_owner: h28_a,
      l2_value: [],
      l1_address: dummy_address(),
      l1_datum: NoDatum,
    },
    signature: Pair(h32_a, h64_a),
    validity,
  }
}

/// The L1 withdrawal order always carries `WithdrawalIsValid` at mint
/// (validators/user-events/withdrawal.ak), so the authenticated reference
/// datum is pinned to that tag here.
fn withdrawal_reference_input(inclusion_time: Int) -> Input {
  let datum: withdrawal.Datum = user_events.OptimisticDatum {
    event: ledger_state.WithdrawalEvent {
      id: out_ref(0),
      info: withdrawal_info(ledger_state.WithdrawalIsValid),
    },
    inclusion_time,
    witness: h28_a,
    refund_address: dummy_address(),
    refund_datum: NoDatum,
  }
  let datum_data: Data = datum
  Input {
    output_reference: out_ref(99),
    output: Output {
      address: dummy_address(),
      value: assets.from_asset(h28_a, event_asset_name, 1),
      datum: InlineDatum(datum_data),
      reference_script: None,
    },
  }
}

fn withdrawal_source_membership(
  withdrawal_id: ledger_state.WithdrawalId,
  info: ledger_state.WithdrawalInfo,
) -> transition_trace.RootMembershipProof<
  ledger_state.WithdrawalId,
  ledger_state.WithdrawalInfo,
> {
  let key_bytes = cbor.serialise(withdrawal_id)
  let value_bytes = cbor.serialise(info)
  let phas_root = mpf.root(mpf.insert(mpf.empty, key_bytes, value_bytes, []))
  transition_trace.RootMembershipProof {
    domain: transition_trace.WithdrawalsRootDomain,
    root: transition_trace.commit_counted_root(
      transition_trace.WithdrawalsRootDomain,
      phas_root,
      1,
    ),
    phas_root,
    count: 1,
    key: withdrawal_id,
    value: info,
    proof: [],
  }
}

// ### Forced-transaction fixtures

fn empty_native_transaction() -> native_types.MidgardTransaction {
  let empty_mint: Data = []
  native_types.MidgardTransaction {
    version: native_types.native_tx_version_v1,
    validity: native_types.TxIsValid,
    body: native_types.MidgardTransactionBody {
      inputs: [],
      reference_inputs: [],
      outputs: [],
      fee: 0,
      validity_interval_start: -1,
      validity_interval_end: -1,
      required_observers: [],
      required_signers: [],
      mint: empty_mint,
      script_integrity_hash: zero_hash,
      auxiliary_data_hash: zero_hash,
      network_id: 0,
    },
    witness_set: native_types.MidgardTransactionWitnessSet {
      addr_tx_wits: [],
      script_tx_wits: [],
      redeemer_tx_wits: [],
    },
  }
}

fn authenticated_tx_order_payload() -> ledger_state.TxOrderPayloadV1 {
  let tx = empty_native_transaction()
  let compact = native_transaction.midgard_transaction_to_compact(tx)
  let proof_source = native_transaction.midgard_transaction_proof_source_v1(tx)
  ledger_state.TxOrderPayloadV1 {
    tx_id: native_compact.native_tx_id_for_version(
      native_types.native_tx_version_v1,
      native_compact.encode_native_tx_body_compact(compact.body),
    ),
    transaction_commitment: native_transaction.midgard_transaction_proof_commitment_v1(
      tx,
    ),
    source: ledger_state.NativeTxProofSourceV1 {
      compact_cbor: proof_source.compact_cbor,
      witness_set_compact_cbor: proof_source.witness_set_compact_cbor,
      field_preimage_lengths_cbor: proof_source.field_preimage_lengths_cbor,
    },
    terminal_receipt_reference: None,
  }
}

fn tx_order_reference_input(inclusion_time: Int) -> Input {
  let datum: tx_order.Datum = user_events.OptimisticDatum {
    event: ledger_state.TxOrderEventV1 {
      id: out_ref(0),
      tx: authenticated_tx_order_payload(),
    },
    inclusion_time,
    witness: h28_a,
    refund_address: dummy_address(),
    refund_datum: NoDatum,
  }
  let datum_data: Data = datum
  Input {
    output_reference: out_ref(99),
    output: Output {
      address: dummy_address(),
      value: assets.from_asset(h28_a, event_asset_name, 1),
      datum: InlineDatum(datum_data),
      reference_script: None,
    },
  }
}

fn authenticated_forced_tx(
  operator_validity: ledger_state.MidgardTxValidity,
) -> ledger_state.ForcedInclusionTxV1 {
  let payload = authenticated_tx_order_payload()
  ledger_state.ForcedInclusionTxV1 {
    tx_id: payload.tx_id,
    transaction_commitment: payload.transaction_commitment,
    source: payload.source,
    operator_validity,
  }
}

fn forced_source_membership(
  tx_order_id: ledger_state.TxOrderId,
  tx: ledger_state.ForcedInclusionTxV1,
) -> transition_trace.RootMembershipProof<
  ledger_state.TxOrderId,
  ledger_state.ForcedInclusionTxV1,
> {
  let key_bytes = cbor.serialise(tx_order_id)
  let value_bytes = cbor.serialise(tx)
  let phas_root = mpf.root(mpf.insert(mpf.empty, key_bytes, value_bytes, []))
  transition_trace.RootMembershipProof {
    domain: transition_trace.ForcedTransactionsV1RootDomain,
    root: transition_trace.commit_counted_root(
      transition_trace.ForcedTransactionsV1RootDomain,
      phas_root,
      1,
    ),
    phas_root,
    count: 1,
    key: tx_order_id,
    value: tx,
    proof: [],
  }
}

// ## Scenario drivers

/// `OmittedDueWithdrawal`: the withdrawal order is published inside the
/// challenged block's window yet the block's counted `withdrawals_root`
/// omits it.
fn omitted_due_withdrawal_is_valid(
  inclusion_time: Int,
  non_membership_domain: transition_trace.RootDomain,
  non_membership_key: ledger_state.WithdrawalId,
) -> Bool {
  let header = base_header()
  proof.validate_l1_event_fault_proof(
    prove(
      header,
      proof.OmittedDueL1Event {
        witness: proof.OmittedDueWithdrawal {
          event_ref_input_index: 0,
          event_asset_name,
          source_non_membership: empty_non_membership(
            non_membership_domain,
            non_membership_key,
          ),
        },
      },
    ),
    asset_name_for(header),
    dummy_hub_datum(),
    [withdrawal_reference_input(inclusion_time)],
  )
}

/// `OutOfWindowWithdrawal`: the block commits the withdrawal even though the
/// order was published outside the block's authenticated window.
fn out_of_window_withdrawal_is_valid(inclusion_time: Int) -> Bool {
  let validity_override = ledger_state.WithdrawalIsValid
  let source_membership = withdrawal_source_membership(
    out_ref(0),
    withdrawal_info(validity_override),
  )
  let header = ledger_state.HeaderV1 {
    ..base_header(),
    withdrawals_root: source_membership.root,
    withdrawal_count: 1,
  }
  proof.validate_l1_event_fault_proof(
    prove(
      header,
      proof.OutOfWindowSourceEvent {
        witness: proof.OutOfWindowWithdrawal {
          event_ref_input_index: 0,
          event_asset_name,
          validity_override,
          source_membership,
        },
      },
    ),
    asset_name_for(header),
    dummy_hub_datum(),
    [withdrawal_reference_input(inclusion_time)],
  )
}

/// `OmittedDueForcedTransaction`: the forced order is due against the block's
/// window and its transaction validity interval, yet the counted
/// `forced_transactions_root` omits it.
fn omitted_due_forced_is_valid(inclusion_time: Int) -> Bool {
  let header = base_header()
  proof.validate_l1_event_fault_proof(
    prove(
      header,
      proof.OmittedDueL1Event {
        witness: proof.OmittedDueForcedTransaction {
          event_ref_input_index: 0,
          event_asset_name,
          validity_override: ledger_state.TxIsValid,
          source_non_membership: empty_non_membership(
            transition_trace.ForcedTransactionsV1RootDomain,
            out_ref(0),
          ),
        },
      },
    ),
    asset_name_for(header),
    dummy_hub_datum(),
    [tx_order_reference_input(inclusion_time)],
  )
}

/// `OutOfWindowForcedTransaction`: the block commits the forced order even
/// though it was published outside the block's authenticated window.
fn out_of_window_forced_is_valid(inclusion_time: Int) -> Bool {
  let source_membership = forced_source_membership(
    out_ref(0),
    authenticated_forced_tx(ledger_state.TxIsValid),
  )
  let header = ledger_state.HeaderV1 {
    ..base_header(),
    forced_transactions_root: source_membership.root,
    forced_transaction_count: 1,
  }
  proof.validate_l1_event_fault_proof(
    prove(
      header,
      proof.OutOfWindowSourceEvent {
        witness: proof.OutOfWindowForcedTransaction {
          event_ref_input_index: 0,
          event_asset_name,
          validity_override: ledger_state.TxIsValid,
          source_membership,
        },
      },
    ),
    asset_name_for(header),
    dummy_hub_datum(),
    [tx_order_reference_input(inclusion_time)],
  )
}

// ## Q47 selectors (GOAL_SPEC.md §9.1 structural-N/A disposition)

/// Variant 2 of 6 — omitted due **withdrawal**. Published at the inclusive
/// `end_time` boundary, so the order is due, and the empty counted
/// `withdrawals_root` omits it.
test q47_omitted_due_withdrawal_accepts() {
  omitted_due_withdrawal_is_valid(
    base_header().end_time,
    transition_trace.WithdrawalsRootDomain,
    out_ref(0),
  )
}

/// Variant 5 of 6 — out-of-window **withdrawal**. Published one tick after
/// the authenticated `end_time`, yet committed by the block.
test q47_out_of_window_withdrawal_accepts() {
  out_of_window_withdrawal_is_valid(base_header().end_time + 1)
}

/// Variant 3 of 6 — omitted due **forced** transaction, at the same inclusive
/// `end_time` boundary.
test q47_omitted_due_forced_accepts() {
  omitted_due_forced_is_valid(base_header().end_time)
}

/// Variant 6 of 6 — out-of-window **forced** transaction, published one tick
/// after the authenticated `end_time`.
test q47_out_of_window_forced_accepts() {
  out_of_window_forced_is_valid(base_header().end_time + 1)
}

/// Adjacent valid-block negative: a withdrawal published *inside* the window
/// is correctly committed, so the out-of-window fault must not hold. The only
/// change from the accepting vector is the publication time.
test q47_in_window_withdrawal_rejects() {
  !out_of_window_withdrawal_is_valid(base_header().end_time)
}

/// Adjacent valid-block negative: a forced order published inside the window
/// is correctly committed, so the out-of-window fault must not hold.
test q47_in_window_forced_rejects() {
  !out_of_window_forced_is_valid(base_header().end_time)
}

/// Control: the omitted-withdrawal non-membership proof must be opened under
/// `WithdrawalsRootDomain`. Substituting `DepositsRootDomain` — with the same
/// empty root and zero count, so only the domain differs — must not prove a
/// withdrawal omission.
test q47_wrong_domain_rejects() {
  !omitted_due_withdrawal_is_valid(
    base_header().end_time,
    transition_trace.DepositsRootDomain,
    out_ref(0),
  )
}

/// Control: the non-membership key must equal the authenticated L1 event id.
/// A different `WithdrawalId` proves nothing about the referenced order.
test q47_wrong_event_id_rejects() {
  !omitted_due_withdrawal_is_valid(
    base_header().end_time,
    transition_trace.WithdrawalsRootDomain,
    out_ref(7),
  )
}
