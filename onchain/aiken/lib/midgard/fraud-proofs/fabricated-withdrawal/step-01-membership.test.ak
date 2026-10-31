//// The committed withdrawal-source membership check of step-01 against a real
//// one-leaf withdrawals root: it accepts the committed leaf and refuses a
//// leaf whose key or value differs.

use aiken/cbor
use aiken/merkle_patricia_forestry as mpf
use cardano/address
use cardano/transaction.{NoDatum, OutputReference}
use midgard/fraud_proofs/fabricated_withdrawal/step_01
use midgard/ledger_state
use midgard/transition_trace

const h28 = #"11111111111111111111111111111111111111111111111111111111"

const h32 = #"3333333333333333333333333333333333333333333333333333333333333333"

fn withdrawal_id(index: Int) -> ledger_state.WithdrawalId {
  OutputReference { transaction_id: h32, output_index: index }
}

fn info(owner: ByteArray) -> ledger_state.WithdrawalInfo {
  ledger_state.WithdrawalInfo {
    body: ledger_state.WithdrawalBody {
      l2_outref: withdrawal_id(7),
      l2_owner: owner,
      l2_value: [],
      l1_address: address.from_script(h28),
      l1_datum: NoDatum,
    },
    signature: Pair(#"aa", #"bb"),
    validity: ledger_state.WithdrawalIsValid,
  }
}

fn membership(
  key: ledger_state.WithdrawalId,
  value: ledger_state.WithdrawalInfo,
) -> step_01.CommittedWithdrawalSourceProofV1 {
  let phas_root = mpf.root(
    mpf.insert(mpf.empty, cbor.serialise(key), cbor.serialise(value), []),
  )
  transition_trace.RootMembershipProof {
    domain: transition_trace.WithdrawalsRootDomain,
    root: transition_trace.commit_counted_root(
      transition_trace.WithdrawalsRootDomain,
      phas_root,
      1,
    ),
    phas_root,
    count: 1,
    key,
    value,
    proof: [],
  }
}

fn header(root: ByteArray) -> ledger_state.HeaderV1 {
  ledger_state.HeaderV1 {
    prev_utxos_root: h32,
    utxos_root: h32,
    withdrawals_root: root,
    forced_transactions_root: h32,
    transactions_root: h32,
    deposits_root: h32,
    transition_trace_root: h32,
    event_to_step_root: h32,
    validation_traces_root: h32,
    withdrawal_count: 1,
    forced_transaction_count: 0,
    l2_transaction_count: 0,
    deposit_count: 0,
    total_event_count: 1,
    transition_step_count: 1,
    validation_trace_count: 0,
    start_time: 10,
    end_time: 20,
    block_slot: 0,
    expected_network_id: 0,
    min_fee_a: 0,
    min_fee_b: 0,
    prev_header_hash: h32,
    operator_vkey: h28,
    protocol_version: 1,
  }
}

test committed_withdrawal_leaf_is_a_member() {
  let committed = membership(withdrawal_id(0), info(h28))
  and {
    step_01.verify_committed_withdrawal_membership_v1(
      header(committed.root),
      committed,
    ),
    step_01.verify_committed_withdrawal_membership_with_value_bytes_v1(
      header(committed.root),
      committed,
      cbor.serialise(committed.value),
    ),
  }
}

test substituted_withdrawal_value_is_not_a_member() {
  let committed = membership(withdrawal_id(0), info(h28))
  let substituted = transition_trace.RootMembershipProof {
    ..committed,
    value: info(#"22"),
  }
  !step_01.verify_committed_withdrawal_membership_v1(
    header(committed.root),
    substituted,
  )
}

test substituted_withdrawal_key_is_not_a_member() {
  let committed = membership(withdrawal_id(0), info(h28))
  let substituted = transition_trace.RootMembershipProof {
    ..committed,
    key: withdrawal_id(1),
  }
  !step_01.verify_committed_withdrawal_membership_v1(
    header(committed.root),
    substituted,
  )
}
