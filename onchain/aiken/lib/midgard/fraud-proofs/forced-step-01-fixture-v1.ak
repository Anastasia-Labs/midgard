//// Forced-source cases for a step-01 forced arm that opens its leaf with the
//// shared forced opener and hands on a `SourceBoundV1`. Each case carries the
//// thread name, the redeemer's header and membership, and the state that arm
//// would hand on for that header and leaf. The state is built from the leaf's
//// own fields, never through the opener, so a case whose leaf the opener
//// refuses still names the state the arm would otherwise produce.
//// Test-only — never referenced by a validator.

use aiken/cbor
use aiken/merkle_patricia_forestry as mpf
use cardano/transaction.{OutputReference}
use midgard/fraud_proofs/direct_script_derivation/types.{SourceBoundV1}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_script_decoding/thread_fixture_v1 as tf
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/ledger_state
use midgard/rejection_reason_v1.{ForcedTxValid}
use midgard/transition_trace

pub type ForcedLeafMembership =
  transition_trace.ForcedLeafMembershipV1<
    ledger_state.TxOrderId,
    ledger_state.ForcedInclusionTxV1,
  >

pub type ForcedStep01CaseV1 {
  thread_name: ByteArray,
  header: ledger_state.HeaderV1,
  membership: ForcedLeafMembership,
  expected_state: Data,
}

const other_order_key_tx_id =
  #"3434343434343434343434343434343434343434343434343434343434343434"

fn other_order_key() -> ledger_state.TxOrderId {
  OutputReference { transaction_id: other_order_key_tx_id, output_index: 1 }
}

fn leaf() -> ledger_state.ForcedInclusionTxV1 {
  tf.forced_leaf_v1(fixture.valid_native_tx_v1(), ForcedTxValid)
}

fn other_leaf() -> ledger_state.ForcedInclusionTxV1 {
  tf.forced_leaf_v1(
    native_types.MidgardTransaction {
      ..fixture.valid_native_tx_v1(),
      validity: native_types.TxIsInvalid,
    },
    ForcedTxValid,
  )
}

/// A header whose counted forced root commits `phas_root` holding `count`
/// leaves, and records `count` itself.
fn header_committing(phas_root: ByteArray, count: Int) -> ledger_state.HeaderV1 {
  let base = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  ledger_state.HeaderV1 {
    ..base.header,
    forced_transactions_root: tf.forced_counted_root_v1(phas_root, count),
    forced_transaction_count: count,
  }
}

fn single_leaf_root(key_bytes: ByteArray, value_bytes: ByteArray) -> ByteArray {
  mpf.root(mpf.insert(mpf.empty, key_bytes, value_bytes, []))
}

/// The acceptance subject the arm hands on for this membership's leaf and key.
fn expected_subject(
  membership: ForcedLeafMembership,
) -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_acceptance,
    source_kind: substrate.source_kind_forced,
    transaction_id: membership.value.tx_id,
    source_key: cbor.serialise(membership.key),
    rejection_reason: None,
  }
}

fn case(
  thread_header: ledger_state.HeaderV1,
  header: ledger_state.HeaderV1,
  membership: ForcedLeafMembership,
) -> ForcedStep01CaseV1 {
  let expected: Data = SourceBoundV1 {
    subject: expected_subject(membership),
    header,
    compact_cbor: membership.value.submitted_source.compact_cbor,
  }
  ForcedStep01CaseV1 {
    thread_name: tf.header_asset_name_v1(thread_header),
    header,
    membership,
    expected_state: expected,
  }
}

/// The leaf is the only member of the forced root of the header the thread
/// names.
pub fn member_of_thread_header_v1() -> ForcedStep01CaseV1 {
  let membership = tf.forced_leaf_membership_v1(
    tf.forced_order_key_v1(),
    leaf(),
  )
  let header = header_committing(membership.phas_root, 1)
  case(header, header, membership)
}

/// The membership's root holds the leaf, but the header commits the root of
/// a forced trie that holds only another leaf.
pub fn root_other_than_the_header_commits_v1() -> ForcedStep01CaseV1 {
  let membership = tf.forced_leaf_membership_v1(
    tf.forced_order_key_v1(),
    leaf(),
  )
  let committed = tf.forced_leaf_membership_v1(other_order_key(), other_leaf())
  let header = header_committing(committed.phas_root, 1)
  case(header, header, membership)
}

/// The header's counted root commits the membership's root as one leaf, but
/// the header records two forced transactions.
pub fn count_other_than_the_header_commits_v1() -> ForcedStep01CaseV1 {
  let membership = tf.forced_leaf_membership_v1(
    tf.forced_order_key_v1(),
    leaf(),
  )
  let header = ledger_state.HeaderV1 {
    ..header_committing(membership.phas_root, 1),
    forced_transaction_count: 2,
  }
  case(header, header, membership)
}

/// The header's forced trie holds the leaf's encoding as its key and the order
/// key's encoding as its value.
pub fn key_and_value_swapped_v1() -> ForcedStep01CaseV1 {
  let swapped_root = single_leaf_root(
    cbor.serialise(leaf()),
    cbor.serialise(tf.forced_order_key_v1()),
  )
  let membership = transition_trace.ForcedLeafMembershipV1 {
    phas_root: swapped_root,
    key: tf.forced_order_key_v1(),
    value: leaf(),
    proof: [],
  }
  let header = header_committing(swapped_root, 1)
  case(header, header, membership)
}

/// The header's forced trie holds the leaf under one order key; the
/// membership names another.
pub fn key_other_than_the_committed_one_v1() -> ForcedStep01CaseV1 {
  let committed = tf.forced_leaf_membership_v1(tf.forced_order_key_v1(), leaf())
  let membership = transition_trace.ForcedLeafMembershipV1 {
    ..committed,
    key: other_order_key(),
  }
  let header = header_committing(committed.phas_root, 1)
  case(header, header, membership)
}

/// The leaf is a member of the redeemer header's forced root, but the thread
/// names a header whose forced root holds only another leaf.
pub fn leaf_of_a_header_the_thread_does_not_name_v1() -> ForcedStep01CaseV1 {
  let membership = tf.forced_leaf_membership_v1(
    tf.forced_order_key_v1(),
    leaf(),
  )
  let leaf_header = header_committing(membership.phas_root, 1)
  let other = tf.forced_leaf_membership_v1(other_order_key(), other_leaf())
  let thread_header = ledger_state.HeaderV1 {
    ..header_committing(other.phas_root, 1),
    prev_utxos_root: tf.h32_d,
  }
  case(thread_header, leaf_header, membership)
}
