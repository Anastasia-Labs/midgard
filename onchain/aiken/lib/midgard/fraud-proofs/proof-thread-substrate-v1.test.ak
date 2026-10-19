//// Focused ABI and mutation selectors for the shared non-interactive
//// proof-thread verdict-subject substrate.

use aiken/cbor
use aiken/merkle_patricia_forestry as mpf
use aiken/primitive/bytearray
use cardano/transaction
use midgard/forced_submission_fixture as forced_fixture
use midgard/fraud_proofs/input_existence_fixture_v1 as carriage_fixture
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_script_decoding/thread_fixture_v1 as tf
use midgard/fraud_proofs/native_tx/compact.{verify_native_tx_proof_source_v1} as native_compact
use midgard/fraud_proofs/native_tx/transaction as native_transaction
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/ledger_state
use midgard/rejection_reason_v1.{ForcedTxInvalid, ForcedTxValid}
use midgard/transition_trace

const prior_root =
  #"5555555555555555555555555555555555555555555555555555555555555555"

fn accepted_verified_v1() -> native_types.VerifiedTxSourceView {
  let tx = fixture.valid_native_tx_v1()
  let source = native_transaction.midgard_transaction_proof_source_v1(tx)
  let compact = native_transaction.midgard_transaction_to_compact(tx)
  let tx_id = native_compact.native_tx_id_for_version(
    1,
    native_compact.encode_native_tx_body_compact(compact.body),
  )
  let Pair(verified, _witness_set) = verify_native_tx_proof_source_v1(
    tx_id,
    source.compact_cbor,
    source.witness_set_compact_cbor,
    source.field_preimage_lengths_cbor,
  )
  verified
}

fn forced_claim_v1(
  leaf: ledger_state.ForcedInclusionTxV1,
) -> (
  ledger_state.HeaderV1,
  transition_trace.RootMembershipProof<
    ledger_state.TxOrderId,
    ledger_state.ForcedInclusionTxV1,
  >,
) {
  let membership = tf.forced_leaf_membership_v1(tf.forced_order_key_v1(), leaf)
  let claim = tf.thread_claim_v1(
    ledger_state.ForcedTransactionEventKey {
      tx_order_id: tf.forced_order_key_v1(),
    },
    ledger_state.ForcedTransaction,
    prior_root,
    Some(membership.root),
  )
  (claim.header, membership)
}

fn rejected_transaction_v1() -> native_types.MidgardTransaction {
  native_types.MidgardTransaction {
    ..fixture.valid_native_tx_v1(),
    validity: native_types.TxIsInvalid,
  }
}

fn input_not_found_reason_v1() -> rejection_reason_v1.RejectionReasonV1 {
  rejection_reason_v1.InputNotFound { source_kind: 1, input_index: 2 }
}

fn forced_accepted_subject_v1() -> substrate.VerdictSubjectV1 {
  let leaf = tf.forced_leaf_v1(fixture.valid_native_tx_v1(), ForcedTxValid)
  let (header, membership) = forced_claim_v1(leaf)
  substrate.open_subject_v1(
    tf.header_asset_name_v1(header),
    substrate.ForcedSource { header, membership },
  )
}

fn forced_rejected_subject_v1() -> substrate.VerdictSubjectV1 {
  let leaf = tf.forced_leaf_v1(
    rejected_transaction_v1(),
    ForcedTxInvalid { reason: input_not_found_reason_v1() },
  )
  let (header, membership) = forced_claim_v1(leaf)
  substrate.open_subject_v1(
    tf.header_asset_name_v1(header),
    substrate.ForcedSource { header, membership },
  )
}

// ## Binding successes and source/direction mutations

test proof_thread_subject_binds_an_accepted_transaction() {
  let subject = substrate.open_subject_v1(
    tf.header_asset_name_v1(
      fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()).header,
    ),
    substrate.AcceptedSource {
      header: fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()).header,
      tx_inclusion: accepted_verified_v1(),
    },
  )
  and {
    substrate.subject_is_canonical_v1(subject),
    subject.direction == substrate.direction_wrongful_acceptance,
    subject.source_kind == substrate.source_kind_accepted,
    subject.source_key == #"",
    subject.rejection_reason == None,
  }
}

test proof_thread_subject_rejects_an_accepted_transaction_with_invalid_scalar() fail {
  substrate.open_subject_v1(
    tf.header_asset_name_v1(
      fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()).header,
    ),
    substrate.AcceptedSource {
      header: fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()).header,
      tx_inclusion: native_types.VerifiedTxSourceView {
        ..accepted_verified_v1(),
        tx_compact: native_types.TxCompactView {
          ..accepted_verified_v1().tx_compact,
          validity_code: Some(1),
        },
      },
    },
  ) == substrate.open_subject_v1(
    tf.header_asset_name_v1(
      fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()).header,
    ),
    substrate.AcceptedSource {
      header: fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()).header,
      tx_inclusion: accepted_verified_v1(),
    },
  )
}

test proof_thread_subject_binds_a_forced_acceptance() {
  let subject = forced_accepted_subject_v1()
  and {
    substrate.subject_is_canonical_v1(subject),
    subject.direction == substrate.direction_wrongful_acceptance,
    subject.source_kind == substrate.source_kind_forced,
    bytearray.length(subject.source_key) > 0,
    subject.rejection_reason == None,
  }
}

test proof_thread_subject_binds_a_forced_rejection_and_exact_reason() {
  let subject = forced_rejected_subject_v1()
  substrate.bind_exact_rejection_reason_v1(subject, input_not_found_reason_v1()) == input_not_found_reason_v1()
}

test proof_thread_subject_rejects_wrongful_rejection_on_an_accepting_leaf() fail {
  substrate.encode_verdict_subject_v1(
    substrate.VerdictSubjectV1 { ..forced_accepted_subject_v1(), direction: 1 },
  ) == #""
}

test proof_thread_subject_rejects_wrongful_acceptance_on_a_rejecting_leaf() fail {
  substrate.encode_verdict_subject_v1(
    substrate.VerdictSubjectV1 { ..forced_rejected_subject_v1(), direction: 0 },
  ) == #""
}

test proof_thread_subject_rejects_forced_valid_with_an_obsolete_four_field_source() fail {
  let original = tf.forced_leaf_v1(rejected_transaction_v1(), ForcedTxValid)
  let obsolete = native_transaction.midgard_transaction_proof_source_v1(
    rejected_transaction_v1(),
  )
  let leaf = ledger_state.ForcedInclusionTxV1 {
    ..original,
    submitted_source: ledger_state.ForcedTxProofSourceV1 {
      ..original.submitted_source,
      compact_cbor: obsolete.compact_cbor,
    },
  }
  let (header, membership) = forced_claim_v1(leaf)
  substrate.open_subject_v1(
    tf.header_asset_name_v1(header),
    substrate.ForcedSource { header, membership },
  ) == forced_accepted_subject_v1()
}

test proof_thread_subject_rejects_forced_invalid_with_an_obsolete_four_field_source() fail {
  let original = tf.forced_leaf_v1(
    fixture.valid_native_tx_v1(),
    ForcedTxInvalid { reason: input_not_found_reason_v1() },
  )
  let obsolete = native_transaction.midgard_transaction_proof_source_v1(
    fixture.valid_native_tx_v1(),
  )
  let leaf = ledger_state.ForcedInclusionTxV1 {
    ..original,
    submitted_source: ledger_state.ForcedTxProofSourceV1 {
      ..original.submitted_source,
      compact_cbor: obsolete.compact_cbor,
    },
  }
  let (header, membership) = forced_claim_v1(leaf)
  substrate.open_subject_v1(
    tf.header_asset_name_v1(header),
    substrate.ForcedSource { header, membership },
  ) == forced_rejected_subject_v1()
}

test proof_thread_subject_rejects_an_unknown_direction() fail {
  substrate.encode_verdict_subject_v1(
    substrate.VerdictSubjectV1 { ..forced_accepted_subject_v1(), direction: 2 },
  ) == #""
}

test proof_thread_subject_rejects_a_substituted_forced_root() fail {
  let leaf = tf.forced_leaf_v1(fixture.valid_native_tx_v1(), ForcedTxValid)
  let (header, membership) = forced_claim_v1(leaf)
  substrate.open_subject_v1(
    tf.header_asset_name_v1(
      ledger_state.HeaderV1 { ..header, forced_transactions_root: prior_root },
    ),
    substrate.ForcedSource {
      header: ledger_state.HeaderV1 {
        ..header,
        forced_transactions_root: prior_root,
      },
      membership,
    },
  ) == forced_accepted_subject_v1()
}

test proof_thread_subject_rejects_a_substituted_thread_header() fail {
  let leaf = tf.forced_leaf_v1(fixture.valid_native_tx_v1(), ForcedTxValid)
  let (header, membership) = forced_claim_v1(leaf)
  substrate.open_subject_v1(
    tf.header_asset_name_v1(header),
    substrate.ForcedSource {
      header: ledger_state.HeaderV1 { ..header, prev_utxos_root: tf.h32_d },
      membership,
    },
  ) == forced_accepted_subject_v1()
}

test proof_thread_subject_rejects_a_substituted_forced_key() fail {
  let leaf = tf.forced_leaf_v1(fixture.valid_native_tx_v1(), ForcedTxValid)
  let (header, membership) = forced_claim_v1(leaf)
  let substituted_key = transaction.OutputReference {
    transaction_id: prior_root,
    output_index: 0,
  }
  expect _ = substrate.open_subject_v1(
    tf.header_asset_name_v1(header),
    substrate.ForcedSource {
      header,
      membership: transition_trace.RootMembershipProof {
        ..membership,
        key: substituted_key,
      },
    },
  )
  True
}

test proof_thread_subject_rejects_substituted_forced_source_bytes() fail {
  let leaf = tf.forced_leaf_v1(fixture.valid_native_tx_v1(), ForcedTxValid)
  let (header, membership) = forced_claim_v1(leaf)
  let substituted_leaf = ledger_state.ForcedInclusionTxV1 {
    ..leaf,
    submitted_source: ledger_state.ForcedTxProofSourceV1 {
      ..leaf.submitted_source,
      compact_cbor: bytearray.concat(leaf.submitted_source.compact_cbor, #"00"),
    },
  }
  expect _ = substrate.open_subject_v1(
    tf.header_asset_name_v1(header),
    substrate.ForcedSource {
      header,
      membership: transition_trace.RootMembershipProof {
        ..membership,
        value: substituted_leaf,
      },
    },
  )
  True
}

test proof_thread_subject_rejects_a_substituted_transaction_id() fail {
  let leaf = tf.forced_leaf_v1(fixture.valid_native_tx_v1(), ForcedTxValid)
  let (header, membership) = forced_claim_v1(leaf)
  let substituted_leaf = ledger_state.ForcedInclusionTxV1 {
    ..leaf,
    tx_id: prior_root,
  }
  expect _ = substrate.open_subject_v1(
    tf.header_asset_name_v1(header),
    substrate.ForcedSource {
      header,
      membership: transition_trace.RootMembershipProof {
        ..membership,
        value: substituted_leaf,
      },
    },
  )
  True
}

// ## Typed-reason and terminal contradiction mutations

test proof_thread_reason_binding_rejects_a_coordinate_mutation() fail {
  substrate.bind_exact_rejection_reason_v1(
    forced_rejected_subject_v1(),
    rejection_reason_v1.InputNotFound { source_kind: 1, input_index: 3 },
  ) == input_not_found_reason_v1()
}

test proof_thread_reason_binding_rejects_a_constructor_mutation() fail {
  substrate.bind_exact_rejection_reason_v1(
    forced_rejected_subject_v1(),
    rejection_reason_v1.OutputBelowMinAda { output_index: 2 },
  ) == input_not_found_reason_v1()
}

test proof_thread_terminal_accepts_both_real_contradiction_polarities() {
  and {
    substrate.terminal_contradiction_v1(
      substrate.open_subject_v1(
        tf.header_asset_name_v1(
          fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()).header,
        ),
        substrate.AcceptedSource {
          header: fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()).header,
          tx_inclusion: accepted_verified_v1(),
        },
      ),
      True,
    ),
    substrate.terminal_contradiction_v1(forced_accepted_subject_v1(), True),
    substrate.terminal_contradiction_v1(forced_rejected_subject_v1(), False),
  }
}

test proof_thread_terminal_refuses_an_honest_acceptance() {
  !substrate.terminal_contradiction_v1(
    substrate.open_subject_v1(
      tf.header_asset_name_v1(
        fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()).header,
      ),
      substrate.AcceptedSource {
        header: fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()).header,
        tx_inclusion: accepted_verified_v1(),
      },
    ),
    False,
  )
}

test proof_thread_terminal_refuses_an_honest_forced_rejection() {
  !substrate.terminal_contradiction_v1(forced_rejected_subject_v1(), True)
}

test proof_thread_terminal_rejects_a_noncanonical_reasonless_rejection() fail {
  substrate.terminal_contradiction_v1(
    substrate.VerdictSubjectV1 {
      ..forced_rejected_subject_v1(),
      rejection_reason: None,
    },
    False,
  )
}

// ## ABI vectors

test proof_thread_subject_accepted_v1() {
  substrate.encode_verdict_subject_v1(
    substrate.open_subject_v1(
      tf.header_asset_name_v1(
        fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()).header,
      ),
      substrate.AcceptedSource {
        header: fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()).header,
        tx_inclusion: accepted_verified_v1(),
      },
    ),
  ) == #"860100005820d4eafc0f14a743a05c3e5cc1aa36f26b420ec12a8a3c4473c5a458cd04c11f234080"
}

test proof_thread_subject_forced_accepted_v1() {
  substrate.encode_verdict_subject_v1(forced_accepted_subject_v1()) == #"860100015820d4eafc0f14a743a05c3e5cc1aa36f26b420ec12a8a3c4473c5a458cd04c11f235827d8799f5820777777777777777777777777777777777777777777777777777777777777777700ff80"
}

test proof_thread_subject_forced_rejected_input_not_found_v1() {
  substrate.encode_verdict_subject_v1(forced_rejected_subject_v1()) == #"860101015820d4eafc0f14a743a05c3e5cc1aa36f26b420ec12a8a3c4473c5a458cd04c11f235827d8799f5820777777777777777777777777777777777777777777777777777777777777777700ff81d9050b9f0102ff"
}

// ## Shared raw membership and verdict/event projection

test proof_thread_raw_and_typed_open_the_same_forced_leaf() {
  let leaf = tf.forced_leaf_v1(fixture.valid_native_tx_v1(), ForcedTxValid)
  let (header, membership) = forced_claim_v1(leaf)
  let raw = substrate.open_forced_leaf_raw_v1(
    header,
    cbor.serialise(membership.key),
    cbor.serialise(leaf),
    membership.phas_root,
    membership.proof,
  )
  raw == cbor.serialise(leaf)
}

test proof_thread_raw_open_rejects_one_byte_appended_to_leaf() fail {
  let leaf = tf.forced_leaf_v1(fixture.valid_native_tx_v1(), ForcedTxValid)
  let (header, membership) = forced_claim_v1(leaf)
  substrate.open_forced_leaf_raw_v1(
    header,
    cbor.serialise(membership.key),
    bytearray.concat(cbor.serialise(leaf), #"00"),
    membership.phas_root,
    membership.proof,
  ) == cbor.serialise(leaf)
}

test proof_thread_verdict_projection_preserves_all_subject_kinds() {
  and {
    substrate.subject_verdict_v1(
      substrate.open_subject_v1(
        tf.header_asset_name_v1(
          fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()).header,
        ),
        substrate.AcceptedSource {
          header: fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()).header,
          tx_inclusion: accepted_verified_v1(),
        },
      ),
    ) == ForcedTxValid,
    substrate.subject_verdict_v1(forced_accepted_subject_v1()) == ForcedTxValid,
    substrate.subject_verdict_v1(forced_rejected_subject_v1()) == ForcedTxInvalid {
      reason: input_not_found_reason_v1(),
    },
  }
}

test proof_thread_event_key_binds_source_and_identity() {
  let normal = substrate.open_subject_v1(
    tf.header_asset_name_v1(
      fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()).header,
    ),
    substrate.AcceptedSource {
      header: fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()).header,
      tx_inclusion: accepted_verified_v1(),
    },
  )
  let normal_key = ledger_state.L2TransactionEventKey {
    tx_id: normal.transaction_id,
  }
  let forced_key = ledger_state.ForcedTransactionEventKey {
    tx_order_id: tf.forced_order_key_v1(),
  }
  and {
    substrate.event_key_matches_subject_v1(normal_key, normal),
    !substrate.event_key_matches_subject_v1(forced_key, normal),
    substrate.event_key_matches_subject_v1(
      forced_key,
      forced_accepted_subject_v1(),
    ),
    !substrate.event_key_matches_subject_v1(
      normal_key,
      forced_accepted_subject_v1(),
    ),
    !substrate.event_key_matches_subject_v1(
      ledger_state.L2TransactionEventKey { tx_id: prior_root },
      normal,
    ),
  }
}

// The event door is the only bound on the selected step index: an event
// committed against a step past the header's last transition step is refused.

fn event_door_subject_v1() -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: 1,
    direction: 0,
    source_kind: 0,
    transaction_id: fixture.challenged_block_v1().tx_id,
    source_key: #"",
    rejection_reason: None,
  }
}

fn honest_event_to_step_root_v1(phas_root: ByteArray) -> ByteArray {
  transition_trace.commit_counted_root(
    transition_trace.EventToStepRootDomain,
    phas_root,
    1,
  )
}

fn event_door_step_index_v1(step_index: Int) -> Int {
  event_door_v1(
    step_index,
    event_door_subject_v1(),
    honest_event_to_step_root_v1,
  )
}

// The prover carries a genuine one-leaf PHAS membership and the header's own
// counted root, so only `header_root_of` decides whether that root commits to
// the carried `phas_root` and count.
fn event_door_v1(
  step_index: Int,
  subject: substrate.VerdictSubjectV1,
  header_root_of: fn(ByteArray) -> ByteArray,
) -> Int {
  let tx_id = fixture.challenged_block_v1().tx_id
  let key = ledger_state.L2TransactionEventKey { tx_id }
  let claim = tf.thread_claim_v1(
    key,
    ledger_state.L2Transaction,
    prior_root,
    None,
  )
  let value = ledger_state.EventToStepValue {
    step_index,
    phase: ledger_state.L2Transaction,
  }
  let phas_root = mpf.root(
    mpf.insert(mpf.empty, cbor.serialise(key), cbor.serialise(value), []),
  )
  let header = ledger_state.HeaderV1 {
    ..claim.header,
    event_to_step_root: header_root_of(phas_root),
  }
  substrate.subject_event_step_index_v1(
    header,
    subject,
    carriage_fixture.direct_carried_membership_v1(
      transition_trace.RootMembershipProof {
        ..claim.event_to_step_membership,
        root: header.event_to_step_root,
        phas_root,
        value,
      },
      0,
    ),
    [],
    [
      fixture.phas_withdraw_redeemer_entry(
        phas_root,
        cbor.serialise(key),
        cbor.serialise(value),
      ),
    ],
  )
}

test proof_thread_event_door_accepts_the_last_transition_step() {
  event_door_step_index_v1(0) == 0
}

test proof_thread_event_door_rejects_a_step_past_the_last_transition_step() fail {
  event_door_step_index_v1(1) >= 0
}

test proof_thread_event_door_rejects_a_negative_step() fail {
  event_door_step_index_v1(-1) < 1
}

// A carried counted membership must commit its PHAS root and count to the
// header root it claims; a genuine PHAS membership under a header root that
// commits some other PHAS root, or another count, is refused.

test proof_thread_event_door_rejects_a_root_committing_another_phas_root() fail {
  event_door_v1(
    0,
    event_door_subject_v1(),
    fn(_phas_root) {
      transition_trace.commit_counted_root(
        transition_trace.EventToStepRootDomain,
        prior_root,
        1,
      )
    },
  ) == 0
}

test proof_thread_event_door_rejects_a_root_committing_another_count() fail {
  event_door_v1(
    0,
    event_door_subject_v1(),
    fn(phas_root) {
      transition_trace.commit_counted_root(
        transition_trace.EventToStepRootDomain,
        phas_root,
        2,
      )
    },
  ) == 0
}

test proof_thread_subject_rejects_a_leaf_outside_the_header_forced_root() fail {
  let honest = tf.forced_leaf_v1(fixture.valid_native_tx_v1(), ForcedTxValid)
  let (header, _honest_membership) = forced_claim_v1(honest)
  let other_leaf = tf.forced_leaf_v1(
    rejected_transaction_v1(),
    ForcedTxInvalid { reason: input_not_found_reason_v1() },
  )
  let other = tf.forced_leaf_membership_v1(tf.forced_order_key_v1(), other_leaf)
  substrate.open_subject_v1(
    tf.header_asset_name_v1(header),
    substrate.ForcedSource {
      header,
      membership: transition_trace.RootMembershipProof {
        ..other,
        root: header.forced_transactions_root,
        count: header.forced_transaction_count,
      },
    },
  ).direction == substrate.direction_wrongful_rejection
}
