//// Focused ABI and mutation selectors for the shared non-interactive
//// proof-thread verdict-subject substrate.

use aiken/cbor
use aiken/primitive/bytearray
use cardano/transaction
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_script_decoding/thread_fixture_v1 as tf
use midgard/fraud_proofs/native_tx/compact.{verify_native_tx_proof_source_v1}
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/ledger_state
use midgard/rejection_reason_v1.{ForcedTxInvalid, ForcedTxValid}
use midgard/transition_trace

const prior_root =
  #"5555555555555555555555555555555555555555555555555555555555555555"

fn accepted_verified_v1() -> native_types.VerifiedMidgardNativeTxCompact {
  let leaf = tf.forced_leaf_v1(fixture.valid_native_tx_v1(), ForcedTxValid)
  let Pair(verified, _witness_set) = verify_native_tx_proof_source_v1(
    leaf.tx_id,
    leaf.source.compact_cbor,
    leaf.source.witness_set_compact_cbor,
    leaf.source.field_preimage_lengths_cbor,
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
  substrate.bind_forced_subject_to_thread_v1(
    tf.header_asset_name_v1(header),
    header,
    membership,
    substrate.direction_wrongful_acceptance,
  )
}

fn forced_rejected_subject_v1() -> substrate.VerdictSubjectV1 {
  let leaf = tf.forced_leaf_v1(
    rejected_transaction_v1(),
    ForcedTxInvalid { reason: input_not_found_reason_v1() },
  )
  let (header, membership) = forced_claim_v1(leaf)
  substrate.bind_forced_subject_to_thread_v1(
    tf.header_asset_name_v1(header),
    header,
    membership,
    substrate.direction_wrongful_rejection,
  )
}

// ## Binding successes and source/direction mutations

test proof_thread_subject_binds_an_accepted_transaction() {
  let subject = substrate.bind_accepted_subject_v1(accepted_verified_v1())
  and {
    substrate.subject_is_canonical_v1(subject),
    subject.direction == substrate.direction_wrongful_acceptance,
    subject.source_kind == substrate.source_kind_accepted,
    subject.source_key == #"",
    subject.rejection_reason == None,
  }
}

test proof_thread_subject_rejects_an_accepted_transaction_with_invalid_scalar() fail {
  substrate.bind_accepted_subject_v1(
    native_types.VerifiedMidgardNativeTxCompact {
      ..accepted_verified_v1(),
      tx_compact: native_types.NativeTxCompact {
        ..accepted_verified_v1().tx_compact,
        validity_code: 1,
      },
    },
  ) == substrate.bind_accepted_subject_v1(accepted_verified_v1())
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
  let leaf = tf.forced_leaf_v1(fixture.valid_native_tx_v1(), ForcedTxValid)
  let (header, membership) = forced_claim_v1(leaf)
  substrate.bind_forced_subject_v1(
    header,
    membership,
    substrate.direction_wrongful_rejection,
  ) == forced_accepted_subject_v1()
}

test proof_thread_subject_rejects_wrongful_acceptance_on_a_rejecting_leaf() fail {
  let leaf = tf.forced_leaf_v1(
    rejected_transaction_v1(),
    ForcedTxInvalid { reason: input_not_found_reason_v1() },
  )
  let (header, membership) = forced_claim_v1(leaf)
  substrate.bind_forced_subject_v1(
    header,
    membership,
    substrate.direction_wrongful_acceptance,
  ) == forced_rejected_subject_v1()
}

test proof_thread_subject_rejects_forced_valid_with_an_invalid_scalar() fail {
  let leaf = tf.forced_leaf_v1(rejected_transaction_v1(), ForcedTxValid)
  let (header, membership) = forced_claim_v1(leaf)
  substrate.bind_forced_subject_v1(
    header,
    membership,
    substrate.direction_wrongful_acceptance,
  ) == forced_accepted_subject_v1()
}

test proof_thread_subject_rejects_forced_invalid_with_a_valid_scalar() fail {
  let leaf = tf.forced_leaf_v1(
    fixture.valid_native_tx_v1(),
    ForcedTxInvalid { reason: input_not_found_reason_v1() },
  )
  let (header, membership) = forced_claim_v1(leaf)
  substrate.bind_forced_subject_v1(
    header,
    membership,
    substrate.direction_wrongful_rejection,
  ) == forced_rejected_subject_v1()
}

test proof_thread_subject_rejects_an_unknown_direction() fail {
  let leaf = tf.forced_leaf_v1(fixture.valid_native_tx_v1(), ForcedTxValid)
  let (header, membership) = forced_claim_v1(leaf)
  substrate.bind_forced_subject_v1(header, membership, 2) == forced_accepted_subject_v1()
}

test proof_thread_subject_rejects_a_substituted_forced_root() fail {
  let leaf = tf.forced_leaf_v1(fixture.valid_native_tx_v1(), ForcedTxValid)
  let (header, membership) = forced_claim_v1(leaf)
  substrate.bind_forced_subject_v1(
    ledger_state.HeaderV1 { ..header, forced_transactions_root: prior_root },
    membership,
    substrate.direction_wrongful_acceptance,
  ) == forced_accepted_subject_v1()
}

test proof_thread_subject_rejects_a_substituted_thread_header() fail {
  let leaf = tf.forced_leaf_v1(fixture.valid_native_tx_v1(), ForcedTxValid)
  let (header, membership) = forced_claim_v1(leaf)
  substrate.bind_forced_subject_to_thread_v1(
    tf.header_asset_name_v1(header),
    ledger_state.HeaderV1 { ..header, prev_utxos_root: tf.h32_d },
    membership,
    substrate.direction_wrongful_acceptance,
  ) == forced_accepted_subject_v1()
}

test proof_thread_subject_rejects_a_substituted_forced_key() fail {
  let leaf = tf.forced_leaf_v1(fixture.valid_native_tx_v1(), ForcedTxValid)
  let (header, membership) = forced_claim_v1(leaf)
  let substituted_key = transaction.OutputReference {
    transaction_id: prior_root,
    output_index: 0,
  }
  substrate.bind_forced_subject_v1(
    header,
    transition_trace.RootMembershipProof { ..membership, key: substituted_key },
    substrate.direction_wrongful_acceptance,
  ) == forced_accepted_subject_v1()
}

test proof_thread_subject_rejects_substituted_forced_source_bytes() fail {
  let leaf = tf.forced_leaf_v1(fixture.valid_native_tx_v1(), ForcedTxValid)
  let (header, membership) = forced_claim_v1(leaf)
  let substituted_leaf = ledger_state.ForcedInclusionTxV1 {
    ..leaf,
    source: ledger_state.NativeTxProofSourceV1 {
      ..leaf.source,
      compact_cbor: bytearray.concat(leaf.source.compact_cbor, #"00"),
    },
  }
  substrate.bind_forced_subject_v1(
    header,
    transition_trace.RootMembershipProof {
      ..membership,
      value: substituted_leaf,
    },
    substrate.direction_wrongful_acceptance,
  ) == forced_accepted_subject_v1()
}

test proof_thread_subject_rejects_a_substituted_transaction_id() fail {
  let leaf = tf.forced_leaf_v1(fixture.valid_native_tx_v1(), ForcedTxValid)
  let (header, membership) = forced_claim_v1(leaf)
  let substituted_leaf = ledger_state.ForcedInclusionTxV1 {
    ..leaf,
    tx_id: prior_root,
  }
  substrate.bind_forced_subject_v1(
    header,
    transition_trace.RootMembershipProof {
      ..membership,
      value: substituted_leaf,
    },
    substrate.direction_wrongful_acceptance,
  ) == forced_accepted_subject_v1()
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
      substrate.bind_accepted_subject_v1(accepted_verified_v1()),
      True,
    ),
    substrate.terminal_contradiction_v1(forced_accepted_subject_v1(), True),
    substrate.terminal_contradiction_v1(forced_rejected_subject_v1(), False),
  }
}

test proof_thread_terminal_refuses_an_honest_acceptance() {
  !substrate.terminal_contradiction_v1(
    substrate.bind_accepted_subject_v1(accepted_verified_v1()),
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
    substrate.bind_accepted_subject_v1(accepted_verified_v1()),
  ) == #"860100005820d4eafc0f14a743a05c3e5cc1aa36f26b420ec12a8a3c4473c5a458cd04c11f234080"
}

test proof_thread_subject_forced_accepted_v1() {
  substrate.encode_verdict_subject_v1(forced_accepted_subject_v1()) == #"860100015820d4eafc0f14a743a05c3e5cc1aa36f26b420ec12a8a3c4473c5a458cd04c11f235827d8799f5820777777777777777777777777777777777777777777777777777777777777777700ff80"
}

test proof_thread_subject_forced_rejected_input_not_found_v1() {
  substrate.encode_verdict_subject_v1(forced_rejected_subject_v1()) == #"860101015820d4eafc0f14a743a05c3e5cc1aa36f26b420ec12a8a3c4473c5a458cd04c11f235827d8799f5820777777777777777777777777777777777777777777777777777777777777777700ff81d9050b9f0102ff"
}
