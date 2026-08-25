//// #640 gate-5 tests: the §2.4.2(c) verdict bridge (`forced_verdict_matches`)
//// and the §2.4.3(d)/(e) source-authoritativeness predicates, each exercised
//// positively and negatively.
////
//// The predicate tests drive the public door
//// `committed_claim_source_is_authenticated`, which reads only the descriptor
//// membership's key and the source membership — every other witness field is
//// an inert dummy here on purpose.

use aiken/cbor
use aiken/merkle_patricia_forestry as mpf
use cardano/transaction.{OutputReference}
use env
use midgard/fraud_proofs/native_tx/compact as native_compact
use midgard/fraud_proofs/native_tx/transaction as native_transaction
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/ledger_state
use midgard/rejection_reason_v1
use midgard/transition_trace
use midgard/validation_claim_v1
use midgard/validation_trace_v1

const h28_a = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const h32_a =
  #"1111111111111111111111111111111111111111111111111111111111111111"

const h32_b =
  #"2222222222222222222222222222222222222222222222222222222222222222"

const zero_hash =
  #"0000000000000000000000000000000000000000000000000000000000000000"

// ## §2.4.2(c) bridge: `forced_verdict_matches`

fn descriptor_with(
  verdict: validation_trace_v1.ValidationVerdict,
  rejection_code_hash: ByteArray,
) -> validation_trace_v1.ValidationTraceDescriptorV1 {
  validation_trace_v1.ValidationTraceDescriptorV1 {
    schema_version: 1,
    machine_version: 1,
    trace_root: h32_a,
    step_count: 1,
    initial_state_hash: h32_a,
    terminal_state_hash: h32_b,
    verdict,
    rejection_code_hash,
  }
}

test forced_verdict_matches_accepts_valid_leaf_on_accepted_descriptor() {
  validation_claim_v1.forced_verdict_matches(
    rejection_reason_v1.ForcedTxValid,
    descriptor_with(validation_trace_v1.Accepted, zero_hash),
  )
}

test forced_verdict_matches_rejects_valid_leaf_on_rejected_descriptor() {
  !validation_claim_v1.forced_verdict_matches(
    rejection_reason_v1.ForcedTxValid,
    descriptor_with(
      validation_trace_v1.Rejected,
      validation_trace_v1.hash_rejection_code(
        rejection_reason_v1.reject_min_fee,
      ),
    ),
  )
}

test forced_verdict_matches_accepts_invalid_leaf_with_matching_code_hash() {
  validation_claim_v1.forced_verdict_matches(
    rejection_reason_v1.ForcedTxInvalid {
      reason: rejection_reason_v1.FeeBelowMinimum,
    },
    descriptor_with(
      validation_trace_v1.Rejected,
      validation_trace_v1.hash_rejection_code(
        rejection_reason_v1.reject_min_fee,
      ),
    ),
  )
}

test forced_verdict_matches_rejects_invalid_leaf_with_wrong_code_hash() {
  !validation_claim_v1.forced_verdict_matches(
    rejection_reason_v1.ForcedTxInvalid {
      reason: rejection_reason_v1.FeeBelowMinimum,
    },
    descriptor_with(
      validation_trace_v1.Rejected,
      validation_trace_v1.hash_rejection_code(
        rejection_reason_v1.reject_min_ada,
      ),
    ),
  )
}

test forced_verdict_matches_rejects_invalid_leaf_on_accepted_descriptor() {
  !validation_claim_v1.forced_verdict_matches(
    rejection_reason_v1.ForcedTxInvalid {
      reason: rejection_reason_v1.PlutusExecutionFailed { execution_index: 0 },
    },
    descriptor_with(
      validation_trace_v1.Accepted,
      validation_trace_v1.hash_rejection_code(
        rejection_reason_v1.reject_plutus_script_invalid,
      ),
    ),
  )
}

// ## Fixtures for the §2.4.3(d)/(e) predicate tests

fn native_tx_with(
  validity: native_types.MidgardTxValidity,
) -> native_types.MidgardTransaction {
  let empty_mint: Data = []
  native_types.MidgardTransaction {
    version: native_types.native_tx_version_v1,
    validity,
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

fn native_tx_id_of(tx: native_types.MidgardTransaction) -> ByteArray {
  let compact = native_transaction.midgard_transaction_to_compact(tx)
  native_compact.native_tx_id_for_version(
    native_types.native_tx_version_v1,
    native_compact.encode_native_tx_body_compact(compact.body),
  )
}

fn proof_source_of(
  tx: native_types.MidgardTransaction,
) -> ledger_state.NativeTxProofSourceV1 {
  let proof_source = native_transaction.midgard_transaction_proof_source_v1(tx)
  ledger_state.NativeTxProofSourceV1 {
    compact_cbor: proof_source.compact_cbor,
    witness_set_compact_cbor: proof_source.witness_set_compact_cbor,
    field_preimage_lengths_cbor: proof_source.field_preimage_lengths_cbor,
  }
}

fn tx_order_id() -> ledger_state.TxOrderId {
  OutputReference { transaction_id: h32_a, output_index: 0 }
}

fn forced_source(
  validity: native_types.MidgardTxValidity,
  verdict: rejection_reason_v1.OperatorVerdictV1,
) -> validation_claim_v1.ValidationSourceMembershipV1 {
  let tx = native_tx_with(validity)
  let forced = ledger_state.ForcedInclusionTxV1 {
    tx_id: native_tx_id_of(tx),
    source: proof_source_of(tx),
    verdict,
  }
  let key_bytes = cbor.serialise(tx_order_id())
  let value_bytes = cbor.serialise(forced)
  let phas_root = mpf.root(mpf.insert(mpf.empty, key_bytes, value_bytes, []))
  validation_claim_v1.ForcedValidationSource {
    membership: transition_trace.RootMembershipProof {
      domain: transition_trace.ForcedTransactionsV1RootDomain,
      root: transition_trace.commit_counted_root(
        transition_trace.ForcedTransactionsV1RootDomain,
        phas_root,
        1,
      ),
      phas_root,
      count: 1,
      key: tx_order_id(),
      value: forced,
      proof: [],
    },
  }
}

fn normal_source(
  validity: native_types.MidgardTxValidity,
) -> validation_claim_v1.ValidationSourceMembershipV1 {
  let tx = native_tx_with(validity)
  let tx_id = native_tx_id_of(tx)
  let source = ledger_state.L2TransactionSourceV1 {
    tx_id,
    source: proof_source_of(tx),
  }
  let value_bytes = cbor.serialise(source)
  let phas_root = mpf.root(mpf.insert(mpf.empty, tx_id, value_bytes, []))
  validation_claim_v1.NormalValidationSource {
    membership: transition_trace.RootMembershipProof {
      domain: transition_trace.TransactionsV1RootDomain,
      root: transition_trace.commit_counted_root(
        transition_trace.TransactionsV1RootDomain,
        phas_root,
        1,
      ),
      phas_root,
      count: 1,
      key: tx_id,
      value: source,
      proof: [],
    },
  }
}

fn source_root(
  source: validation_claim_v1.ValidationSourceMembershipV1,
) -> ByteArray {
  when source is {
    validation_claim_v1.ForcedValidationSource { membership } -> membership.root
    validation_claim_v1.NormalValidationSource { membership } -> membership.root
  }
}

fn header_for(
  source: validation_claim_v1.ValidationSourceMembershipV1,
) -> ledger_state.HeaderV1 {
  let base = ledger_state.HeaderV1 {
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
    prev_header_hash: h28_a,
    operator_vkey: h28_a,
    protocol_version: ledger_state.protocol_version_v1,
  }
  when source is {
    validation_claim_v1.ForcedValidationSource { .. } ->
      ledger_state.HeaderV1 {
        ..base,
        forced_transactions_root: source_root(source),
        forced_transaction_count: 1,
      }
    validation_claim_v1.NormalValidationSource { .. } ->
      ledger_state.HeaderV1 {
        ..base,
        transactions_root: source_root(source),
        l2_transaction_count: 1,
      }
  }
}

fn event_key_for(
  source: validation_claim_v1.ValidationSourceMembershipV1,
) -> ledger_state.EventKey {
  when source is {
    validation_claim_v1.ForcedValidationSource { .. } ->
      ledger_state.ForcedTransactionEventKey { tx_order_id: tx_order_id() }
    validation_claim_v1.NormalValidationSource { membership } ->
      ledger_state.L2TransactionEventKey { tx_id: membership.key }
  }
}

fn dummy_machine_state() -> validation_trace_v1.ValidationMachineStateV1 {
  validation_trace_v1.ValidationMachineStateV1 {
    machine_version: 1,
    event_key_hash: zero_hash,
    transaction_id: zero_hash,
    transaction_commitment: zero_hash,
    validation_context_hash: zero_hash,
    source_kind: validation_trace_v1.Forced,
    prior_ledger_root: zero_hash,
    phase: validation_trace_v1.CanonicalDecode,
    program_counter: 0,
    work_root: zero_hash,
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero_hash,
    ledger_delta_root: zero_hash,
  }
}

fn dummy_trace_proof() -> validation_trace_v1.ValidationTraceProof {
  validation_trace_v1.ValidationTraceProof {
    state_index: 0,
    state_hash: zero_hash,
    siblings: [],
  }
}

/// A witness whose only live fields are the descriptor membership's key and
/// the source membership — exactly what
/// `committed_claim_source_is_authenticated` reads.
fn witness_for(
  source: validation_claim_v1.ValidationSourceMembershipV1,
) -> validation_claim_v1.ValidationClaimWitnessV1 {
  let event_key = event_key_for(source)
  validation_claim_v1.ValidationClaimWitnessV1 {
    version: validation_claim_v1.claim_version,
    descriptor_membership: transition_trace.RootMembershipProof {
      domain: transition_trace.ValidationTracesRootDomain,
      root: zero_hash,
      phas_root: zero_hash,
      count: 1,
      key: event_key,
      value: descriptor_with(validation_trace_v1.Accepted, zero_hash),
      proof: [],
    },
    transition_step_membership: transition_trace.RootMembershipProof {
      domain: transition_trace.TransitionTraceRootDomain,
      root: zero_hash,
      phas_root: zero_hash,
      count: 1,
      key: 0,
      value: ledger_state.TransitionStep {
        schema_version: ledger_state.transition_step_schema_version_v1,
        step_index: 0,
        event_key,
        phase: ledger_state.ForcedTransaction,
        pre_utxos_root: h32_a,
        post_utxos_root: h32_a,
      },
      proof: [],
    },
    event_to_step_membership: transition_trace.RootMembershipProof {
      domain: transition_trace.EventToStepRootDomain,
      root: zero_hash,
      phas_root: zero_hash,
      count: 1,
      key: event_key,
      value: ledger_state.EventToStepValue {
        step_index: 0,
        phase: ledger_state.ForcedTransaction,
      },
      proof: [],
    },
    source_membership: source,
    validation_context_cbor: #"",
    initial_state: dummy_machine_state(),
    terminal_state: dummy_machine_state(),
    initial_state_proof: dummy_trace_proof(),
    terminal_state_proof: dummy_trace_proof(),
  }
}

fn source_is_authenticated(
  source: validation_claim_v1.ValidationSourceMembershipV1,
) -> Bool {
  validation_claim_v1.committed_claim_source_is_authenticated(
    header_for(source),
    witness_for(source),
  )
}

// ## §2.4.3(e) Forced-source authoritativeness (leaf-internal bit equality)

test forced_source_accepts_valid_verdict_with_validity_code_zero() {
  source_is_authenticated(
    forced_source(native_types.TxIsValid, rejection_reason_v1.ForcedTxValid),
  )
}

test forced_source_rejects_valid_verdict_with_validity_code_one() {
  !source_is_authenticated(
    forced_source(native_types.TxIsInvalid, rejection_reason_v1.ForcedTxValid),
  )
}

test forced_source_accepts_invalid_verdict_with_validity_code_one() {
  source_is_authenticated(
    forced_source(
      native_types.TxIsInvalid,
      rejection_reason_v1.ForcedTxInvalid {
        reason: rejection_reason_v1.PlutusExecutionFailed { execution_index: 0 },
      },
    ),
  )
}

test forced_source_rejects_invalid_verdict_with_validity_code_zero() {
  !source_is_authenticated(
    forced_source(
      native_types.TxIsValid,
      rejection_reason_v1.ForcedTxInvalid {
        reason: rejection_reason_v1.FeeBelowMinimum,
      },
    ),
  )
}

// ## §2.4.3(d) Normal-source authoritativeness (acceptance scalar)

test normal_source_accepts_validity_code_zero() {
  source_is_authenticated(normal_source(native_types.TxIsValid))
}

test normal_source_rejects_validity_code_one() {
  !source_is_authenticated(normal_source(native_types.TxIsInvalid))
}
