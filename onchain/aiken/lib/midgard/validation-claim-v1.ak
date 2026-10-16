use aiken/builtin
use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/compact.{
  forced_tx_proof_commitment_v1, native_tx_proof_commitment_v1,
  verify_forced_tx_proof_source_v1, verify_native_tx_proof_source_v1,
}
use midgard/fraud_proofs/native_tx/types.{NativeTxWitnessSetCompact}
use midgard/ledger_state
use midgard/native_tx_field_access_v1
use midgard/transition_trace
use midgard/validation_machine/witness as vm_witness
use midgard/validation_trace_v1

pub const claim_version: Int = 1

pub type ValidationSourceMembershipV1 {
  ForcedValidationSource {
    membership: transition_trace.RootMembershipProof<
      ledger_state.TxOrderId,
      ledger_state.ForcedInclusionTxV1,
    >,
  }
  NormalValidationSource {
    membership: transition_trace.RootMembershipProof<
      ByteArray,
      ledger_state.L2TransactionSourceV1,
    >,
  }
}

pub type ValidationClaimWitnessV1 {
  version: Int,
  descriptor_membership: transition_trace.RootMembershipProof<
    ledger_state.EventKey,
    validation_trace_v1.ValidationTraceDescriptorV1,
  >,
  transition_step_membership: transition_trace.RootMembershipProof<
    Int,
    ledger_state.TransitionStep,
  >,
  event_to_step_membership: transition_trace.RootMembershipProof<
    ledger_state.EventKey,
    ledger_state.EventToStepValue,
  >,
  source_membership: ValidationSourceMembershipV1,
}

const validation_context_version_v1: Int = 1

const validation_context_profile_id_v1: ByteArray =
  #"6d6964676172642d636f6e73656e7375732d7631"

/// The validation context every state of the trace commits to: the CBOR of
/// the Plutus Data list `[version, profile_id, end_time, expected_network_id,
/// min_fee_a, min_fee_b, block_slot]`. A disputable block carries L2
/// transactions, so state-queue R2 makes its `end_time` its commit time.
pub fn derive_validation_context_cbor(
  header: ledger_state.HeaderV1,
) -> ByteArray {
  builtin.serialise_data(
    builtin.list_data(
      [
        builtin.i_data(validation_context_version_v1),
        builtin.b_data(validation_context_profile_id_v1),
        builtin.i_data(header.end_time),
        builtin.i_data(header.expected_network_id),
        builtin.i_data(header.min_fee_a),
        builtin.i_data(header.min_fee_b),
        builtin.i_data(header.block_slot),
      ],
    ),
  )
}

/// State 0 of the transaction's validation trace, built from committed data
/// alone: the header's context, the event key, the authenticated source and
/// the step's pre-state root. Every other field is the machine's start value.
pub fn derive_initial_state(
  header: ledger_state.HeaderV1,
  event_key: ledger_state.EventKey,
  source: ValidationSourceMembershipV1,
  pre_utxos_root: ByteArray,
) -> validation_trace_v1.ValidationMachineStateV1 {
  let context_cbor = derive_validation_context_cbor(header)
  let (transaction_id, transaction_commitment, source_kind, first_work_witness)
    = when source is {
    ForcedValidationSource { membership } -> {
      let triple = membership.value.submitted_source
      (
        membership.value.tx_id,
        forced_tx_proof_commitment_v1(
          triple.compact_cbor,
          triple.witness_set_compact_cbor,
          triple.field_preimage_lengths_cbor,
        ),
        validation_trace_v1.Forced,
        first_field_scan_witness(
          triple.compact_cbor,
          triple.witness_set_compact_cbor,
          triple.field_preimage_lengths_cbor,
          context_cbor,
        ),
      )
    }
    NormalValidationSource { membership } -> {
      let triple = membership.value.source
      (
        membership.value.tx_id,
        native_tx_proof_commitment_v1(
          triple.compact_cbor,
          triple.witness_set_compact_cbor,
          triple.field_preimage_lengths_cbor,
        ),
        validation_trace_v1.Normal,
        first_field_scan_witness(
          triple.compact_cbor,
          triple.witness_set_compact_cbor,
          triple.field_preimage_lengths_cbor,
          context_cbor,
        ),
      )
    }
  }
  validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: blake2b_256(cbor.serialise(event_key)),
    transaction_id,
    transaction_commitment,
    validation_context_hash: validation_trace_v1.hash_validation_context(
      context_cbor,
    ),
    source_kind,
    prior_ledger_root: pre_utxos_root,
    phase: validation_trace_v1.CanonicalDecode,
    program_counter: 0,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.CanonicalDecode,
      0,
      first_work_witness,
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: validation_trace_v1.no_rejection_code_hash,
  }
}

/// The machine's first work witness: the field scan at field 0, item 0, chunk
/// 0, before any item has been counted.
fn first_field_scan_witness(
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  context_cbor: ByteArray,
) -> ByteArray {
  vm_witness.encode_transaction_field_scan_witness(
    compact_cbor,
    witness_set_compact_cbor,
    field_preimage_lengths_cbor,
    context_cbor,
    0,
    0,
    0,
    -1,
    0,
  )
}

fn verify_descriptor_membership(
  header: ledger_state.HeaderV1,
  membership: transition_trace.RootMembershipProof<
    ledger_state.EventKey,
    validation_trace_v1.ValidationTraceDescriptorV1,
  >,
) -> Bool {
  transition_trace.verify_root_membership_with_bytes(
    membership,
    transition_trace.ValidationTracesRootDomain,
    header.validation_traces_root,
    header.validation_trace_count,
    cbor.serialise(membership.key),
    cbor.serialise(membership.value),
  )
}

fn verify_transition_step_membership(
  header: ledger_state.HeaderV1,
  membership: transition_trace.RootMembershipProof<
    Int,
    ledger_state.TransitionStep,
  >,
) -> Bool {
  and {
    transition_trace.verify_root_membership_with_bytes(
      membership,
      transition_trace.TransitionTraceRootDomain,
      header.transition_trace_root,
      header.transition_step_count,
      cbor.serialise(membership.key),
      cbor.serialise(membership.value),
    ),
    membership.key == membership.value.step_index,
    membership.key >= 0,
    membership.key < membership.count,
    membership.value.schema_version == ledger_state.transition_step_schema_version_v1,
  }
}

fn verify_event_to_step_membership(
  header: ledger_state.HeaderV1,
  membership: transition_trace.RootMembershipProof<
    ledger_state.EventKey,
    ledger_state.EventToStepValue,
  >,
) -> Bool {
  transition_trace.verify_root_membership_with_bytes(
    membership,
    transition_trace.EventToStepRootDomain,
    header.event_to_step_root,
    header.total_event_count,
    cbor.serialise(membership.key),
    cbor.serialise(membership.value),
  )
}

/// The source's compact witness set when the source is authenticated under the
/// header, `None` otherwise.
fn authenticated_source_witness_set(
  header: ledger_state.HeaderV1,
  event_key: ledger_state.EventKey,
  source: ValidationSourceMembershipV1,
) -> Option<NativeTxWitnessSetCompact> {
  when source is {
    ForcedValidationSource { membership } -> {
      expect ledger_state.ForcedTransactionEventKey { tx_order_id } = event_key
      let Pair(verified, witness_set) = verify_forced_tx_proof_source_v1(
        membership.value.tx_id,
        membership.value.submitted_source.compact_cbor,
        membership.value.submitted_source.witness_set_compact_cbor,
        membership.value.submitted_source.field_preimage_lengths_cbor,
      )
      let authenticated = and {
        transition_trace.verify_root_membership_with_bytes(
          membership,
          transition_trace.ForcedTransactionsV1RootDomain,
          header.forced_transactions_root,
          header.forced_transaction_count,
          cbor.serialise(membership.key),
          cbor.serialise(membership.value),
        ),
        membership.key == tx_order_id,
        verified.version == 1,
      }
      if authenticated {
        Some(witness_set)
      } else {
        None
      }
    }

    NormalValidationSource { membership } -> {
      expect ledger_state.L2TransactionEventKey { tx_id } = event_key
      let Pair(verified, witness_set) = verify_native_tx_proof_source_v1(
        tx_id,
        membership.value.source.compact_cbor,
        membership.value.source.witness_set_compact_cbor,
        membership.value.source.field_preimage_lengths_cbor,
      )
      let authenticated = and {
        transition_trace.verify_root_membership_with_bytes(
          membership,
          transition_trace.TransactionsV1RootDomain,
          header.transactions_root,
          header.l2_transaction_count,
          membership.key,
          cbor.serialise(membership.value),
        ),
        membership.key == membership.value.tx_id,
        membership.value.tx_id == tx_id,
        verified.version == 1,
        // §2.4.3(d) Normal-source authoritativeness: a Normal leaf under
        // `transactions_root` IS an acceptance verdict — its embedded
        // validity scalar must claim `TxIsValid` (0).
        verified.tx_compact.validity_code == Some(0),
      }
      if authenticated {
        Some(witness_set)
      } else {
        None
      }
    }
  }
}

fn phase_for_event_key(
  event_key: ledger_state.EventKey,
) -> ledger_state.TransitionPhase {
  when event_key is {
    ledger_state.WithdrawalEventKey { .. } -> ledger_state.Withdrawal
    ledger_state.ForcedTransactionEventKey { .. } ->
      ledger_state.ForcedTransaction
    ledger_state.L2TransactionEventKey { .. } -> ledger_state.L2Transaction
    ledger_state.DepositEventKey { .. } -> ledger_state.Deposit
  }
}

/// Authenticates the block-owned roots and the transition location only. The
/// initial-state and verdict checks deliberately happen in the following
/// source step, so a committed initial state that is not the derived one is
/// itself a unilateral fault instead of making an honest challenge impossible
/// to open.
pub fn committed_claim_structure_is_valid(
  header: ledger_state.HeaderV1,
  witness: ValidationClaimWitnessV1,
) -> Bool {
  let event_key = witness.descriptor_membership.key
  let transition_step = witness.transition_step_membership.value
  let event_to_step = witness.event_to_step_membership.value

  and {
    witness.version == claim_version,
    header.protocol_version == ledger_state.protocol_version_v1,
    verify_descriptor_membership(header, witness.descriptor_membership),
    verify_transition_step_membership(
      header,
      witness.transition_step_membership,
    ),
    verify_event_to_step_membership(header, witness.event_to_step_membership),
    witness.transition_step_membership.key == event_to_step.step_index,
    witness.event_to_step_membership.key == event_key,
    transition_step.event_key == event_key,
    transition_step.phase == phase_for_event_key(event_key),
    transition_step.phase == event_to_step.phase,
  }
}

/// The authenticated source's compact witness set, or `None` when the source
/// is not authenticated under the header.
pub fn committed_claim_source_witness_set(
  header: ledger_state.HeaderV1,
  witness: ValidationClaimWitnessV1,
) -> Option<NativeTxWitnessSetCompact> {
  authenticated_source_witness_set(
    header,
    witness.descriptor_membership.key,
    witness.source_membership,
  )
}

pub fn committed_claim_source_is_authenticated(
  header: ledger_state.HeaderV1,
  witness: ValidationClaimWitnessV1,
) -> Bool {
  when committed_claim_source_witness_set(header, witness) is {
    Some(_) -> True
    None -> False
  }
}

/// Rule R-DESC: only a normal transaction with a non-empty redeemer field
/// (witness-set field 8) runs Plutus, so only it has a real trace to dispute.
/// Decided from the authenticated source bytes, never from the descriptor.
pub fn source_requires_trace(
  source: ValidationSourceMembershipV1,
  witness_set: NativeTxWitnessSetCompact,
) -> Bool {
  when source is {
    ForcedValidationSource { .. } -> False
    NormalValidationSource { .. } ->
      witness_set.redeemer_tx_wits_hash != native_tx_field_access_v1.empty_field_commitment
  }
}

/// The operator descriptor's committed initial state is the derived one, so
/// the dispute's agreed low state (the descriptor's `initial_state_hash`) is
/// the derived state's hash.
///
/// The header and step guards come first: they keep a context or root the
/// machine cannot represent a refusal (the challenger wins) rather than a
/// failure inside the state encoder. `block_slot` needs no guard (state-queue
/// R3 bounds it to the slot domain), and the source's `tx_id` is 32 bytes once
/// `committed_claim_source_is_authenticated` holds.
pub fn committed_initial_state_is_derived(
  header: ledger_state.HeaderV1,
  witness: ValidationClaimWitnessV1,
) -> Bool {
  let transition_step = witness.transition_step_membership.value
  and {
    header.expected_network_id == 0 || header.expected_network_id == 1,
    header.min_fee_a >= 0,
    header.min_fee_b >= 0,
    bytearray.length(transition_step.pre_utxos_root) == 32,
    witness.descriptor_membership.value.initial_state_hash == validation_trace_v1.hash_machine_state(
      derive_initial_state(
        header,
        transition_step.event_key,
        witness.source_membership,
        transition_step.pre_utxos_root,
      ),
    ),
  }
}

pub fn committed_claim_endpoints_and_source_are_valid(
  header: ledger_state.HeaderV1,
  witness: ValidationClaimWitnessV1,
) -> Bool {
  let descriptor = witness.descriptor_membership.value
  let transition_step = witness.transition_step_membership.value
  and {
    committed_initial_state_is_derived(header, witness),
    if descriptor.verdict == validation_trace_v1.Rejected {
      transition_step.pre_utxos_root == transition_step.post_utxos_root
    } else {
      True
    },
  }
}

pub fn committed_claim_is_valid(
  header: ledger_state.HeaderV1,
  witness: ValidationClaimWitnessV1,
) -> Bool {
  and {
    committed_claim_structure_is_valid(header, witness),
    committed_claim_source_is_authenticated(header, witness),
    committed_claim_endpoints_and_source_are_valid(header, witness),
  }
}
