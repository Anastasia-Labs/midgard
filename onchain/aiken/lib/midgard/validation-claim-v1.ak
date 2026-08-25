use aiken/builtin
use aiken/cbor
use aiken/crypto.{blake2b_256}
use midgard/fraud_proofs/native_tx/compact.{
  native_tx_proof_commitment_v1, verify_native_tx_proof_source_v1,
}
use midgard/ledger_state
use midgard/rejection_reason_v1.{ForcedTxInvalid, ForcedTxValid}
use midgard/transition_trace
use midgard/validation_machine_v1
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
  validation_context_cbor: ByteArray,
  initial_state: validation_trace_v1.ValidationMachineStateV1,
  terminal_state: validation_trace_v1.ValidationMachineStateV1,
  initial_state_proof: validation_trace_v1.ValidationTraceProof,
  terminal_state_proof: validation_trace_v1.ValidationTraceProof,
}

const validation_context_version_v1: Int = 1

const validation_context_profile_id_v1: ByteArray =
  #"6d6964676172642d636f6e73656e7375732d7631"

fn validation_context_is_exact(
  header: ledger_state.HeaderV1,
  context_cbor: ByteArray,
  initial: validation_trace_v1.ValidationMachineStateV1,
) -> Bool {
  expect Some(context_data) = cbor.deserialise(context_cbor)
  expect
      [
        version_data, profile_id_data, block_end_time_data,
        expected_network_id_data, min_fee_a_data, min_fee_b_data,
        block_slot_data,
      ]
    = builtin.un_list_data(context_data)
  let version = builtin.un_i_data(version_data)
  let profile_id = builtin.un_b_data(profile_id_data)
  let block_end_time = builtin.un_i_data(block_end_time_data)
  let expected_network_id = builtin.un_i_data(expected_network_id_data)
  let min_fee_a = builtin.un_i_data(min_fee_a_data)
  let min_fee_b = builtin.un_i_data(min_fee_b_data)
  let block_slot = builtin.un_i_data(block_slot_data)
  and {
    cbor.serialise(context_data) == context_cbor,
    version == validation_context_version_v1,
    profile_id == validation_context_profile_id_v1,
    block_end_time == header.end_time,
    expected_network_id == header.expected_network_id,
    expected_network_id == 0 || expected_network_id == 1,
    min_fee_a == header.min_fee_a,
    min_fee_a >= 0,
    min_fee_b == header.min_fee_b,
    min_fee_b >= 0,
    block_slot == header.block_slot,
    block_slot >= 0,
    validation_trace_v1.hash_validation_context(context_cbor) == initial.validation_context_hash,
  }
}

fn initial_work_root_is_exact(
  initial: validation_trace_v1.ValidationMachineStateV1,
  context_cbor: ByteArray,
  source: ValidationSourceMembershipV1,
) -> Bool {
  let first_work_witness = when source is {
    ForcedValidationSource { membership } ->
      validation_machine_v1.encode_transaction_field_scan_witness(
        membership.value.source.compact_cbor,
        membership.value.source.witness_set_compact_cbor,
        membership.value.source.field_preimage_lengths_cbor,
        context_cbor,
        0,
        0,
        0,
        -1,
        0,
      )
    NormalValidationSource { membership } ->
      validation_machine_v1.encode_transaction_field_scan_witness(
        membership.value.source.compact_cbor,
        membership.value.source.witness_set_compact_cbor,
        membership.value.source.field_preimage_lengths_cbor,
        context_cbor,
        0,
        0,
        0,
        -1,
        0,
      )
  }
  initial.work_root == validation_trace_v1.hash_work_witness(
    validation_trace_v1.CanonicalDecode,
    0,
    first_work_witness,
  )
}

fn immutable_context_matches(
  initial: validation_trace_v1.ValidationMachineStateV1,
  terminal: validation_trace_v1.ValidationMachineStateV1,
) -> Bool {
  and {
    initial.machine_version == terminal.machine_version,
    initial.event_key_hash == terminal.event_key_hash,
    initial.transaction_id == terminal.transaction_id,
    initial.transaction_commitment == terminal.transaction_commitment,
    initial.validation_context_hash == terminal.validation_context_hash,
    initial.source_kind == terminal.source_kind,
    initial.prior_ledger_root == terminal.prior_ledger_root,
    initial.ledger_delta_root == terminal.ledger_delta_root,
  }
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

/// The §2.4.2(c) bridge from the forced leaf's verdict to the FROZEN
/// descriptor format: an accepting leaf demands an accepting descriptor; a
/// rejecting leaf demands a rejecting descriptor whose committed
/// `rejection_code_hash` is exactly the hash of the legacy `E_*` code the
/// leaf's reason maps back to (`rejection_code_of`, the total 47 → 19 map).
///
/// Exported so the #640 gate-5 tests can drive the bridge directly; the only
/// production caller remains `source_binding_is_exact` in this module.
pub fn forced_verdict_matches(
  leaf_verdict: rejection_reason_v1.OperatorVerdictV1,
  descriptor: validation_trace_v1.ValidationTraceDescriptorV1,
) -> Bool {
  when leaf_verdict is {
    ForcedTxValid -> descriptor.verdict == validation_trace_v1.Accepted
    ForcedTxInvalid { reason } -> and {
        descriptor.verdict == validation_trace_v1.Rejected,
        validation_trace_v1.hash_rejection_code(
          rejection_reason_v1.rejection_code_of(reason),
        ) == descriptor.rejection_code_hash,
      }
  }
}

fn verify_source_authentication(
  header: ledger_state.HeaderV1,
  event_key: ledger_state.EventKey,
  source: ValidationSourceMembershipV1,
) -> Bool {
  when source is {
    ForcedValidationSource { membership } -> {
      expect ledger_state.ForcedTransactionEventKey { tx_order_id } = event_key
      let Pair(verified, _witness_set) = verify_native_tx_proof_source_v1(
        membership.value.tx_id,
        membership.value.source.compact_cbor,
        membership.value.source.witness_set_compact_cbor,
        membership.value.source.field_preimage_lengths_cbor,
      )
      and {
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
        // §2.4.3(e) Forced-source authoritativeness, leaf-internal: the
        // leaf's verdict must agree with the embedded validity scalar of the
        // same leaf's own compact bytes (post-wave bit equality; never a
        // cross-root opening).
        when membership.value.verdict is {
          ForcedTxValid -> verified.tx_compact.validity_code == 0
          ForcedTxInvalid { .. } -> verified.tx_compact.validity_code == 1
        },
      }
    }
    NormalValidationSource { membership } -> {
      expect ledger_state.L2TransactionEventKey { tx_id } = event_key
      let Pair(verified, _witness_set) = verify_native_tx_proof_source_v1(
        tx_id,
        membership.value.source.compact_cbor,
        membership.value.source.witness_set_compact_cbor,
        membership.value.source.field_preimage_lengths_cbor,
      )
      and {
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
        verified.tx_compact.validity_code == 0,
      }
    }
  }
}

/// What the machine's `transaction_commitment` is anchored to, now that the
/// committed source no longer carries one.
///
/// §4's flat reversion left `L2TransactionSourceV1.transaction_commitment` and
/// its `ForcedInclusionTxV1` twin with exactly one reader — this file — and that
/// reader used the carried value twice over: once to assert it equalled
/// `native_tx_proof_commitment_v1` of the very triple it sat beside, and once to
/// bind it to the machine state. The first assertion made the second one's
/// subject the derivation rather than the field, so retiring the field changes
/// nothing about what is anchored: the machine's `transaction_commitment` is
/// still the §7 proof-source commitment of the source membership's own triple.
/// It is now derived at the one place it is used instead of carried on every
/// leaf of two block-committed roots and checked back against its own
/// derivation.
///
/// Derived here rather than in `verify_source_authentication` — which is where
/// the retired equality check lived — because this is the only consumer. Both
/// functions run on every claim, so no work is duplicated by the move and none
/// is added: exactly one `native_tx_proof_commitment_v1` per claim, as before.
fn source_proof_commitment(
  source: ledger_state.NativeTxProofSourceV1,
) -> ByteArray {
  native_tx_proof_commitment_v1(
    source.compact_cbor,
    source.witness_set_compact_cbor,
    source.field_preimage_lengths_cbor,
  )
}

fn source_binding_is_exact(
  descriptor: validation_trace_v1.ValidationTraceDescriptorV1,
  initial_state: validation_trace_v1.ValidationMachineStateV1,
  source: ValidationSourceMembershipV1,
) -> Bool {
  when source is {
    ForcedValidationSource { membership } -> and {
        membership.value.tx_id == initial_state.transaction_id,
        source_proof_commitment(membership.value.source) == initial_state.transaction_commitment,
        initial_state.source_kind == validation_trace_v1.Forced,
        forced_verdict_matches(membership.value.verdict, descriptor),
      }
    NormalValidationSource { membership } -> and {
        membership.value.tx_id == initial_state.transaction_id,
        source_proof_commitment(membership.value.source) == initial_state.transaction_commitment,
        initial_state.source_kind == validation_trace_v1.Normal,
        descriptor.verdict == validation_trace_v1.Accepted,
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

/// Authenticates the block-owned roots, descriptor endpoints, and transition
/// location only. Normative endpoint/source checks deliberately happen in the
/// following source step so a malformed committed initial or terminal state is
/// itself a unilateral fault instead of making an honest challenge impossible
/// to open.
pub fn committed_claim_structure_is_valid(
  header: ledger_state.HeaderV1,
  witness: ValidationClaimWitnessV1,
) -> Bool {
  let descriptor = witness.descriptor_membership.value
  let event_key = witness.descriptor_membership.key
  let transition_step = witness.transition_step_membership.value
  let event_to_step = witness.event_to_step_membership.value
  let initial = witness.initial_state
  let terminal = witness.terminal_state

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
    witness.initial_state_proof.state_index == 0,
    witness.initial_state_proof.state_hash == validation_trace_v1.hash_machine_state(
      initial,
    ),
    validation_trace_v1.verify_trace_proof(
      descriptor,
      witness.initial_state_proof,
    ),
    witness.terminal_state_proof.state_index == descriptor.step_count,
    witness.terminal_state_proof.state_hash == validation_trace_v1.hash_machine_state(
      terminal,
    ),
    validation_trace_v1.verify_trace_proof(
      descriptor,
      witness.terminal_state_proof,
    ),
    descriptor.initial_state_hash == witness.initial_state_proof.state_hash,
    descriptor.terminal_state_hash == witness.terminal_state_proof.state_hash,
  }
}

pub fn committed_claim_source_is_authenticated(
  header: ledger_state.HeaderV1,
  witness: ValidationClaimWitnessV1,
) -> Bool {
  verify_source_authentication(
    header,
    witness.descriptor_membership.key,
    witness.source_membership,
  )
}

pub fn committed_claim_endpoints_and_source_are_valid(
  header: ledger_state.HeaderV1,
  witness: ValidationClaimWitnessV1,
) -> Bool {
  let descriptor = witness.descriptor_membership.value
  let event_key = witness.descriptor_membership.key
  let transition_step = witness.transition_step_membership.value
  let initial = witness.initial_state
  let terminal = witness.terminal_state
  and {
    validation_trace_v1.machine_state_is_well_formed(initial),
    validation_trace_v1.machine_state_is_well_formed(terminal),
    initial.event_key_hash == blake2b_256(cbor.serialise(event_key)),
    initial.prior_ledger_root == transition_step.pre_utxos_root,
    initial.phase == validation_trace_v1.CanonicalDecode,
    initial.program_counter == 0,
    initial.execution_cpu == 0,
    initial.execution_memory == 0,
    initial.verdict == validation_trace_v1.Pending,
    validation_context_is_exact(
      header,
      witness.validation_context_cbor,
      initial,
    ),
    initial_work_root_is_exact(
      initial,
      witness.validation_context_cbor,
      witness.source_membership,
    ),
    terminal.phase == validation_trace_v1.Terminal,
    terminal.program_counter == descriptor.step_count,
    terminal.verdict == descriptor.verdict,
    terminal.rejection_code_hash == descriptor.rejection_code_hash,
    immutable_context_matches(initial, terminal),
    source_binding_is_exact(descriptor, initial, witness.source_membership),
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
