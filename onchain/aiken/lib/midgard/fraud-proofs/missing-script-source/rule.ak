//// Complete-purpose/source proof for `missingScriptSource` (`0000002d`).

use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/codec.{encode_definite_bytes}
use midgard/fraud_proofs/native_tx/compact.{native_tx_proof_commitment_v1}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/ledger_state
use midgard/rejection_reason_v1.{ScriptSourceMissing}
use midgard/script_proof_v1
use midgard/transition_trace
use midgard/validation_machine_v1
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

pub const source_location_witness: Int = 0

pub const source_location_resolved_reference: Int = 1

pub const staged_source_budget: Int = 24

const checkpoint_domain: ByteArray =
  "midgard/fraud-proofs/missing-script-source/checkpoint-v1"

pub type BoundPurposeV1 {
  subject: substrate.VerdictSubjectV1,
  validation_traces_root: ByteArray,
  validation_trace_count: Int,
  witness_set_hash: ByteArray,
  purpose_kind: Int,
  purpose_index: Int,
}

pub type AuthenticatedPurposeV1 {
  bound: BoundPurposeV1,
  prior_ledger_root: ByteArray,
  required_script_hash: ByteArray,
  source_count: Int,
  scan_limit: Int,
  source_peaks: List<validation_merkle_v1.FrontierPeak>,
}

pub type AuthenticatedTraceStateV1 {
  bound: BoundPurposeV1,
  machine_state: validation_trace_v1.ValidationMachineStateV1,
}

pub type AuthenticatedTransactionSourcesV1 {
  purpose: AuthenticatedPurposeV1,
  transaction_source_count: Int,
}

pub type AuthenticatedResolvedSourcesV1 {
  purpose: AuthenticatedPurposeV1,
  transaction_source_count: Int,
  resolved_reference_source_count: Int,
  source_identity_hash: ByteArray,
}

pub type SourceDescriptorV1 {
  source_index: Int,
  location_kind: Int,
  source_key: ByteArray,
  language_tag: Int,
  script_hash: ByteArray,
  total_length: Int,
  item_commitment: ByteArray,
  siblings: List<ByteArray>,
}

pub type SourceScanStateV1 {
  authenticated: AuthenticatedResolvedSourcesV1,
  cursor: Int,
  found: Bool,
  next_expected_script_hash: ByteArray,
  checkpoint_hash: ByteArray,
}

pub fn bind_purpose_v1(
  subject: substrate.VerdictSubjectV1,
  validation_traces_root: ByteArray,
  validation_trace_count: Int,
  witness_set_hash: ByteArray,
  purpose_kind: Int,
  purpose_index: Int,
) -> BoundPurposeV1 {
  expect and {
      substrate.subject_is_canonical_v1(subject),
      bytearray.length(validation_traces_root) == 32,
      validation_trace_count > 0,
      bytearray.length(witness_set_hash) == 32,
      purpose_kind >= 0,
      purpose_kind <= 3,
      purpose_index >= 0,
    }
  expect
    if subject.direction == substrate.direction_wrongful_rejection {
      expect
          ScriptSourceMissing {
            purpose_kind: accused_kind,
            purpose_index: accused_index,
          }
        = substrate.rejection_reason_of_v1(subject)
      and {
        accused_kind == purpose_kind,
        accused_index == purpose_index,
      }
    } else {
      True
    }
  BoundPurposeV1 {
    subject,
    validation_traces_root,
    validation_trace_count,
    witness_set_hash,
    purpose_kind,
    purpose_index,
  }
}

fn event_key_matches_subject(
  event_key: ledger_state.EventKey,
  subject: substrate.VerdictSubjectV1,
) -> Bool {
  when event_key is {
    ledger_state.L2TransactionEventKey { tx_id } -> and {
        subject.source_kind == substrate.source_kind_accepted,
        tx_id == subject.transaction_id,
      }
    ledger_state.ForcedTransactionEventKey { tx_order_id } -> and {
        subject.source_kind == substrate.source_kind_forced,
        cbor.serialise(tx_order_id) == subject.source_key,
      }
    _ -> False
  }
}

pub fn authenticate_trace_state_v1(
  bound: BoundPurposeV1,
  trace_membership: transition_trace.RootMembershipProof<
    ledger_state.EventKey,
    validation_trace_v1.ValidationTraceDescriptorV1,
  >,
  machine_state: validation_trace_v1.ValidationMachineStateV1,
  trace_proof: validation_trace_v1.ValidationTraceProof,
) -> AuthenticatedTraceStateV1 {
  let descriptor = trace_membership.value
  expect and {
      event_key_matches_subject(trace_membership.key, bound.subject),
      machine_state.event_key_hash == blake2b_256(
        cbor.serialise(trace_membership.key),
      ),
      transition_trace.verify_root_membership_with_bytes(
        trace_membership,
        transition_trace.ValidationTracesRootDomain,
        bound.validation_traces_root,
        bound.validation_trace_count,
        cbor.serialise(trace_membership.key),
        cbor.serialise(descriptor),
      ),
      machine_state.transaction_id == bound.subject.transaction_id,
      if bound.subject.source_kind == substrate.source_kind_accepted {
        and {
          machine_state.source_kind == validation_trace_v1.Normal,
          descriptor.verdict == validation_trace_v1.Accepted,
        }
      } else {
        and {
          machine_state.source_kind == validation_trace_v1.Forced,
          descriptor.verdict == validation_trace_v1.Rejected,
          descriptor.rejection_code_hash == validation_trace_v1.hash_rejection_code(
            rejection_reason_v1.rejection_code_of(
              substrate.rejection_reason_of_v1(bound.subject),
            ),
          ),
        }
      },
      validation_trace_v1.machine_state_is_well_formed(machine_state),
      machine_state.phase == validation_trace_v1.ScriptSources,
      machine_state.verdict == validation_trace_v1.Pending,
      machine_state.rejection_code_hash == #"0000000000000000000000000000000000000000000000000000000000000000",
      trace_proof.state_hash == validation_trace_v1.hash_machine_state(
        machine_state,
      ),
      validation_trace_v1.verify_trace_proof(descriptor, trace_proof),
    }
  AuthenticatedTraceStateV1 { bound, machine_state }
}

pub fn authenticate_purpose_control_v1(
  authenticated_trace: AuthenticatedTraceStateV1,
  control: validation_machine_v1.ScriptSourcesControlV1,
  absolute_purpose_index: Int,
  required_script_hash: ByteArray,
  purpose_subject: ByteArray,
  purpose_siblings: List<ByteArray>,
) -> AuthenticatedPurposeV1 {
  let bound = authenticated_trace.bound
  let machine_state = authenticated_trace.machine_state
  let discovery = control.discovery
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(
    bound.purpose_kind,
    bound.purpose_index,
    required_script_hash,
    purpose_subject,
  )
  let scan_limit = if bound.subject.direction == substrate.direction_wrongful_acceptance {
    control.source_count
  } else {
    discovery.source_cursor + 1
  }
  expect and {
      machine_state.transaction_commitment == native_tx_proof_commitment_v1(
        control.compact_cbor,
        control.witness_set_compact_cbor,
        control.field_preimage_lengths_cbor,
      ),
      blake2b_256(control.witness_set_compact_cbor) == bound.witness_set_hash,
      machine_state.validation_context_hash == validation_trace_v1.hash_validation_context(
        control.context_cbor,
      ),
      machine_state.work_root == validation_trace_v1.hash_work_witness(
        validation_trace_v1.ScriptSources,
        machine_state.program_counter,
        validation_machine_v1.encode_script_sources_discovery_witness(
          control,
          9,
          discovery,
        ),
      ),
      absolute_purpose_index >= 0,
      absolute_purpose_index == discovery.purpose_cursor,
      absolute_purpose_index < control.purpose_count,
      validation_merkle_v1.verify_membership(
        control.purpose_count,
        control.purpose_peaks,
        absolute_purpose_index,
        purpose_leaf,
        purpose_siblings,
      ),
      bytearray.length(required_script_hash) == 28,
      control.stage == 9,
      control.source_total_count == control.source_count,
      validation_merkle_v1.frontier_is_well_formed(
        control.source_count,
        control.source_peaks,
      ),
      validation_merkle_v1.frontier_is_well_formed(
        control.purpose_count,
        control.purpose_peaks,
      ),
      discovery.current_purpose_kind == bound.purpose_kind,
      discovery.current_purpose_index == bound.purpose_index,
      discovery.current_script_hash == required_script_hash,
      discovery.current_subject == purpose_subject,
      if bound.subject.direction == substrate.direction_wrongful_acceptance {
        discovery.source_cursor == control.source_count
      } else {
        and {
          discovery.source_cursor >= 0,
          discovery.source_cursor < control.source_count,
        }
      },
      discovery.matched_source_index == -1,
      control.pending_source_cbor == #"",
      control.output_proof == None,
    }
  AuthenticatedPurposeV1 {
    bound,
    prior_ledger_root: machine_state.prior_ledger_root,
    required_script_hash,
    source_count: control.source_count,
    scan_limit,
    source_peaks: control.source_peaks,
  }
}

pub fn authenticate_purpose_frontier_v1(
  bound: BoundPurposeV1,
  trace_membership: transition_trace.RootMembershipProof<
    ledger_state.EventKey,
    validation_trace_v1.ValidationTraceDescriptorV1,
  >,
  machine_state: validation_trace_v1.ValidationMachineStateV1,
  trace_proof: validation_trace_v1.ValidationTraceProof,
  control: validation_machine_v1.ScriptSourcesControlV1,
  absolute_purpose_index: Int,
  required_script_hash: ByteArray,
  purpose_subject: ByteArray,
  purpose_siblings: List<ByteArray>,
) -> AuthenticatedPurposeV1 {
  authenticate_trace_state_v1(
    bound,
    trace_membership,
    machine_state,
    trace_proof,
  )
    |> authenticate_purpose_control_v1(
        control,
        absolute_purpose_index,
        required_script_hash,
        purpose_subject,
        purpose_siblings,
      )
}

pub fn authenticate_transaction_sources_v1(
  purpose: AuthenticatedPurposeV1,
  transaction_source_count: Int,
) -> AuthenticatedTransactionSourcesV1 {
  expect and {
      transaction_source_count >= 0,
      transaction_source_count <= purpose.scan_limit,
    }
  AuthenticatedTransactionSourcesV1 { purpose, transaction_source_count }
}

pub fn source_identity_hash_v1(
  purpose: AuthenticatedPurposeV1,
  transaction_source_count: Int,
  resolved_reference_source_count: Int,
) -> ByteArray {
  checkpoint_domain
    |> bytearray.concat(purpose.prior_ledger_root)
    |> bytearray.concat(cbor.serialise(purpose.source_count))
    |> bytearray.concat(cbor.serialise(purpose.scan_limit))
    |> bytearray.concat(cbor.serialise(purpose.source_peaks))
    |> bytearray.concat(cbor.serialise(transaction_source_count))
    |> bytearray.concat(cbor.serialise(resolved_reference_source_count))
    |> blake2b_256
}

pub fn authenticate_resolved_sources_v1(
  staged: AuthenticatedTransactionSourcesV1,
  resolved_reference_source_count: Int,
) -> AuthenticatedResolvedSourcesV1 {
  expect and {
      resolved_reference_source_count >= 0,
      staged.transaction_source_count + resolved_reference_source_count == staged.purpose.scan_limit,
    }
  AuthenticatedResolvedSourcesV1 {
    purpose: staged.purpose,
    transaction_source_count: staged.transaction_source_count,
    resolved_reference_source_count,
    source_identity_hash: source_identity_hash_v1(
      staged.purpose,
      staged.transaction_source_count,
      resolved_reference_source_count,
    ),
  }
}

fn expected_location(state: AuthenticatedResolvedSourcesV1, index: Int) -> Int {
  if index < state.transaction_source_count {
    source_location_witness
  } else {
    source_location_resolved_reference
  }
}

fn descriptor_leaf(descriptor: SourceDescriptorV1) -> ByteArray {
  script_proof_v1.source_descriptor_leaf_hash(
    if descriptor.location_kind == source_location_witness {
      0
    } else {
      1
    },
    descriptor.source_key,
    descriptor.language_tag,
    descriptor.script_hash,
    descriptor.total_length,
    descriptor.item_commitment,
  )
}

pub fn checkpoint_v1(
  authenticated: AuthenticatedResolvedSourcesV1,
  cursor: Int,
  found: Bool,
  next_expected_script_hash: ByteArray,
) -> ByteArray {
  checkpoint_domain
    |> bytearray.concat(authenticated.source_identity_hash)
    |> bytearray.concat(cbor.serialise(cursor))
    |> bytearray.concat(cbor.serialise(found))
    |> bytearray.concat(encode_definite_bytes(next_expected_script_hash))
    |> blake2b_256
}

pub fn initial_scan_v1(
  authenticated: AuthenticatedResolvedSourcesV1,
  scan_script_hash: ByteArray,
) -> SourceScanStateV1 {
  expect bytearray.length(scan_script_hash) == 28
  SourceScanStateV1 {
    authenticated,
    cursor: 0,
    found: False,
    next_expected_script_hash: scan_script_hash,
    checkpoint_hash: checkpoint_v1(authenticated, 0, False, scan_script_hash),
  }
}

pub fn state_is_authentic_v1(state: SourceScanStateV1) -> Bool {
  and {
    bytearray.length(state.authenticated.source_identity_hash) == 32,
    bytearray.length(state.next_expected_script_hash) == 28,
    state.cursor >= 0,
    state.cursor <= state.authenticated.purpose.scan_limit,
    state.checkpoint_hash == checkpoint_v1(
      state.authenticated,
      state.cursor,
      state.found,
      state.next_expected_script_hash,
    ),
  }
}

pub fn scan_source_v1(
  state: SourceScanStateV1,
  descriptor: SourceDescriptorV1,
  next_script_hash: ByteArray,
) -> SourceScanStateV1 {
  expect and {
      state_is_authentic_v1(state),
      state.cursor < state.authenticated.purpose.scan_limit,
      descriptor.source_index == state.cursor,
      descriptor.location_kind == expected_location(
        state.authenticated,
        state.cursor,
      ),
      descriptor.total_length > 0,
      bytearray.length(descriptor.script_hash) == 28,
      bytearray.length(descriptor.item_commitment) == 32,
      validation_merkle_v1.verify_membership(
        state.authenticated.purpose.source_count,
        state.authenticated.purpose.source_peaks,
        descriptor.source_index,
        descriptor_leaf(descriptor),
        descriptor.siblings,
      ),
      bytearray.length(next_script_hash) == 28,
    }
  let cursor = state.cursor + 1
  let found = or {
    state.found,
    descriptor.script_hash == state.authenticated.purpose.required_script_hash,
  }
  SourceScanStateV1 {
    ..state,
    cursor,
    found,
    next_expected_script_hash: next_script_hash,
    checkpoint_hash: checkpoint_v1(
      state.authenticated,
      cursor,
      found,
      next_script_hash,
    ),
  }
}

pub fn scan_complete_v1(state: SourceScanStateV1) -> Bool {
  and {
    state_is_authentic_v1(state),
    state.cursor == state.authenticated.purpose.scan_limit,
  }
}

pub fn terminal_contradiction_v1(state: SourceScanStateV1) -> Bool {
  expect scan_complete_v1(state)
  substrate.terminal_contradiction_v1(
    state.authenticated.purpose.bound.subject,
    !state.found,
  )
}

pub fn encode_bound_v1(value: BoundPurposeV1) -> ByteArray {
  cbor.serialise(value)
}

pub fn encode_purpose_v1(value: AuthenticatedPurposeV1) -> ByteArray {
  cbor.serialise(value)
}

pub fn encode_scan_state_v1(value: SourceScanStateV1) -> ByteArray {
  cbor.serialise(value)
}
