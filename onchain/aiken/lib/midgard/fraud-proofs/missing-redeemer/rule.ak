//// Trace-authenticated purpose/source selection and committed field-8 scan
//// for `missingRedeemer` (`0000002e`). A fabricated frontier contradicts the
//// producer-committed trace; an honest canonical trace is non-convictable.

use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/canonical_cbor_scan_v1
use midgard/fraud_proofs/native_tx/compact.{native_tx_proof_commitment_v1}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/ledger_state
use midgard/rejection_reason_v1.{RedeemerMissing, rejection_code_of}
use midgard/script_proof_v1
use midgard/transition_trace
use midgard/validation_machine/machine_types as vm_machine_types
use midgard/validation_machine/witness as vm_witness
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

pub const redeemer_field_index: Int = 8

pub const scan_batch: Int = 16

pub type BoundPurposeV1 {
  subject: substrate.VerdictSubjectV1,
  witness_set_hash: ByteArray,
  validation_traces_root: ByteArray,
  validation_trace_count: Int,
  purpose_kind: Int,
  purpose_index: Int,
}

pub type AuthenticatedPurposeV1 {
  bound: BoundPurposeV1,
  purpose_count: Int,
  redeemer_tag: Int,
  required_script_hash: ByteArray,
  source_index: Int,
  source_language_tag: Int,
  source_leaf: ByteArray,
}

pub type AuthenticatedStageTenV1 {
  bound: BoundPurposeV1,
  source_count: Int,
  source_peaks: List<validation_merkle_v1.FrontierPeak>,
  purpose_count: Int,
  purpose_peaks: List<validation_merkle_v1.FrontierPeak>,
  discovery: vm_machine_types.ScriptDiscoveryControlV1,
}

pub type AuthenticatedDescriptorV1 {
  bound: BoundPurposeV1,
  event_key_hash: ByteArray,
  descriptor: validation_trace_v1.ValidationTraceDescriptorV1,
}

pub type ScanStateV1 {
  authenticated: AuthenticatedPurposeV1,
  checkpoint_hash: ByteArray,
  cursor: Int,
  item_count: Int,
  found: Bool,
}

pub type DecisionStateV1 {
  bound: BoundPurposeV1,
  redeemer_missing: Bool,
}

pub fn redeemer_tag_for_purpose_kind_v1(kind: Int) -> Option<Int> {
  if kind == 0 {
    Some(0)
  } else if kind == 1 {
    Some(1)
  } else if kind == 2 {
    Some(3)
  } else if kind == 3 {
    Some(6)
  } else {
    None
  }
}

pub fn is_redeemer_bearing_language_v1(language_tag: Int) -> Bool {
  or {
    language_tag == 3,
    language_tag == 128,
  }
}

pub fn bind_purpose_v1(
  subject: substrate.VerdictSubjectV1,
  witness_set_hash: ByteArray,
  validation_traces_root: ByteArray,
  validation_trace_count: Int,
  purpose_kind: Int,
  purpose_index: Int,
) -> BoundPurposeV1 {
  expect and {
      substrate.subject_is_canonical_v1(subject),
      bytearray.length(witness_set_hash) == 32,
      bytearray.length(validation_traces_root) == 32,
      validation_trace_count > 0,
      purpose_index >= 0,
    }
  expect Some(_) = redeemer_tag_for_purpose_kind_v1(purpose_kind)
  expect _reason = if subject.direction == substrate.direction_wrongful_rejection {
    Some(
      substrate.bind_exact_rejection_reason_v1(
        subject,
        RedeemerMissing { purpose_kind, purpose_index },
      ),
    )
  } else {
    None
  }
  BoundPurposeV1 {
    subject,
    witness_set_hash,
    validation_traces_root,
    validation_trace_count,
    purpose_kind,
    purpose_index,
  }
}

fn event_key_matches_subject(
  key: ledger_state.EventKey,
  subject: substrate.VerdictSubjectV1,
) -> Bool {
  when key is {
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

fn descriptor_verdict_matches_subject(
  descriptor: validation_trace_v1.ValidationTraceDescriptorV1,
  subject: substrate.VerdictSubjectV1,
) -> Bool {
  if subject.direction == substrate.direction_wrongful_acceptance {
    and {
      descriptor.verdict == validation_trace_v1.Accepted,
      descriptor.rejection_code_hash == #"0000000000000000000000000000000000000000000000000000000000000000",
    }
  } else {
    and {
      descriptor.verdict == validation_trace_v1.Rejected,
      descriptor.rejection_code_hash == validation_trace_v1.hash_rejection_code(
        rejection_code_of(substrate.rejection_reason_of_v1(subject)),
      ),
    }
  }
}

pub fn authenticate_descriptor_v1(
  bound: BoundPurposeV1,
  trace_membership: transition_trace.RootMembershipProof<
    ledger_state.EventKey,
    validation_trace_v1.ValidationTraceDescriptorV1,
  >,
) -> AuthenticatedDescriptorV1 {
  let descriptor = trace_membership.value
  expect and {
      event_key_matches_subject(trace_membership.key, bound.subject),
      descriptor_verdict_matches_subject(descriptor, bound.subject),
      transition_trace.verify_root_membership_with_bytes(
        trace_membership,
        transition_trace.ValidationTracesRootDomain,
        bound.validation_traces_root,
        bound.validation_trace_count,
        cbor.serialise(trace_membership.key),
        cbor.serialise(descriptor),
      ),
    }
  AuthenticatedDescriptorV1 {
    bound,
    event_key_hash: blake2b_256(cbor.serialise(trace_membership.key)),
    descriptor,
  }
}

pub fn authenticate_stage_ten_trace_v1(
  authenticated_descriptor: AuthenticatedDescriptorV1,
  machine_state: validation_trace_v1.ValidationMachineStateV1,
  trace_proof: validation_trace_v1.ValidationTraceProof,
  control: vm_machine_types.ScriptSourcesControlV1,
) -> AuthenticatedStageTenV1 {
  let bound = authenticated_descriptor.bound
  let descriptor = authenticated_descriptor.descriptor
  let discovery = control.discovery
  let exact_control = vm_witness.encode_script_sources_discovery_witness(
    control,
    10,
    discovery,
  )
  expect and {
      validation_trace_v1.machine_state_is_well_formed(machine_state),
      machine_state.event_key_hash == authenticated_descriptor.event_key_hash,
      machine_state.transaction_id == bound.subject.transaction_id,
      machine_state.source_kind == if bound.subject.source_kind == substrate.source_kind_accepted {
        validation_trace_v1.Normal
      } else {
        validation_trace_v1.Forced
      },
      machine_state.phase == validation_trace_v1.ScriptSources,
      machine_state.verdict == validation_trace_v1.Pending,
      machine_state.rejection_code_hash == #"0000000000000000000000000000000000000000000000000000000000000000",
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
        exact_control,
      ),
      trace_proof.state_hash == validation_trace_v1.hash_machine_state(
        machine_state,
      ),
      validation_trace_v1.verify_trace_proof(descriptor, trace_proof),
      control.stage == 10,
      control.source_total_count == control.source_count,
      control.redeemer_total_count == control.redeemer_count,
      validation_merkle_v1.frontier_is_well_formed(
        control.source_count,
        control.source_peaks,
      ),
      validation_merkle_v1.frontier_is_well_formed(
        control.redeemer_count,
        control.redeemer_peaks,
      ),
      validation_merkle_v1.frontier_is_well_formed(
        control.purpose_count,
        control.purpose_peaks,
      ),
      discovery.purpose_cursor < control.purpose_count,
      discovery.source_cursor <= control.source_count,
      discovery.redeemer_cursor >= 0,
      discovery.redeemer_cursor <= control.redeemer_count,
      discovery.execution_count == discovery.purpose_cursor,
      control.pending_source_cbor == #"",
      control.output_proof == None,
    }
  AuthenticatedStageTenV1 {
    bound,
    source_count: control.source_count,
    source_peaks: control.source_peaks,
    purpose_count: control.purpose_count,
    purpose_peaks: control.purpose_peaks,
    discovery,
  }
}

pub fn authenticate_stage_ten_selection_v1(
  stage_ten: AuthenticatedStageTenV1,
  absolute_purpose_index: Int,
  purpose_siblings: List<ByteArray>,
  source_origin_kind: Int,
  source_key: ByteArray,
  source_language_tag: Int,
  source_script_hash: ByteArray,
  source_total_length: Int,
  source_item_commitment: ByteArray,
  source_siblings: List<ByteArray>,
) -> AuthenticatedPurposeV1 {
  let bound = stage_ten.bound
  let discovery = stage_ten.discovery
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(
    bound.purpose_kind,
    bound.purpose_index,
    discovery.current_script_hash,
    discovery.current_subject,
  )
  let source_leaf = script_proof_v1.source_descriptor_leaf_hash(
    source_origin_kind,
    source_key,
    source_language_tag,
    source_script_hash,
    source_total_length,
    source_item_commitment,
  )
  expect and {
      absolute_purpose_index == discovery.purpose_cursor,
      validation_merkle_v1.verify_membership(
        stage_ten.purpose_count,
        stage_ten.purpose_peaks,
        absolute_purpose_index,
        purpose_leaf,
        purpose_siblings,
      ),
      discovery.current_purpose_kind == bound.purpose_kind,
      discovery.current_purpose_index == bound.purpose_index,
      bytearray.length(discovery.current_script_hash) == 28,
      discovery.current_subject != #"",
      discovery.matched_source_index >= 0,
      discovery.matched_source_index < stage_ten.source_count,
      source_script_hash == discovery.current_script_hash,
      source_language_tag == discovery.matched_language_tag,
      source_leaf == discovery.matched_source_leaf,
      validation_merkle_v1.verify_membership(
        stage_ten.source_count,
        stage_ten.source_peaks,
        discovery.matched_source_index,
        source_leaf,
        source_siblings,
      ),
      is_redeemer_bearing_language_v1(discovery.matched_language_tag),
      bytearray.length(discovery.matched_source_leaf) == 32,
    }
  expect Some(tag) = redeemer_tag_for_purpose_kind_v1(bound.purpose_kind)
  AuthenticatedPurposeV1 {
    bound,
    purpose_count: stage_ten.purpose_count,
    redeemer_tag: tag,
    required_script_hash: discovery.current_script_hash,
    source_index: discovery.matched_source_index,
    source_language_tag: discovery.matched_language_tag,
    source_leaf,
  }
}

pub fn authenticate_stage_ten_purpose_v1(
  bound: BoundPurposeV1,
  trace_membership: transition_trace.RootMembershipProof<
    ledger_state.EventKey,
    validation_trace_v1.ValidationTraceDescriptorV1,
  >,
  machine_state: validation_trace_v1.ValidationMachineStateV1,
  trace_proof: validation_trace_v1.ValidationTraceProof,
  control: vm_machine_types.ScriptSourcesControlV1,
  absolute_purpose_index: Int,
  purpose_siblings: List<ByteArray>,
  source_origin_kind: Int,
  source_key: ByteArray,
  source_language_tag: Int,
  source_script_hash: ByteArray,
  source_total_length: Int,
  source_item_commitment: ByteArray,
  source_siblings: List<ByteArray>,
) -> AuthenticatedPurposeV1 {
  authenticate_stage_ten_selection_v1(
    authenticate_stage_ten_trace_v1(
      authenticate_descriptor_v1(bound, trace_membership),
      machine_state,
      trace_proof,
      control,
    ),
    absolute_purpose_index,
    purpose_siblings,
    source_origin_kind,
    source_key,
    source_language_tag,
    source_script_hash,
    source_total_length,
    source_item_commitment,
    source_siblings,
  )
}

pub fn initial_scan_v1(
  authenticated: AuthenticatedPurposeV1,
  checkpoint_hash: ByteArray,
  item_count: Int,
) -> ScanStateV1 {
  expect and {
      bytearray.length(checkpoint_hash) == 32,
      item_count >= 0,
    }
  ScanStateV1 {
    authenticated,
    checkpoint_hash,
    cursor: 0,
    item_count,
    found: False,
  }
}

pub fn pointer_from_item_v1(item: ByteArray) -> Option<Pair<Int, Int>> {
  when canonical_cbor_scan_v1.head_at_v1(item, 0, 4) is {
    None -> None
    Some(outer) ->
      if outer.value != 4 {
        None
      } else {
        when canonical_cbor_scan_v1.head_at_v1(item, outer.next_offset, 0) is {
          None -> None
          Some(tag) ->
            when
              canonical_cbor_scan_v1.head_at_v1(item, tag.next_offset, 0)
            is {
              None -> None
              Some(index) -> Some(Pair(tag.value, index.value))
            }
        }
      }
  }
}

pub fn scan_item_v1(
  state: ScanStateV1,
  item_index: Int,
  item: ByteArray,
) -> ScanStateV1 {
  expect and {
      !state.found,
      item_index == state.cursor,
      item_index < state.item_count,
    }
  expect Some(Pair(tag, index)) = pointer_from_item_v1(item)
  ScanStateV1 { ..state, cursor: item_index + 1, found: and {
      tag == state.authenticated.redeemer_tag,
      index == state.authenticated.bound.purpose_index,
    } }
}

pub fn decision_v1(state: ScanStateV1) -> DecisionStateV1 {
  expect or {
      state.found,
      state.cursor == state.item_count,
    }
  DecisionStateV1 {
    bound: state.authenticated.bound,
    redeemer_missing: !state.found,
  }
}

pub fn terminal_contradiction_v1(state: DecisionStateV1) -> Bool {
  substrate.terminal_contradiction_v1(
    state.bound.subject,
    state.redeemer_missing,
  )
}

pub fn encode_bound_v1(value: BoundPurposeV1) -> ByteArray {
  cbor.serialise(value)
}

pub fn encode_authenticated_v1(value: AuthenticatedPurposeV1) -> ByteArray {
  cbor.serialise(value)
}

pub fn encode_scan_v1(value: ScanStateV1) -> ByteArray {
  cbor.serialise(value)
}
