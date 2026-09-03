//// Narrow receive-purpose/language rule for category `00000034`.

use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/ledger_state
use midgard/rejection_reason_v1.{ReceivePurposePlutusV3Forbidden}
use midgard/script_proof_v1
use midgard/transition_trace
use midgard/validation_machine/machine_types.{NativeScriptsControlV1}
use midgard/validation_machine/witness as vm_witness
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

pub const receive_purpose_kind: Int = 3

pub const plutus_v3_language_tag: Int = 3

pub type BoundExecutionV1 {
  subject: substrate.VerdictSubjectV1,
  validation_traces_root: ByteArray,
  validation_trace_count: Int,
  execution_index: Int,
}

pub type AuthenticatedReceiveLanguageV1 {
  bound: BoundExecutionV1,
  prior_ledger_root: ByteArray,
  purpose_kind: Int,
  purpose_index: Int,
  source_index: Int,
  origin_kind: Int,
  source_key: ByteArray,
  language_tag: Int,
  script_hash: ByteArray,
}

pub fn bind_execution_v1(
  subject: substrate.VerdictSubjectV1,
  validation_traces_root: ByteArray,
  validation_trace_count: Int,
  execution_index: Int,
) -> BoundExecutionV1 {
  expect substrate.subject_is_canonical_v1(subject)
  expect bytearray.length(validation_traces_root) == 32
  expect validation_trace_count > 0
  expect execution_index >= 0
  expect _reason = if subject.direction == substrate.direction_wrongful_rejection {
    expect ReceivePurposePlutusV3Forbidden { execution_index: actual } = substrate.rejection_reason_of_v1(
      subject,
    )
    expect actual == execution_index
    Some(actual)
  } else {
    None
  }
  BoundExecutionV1 {
    subject,
    validation_traces_root,
    validation_trace_count,
    execution_index,
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

pub fn authenticate_receive_language_v1(
  bound: BoundExecutionV1,
  trace_membership: transition_trace.RootMembershipProof<
    ledger_state.EventKey,
    validation_trace_v1.ValidationTraceDescriptorV1,
  >,
  machine_state: validation_trace_v1.ValidationMachineStateV1,
  trace_proof: validation_trace_v1.ValidationTraceProof,
  control: NativeScriptsControlV1,
  purpose_index: Int,
  script_hash: ByteArray,
  purpose_subject: ByteArray,
  purpose_siblings: List<ByteArray>,
  source_index: Int,
  origin_kind: Int,
  source_key: ByteArray,
  language_tag: Int,
  total_length: Int,
  item_commitment: ByteArray,
  source_siblings: List<ByteArray>,
  redeemer_leaf: ByteArray,
  execution_siblings: List<ByteArray>,
) -> AuthenticatedReceiveLanguageV1 {
  expect or {
      language_tag == 0,
      language_tag == 3,
      language_tag == 128,
    }
  let descriptor = trace_membership.value
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(
    receive_purpose_kind,
    purpose_index,
    script_hash,
    purpose_subject,
  )
  let source_leaf = script_proof_v1.source_descriptor_leaf_hash(
    origin_kind,
    source_key,
    language_tag,
    script_hash,
    total_length,
    item_commitment,
  )
  let execution_leaf = script_proof_v1.execution_leaf_hash(
    language_tag,
    purpose_leaf,
    source_leaf,
    redeemer_leaf,
  )
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
      machine_state.phase == validation_trace_v1.NativeScripts,
      machine_state.work_root == validation_trace_v1.hash_work_witness(
        validation_trace_v1.NativeScripts,
        machine_state.program_counter,
        vm_witness.encode_native_scripts_control_v1(control),
      ),
      trace_proof.state_hash == validation_trace_v1.hash_machine_state(
        machine_state,
      ),
      validation_trace_v1.verify_trace_proof(descriptor, trace_proof),
      bound.execution_index < control.execution_count,
      control.execution_count == control.purpose_count,
      validation_merkle_v1.verify_membership(
        control.purpose_count,
        control.purpose_peaks,
        bound.execution_index,
        purpose_leaf,
        purpose_siblings,
      ),
      validation_merkle_v1.verify_membership(
        control.source_count,
        control.source_peaks,
        source_index,
        source_leaf,
        source_siblings,
      ),
      validation_merkle_v1.verify_membership(
        control.execution_count,
        control.execution_peaks,
        bound.execution_index,
        execution_leaf,
        execution_siblings,
      ),
      total_length > 0,
      bytearray.length(script_hash) == 28,
      bytearray.length(item_commitment) == 32,
    }
  AuthenticatedReceiveLanguageV1 {
    bound,
    prior_ledger_root: machine_state.prior_ledger_root,
    purpose_kind: receive_purpose_kind,
    purpose_index,
    source_index,
    origin_kind,
    source_key,
    language_tag,
    script_hash,
  }
}

pub fn forbidden_receive_language_holds_v1(
  purpose_kind: Int,
  language_tag: Int,
) -> Bool {
  and {
    purpose_kind == receive_purpose_kind,
    language_tag == plutus_v3_language_tag,
  }
}

pub fn terminal_contradiction_v1(state: AuthenticatedReceiveLanguageV1) -> Bool {
  substrate.terminal_contradiction_v1(
    state.bound.subject,
    forbidden_receive_language_holds_v1(state.purpose_kind, state.language_tag),
  )
}
