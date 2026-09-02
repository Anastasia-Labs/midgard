//// Direct script-integrity mismatch rule for category `00000033`.

use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/ledger_state
use midgard/rejection_reason_v1.{ScriptIntegrityHashMismatch}
use midgard/script_language_views_v1
use midgard/transition_trace
use midgard/validation_machine_v1.{NativeScriptsControlV1}
use midgard/validation_trace_v1

pub type BoundIntegrityV1 {
  subject: substrate.VerdictSubjectV1,
  validation_traces_root: ByteArray,
  validation_trace_count: Int,
  script_integrity_hash: ByteArray,
}

pub type AuthenticatedIntegrityV1 {
  bound: BoundIntegrityV1,
  prior_ledger_root: ByteArray,
  redeemer_witness_hash: ByteArray,
  selected_language_bitmap: Int,
  execution_count: Int,
}

pub type LanguageFoldV1 {
  authenticated: AuthenticatedIntegrityV1,
  cursor: Int,
  rebuilt_language_bitmap: Int,
  selected_language_count: Int,
}

pub type DecisionV1 {
  authenticated: AuthenticatedIntegrityV1,
  expected_hash: ByteArray,
}

pub fn bind_integrity_v1(
  subject: substrate.VerdictSubjectV1,
  validation_traces_root: ByteArray,
  validation_trace_count: Int,
  script_integrity_hash: ByteArray,
) -> BoundIntegrityV1 {
  expect substrate.subject_is_canonical_v1(subject)
  expect bytearray.length(validation_traces_root) == 32
  expect validation_trace_count > 0
  expect bytearray.length(script_integrity_hash) == 32
  expect _reason = if subject.direction == substrate.direction_wrongful_rejection {
    expect ScriptIntegrityHashMismatch = substrate.rejection_reason_of_v1(
      subject,
    )
    True
  } else {
    True
  }
  BoundIntegrityV1 {
    subject,
    validation_traces_root,
    validation_trace_count,
    script_integrity_hash,
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

pub fn authenticate_integrity_v1(
  bound: BoundIntegrityV1,
  trace_membership: transition_trace.RootMembershipProof<
    ledger_state.EventKey,
    validation_trace_v1.ValidationTraceDescriptorV1,
  >,
  machine_state: validation_trace_v1.ValidationMachineStateV1,
  trace_proof: validation_trace_v1.ValidationTraceProof,
  control: NativeScriptsControlV1,
  redeemer_witness_hash: ByteArray,
) -> AuthenticatedIntegrityV1 {
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
      machine_state.phase == validation_trace_v1.ScriptIntegrity,
      machine_state.work_root == validation_trace_v1.hash_work_witness(
        validation_trace_v1.ScriptIntegrity,
        machine_state.program_counter,
        validation_machine_v1.encode_script_integrity_finalize_witness_v1(
          control,
          bound.script_integrity_hash,
          redeemer_witness_hash,
        ),
      ),
      trace_proof.state_hash == validation_trace_v1.hash_machine_state(
        machine_state,
      ),
      validation_trace_v1.verify_trace_proof(descriptor, trace_proof),
      control.execution_cursor == control.execution_count,
      control.execution_count == control.purpose_count,
      control.language_bitmap >= 0,
      control.language_bitmap <= 3,
      bytearray.length(redeemer_witness_hash) == 32,
    }
  AuthenticatedIntegrityV1 {
    bound,
    prior_ledger_root: machine_state.prior_ledger_root,
    redeemer_witness_hash,
    selected_language_bitmap: control.language_bitmap,
    execution_count: control.execution_count,
  }
}

pub fn initialize_language_fold_v1(
  authenticated: AuthenticatedIntegrityV1,
) -> LanguageFoldV1 {
  expect and {
      authenticated.selected_language_bitmap >= 0,
      authenticated.selected_language_bitmap <= 3,
      bytearray.length(authenticated.redeemer_witness_hash) == 32,
    }
  LanguageFoldV1 {
    authenticated,
    cursor: 0,
    rebuilt_language_bitmap: 0,
    selected_language_count: 0,
  }
}

pub fn fold_next_language_v1(state: LanguageFoldV1) -> LanguageFoldV1 {
  expect state.cursor >= 0
  expect state.cursor < 2
  let selected = if state.cursor == 0 {
    state.authenticated.selected_language_bitmap % 2 == 1
  } else {
    state.authenticated.selected_language_bitmap >= 2
  }
  LanguageFoldV1 {
    ..state,
    cursor: state.cursor + 1,
    rebuilt_language_bitmap: state.rebuilt_language_bitmap + if selected {
      if state.cursor == 0 {
        1
      } else {
        2
      }
    } else {
      0
    },
    selected_language_count: state.selected_language_count + if selected {
      1
    } else {
      0
    },
  }
}

pub fn decide_integrity_v1(state: LanguageFoldV1) -> DecisionV1 {
  expect and {
      state.cursor == 2,
      state.rebuilt_language_bitmap == state.authenticated.selected_language_bitmap,
      state.selected_language_count >= 0,
      state.selected_language_count <= 2,
    }
  DecisionV1 {
    authenticated: state.authenticated,
    expected_hash: script_language_views_v1.expected_script_integrity_hash(
      state.authenticated.redeemer_witness_hash,
      state.rebuilt_language_bitmap,
    ),
  }
}

pub fn terminal_contradiction_v1(decision: DecisionV1) -> Bool {
  expect bytearray.length(decision.expected_hash) == 32
  substrate.terminal_contradiction_v1(
    decision.authenticated.bound.subject,
    decision.authenticated.bound.script_integrity_hash != decision.expected_hash,
  )
}
