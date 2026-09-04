//// Accepted-direction canonical purpose/source reconstruction.
////
//// This state is the logical-step-2 prelude used when the honest evaluator
//// stops at Phase-A native rejection before a ScriptSources frontier exists.
//// Applied doors authenticate each item against the step-01-carried compact
//// transaction and each resolved output against `prior_ledger_root`; this
//// module owns only the deterministic consensus-order fold and checkpoint.

use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/fraud_proofs/execution_native_script_invalid/rule.{
  AuthenticatedExecutionSourceV1, BoundExecutionV1,
}
use midgard/fraud_proofs/native_tx/codec.{encode_definite_bytes}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate

pub const phase_spend: Int = 0

pub const phase_mint: Int = 1

pub const phase_observe: Int = 2

pub const phase_receive: Int = 3

pub const phase_inline_source: Int = 4

pub const phase_reference_source: Int = 5

pub const phase_complete: Int = 6

const checkpoint_domain: ByteArray =
  "midgard/fraud-proofs/execution-native-script-invalid/accepted-reconstruction-v1"

pub type SelectedPurposeV1 {
  purpose_kind: Int,
  purpose_index: Int,
  script_hash: ByteArray,
  subject: ByteArray,
}

pub type SelectedSourceV1 {
  source_index: Int,
  origin_kind: Int,
  source_key: ByteArray,
  language_tag: Int,
  script_hash: ByteArray,
  total_length: Int,
  item_commitment: ByteArray,
}

pub type StateV1 {
  bound: BoundExecutionV1,
  phase: Int,
  field_cursor: Int,
  execution_cursor: Int,
  previous_key: ByteArray,
  /// During the receive phase this is the least protected script credential
  /// strictly greater than `previous_key` seen in the current complete output
  /// pass. Repeated full passes therefore enumerate the unique credential set
  /// in consensus order without trusting a prover-supplied set or carrying an
  /// unbounded collection in the datum.
  receive_candidate: ByteArray,
  source_base_index: Int,
  source_cursor: Int,
  selected_purpose: Option<SelectedPurposeV1>,
  selected_source: Option<SelectedSourceV1>,
  next_expected_script_hash: ByteArray,
  checkpoint_hash: ByteArray,
}

fn purpose_cbor(value: Option<SelectedPurposeV1>) -> ByteArray {
  cbor.serialise(value)
}

fn source_cbor(value: Option<SelectedSourceV1>) -> ByteArray {
  cbor.serialise(value)
}

pub fn checkpoint_v1(
  state: StateV1,
  next_expected_script_hash: ByteArray,
) -> ByteArray {
  checkpoint_domain
    |> bytearray.concat(
        substrate.encode_verdict_subject_v1(state.bound.subject),
      )
    |> bytearray.concat(encode_definite_bytes(state.bound.compact_cbor))
    |> bytearray.concat(state.bound.prior_ledger_root)
    |> bytearray.concat(cbor.serialise(state.bound.execution_index))
    |> bytearray.concat(cbor.serialise(state.phase))
    |> bytearray.concat(cbor.serialise(state.field_cursor))
    |> bytearray.concat(cbor.serialise(state.execution_cursor))
    |> bytearray.concat(encode_definite_bytes(state.previous_key))
    |> bytearray.concat(encode_definite_bytes(state.receive_candidate))
    |> bytearray.concat(cbor.serialise(state.source_base_index))
    |> bytearray.concat(cbor.serialise(state.source_cursor))
    |> bytearray.concat(purpose_cbor(state.selected_purpose))
    |> bytearray.concat(source_cbor(state.selected_source))
    |> bytearray.concat(encode_definite_bytes(next_expected_script_hash))
    |> blake2b_256
}

fn with_checkpoint(
  state: StateV1,
  next_expected_script_hash: ByteArray,
) -> StateV1 {
  let next = StateV1 { ..state, next_expected_script_hash }
  StateV1 {
    ..next,
    checkpoint_hash: checkpoint_v1(next, next_expected_script_hash),
  }
}

pub fn initial_v1(
  bound: BoundExecutionV1,
  next_expected_script_hash: ByteArray,
) -> StateV1 {
  expect bound.subject.direction == substrate.direction_wrongful_acceptance
  expect bytearray.length(next_expected_script_hash) == 28
  with_checkpoint(
    StateV1 {
      bound,
      phase: phase_spend,
      field_cursor: 0,
      execution_cursor: 0,
      previous_key: #"",
      receive_candidate: #"",
      source_base_index: 0,
      source_cursor: 0,
      selected_purpose: None,
      selected_source: None,
      next_expected_script_hash,
      checkpoint_hash: #"",
    },
    next_expected_script_hash,
  )
}

pub fn state_is_authentic_v1(state: StateV1) -> Bool {
  and {
    state.bound.subject.direction == substrate.direction_wrongful_acceptance,
    state.phase >= phase_spend,
    state.phase <= phase_complete,
    state.field_cursor >= 0,
    state.execution_cursor >= 0,
    state.source_base_index >= 0,
    state.source_cursor >= state.source_base_index,
    or {
      state.receive_candidate == #"",
      bytearray.length(state.receive_candidate) == 28,
    },
    bytearray.length(state.next_expected_script_hash) == 28,
    state.checkpoint_hash == checkpoint_v1(
      state,
      state.next_expected_script_hash,
    ),
  }
}

/// Consume one authenticated output while selecting the least protected
/// script credential above the credential emitted by the preceding pass.
/// `candidate` is `None` for unprotected outputs and non-script credentials.
pub fn scan_receive_output_v1(
  state: StateV1,
  candidate: Option<ByteArray>,
  next_expected_script_hash: ByteArray,
) -> StateV1 {
  expect and {
      state_is_authentic_v1(state),
      state.phase == phase_receive,
      state.selected_purpose == None,
    }
  let receive_candidate = when candidate is {
    None -> state.receive_candidate
    Some(script_hash) -> {
      expect bytearray.length(script_hash) == 28
      if and {
        or {
          state.previous_key == #"",
          bytearray.compare(state.previous_key, script_hash) == Less,
        },
        or {
          state.receive_candidate == #"",
          bytearray.compare(script_hash, state.receive_candidate) == Less,
        },
      } {
        script_hash
      } else {
        state.receive_candidate
      }
    }
  }
  with_checkpoint(
    StateV1 { ..state, field_cursor: state.field_cursor + 1, receive_candidate },
    next_expected_script_hash,
  )
}

/// Finish one complete authenticated output pass. A discovered candidate is
/// emitted exactly once and another pass begins; no candidate means the
/// unique sorted receive-purpose set is exhausted.
pub fn finish_receive_pass_v1(
  state: StateV1,
  next_scan_script_hash: ByteArray,
  next_source_script_hash: ByteArray,
) -> StateV1 {
  expect and {
      state_is_authentic_v1(state),
      state.phase == phase_receive,
      state.selected_purpose == None,
    }
  expect state.receive_candidate != #""
  let emitted = append_purpose_v1(
    state,
    phase_receive,
    state.execution_cursor,
    state.receive_candidate,
    state.receive_candidate,
    state.receive_candidate,
    if state.execution_cursor == state.bound.execution_index {
      next_source_script_hash
    } else {
      next_scan_script_hash
    },
  )
  StateV1 {
    ..emitted,
    field_cursor: 0,
    receive_candidate: #"",
    checkpoint_hash: checkpoint_v1(
      StateV1 { ..emitted, field_cursor: 0, receive_candidate: #"" },
      emitted.next_expected_script_hash,
    ),
  }
}

pub fn advance_non_script_item_v1(
  state: StateV1,
  canonical_key: ByteArray,
  next_expected_script_hash: ByteArray,
) -> StateV1 {
  expect and {
      state_is_authentic_v1(state),
      state.selected_purpose == None,
      or {
        state.previous_key == #"",
        bytearray.compare(state.previous_key, canonical_key) == Less,
      },
    }
  with_checkpoint(
    StateV1 {
      ..state,
      field_cursor: state.field_cursor + 1,
      previous_key: canonical_key,
    },
    next_expected_script_hash,
  )
}

/// Append one script-bearing purpose after its field-specific applied door has
/// proved that it is the next canonical member. Non-script spend/output items
/// advance only the field cursor in their door and never call this function.
pub fn append_purpose_v1(
  state: StateV1,
  purpose_kind: Int,
  purpose_index: Int,
  script_hash: ByteArray,
  subject: ByteArray,
  canonical_key: ByteArray,
  next_expected_script_hash: ByteArray,
) -> StateV1 {
  expect and {
      state_is_authentic_v1(state),
      state.selected_purpose == None,
      purpose_kind == state.phase,
      purpose_kind >= phase_spend,
      purpose_kind <= phase_receive,
      purpose_index >= 0,
      bytearray.length(script_hash) == 28,
      or {
        state.previous_key == #"",
        bytearray.compare(state.previous_key, canonical_key) == Less,
      },
    }
  let selected = if state.execution_cursor == state.bound.execution_index {
    Some(
      SelectedPurposeV1 { purpose_kind, purpose_index, script_hash, subject },
    )
  } else {
    None
  }
  with_checkpoint(
    StateV1 {
      ..state,
      phase: if selected != None {
        phase_inline_source
      } else {
        state.phase
      },
      field_cursor: if selected != None {
        0
      } else {
        state.field_cursor + 1
      },
      execution_cursor: state.execution_cursor + 1,
      previous_key: if selected != None {
        #""
      } else {
        canonical_key
      },
      selected_purpose: selected,
    },
    next_expected_script_hash,
  )
}

/// The applied field door calls this only after proving the current collection
/// exhausted. A selected execution moves directly to source discovery; an
/// unselected one advances through the fixed consensus purpose order.
pub fn finish_purpose_phase_v1(
  state: StateV1,
  next_expected_script_hash: ByteArray,
) -> StateV1 {
  expect and {
      state_is_authentic_v1(state),
      state.phase >= phase_spend,
      state.phase <= phase_receive,
    }
  let next_phase = if state.selected_purpose != None {
    phase_inline_source
  } else {
    state.phase + 1
  }
  // Exhausting receive without reaching the challenged execution coordinate
  // is an honest refusal, not a terminal proof.
  expect next_phase <= phase_inline_source
  with_checkpoint(
    StateV1 { ..state, phase: next_phase, field_cursor: 0, previous_key: #"" },
    next_expected_script_hash,
  )
}

pub fn append_source_v1(
  state: StateV1,
  source: SelectedSourceV1,
  next_expected_script_hash: ByteArray,
) -> StateV1 {
  expect and {
      state_is_authentic_v1(state),
      state.selected_source == None,
      state.phase >= phase_inline_source,
      state.phase <= phase_reference_source,
      source.origin_kind == state.phase - phase_inline_source,
      source.source_index == state.source_cursor,
      source.total_length > 0,
      bytearray.length(source.script_hash) == 28,
      bytearray.length(source.item_commitment) == 32,
    }
  expect Some(purpose) = state.selected_purpose
  let selected = if source.script_hash == purpose.script_hash {
    Some(source)
  } else {
    None
  }
  with_checkpoint(
    StateV1 {
      ..state,
      field_cursor: state.field_cursor + 1,
      source_cursor: state.source_cursor + 1,
      selected_source: selected,
    },
    next_expected_script_hash,
  )
}

pub fn advance_reference_without_source_v1(
  state: StateV1,
  next_expected_script_hash: ByteArray,
) -> StateV1 {
  expect and {
      state_is_authentic_v1(state),
      state.phase == phase_reference_source,
      state.selected_source == None,
    }
  with_checkpoint(
    StateV1 { ..state, field_cursor: state.field_cursor + 1 },
    next_expected_script_hash,
  )
}

/// Collapse the independently reconstructed purpose/source pair into the
/// datum already consumed by logical evaluator step 3.
pub fn authenticated_source_v1(state: StateV1) -> AuthenticatedExecutionSourceV1 {
  expect state_is_authentic_v1(state)
  expect Some(purpose) = state.selected_purpose
  expect Some(source) = state.selected_source
  expect and {
      purpose.script_hash == source.script_hash,
      purpose.purpose_kind >= phase_spend,
      purpose.purpose_kind <= phase_receive,
    }
  AuthenticatedExecutionSourceV1 {
    bound: state.bound,
    prior_ledger_root: state.bound.prior_ledger_root,
    source_index: source.source_index,
    origin_kind: source.origin_kind,
    source_key: source.source_key,
    language_tag: source.language_tag,
    script_hash: source.script_hash,
    total_length: source.total_length,
    item_commitment: source.item_commitment,
    compact_cbor: state.bound.compact_cbor,
  }
}

pub fn finish_inline_sources_v1(
  state: StateV1,
  next_expected_script_hash: ByteArray,
) -> StateV1 {
  expect and {
      state_is_authentic_v1(state),
      state.phase == phase_inline_source,
    }
  with_checkpoint(
    StateV1 {
      ..state,
      phase: if state.selected_source == None {
        phase_reference_source
      } else {
        phase_complete
      },
      field_cursor: 0,
      source_base_index: state.source_cursor,
    },
    next_expected_script_hash,
  )
}

pub fn finish_reference_sources_v1(
  state: StateV1,
  next_expected_script_hash: ByteArray,
) -> StateV1 {
  expect and {
      state_is_authentic_v1(state),
      state.phase == phase_reference_source,
      state.selected_source != None,
    }
  with_checkpoint(
    StateV1 { ..state, phase: phase_complete },
    next_expected_script_hash,
  )
}
