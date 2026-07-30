use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/validation_dispute_v1
use midgard/validation_machine_v1
use midgard/validation_trace_v1

pub const resolution_version: Int = 1

pub const prepared_resolution_version: Int = 1

pub const winning_resolution_version: Int = 1

/// Authenticated one-step boundary handed from the dispute hub to exactly one
/// narrow resolver. Trace proofs are checked by the hub and intentionally are
/// not copied into this datum.
pub type ValidationResolutionStateV1 {
  version: Int,
  pre_state: validation_trace_v1.ValidationMachineStateV1,
  operator_successor_hash: ByteArray,
  challenger_successor_hash: ByteArray,
}

pub type ValidationBoundaryEvidenceV1 {
  pre_state: validation_trace_v1.ValidationMachineStateV1,
  operator_post: validation_trace_v1.ValidationTraceProof,
  challenger_post: validation_trace_v1.ValidationTraceProof,
}

pub type PreparedValidationResolutionStateV1 {
  version: Int,
  resolution: ValidationResolutionStateV1,
  evidence_hash: ByteArray,
}

pub type WinningValidationResolutionStateV1 {
  version: Int,
}

const one_step_evidence_domain: ByteArray =
  #"4d69646761726456616c69646174696f6e4f6e655374657045766964656e63655631"

pub fn hash_one_step_evidence(transition: Data, auxiliary: Data) -> ByteArray {
  one_step_evidence_domain
    |> bytearray.concat(cbor.serialise([transition, auxiliary]))
    |> blake2b_256
}

pub fn resolver_index(phase: validation_trace_v1.ValidationPhase) -> Int {
  when phase is {
    validation_trace_v1.CanonicalDecode -> 0
    validation_trace_v1.CompactBinding -> 1
    validation_trace_v1.StaticLedgerRules -> 2
    validation_trace_v1.InputSets -> 3
    validation_trace_v1.Signatures -> 4
    validation_trace_v1.PhaseANativeScripts -> 5
    validation_trace_v1.PhaseAScriptPreconditions -> 6
    validation_trace_v1.ResolveInputs -> 7
    validation_trace_v1.ScriptSources -> 8
    validation_trace_v1.NativeScripts -> 9
    validation_trace_v1.ScriptIntegrity -> 10
    validation_trace_v1.Cek -> 11
    validation_trace_v1.ValueAndMint -> 12
    validation_trace_v1.LedgerDelta -> 13
    validation_trace_v1.Terminal -> -1
  }
}

pub const resolver_count: Int = 14

pub fn one_step_boundary_is_authenticated(
  dispute: validation_dispute_v1.ValidationDisputeV1,
  pre_state: validation_trace_v1.ValidationMachineStateV1,
  operator_post: validation_trace_v1.ValidationTraceProof,
  challenger_post: validation_trace_v1.ValidationTraceProof,
) -> Bool {
  and {
    dispute.version == validation_dispute_v1.dispute_version,
    dispute.turn == validation_dispute_v1.ReadyForOneStep,
    dispute.high_index == dispute.low_index + 1,
    resolver_index(pre_state.phase) >= 0,
    validation_trace_v1.machine_state_is_well_formed(pre_state),
    validation_trace_v1.hash_machine_state(pre_state) == dispute.agreed_low_hash,
    operator_post.state_index == dispute.high_index,
    challenger_post.state_index == dispute.high_index,
    validation_trace_v1.verify_trace_proof(
      dispute.operator_descriptor,
      operator_post,
    ),
    validation_trace_v1.verify_trace_proof(
      dispute.challenger_descriptor,
      challenger_post,
    ),
    operator_post.state_hash == dispute.operator_high_hash,
    challenger_post.state_hash == dispute.challenger_high_hash,
  }
}

pub fn prepare(
  dispute: validation_dispute_v1.ValidationDisputeV1,
  pre_state: validation_trace_v1.ValidationMachineStateV1,
  operator_post: validation_trace_v1.ValidationTraceProof,
  challenger_post: validation_trace_v1.ValidationTraceProof,
) -> ValidationResolutionStateV1 {
  expect
    one_step_boundary_is_authenticated(
      dispute,
      pre_state,
      operator_post,
      challenger_post,
    )
  ValidationResolutionStateV1 {
    version: resolution_version,
    pre_state,
    operator_successor_hash: operator_post.state_hash,
    challenger_successor_hash: challenger_post.state_hash,
  }
}

pub fn state_is_well_formed(state: ValidationResolutionStateV1) -> Bool {
  and {
    state.version == resolution_version,
    validation_trace_v1.machine_state_is_well_formed(state.pre_state),
    resolver_index(state.pre_state.phase) >= 0,
    bytearray.length(state.operator_successor_hash) == 32,
    bytearray.length(state.challenger_successor_hash) == 32,
    state.operator_successor_hash != state.challenger_successor_hash,
  }
}

pub fn challenger_uniquely_wins(
  state: ValidationResolutionStateV1,
  operator_successor: validation_trace_v1.ValidationMachineStateV1,
  challenger_successor: validation_trace_v1.ValidationMachineStateV1,
  operator_transition_is_valid: Bool,
  challenger_transition_is_valid: Bool,
) -> Bool {
  and {
    state_is_well_formed(state),
    validation_trace_v1.hash_machine_state(operator_successor) == state.operator_successor_hash,
    validation_trace_v1.hash_machine_state(challenger_successor) == state.challenger_successor_hash,
    !operator_transition_is_valid,
    challenger_transition_is_valid,
  }
}

/// A valid deterministic successor committed by the challenger is sufficient
/// to resolve the one-step dispute. The authenticated operator hash is already
/// distinct, so revealing the operator's potentially large witness again
/// would only consume proof-envelope capacity without adding soundness.
pub fn challenger_wins_with_valid_successor(
  state: ValidationResolutionStateV1,
  challenger_successor: validation_trace_v1.ValidationMachineStateV1,
  challenger_transition_is_valid: Bool,
) -> Bool {
  and {
    state_is_well_formed(state),
    validation_trace_v1.hash_machine_state(challenger_successor) == state.challenger_successor_hash,
    challenger_transition_is_valid,
  }
}

pub fn prepare_semantic_resolution(
  state: ValidationResolutionStateV1,
  transition: validation_machine_v1.ValidationOneStepWitnessV1,
  auxiliary: Data,
) -> PreparedValidationResolutionStateV1 {
  expect state_is_well_formed(state)
  expect
    validation_trace_v1.hash_machine_state(transition.claimed_successor) == state.challenger_successor_hash
  expect
    validation_machine_v1.structural_transition_is_valid(
      state.pre_state,
      transition,
    )
  let transition_data: Data = transition
  PreparedValidationResolutionStateV1 {
    version: prepared_resolution_version,
    resolution: state,
    evidence_hash: hash_one_step_evidence(transition_data, auxiliary),
  }
}

pub fn prepared_resolution_is_well_formed(
  state: PreparedValidationResolutionStateV1,
) -> Bool {
  and {
    state.version == prepared_resolution_version,
    state_is_well_formed(state.resolution),
    bytearray.length(state.evidence_hash) == 32,
  }
}

pub fn winning_resolution() -> WinningValidationResolutionStateV1 {
  WinningValidationResolutionStateV1 { version: winning_resolution_version }
}

pub fn winning_resolution_is_well_formed(
  state: WinningValidationResolutionStateV1,
) -> Bool {
  state.version == winning_resolution_version
}
