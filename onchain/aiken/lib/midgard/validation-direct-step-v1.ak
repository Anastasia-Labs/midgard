use midgard/validation_resolution_v1
use midgard/validation_trace_v1

/// Both endpoints belong to the operator's authenticated descriptor. The
/// challenger successor is a claim only; the existing phase resolver proves it.
pub type CommittedStepEvidenceV1 {
  pre_state: validation_trace_v1.ValidationMachineStateV1,
  pre_proof: validation_trace_v1.ValidationTraceProof,
  post_proof: validation_trace_v1.ValidationTraceProof,
  challenger_successor_hash: ByteArray,
}

pub fn prepare(
  descriptor: validation_trace_v1.ValidationTraceDescriptorV1,
  evidence: CommittedStepEvidenceV1,
) -> validation_resolution_v1.ValidationResolutionStateV1 {
  // Address-item selection does not yet establish a unique Signatures successor.
  expect evidence.pre_state.phase != validation_trace_v1.Signatures
  expect evidence.pre_state.phase != validation_trace_v1.Cek
  expect evidence.pre_state.phase != validation_trace_v1.Terminal
  expect validation_trace_v1.machine_state_is_well_formed(evidence.pre_state)
  expect
    evidence.pre_proof.state_hash == validation_trace_v1.hash_machine_state(
      evidence.pre_state,
    )
  expect evidence.post_proof.state_index == evidence.pre_proof.state_index + 1
  expect validation_trace_v1.verify_trace_proof(descriptor, evidence.pre_proof)
  expect validation_trace_v1.verify_trace_proof(descriptor, evidence.post_proof)
  let resolution = validation_resolution_v1.ValidationResolutionStateV1 {
    version: validation_resolution_v1.resolution_version,
    pre_state: evidence.pre_state,
    operator_successor_hash: evidence.post_proof.state_hash,
    challenger_successor_hash: evidence.challenger_successor_hash,
  }
  expect validation_resolution_v1.state_is_well_formed(resolution)
  resolution
}
