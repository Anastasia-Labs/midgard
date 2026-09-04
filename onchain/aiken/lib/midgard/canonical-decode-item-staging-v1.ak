use midgard/validation_machine/machine_types.{
  CanonicalDecodeItemObservationV1, CanonicalDecodeItemProofV1,
  CanonicalDecodeItemSourceV1, ValidationOneStepWitnessV1,
}
use midgard/validation_resolution_v1.{PreparedValidationResolutionStateV1}
use midgard/validation_trace_v1.{CanonicalDecode}

pub const version: Int = 1

pub type AuthenticatedCanonicalDecodeItemV1 {
  version: Int,
  base: PreparedValidationResolutionStateV1,
  transition: ValidationOneStepWitnessV1,
}

pub type PreparedCanonicalDecodeItemV1 {
  version: Int,
  authenticated: AuthenticatedCanonicalDecodeItemV1,
  source: CanonicalDecodeItemSourceV1,
}

pub type ObservedCanonicalDecodeItemV1 {
  version: Int,
  prepared: PreparedCanonicalDecodeItemV1,
  observation: CanonicalDecodeItemObservationV1,
}

pub type VerifiedCanonicalDecodeItemV1 {
  version: Int,
  observed: ObservedCanonicalDecodeItemV1,
  proof: CanonicalDecodeItemProofV1,
}

pub fn authenticate(
  base: PreparedValidationResolutionStateV1,
  transition: ValidationOneStepWitnessV1,
) -> AuthenticatedCanonicalDecodeItemV1 {
  expect base.resolution.pre_state.phase == CanonicalDecode
  AuthenticatedCanonicalDecodeItemV1 { version, base, transition }
}

pub fn authenticated_is_well_formed(
  state: AuthenticatedCanonicalDecodeItemV1,
) -> Bool {
  and {
    state.version == version,
    state.base.resolution.pre_state.phase == CanonicalDecode,
  }
}

pub fn prepare(
  authenticated: AuthenticatedCanonicalDecodeItemV1,
  source: CanonicalDecodeItemSourceV1,
) -> PreparedCanonicalDecodeItemV1 {
  expect authenticated_is_well_formed(authenticated)
  PreparedCanonicalDecodeItemV1 { version, authenticated, source }
}

pub fn prepared_is_well_formed(state: PreparedCanonicalDecodeItemV1) -> Bool {
  and {
    state.version == version,
    authenticated_is_well_formed(state.authenticated),
  }
}

pub fn observe(
  prepared: PreparedCanonicalDecodeItemV1,
  observation: CanonicalDecodeItemObservationV1,
) -> ObservedCanonicalDecodeItemV1 {
  expect prepared_is_well_formed(prepared)
  ObservedCanonicalDecodeItemV1 { version, prepared, observation }
}

pub fn observed_is_well_formed(state: ObservedCanonicalDecodeItemV1) -> Bool {
  and {
    state.version == version,
    prepared_is_well_formed(state.prepared),
  }
}

pub fn verify(
  observed: ObservedCanonicalDecodeItemV1,
  proof: CanonicalDecodeItemProofV1,
) -> VerifiedCanonicalDecodeItemV1 {
  expect observed_is_well_formed(observed)
  VerifiedCanonicalDecodeItemV1 { version, observed, proof }
}

pub fn verified_is_well_formed(state: VerifiedCanonicalDecodeItemV1) -> Bool {
  and {
    state.version == version,
    observed_is_well_formed(state.observed),
  }
}
