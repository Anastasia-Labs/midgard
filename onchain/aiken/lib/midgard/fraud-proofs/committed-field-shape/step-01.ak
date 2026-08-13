use midgard/computation_thread as ct
use midgard/fraud_proofs/canonical_decodability/rule.{CommittedFieldClaimV1}
use midgard/fraud_proofs/common.{NativeTxInclusionCarriage}

pub type Datum =
  ct.StepDatum<Data>

/// The inclusion carriage every native family's step-01 takes, plus the one
/// thing this family adds: **which** committed field is accused and how its
/// committed bytes reach the step.
///
/// `claim` is §12.7's `CommittedFieldClaimV1`, reused rather than re-declared —
/// the accusation the two sibling fault kinds make is the same accusation, and
/// §6.1's one-spelling rule applies to a wire type as much as to a scalar. See
/// `committed_field_shape/rule.committed_field_shape_v1` for the full argument,
/// including what is deliberately *not* shared (the verdict code space and the
/// state it travels in).
///
/// The two members are separate because they carry different things through
/// different checks. `inclusion` is the block-side evidence — the leaf, its
/// membership proof and the state-queue node — and it is
/// `pass_native_tx_to_next_step_carried`'s argument unchanged, so this family
/// inherits the published-chunk route for deep membership proofs for free.
/// `claim` is the field-side evidence, and it is checked by the §8.8 door
/// against the compact structure the first half just authenticated.
pub type Args {
  inclusion: NativeTxInclusionCarriage,
  claim: CommittedFieldClaimV1,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
