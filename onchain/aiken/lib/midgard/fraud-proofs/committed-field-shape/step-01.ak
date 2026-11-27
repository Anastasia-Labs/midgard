use midgard/computation_thread as ct
use midgard/fraud_proofs/canonical_decodability/rule.{CommittedFieldClaimV1}
use midgard/fraud_proofs/common.{NativeTxInclusionCarriage}

/// The thread state between two batches of one field's §5.1 walk: which
/// transaction and slot are being read, how many items are still owed, and the
/// byte the next batch starts at.
///
/// Only step 01 writes it, and only at its own address: the thread is minted
/// with no state, and step 01 is the only script that produces one at step 01.
/// So a `WalkState` a step reads is the one the previous batch derived from the
/// same committed bytes, and the batch that resumes it re-authenticates those
/// bytes and checks the slot and the transaction against it.
///
/// **Structurally identical to §12.7's cursor, and deliberately not the same
/// type**, for the reason `step_02.State` gives: the two families' threads are
/// held apart by the script hashes their step-01s pin and by their types, not
/// by convention.
pub type WalkState {
  bad_tx_id: ByteArray,
  field_index: Int,
  remaining: Int,
  offset: Int,
}

pub type Datum =
  ct.StepDatum<WalkState>

/// The inclusion carriage every native family's step-01 takes, plus the one
/// thing this family adds: **which** committed field is accused and how its
/// committed bytes reach the step.
///
/// `claim` is §12.7's `CommittedFieldClaimV1`, reused rather than re-declared —
/// the accusation the two sibling fault kinds make is the same accusation, and
/// §6.1's one-spelling rule applies to a wire type as much as to a scalar. What
/// is deliberately *not* shared is the verdict code space and the states it
/// travels in (`WalkState` above, `step_02.State`).
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
