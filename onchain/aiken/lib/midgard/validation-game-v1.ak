use midgard/ledger_state
use midgard/validation_claim_v1
use midgard/validation_dispute_v1

pub type PendingValidationClaimV1 {
  challenged_header_hash: ByteArray,
  challenged_header: ledger_state.HeaderV1,
  claim: validation_claim_v1.ValidationClaimWitnessV1,
  /// The challenger's trace root over states `0..step_count`.
  challenger_root: ByteArray,
  /// S: the challenger's step count.
  step_count: Int,
  /// The challenged block's state-queue `block_time`, read at Open from the
  /// authenticated node. VerifySource's open guard runs from it.
  block_end: Int,
}

pub type ValidationGameStateV1 {
  challenged_header_hash: ByteArray,
  dispute: validation_dispute_v1.ValidationDisputeV1,
}
