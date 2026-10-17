//// The claimed purpose reopened in its authenticated domain, before source
//// lookup. Constructor and field order below is wire format.

use midgard/fraud_proofs/direct_script_derivation/first_fault_seam.{
  PurposeCoordinateV1,
}
use midgard/fraud_proofs/direct_script_derivation/types.{PurposeV1}
use midgard/fraud_proofs/missing_redeemer/claim.{ClaimContextV1}

/// 0 NotRequired, 1 ImpossiblePurpose, 2 ExistingPurpose.
pub type ClaimedPurposeFactV1 {
  NotRequired
  ImpossiblePurpose { coordinate: PurposeCoordinateV1 }
  ExistingPurpose { purpose: PurposeV1 }
}

/// Constr 0 [context, purpose].
pub type ClaimPurposeStateV1 {
  context: ClaimContextV1,
  purpose: ClaimedPurposeFactV1,
}
