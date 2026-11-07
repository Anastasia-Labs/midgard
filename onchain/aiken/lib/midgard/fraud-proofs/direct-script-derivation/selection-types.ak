use midgard/fraud_proofs/direct_script_derivation/source_index
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1, PurposeV1}
use midgard/fraud_proofs/first_fault_v1.{PurposeCoordinateV1}

/// A complete authenticated purpose lookup. None is meaningful only after
/// the physical lookup's exhaustion/count check, never a partial scan default.
pub type SelectionV1 {
  bound: BoundV1,
  source_index: source_index.StateV1,
  coordinate: PurposeCoordinateV1,
  purpose: Option<PurposeV1>,
}
