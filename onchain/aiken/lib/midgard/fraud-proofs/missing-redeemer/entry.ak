//// Family entry for `missingRedeemer`. The shared source graph hands over its
//// exact binding; the prover chooses the observed route or, for a leaf whose
//// actual arm is RedeemerMissing, the claimed-only route.

use midgard/computation_thread as ct
use midgard/fraud_proofs/direct_script_derivation/claim.{
  ClaimContextV1, LeafPurpose,
}
use midgard/fraud_proofs/direct_script_derivation/source_scan.{BoundIndexV1}
use midgard/rejection_reason_v1.{RedeemerMissing}

pub type ActionV1 {
  Observe { input_index: Int, output_index: Int }
  Claim { input_index: Int, output_index: Int }
}

pub type Datum =
  ct.StepDatum<BoundIndexV1>

pub type SpendRedeemer =
  ct.StepRedeemer<ActionV1>

pub fn indices_v1(action: ActionV1) -> Pair<Int, Int> {
  when action is {
    Observe { input_index, output_index } -> Pair(input_index, output_index)
    Claim { input_index, output_index } -> Pair(input_index, output_index)
  }
}

/// The claimed-only context. The subject was opened by the shared step-01
/// door, so it is canonical; a reason is present only on a rejected leaf.
pub fn claim_context_v1(bound: BoundIndexV1) -> ClaimContextV1 {
  expect Some(RedeemerMissing { .. }) = bound.bound.subject.rejection_reason
  ClaimContextV1 { bound, observed: None, target: LeafPurpose }
}
