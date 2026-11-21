//// The observed route of `missingScriptSource`. The shared source lookup
//// hands over a selected purpose and its source-map lookup under the exact
//// source-finish binding; a missing source becomes the decision claim start
//// adjudicates.

use midgard/computation_thread as ct
use midgard/fraud_proofs/direct_script_derivation/claim.{ClaimContextV1}
use midgard/fraud_proofs/direct_script_derivation/purpose_workers.{
  SourceSelectionV1,
}
use midgard/fraud_proofs/direct_script_derivation/redeemer_scan.{
  DecisionStateV1,
}

pub type Args {
  input_index: Int,
  output_index: Int,
}

pub type Datum =
  ct.StepDatum<SourceSelectionV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>

/// `purpose_workers.lookup_v1` already proved the purpose present at a valid
/// coordinate equal to the selection's, and the source map's membership or
/// nonmembership for its script hash; only the absence is required here. A
/// decision on a missing source carries `redeemer_missing: False`.
pub fn observed_context_v1(authenticated: SourceSelectionV1) -> ClaimContextV1 {
  expect None = authenticated.source
  claim.observed_context_v1(
    DecisionStateV1 { authenticated, redeemer_missing: False },
  )
}
