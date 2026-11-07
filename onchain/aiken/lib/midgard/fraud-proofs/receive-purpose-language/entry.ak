//// Family entry for `receivePurposeLanguage`. The shared source graph hands
//// over its exact binding. The prover either selects one receive purpose
//// through the shared receive route, or, for a leaf whose recorded
//// `ReceivePurposePlutusV3Forbidden` names no receive coordinate, goes
//// straight to the terminal.

use midgard/computation_thread as ct
use midgard/fraud_proofs/direct_script_derivation/claim.{
  ClaimContextV1, FinalStateV1, ImpossiblePurpose, LeafPurpose,
}
use midgard/fraud_proofs/direct_script_derivation/purpose.{receive}
use midgard/fraud_proofs/direct_script_derivation/source_scan.{BoundIndexV1}
use midgard/fraud_proofs/first_fault_v1.{PurposeCoordinateV1}
use midgard/rejection_reason_v1.{ReceivePurposePlutusV3Forbidden}

pub type ActionV1 {
  Observe { input_index: Int, output_index: Int }
  Contradict { input_index: Int, output_index: Int }
}

pub type Datum =
  ct.StepDatum<BoundIndexV1>

pub type SpendRedeemer =
  ct.StepRedeemer<ActionV1>

pub fn indices_v1(action: ActionV1) -> Pair<Int, Int> {
  when action is {
    Observe { input_index, output_index } -> Pair(input_index, output_index)
    Contradict { input_index, output_index } -> Pair(input_index, output_index)
  }
}

/// The context every receive-language final state carries: the exact
/// source-finish binding, no observed decision, the leaf's own purpose.
pub fn context_v1(bound: BoundIndexV1) -> ClaimContextV1 {
  ClaimContextV1 { bound, observed: None, target: LeafPurpose }
}

/// A recorded receive-language reason whose coordinate is not a receive
/// coordinate (another purpose kind, or a negative index) names no receive
/// purpose. A receive coordinate is decided by the receive route instead.
pub fn contradiction_v1(bound: BoundIndexV1) -> FinalStateV1 {
  expect Some(ReceivePurposePlutusV3Forbidden { purpose_kind, purpose_index }) = bound.bound.subject.rejection_reason
  expect purpose_kind != receive || purpose_index < 0
  FinalStateV1 {
    context: context_v1(bound),
    claim: ImpossiblePurpose {
      coordinate: PurposeCoordinateV1 {
        kind: purpose_kind,
        index: purpose_index,
      },
    },
  }
}
