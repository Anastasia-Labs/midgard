//// The receive-language fact for the receive purpose the shared receive
//// route selected. An ordinal past the last receive purpose names none. A
//// present PlutusV3 source opens field 8, and the claimed pointer scan
//// decides whether a redeemer reaches the execution; any other source
//// decides the fact by lookup alone. The redeemer actions are the shared
//// adjudication's (`claim_adjudicate.ActionV1`), over the selection datum.

use aiken/merkle_patricia_forestry.{Proof}
use cardano/assets.{PolicyId}
use cardano/transaction.{Input}
use midgard/computation_thread as ct
use midgard/fraud_proofs/direct_script_derivation/claim.{
  ClaimContextV1, ClaimedRedeemerScanV1, FinalStateV1, ImpossiblePurpose,
  SourceAbsent, SourcePresent,
}
use midgard/fraud_proofs/direct_script_derivation/claim_adjudicate.{ActionV1}
use midgard/fraud_proofs/direct_script_derivation/purpose_workers as workers
use midgard/fraud_proofs/direct_script_derivation/redeemer_scan
use midgard/fraud_proofs/direct_script_derivation/selection_types.{SelectionV1}
use midgard/fraud_proofs/direct_script_derivation/source.{SourceV1}
use midgard/fraud_proofs/direct_script_derivation/source_scan.{BoundIndexV1}
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/receive_purpose_language/entry

/// `script_proof_v1.language_tag(PlutusV3Script)`.
pub const plutus_v3_language_tag: Int = 3

pub type Datum =
  ct.StepDatum<SelectionV1>

pub type SpendRedeemer =
  ct.StepRedeemer<ActionV1>

fn context_of_v1(selection: SelectionV1) -> ClaimContextV1 {
  entry.context_v1(
    BoundIndexV1 {
      bound: selection.bound,
      source_index: selection.source_index,
    },
  )
}

/// The receive declaration found no purpose at the selected ordinal.
pub fn resolve_v1(selection: SelectionV1) -> FinalStateV1 {
  expect None = selection.purpose
  FinalStateV1 {
    context: context_of_v1(selection),
    claim: ImpossiblePurpose { coordinate: selection.coordinate },
  }
}

/// No source, or a present source of another language. A PlutusV3 source
/// must take the scan, so this fact never hides one.
pub fn lookup_v1(
  selection: SelectionV1,
  source: Option<SourceV1>,
  proof: Proof,
) -> FinalStateV1 {
  let authenticated = workers.lookup_v1(selection, source, proof)
  FinalStateV1 {
    context: context_of_v1(selection),
    claim: when authenticated.source is {
      None -> SourceAbsent { selection: authenticated }
      Some(present) -> {
        expect present.language_tag != plutus_v3_language_tag
        SourcePresent { selection: authenticated }
      }
    },
  }
}

/// A present PlutusV3 source: opens the claimed field-8 pointer scan for the
/// selected receive purpose.
pub fn lookup_scan_v1(
  selection: SelectionV1,
  source: Option<SourceV1>,
  proof: Proof,
  opening: FieldOpeningV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> ClaimedRedeemerScanV1 {
  let authenticated = workers.lookup_v1(selection, source, proof)
  expect Some(present) = authenticated.source
  expect present.language_tag == plutus_v3_language_tag
  let Pair(scan, _) = redeemer_scan.initial_scan_v1(
    authenticated,
    opening,
    reference_inputs,
    certificate_policy_id,
  )
  ClaimedRedeemerScanV1 { context: context_of_v1(selection), scan }
}
