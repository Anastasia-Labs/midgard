//// Adjudication for `missingRedeemer`. `claim-adjudicate` turns the leaf's
//// reopened purpose into a predicate fact; the claimed field-8 roles finish a
//// pointer scan; the terminal decides conviction from the final state alone.

use aiken/merkle_patricia_forestry.{Proof}
use cardano/assets.{PolicyId}
use cardano/transaction.{Input}
use midgard/computation_thread as ct
use midgard/fraud_proofs/direct_script_derivation/first_fault_seam.{
  NetworkOrderContextV1, PurposeFaultPositionV1, RedeemerRequired,
  SourceRequired,
} as seam
use midgard/fraud_proofs/direct_script_derivation/purpose_workers.{
  ProgressArgsV1, SourceSelectionV1,
} as workers
use midgard/fraud_proofs/direct_script_derivation/selection_types.{SelectionV1}
use midgard/fraud_proofs/direct_script_derivation/source.{SourceV1}
use midgard/fraud_proofs/direct_script_derivation/types.{PurposeV1}
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/missing_redeemer/claim.{
  ClaimContextV1, ClaimedPredicateFactV1, ClaimedRedeemerScanV1, FinalStateV1,
}
use midgard/fraud_proofs/missing_redeemer/claim_purpose.{ClaimPurposeStateV1}
use midgard/fraud_proofs/missing_redeemer/rule.{DecisionStateV1}
use midgard/rejection_reason_v1 as reason

/// `Lookup` resolves a source without field 8; `LookupScan` also opens field 8
/// for a redeemer-bearing source. Each refuses the other's source language.
pub type ActionV1 {
  Resolve { input_index: Int, output_index: Int }
  Lookup {
    input_index: Int,
    output_index: Int,
    source: Option<SourceV1>,
    proof: Proof,
  }
  LookupScan {
    input_index: Int,
    output_index: Int,
    source: Option<SourceV1>,
    proof: Proof,
    opening: FieldOpeningV1,
  }
}

pub type ClaimedScanActionV1 {
  Scan { args: ProgressArgsV1 }
  Finish { input_index: Int, output_index: Int }
}

pub type Datum =
  ct.StepDatum<ClaimPurposeStateV1>

pub type SpendRedeemer =
  ct.StepRedeemer<ActionV1>

pub type ClaimedScanDatum =
  ct.StepDatum<ClaimedRedeemerScanV1>

pub type ClaimedGrammarRedeemer =
  ct.StepRedeemer<ProgressArgsV1>

pub type ClaimedScanRedeemer =
  ct.StepRedeemer<ClaimedScanActionV1>

pub fn indices_v1(action: ActionV1) -> Pair<Int, Int> {
  when action is {
    Resolve { input_index, output_index } -> Pair(input_index, output_index)
    Lookup { input_index, output_index, .. } -> Pair(input_index, output_index)
    LookupScan { input_index, output_index, .. } ->
      Pair(input_index, output_index)
  }
}

pub fn claimed_scan_indices_v1(action: ClaimedScanActionV1) -> Pair<Int, Int> {
  when action is {
    Scan { args } -> Pair(args.input_index, args.output_index)
    Finish { input_index, output_index } -> Pair(input_index, output_index)
  }
}

fn leaf_reason_v1(context: ClaimContextV1) -> reason.RejectionReasonV1 {
  expect Some(r) = context.bound.bound.subject.rejection_reason
  r
}

/// Facts that need no source lookup: no purpose coordinate, an impossible
/// purpose, or the observed route's own purpose, whose source lookup and
/// pointer scan the carried decision already completed.
pub fn resolve_v1(state: ClaimPurposeStateV1) -> FinalStateV1 {
  let context = state.context
  let fact = when state.purpose is {
    claim_purpose.NotRequired -> claim.NotRequired
    claim_purpose.ImpossiblePurpose { coordinate } ->
      claim.ImpossiblePurpose { coordinate }
    claim_purpose.ExistingPurpose { purpose } -> {
      expect Some(decision) = context.observed
      expect decision.authenticated.selection.purpose == Some(purpose)
      when leaf_reason_v1(context) is {
        reason.RedeemerMissing { .. } ->
          if decision.redeemer_missing {
            claim.RedeemerAbsent { decision }
          } else {
            claim.RedeemerPresent { decision }
          }
        reason.ScriptSourceMissing { .. } ->
          claim.SourcePresent { selection: decision.authenticated }
        _ -> fail
      }
    }
  }
  FinalStateV1 { context, claim: fact }
}

/// Source-map membership or nonmembership of the reopened purpose, under the
/// exact source-finish binding the context carries.
fn lookup_selection_v1(
  state: ClaimPurposeStateV1,
  source: Option<SourceV1>,
  proof: Proof,
) -> SourceSelectionV1 {
  expect claim_purpose.ExistingPurpose { purpose } = state.purpose
  let PurposeV1 { coordinate, .. } = purpose
  workers.lookup_v1(
    SelectionV1 {
      bound: state.context.bound.bound,
      source_index: state.context.bound.source_index,
      coordinate,
      purpose: Some(purpose),
    },
    source,
    proof,
  )
}

pub fn lookup_v1(
  state: ClaimPurposeStateV1,
  source: Option<SourceV1>,
  proof: Proof,
) -> FinalStateV1 {
  let selection = lookup_selection_v1(state, source, proof)
  let fact = when leaf_reason_v1(state.context) is {
    reason.ScriptSourceMissing { .. } ->
      when selection.source is {
        None -> claim.SourceAbsent { selection }
        Some(_) -> claim.SourcePresent { selection }
      }
    reason.RedeemerMissing { .. } ->
      when selection.source is {
        None -> claim.SourceAbsent { selection }
        Some(present) -> {
          expect present.language_tag == 0
          claim.NativeSource { selection }
        }
      }
    _ -> fail
  }
  FinalStateV1 { context: state.context, claim: fact }
}

pub fn lookup_scan_v1(
  state: ClaimPurposeStateV1,
  source: Option<SourceV1>,
  proof: Proof,
  opening: FieldOpeningV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> ClaimedRedeemerScanV1 {
  expect reason.RedeemerMissing { .. } = leaf_reason_v1(state.context)
  let Pair(scan, _) = rule.initial_scan_v1(
    lookup_selection_v1(state, source, proof),
    opening,
    reference_inputs,
    certificate_policy_id,
  )
  ClaimedRedeemerScanV1 { context: state.context, scan }
}

pub fn claimed_certify_v1(
  state: ClaimedRedeemerScanV1,
  args: ProgressArgsV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> ClaimedRedeemerScanV1 {
  ClaimedRedeemerScanV1 {
    ..state,
    scan: rule.certify_v1(
      state.scan,
      args,
      reference_inputs,
      certificate_policy_id,
    ),
  }
}

pub fn claimed_advance_v1(
  state: ClaimedRedeemerScanV1,
  args: ProgressArgsV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> ClaimedRedeemerScanV1 {
  ClaimedRedeemerScanV1 {
    ..state,
    scan: rule.advance_v1(
      state.scan,
      args,
      reference_inputs,
      certificate_policy_id,
    ),
  }
}

pub fn claimed_decision_v1(state: ClaimedRedeemerScanV1) -> FinalStateV1 {
  let decision = rule.decision_v1(state.scan)
  FinalStateV1 {
    context: state.context,
    claim: if decision.redeemer_missing {
      claim.RedeemerAbsent { decision }
    } else {
      claim.RedeemerPresent { decision }
    },
  }
}

/// The leaf's exact RedeemerMissing predicate is refuted by its own fact.
fn claimed_redeemer_missing_is_false_v1(fact: ClaimedPredicateFactV1) -> Bool {
  when fact is {
    claim.ImpossiblePurpose { .. } -> True
    claim.SourceAbsent { .. } -> True
    claim.NativeSource { .. } -> True
    claim.RedeemerPresent { .. } -> True
    claim.RedeemerAbsent { .. } -> False
    _ -> fail
  }
}

/// Whether the leaf's reason is at or before the observed missing redeemer in
/// the machine's traversal order. A leaf whose own purpose predicate is false
/// is never admitted.
fn leaf_admits_observed_v1(
  r: reason.RejectionReasonV1,
  context: NetworkOrderContextV1,
  observed: DecisionStateV1,
  fact: ClaimedPredicateFactV1,
) -> Bool {
  let phase = seam.phase_of_reason_v1(r, context)
  if phase != 8 {
    expect fact == claim.NotRequired
    phase < 8
  } else {
    let observed_position = PurposeFaultPositionV1 {
      coordinate: observed.authenticated.selection.coordinate,
      subcheck: RedeemerRequired,
    }
    when r is {
      reason.RedeemerMalformed { .. } |
      reason.OutputNonCanonical { .. } |
      reason.OutputReferenceScriptMalformed { .. } |
      reason.OutputReferenceScriptNodeLimit { .. } |
      reason.OutputReferenceScriptDepthLimit { .. } |
      reason.ProtectedOutputSignerMissing { .. } |
      reason.MintDeclaredAssetLimit { .. } |
      reason.MintItemMalformed { .. } |
      reason.NetworkIdMismatch -> {
        expect fact == claim.NotRequired
        True
      }
      reason.UnusedScriptWitness { .. } | reason.UnusedRedeemer { .. } -> {
        expect fact == claim.NotRequired
        False
      }
      reason.ScriptSourceMissing { purpose_kind, purpose_index } ->
        when fact is {
          claim.ImpossiblePurpose { .. } -> False
          claim.SourcePresent { .. } -> False
          claim.SourceAbsent { .. } ->
            seam.purpose_position_precedes_or_equals_v1(
              PurposeFaultPositionV1 {
                coordinate: seam.PurposeCoordinateV1 {
                  kind: purpose_kind,
                  index: purpose_index,
                },
                subcheck: SourceRequired,
              },
              observed_position,
            )
          _ -> fail
        }
      reason.RedeemerMissing { purpose_kind, purpose_index } ->
        when fact is {
          claim.RedeemerAbsent { .. } ->
            seam.purpose_position_precedes_or_equals_v1(
              PurposeFaultPositionV1 {
                coordinate: seam.PurposeCoordinateV1 {
                  kind: purpose_kind,
                  index: purpose_index,
                },
                subcheck: RedeemerRequired,
              },
              observed_position,
            )
          _ -> !claimed_redeemer_missing_is_false_v1(fact)
        }
      _ -> fail
    }
  }
}

/// Terminal conviction. An accepted leaf convicts on an observed missing
/// redeemer. A rejected leaf convicts when its own predicate is refuted, or
/// when the observed missing redeemer precedes its reason.
pub fn convicts_v1(state: FinalStateV1) -> Bool {
  let context = state.context
  let bound = context.bound.bound
  when bound.subject.rejection_reason is {
    None -> {
      expect Some(decision) = context.observed
      expect state.claim == claim.NotRequired
      decision.redeemer_missing
    }
    Some(r) ->
      when context.observed is {
        None -> claimed_redeemer_missing_is_false_v1(state.claim)
        Some(decision) -> and {
            decision.redeemer_missing,
            !leaf_admits_observed_v1(
              r,
              bound.network_order_context,
              decision,
              state.claim,
            ),
          }
      }
  }
}
