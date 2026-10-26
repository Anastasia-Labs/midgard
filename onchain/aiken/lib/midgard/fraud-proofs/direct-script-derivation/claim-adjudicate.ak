//// Adjudication shared by the direct purpose families (`missingRedeemer`,
//// `missingScriptSource`, `unusedRedeemer`). `claim-adjudicate` turns the
//// reopened purpose into a predicate fact; the claimed field-8 roles finish a
//// pointer scan; the terminal decides conviction from the final state alone.
//// A `LeafPurpose` context targets the leaf's own purpose and is created only
//// by the first two families' entries and observed routes. A
//// `RedeemerPointer` context is created only by the unused-redeemer field-8
//// walk, which authenticated the redeemer's index and pointer.

use aiken/merkle_patricia_forestry.{Proof}
use cardano/assets.{PolicyId}
use cardano/transaction.{Input}
use midgard/computation_thread as ct
use midgard/fraud_proofs/direct_script_derivation/claim.{
  ClaimContextV1, ClaimedPredicateFactV1, ClaimedRedeemerScanV1, FinalStateV1,
  LeafPurpose, RedeemerPointer,
}
use midgard/fraud_proofs/direct_script_derivation/claim_purpose.{
  ClaimPurposeStateV1,
}
use midgard/fraud_proofs/direct_script_derivation/purpose_workers.{
  ProgressArgsV1, SourceSelectionV1,
} as workers
use midgard/fraud_proofs/direct_script_derivation/redeemer_scan.{
  DecisionStateV1,
} as rule
use midgard/fraud_proofs/direct_script_derivation/selection_types.{SelectionV1}
use midgard/fraud_proofs/direct_script_derivation/source.{SourceV1}
use midgard/fraud_proofs/direct_script_derivation/types.{PurposeV1}
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/first_fault_v1.{
  NetworkOrderContextV1, PurposeFaultPositionV1, PurposeSubcheckV1,
  RedeemerRequired, SourceRequired, UnusedAuditFault, UnusedRedeemerAudit,
} as first_fault
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
/// purpose, or the observed route's own purpose, whose source lookup (and,
/// for a present source, pointer scan) the carried decision already completed.
pub fn resolve_v1(state: ClaimPurposeStateV1) -> FinalStateV1 {
  let context = state.context
  let fact = when state.purpose is {
    claim_purpose.NotRequired -> claim.NotRequired
    claim_purpose.ImpossiblePurpose { coordinate } ->
      claim.ImpossiblePurpose { coordinate }
    claim_purpose.ExistingPurpose { purpose } -> {
      expect Some(decision) = context.observed
      let selection = decision.authenticated
      expect selection.selection.purpose == Some(purpose)
      when leaf_reason_v1(context) is {
        reason.RedeemerMissing { .. } ->
          if selection.source == None {
            claim.SourceAbsent { selection }
          } else if decision.redeemer_missing {
            claim.RedeemerAbsent { decision }
          } else {
            claim.RedeemerPresent { decision }
          }
        reason.ScriptSourceMissing { .. } ->
          if selection.source == None {
            claim.SourceAbsent { selection }
          } else {
            claim.SourcePresent { selection }
          }
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

/// A redeemer pointer's purpose consumes the redeemer exactly when its source
/// is present and not native; the producer requests a redeemer for every
/// non-native source, and native is language 0.
pub fn lookup_v1(
  state: ClaimPurposeStateV1,
  source: Option<SourceV1>,
  proof: Proof,
) -> FinalStateV1 {
  let selection = lookup_selection_v1(state, source, proof)
  let fact = when state.context.target is {
    RedeemerPointer { .. } ->
      when selection.source is {
        None -> claim.SourceAbsent { selection }
        Some(present) ->
          if present.language_tag == 0 {
            claim.NativeSource { selection }
          } else {
            claim.SourcePresent { selection }
          }
      }
    LeafPurpose -> leaf_lookup_fact_v1(state.context, selection)
  }
  FinalStateV1 { context: state.context, claim: fact }
}

fn leaf_lookup_fact_v1(
  context: ClaimContextV1,
  selection: SourceSelectionV1,
) -> ClaimedPredicateFactV1 {
  when leaf_reason_v1(context) is {
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

/// The leaf's exact ScriptSourceMissing predicate is refuted by its own fact.
fn claimed_source_missing_is_false_v1(fact: ClaimedPredicateFactV1) -> Bool {
  when fact is {
    claim.ImpossiblePurpose { .. } -> True
    claim.SourcePresent { .. } -> True
    claim.SourceAbsent { .. } -> False
    _ -> fail
  }
}

/// The leaf's own purpose predicate is refuted by its fact.
fn claimed_predicate_is_false_v1(
  r: reason.RejectionReasonV1,
  fact: ClaimedPredicateFactV1,
) -> Bool {
  when r is {
    reason.RedeemerMissing { .. } -> claimed_redeemer_missing_is_false_v1(fact)
    reason.ScriptSourceMissing { .. } ->
      claimed_source_missing_is_false_v1(fact)
    _ -> fail
  }
}

/// The purpose fault an observed decision proves at its own coordinate: a
/// missing source, or a redeemer-bearing source with no redeemer at the
/// purpose's pointer. `redeemer_missing` is a scan result, and a decision
/// carries one only for a present source.
pub fn observed_fault_v1(
  decision: DecisionStateV1,
) -> Option<PurposeFaultPositionV1> {
  let coordinate = decision.authenticated.selection.coordinate
  let subcheck: Option<PurposeSubcheckV1> = when
    decision.authenticated.source
  is {
    None -> Some(SourceRequired)
    Some(_) ->
      if decision.redeemer_missing {
        Some(RedeemerRequired)
      } else {
        None
      }
  }
  when subcheck is {
    None -> None
    Some(subcheck) -> Some(PurposeFaultPositionV1 { coordinate, subcheck })
  }
}

/// Whether the leaf's reason is at or before the observed purpose fault in
/// the machine's traversal order. A leaf whose own purpose predicate is false
/// is never admitted.
fn leaf_admits_observed_v1(
  r: reason.RejectionReasonV1,
  context: NetworkOrderContextV1,
  observed_position: PurposeFaultPositionV1,
  fact: ClaimedPredicateFactV1,
) -> Bool {
  let phase = first_fault.phase_of_reason_v1(r, context)
  if phase != 8 {
    expect fact == claim.NotRequired
    phase < 8
  } else {
    when r is {
      reason.RedeemerMalformed { .. } |
      reason.OutputNonCanonical { .. } |
      reason.OutputReferenceScriptMalformed { .. } |
      reason.OutputReferenceScriptNodeLimit { .. } |
      reason.OutputReferenceScriptDepthLimit { .. } |
      reason.ProtectedOutputSignerMissing { .. } |
      reason.MintDeclaredAssetLimit { .. } |
      reason.MintItemMalformed { .. } |
      reason.PlutusScriptUndecodable { .. } |
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
          claim.SourceAbsent { .. } ->
            first_fault.purpose_position_precedes_or_equals_v1(
              PurposeFaultPositionV1 {
                coordinate: first_fault.PurposeCoordinateV1 {
                  kind: purpose_kind,
                  index: purpose_index,
                },
                subcheck: SourceRequired,
              },
              observed_position,
            )
          _ -> !claimed_source_missing_is_false_v1(fact)
        }
      reason.RedeemerMissing { purpose_kind, purpose_index } ->
        when fact is {
          claim.RedeemerAbsent { .. } ->
            first_fault.purpose_position_precedes_or_equals_v1(
              PurposeFaultPositionV1 {
                coordinate: first_fault.PurposeCoordinateV1 {
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

/// Terminal conviction over the context's target.
pub fn convicts_v1(state: FinalStateV1) -> Bool {
  when state.context.target is {
    LeafPurpose -> leaf_purpose_convicts_v1(state)
    RedeemerPointer { redeemer_index, .. } ->
      redeemer_pointer_convicts_v1(state, redeemer_index)
  }
}

/// Whether the walked redeemer is one the producer leaves unused: a repeat of
/// an earlier pointer, a pointer with no purpose, or a native purpose's
/// pointer. `Some(False)`: a non-native purpose consumes it. `None`: the fact
/// decides neither; a purpose without a source is a purpose fault, which
/// precedes the audit and belongs to `missingScriptSource`, and an
/// out-of-range index names no redeemer.
fn redeemer_unused_v1(fact: ClaimedPredicateFactV1) -> Option<Bool> {
  when fact is {
    claim.RedeemerRepeated -> Some(True)
    claim.ImpossiblePurpose { .. } -> Some(True)
    claim.NativeSource { .. } -> Some(True)
    claim.SourcePresent { .. } -> Some(False)
    claim.SourceAbsent { .. } -> None
    claim.RedeemerOutOfRange -> None
    _ -> fail
  }
}

/// A leaf naming this redeemer as unused is refuted when a non-native purpose
/// consumes it or the field has no redeemer at its index. Any other leaf,
/// accepted ones included, convicts on an unused redeemer whose audit
/// position precedes the leaf's reason.
fn redeemer_pointer_convicts_v1(
  state: FinalStateV1,
  redeemer_index: Int,
) -> Bool {
  let bound = state.context.bound.bound
  let unused = redeemer_unused_v1(state.claim)
  let names_this_redeemer = when bound.subject.rejection_reason is {
    Some(reason.UnusedRedeemer { redeemer_index: leaf_index }) ->
      leaf_index == redeemer_index
    _ -> False
  }
  if names_this_redeemer {
    unused == Some(False) || state.claim == claim.RedeemerOutOfRange
  } else {
    unused == Some(True) && !first_fault.leaf_admits_later_fault_v1(
      bound.subject,
      bound.network_order_context,
      UnusedAuditFault(UnusedRedeemerAudit { redeemer_index }),
    )
  }
}

/// An accepted leaf convicts on an observed purpose fault. A rejected leaf
/// convicts when its own predicate is refuted, or when the observed purpose
/// fault precedes its reason.
fn leaf_purpose_convicts_v1(state: FinalStateV1) -> Bool {
  let context = state.context
  let bound = context.bound.bound
  when bound.subject.rejection_reason is {
    None -> {
      expect Some(decision) = context.observed
      expect state.claim == claim.NotRequired
      observed_fault_v1(decision) != None
    }
    Some(r) ->
      when context.observed is {
        None -> claimed_predicate_is_false_v1(r, state.claim)
        Some(decision) ->
          when observed_fault_v1(decision) is {
            None -> False
            Some(position) ->
              !leaf_admits_observed_v1(
                r,
                bound.network_order_context,
                position,
                state.claim,
              )
          }
      }
  }
}
