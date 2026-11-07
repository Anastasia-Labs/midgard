//// Direct source-language eligibility and an exhausted authenticated field-8
//// pointer scan for one selected purpose, shared by the missing-redeemer
//// observed route and the claimed redeemer roles. The scan decides only the
//// local predicate; `claim-adjudicate` decides conviction.

use cardano/assets.{PolicyId}
use cardano/transaction.{Input}
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/purpose_workers.{
  ProgressArgsV1, SourceSelectionV1,
}
use midgard/fraud_proofs/direct_script_derivation/redeemer
use midgard/fraud_proofs/field_opening_v1.{
  FieldOpeningV1, redeemers_field_index,
}

pub const scan_batch: Int = 16

/// Wire format: Constr 0 [authenticated, scan].
pub type ScanStateV1 {
  authenticated: SourceSelectionV1,
  scan: redeemer.PointerStateV1,
}

/// Wire format: Constr 0 [authenticated, redeemer_missing]. The scan result
/// `redeemer_missing` is meaningful only when `authenticated.source` is
/// present; a decision on a missing source carries False.
pub type DecisionStateV1 {
  authenticated: SourceSelectionV1,
  redeemer_missing: Bool,
}

pub fn is_redeemer_bearing_language_v1(language_tag: Int) -> Bool {
  language_tag == 3 || language_tag == 128
}

pub fn initial_scan_v1(
  authenticated: SourceSelectionV1,
  opening: FieldOpeningV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> Pair<ScanStateV1, ByteArray> {
  let selection = authenticated.selection
  expect Some(actual) = selection.purpose
  expect Some(source) = authenticated.source
  expect and {
      actual.coordinate == selection.coordinate,
      source.script_hash == actual.script_hash,
      is_redeemer_bearing_language_v1(source.language_tag),
    }
  let (field_cursor, checkpoint, complete) = cursor.initial_redeemers_v1(
    selection.bound,
    opening,
    reference_inputs,
    certificate_policy_id,
  )
  Pair(
    ScanStateV1 {
      authenticated,
      scan: redeemer.PointerStateV1 {
        cursor: field_cursor,
        coordinate: selection.coordinate,
        found: False,
        complete,
      },
    },
    checkpoint,
  )
}

/// Bounded field-8 grammar certification before the pointer walk. The scan
/// has found nothing yet, so no observation can be carried across it.
pub fn certify_v1(
  state: ScanStateV1,
  args: ProgressArgsV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> ScanStateV1 {
  expect and {
      !state.scan.complete,
      !state.scan.found,
      args.budget <= scan_batch,
    }
  let (advanced, _, complete) = cursor.certify_with_completion_at_v1(
    state.scan.cursor,
    redeemers_field_index,
    args.opening,
    args.checkpoint_bytes,
    args.budget,
    reference_inputs,
    certificate_policy_id,
  )
  ScanStateV1 {
    ..state,
    scan: redeemer.PointerStateV1 { ..state.scan, cursor: advanced, complete },
  }
}

/// One bounded batch of the authenticated field-8 pointer walk.
pub fn advance_v1(
  state: ScanStateV1,
  args: ProgressArgsV1,
  reference_inputs: List<Input>,
  certificate_policy_id: PolicyId,
) -> ScanStateV1 {
  expect args.budget <= scan_batch
  let Pair(advanced, _) = redeemer.advance_pointers_v1(
    state.scan,
    args.opening,
    args.checkpoint_bytes,
    args.budget,
    reference_inputs,
    certificate_policy_id,
  )
  ScanStateV1 { ..state, scan: advanced }
}

pub fn decision_v1(state: ScanStateV1) -> DecisionStateV1 {
  expect and {
      state.scan.complete,
      state.scan.cursor.phase == cursor.Walking,
      state.scan.cursor.field_index == redeemers_field_index,
      state.scan.cursor.bound == state.authenticated.selection.bound,
      state.scan.coordinate == state.authenticated.selection.coordinate,
    }
  DecisionStateV1 {
    authenticated: state.authenticated,
    redeemer_missing: !state.scan.found,
  }
}
