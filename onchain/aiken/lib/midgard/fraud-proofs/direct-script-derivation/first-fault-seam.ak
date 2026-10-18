//// Narrow first-fault interface consumed by the direct-script-derivation
//// library and its families. The declarations are byte-for-byte copies of the
//// frozen shared first-fault API (network-order context, purpose coordinate,
//// phase ranks and purpose-position order), so the Plutus Data encoding and
//// the traversal order are identical. When the shared module carries these
//// declarations, every importer switches to it and this module is deleted.

use midgard/fraud_proofs/native_tx/types.{TxCompactView}
use midgard/ledger_state.{HeaderV1}
use midgard/rejection_reason_v1 as reason

pub type NetworkOrderContextV1 {
  body_network_mismatch: Bool,
}

/// Both projections must already be authenticated under the same source/header.
pub fn network_order_context_v1(
  compact: TxCompactView,
  header: HeaderV1,
) -> NetworkOrderContextV1 {
  NetworkOrderContextV1 {
    body_network_mismatch: compact.body.network_id != 255 && compact.body.network_id != header.expected_network_id,
  }
}

pub type PurposeCoordinateV1 {
  kind: Int,
  index: Int,
}

pub type PurposeSubcheckV1 {
  SourceRequired
  RedeemerRequired
}

pub type PurposeFaultPositionV1 {
  coordinate: PurposeCoordinateV1,
  subcheck: PurposeSubcheckV1,
}

/// Phase ranks are the machine's forward phase order, including the static
/// rules before InputSets. They are intentionally independent of wire tags.
pub fn phase_of_reason_v1(
  r: reason.RejectionReasonV1,
  context: NetworkOrderContextV1,
) -> Int {
  when r is {
    reason.FieldPreimageLengthMismatch { .. } -> 0
    reason.FieldItemWidthIllegal { .. } -> 0
    reason.NetworkIdMismatch ->
      if context.body_network_mismatch {
        2
      } else {
        8
      }
    reason.FeeBelowMinimum -> 2
    reason.EmptyInputs -> 3
    reason.DuplicateInput { .. } -> 3
    reason.ValidityIntervalMalformed -> 3
    reason.AddressWitnessSignatureInvalid { .. } -> 4
    reason.RequiredSignerUnsigned { .. } -> 4
    reason.WitnessScriptHeaderMalformed { .. } -> 5
    reason.WitnessNativeScriptMalformed { .. } -> 5
    reason.WitnessNativeScriptNodeLimit { .. } -> 5
    reason.WitnessNativeScriptDepthLimit { .. } -> 5
    reason.WitnessNativeScriptFalse { .. } -> 5
    reason.ScriptIntegrityHashMissing -> 6
    reason.ObserversForbiddenOnUntaggedNetwork -> 6
    reason.ObserverOrderInvalid { .. } -> 6
    reason.ValidityIntervalExcludesBlockSlot -> 7
    reason.InputNotFound { .. } -> 7
    reason.InputSpentOutputNonCanonical { .. } -> 7
    reason.ResolvedReferenceScriptMalformed { .. } -> 7
    reason.ResolvedReferenceScriptNodeLimit { .. } -> 7
    reason.ResolvedReferenceScriptDepthLimit { .. } -> 7
    reason.SpendInputSignerMissing { .. } -> 7
    reason.RedeemerMalformed { .. } -> 8
    reason.OutputNonCanonical { .. } -> 8
    reason.OutputReferenceScriptMalformed { .. } -> 8
    reason.OutputReferenceScriptNodeLimit { .. } -> 8
    reason.OutputReferenceScriptDepthLimit { .. } -> 8
    reason.ProtectedOutputSignerMissing { .. } -> 8
    reason.MintDeclaredAssetLimit { .. } -> 8
    reason.ScriptSourceMissing { .. } -> 8
    reason.RedeemerMissing { .. } -> 8
    reason.UnusedScriptWitness { .. } -> 8
    reason.UnusedRedeemer { .. } -> 8
    reason.MintItemMalformed { .. } -> 8
    reason.ExecutionNativeScriptMalformed { .. } -> 9
    reason.ExecutionNativeScriptNodeLimit { .. } -> 9
    reason.ExecutionNativeScriptDepthLimit { .. } -> 9
    reason.ExecutionNativeScriptFalse { .. } -> 9
    reason.ScriptIntegrityHashMismatch -> 10
    reason.ReceivePurposePlutusV3Forbidden { .. } -> 11
    reason.PlutusExecutionFailed { .. } -> 11
    reason.InputAssetAccumulationLimit { .. } -> 12
    reason.OutputAssetAccumulationLimit { .. } -> 12
    reason.MintAssetAccumulationLimit { .. } -> 12
    reason.OutputBelowMinAda { .. } -> 12
    reason.ValueNotPreserved -> 12
    // A Plutus item is decoded where its native siblings are: a witness
    // script at PhaseANativeScripts, an output reference script at
    // ScriptSources. No other item kind exists.
    reason.PlutusScriptUndecodable { item_kind, .. } ->
      if item_kind == 0 {
        5
      } else if item_kind == 1 {
        8
      } else {
        fail
      }
  }
}

pub fn purpose_coordinate_of_reason_v1(
  r: reason.RejectionReasonV1,
) -> Option<PurposeCoordinateV1> {
  when r is {
    reason.ScriptSourceMissing { purpose_kind, purpose_index } |
    reason.RedeemerMissing { purpose_kind, purpose_index } ->
      Some(PurposeCoordinateV1 { kind: purpose_kind, index: purpose_index })
    _ -> None
  }
}

pub fn purpose_subcheck_of_reason_v1(
  r: reason.RejectionReasonV1,
) -> PurposeSubcheckV1 {
  when r is {
    reason.ScriptSourceMissing { .. } -> SourceRequired
    reason.RedeemerMissing { .. } -> RedeemerRequired
    _ -> fail
  }
}

pub fn purpose_coordinate_is_valid_v1(c: PurposeCoordinateV1) -> Bool {
  c.kind >= 0 && c.kind <= 3 && c.index >= 0
}

pub fn purpose_position_precedes_or_equals_v1(
  left: PurposeFaultPositionV1,
  right: PurposeFaultPositionV1,
) -> Bool {
  expect purpose_coordinate_is_valid_v1(left.coordinate)
  expect purpose_coordinate_is_valid_v1(right.coordinate)
  if left.coordinate.kind != right.coordinate.kind {
    left.coordinate.kind < right.coordinate.kind
  } else if left.coordinate.index != right.coordinate.index {
    left.coordinate.index < right.coordinate.index
  } else {
    left.subcheck == SourceRequired || right.subcheck == RedeemerRequired
  }
}
