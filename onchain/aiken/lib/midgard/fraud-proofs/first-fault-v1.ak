//// Traversal precedence for canonical, resolution and direct-purpose faults,
//// and for the later faults: the ScriptSources unused audits and the
//// NativeScripts, ScriptIntegrity, Cek and ValueAndMint phases.
//// Constructor tags and raw input ordinals are not traversal keys.

use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/types.{TxCompactView}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/ledger_state.{HeaderV1}
use midgard/rejection_reason_v1 as reason

pub type CanonicalPositionV1 {
  CanonicalItem { field_index: Int, item_index: Int }
  CanonicalFieldEnd { field_index: Int }
}

/// Constructed by a source-opening consumer from its authenticated original
/// field position. `out_ref` is the canonical input payload, never a hash or a
/// replay cursor. Both inputs must have passed CanonicalDecode/InputSets.
pub type ResolutionPositionV1 {
  source_kind: Int,
  input_index: Int,
  out_ref: ByteArray,
  subcheck: Int,
}

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

/// The owning CT route proves existence/nonexistence under the same source.
/// ExistingPurpose does not establish that the leaf's failure predicate is true.
pub type ClaimedPurposeOrderV1 {
  ExistingPurpose { coordinate: PurposeCoordinateV1 }
  ImpossiblePurpose { coordinate: PurposeCoordinateV1 }
}

pub type ClaimedFaultOrderV1 {
  NotRequired
  ResolutionReopened(ResolutionPositionV1)
  PurposeReopened(ClaimedPurposeOrderV1)
  /// The source-opening consumer established that the leaf's own resolution
  /// coordinate names no input: a field other than 0 or 1, a negative index,
  /// or an index at or past that field's authenticated item count.
  ResolutionOutOfDomain { source_kind: Int, input_index: Int }
}

pub type FaultPositionV1 {
  CanonicalFault(CanonicalPositionV1)
  ResolutionFault(ResolutionPositionV1)
  PurposeFault(PurposeFaultPositionV1)
}

pub const input_absence_subcheck: Int = 0

pub const output_canonicity_subcheck: Int = 1

pub const reference_script_malformed_subcheck: Int = 2

pub const reference_script_node_subcheck: Int = 3

pub const reference_script_depth_subcheck: Int = 4

pub const spend_signer_subcheck: Int = 5

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

pub fn canonical_position_of_reason_v1(
  r: reason.RejectionReasonV1,
) -> CanonicalPositionV1 {
  when r is {
    reason.FieldItemWidthIllegal { field_index, item_index } ->
      CanonicalItem { field_index, item_index }
    reason.FieldPreimageLengthMismatch { field_index } ->
      CanonicalFieldEnd { field_index }
    _ -> fail
  }
}

pub fn canonical_position_precedes_or_equals_v1(
  left: CanonicalPositionV1,
  right: CanonicalPositionV1,
) -> Bool {
  let left_field = when left is {
    CanonicalItem { field_index, .. } -> field_index
    CanonicalFieldEnd { field_index } -> field_index
  }
  let right_field = when right is {
    CanonicalItem { field_index, .. } -> field_index
    CanonicalFieldEnd { field_index } -> field_index
  }
  if left_field != right_field {
    left_field < right_field
  } else {
    when Pair(left, right) is {
      Pair(CanonicalItem { item_index: l, .. },
      CanonicalItem { item_index: r, .. }) -> l <= r
      Pair(CanonicalItem { .. }, CanonicalFieldEnd { .. }) -> True
      Pair(CanonicalFieldEnd { .. }, CanonicalItem { .. }) -> False
      Pair(CanonicalFieldEnd { .. }, CanonicalFieldEnd { .. }) -> True
    }
  }
}

/// A rejected leaf admits a fault at or after its named CanonicalDecode
/// position. A later phase never admits an earlier CanonicalDecode fault.
fn leaf_admits_canonical_fault_v1(
  subject: substrate.VerdictSubjectV1,
  observed: CanonicalPositionV1,
) -> Bool {
  expect substrate.subject_is_canonical_v1(subject)
  when subject.rejection_reason is {
    None -> False
    Some(r) ->
      when r is {
        reason.FieldItemWidthIllegal { .. } |
        reason.FieldPreimageLengthMismatch { .. } ->
          canonical_position_precedes_or_equals_v1(
            canonical_position_of_reason_v1(r),
            observed,
          )
        _ -> False
      }
  }
}

/// The source-opening step obtains this coordinate from the actual reason,
/// then opens that exact source field/index before attaching an out-ref.
pub fn resolution_coordinate_of_reason_v1(
  r: reason.RejectionReasonV1,
) -> Option<Pair<Int, Int>> {
  when r is {
    reason.InputNotFound { source_kind, input_index } ->
      Some(Pair(source_kind, input_index))
    reason.InputSpentOutputNonCanonical { source_kind, input_index } ->
      Some(Pair(source_kind, input_index))
    reason.ResolvedReferenceScriptMalformed { source_kind, input_index } ->
      Some(Pair(source_kind, input_index))
    reason.ResolvedReferenceScriptNodeLimit { source_kind, input_index } ->
      Some(Pair(source_kind, input_index))
    reason.ResolvedReferenceScriptDepthLimit { source_kind, input_index } ->
      Some(Pair(source_kind, input_index))
    reason.SpendInputSignerMissing { input_index } -> Some(Pair(0, input_index))
    _ -> None
  }
}

pub fn resolution_subcheck_of_reason_v1(r: reason.RejectionReasonV1) -> Int {
  when r is {
    reason.InputNotFound { .. } -> input_absence_subcheck
    reason.InputSpentOutputNonCanonical { .. } -> output_canonicity_subcheck
    reason.ResolvedReferenceScriptMalformed { .. } ->
      reference_script_malformed_subcheck
    reason.ResolvedReferenceScriptNodeLimit { .. } ->
      reference_script_node_subcheck
    reason.ResolvedReferenceScriptDepthLimit { .. } ->
      reference_script_depth_subcheck
    reason.SpendInputSignerMissing { .. } -> spend_signer_subcheck
    _ -> fail
  }
}

pub fn resolution_position_precedes_or_equals_v1(
  left: ResolutionPositionV1,
  right: ResolutionPositionV1,
) -> Bool {
  expect and {
      left.source_kind == 0 || left.source_kind == 1,
      right.source_kind == 0 || right.source_kind == 1,
      left.input_index >= 0,
      right.input_index >= 0,
      bytearray.length(left.out_ref) == 38,
      bytearray.length(right.out_ref) == 38,
      left.subcheck >= input_absence_subcheck && left.subcheck <= spend_signer_subcheck,
      right.subcheck >= input_absence_subcheck && right.subcheck <= spend_signer_subcheck,
    }
  when bytearray.compare(left.out_ref, right.out_ref) is {
    Less -> True
    Greater -> False
    Equal -> {
      // InputSets has excluded duplicate outrefs, including across fields.
      // An equality at different raw positions is not an alternative order.
      expect
        left.source_kind == right.source_kind && left.input_index == right.input_index
      left.subcheck <= right.subcheck
    }
  }
}

fn leaf_admits_resolution_fault_v1(
  subject: substrate.VerdictSubjectV1,
  observed: ResolutionPositionV1,
  context: NetworkOrderContextV1,
  claimed: ClaimedFaultOrderV1,
) -> Bool {
  expect substrate.subject_is_canonical_v1(subject)
  when subject.rejection_reason is {
    None -> {
      expect claimed == NotRequired
      False
    }
    Some(r) -> {
      let phase = phase_of_reason_v1(r, context)
      if phase < 7 {
        expect claimed == NotRequired
        True
      } else if phase > 7 {
        expect claimed == NotRequired
        False
      } else if r == reason.ValidityIntervalExcludesBlockSlot {
        expect claimed == NotRequired
        True
      } else {
        expect Some(Pair(kind, index)) = resolution_coordinate_of_reason_v1(r)
        when claimed is {
          // The machine never names an input outside the transaction, so such
          // a leaf admits no fault, and no position is compared.
          ResolutionOutOfDomain { source_kind, input_index } -> {
            expect source_kind == kind && input_index == index
            False
          }
          ResolutionReopened(point) -> {
            expect and {
                point.source_kind == kind,
                point.input_index == index,
                point.subcheck == resolution_subcheck_of_reason_v1(r),
              }
            resolution_position_precedes_or_equals_v1(point, observed)
          }
          _ -> fail
        }
      }
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

fn leaf_admits_purpose_fault_v1(
  subject: substrate.VerdictSubjectV1,
  context: NetworkOrderContextV1,
  observed: PurposeFaultPositionV1,
  claimed: ClaimedFaultOrderV1,
) -> Bool {
  expect purpose_coordinate_is_valid_v1(observed.coordinate)
  when subject.rejection_reason is {
    None -> {
      expect claimed == NotRequired
      False
    }
    Some(r) -> {
      let phase = phase_of_reason_v1(r, context)
      if phase != 8 {
        expect claimed == NotRequired
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
            expect claimed == NotRequired
            True
          }
          reason.UnusedScriptWitness { .. } | reason.UnusedRedeemer { .. } -> {
            expect claimed == NotRequired
            False
          }
          reason.ScriptSourceMissing { .. } | reason.RedeemerMissing { .. } -> {
            expect PurposeReopened(receipt) = claimed
            expect Some(coordinate) = purpose_coordinate_of_reason_v1(r)
            when receipt is {
              ExistingPurpose { coordinate: reopened } -> {
                expect reopened == coordinate
                purpose_position_precedes_or_equals_v1(
                  PurposeFaultPositionV1 {
                    coordinate,
                    subcheck: purpose_subcheck_of_reason_v1(r),
                  },
                  observed,
                )
              }
              ImpossiblePurpose { coordinate: reopened } -> {
                // The owning CT route proves nonexistence, including in-domain
                // exhaustion or a key credential; this is not a free redeemer.
                expect reopened == coordinate
                False
              }
            }
          }
          _ -> fail
        }
      }
    }
  }
}

/// The owning CT route authenticates receipts under the observed proof's source.
/// An unused or missing receipt aborts, because terminal callers negate admission.
pub fn leaf_admits_fault_v1(
  subject: substrate.VerdictSubjectV1,
  context: Option<NetworkOrderContextV1>,
  observed: FaultPositionV1,
  claimed: ClaimedFaultOrderV1,
) -> Bool {
  expect substrate.subject_is_canonical_v1(subject)
  when observed is {
    CanonicalFault(position) -> {
      expect context == None
      expect claimed == NotRequired
      leaf_admits_canonical_fault_v1(subject, position)
    }
    ResolutionFault(position) -> {
      expect Some(network_context) = context
      leaf_admits_resolution_fault_v1(
        subject,
        position,
        network_context,
        claimed,
      )
    }
    PurposeFault(position) -> {
      expect Some(network_context) = context
      leaf_admits_purpose_fault_v1(subject, network_context, position, claimed)
    }
  }
}

// ── Later faults: after the direct purposes ─────────────────────────────
//
// Each later position names a fault an owning route proved under the leaf's
// source. Its key is (phase, stage, coordinate, subcheck), the machine's
// traversal order:
//   - ScriptSources stage 11 audits inline witnesses by field-6 ordinal, then
//     stage 12 audits redeemers by field-8 ordinal; both follow every purpose;
//   - NativeScripts visits executions in purpose-cursor order, (kind, index);
//     one execution's token scan precedes its evaluation;
//   - ScriptIntegrity is transaction-global;
//   - Cek visits executions in purpose-cursor order; one execution's
//     selection precedes its run;
//   - ValueAndMint folds inputs in schedule order, then each output (its
//     minimum-Ada floor before its assets), then mint assets, then checks
//     preservation.
// A later position needs no claimed receipt: inside a phase the leaf's own
// coordinate orders it directly.

pub type UnusedAuditPositionV1 {
  UnusedWitnessAudit { script_index: Int }
  UnusedRedeemerAudit { redeemer_index: Int }
}

/// The token scan's three arms share one rank: which one trips first depends
/// on a byte position the coordinate does not carry.
pub type ExecutionSubcheckV1 {
  NativeScriptScan
  NativeScriptEvaluation
}

pub type ExecutionFaultPositionV1 {
  coordinate: PurposeCoordinateV1,
  subcheck: ExecutionSubcheckV1,
}

pub type CekSubcheckV1 {
  ReceiveLanguage
  ScriptRun
}

pub type CekFaultPositionV1 {
  coordinate: PurposeCoordinateV1,
  subcheck: CekSubcheckV1,
}

pub type ValueFaultPositionV1 {
  InputAssetFault { input_index: Int, asset_index: Int }
  OutputAssetFault { output_index: Int, asset_index: Int }
  MintAssetFault { mint_index: Int }
}

pub type LaterFaultPositionV1 {
  UnusedAuditFault(UnusedAuditPositionV1)
  ExecutionFault(ExecutionFaultPositionV1)
  ScriptIntegrityFault
  CekFault(CekFaultPositionV1)
  ValueFault(ValueFaultPositionV1)
}

pub fn later_fault_phase_v1(position: LaterFaultPositionV1) -> Int {
  when position is {
    UnusedAuditFault(_) -> 8
    ExecutionFault(_) -> 9
    ScriptIntegrityFault -> 10
    CekFault(_) -> 11
    ValueFault(_) -> 12
  }
}

pub fn unused_audit_position_of_reason_v1(
  r: reason.RejectionReasonV1,
) -> Option<UnusedAuditPositionV1> {
  when r is {
    reason.UnusedScriptWitness { script_index } ->
      Some(UnusedWitnessAudit { script_index })
    reason.UnusedRedeemer { redeemer_index } ->
      Some(UnusedRedeemerAudit { redeemer_index })
    _ -> None
  }
}

pub fn unused_audit_position_precedes_or_equals_v1(
  left: UnusedAuditPositionV1,
  right: UnusedAuditPositionV1,
) -> Bool {
  when Pair(left, right) is {
    Pair(UnusedWitnessAudit { script_index: l },
    UnusedWitnessAudit { script_index: r }) -> {
      expect l >= 0 && r >= 0
      l <= r
    }
    Pair(UnusedWitnessAudit { script_index: l },
    UnusedRedeemerAudit { redeemer_index: r }) -> {
      expect l >= 0 && r >= 0
      True
    }
    Pair(UnusedRedeemerAudit { redeemer_index: l },
    UnusedWitnessAudit { script_index: r }) -> {
      expect l >= 0 && r >= 0
      False
    }
    Pair(UnusedRedeemerAudit { redeemer_index: l },
    UnusedRedeemerAudit { redeemer_index: r }) -> {
      expect l >= 0 && r >= 0
      l <= r
    }
  }
}

pub fn execution_position_of_reason_v1(
  r: reason.RejectionReasonV1,
) -> Option<ExecutionFaultPositionV1> {
  when r is {
    reason.ExecutionNativeScriptMalformed { purpose_kind, purpose_index } |
    reason.ExecutionNativeScriptNodeLimit { purpose_kind, purpose_index } |
    reason.ExecutionNativeScriptDepthLimit { purpose_kind, purpose_index } ->
      Some(
        ExecutionFaultPositionV1 {
          coordinate: PurposeCoordinateV1 {
            kind: purpose_kind,
            index: purpose_index,
          },
          subcheck: NativeScriptScan,
        },
      )
    reason.ExecutionNativeScriptFalse { purpose_kind, purpose_index } ->
      Some(
        ExecutionFaultPositionV1 {
          coordinate: PurposeCoordinateV1 {
            kind: purpose_kind,
            index: purpose_index,
          },
          subcheck: NativeScriptEvaluation,
        },
      )
    _ -> None
  }
}

/// Lexicographic (kind, index); at one coordinate the caller's subcheck
/// verdict decides.
fn coordinate_precedes_or_equals_v1(
  left: PurposeCoordinateV1,
  right: PurposeCoordinateV1,
  subcheck_precedes_or_equals: Bool,
) -> Bool {
  expect purpose_coordinate_is_valid_v1(left)
  expect purpose_coordinate_is_valid_v1(right)
  if left.kind != right.kind {
    left.kind < right.kind
  } else if left.index != right.index {
    left.index < right.index
  } else {
    subcheck_precedes_or_equals
  }
}

pub fn execution_position_precedes_or_equals_v1(
  left: ExecutionFaultPositionV1,
  right: ExecutionFaultPositionV1,
) -> Bool {
  coordinate_precedes_or_equals_v1(
    left.coordinate,
    right.coordinate,
    left.subcheck == NativeScriptScan || right.subcheck == NativeScriptEvaluation,
  )
}

pub fn cek_position_of_reason_v1(
  r: reason.RejectionReasonV1,
) -> Option<CekFaultPositionV1> {
  when r is {
    reason.ReceivePurposePlutusV3Forbidden { purpose_kind, purpose_index } ->
      Some(
        CekFaultPositionV1 {
          coordinate: PurposeCoordinateV1 {
            kind: purpose_kind,
            index: purpose_index,
          },
          subcheck: ReceiveLanguage,
        },
      )
    reason.PlutusExecutionFailed { purpose_kind, purpose_index } ->
      Some(
        CekFaultPositionV1 {
          coordinate: PurposeCoordinateV1 {
            kind: purpose_kind,
            index: purpose_index,
          },
          subcheck: ScriptRun,
        },
      )
    _ -> None
  }
}

pub fn cek_position_precedes_or_equals_v1(
  left: CekFaultPositionV1,
  right: CekFaultPositionV1,
) -> Bool {
  coordinate_precedes_or_equals_v1(
    left.coordinate,
    right.coordinate,
    left.subcheck == ReceiveLanguage || right.subcheck == ScriptRun,
  )
}

/// (stage, major, minor) in ValueAndMint traversal order. Inside one output
/// the minimum-Ada floor (minor -1) precedes every asset (minor >= 0).
fn value_key_of_reason_v1(r: reason.RejectionReasonV1) -> (Int, Int, Int) {
  when r is {
    reason.InputAssetAccumulationLimit { input_index, asset_index } -> {
      expect input_index >= 0 && asset_index >= 0
      (0, input_index, asset_index)
    }
    reason.OutputBelowMinAda { output_index } -> {
      expect output_index >= 0
      (1, output_index, -1)
    }
    reason.OutputAssetAccumulationLimit { output_index, asset_index } -> {
      expect output_index >= 0 && asset_index >= 0
      (1, output_index, asset_index)
    }
    reason.MintAssetAccumulationLimit { mint_index } -> {
      expect mint_index >= 0
      (2, mint_index, 0)
    }
    reason.ValueNotPreserved -> (3, 0, 0)
    _ -> fail
  }
}

pub fn value_position_of_reason_v1(
  r: reason.RejectionReasonV1,
) -> Option<ValueFaultPositionV1> {
  when r is {
    reason.InputAssetAccumulationLimit { input_index, asset_index } ->
      Some(InputAssetFault { input_index, asset_index })
    reason.OutputAssetAccumulationLimit { output_index, asset_index } ->
      Some(OutputAssetFault { output_index, asset_index })
    reason.MintAssetAccumulationLimit { mint_index } ->
      Some(MintAssetFault { mint_index })
    _ -> None
  }
}

fn value_key_of_position_v1(position: ValueFaultPositionV1) -> (Int, Int, Int) {
  when position is {
    InputAssetFault { input_index, asset_index } -> {
      expect input_index >= 0 && asset_index >= 0
      (0, input_index, asset_index)
    }
    OutputAssetFault { output_index, asset_index } -> {
      expect output_index >= 0 && asset_index >= 0
      (1, output_index, asset_index)
    }
    MintAssetFault { mint_index } -> {
      expect mint_index >= 0
      (2, mint_index, 0)
    }
  }
}

/// `r` must be a ValueAndMint (phase 12) reason.
pub fn value_reason_precedes_or_equals_v1(
  r: reason.RejectionReasonV1,
  observed: ValueFaultPositionV1,
) -> Bool {
  let (ls, lm, ln) = value_key_of_reason_v1(r)
  let (rs, rm, rn) = value_key_of_position_v1(observed)
  if ls != rs {
    ls < rs
  } else if lm != rm {
    lm < rm
  } else {
    ln <= rn
  }
}

/// Whether `r` is at or before `observed` in the machine's traversal order.
/// A ScriptSources reason other than an unused audit belongs to an earlier
/// stage than either audit.
pub fn reason_precedes_or_equals_later_fault_v1(
  r: reason.RejectionReasonV1,
  context: NetworkOrderContextV1,
  observed: LaterFaultPositionV1,
) -> Bool {
  let phase = phase_of_reason_v1(r, context)
  let observed_phase = later_fault_phase_v1(observed)
  if phase != observed_phase {
    phase < observed_phase
  } else {
    when observed is {
      UnusedAuditFault(position) ->
        when unused_audit_position_of_reason_v1(r) is {
          Some(recorded) ->
            unused_audit_position_precedes_or_equals_v1(recorded, position)
          None -> True
        }
      ExecutionFault(position) -> {
        expect Some(recorded) = execution_position_of_reason_v1(r)
        execution_position_precedes_or_equals_v1(recorded, position)
      }
      ScriptIntegrityFault -> True
      CekFault(position) -> {
        expect Some(recorded) = cek_position_of_reason_v1(r)
        cek_position_precedes_or_equals_v1(recorded, position)
      }
      ValueFault(position) -> value_reason_precedes_or_equals_v1(r, position)
    }
  }
}

/// The later-fault twin of `leaf_admits_fault_v1`, with the same contract:
/// terminal callers negate it, so an accepted leaf admits nothing and a
/// malformed coordinate on either side aborts.
pub fn leaf_admits_later_fault_v1(
  subject: substrate.VerdictSubjectV1,
  context: NetworkOrderContextV1,
  observed: LaterFaultPositionV1,
) -> Bool {
  expect substrate.subject_is_canonical_v1(subject)
  when subject.rejection_reason is {
    None -> False
    Some(r) -> reason_precedes_or_equals_later_fault_v1(r, context, observed)
  }
}
