//// The reference-script-auth role table of the shared ledger-output-proof
//// (LOP) yield family. Both step dispatchers (ScriptSources output proof and
//// ResolveInputs membership) select a stage yield by index through
//// `stage_role`; both finalize dispatchers require all four `descriptor_role`
//// positions. The indices are the `expected_role` constants compiled into the
//// yield validators, so a name here and an index there are one fact.

use cardano/assets.{AssetName}

/// Stage yields, indexed by the role selector carried in the step dispatcher
/// redeemer. Out-of-range selectors abort: an unknown role can never be
/// authenticated, so it must not resolve to any name.
pub fn stage_role(index: Int) -> AssetName {
  when index is {
    0 -> "V1VtLopStructureYield"
    1 -> "V1VtLopValueYield"
    2 -> "V1VtLopDatumFoldMapYield"
    3 -> "V1VtLopDatumFinalizeFrameYield"
    4 -> "V1VtLopDatumHeadScalarYield"
    5 -> "V1VtLopDatumAttachIntegerYield"
    6 -> "V1VtLopDatumFoldListYield"
    7 -> "V1VtLopDatumAdvanceIntegerYield"
    8 -> "V1VtLopRefScriptYield"
    9 -> "V1VtLopScriptHashYield"
    10 -> "V1VtLopNativeScriptYield"
    11 -> "V1VtLopStructureAssetsYield"
    12 -> "V1VtLopStructureOptionalYield"
    13 -> "V1VtLopStructureFinishYield"
    14 -> "V1VtLopDatumHeadSequenceYield"
    15 -> "V1VtLopDatumHeadMapYield"
    16 -> "V1VtLopDatumHeadLargeCtorYield"
    17 -> "V1VtLopDatumAttachBytesYield"
    18 -> "V1VtLopDatumAdvanceBytesYield"
    19 -> "V1VtLopDatumFinishYield"
    20 -> "V1VtLopDatumLargeCtorYield"
    21 -> "V1VtLopDatumLargeFieldsYield"
    22 -> "V1VtLopDatumCloseYield"
    _ -> fail
  }
}

/// The four descriptor yields of the finalize dispatchers, in the fixed
/// order the redeemer's reference-input index list is read.
pub fn descriptor_role(index: Int) -> AssetName {
  when index is {
    0 -> "V1VtLopDescScanFactsYield"
    1 -> "V1VtLopDescRefScriptYield"
    2 -> "V1VtLopDescDatumSummaryYield"
    3 -> "V1VtLopDescValueSummaryYield"
    _ -> fail
  }
}

/// The descriptor roles in redeemer order, for the finalize dispatchers'
/// exact-length reference-input index list.
pub fn descriptor_roles() -> List<AssetName> {
  [
    descriptor_role(0),
    descriptor_role(1),
    descriptor_role(2),
    descriptor_role(3),
  ]
}

/// Attestation yields of the step path. The span yield verifies the chunk
/// merkle membership of the output span the dispatcher action claims; the two
/// scalar yields verify that the pinned control's active scalar sub-control
/// decodes to the claimed scalar tuple. A stage yield consumes those claims
/// without re-verifying them, so each stage role names the exact attestation
/// roles its transactions must also present.
pub const span_attestation_role: AssetName = "V1VtLopSpanYield"

pub const scalar_integer_attestation_role: AssetName =
  "V1VtLopScalarIntegerYield"

pub const scalar_bytes_attestation_role: AssetName = "V1VtLopScalarBytesYield"

/// The attestation roles a step transaction must present after its stage
/// yield, in the fixed order the redeemer's reference-input index list is
/// read: the span role first, then the scalar role of the stage's codec
/// family. Stages that read no authenticated span and no scalar sub-control
/// require none.
pub fn stage_attestation_roles(index: Int) -> List<AssetName> {
  if index == 4 || index == 8 || index == 9 || index >= 14 && index <= 16 {
    // datum head yields, reference-script commitment, script hash
    [span_attestation_role]
  } else if index == 5 {
    // attach integer
    [scalar_integer_attestation_role]
  } else if index == 17 {
    // attach bytes
    [scalar_bytes_attestation_role]
  } else if index == 7 {
    // advance integer
    [span_attestation_role, scalar_integer_attestation_role]
  } else if index == 18 {
    // advance bytes
    [span_attestation_role, scalar_bytes_attestation_role]
  } else if index >= 20 && index <= 22 {
    // datum large-constructor streaming, large-fields completion and close:
    // their source spans are claimed in content mode and verified by the span
    // yield; the traverse sub-control is re-validated locally, so no scalar
    // attestation applies.
    [span_attestation_role]
  } else {
    expect index >= 0 && index < stage_role_count
    []
  }
}

pub const stage_role_count: Int = 23

pub const descriptor_role_count: Int = 4

pub const attestation_role_count: Int = 3
