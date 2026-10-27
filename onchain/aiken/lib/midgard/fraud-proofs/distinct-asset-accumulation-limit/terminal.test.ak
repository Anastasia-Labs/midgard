use midgard/fraud_proofs/distinct_asset_accumulation_limit/fixture.{
  ShapeV1, SubjectV1, derive, subject_tx_of,
}
use midgard/fraud_proofs/distinct_asset_accumulation_limit/fold.{TerminalV1}
use midgard/fraud_proofs/distinct_asset_accumulation_limit/terminal.{
  convicts_v1,
}
use midgard/fraud_proofs/first_fault_v1.{
  InputAssetFault, MintAssetFault, OutputAssetFault, ValueFaultPositionV1,
}
use midgard/rejection_reason_v1.{
  InputAssetAccumulationLimit, InputNotFound, MintAssetAccumulationLimit,
  OutputAssetAccumulationLimit, OutputBelowMinAda, RejectionReasonV1,
  ValueNotPreserved,
}

// Shapes whose derivation crosses in each fold, and one that does not.

const input_crossing =
  ShapeV1 { spends: [16_000, 385], references: [], outputs: [], mint: [] }

const output_crossing =
  ShapeV1 { spends: [16_382], references: [], outputs: [[1], [2]], mint: [] }

const mint_crossing =
  ShapeV1 { spends: [16_381], references: [], outputs: [], mint: [2, 2] }

const at_the_bound =
  ShapeV1 { spends: [16_382], references: [], outputs: [[1]], mint: [1] }

fn derived(
  shape: ShapeV1,
  source_kind: Int,
  rejection_reason: Option<RejectionReasonV1>,
) -> TerminalV1 {
  derive(subject_tx_of(shape, SubjectV1 { source_kind, rejection_reason }))
}

fn convicts(
  shape: ShapeV1,
  source_kind: Int,
  rejection_reason: Option<RejectionReasonV1>,
) -> Bool {
  convicts_v1(derived(shape, source_kind, rejection_reason))
}

fn forced(shape: ShapeV1, reason: RejectionReasonV1) -> Bool {
  convicts(shape, 1, Some(reason))
}

// ## Wrongful acceptance, normal source (A-N)

test accepted_normal_with_a_crossing_convicts() {
  and {
    convicts(input_crossing, 0, None),
    convicts(output_crossing, 0, None),
    convicts(mint_crossing, 0, None),
  }
}

test accepted_normal_at_the_bound_does_not_convict() {
  !convicts(at_the_bound, 0, None)
}

// ## Wrongful acceptance, forced source (A-F)

test accepted_forced_with_a_crossing_convicts() {
  and {
    convicts(input_crossing, 1, None),
    convicts(output_crossing, 1, None),
    convicts(mint_crossing, 1, None),
  }
}

test accepted_forced_at_the_bound_does_not_convict() {
  !convicts(at_the_bound, 1, None)
}

// ## Wrongful rejection naming the accumulation limit (R-F)

test rejection_at_the_derived_crossing_does_not_convict() {
  and {
    !forced(
      input_crossing,
      InputAssetAccumulationLimit { input_index: 1, asset_index: 384 },
    ),
    !forced(
      output_crossing,
      OutputAssetAccumulationLimit { output_index: 1, asset_index: 1 },
    ),
    !forced(mint_crossing, MintAssetAccumulationLimit { mint_index: 3 }),
  }
}

test rejection_without_a_crossing_convicts() {
  and {
    forced(
      at_the_bound,
      InputAssetAccumulationLimit { input_index: 0, asset_index: 16_384 },
    ),
    forced(
      at_the_bound,
      OutputAssetAccumulationLimit { output_index: 0, asset_index: 0 },
    ),
    forced(at_the_bound, MintAssetAccumulationLimit { mint_index: 0 }),
  }
}

test rejection_at_another_coordinate_convicts() {
  and {
    forced(
      input_crossing,
      InputAssetAccumulationLimit { input_index: 1, asset_index: 383 },
    ),
    forced(
      input_crossing,
      InputAssetAccumulationLimit { input_index: 0, asset_index: 384 },
    ),
    forced(
      output_crossing,
      OutputAssetAccumulationLimit { output_index: 1, asset_index: 0 },
    ),
    forced(mint_crossing, MintAssetAccumulationLimit { mint_index: 2 }),
    forced(
      mint_crossing,
      OutputAssetAccumulationLimit { output_index: 0, asset_index: 0 },
    ),
  }
}

test rejection_with_the_unset_input_coordinate_convicts() {
  forced(
    input_crossing,
    InputAssetAccumulationLimit { input_index: -1, asset_index: 384 },
  )
}

// ## Wrongful rejection for another reason (R-F, later reason)

test rejection_for_a_later_reason_over_a_crossing_convicts() {
  and {
    forced(input_crossing, ValueNotPreserved),
    forced(mint_crossing, ValueNotPreserved),
    forced(output_crossing, OutputBelowMinAda { output_index: 2 }),
  }
}

test rejection_for_a_later_reason_without_a_crossing_does_not_convict() {
  !forced(at_the_bound, ValueNotPreserved)
}

test rejection_for_a_reason_before_the_crossing_does_not_convict() {
  and {
    !forced(output_crossing, OutputBelowMinAda { output_index: 1 }),
    !forced(output_crossing, OutputBelowMinAda { output_index: 0 }),
    !forced(mint_crossing, OutputBelowMinAda { output_index: 0 }),
    !forced(input_crossing, InputNotFound { source_kind: 1, input_index: 0 }),
  }
}

// ## The terminal reads only the derived crossing

fn terminal_of(
  shape: ShapeV1,
  reason: RejectionReasonV1,
  crossing: Option<ValueFaultPositionV1>,
) -> Bool {
  convicts_v1(TerminalV1 { ..derived(shape, 1, Some(reason)), crossing })
}

test the_recorded_coordinate_must_equal_the_crossing() {
  let reason = MintAssetAccumulationLimit { mint_index: 3 }
  and {
    !terminal_of(mint_crossing, reason, Some(MintAssetFault { mint_index: 3 })),
    terminal_of(mint_crossing, reason, Some(MintAssetFault { mint_index: 4 })),
    terminal_of(
      mint_crossing,
      reason,
      Some(InputAssetFault { input_index: 0, asset_index: 0 }),
    ),
    terminal_of(
      mint_crossing,
      reason,
      Some(OutputAssetFault { output_index: 0, asset_index: 3 }),
    ),
    terminal_of(mint_crossing, reason, None),
  }
}
