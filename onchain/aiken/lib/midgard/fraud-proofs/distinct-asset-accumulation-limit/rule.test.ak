use midgard/fraud_proofs/distinct_asset_accumulation_limit/rule
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/rejection_reason_v1.{
  InputAssetAccumulationLimit, MintAssetAccumulationLimit,
  OutputAssetAccumulationLimit, OutputNonCanonical,
}

const tx_id =
  #"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"

const root = #"1111111111111111111111111111111111111111111111111111111111111111"

fn accepted() -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: 1,
    direction: 0,
    source_kind: 0,
    transaction_id: tx_id,
    source_key: #"",
    rejection_reason: None,
  }
}

fn forced(reason) -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: 1,
    direction: 1,
    source_kind: 1,
    transaction_id: tx_id,
    source_key: #"01",
    rejection_reason: Some(reason),
  }
}

fn terminal(subject, coordinate, fault) -> rule.FoldStateV1 {
  rule.FoldStateV1 {
    bound: rule.bind_coordinate_v1(subject, root, 1, coordinate),
    control: None,
    stage: 3,
    decisive_fault_holds: Some(fault),
  }
}

test input_coordinate_binds_forced_reason() {
  rule.bind_coordinate_v1(
    forced(InputAssetAccumulationLimit { input_index: 2, asset_index: 7 }),
    root,
    1,
    rule.CoordinateV1 { fold: 0, primary_index: 2, asset_index: 7 },
  ).coordinate.asset_index == 7
}

test output_coordinate_binds_forced_reason() {
  rule.bind_coordinate_v1(
    forced(OutputAssetAccumulationLimit { output_index: 3, asset_index: 8 }),
    root,
    1,
    rule.CoordinateV1 { fold: 1, primary_index: 3, asset_index: 8 },
  ).coordinate.primary_index == 3
}

test mint_coordinate_binds_forced_reason() {
  rule.bind_coordinate_v1(
    forced(MintAssetAccumulationLimit { mint_index: 9 }),
    root,
    1,
    rule.CoordinateV1 { fold: 2, primary_index: 9, asset_index: 0 },
  ).coordinate.primary_index == 9
}

test accepted_first_crossing_convicts() {
  rule.terminal_contradiction_v1(
    terminal(
      accepted(),
      rule.CoordinateV1 { fold: 0, primary_index: 0, asset_index: 16384 },
      True,
    ),
  )
}

test forced_exact_boundary_convicts() {
  rule.terminal_contradiction_v1(
    terminal(
      forced(
        OutputAssetAccumulationLimit { output_index: 0, asset_index: 16383 },
      ),
      rule.CoordinateV1 { fold: 1, primary_index: 0, asset_index: 16383 },
      False,
    ),
  )
}

test accepted_boundary_refuses() fail {
  rule.terminal_contradiction_v1(
    terminal(
      accepted(),
      rule.CoordinateV1 { fold: 1, primary_index: 0, asset_index: 16383 },
      False,
    ),
  )
}

test forced_crossing_refuses() fail {
  rule.terminal_contradiction_v1(
    terminal(
      forced(MintAssetAccumulationLimit { mint_index: 16384 }),
      rule.CoordinateV1 { fold: 2, primary_index: 16384, asset_index: 0 },
      True,
    ),
  )
}

test reason_coordinate_mutation_refuses() fail {
  rule.bind_coordinate_v1(
    forced(InputAssetAccumulationLimit { input_index: 2, asset_index: 7 }),
    root,
    1,
    rule.CoordinateV1 { fold: 0, primary_index: 2, asset_index: 8 },
  ).coordinate.asset_index == 8
}

test reason_constructor_mutation_refuses() fail {
  rule.bind_coordinate_v1(
    forced(OutputNonCanonical { output_index: 2 }),
    root,
    1,
    rule.CoordinateV1 { fold: 1, primary_index: 2, asset_index: 0 },
  ).coordinate.primary_index == 2
}

test mint_nonzero_secondary_coordinate_refuses() fail {
  rule.bind_coordinate_v1(
    accepted(),
    root,
    1,
    rule.CoordinateV1 { fold: 2, primary_index: 0, asset_index: 1 },
  ).coordinate.asset_index == 1
}

test skips_only_non_target_folds() {
  let initialized = rule.FoldStateV1 {
    bound: rule.bind_coordinate_v1(
      accepted(),
      root,
      1,
      rule.CoordinateV1 { fold: 2, primary_index: 0, asset_index: 0 },
    ),
    control: None,
    stage: 0,
    decisive_fault_holds: None,
  }
  let inputs = rule.skip_fold_v1(initialized, 0)
  rule.skip_fold_v1(inputs, 1).stage == 2
}

test target_fold_cannot_be_skipped() fail {
  let initialized = rule.FoldStateV1 {
    bound: rule.bind_coordinate_v1(
      accepted(),
      root,
      1,
      rule.CoordinateV1 { fold: 0, primary_index: 0, asset_index: 0 },
    ),
    control: None,
    stage: 0,
    decisive_fault_holds: None,
  }
  rule.skip_fold_v1(initialized, 0).stage == 1
}
