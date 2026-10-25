use aiken/collection/list
use midgard/fraud_proofs/first_fault_v1 as order
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/rejection_reason_v1 as reason

const tx_id: ByteArray =
  #"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"

fn rejected(r: reason.RejectionReasonV1) -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: 1,
    direction: 1,
    source_kind: 1,
    transaction_id: tx_id,
    source_key: #"00",
    rejection_reason: Some(r),
  }
}

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

fn ctx() -> order.NetworkOrderContextV1 {
  order.NetworkOrderContextV1 { body_network_mismatch: False }
}

fn admits(
  r: reason.RejectionReasonV1,
  observed: order.LaterFaultPositionV1,
) -> Bool {
  order.leaf_admits_later_fault_v1(rejected(r), ctx(), observed)
}

fn witness(script_index: Int) -> order.LaterFaultPositionV1 {
  order.UnusedAuditFault(order.UnusedWitnessAudit { script_index })
}

fn redeemer(redeemer_index: Int) -> order.LaterFaultPositionV1 {
  order.UnusedAuditFault(order.UnusedRedeemerAudit { redeemer_index })
}

fn scan(kind: Int, index: Int) -> order.LaterFaultPositionV1 {
  order.ExecutionFault(
    order.ExecutionFaultPositionV1 {
      coordinate: order.PurposeCoordinateV1 { kind, index },
      subcheck: order.NativeScriptScan,
    },
  )
}

fn evaluation(kind: Int, index: Int) -> order.LaterFaultPositionV1 {
  order.ExecutionFault(
    order.ExecutionFaultPositionV1 {
      coordinate: order.PurposeCoordinateV1 { kind, index },
      subcheck: order.NativeScriptEvaluation,
    },
  )
}

fn receive(kind: Int, index: Int) -> order.LaterFaultPositionV1 {
  order.CekFault(
    order.CekFaultPositionV1 {
      coordinate: order.PurposeCoordinateV1 { kind, index },
      subcheck: order.ReceiveLanguage,
    },
  )
}

fn run(kind: Int, index: Int) -> order.LaterFaultPositionV1 {
  order.CekFault(
    order.CekFaultPositionV1 {
      coordinate: order.PurposeCoordinateV1 { kind, index },
      subcheck: order.ScriptRun,
    },
  )
}

fn input_asset(input_index: Int, asset_index: Int) -> order.LaterFaultPositionV1 {
  order.ValueFault(order.InputAssetFault { input_index, asset_index })
}

fn output_asset(
  output_index: Int,
  asset_index: Int,
) -> order.LaterFaultPositionV1 {
  order.ValueFault(order.OutputAssetFault { output_index, asset_index })
}

fn mint_asset(mint_index: Int) -> order.LaterFaultPositionV1 {
  order.ValueFault(order.MintAssetFault { mint_index })
}

// ── Phase ranks ─────────────────────────────────────────────────────────

test later_phases_follow_the_machine_order() {
  and {
    order.later_fault_phase_v1(witness(0)) == 8,
    order.later_fault_phase_v1(redeemer(0)) == 8,
    order.later_fault_phase_v1(scan(0, 0)) == 9,
    order.later_fault_phase_v1(evaluation(0, 0)) == 9,
    order.later_fault_phase_v1(order.ScriptIntegrityFault) == 10,
    order.later_fault_phase_v1(receive(3, 0)) == 11,
    order.later_fault_phase_v1(run(0, 0)) == 11,
    order.later_fault_phase_v1(input_asset(0, 0)) == 12,
    order.later_fault_phase_v1(output_asset(0, 0)) == 12,
    order.later_fault_phase_v1(mint_asset(0)) == 12,
  }
}

test earlier_phases_admit_every_later_fault() {
  let r = reason.FieldPreimageLengthMismatch { field_index: 0 }
  list.all(
    [
      witness(0),
      redeemer(0),
      scan(0, 0),
      evaluation(0, 0),
      order.ScriptIntegrityFault,
      receive(3, 0),
      run(0, 0),
      input_asset(0, 0),
      output_asset(0, 0),
      mint_asset(0),
    ],
    fn(observed) { admits(r, observed) },
  )
}

// ── Arm 33: UnusedScriptWitness (phase 8, stage 11) ─────────────────────

test unused_witness_follows_a_purpose_fault() {
  admits(
    reason.ScriptSourceMissing { purpose_kind: 3, purpose_index: 9 },
    witness(0),
  )
}

test unused_witness_follows_a_reopened_redeemer_fault() {
  admits(
    reason.RedeemerMissing { purpose_kind: 0, purpose_index: 0 },
    witness(0),
  )
}

test unused_witness_follows_a_pre_purpose_source_fault() {
  admits(reason.MintItemMalformed { policy_index: 0 }, witness(0))
}

test unused_witness_follows_the_output_network_check() {
  order.leaf_admits_later_fault_v1(
    rejected(reason.NetworkIdMismatch),
    ctx(),
    witness(0),
  )
}

test unused_witness_precedes_any_unused_redeemer() {
  !admits(reason.UnusedRedeemer { redeemer_index: 0 }, witness(7))
}

test unused_witness_precedes_native_scripts() {
  !admits(
    reason.ExecutionNativeScriptMalformed { purpose_kind: 0, purpose_index: 0 },
    witness(7),
  )
}

test unused_witness_precedes_value_and_mint() {
  !admits(reason.ValueNotPreserved, witness(7))
}

test unused_witness_earlier_ordinal_wins() {
  !admits(reason.UnusedScriptWitness { script_index: 3 }, witness(2))
}

test unused_witness_same_ordinal_is_admitted() {
  admits(reason.UnusedScriptWitness { script_index: 2 }, witness(2))
}

test unused_witness_later_ordinal_cannot_refute_earlier() {
  admits(reason.UnusedScriptWitness { script_index: 1 }, witness(2))
}

// ── Arm 34: UnusedRedeemer (phase 8, stage 12) ──────────────────────────

test unused_redeemer_follows_any_unused_witness() {
  admits(reason.UnusedScriptWitness { script_index: 9 }, redeemer(0))
}

test unused_redeemer_follows_a_purpose_fault() {
  admits(
    reason.RedeemerMissing { purpose_kind: 3, purpose_index: 9 },
    redeemer(0),
  )
}

test unused_redeemer_precedes_native_scripts() {
  !admits(
    reason.ExecutionNativeScriptFalse { purpose_kind: 0, purpose_index: 0 },
    redeemer(9),
  )
}

test unused_redeemer_precedes_cek() {
  !admits(
    reason.PlutusExecutionFailed { purpose_kind: 0, purpose_index: 0 },
    redeemer(9),
  )
}

test unused_redeemer_earlier_ordinal_wins() {
  !admits(reason.UnusedRedeemer { redeemer_index: 4 }, redeemer(3))
}

test unused_redeemer_same_ordinal_is_admitted() {
  admits(reason.UnusedRedeemer { redeemer_index: 3 }, redeemer(3))
}

// ── Arms 35-38: NativeScripts (phase 9) ─────────────────────────────────

test native_follows_the_unused_audits() {
  and {
    admits(reason.UnusedRedeemer { redeemer_index: 9 }, scan(0, 0)),
    admits(reason.UnusedScriptWitness { script_index: 9 }, evaluation(0, 0)),
  }
}

test native_precedes_script_integrity() {
  and {
    !admits(reason.ScriptIntegrityHashMismatch, scan(3, 9)),
    !admits(reason.ScriptIntegrityHashMismatch, evaluation(3, 9)),
  }
}

test native_scan_precedes_evaluation_at_one_execution() {
  !admits(
    reason.ExecutionNativeScriptFalse { purpose_kind: 1, purpose_index: 2 },
    scan(1, 2),
  )
}

test native_evaluation_follows_scan_at_one_execution() {
  admits(
    reason.ExecutionNativeScriptMalformed { purpose_kind: 1, purpose_index: 2 },
    evaluation(1, 2),
  )
}

test native_scan_arms_share_one_rank() {
  and {
    admits(
      reason.ExecutionNativeScriptMalformed {
        purpose_kind: 1,
        purpose_index: 2,
      },
      scan(1, 2),
    ),
    admits(
      reason.ExecutionNativeScriptNodeLimit {
        purpose_kind: 1,
        purpose_index: 2,
      },
      scan(1, 2),
    ),
    admits(
      reason.ExecutionNativeScriptDepthLimit {
        purpose_kind: 1,
        purpose_index: 2,
      },
      scan(1, 2),
    ),
  }
}

test native_same_evaluation_is_admitted() {
  admits(
    reason.ExecutionNativeScriptFalse { purpose_kind: 1, purpose_index: 2 },
    evaluation(1, 2),
  )
}

test native_earlier_index_wins() {
  !admits(
    reason.ExecutionNativeScriptNodeLimit { purpose_kind: 1, purpose_index: 3 },
    evaluation(1, 2),
  )
}

test native_later_index_cannot_refute_earlier() {
  admits(
    reason.ExecutionNativeScriptFalse { purpose_kind: 1, purpose_index: 1 },
    scan(1, 2),
  )
}

test native_kind_precedes_index() {
  and {
    !admits(
      reason.ExecutionNativeScriptDepthLimit {
        purpose_kind: 1,
        purpose_index: 0,
      },
      scan(0, 9),
    ),
    admits(
      reason.ExecutionNativeScriptDepthLimit {
        purpose_kind: 0,
        purpose_index: 9,
      },
      scan(1, 0),
    ),
  }
}

// ── Arm 39: ScriptIntegrityHashMismatch (phase 10) ──────────────────────

test integrity_follows_native_scripts() {
  admits(
    reason.ExecutionNativeScriptFalse { purpose_kind: 3, purpose_index: 9 },
    order.ScriptIntegrityFault,
  )
}

test integrity_same_fault_is_admitted() {
  admits(reason.ScriptIntegrityHashMismatch, order.ScriptIntegrityFault)
}

test integrity_precedes_cek() {
  and {
    !admits(
      reason.ReceivePurposePlutusV3Forbidden {
        purpose_kind: 3,
        purpose_index: 0,
      },
      order.ScriptIntegrityFault,
    ),
    !admits(
      reason.PlutusExecutionFailed { purpose_kind: 0, purpose_index: 0 },
      order.ScriptIntegrityFault,
    ),
  }
}

test integrity_precedes_value_and_mint() {
  !admits(
    reason.InputAssetAccumulationLimit { input_index: 0, asset_index: 0 },
    order.ScriptIntegrityFault,
  )
}

// ── Arms 40-41: Cek (phase 11) ──────────────────────────────────────────

test cek_follows_script_integrity() {
  and {
    admits(reason.ScriptIntegrityHashMismatch, receive(3, 0)),
    admits(reason.ScriptIntegrityHashMismatch, run(0, 0)),
  }
}

test cek_precedes_value_and_mint() {
  and {
    !admits(reason.MintAssetAccumulationLimit { mint_index: 0 }, receive(3, 9)),
    !admits(reason.OutputBelowMinAda { output_index: 0 }, run(3, 9)),
  }
}

test cek_receive_language_precedes_run_at_one_execution() {
  !admits(
    reason.PlutusExecutionFailed { purpose_kind: 3, purpose_index: 1 },
    receive(3, 1),
  )
}

test cek_run_follows_receive_language_at_one_execution() {
  admits(
    reason.ReceivePurposePlutusV3Forbidden { purpose_kind: 3, purpose_index: 1 },
    run(3, 1),
  )
}

test cek_same_fault_is_admitted() {
  and {
    admits(
      reason.ReceivePurposePlutusV3Forbidden {
        purpose_kind: 3,
        purpose_index: 1,
      },
      receive(3, 1),
    ),
    admits(
      reason.PlutusExecutionFailed { purpose_kind: 0, purpose_index: 4 },
      run(0, 4),
    ),
  }
}

test cek_earlier_execution_wins() {
  and {
    !admits(
      reason.PlutusExecutionFailed { purpose_kind: 3, purpose_index: 2 },
      receive(3, 1),
    ),
    !admits(
      reason.ReceivePurposePlutusV3Forbidden {
        purpose_kind: 3,
        purpose_index: 2,
      },
      run(3, 1),
    ),
    admits(
      reason.PlutusExecutionFailed { purpose_kind: 3, purpose_index: 0 },
      receive(3, 1),
    ),
  }
}

test cek_kind_precedes_index() {
  and {
    admits(
      reason.PlutusExecutionFailed { purpose_kind: 0, purpose_index: 5 },
      receive(3, 1),
    ),
    !admits(
      reason.PlutusExecutionFailed { purpose_kind: 2, purpose_index: 0 },
      run(1, 7),
    ),
  }
}

// ── Arms 42-44: ValueAndMint (phase 12) ─────────────────────────────────

test value_follows_cek() {
  and {
    admits(
      reason.PlutusExecutionFailed { purpose_kind: 3, purpose_index: 9 },
      input_asset(0, 0),
    ),
    admits(
      reason.ReceivePurposePlutusV3Forbidden {
        purpose_kind: 3,
        purpose_index: 9,
      },
      mint_asset(0),
    ),
  }
}

test input_asset_same_fault_is_admitted() {
  admits(
    reason.InputAssetAccumulationLimit { input_index: 1, asset_index: 4 },
    input_asset(1, 4),
  )
}

test input_asset_earlier_asset_wins() {
  !admits(
    reason.InputAssetAccumulationLimit { input_index: 1, asset_index: 5 },
    input_asset(1, 4),
  )
}

test input_asset_earlier_input_wins() {
  and {
    !admits(
      reason.InputAssetAccumulationLimit { input_index: 2, asset_index: 0 },
      input_asset(1, 4),
    ),
    admits(
      reason.InputAssetAccumulationLimit { input_index: 0, asset_index: 9 },
      input_asset(1, 4),
    ),
  }
}

test input_asset_precedes_outputs_mint_and_preservation() {
  and {
    !admits(reason.OutputBelowMinAda { output_index: 0 }, input_asset(9, 9)),
    !admits(
      reason.OutputAssetAccumulationLimit { output_index: 0, asset_index: 0 },
      input_asset(9, 9),
    ),
    !admits(
      reason.MintAssetAccumulationLimit { mint_index: 0 },
      input_asset(9, 9),
    ),
    !admits(reason.ValueNotPreserved, input_asset(9, 9)),
  }
}

test output_asset_follows_every_input() {
  admits(
    reason.InputAssetAccumulationLimit { input_index: 9, asset_index: 9 },
    output_asset(2, 3),
  )
}

test output_min_ada_precedes_that_outputs_assets() {
  admits(reason.OutputBelowMinAda { output_index: 2 }, output_asset(2, 0))
}

test output_asset_precedes_later_output_min_ada() {
  !admits(reason.OutputBelowMinAda { output_index: 3 }, output_asset(2, 3))
}

test output_asset_earlier_asset_wins() {
  !admits(
    reason.OutputAssetAccumulationLimit { output_index: 2, asset_index: 4 },
    output_asset(2, 3),
  )
}

test output_asset_earlier_output_wins() {
  and {
    admits(
      reason.OutputAssetAccumulationLimit { output_index: 1, asset_index: 9 },
      output_asset(2, 3),
    ),
    admits(
      reason.OutputAssetAccumulationLimit { output_index: 2, asset_index: 3 },
      output_asset(2, 3),
    ),
  }
}

test output_asset_precedes_mint_and_preservation() {
  and {
    !admits(
      reason.MintAssetAccumulationLimit { mint_index: 0 },
      output_asset(9, 9),
    ),
    !admits(reason.ValueNotPreserved, output_asset(9, 9)),
  }
}

test mint_asset_follows_every_output() {
  and {
    admits(
      reason.OutputAssetAccumulationLimit { output_index: 99, asset_index: 99 },
      mint_asset(4),
    ),
    admits(reason.OutputBelowMinAda { output_index: 99 }, mint_asset(4)),
  }
}

test mint_asset_earlier_index_wins() {
  and {
    !admits(reason.MintAssetAccumulationLimit { mint_index: 5 }, mint_asset(4)),
    admits(reason.MintAssetAccumulationLimit { mint_index: 4 }, mint_asset(4)),
    admits(reason.MintAssetAccumulationLimit { mint_index: 3 }, mint_asset(4)),
  }
}

test mint_asset_precedes_preservation() {
  !admits(reason.ValueNotPreserved, mint_asset(4))
}

// ── Contract: negate-to-convict, missing or malformed evidence aborts ───

test accepted_leaf_admits_no_later_fault() {
  list.all(
    [
      witness(0),
      redeemer(0),
      scan(0, 0),
      evaluation(0, 0),
      order.ScriptIntegrityFault,
      receive(3, 0),
      run(0, 0),
      input_asset(0, 0),
      output_asset(0, 0),
      mint_asset(0),
    ],
    fn(observed) {
      !order.leaf_admits_later_fault_v1(accepted(), ctx(), observed)
    },
  )
}

test non_canonical_subject_refuses() fail {
  let subject = rejected(reason.ValueNotPreserved)
  order.leaf_admits_later_fault_v1(
    substrate.VerdictSubjectV1 { ..subject, transaction_id: #"00" },
    ctx(),
    mint_asset(0),
  )
}

test negative_observed_witness_refuses() fail {
  admits(reason.UnusedScriptWitness { script_index: 0 }, witness(-1))
}

test negative_recorded_redeemer_refuses() fail {
  admits(reason.UnusedRedeemer { redeemer_index: -1 }, redeemer(0))
}

test negative_recorded_redeemer_refuses_against_witness() fail {
  admits(reason.UnusedRedeemer { redeemer_index: -1 }, witness(0))
}

test unknown_observed_execution_kind_refuses() fail {
  admits(
    reason.ExecutionNativeScriptFalse { purpose_kind: 0, purpose_index: 0 },
    scan(4, 0),
  )
}

test negative_recorded_execution_index_refuses() fail {
  admits(
    reason.ExecutionNativeScriptFalse { purpose_kind: 0, purpose_index: -1 },
    scan(0, 0),
  )
}

test negative_observed_cek_index_refuses() fail {
  admits(
    reason.PlutusExecutionFailed { purpose_kind: 0, purpose_index: 0 },
    run(0, -1),
  )
}

test negative_observed_asset_refuses() fail {
  admits(
    reason.InputAssetAccumulationLimit { input_index: 0, asset_index: 0 },
    input_asset(0, -1),
  )
}

test negative_recorded_min_ada_output_refuses() fail {
  admits(reason.OutputBelowMinAda { output_index: -1 }, output_asset(0, 0))
}

test negative_observed_mint_refuses() fail {
  admits(reason.ValueNotPreserved, mint_asset(-1))
}

// ── Twin table ──────────────────────────────────────────────────────────
// The same rows are pinned off-chain in
// demo/midgard-sdk/tests/first-fault-later-phases-twin.test.ts;
// change both together.

test twin_table() {
  let rows: List<(reason.RejectionReasonV1, order.LaterFaultPositionV1, Bool)> = [
    (
      reason.RedeemerMissing { purpose_kind: 3, purpose_index: 9 },
      witness(0),
      True,
    ),
    (reason.UnusedScriptWitness { script_index: 3 }, witness(2), False),
    (reason.UnusedScriptWitness { script_index: 2 }, witness(2), True),
    (reason.UnusedRedeemer { redeemer_index: 0 }, witness(7), False),
    (reason.UnusedScriptWitness { script_index: 9 }, redeemer(0), True),
    (reason.UnusedRedeemer { redeemer_index: 4 }, redeemer(3), False),
    (
      reason.ExecutionNativeScriptMalformed {
        purpose_kind: 0,
        purpose_index: 0,
      },
      redeemer(9),
      False,
    ),
    (reason.UnusedRedeemer { redeemer_index: 9 }, scan(0, 0), True),
    (
      reason.ExecutionNativeScriptFalse { purpose_kind: 1, purpose_index: 2 },
      scan(1, 2),
      False,
    ),
    (
      reason.ExecutionNativeScriptNodeLimit {
        purpose_kind: 1,
        purpose_index: 2,
      },
      scan(1, 2),
      True,
    ),
    (
      reason.ExecutionNativeScriptDepthLimit {
        purpose_kind: 1,
        purpose_index: 0,
      },
      scan(0, 9),
      False,
    ),
    (
      reason.ExecutionNativeScriptMalformed {
        purpose_kind: 1,
        purpose_index: 2,
      },
      evaluation(1, 2),
      True,
    ),
    (
      reason.ExecutionNativeScriptNodeLimit {
        purpose_kind: 1,
        purpose_index: 3,
      },
      evaluation(1, 2),
      False,
    ),
    (reason.ScriptIntegrityHashMismatch, evaluation(3, 9), False),
    (
      reason.ExecutionNativeScriptFalse { purpose_kind: 3, purpose_index: 9 },
      order.ScriptIntegrityFault,
      True,
    ),
    (reason.ScriptIntegrityHashMismatch, order.ScriptIntegrityFault, True),
    (
      reason.PlutusExecutionFailed { purpose_kind: 0, purpose_index: 0 },
      order.ScriptIntegrityFault,
      False,
    ),
    (reason.ScriptIntegrityHashMismatch, receive(3, 0), True),
    (
      reason.PlutusExecutionFailed { purpose_kind: 3, purpose_index: 1 },
      receive(3, 1),
      False,
    ),
    (
      reason.ReceivePurposePlutusV3Forbidden {
        purpose_kind: 3,
        purpose_index: 1,
      },
      run(3, 1),
      True,
    ),
    (
      reason.PlutusExecutionFailed { purpose_kind: 0, purpose_index: 5 },
      receive(3, 1),
      True,
    ),
    (
      reason.ReceivePurposePlutusV3Forbidden {
        purpose_kind: 3,
        purpose_index: 2,
      },
      run(3, 1),
      False,
    ),
    (
      reason.PlutusExecutionFailed { purpose_kind: 3, purpose_index: 0 },
      receive(3, 1),
      True,
    ),
    (reason.OutputBelowMinAda { output_index: 0 }, run(3, 9), False),
    (
      reason.PlutusExecutionFailed { purpose_kind: 3, purpose_index: 9 },
      input_asset(0, 0),
      True,
    ),
    (
      reason.InputAssetAccumulationLimit { input_index: 1, asset_index: 5 },
      input_asset(1, 4),
      False,
    ),
    (
      reason.InputAssetAccumulationLimit { input_index: 0, asset_index: 9 },
      input_asset(1, 4),
      True,
    ),
    (reason.OutputBelowMinAda { output_index: 0 }, input_asset(9, 9), False),
    (reason.OutputBelowMinAda { output_index: 2 }, output_asset(2, 0), True),
    (reason.OutputBelowMinAda { output_index: 3 }, output_asset(2, 3), False),
    (
      reason.OutputAssetAccumulationLimit { output_index: 2, asset_index: 4 },
      output_asset(2, 3),
      False,
    ),
    (
      reason.MintAssetAccumulationLimit { mint_index: 0 },
      output_asset(9, 9),
      False,
    ),
    (reason.OutputBelowMinAda { output_index: 99 }, mint_asset(4), True),
    (reason.MintAssetAccumulationLimit { mint_index: 5 }, mint_asset(4), False),
    (reason.MintAssetAccumulationLimit { mint_index: 4 }, mint_asset(4), True),
    (reason.ValueNotPreserved, mint_asset(4), False),
  ]
  list.all(rows, fn(row) { admits(row.1st, row.2nd) == row.3rd })
}
