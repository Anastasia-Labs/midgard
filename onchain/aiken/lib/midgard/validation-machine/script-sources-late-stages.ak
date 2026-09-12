use aiken/builtin
use aiken/cbor
use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/codec.{encode_definite_bytes}
use midgard/fraud_proofs/native_tx/compact.{
  decode_tx_compact_view_v1, tx_proof_commitment_v1, verify_tx_proof_source_v1,
}
use midgard/fraud_proofs/native_tx/types.{
  NativeTxWitnessSetCompact, VerifiedTxSourceView,
}
use midgard/ledger_output_v1
use midgard/native_tx_field_access_v1
use midgard/redeemer_item_proof_v1
use midgard/rejection_reason_v1.{
  reject_invalid_field_type, reject_missing_required_witness,
}
use midgard/script_proof_v1
use midgard/script_sources_descriptor as descriptor_claim
use midgard/validation_machine/machine_types.{
  NoAuxiliaryWitness, ObserverPurposeScanControlV1, ReceivePurposeScanControlV1,
  RedeemerItemStepWitness, RedeemerScanBeginWitness, ScriptDiscoveryControlV1,
  ScriptPurposeScanWitness, ScriptSourceScanWitness, ScriptSourcesControlV1,
  TransactionFieldChunkWitness, ValidationAuxiliaryWitnessV1,
  ValidationOneStepWitnessV1,
}
use midgard/validation_machine/script_sources_early_stages.{
  script_sources_control_successor_is_exact,
  script_sources_stage_zero_prefix_before_stage,
  script_sources_stage_zero_successor_work_is_exact,
}
use midgard/validation_machine/shared.{
  append_script_execution, current_script_purpose_leaf,
  max_tx_size_derived_collection_item_count, redeemer_pointer_matches_purpose_v1,
  rejected_successor_is_exact, script_discovery_bit, script_discovery_bitmap_has,
  script_discovery_bitmap_insert,
}
use midgard/validation_machine/witness.{
  empty_observer_purpose_scan_control, empty_receive_purpose_scan_control,
  empty_script_discovery_control, encode_native_scripts_witness_v1,
  encode_script_discovery_control, encode_script_sources_discovery_witness,
  encode_script_sources_witness, mint_fold_active_state_is_empty,
  reset_script_discovery_current,
  script_sources_redeemer_scan_control_from_witness,
  script_sources_stage_eight_control_from_witness,
  script_sources_stage_eleven_control_from_witness,
  script_sources_stage_nine_control_from_witness,
  script_sources_stage_seven_control_from_witness,
  script_sources_stage_ten_control_from_witness,
  script_sources_stage_twelve_control_from_witness,
}
use midgard/validation_machine_field_door_v1 as field_door
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

fn next_receive_candidate(
  scan: ReceivePurposeScanControlV1,
  script_hash: ByteArray,
) -> ByteArray {
  if and {
    or {
      scan.previous_hash == #"",
      bytearray.compare(scan.previous_hash, script_hash) == Less,
    },
    or {
      scan.candidate_hash == #"",
      bytearray.compare(script_hash, scan.candidate_hash) == Less,
    },
  } {
    script_hash
  } else {
    scan.candidate_hash
  }
}

fn script_sources_receive_scan_item(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  expect
      ScriptPurposeScanWitness {
        purpose_kind,
        purpose_index,
        script_hash,
        subject,
        siblings,
      }
    = auxiliary
  and {
    purpose_kind == 3,
    purpose_index == control.output_cursor,
    bytearray.length(script_hash) == 28,
    subject == script_hash,
    validation_merkle_v1.verify_membership(
      control.receive_scan.source_count,
      control.receive_scan.source_peaks,
      purpose_index,
      script_proof_v1.purpose_leaf_hash(
        purpose_kind,
        purpose_index,
        script_hash,
        subject,
      ),
      siblings,
    ),
    script_sources_control_successor_is_exact(
      pre,
      witness,
      ScriptSourcesControlV1 {
        ..control,
        output_cursor: control.output_cursor + 1,
        receive_scan: ReceivePurposeScanControlV1 {
          ..control.receive_scan,
          candidate_hash: next_receive_candidate(
            control.receive_scan,
            script_hash,
          ),
        },
      },
    ),
  }
}

fn script_sources_receive_scan_finish(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  and {
    auxiliary == NoAuxiliaryWitness,
    if control.receive_scan.candidate_hash == #"" {
      script_sources_control_successor_is_exact(
        pre,
        witness,
        ScriptSourcesControlV1 {
          ..control,
          stage: 8,
          output_cursor: control.output_count,
          receive_scan: ReceivePurposeScanControlV1 {
            ..empty_receive_purpose_scan_control(),
            descriptor_peaks: control.receive_scan.descriptor_peaks,
          },
          observer_scan: empty_observer_purpose_scan_control(),
          discovery: empty_script_discovery_control(),
        },
      )
    } else {
      let script_hash = control.receive_scan.candidate_hash
      script_sources_control_successor_is_exact(
        pre,
        witness,
        ScriptSourcesControlV1 {
          ..control,
          output_cursor: 0,
          purpose_count: control.purpose_count + 1,
          purpose_peaks: validation_merkle_v1.append_leaf(
            control.purpose_count,
            control.purpose_peaks,
            script_proof_v1.purpose_leaf_hash(
              3,
              control.receive_scan.receive_count,
              script_hash,
              script_hash,
            ),
          ),
          receive_scan: ReceivePurposeScanControlV1 {
            source_count: control.receive_scan.source_count,
            source_peaks: control.receive_scan.source_peaks,
            receive_count: control.receive_scan.receive_count + 1,
            previous_hash: script_hash,
            candidate_hash: #"",
            descriptor_peaks: control.receive_scan.descriptor_peaks,
          },
        },
      )
    },
  }
}

pub fn script_sources_stage_seven_with_observer_commitment(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ScriptSourcesControlV1,
  observer_commitment: ByteArray,
  verified: VerifiedTxSourceView,
  witness_set: NativeTxWitnessSetCompact,
  door: field_door.MachineFieldDoorV1,
) -> Bool {
  let empty_observer_commitment = native_tx_field_access_v1.empty_field_commitment
  let observer_scan_is_complete = if observer_commitment == empty_observer_commitment {
    control.observer_scan == empty_observer_purpose_scan_control()
  } else {
    and {
      control.observer_scan.total_count > 0,
      control.observer_scan.seen == control.observer_scan.total_count,
    }
  }
  if observer_scan_is_complete {
    if control.output_cursor == control.receive_scan.source_count {
      script_sources_receive_scan_finish(pre, witness, auxiliary, control)
    } else {
      script_sources_receive_scan_item(pre, witness, auxiliary, control)
    }
  } else {
    expect TransactionFieldChunkWitness { field_index, item_index, carriage } = auxiliary
    let item = field_door.open_machine_field_item(
      door,
      verified,
      witness_set,
      3,
      control.observer_scan.seen,
      carriage,
    )
    let item_count = field_door.machine_field_item_count(item)
    let active_count = if control.observer_scan.total_count == 0 {
      item_count
    } else {
      control.observer_scan.total_count
    }
    let observer_hash = field_door.machine_field_item_bytes(item)
    and {
      control.output_cursor == 0,
      control.receive_scan.receive_count == 0,
      control.receive_scan.previous_hash == #"",
      control.receive_scan.candidate_hash == #"",
      active_count > 0,
      active_count <= max_tx_size_derived_collection_item_count,
      field_index == 3,
      item_index == control.observer_scan.seen,
      item_count == active_count,
      field_door.machine_field_item_length(item) == 28,
      if and {
        control.observer_scan.seen > 0,
        bytearray.compare(control.observer_scan.previous_hash, observer_hash) != Less,
      } {
        rejected_successor_is_exact(
          pre,
          witness.claimed_successor,
          reject_invalid_field_type,
        )
      } else {
        script_sources_control_successor_is_exact(
          pre,
          witness,
          ScriptSourcesControlV1 {
            ..control,
            purpose_count: control.purpose_count + 1,
            purpose_peaks: validation_merkle_v1.append_leaf(
              control.purpose_count,
              control.purpose_peaks,
              script_proof_v1.purpose_leaf_hash(
                2,
                control.observer_scan.seen,
                observer_hash,
                observer_hash,
              ),
            ),
            observer_scan: ObserverPurposeScanControlV1 {
              total_count: active_count,
              seen: control.observer_scan.seen + 1,
              previous_hash: observer_hash,
            },
          },
        )
      },
    }
  }
}

pub fn script_sources_stage_seven(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ScriptSourcesControlV1,
  door: field_door.MachineFieldDoorV1,
) -> Bool {
  let Pair(verified_source, witness_set) = verify_tx_proof_source_v1(
    pre.source_kind,
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  script_sources_stage_seven_with_observer_commitment(
    pre,
    witness,
    auxiliary,
    control,
    verified_source.tx_compact.body.required_observers_hash,
    verified_source,
    witness_set,
    door,
  )
}

// Stage seven can produce only base or discovery controls. It never reaches
// the unrelated pending source, item traversal or output-proof codecs.
fn script_sources_stage_seven_exact_control(
  control: ScriptSourcesControlV1,
) -> ByteArray {
  expect control.pending_source_cbor == #""
  if control.stage < 8 {
    expect control.output_proof == None
    encode_script_sources_witness(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
      control.context_cbor,
      control.resolved_input_count,
      control.resolved_inputs_accumulator,
      control.signer_count,
      control.signer_frontier_commitment,
      control.resolved_item_peaks,
      control.stage,
      control.source_count,
      control.source_peaks,
      control.redeemer_count,
      control.redeemer_peaks,
      control.replay_cursor,
      control.replay_accumulator,
      control.replay_remaining_schedule_hash,
      control.spend_index,
      control.purpose_count,
      control.purpose_peaks,
      control.output_cursor,
      control.output_count,
      control.output_peaks,
      control.output_total_count,
      control.receive_scan,
      control.source_total_count,
      control.redeemer_total_count,
      control.observer_scan,
      control.mint_fold,
      control.resolution_schedule_hash,
    )
  } else {
    expect control.output_proof == None
    encode_script_sources_discovery_witness(
      control,
      control.stage,
      control.discovery,
    )
  }
}

fn script_sources_stage_seven_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  script_sources_stage_zero_successor_work_is_exact(
    pre,
    witness,
    script_sources_stage_seven_exact_control(control),
  )
}

// Prepared evidence authenticates the canonical predecessor. Every discovery
// transition copies its middle and re-encodes only this prefix/stage/suffix.
fn script_sources_discovery_frame_is_exact(
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  let prefix = script_sources_stage_zero_prefix_before_stage(control, 31)
  let stage_offset = bytearray.length(prefix)
  let encoded_discovery = encode_definite_bytes(
    encode_script_discovery_control(control.discovery),
  )
  let discovery_offset = bytearray.length(witness.work_witness_cbor) - bytearray.length(
    encoded_discovery,
  )
  and {
    control.pending_source_cbor == #"",
    control.output_proof == None,
    discovery_offset >= stage_offset + 1,
    builtin.slice_bytearray(0, stage_offset, witness.work_witness_cbor) == prefix,
    builtin.slice_bytearray(stage_offset, 1, witness.work_witness_cbor) == cbor.serialise(
      control.stage,
    ),
    builtin.slice_bytearray(
      discovery_offset,
      bytearray.length(encoded_discovery),
      witness.work_witness_cbor,
    ) == encoded_discovery,
  }
}

fn script_sources_stage_seven_control_is_bound(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  let receive_scan = control.receive_scan
  let observer_scan = control.observer_scan
  let mint_fold = control.mint_fold
  and {
    script_sources_stage_seven_exact_control(control) == witness.work_witness_cbor,
    control.stage == 7,
    tx_proof_commitment_v1(
      pre.source_kind,
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
    ) == pre.transaction_commitment,
    validation_trace_v1.hash_validation_context(control.context_cbor) == pre.validation_context_hash,
    control.source_total_count == control.source_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.source_count,
      control.source_peaks,
    ),
    control.redeemer_total_count == control.redeemer_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.redeemer_count,
      control.redeemer_peaks,
    ),
    validation_merkle_v1.frontier_is_well_formed(
      control.purpose_count,
      control.purpose_peaks,
    ),
    control.output_total_count == control.output_count,
    control.output_count <= max_tx_size_derived_collection_item_count,
    control.output_cursor >= 0,
    control.output_cursor <= receive_scan.source_count,
    receive_scan.source_count >= 0,
    receive_scan.source_count <= control.output_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.output_count,
      control.output_peaks,
    ),
    validation_merkle_v1.frontier_is_well_formed(
      receive_scan.source_count,
      receive_scan.source_peaks,
    ),
    validation_merkle_v1.frontier_is_well_formed(
      control.output_count,
      receive_scan.descriptor_peaks,
    ),
    receive_scan.receive_count >= 0,
    receive_scan.receive_count <= receive_scan.source_count,
    if receive_scan.receive_count == 0 {
      receive_scan.previous_hash == #""
    } else {
      bytearray.length(receive_scan.previous_hash) == 28
    },
    or {
      receive_scan.candidate_hash == #"",
      and {
        bytearray.length(receive_scan.candidate_hash) == 28,
        or {
          receive_scan.previous_hash == #"",
          bytearray.compare(
            receive_scan.previous_hash,
            receive_scan.candidate_hash,
          ) == Less,
        },
      },
    },
    observer_scan.total_count >= 0,
    observer_scan.total_count <= max_tx_size_derived_collection_item_count,
    observer_scan.seen >= 0,
    observer_scan.seen <= observer_scan.total_count,
    if observer_scan.seen == 0 {
      observer_scan.previous_hash == #""
    } else {
      bytearray.length(observer_scan.previous_hash) == 28
    },
    mint_fold.policy_count >= 0,
    mint_fold.policy_count <= max_tx_size_derived_collection_item_count,
    mint_fold.policy_cursor == mint_fold.policy_count,
    mint_fold_active_state_is_empty(mint_fold),
    mint_fold.asset_count >= 0,
    mint_fold.asset_count <= ledger_output_v1.max_distinct_asset_count,
    validation_merkle_v1.frontier_is_well_formed(
      mint_fold.asset_count,
      mint_fold.asset_peaks,
    ),
    if mint_fold.policy_cursor == 0 {
      mint_fold.previous_policy == #""
    } else {
      bytearray.length(mint_fold.previous_policy) == 28
    },
  }
}

fn script_sources_stage_seven_observer_scan_is_complete(
  control: ScriptSourcesControlV1,
  observer_commitment: ByteArray,
) -> Bool {
  if observer_commitment == native_tx_field_access_v1.empty_field_commitment {
    control.observer_scan == empty_observer_purpose_scan_control()
  } else {
    and {
      control.observer_scan.total_count > 0,
      control.observer_scan.seen == control.observer_scan.total_count,
    }
  }
}

// The stage-7 successor checks below must commit to the *canonical* successor
// witness encoding produced by `exact_script_sources_control` (definite-length
// headers, `encode_definite_bytes` for every bytestring). They previously
// spliced replacement items into the decoded `Data` list and re-serialised with
// `builtin.list_data |> cbor.serialise`, i.e. Plutus `serialiseData`, which
// emits an indefinite `9f..ff` array and chunks bytestrings over 64 bytes as
// `5f..ff`. That form is rejected by every other check of the same rule -- by
// the binding the next step applies (`script_sources_stage_seven_control_is_bound`
// / `script_sources_control_is_bound`), by the stage-0 shape gate, and by the
// evidence path's `script_sources_control_successor_is_exact` -- so no stage-7
// step committed through the deployed wrappers could ever be continued.
// Build the successor control and reuse the single canonical encoder instead.
fn script_sources_stage_seven_observer_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
  observer_hash: ByteArray,
  active_count: Int,
) -> Bool {
  script_sources_stage_seven_successor_is_exact(
    pre,
    witness,
    ScriptSourcesControlV1 {
      ..control,
      purpose_count: control.purpose_count + 1,
      purpose_peaks: validation_merkle_v1.append_leaf(
        control.purpose_count,
        control.purpose_peaks,
        script_proof_v1.purpose_leaf_hash(
          2,
          control.observer_scan.seen,
          observer_hash,
          observer_hash,
        ),
      ),
      observer_scan: ObserverPurposeScanControlV1 {
        total_count: active_count,
        seen: control.observer_scan.seen + 1,
        previous_hash: observer_hash,
      },
    },
  )
}

fn script_sources_stage_seven_receive_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
  candidate_hash: ByteArray,
) -> Bool {
  script_sources_stage_seven_successor_is_exact(
    pre,
    witness,
    ScriptSourcesControlV1 {
      ..control,
      output_cursor: control.output_cursor + 1,
      receive_scan: ReceivePurposeScanControlV1 {
        ..control.receive_scan,
        candidate_hash,
      },
    },
  )
}

fn script_sources_stage_seven_finish_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  let scan = control.receive_scan
  if scan.candidate_hash == #"" {
    script_sources_stage_seven_successor_is_exact(
      pre,
      witness,
      ScriptSourcesControlV1 {
        ..control,
        stage: 8,
        output_cursor: control.output_count,
        receive_scan: ReceivePurposeScanControlV1 {
          ..empty_receive_purpose_scan_control(),
          descriptor_peaks: scan.descriptor_peaks,
        },
        observer_scan: empty_observer_purpose_scan_control(),
        discovery: empty_script_discovery_control(),
      },
    )
  } else {
    let script_hash = scan.candidate_hash
    script_sources_stage_seven_successor_is_exact(
      pre,
      witness,
      ScriptSourcesControlV1 {
        ..control,
        output_cursor: 0,
        purpose_count: control.purpose_count + 1,
        purpose_peaks: validation_merkle_v1.append_leaf(
          control.purpose_count,
          control.purpose_peaks,
          script_proof_v1.purpose_leaf_hash(
            3,
            scan.receive_count,
            script_hash,
            script_hash,
          ),
        ),
        receive_scan: ReceivePurposeScanControlV1 {
          source_count: scan.source_count,
          source_peaks: scan.source_peaks,
          receive_count: scan.receive_count + 1,
          previous_hash: script_hash,
          candidate_hash: #"",
          descriptor_peaks: scan.descriptor_peaks,
        },
      },
    )
  }
}

pub fn verify_script_sources_stage_seven_observer_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  door: field_door.MachineFieldDoorV1,
  field_index: Int,
  item_index: Int,
  carriage: native_tx_field_access_v1.FieldCarriageV1,
) -> Bool {
  let control = script_sources_stage_seven_control_from_witness(
    witness.work_witness_cbor,
  )
  let Pair(verified, witness_set) = verify_tx_proof_source_v1(
    pre.source_kind,
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  let observer_commitment = verified.tx_compact.body.required_observers_hash
  let observer_scan = control.observer_scan
  let item = field_door.open_machine_field_item(
    door,
    verified,
    witness_set,
    3,
    observer_scan.seen,
    carriage,
  )
  let item_count = field_door.machine_field_item_count(item)
  let active_count = if observer_scan.total_count == 0 {
    item_count
  } else {
    observer_scan.total_count
  }
  let observer_hash = field_door.machine_field_item_bytes(item)
  let common_is_valid = and {
    script_sources_stage_seven_control_is_bound(pre, witness, control),
    observer_commitment != native_tx_field_access_v1.empty_field_commitment,
    !script_sources_stage_seven_observer_scan_is_complete(
      control,
      observer_commitment,
    ),
    control.output_cursor == 0,
    control.receive_scan.receive_count == 0,
    control.receive_scan.previous_hash == #"",
    control.receive_scan.candidate_hash == #"",
    active_count > 0,
    active_count <= max_tx_size_derived_collection_item_count,
    field_index == 3,
    item_index == observer_scan.seen,
    item_count == active_count,
    field_door.machine_field_item_length(item) == 28,
    bytearray.length(observer_hash) == 28,
  }
  if and {
    common_is_valid,
    observer_scan.seen > 0,
    bytearray.compare(observer_scan.previous_hash, observer_hash) != Less,
  } {
    rejected_successor_is_exact(
      pre,
      witness.claimed_successor,
      reject_invalid_field_type,
    )
  } else {
    and {
      common_is_valid,
      script_sources_stage_seven_observer_successor_is_exact(
        pre,
        witness,
        control,
        observer_hash,
        active_count,
      ),
    }
  }
}

pub fn verify_script_sources_stage_seven_observer_item_facts_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  door: field_door.MachineFieldDoorV1,
  field_index: Int,
  item_index: Int,
  carriage: native_tx_field_access_v1.FieldCarriageV1,
  claimed_observer_hash: ByteArray,
  claimed_active_count: Int,
) -> Bool {
  let control = script_sources_stage_seven_control_from_witness(
    witness.work_witness_cbor,
  )
  let Pair(verified, witness_set) = verify_tx_proof_source_v1(
    pre.source_kind,
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  let observer_commitment = verified.tx_compact.body.required_observers_hash
  let observer_scan = control.observer_scan
  let item = field_door.open_machine_field_item(
    door,
    verified,
    witness_set,
    3,
    observer_scan.seen,
    carriage,
  )
  let item_count = field_door.machine_field_item_count(item)
  let active_count = if observer_scan.total_count == 0 {
    item_count
  } else {
    observer_scan.total_count
  }
  let observer_hash = field_door.machine_field_item_bytes(item)
  and {
    observer_commitment != native_tx_field_access_v1.empty_field_commitment,
    !script_sources_stage_seven_observer_scan_is_complete(
      control,
      observer_commitment,
    ),
    active_count > 0,
    active_count <= max_tx_size_derived_collection_item_count,
    field_index == 3,
    item_index == observer_scan.seen,
    item_count == active_count,
    field_door.machine_field_item_length(item) == 28,
    claimed_observer_hash == observer_hash,
    claimed_active_count == active_count,
  }
}

pub fn verify_script_sources_stage_seven_observer_bound_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  observer_hash: ByteArray,
  active_count: Int,
) -> Bool {
  let control = script_sources_stage_seven_control_from_witness(
    witness.work_witness_cbor,
  )
  let observer_scan = control.observer_scan
  let common_is_valid = and {
    script_sources_stage_seven_control_is_bound(pre, witness, control),
    control.output_cursor == 0,
    control.receive_scan.receive_count == 0,
    control.receive_scan.previous_hash == #"",
    control.receive_scan.candidate_hash == #"",
    bytearray.length(observer_hash) == 28,
    active_count > 0,
    observer_scan.seen < active_count,
  }
  if and {
    common_is_valid,
    observer_scan.seen > 0,
    bytearray.compare(observer_scan.previous_hash, observer_hash) != Less,
  } {
    rejected_successor_is_exact(
      pre,
      witness.claimed_successor,
      reject_invalid_field_type,
    )
  } else {
    and {
      common_is_valid,
      script_sources_stage_seven_observer_successor_is_exact(
        pre,
        witness,
        control,
        observer_hash,
        active_count,
      ),
    }
  }
}

pub fn verify_script_sources_stage_seven_receive_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  purpose_kind: Int,
  purpose_index: Int,
  script_hash: ByteArray,
  subject: ByteArray,
  siblings: List<ByteArray>,
) -> Bool {
  let control = script_sources_stage_seven_control_from_witness(
    witness.work_witness_cbor,
  )
  let observer_commitment = decode_tx_compact_view_v1(
    pre.source_kind,
    control.compact_cbor,
  ).body.required_observers_hash
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(
    purpose_kind,
    purpose_index,
    script_hash,
    subject,
  )
  and {
    script_sources_stage_seven_control_is_bound(pre, witness, control),
    script_sources_stage_seven_observer_scan_is_complete(
      control,
      observer_commitment,
    ),
    control.output_cursor < control.receive_scan.source_count,
    purpose_kind == 3,
    purpose_index == control.output_cursor,
    bytearray.length(script_hash) == 28,
    subject == script_hash,
    validation_merkle_v1.verify_membership(
      control.receive_scan.source_count,
      control.receive_scan.source_peaks,
      purpose_index,
      purpose_leaf,
      siblings,
    ),
    script_sources_stage_seven_receive_successor_is_exact(
      pre,
      witness,
      control,
      next_receive_candidate(control.receive_scan, script_hash),
    ),
  }
}

pub fn verify_script_sources_stage_seven_finish_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let control = script_sources_stage_seven_control_from_witness(
    witness.work_witness_cbor,
  )
  let observer_commitment = decode_tx_compact_view_v1(
    pre.source_kind,
    control.compact_cbor,
  ).body.required_observers_hash
  and {
    script_sources_stage_seven_control_is_bound(pre, witness, control),
    script_sources_stage_seven_observer_scan_is_complete(
      control,
      observer_commitment,
    ),
    control.output_cursor == control.receive_scan.source_count,
    script_sources_stage_seven_finish_successor_is_exact(pre, witness, control),
  }
}

fn script_discovery_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
  stage: Int,
  discovery: ScriptDiscoveryControlV1,
) -> Bool {
  let prefix = script_sources_stage_zero_prefix_before_stage(control, 31)
  let stage_offset = bytearray.length(prefix)
  let old_stage = cbor.serialise(control.stage)
  let old_discovery = encode_definite_bytes(
    encode_script_discovery_control(control.discovery),
  )
  let work_length = bytearray.length(witness.work_witness_cbor)
  let discovery_offset = work_length - bytearray.length(old_discovery)
  expect discovery_offset >= stage_offset + bytearray.length(old_stage)
  expect
    builtin.slice_bytearray(0, stage_offset, witness.work_witness_cbor) == prefix
  expect
    builtin.slice_bytearray(
      stage_offset,
      bytearray.length(old_stage),
      witness.work_witness_cbor,
    ) == old_stage
  expect
    builtin.slice_bytearray(
      discovery_offset,
      bytearray.length(old_discovery),
      witness.work_witness_cbor,
    ) == old_discovery
  let next_control = builtin.slice_bytearray(
    0,
    stage_offset,
    witness.work_witness_cbor,
  )
    |> bytearray.concat(cbor.serialise(stage))
    |> bytearray.concat(
        builtin.slice_bytearray(
          stage_offset + bytearray.length(old_stage),
          discovery_offset - stage_offset - bytearray.length(old_stage),
          witness.work_witness_cbor,
        ),
      )
    |> bytearray.concat(
        encode_definite_bytes(encode_script_discovery_control(discovery)),
      )
  script_sources_stage_zero_successor_work_is_exact(pre, witness, next_control)
}

pub fn script_sources_stage_eight(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  let discovery = control.discovery
  if discovery.purpose_cursor == control.purpose_count {
    and {
      auxiliary == NoAuxiliaryWitness,
      script_discovery_successor_is_exact(
        pre,
        witness,
        control,
        11,
        reset_script_discovery_current(discovery),
      ),
    }
  } else {
    expect
        ScriptPurposeScanWitness {
          purpose_kind,
          purpose_index,
          script_hash,
          subject,
          siblings,
        }
      = auxiliary
    let purpose_leaf = script_proof_v1.purpose_leaf_hash(
      purpose_kind,
      purpose_index,
      script_hash,
      subject,
    )
    and {
      discovery.purpose_cursor < control.purpose_count,
      validation_merkle_v1.verify_membership(
        control.purpose_count,
        control.purpose_peaks,
        discovery.purpose_cursor,
        purpose_leaf,
        siblings,
      ),
      script_discovery_successor_is_exact(
        pre,
        witness,
        control,
        9,
        ScriptDiscoveryControlV1 {
          ..discovery,
          source_cursor: 0,
          redeemer_cursor: 0,
          current_purpose_kind: purpose_kind,
          current_purpose_index: purpose_index,
          current_script_hash: script_hash,
          current_subject: subject,
          matched_source_index: -1,
          matched_language_tag: -1,
          matched_source_leaf: #"",
        },
      ),
    }
  }
}

fn script_sources_stage_eight_control_is_bound(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  let discovery = control.discovery
  and {
    script_sources_discovery_frame_is_exact(witness, control),
    control.stage == 8,
    tx_proof_commitment_v1(
      pre.source_kind,
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
    ) == pre.transaction_commitment,
    validation_trace_v1.hash_validation_context(control.context_cbor) == pre.validation_context_hash,
    control.source_total_count == control.source_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.source_count,
      control.source_peaks,
    ),
    control.redeemer_total_count == control.redeemer_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.redeemer_count,
      control.redeemer_peaks,
    ),
    discovery.purpose_cursor >= 0,
    discovery.purpose_cursor <= control.purpose_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.purpose_count,
      control.purpose_peaks,
    ),
    discovery.source_cursor == 0,
    discovery.redeemer_cursor == 0,
    discovery.execution_count == discovery.purpose_cursor,
    validation_merkle_v1.frontier_is_well_formed(
      discovery.execution_count,
      discovery.execution_peaks,
    ),
    discovery.current_purpose_kind == -1,
    discovery.current_purpose_index == -1,
    discovery.current_script_hash == #"",
    discovery.current_subject == #"",
    discovery.matched_source_index == -1,
    discovery.matched_language_tag == -1,
    discovery.matched_source_leaf == #"",
    discovery.used_inline_bitmap >= 0,
    discovery.used_inline_bitmap < script_discovery_bit(control.source_count),
    discovery.used_redeemer_bitmap >= 0,
    discovery.used_redeemer_bitmap < script_discovery_bit(
      control.redeemer_count,
    ),
  }
}

pub fn verify_script_sources_stage_eight_finish_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let control = script_sources_stage_eight_control_from_witness(
    witness.work_witness_cbor,
  )
  let discovery = control.discovery
  and {
    script_sources_stage_eight_control_is_bound(pre, witness, control),
    discovery.purpose_cursor == control.purpose_count,
    script_discovery_successor_is_exact(
      pre,
      witness,
      control,
      11,
      reset_script_discovery_current(discovery),
    ),
  }
}

pub fn verify_script_sources_stage_eight_purpose_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  purpose_kind: Int,
  purpose_index: Int,
  script_hash: ByteArray,
  subject: ByteArray,
  siblings: List<ByteArray>,
) -> Bool {
  let control = script_sources_stage_eight_control_from_witness(
    witness.work_witness_cbor,
  )
  let discovery = control.discovery
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(
    purpose_kind,
    purpose_index,
    script_hash,
    subject,
  )
  and {
    script_sources_stage_eight_control_is_bound(pre, witness, control),
    discovery.purpose_cursor < control.purpose_count,
    purpose_kind >= 0,
    purpose_kind <= 3,
    purpose_index >= 0,
    bytearray.length(script_hash) == 28,
    subject != #"",
    validation_merkle_v1.verify_membership(
      control.purpose_count,
      control.purpose_peaks,
      discovery.purpose_cursor,
      purpose_leaf,
      siblings,
    ),
    script_discovery_successor_is_exact(
      pre,
      witness,
      control,
      9,
      ScriptDiscoveryControlV1 {
        ..discovery,
        source_cursor: 0,
        redeemer_cursor: 0,
        current_purpose_kind: purpose_kind,
        current_purpose_index: purpose_index,
        current_script_hash: script_hash,
        current_subject: subject,
        matched_source_index: -1,
        matched_language_tag: -1,
        matched_source_leaf: #"",
      },
    ),
  }
}

pub fn script_sources_stage_nine(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  let discovery = control.discovery
  if discovery.source_cursor == control.source_count {
    and {
      auxiliary == NoAuxiliaryWitness,
      rejected_successor_is_exact(
        pre,
        witness.claimed_successor,
        reject_missing_required_witness,
      ),
    }
  } else {
    expect
        ScriptSourceScanWitness {
          source_index,
          origin_kind,
          source_key,
          script_language_tag,
          script_hash,
          script_total_length,
          script_item_commitment,
          siblings,
        }
      = auxiliary
    let source_leaf = script_proof_v1.source_descriptor_leaf_hash(
      origin_kind,
      source_key,
      script_language_tag,
      script_hash,
      script_total_length,
      script_item_commitment,
    )
    if and {
      source_index == discovery.source_cursor,
      validation_merkle_v1.verify_membership(
        control.source_count,
        control.source_peaks,
        source_index,
        source_leaf,
        siblings,
      ),
    } {
      if script_hash == discovery.current_script_hash {
        let next_used_inline = if origin_kind == 0 {
          script_discovery_bitmap_insert(
            discovery.used_inline_bitmap,
            source_index,
          )
        } else {
          discovery.used_inline_bitmap
        }
        if script_language_tag == 0 {
          let next_execution = append_script_execution(
            discovery,
            current_script_purpose_leaf(discovery),
            source_leaf,
            script_language_tag,
            #"",
          )
          let completed = reset_script_discovery_current(
            ScriptDiscoveryControlV1 {
              ..discovery,
              purpose_cursor: discovery.purpose_cursor + 1,
              used_inline_bitmap: next_used_inline,
              execution_count: next_execution.count,
              execution_peaks: next_execution.peaks,
            },
          )
          script_discovery_successor_is_exact(
            pre,
            witness,
            control,
            8,
            completed,
          )
        } else {
          script_discovery_successor_is_exact(
            pre,
            witness,
            control,
            10,
            ScriptDiscoveryControlV1 {
              ..discovery,
              source_cursor: source_index + 1,
              redeemer_cursor: 0,
              matched_source_index: source_index,
              matched_language_tag: script_language_tag,
              matched_source_leaf: source_leaf,
              used_inline_bitmap: next_used_inline,
            },
          )
        }
      } else {
        script_discovery_successor_is_exact(
          pre,
          witness,
          control,
          9,
          ScriptDiscoveryControlV1 {
            ..discovery,
            source_cursor: source_index + 1,
          },
        )
      }
    } else {
      False
    }
  }
}

fn script_sources_stage_nine_control_is_bound(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  let discovery = control.discovery
  let prefix = script_sources_stage_zero_prefix_before_stage(control, 31)
  let stage_offset = bytearray.length(prefix)
  let encoded_discovery = encode_definite_bytes(
    encode_script_discovery_control(discovery),
  )
  let discovery_offset = bytearray.length(witness.work_witness_cbor) - bytearray.length(
    encoded_discovery,
  )
  and {
    control.stage == 9,
    tx_proof_commitment_v1(
      pre.source_kind,
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
    ) == pre.transaction_commitment,
    validation_trace_v1.hash_validation_context(control.context_cbor) == pre.validation_context_hash,
    control.source_total_count == control.source_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.source_count,
      control.source_peaks,
    ),
    control.redeemer_total_count == control.redeemer_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.redeemer_count,
      control.redeemer_peaks,
    ),
    control.purpose_count > discovery.purpose_cursor,
    validation_merkle_v1.frontier_is_well_formed(
      control.purpose_count,
      control.purpose_peaks,
    ),
    discovery.source_cursor <= control.source_count,
    discovery.redeemer_cursor <= control.redeemer_count,
    discovery.execution_count == discovery.purpose_cursor,
    validation_merkle_v1.frontier_is_well_formed(
      discovery.execution_count,
      discovery.execution_peaks,
    ),
    discovery.current_purpose_kind >= 0,
    discovery.current_purpose_kind <= 3,
    discovery.current_purpose_index >= 0,
    bytearray.length(discovery.current_script_hash) == 28,
    discovery.current_subject != #"",
    discovery.matched_source_index == -1,
    discovery.matched_language_tag == -1,
    discovery.matched_source_leaf == #"",
    control.pending_source_cbor == #"",
    control.output_proof == None,
    discovery_offset >= stage_offset + 1,
    builtin.slice_bytearray(0, stage_offset, witness.work_witness_cbor) == prefix,
    builtin.slice_bytearray(stage_offset, 1, witness.work_witness_cbor) == #"09",
    builtin.slice_bytearray(
      discovery_offset,
      bytearray.length(encoded_discovery),
      witness.work_witness_cbor,
    ) == encoded_discovery,
  }
}

fn script_sources_stage_nine_source_is_authenticated(
  control: ScriptSourcesControlV1,
  source_index: Int,
  origin_kind: Int,
  source_key: ByteArray,
  script_language_tag: Int,
  script_hash: ByteArray,
  script_total_length: Int,
  script_item_commitment: ByteArray,
  siblings: List<ByteArray>,
) -> Bool {
  let source_leaf = script_proof_v1.source_descriptor_leaf_hash(
    origin_kind,
    source_key,
    script_language_tag,
    script_hash,
    script_total_length,
    script_item_commitment,
  )
  and {
    source_index == control.discovery.source_cursor,
    validation_merkle_v1.verify_membership(
      control.source_count,
      control.source_peaks,
      source_index,
      source_leaf,
      siblings,
    ),
  }
}

pub fn verify_script_sources_stage_nine_mismatch_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  source_index: Int,
  origin_kind: Int,
  source_key: ByteArray,
  script_language_tag: Int,
  script_hash: ByteArray,
  script_total_length: Int,
  script_item_commitment: ByteArray,
  siblings: List<ByteArray>,
) -> Bool {
  let control = script_sources_stage_nine_control_from_witness(
    witness.work_witness_cbor,
  )
  let discovery = control.discovery
  and {
    script_sources_stage_nine_control_is_bound(pre, witness, control),
    discovery.source_cursor < control.source_count,
    script_sources_stage_nine_source_is_authenticated(
      control,
      source_index,
      origin_kind,
      source_key,
      script_language_tag,
      script_hash,
      script_total_length,
      script_item_commitment,
      siblings,
    ),
    script_hash != discovery.current_script_hash,
    script_discovery_successor_is_exact(
      pre,
      witness,
      control,
      9,
      ScriptDiscoveryControlV1 { ..discovery, source_cursor: source_index + 1 },
    ),
  }
}

pub fn verify_script_sources_stage_nine_native_match_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  source_index: Int,
  origin_kind: Int,
  source_key: ByteArray,
  script_hash: ByteArray,
  script_total_length: Int,
  script_item_commitment: ByteArray,
  siblings: List<ByteArray>,
) -> Bool {
  let control = script_sources_stage_nine_control_from_witness(
    witness.work_witness_cbor,
  )
  let discovery = control.discovery
  let source_leaf = script_proof_v1.source_descriptor_leaf_hash(
    origin_kind,
    source_key,
    0,
    script_hash,
    script_total_length,
    script_item_commitment,
  )
  let next_used_inline = if origin_kind == 0 {
    script_discovery_bitmap_insert(discovery.used_inline_bitmap, source_index)
  } else {
    discovery.used_inline_bitmap
  }
  let next_execution = append_script_execution(
    discovery,
    current_script_purpose_leaf(discovery),
    source_leaf,
    0,
    #"",
  )
  let completed = reset_script_discovery_current(
    ScriptDiscoveryControlV1 {
      ..discovery,
      purpose_cursor: discovery.purpose_cursor + 1,
      used_inline_bitmap: next_used_inline,
      execution_count: next_execution.count,
      execution_peaks: next_execution.peaks,
    },
  )
  and {
    script_sources_stage_nine_control_is_bound(pre, witness, control),
    discovery.source_cursor < control.source_count,
    script_sources_stage_nine_source_is_authenticated(
      control,
      source_index,
      origin_kind,
      source_key,
      0,
      script_hash,
      script_total_length,
      script_item_commitment,
      siblings,
    ),
    script_hash == discovery.current_script_hash,
    script_discovery_successor_is_exact(pre, witness, control, 8, completed),
  }
}

pub fn verify_script_sources_stage_nine_effectful_match_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  source_index: Int,
  origin_kind: Int,
  source_key: ByteArray,
  script_language_tag: Int,
  script_hash: ByteArray,
  script_total_length: Int,
  script_item_commitment: ByteArray,
  siblings: List<ByteArray>,
) -> Bool {
  let control = script_sources_stage_nine_control_from_witness(
    witness.work_witness_cbor,
  )
  let discovery = control.discovery
  and {
    or {
      script_language_tag == 3,
      script_language_tag == 128,
    },
    script_sources_stage_nine_control_is_bound(pre, witness, control),
    discovery.source_cursor < control.source_count,
    script_sources_stage_nine_source_is_authenticated(
      control,
      source_index,
      origin_kind,
      source_key,
      script_language_tag,
      script_hash,
      script_total_length,
      script_item_commitment,
      siblings,
    ),
    script_hash == discovery.current_script_hash,
    script_discovery_successor_is_exact(
      pre,
      witness,
      control,
      10,
      ScriptDiscoveryControlV1 {
        ..discovery,
        source_cursor: source_index + 1,
        redeemer_cursor: 0,
        matched_source_index: source_index,
        matched_language_tag: script_language_tag,
        matched_source_leaf: script_proof_v1.source_descriptor_leaf_hash(
          origin_kind,
          source_key,
          script_language_tag,
          script_hash,
          script_total_length,
          script_item_commitment,
        ),
        used_inline_bitmap: if origin_kind == 0 {
          script_discovery_bitmap_insert(
            discovery.used_inline_bitmap,
            source_index,
          )
        } else {
          discovery.used_inline_bitmap
        },
      },
    ),
  }
}

pub fn verify_script_sources_stage_nine_missing_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let control = script_sources_stage_nine_control_from_witness(
    witness.work_witness_cbor,
  )
  and {
    script_sources_stage_nine_control_is_bound(pre, witness, control),
    control.discovery.source_cursor == control.source_count,
    rejected_successor_is_exact(
      pre,
      witness.claimed_successor,
      reject_missing_required_witness,
    ),
  }
}

pub fn redeemer_descriptor_matches_current_purpose(
  descriptor: redeemer_item_proof_v1.RedeemerItemDescriptorV1,
  discovery: ScriptDiscoveryControlV1,
) -> Bool {
  redeemer_pointer_matches_purpose_v1(
    discovery.current_purpose_kind,
    discovery.current_purpose_index,
    descriptor.purpose_tag,
    descriptor.pointer_index,
  )
}

pub fn script_sources_stage_ten(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  let discovery = control.discovery
  if discovery.redeemer_cursor == control.redeemer_count {
    and {
      discovery.redeemer_item_control_hash == #"",
      auxiliary == NoAuxiliaryWitness,
      rejected_successor_is_exact(
        pre,
        witness.claimed_successor,
        reject_missing_required_witness,
      ),
    }
  } else if discovery.redeemer_item_control_hash == #"" {
    expect
        RedeemerScanBeginWitness {
          item_index,
          item_count,
          total_length,
          item_commitment,
          siblings,
        }
      = auxiliary
    let redeemer_leaf = script_proof_v1.redeemer_item_leaf_hash(
      item_index,
      item_commitment,
    )
    let item_control = redeemer_item_proof_v1.initial_control_v1(
      redeemer_item_proof_v1.mode_descriptor,
      item_index,
      item_count,
      total_length,
      item_commitment,
      -1,
      -1,
    )
    and {
      item_index == discovery.redeemer_cursor,
      item_count == control.redeemer_count,
      validation_merkle_v1.verify_membership(
        control.redeemer_count,
        control.redeemer_peaks,
        item_index,
        redeemer_leaf,
        siblings,
      ),
      script_discovery_successor_is_exact(
        pre,
        witness,
        control,
        10,
        ScriptDiscoveryControlV1 {
          ..discovery,
          redeemer_item_control_hash: redeemer_item_proof_v1.hash_control_v1(
            item_control,
          ),
        },
      ),
    }
  } else {
    expect
        RedeemerItemStepWitness {
          redeemer_control: None,
          control: item_control,
          witness: item_witness,
        }
      = auxiliary
    let current_matches = and {
      item_control.mode == redeemer_item_proof_v1.mode_descriptor,
      item_control.item_index == discovery.redeemer_cursor,
      item_control.item_count == control.redeemer_count,
      redeemer_item_proof_v1.hash_control_v1(item_control) == discovery.redeemer_item_control_hash,
    }
    when redeemer_item_proof_v1.step_v1(item_control, item_witness) is {
      None -> False
      Some(redeemer_item_proof_v1.RedeemerItemProofInvalid) -> False
      Some(redeemer_item_proof_v1.RedeemerItemProofAdvanced { control: next }) ->
        if next.stage == redeemer_item_proof_v1.stage_terminal {
          when redeemer_item_proof_v1.descriptor_v1(next) is {
            None -> False
            Some(descriptor) -> {
              let redeemer_leaf = script_proof_v1.redeemer_item_leaf_hash(
                descriptor.item_index,
                descriptor.item_commitment,
              )
              if redeemer_descriptor_matches_current_purpose(
                descriptor,
                discovery,
              ) {
                let next_execution = append_script_execution(
                  discovery,
                  current_script_purpose_leaf(discovery),
                  discovery.matched_source_leaf,
                  discovery.matched_language_tag,
                  redeemer_leaf,
                )
                let completed = reset_script_discovery_current(
                  ScriptDiscoveryControlV1 {
                    ..discovery,
                    purpose_cursor: discovery.purpose_cursor + 1,
                    used_redeemer_bitmap: script_discovery_bitmap_insert(
                      discovery.used_redeemer_bitmap,
                      descriptor.item_index,
                    ),
                    execution_count: next_execution.count,
                    execution_peaks: next_execution.peaks,
                  },
                )
                and {
                  current_matches,
                  script_discovery_successor_is_exact(
                    pre,
                    witness,
                    control,
                    8,
                    completed,
                  ),
                }
              } else {
                and {
                  current_matches,
                  script_discovery_successor_is_exact(
                    pre,
                    witness,
                    control,
                    10,
                    ScriptDiscoveryControlV1 {
                      ..discovery,
                      redeemer_cursor: descriptor.item_index + 1,
                      redeemer_item_control_hash: #"",
                    },
                  ),
                }
              }
            }
          }
        } else {
          and {
            current_matches,
            script_discovery_successor_is_exact(
              pre,
              witness,
              control,
              10,
              ScriptDiscoveryControlV1 {
                ..discovery,
                redeemer_item_control_hash: redeemer_item_proof_v1.hash_control_v1(
                  next,
                ),
              },
            ),
          }
        }
    }
  }
}

fn script_sources_stage_ten_control_is_bound(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  let discovery = control.discovery
  and {
    script_sources_discovery_frame_is_exact(witness, control),
    control.stage == 10,
    tx_proof_commitment_v1(
      pre.source_kind,
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
    ) == pre.transaction_commitment,
    validation_trace_v1.hash_validation_context(control.context_cbor) == pre.validation_context_hash,
    control.source_total_count == control.source_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.source_count,
      control.source_peaks,
    ),
    control.redeemer_total_count == control.redeemer_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.redeemer_count,
      control.redeemer_peaks,
    ),
    discovery.purpose_cursor < control.purpose_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.purpose_count,
      control.purpose_peaks,
    ),
    discovery.source_cursor >= 0,
    discovery.source_cursor <= control.source_count,
    discovery.redeemer_cursor >= 0,
    discovery.redeemer_cursor <= control.redeemer_count,
    discovery.execution_count == discovery.purpose_cursor,
    validation_merkle_v1.frontier_is_well_formed(
      discovery.execution_count,
      discovery.execution_peaks,
    ),
    discovery.current_purpose_kind >= 0,
    discovery.current_purpose_kind <= 3,
    discovery.current_purpose_index >= 0,
    bytearray.length(discovery.current_script_hash) == 28,
    discovery.current_subject != #"",
    discovery.matched_source_index >= 0,
    discovery.matched_source_index < control.source_count,
    or {
      discovery.matched_language_tag == 3,
      discovery.matched_language_tag == 128,
    },
    bytearray.length(discovery.matched_source_leaf) == 32,
    discovery.used_inline_bitmap >= 0,
    discovery.used_inline_bitmap < script_discovery_bit(control.source_count),
    discovery.used_redeemer_bitmap >= 0,
    discovery.used_redeemer_bitmap < script_discovery_bit(
      control.redeemer_count,
    ),
  }
}

pub fn verify_script_sources_stage_ten_missing_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let control = script_sources_stage_ten_control_from_witness(
    witness.work_witness_cbor,
  )
  and {
    script_sources_stage_ten_control_is_bound(pre, witness, control),
    control.discovery.redeemer_cursor == control.redeemer_count,
    rejected_successor_is_exact(
      pre,
      witness.claimed_successor,
      reject_missing_required_witness,
    ),
  }
}

pub fn script_sources_stage_eleven(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  let discovery = control.discovery
  if discovery.source_cursor == control.source_count {
    and {
      auxiliary == NoAuxiliaryWitness,
      script_discovery_successor_is_exact(
        pre,
        witness,
        control,
        12,
        ScriptDiscoveryControlV1 { ..discovery, redeemer_cursor: 0 },
      ),
    }
  } else {
    expect
        ScriptSourceScanWitness {
          source_index,
          origin_kind,
          source_key,
          script_language_tag,
          script_hash,
          script_total_length,
          script_item_commitment,
          siblings,
        }
      = auxiliary
    let source_leaf = script_proof_v1.source_descriptor_leaf_hash(
      origin_kind,
      source_key,
      script_language_tag,
      script_hash,
      script_total_length,
      script_item_commitment,
    )
    if and {
      source_index == discovery.source_cursor,
      validation_merkle_v1.verify_membership(
        control.source_count,
        control.source_peaks,
        source_index,
        source_leaf,
        siblings,
      ),
    } {
      if and {
        origin_kind == 0,
        !script_discovery_bitmap_has(discovery.used_inline_bitmap, source_index),
      } {
        rejected_successor_is_exact(
          pre,
          witness.claimed_successor,
          reject_invalid_field_type,
        )
      } else {
        script_discovery_successor_is_exact(
          pre,
          witness,
          control,
          11,
          ScriptDiscoveryControlV1 {
            ..discovery,
            source_cursor: source_index + 1,
          },
        )
      }
    } else {
      False
    }
  }
}

fn script_sources_stage_eleven_control_is_bound(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  let discovery = control.discovery
  and {
    script_sources_discovery_frame_is_exact(witness, control),
    control.stage == 11,
    tx_proof_commitment_v1(
      pre.source_kind,
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
    ) == pre.transaction_commitment,
    validation_trace_v1.hash_validation_context(control.context_cbor) == pre.validation_context_hash,
    control.source_total_count == control.source_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.source_count,
      control.source_peaks,
    ),
    control.redeemer_total_count == control.redeemer_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.redeemer_count,
      control.redeemer_peaks,
    ),
    control.purpose_count == discovery.purpose_cursor,
    validation_merkle_v1.frontier_is_well_formed(
      control.purpose_count,
      control.purpose_peaks,
    ),
    discovery.source_cursor >= 0,
    discovery.source_cursor <= control.source_count,
    discovery.redeemer_cursor >= 0,
    discovery.redeemer_cursor <= control.redeemer_count,
    discovery.execution_count == discovery.purpose_cursor,
    validation_merkle_v1.frontier_is_well_formed(
      discovery.execution_count,
      discovery.execution_peaks,
    ),
    discovery.current_purpose_kind == -1,
    discovery.current_purpose_index == -1,
    discovery.current_script_hash == #"",
    discovery.current_subject == #"",
    discovery.matched_source_index == -1,
    discovery.matched_language_tag == -1,
    discovery.matched_source_leaf == #"",
    discovery.used_inline_bitmap >= 0,
    discovery.used_inline_bitmap < script_discovery_bit(control.source_count),
    discovery.used_redeemer_bitmap >= 0,
    discovery.used_redeemer_bitmap < script_discovery_bit(
      control.redeemer_count,
    ),
  }
}

pub fn verify_script_sources_stage_eleven_finish_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let control = script_sources_stage_eleven_control_from_witness(
    witness.work_witness_cbor,
  )
  let discovery = control.discovery
  and {
    script_sources_stage_eleven_control_is_bound(pre, witness, control),
    discovery.source_cursor == control.source_count,
    script_discovery_successor_is_exact(
      pre,
      witness,
      control,
      12,
      ScriptDiscoveryControlV1 { ..discovery, redeemer_cursor: 0 },
    ),
  }
}

pub fn verify_script_sources_stage_eleven_source_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  source_index: Int,
  origin_kind: Int,
  source_key: ByteArray,
  script_language_tag: Int,
  script_hash: ByteArray,
  script_total_length: Int,
  script_item_commitment: ByteArray,
  siblings: List<ByteArray>,
) -> Bool {
  let control = script_sources_stage_eleven_control_from_witness(
    witness.work_witness_cbor,
  )
  let discovery = control.discovery
  and {
    script_sources_stage_eleven_control_is_bound(pre, witness, control),
    discovery.source_cursor < control.source_count,
    script_sources_stage_nine_source_is_authenticated(
      control,
      source_index,
      origin_kind,
      source_key,
      script_language_tag,
      script_hash,
      script_total_length,
      script_item_commitment,
      siblings,
    ),
    if and {
      origin_kind == 0,
      !script_discovery_bitmap_has(discovery.used_inline_bitmap, source_index),
    } {
      rejected_successor_is_exact(
        pre,
        witness.claimed_successor,
        reject_invalid_field_type,
      )
    } else {
      script_discovery_successor_is_exact(
        pre,
        witness,
        control,
        11,
        ScriptDiscoveryControlV1 {
          ..discovery,
          source_cursor: source_index + 1,
        },
      )
    },
  }
}

pub fn script_sources_stage_twelve(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  let discovery = control.discovery
  if discovery.redeemer_cursor == control.redeemer_count {
    let next_control = encode_native_scripts_witness_v1(control, 0, 0)
    and {
      discovery.redeemer_item_control_hash == #"",
      auxiliary == NoAuxiliaryWitness,
      witness.claimed_successor.phase == validation_trace_v1.NativeScripts,
      witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
        validation_trace_v1.NativeScripts,
        pre.program_counter + 1,
        next_control,
      ),
    }
  } else if discovery.redeemer_item_control_hash == #"" {
    expect
        RedeemerScanBeginWitness {
          item_index,
          item_count,
          total_length,
          item_commitment,
          siblings,
        }
      = auxiliary
    let redeemer_leaf = script_proof_v1.redeemer_item_leaf_hash(
      item_index,
      item_commitment,
    )
    let item_control = redeemer_item_proof_v1.initial_control_v1(
      redeemer_item_proof_v1.mode_descriptor,
      item_index,
      item_count,
      total_length,
      item_commitment,
      -1,
      -1,
    )
    and {
      item_index == discovery.redeemer_cursor,
      item_count == control.redeemer_count,
      validation_merkle_v1.verify_membership(
        control.redeemer_count,
        control.redeemer_peaks,
        item_index,
        redeemer_leaf,
        siblings,
      ),
      script_discovery_successor_is_exact(
        pre,
        witness,
        control,
        12,
        ScriptDiscoveryControlV1 {
          ..discovery,
          redeemer_item_control_hash: redeemer_item_proof_v1.hash_control_v1(
            item_control,
          ),
        },
      ),
    }
  } else {
    expect
        RedeemerItemStepWitness {
          redeemer_control: None,
          control: item_control,
          witness: item_witness,
        }
      = auxiliary
    let current_matches = and {
      item_control.mode == redeemer_item_proof_v1.mode_descriptor,
      item_control.item_index == discovery.redeemer_cursor,
      item_control.item_count == control.redeemer_count,
      redeemer_item_proof_v1.hash_control_v1(item_control) == discovery.redeemer_item_control_hash,
    }
    when redeemer_item_proof_v1.step_v1(item_control, item_witness) is {
      None -> False
      Some(redeemer_item_proof_v1.RedeemerItemProofInvalid) -> False
      Some(redeemer_item_proof_v1.RedeemerItemProofAdvanced { control: next }) ->
        if next.stage == redeemer_item_proof_v1.stage_terminal {
          when redeemer_item_proof_v1.descriptor_v1(next) is {
            None -> False
            Some(descriptor) -> and {
                current_matches,
                if !script_discovery_bitmap_has(
                  discovery.used_redeemer_bitmap,
                  descriptor.item_index,
                ) {
                  rejected_successor_is_exact(
                    pre,
                    witness.claimed_successor,
                    reject_invalid_field_type,
                  )
                } else {
                  script_discovery_successor_is_exact(
                    pre,
                    witness,
                    control,
                    12,
                    ScriptDiscoveryControlV1 {
                      ..discovery,
                      redeemer_cursor: descriptor.item_index + 1,
                      redeemer_item_control_hash: #"",
                    },
                  )
                },
              }
          }
        } else {
          and {
            current_matches,
            script_discovery_successor_is_exact(
              pre,
              witness,
              control,
              12,
              ScriptDiscoveryControlV1 {
                ..discovery,
                redeemer_item_control_hash: redeemer_item_proof_v1.hash_control_v1(
                  next,
                ),
              },
            ),
          }
        }
    }
  }
}

fn script_sources_stage_twelve_control_is_bound(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  let discovery = control.discovery
  and {
    script_sources_discovery_frame_is_exact(witness, control),
    control.stage == 12,
    tx_proof_commitment_v1(
      pre.source_kind,
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
    ) == pre.transaction_commitment,
    validation_trace_v1.hash_validation_context(control.context_cbor) == pre.validation_context_hash,
    control.source_total_count == control.source_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.source_count,
      control.source_peaks,
    ),
    control.redeemer_total_count == control.redeemer_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.redeemer_count,
      control.redeemer_peaks,
    ),
    control.purpose_count == discovery.purpose_cursor,
    validation_merkle_v1.frontier_is_well_formed(
      control.purpose_count,
      control.purpose_peaks,
    ),
    discovery.source_cursor == control.source_count,
    discovery.redeemer_cursor >= 0,
    discovery.redeemer_cursor <= control.redeemer_count,
    discovery.execution_count == discovery.purpose_cursor,
    validation_merkle_v1.frontier_is_well_formed(
      discovery.execution_count,
      discovery.execution_peaks,
    ),
    discovery.current_purpose_kind == -1,
    discovery.current_purpose_index == -1,
    discovery.current_script_hash == #"",
    discovery.current_subject == #"",
    discovery.matched_source_index == -1,
    discovery.matched_language_tag == -1,
    discovery.matched_source_leaf == #"",
    discovery.used_inline_bitmap >= 0,
    discovery.used_inline_bitmap < script_discovery_bit(control.source_count),
    discovery.used_redeemer_bitmap >= 0,
    discovery.used_redeemer_bitmap < script_discovery_bit(
      control.redeemer_count,
    ),
  }
}

pub fn verify_script_sources_stage_twelve_finish_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let control = script_sources_stage_twelve_control_from_witness(
    witness.work_witness_cbor,
  )
  let next_control = encode_native_scripts_witness_v1(control, 0, 0)
  and {
    script_sources_stage_twelve_control_is_bound(pre, witness, control),
    control.discovery.redeemer_cursor == control.redeemer_count,
    witness.claimed_successor.phase == validation_trace_v1.NativeScripts,
    witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.NativeScripts,
      pre.program_counter + 1,
      next_control,
    ),
  }
}

pub fn verify_descriptor_step_claim(
  witness: ValidationOneStepWitnessV1,
  claim: descriptor_claim.DescriptorStepClaim,
) -> Bool {
  let control = script_sources_redeemer_scan_control_from_witness(
    witness.work_witness_cbor,
  )
  let current = descriptor_claim.full_control(claim.control)
  and {
    control.discovery.redeemer_cursor < control.redeemer_count,
    control.discovery.redeemer_item_control_hash != #"",
    current.item_index == control.discovery.redeemer_cursor,
    current.item_count == control.redeemer_count,
    redeemer_item_proof_v1.hash_descriptor_control_v1(current) == control.discovery.redeemer_item_control_hash,
    redeemer_item_proof_v1.descriptor_step_v1(
      current,
      claim.open_tail,
      claim.chunk_proof,
      claim.next_chunk_proof,
    ) == Some(
      redeemer_item_proof_v1.RedeemerItemProofAdvanced {
        control: descriptor_claim.full_control(claim.claimed_next),
      },
    ),
  }
}

pub fn verify_descriptor_match(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  claim: descriptor_claim.DescriptorStepClaim,
) -> Bool {
  let control = script_sources_stage_ten_control_from_witness(
    witness.work_witness_cbor,
  )
  let discovery = control.discovery
  let next = descriptor_claim.full_control(claim.claimed_next)
  let terminal = next.stage == redeemer_item_proof_v1.stage_terminal
  let matches = terminal && redeemer_pointer_matches_purpose_v1(
    discovery.current_purpose_kind,
    discovery.current_purpose_index,
    next.purpose_tag,
    next.pointer_index,
  )
  and {
    script_sources_stage_ten_control_is_bound(pre, witness, control),
    discovery.redeemer_cursor < control.redeemer_count,
    discovery.redeemer_item_control_hash != #"",
    matches,
    {
      let execution = append_script_execution(
        discovery,
        current_script_purpose_leaf(discovery),
        discovery.matched_source_leaf,
        discovery.matched_language_tag,
        script_proof_v1.redeemer_item_leaf_hash(
          next.item_index,
          next.item_commitment,
        ),
      )
      script_discovery_successor_is_exact(
        pre,
        witness,
        control,
        8,
        reset_script_discovery_current(
          ScriptDiscoveryControlV1 {
            ..discovery,
            purpose_cursor: discovery.purpose_cursor + 1,
            used_redeemer_bitmap: script_discovery_bitmap_insert(
              discovery.used_redeemer_bitmap,
              next.item_index,
            ),
            execution_count: execution.count,
            execution_peaks: execution.peaks,
          },
        ),
      )
    },
  }
}

pub fn verify_descriptor_mismatch(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  claim: descriptor_claim.DescriptorStepClaim,
) -> Bool {
  let control = script_sources_stage_ten_control_from_witness(
    witness.work_witness_cbor,
  )
  let discovery = control.discovery
  let next = descriptor_claim.full_control(claim.claimed_next)
  let terminal = next.stage == redeemer_item_proof_v1.stage_terminal
  let matches = terminal && redeemer_pointer_matches_purpose_v1(
    discovery.current_purpose_kind,
    discovery.current_purpose_index,
    next.purpose_tag,
    next.pointer_index,
  )
  and {
    script_sources_stage_ten_control_is_bound(pre, witness, control),
    discovery.redeemer_cursor < control.redeemer_count,
    discovery.redeemer_item_control_hash != #"",
    !matches,
    {
      let after = if terminal {
        ScriptDiscoveryControlV1 {
          ..discovery,
          redeemer_cursor: next.item_index + 1,
          redeemer_item_control_hash: #"",
        }
      } else {
        ScriptDiscoveryControlV1 {
          ..discovery,
          redeemer_item_control_hash: redeemer_item_proof_v1.hash_descriptor_control_v1(
            next,
          ),
        }
      }
      script_discovery_successor_is_exact(pre, witness, control, 10, after)
    },
  }
}

pub fn verify_descriptor_used(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  claim: descriptor_claim.DescriptorStepClaim,
) -> Bool {
  let control = script_sources_stage_twelve_control_from_witness(
    witness.work_witness_cbor,
  )
  let discovery = control.discovery
  let next = descriptor_claim.full_control(claim.claimed_next)
  let terminal = next.stage == redeemer_item_proof_v1.stage_terminal
  and {
    script_sources_stage_twelve_control_is_bound(pre, witness, control),
    discovery.redeemer_cursor < control.redeemer_count,
    discovery.redeemer_item_control_hash != #"",
    if terminal && !script_discovery_bitmap_has(
      discovery.used_redeemer_bitmap,
      next.item_index,
    ) {
      rejected_successor_is_exact(
        pre,
        witness.claimed_successor,
        reject_invalid_field_type,
      )
    } else {
      let after = if terminal {
        ScriptDiscoveryControlV1 {
          ..discovery,
          redeemer_cursor: next.item_index + 1,
          redeemer_item_control_hash: #"",
        }
      } else {
        ScriptDiscoveryControlV1 {
          ..discovery,
          redeemer_item_control_hash: redeemer_item_proof_v1.hash_descriptor_control_v1(
            next,
          ),
        }
      }
      script_discovery_successor_is_exact(pre, witness, control, 12, after)
    },
  }
}

pub fn verify_descriptor_begin_ten(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  item_index: Int,
  item_count: Int,
  total_length: Int,
  item_commitment: ByteArray,
  siblings: List<ByteArray>,
) -> Bool {
  let control = script_sources_stage_ten_control_from_witness(
    witness.work_witness_cbor,
  )
  script_sources_stage_ten_control_is_bound(pre, witness, control) && descriptor_begin_successor(
    pre,
    witness,
    control,
    item_index,
    item_count,
    total_length,
    item_commitment,
    siblings,
  )
}

pub fn verify_descriptor_begin_twelve(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  item_index: Int,
  item_count: Int,
  total_length: Int,
  item_commitment: ByteArray,
  siblings: List<ByteArray>,
) -> Bool {
  let control = script_sources_stage_twelve_control_from_witness(
    witness.work_witness_cbor,
  )
  script_sources_stage_twelve_control_is_bound(pre, witness, control) && descriptor_begin_successor(
    pre,
    witness,
    control,
    item_index,
    item_count,
    total_length,
    item_commitment,
    siblings,
  )
}

fn descriptor_begin_successor(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
  item_index: Int,
  item_count: Int,
  total_length: Int,
  item_commitment: ByteArray,
  siblings: List<ByteArray>,
) -> Bool {
  let discovery = control.discovery
  let initial = redeemer_item_proof_v1.initial_control_v1(
    redeemer_item_proof_v1.mode_descriptor,
    item_index,
    item_count,
    total_length,
    item_commitment,
    -1,
    -1,
  )
  and {
    discovery.redeemer_cursor < control.redeemer_count,
    discovery.redeemer_item_control_hash == #"",
    item_index == discovery.redeemer_cursor,
    item_count == control.redeemer_count,
    validation_merkle_v1.verify_membership(
      control.redeemer_count,
      control.redeemer_peaks,
      item_index,
      script_proof_v1.redeemer_item_leaf_hash(item_index, item_commitment),
      siblings,
    ),
    script_discovery_successor_is_exact(
      pre,
      witness,
      control,
      control.stage,
      ScriptDiscoveryControlV1 {
        ..discovery,
        redeemer_item_control_hash: redeemer_item_proof_v1.hash_descriptor_control_v1(
          initial,
        ),
      },
    ),
  }
}
