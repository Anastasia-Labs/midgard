//// Narrow middle-stage predicates over the authenticated canonical frame.
//// Reuse the generic semantic engines with exact bounded successor splices.

use aiken/cbor
use aiken/primitive/bytearray
use midgard/bounded_item_v1
use midgard/fraud_proofs/native_tx/codec.{encode_definite_bytes}
use midgard/fraud_proofs/native_tx/compact.{
  decode_native_tx_compact_v1, verify_native_tx_proof_source_v1,
}
use midgard/native_tx_field_access_v1
use midgard/script_sources_raw_frame_v1 as raw
use midgard/validation_machine/machine_types.{
  MintFoldControlV1, ScriptSourcesControlV1, TransactionRedeemerItemBeginWitness,
  ValidationOneStepWitnessV1,
}
use midgard/validation_machine/script_sources_middle_stages as middle
use midgard/validation_machine/witness.{
  empty_resolution_schedule_hash, empty_script_discovery_control,
  encode_mint_fold_control, initial_resolution_accumulator,
  script_sources_control_from_data_items,
}
use midgard/validation_machine_field_door_v1 as field_door
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

fn base_control(frame: raw.RawScriptSourcesFrameV1) -> ScriptSourcesControlV1 {
  script_sources_control_from_data_items(
    frame.items,
    empty_script_discovery_control(),
    None,
  )
}

pub fn stage_two_advance(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let frame = raw.open_frame_v1(pre, witness, 30, 2)
  and {
    raw.item_int_v1(frame, 4) > 0,
    raw.item_int_v1(frame, 14) == 0,
    raw.item_frontier_v1(frame, 8) == [],
    raw.item_bytes_v1(frame, 15) == initial_resolution_accumulator(),
    raw.item_bytes_v1(frame, 16) == empty_resolution_schedule_hash(),
    raw.item_int_v1(frame, 17) == 0,
    raw.item_int_v1(frame, 18) == 0,
    raw.item_frontier_v1(frame, 19) == [],
    raw.item_bytes_v1(frame, 29) != empty_resolution_schedule_hash(),
    raw.successor_is_exact_v1(
      pre,
      witness,
      raw.replace_stage_v1(
        frame,
        raw.replace_items_v1(
          frame,
          witness.work_witness_cbor,
          16,
          1,
          encode_definite_bytes(raw.item_bytes_v1(frame, 29)),
        ),
        3,
      ),
    ),
  }
}

pub fn stage_three_replay(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  source_kind: Int,
  key: ByteArray,
  next_schedule_hash: ByteArray,
  value: ByteArray,
) -> Bool {
  let frame = raw.open_frame_v1(pre, witness, 30, 3)
  and {
    raw.item_bytes_v1(frame, 16) != empty_resolution_schedule_hash(),
    middle.script_sources_replay_item(
      pre,
      witness,
      base_control(frame),
      source_kind,
      key,
      next_schedule_hash,
      value,
    ),
  }
}

pub fn stage_three_finish(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let frame = raw.open_frame_v1(pre, witness, 30, 3)
  and {
    raw.item_bytes_v1(frame, 16) == empty_resolution_schedule_hash(),
    raw.item_int_v1(frame, 14) == raw.item_int_v1(frame, 4),
    raw.item_bytes_v1(frame, 15) == raw.item_bytes_v1(frame, 5),
    raw.successor_is_exact_v1(
      pre,
      witness,
      raw.replace_stage_v1(frame, witness.work_witness_cbor, 4),
    ),
  }
}

pub fn stage_four_begin(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  door: field_door.MachineFieldDoorV1,
  carriage: native_tx_field_access_v1.FieldCarriageV1,
) -> Bool {
  let frame = raw.open_frame_v1(pre, witness, 30, 4)
  middle.script_sources_stage_four_with_successor(
    pre,
    witness,
    TransactionRedeemerItemBeginWitness { carriage },
    base_control(frame),
    door,
    fn(next) {
      raw.successor_is_exact_v1(
        pre,
        witness,
        raw.replace_items_v1(
          frame,
          witness.work_witness_cbor,
          21,
          3,
          cbor.serialise(next.output_count)
            |> bytearray.concat(
                validation_merkle_v1.encode_frontier(next.output_peaks),
              )
            |> bytearray.concat(cbor.serialise(next.output_total_count)),
        ),
      )
    },
  )
}

pub fn stage_four_finish(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let frame = raw.open_frame_v1(pre, witness, 30, 4)
  let compact = decode_native_tx_compact_v1(frame.compact_cbor)
  let control = base_control(frame)
  let empty = compact.body.outputs_hash == native_tx_field_access_v1.empty_field_commitment
  and {
    if empty {
      control.output_cursor == 0 && control.output_count == 0 && control.output_total_count == 0 && control.output_peaks == []
    } else {
      control.output_total_count > 0 && control.output_count == control.output_total_count
    },
    control.output_cursor == 0,
    control.receive_scan.source_count == 0 && control.receive_scan.source_peaks == [] && control.receive_scan.receive_count == 0 && control.receive_scan.previous_hash == #"" && control.receive_scan.candidate_hash == #"" && control.receive_scan.descriptor_peaks == [],
    raw.successor_is_exact_v1(
      pre,
      witness,
      raw.replace_stage_v1(frame, witness.work_witness_cbor, 5),
    ),
  }
}

pub fn stage_six_begin(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  door: field_door.MachineFieldDoorV1,
  field_index: Int,
  item_index: Int,
  carriage: native_tx_field_access_v1.FieldCarriageV1,
) -> Bool {
  let frame = raw.open_frame_v1(pre, witness, 30, 6)
  let Pair(verified, ws) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    frame.compact_cbor,
    frame.witness_set_compact_cbor,
    frame.field_preimage_lengths_cbor,
  )
  middle.script_sources_begin_mint_policy_with_successor(
    pre,
    witness,
    base_control(frame),
    verified,
    ws,
    door,
    field_index,
    item_index,
    carriage,
    fn(next) {
      let next_cbor = raw.replace_items_v1(
        frame,
        witness.work_witness_cbor,
        28,
        1,
        encode_mint_fold_control(next.mint_fold),
      )
      raw.successor_is_exact_v1(
        pre,
        witness,
        raw.replace_items_v1(
          frame,
          next_cbor,
          18,
          2,
          cbor.serialise(next.purpose_count)
            |> bytearray.concat(
                validation_merkle_v1.encode_frontier(next.purpose_peaks),
              ),
        ),
      )
    },
  )
}

pub fn stage_six_asset(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Bool {
  let frame = raw.open_frame_v1(pre, witness, 30, 6)
  middle.script_sources_fold_mint_asset_with_successor(
    pre,
    witness,
    base_control(frame),
    chunk_proof,
    next_chunk_proof,
    fn(next) {
      raw.successor_is_exact_v1(
        pre,
        witness,
        raw.replace_items_v1(
          frame,
          witness.work_witness_cbor,
          28,
          1,
          encode_mint_fold_control(next.mint_fold),
        ),
      )
    },
  )
}

pub fn stage_six_finish(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let frame = raw.open_frame_v1(pre, witness, 30, 6)
  let control = base_control(frame)
  let fold = control.mint_fold
  let compact = decode_native_tx_compact_v1(frame.compact_cbor)
  let empty = fold.policy_count == -1 && compact.body.mint_hash == native_tx_field_access_v1.empty_field_commitment
  let next_fold = if empty {
    MintFoldControlV1 { ..fold, policy_count: 0 }
  } else {
    fold
  }
  let next_cbor = raw.replace_items_v1(
    frame,
    witness.work_witness_cbor,
    28,
    1,
    encode_mint_fold_control(next_fold),
  )
  let next_cbor = raw.replace_items_v1(frame, next_cbor, 20, 1, #"00")
  and {
    empty || fold.policy_count >= 0 && fold.policy_cursor == fold.policy_count && fold.active_policy == #"",
    raw.successor_is_exact_v1(
      pre,
      witness,
      raw.replace_stage_v1(frame, next_cbor, 7),
    ),
  }
}
