use aiken/builtin
use aiken/cbor
use aiken/primitive/bytearray
use midgard/blake2b_224_trace_v1
use midgard/bounded_item_v1
use midgard/fraud_proofs/native_tx/codec.{
  decode_definite_array_header_at, encode_definite_array_header,
  encode_definite_bytes,
}
use midgard/fraud_proofs/native_tx/compact.{
  decode_native_tx_witness_set_compact, native_tx_proof_commitment_v1,
  verify_native_tx_proof_source_v1,
}
use midgard/native_script_scan_v1
use midgard/native_tx_field_access_v1
use midgard/redeemer_item_proof_v1
use midgard/rejection_reason_v1.{reject_invalid_field_type}
use midgard/script_proof_v1
use midgard/validation_machine/machine_types.{
  InlineSourceHashControlV1, NoAuxiliaryWitness, RedeemerItemStepWitness,
  ScriptSourceHashBlockWitness, ScriptSourcesControlV1,
  TransactionFieldChunkWitness, TransactionRedeemerItemBeginWitness,
  ValidationAuxiliaryWitnessV1, ValidationOneStepWitnessV1,
}
use midgard/validation_machine/shared.{
  max_aggregate_field_preimage_bytes, max_tx_size_derived_collection_item_count,
  rejected_successor_is_exact,
}
use midgard/validation_machine/witness.{
  decode_frontier_peaks, empty_mint_fold_control,
  empty_observer_purpose_scan_control, empty_receive_purpose_scan_control,
  empty_resolution_schedule_hash, empty_script_discovery_control,
  encode_inline_source_hash_control_v1, encode_mint_fold_control,
  encode_observer_purpose_scan_control, exact_script_sources_control,
  initial_resolution_accumulator, inline_source_hash_control_from_cbor,
  script_sources_control_from_witness,
  script_sources_stage_zero_control_from_witness,
}
use midgard/validation_machine_field_door_v1 as field_door
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

fn script_sources_stage_zero_work_witness_has_canonical_shape(
  work_witness_cbor: ByteArray,
  pending_source_cbor: ByteArray,
) -> Bool {
  let Pair(offset, item_count) = decode_definite_array_header_at(
    work_witness_cbor,
    0,
  )
  let expected_item_count = if pending_source_cbor == #"" {
    30
  } else {
    31
  }
  and {
    offset == 2,
    item_count == expected_item_count,
    if pending_source_cbor == #"" {
      True
    } else {
      let encoded_pending = encode_definite_bytes(pending_source_cbor)
      let pending_offset = bytearray.length(work_witness_cbor) - bytearray.length(
        encoded_pending,
      )
      and {
        pending_offset >= offset,
        builtin.slice_bytearray(
          pending_offset,
          bytearray.length(encoded_pending),
          work_witness_cbor,
        ) == encoded_pending,
      }
    },
  }
}

pub fn script_sources_stage_zero_prefix_before_stage(
  control: ScriptSourcesControlV1,
  item_count: Int,
) -> ByteArray {
  encode_definite_array_header(item_count)
    |> bytearray.concat(encode_definite_bytes(control.compact_cbor))
    |> bytearray.concat(encode_definite_bytes(control.witness_set_compact_cbor))
    |> bytearray.concat(
        encode_definite_bytes(control.field_preimage_lengths_cbor),
      )
    |> bytearray.concat(encode_definite_bytes(control.context_cbor))
    |> bytearray.concat(cbor.serialise(control.resolved_input_count))
    |> bytearray.concat(
        encode_definite_bytes(control.resolved_inputs_accumulator),
      )
    |> bytearray.concat(cbor.serialise(control.signer_count))
    |> bytearray.concat(
        encode_definite_bytes(control.signer_frontier_commitment),
      )
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.resolved_item_peaks),
      )
}

fn script_sources_stage_zero_control_is_bound(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  let pending_is_bound = if control.pending_source_cbor == #"" {
    True
  } else {
    let pending = inline_source_hash_control_from_cbor(
      control.pending_source_cbor,
    )
    and {
      pending.source_index == control.source_count,
      pending.source_total_count == control.source_total_count,
    }
  }
  and {
    control.stage == 0,
    native_tx_proof_commitment_v1(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
    ) == pre.transaction_commitment,
    validation_trace_v1.hash_validation_context(control.context_cbor) == pre.validation_context_hash,
    control.resolved_input_count >= 0,
    bytearray.length(control.resolved_inputs_accumulator) == 32,
    control.signer_count >= 0,
    bytearray.length(control.signer_frontier_commitment) == 32,
    control.resolved_item_peaks == [],
    control.replay_cursor == 0,
    bytearray.length(control.replay_accumulator) == 32,
    bytearray.length(control.replay_remaining_schedule_hash) == 32,
    bytearray.length(control.resolution_schedule_hash) == 32,
    control.spend_index == 0,
    validation_merkle_v1.frontier_is_well_formed(
      control.source_count,
      control.source_peaks,
    ),
    control.source_total_count >= control.source_count,
    control.source_total_count <= max_tx_size_derived_collection_item_count,
    control.redeemer_count == 0,
    control.redeemer_peaks == [],
    control.redeemer_total_count == 0,
    control.purpose_count == 0,
    control.purpose_peaks == [],
    control.output_cursor == 0,
    control.output_count == 0,
    control.output_peaks == [],
    control.output_total_count == 0,
    control.receive_scan == empty_receive_purpose_scan_control(),
    control.observer_scan == empty_observer_purpose_scan_control(),
    control.mint_fold == empty_mint_fold_control(),
    control.discovery == empty_script_discovery_control(),
    control.output_proof == None,
    pending_is_bound,
    script_sources_stage_zero_work_witness_has_canonical_shape(
      witness.work_witness_cbor,
      control.pending_source_cbor,
    ),
  }
}

pub fn script_sources_stage_zero_successor_work_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  next_work_witness_cbor: ByteArray,
) -> Bool {
  and {
    witness.claimed_successor.phase == validation_trace_v1.ScriptSources,
    witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.ScriptSources,
      pre.program_counter + 1,
      next_work_witness_cbor,
    ),
  }
}

fn script_sources_stage_zero_finish_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  let prefix = script_sources_stage_zero_prefix_before_stage(control, 30)
  let stage_offset = bytearray.length(prefix)
  let work_length = bytearray.length(witness.work_witness_cbor)
  expect
    builtin.slice_bytearray(0, stage_offset, witness.work_witness_cbor) == prefix
  expect
    builtin.slice_bytearray(stage_offset, 1, witness.work_witness_cbor) == #"00"
  let next_work_witness_cbor = prefix
    |> bytearray.concat(#"01")
    |> bytearray.concat(
        builtin.slice_bytearray(
          stage_offset + 1,
          work_length - stage_offset - 1,
          witness.work_witness_cbor,
        ),
      )
  script_sources_stage_zero_successor_work_is_exact(
    pre,
    witness,
    next_work_witness_cbor,
  )
}

fn script_sources_stage_zero_begin_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
  source_total_count: Int,
  pending_source_cbor: ByteArray,
) -> Bool {
  let encoded_observer_scan = encode_observer_purpose_scan_control(
    control.observer_scan,
  )
  let old_suffix = cbor.serialise(control.source_total_count)
    |> bytearray.concat(cbor.serialise(control.redeemer_total_count))
    |> bytearray.concat(encoded_observer_scan)
    |> bytearray.concat(encode_mint_fold_control(control.mint_fold))
    |> bytearray.concat(encode_definite_bytes(control.resolution_schedule_hash))
  let work_length = bytearray.length(witness.work_witness_cbor)
  let suffix_offset = work_length - bytearray.length(old_suffix)
  expect suffix_offset >= 2
  expect
    builtin.slice_bytearray(
      suffix_offset,
      bytearray.length(old_suffix),
      witness.work_witness_cbor,
    ) == old_suffix
  let next_work_witness_cbor = encode_definite_array_header(31)
    |> bytearray.concat(
        builtin.slice_bytearray(2, suffix_offset - 2, witness.work_witness_cbor),
      )
    |> bytearray.concat(cbor.serialise(source_total_count))
    |> bytearray.concat(cbor.serialise(control.redeemer_total_count))
    |> bytearray.concat(encoded_observer_scan)
    |> bytearray.concat(encode_mint_fold_control(control.mint_fold))
    |> bytearray.concat(encode_definite_bytes(control.resolution_schedule_hash))
    |> bytearray.concat(encode_definite_bytes(pending_source_cbor))
  script_sources_stage_zero_successor_work_is_exact(
    pre,
    witness,
    next_work_witness_cbor,
  )
}

fn script_sources_stage_zero_pending_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
  pending_source_cbor: ByteArray,
) -> Bool {
  let old_pending = encode_definite_bytes(control.pending_source_cbor)
  let work_length = bytearray.length(witness.work_witness_cbor)
  let pending_offset = work_length - bytearray.length(old_pending)
  expect pending_offset >= 2
  expect
    builtin.slice_bytearray(
      pending_offset,
      bytearray.length(old_pending),
      witness.work_witness_cbor,
    ) == old_pending
  let next_work_witness_cbor = builtin.slice_bytearray(
    0,
    pending_offset,
    witness.work_witness_cbor,
  )
    |> bytearray.concat(encode_definite_bytes(pending_source_cbor))
  script_sources_stage_zero_successor_work_is_exact(
    pre,
    witness,
    next_work_witness_cbor,
  )
}

fn script_sources_stage_zero_terminal_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
  source_count: Int,
  source_peaks: List<validation_merkle_v1.FrontierPeak>,
) -> Bool {
  let prefix = script_sources_stage_zero_prefix_before_stage(control, 31)
    |> bytearray.concat(#"00")
  let source_offset = bytearray.length(prefix)
  let old_source_fields = cbor.serialise(control.source_count)
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.source_peaks),
      )
  let after_source_offset = source_offset + bytearray.length(old_source_fields)
  let old_pending = encode_definite_bytes(control.pending_source_cbor)
  let work_length = bytearray.length(witness.work_witness_cbor)
  let pending_offset = work_length - bytearray.length(old_pending)
  expect pending_offset >= after_source_offset
  expect
    builtin.slice_bytearray(0, source_offset, witness.work_witness_cbor) == prefix
  expect
    builtin.slice_bytearray(
      source_offset,
      bytearray.length(old_source_fields),
      witness.work_witness_cbor,
    ) == old_source_fields
  expect
    builtin.slice_bytearray(
      pending_offset,
      bytearray.length(old_pending),
      witness.work_witness_cbor,
    ) == old_pending
  let next_work_witness_cbor = encode_definite_array_header(30)
    |> bytearray.concat(builtin.slice_bytearray(2, source_offset - 2, prefix))
    |> bytearray.concat(cbor.serialise(source_count))
    |> bytearray.concat(validation_merkle_v1.encode_frontier(source_peaks))
    |> bytearray.concat(
        builtin.slice_bytearray(
          after_source_offset,
          pending_offset - after_source_offset,
          witness.work_witness_cbor,
        ),
      )
  script_sources_stage_zero_successor_work_is_exact(
    pre,
    witness,
    next_work_witness_cbor,
  )
}

pub fn script_sources_control_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  next_control: ScriptSourcesControlV1,
) -> Bool {
  and {
    witness.claimed_successor.phase == validation_trace_v1.ScriptSources,
    witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.ScriptSources,
      pre.program_counter + 1,
      exact_script_sources_control(next_control),
    ),
  }
}

pub fn inline_source_hash_block_v1(
  pending: InlineSourceHashControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Option<ByteArray> {
  let hash_control = pending.hash_control
  if hash_control.stage != blake2b_224_trace_v1.stage_ready {
    None
  } else {
    let remaining = hash_control.total_length - hash_control.cursor
    let block_length = if remaining < blake2b_224_trace_v1.block_bytes {
      remaining
    } else {
      blake2b_224_trace_v1.block_bytes
    }
    let content_length = if hash_control.cursor == 0 {
      block_length - 1
    } else {
      block_length
    }
    let item_cursor = if hash_control.cursor == 0 {
      pending.payload_offset
    } else {
      pending.payload_offset + hash_control.cursor - 1
    }
    let expected_chunk_index = if content_length == 0 {
      0
    } else {
      item_cursor / bounded_item_v1.chunk_bytes
    }
    let offset = if content_length == 0 {
      pending.payload_offset
    } else {
      item_cursor - expected_chunk_index * bounded_item_v1.chunk_bytes
    }
    if !and {
      hash_control.total_length == pending.payload_length + 1,
      chunk_proof.field_index == 6,
      chunk_proof.item_index == pending.source_index,
      chunk_proof.total_length == pending.item_length,
      chunk_proof.chunk_index == expected_chunk_index,
      bounded_item_v1.verify_chunk(pending.item_commitment, chunk_proof),
      offset >= 0,
      offset <= bytearray.length(chunk_proof.chunk),
    } {
      None
    } else {
      let available = bytearray.length(chunk_proof.chunk) - offset
      let crosses_chunk = content_length > available
      let authenticated_window = if crosses_chunk {
        when next_chunk_proof is {
          None -> None
          Some(next) ->
            if and {
              next.field_index == 6,
              next.item_index == pending.source_index,
              next.total_length == pending.item_length,
              next.chunk_index == expected_chunk_index + 1,
              bounded_item_v1.verify_chunk(pending.item_commitment, next),
            } {
              Some(bytearray.concat(chunk_proof.chunk, next.chunk))
            } else {
              None
            }
        }
      } else if next_chunk_proof == None {
        Some(chunk_proof.chunk)
      } else {
        None
      }
      when authenticated_window is {
        None -> None
        Some(window) -> {
          let content = builtin.slice_bytearray(offset, content_length, window)
          let block = if hash_control.cursor == 0 {
            #""
              |> bytearray.push(pending.language_tag)
              |> bytearray.concat(content)
          } else {
            content
          }
          if bytearray.length(block) == block_length {
            Some(block)
          } else {
            None
          }
        }
      }
    }
  }
}

pub fn verify_script_sources_stage_zero_finish_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let control = script_sources_stage_zero_control_from_witness(
    witness.work_witness_cbor,
  )
  let witness_set = decode_native_tx_witness_set_compact(
    control.witness_set_compact_cbor,
  )
  let script_commitment = witness_set.script_tx_wits_hash
  let empty_script_commitment = native_tx_field_access_v1.empty_field_commitment
  let source_scan_is_complete = if script_commitment == empty_script_commitment {
    and {
      control.source_count == 0,
      control.source_total_count == 0,
      control.source_peaks == [],
    }
  } else {
    and {
      control.source_total_count > 0,
      control.source_count == control.source_total_count,
    }
  }
  and {
    script_sources_stage_zero_control_is_bound(pre, witness, control),
    control.pending_source_cbor == #"",
    source_scan_is_complete,
    script_sources_stage_zero_finish_successor_is_exact(pre, witness, control),
  }
}

pub fn verify_script_sources_stage_zero_begin_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  door: field_door.MachineFieldDoorV1,
  field_index: Int,
  item_index: Int,
  carriage: native_tx_field_access_v1.FieldCarriageV1,
) -> Bool {
  let control = script_sources_stage_zero_control_from_witness(
    witness.work_witness_cbor,
  )
  // The door takes tx-id-verified compact structures, never a free-standing
  // field hash, so this entry point re-derives them from the control's own
  // bytes. `script_sources_stage_zero_control_is_bound` has already pinned those
  // bytes to `pre.transaction_commitment`; what the re-derivation adds is §3's
  // id, which is what the door authenticates a tier-3 certificate against.
  let Pair(verified, witness_set) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  let item = field_door.open_machine_field_item(
    door,
    verified,
    witness_set,
    6,
    control.source_count,
    carriage,
  )
  let item_count = field_door.machine_field_item_count(item)
  let item_length = field_door.machine_field_item_length(item)
  let active_total_count = if control.source_total_count == 0 {
    item_count
  } else {
    control.source_total_count
  }
  let header = native_script_scan_v1.versioned_script_header_v1(
    field_door.machine_field_item_chunk(item, 0),
    item_length,
  )
  and {
    script_sources_stage_zero_control_is_bound(pre, witness, control),
    control.pending_source_cbor == #"",
    active_total_count > 0,
    active_total_count <= max_tx_size_derived_collection_item_count,
    item_length > 0,
    item_length <= max_aggregate_field_preimage_bytes,
    field_index == 6,
    item_index == control.source_count,
    item_count == active_total_count,
    when header is {
      None ->
        rejected_successor_is_exact(
          pre,
          witness.claimed_successor,
          reject_invalid_field_type,
        )
      Some(item_header) -> {
        let pending_source_cbor = encode_inline_source_hash_control_v1(
          InlineSourceHashControlV1 {
            version: 1,
            source_index: control.source_count,
            source_total_count: active_total_count,
            language_tag: item_header.language_tag,
            payload_offset: item_header.payload_offset,
            payload_length: item_header.payload_length,
            item_length,
            item_commitment: field_door.machine_field_item_commitment(item),
            hash_control: blake2b_224_trace_v1.initial_control_v1(
              item_header.payload_length + 1,
            ),
          },
        )
        script_sources_stage_zero_begin_successor_is_exact(
          pre,
          witness,
          control,
          active_total_count,
          pending_source_cbor,
        )
      }
    },
  }
}

pub fn verify_script_sources_stage_zero_hash_block_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Bool {
  let control = script_sources_stage_zero_control_from_witness(
    witness.work_witness_cbor,
  )
  expect control.pending_source_cbor != #""
  let pending = inline_source_hash_control_from_cbor(
    control.pending_source_cbor,
  )
  when inline_source_hash_block_v1(pending, chunk_proof, next_chunk_proof) is {
    None -> False
    Some(block) -> {
      expect Some(next_hash_control) = blake2b_224_trace_v1.step_v1(
        pending.hash_control,
        Some(block),
      )
      and {
        script_sources_stage_zero_control_is_bound(pre, witness, control),
        script_sources_stage_zero_pending_successor_is_exact(
          pre,
          witness,
          control,
          encode_inline_source_hash_control_v1(
            InlineSourceHashControlV1 {
              ..pending,
              hash_control: next_hash_control,
            },
          ),
        ),
      }
    }
  }
}

pub fn verify_script_sources_stage_zero_hash_advance_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let control = script_sources_stage_zero_control_from_witness(
    witness.work_witness_cbor,
  )
  expect control.pending_source_cbor != #""
  let pending = inline_source_hash_control_from_cbor(
    control.pending_source_cbor,
  )
  expect pending.hash_control.stage != blake2b_224_trace_v1.stage_ready
  expect pending.hash_control.stage != blake2b_224_trace_v1.stage_terminal
  expect Some(next_hash_control) = blake2b_224_trace_v1.step_v1(
    pending.hash_control,
    None,
  )
  and {
    script_sources_stage_zero_control_is_bound(pre, witness, control),
    script_sources_stage_zero_pending_successor_is_exact(
      pre,
      witness,
      control,
      encode_inline_source_hash_control_v1(
        InlineSourceHashControlV1 { ..pending, hash_control: next_hash_control },
      ),
    ),
  }
}

pub fn verify_script_sources_stage_zero_hash_terminal_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let control = script_sources_stage_zero_control_from_witness(
    witness.work_witness_cbor,
  )
  expect control.pending_source_cbor != #""
  let pending = inline_source_hash_control_from_cbor(
    control.pending_source_cbor,
  )
  expect Some(script_hash) = blake2b_224_trace_v1.digest_v1(
    pending.hash_control,
  )
  let next_source_count = control.source_count + 1
  let next_source_peaks = validation_merkle_v1.append_leaf(
    control.source_count,
    control.source_peaks,
    script_proof_v1.inline_source_leaf_hash(
      control.source_count,
      pending.language_tag,
      script_hash,
      pending.item_length,
      pending.item_commitment,
    ),
  )
  and {
    script_sources_stage_zero_control_is_bound(pre, witness, control),
    script_sources_stage_zero_terminal_successor_is_exact(
      pre,
      witness,
      control,
      next_source_count,
      next_source_peaks,
    ),
  }
}

pub fn script_sources_stage_zero(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ScriptSourcesControlV1,
  door: field_door.MachineFieldDoorV1,
) -> Bool {
  let Pair(verified, witness_set) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  let script_commitment = witness_set.script_tx_wits_hash
  let empty_script_commitment = native_tx_field_access_v1.empty_field_commitment
  let common_control_is_initial = and {
    control.redeemer_count == 0,
    control.redeemer_total_count == 0,
    control.redeemer_peaks == [],
    control.resolved_item_peaks == [],
    control.replay_cursor == 0,
    control.spend_index == 0,
    control.purpose_count == 0,
    control.output_cursor == 0,
    control.output_count == 0,
    control.receive_scan == empty_receive_purpose_scan_control(),
  }
  if control.pending_source_cbor != #"" {
    let pending = inline_source_hash_control_from_cbor(
      control.pending_source_cbor,
    )
    let hash_control = pending.hash_control
    if hash_control.stage == blake2b_224_trace_v1.stage_ready {
      expect ScriptSourceHashBlockWitness { chunk_proof, next_chunk_proof } = auxiliary
      when
        inline_source_hash_block_v1(pending, chunk_proof, next_chunk_proof)
      is {
        None -> False
        Some(block) -> {
          expect Some(next_hash_control) = blake2b_224_trace_v1.step_v1(
            hash_control,
            Some(block),
          )
          and {
            common_control_is_initial,
            script_sources_control_successor_is_exact(
              pre,
              witness,
              ScriptSourcesControlV1 {
                ..control,
                pending_source_cbor: encode_inline_source_hash_control_v1(
                  InlineSourceHashControlV1 {
                    ..pending,
                    hash_control: next_hash_control,
                  },
                ),
              },
            ),
          }
        }
      }
    } else if hash_control.stage == blake2b_224_trace_v1.stage_terminal {
      expect Some(script_hash) = blake2b_224_trace_v1.digest_v1(hash_control)
      and {
        common_control_is_initial,
        auxiliary == NoAuxiliaryWitness,
        script_sources_control_successor_is_exact(
          pre,
          witness,
          ScriptSourcesControlV1 {
            ..control,
            source_count: control.source_count + 1,
            source_peaks: validation_merkle_v1.append_leaf(
              control.source_count,
              control.source_peaks,
              script_proof_v1.inline_source_leaf_hash(
                control.source_count,
                pending.language_tag,
                script_hash,
                pending.item_length,
                pending.item_commitment,
              ),
            ),
            pending_source_cbor: #"",
          },
        ),
      }
    } else {
      expect Some(next_hash_control) = blake2b_224_trace_v1.step_v1(
        hash_control,
        None,
      )
      and {
        common_control_is_initial,
        auxiliary == NoAuxiliaryWitness,
        script_sources_control_successor_is_exact(
          pre,
          witness,
          ScriptSourcesControlV1 {
            ..control,
            pending_source_cbor: encode_inline_source_hash_control_v1(
              InlineSourceHashControlV1 {
                ..pending,
                hash_control: next_hash_control,
              },
            ),
          },
        ),
      }
    }
  } else if script_commitment == empty_script_commitment {
    and {
      common_control_is_initial,
      auxiliary == NoAuxiliaryWitness,
      control.source_count == 0,
      control.source_total_count == 0,
      control.source_peaks == [],
      script_sources_control_successor_is_exact(
        pre,
        witness,
        ScriptSourcesControlV1 { ..control, stage: 1 },
      ),
    }
  } else if and {
    control.source_total_count > 0,
    control.source_count == control.source_total_count,
  } {
    and {
      common_control_is_initial,
      auxiliary == NoAuxiliaryWitness,
      script_sources_control_successor_is_exact(
        pre,
        witness,
        ScriptSourcesControlV1 { ..control, stage: 1 },
      ),
    }
  } else {
    expect TransactionFieldChunkWitness { field_index, item_index, carriage } = auxiliary
    let item = field_door.open_machine_field_item(
      door,
      verified,
      witness_set,
      6,
      control.source_count,
      carriage,
    )
    let item_count = field_door.machine_field_item_count(item)
    let item_length = field_door.machine_field_item_length(item)
    let active_total_count = if control.source_total_count == 0 {
      item_count
    } else {
      control.source_total_count
    }
    let header = native_script_scan_v1.versioned_script_header_v1(
      field_door.machine_field_item_chunk(item, 0),
      item_length,
    )
    and {
      common_control_is_initial,
      active_total_count > 0,
      active_total_count <= max_tx_size_derived_collection_item_count,
      item_length > 0,
      item_length <= max_aggregate_field_preimage_bytes,
      field_index == 6,
      item_index == control.source_count,
      item_count == active_total_count,
      when header is {
        None ->
          rejected_successor_is_exact(
            pre,
            witness.claimed_successor,
            reject_invalid_field_type,
          )
        Some(item_header) ->
          script_sources_control_successor_is_exact(
            pre,
            witness,
            ScriptSourcesControlV1 {
              ..control,
              source_total_count: active_total_count,
              pending_source_cbor: encode_inline_source_hash_control_v1(
                InlineSourceHashControlV1 {
                  version: 1,
                  source_index: control.source_count,
                  source_total_count: active_total_count,
                  language_tag: item_header.language_tag,
                  payload_offset: item_header.payload_offset,
                  payload_length: item_header.payload_length,
                  item_length,
                  item_commitment: field_door.machine_field_item_commitment(
                    item,
                  ),
                  hash_control: blake2b_224_trace_v1.initial_control_v1(
                    item_header.payload_length + 1,
                  ),
                },
              ),
            },
          )
      },
    }
  }
}

fn script_sources_stage_one_raw_common_control_is_initial(
  resolved_item_peaks_data: Data,
  source_count_data: Data,
  replay_cursor_data: Data,
  spend_index_data: Data,
  purpose_count_data: Data,
  output_cursor_data: Data,
  output_count_data: Data,
  receive_scan_data: Data,
  source_total_count_data: Data,
) -> Bool {
  expect
      [
        receive_source_count_data, receive_source_peaks_data, receive_count_data,
        receive_previous_hash_data, receive_candidate_hash_data,
        receive_descriptor_peaks_data,
      ]
    = builtin.un_list_data(receive_scan_data)
  and {
    builtin.un_i_data(source_total_count_data) == builtin.un_i_data(
      source_count_data,
    ),
    builtin.un_list_data(resolved_item_peaks_data) == [],
    builtin.un_i_data(replay_cursor_data) == 0,
    builtin.un_i_data(spend_index_data) == 0,
    builtin.un_i_data(purpose_count_data) == 0,
    builtin.un_i_data(output_cursor_data) == 0,
    builtin.un_i_data(output_count_data) == 0,
    builtin.un_i_data(receive_source_count_data) == 0,
    builtin.un_list_data(receive_source_peaks_data) == [],
    builtin.un_i_data(receive_count_data) == 0,
    builtin.un_b_data(receive_previous_hash_data) == #"",
    builtin.un_b_data(receive_candidate_hash_data) == #"",
    builtin.un_list_data(receive_descriptor_peaks_data) == [],
  }
}

pub fn verify_script_sources_stage_one_finish_raw_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  expect Some(control_data) = cbor.deserialise(witness.work_witness_cbor)
  expect
      [
        compact_cbor_data, witness_set_compact_cbor_data,
        field_preimage_lengths_cbor_data, _, _, _, _, _,
        resolved_item_peaks_data, stage_data, source_count_data, _,
        redeemer_count_data, redeemer_peaks_data, replay_cursor_data, _, _,
        spend_index_data, purpose_count_data, _, output_cursor_data,
        output_count_data, _, _, receive_scan_data, source_total_count_data,
        redeemer_total_count_data, _, _, resolution_schedule_hash_data,
      ]
    = builtin.un_list_data(control_data)
  let control = script_sources_control_from_witness(witness.work_witness_cbor)
  let compact_cbor = builtin.un_b_data(compact_cbor_data)
  let witness_set_compact_cbor = builtin.un_b_data(
    witness_set_compact_cbor_data,
  )
  let field_preimage_lengths_cbor = builtin.un_b_data(
    field_preimage_lengths_cbor_data,
  )
  let witness_set = decode_native_tx_witness_set_compact(
    witness_set_compact_cbor,
  )
  let redeemer_count = builtin.un_i_data(redeemer_count_data)
  let redeemer_peaks = decode_frontier_peaks(redeemer_peaks_data)
  let redeemer_total_count = builtin.un_i_data(redeemer_total_count_data)
  let redeemer_commitment = witness_set.redeemer_tx_wits_hash
  let empty_redeemer_commitment = native_tx_field_access_v1.empty_field_commitment
  let scan_is_complete = if redeemer_commitment == empty_redeemer_commitment {
    and {
      redeemer_count == 0,
      redeemer_total_count == 0,
      redeemer_peaks == [],
    }
  } else {
    and {
      redeemer_total_count > 0,
      redeemer_count == redeemer_total_count,
      validation_merkle_v1.frontier_is_well_formed(
        redeemer_count,
        redeemer_peaks,
      ),
    }
  }
  // The witness slice this prefix is compared against is the *canonical*
  // ScriptSources encoding (`encode_script_sources_witness:1062`), which uses
  // definite-length headers throughout. `cbor.serialise` on the decoded `Data`
  // items is Plutus `serialiseData`, which chunks any bytestring longer than 64
  // bytes into an indefinite `5f..ff` run and would never match. Rebuild the
  // prefix from the canonical encoder instead; the control is proven canonical
  // by the `exact_script_sources_control` conjunct below.
  let stage_prefix = script_sources_stage_zero_prefix_before_stage(control, 30)
  let stage_offset = bytearray.length(stage_prefix)
  let next_work_witness_cbor = stage_prefix
    |> bytearray.concat(#"02")
    |> bytearray.concat(
        builtin.slice_bytearray(
          stage_offset + 1,
          bytearray.length(witness.work_witness_cbor) - stage_offset - 1,
          witness.work_witness_cbor,
        ),
      )
  and {
    exact_script_sources_control(control) == witness.work_witness_cbor,
    builtin.un_i_data(stage_data) == 1,
    native_tx_proof_commitment_v1(
      compact_cbor,
      witness_set_compact_cbor,
      field_preimage_lengths_cbor,
    ) == pre.transaction_commitment,
    script_sources_stage_one_raw_common_control_is_initial(
      resolved_item_peaks_data,
      source_count_data,
      replay_cursor_data,
      spend_index_data,
      purpose_count_data,
      output_cursor_data,
      output_count_data,
      receive_scan_data,
      source_total_count_data,
    ),
    bytearray.length(redeemer_commitment) == 32,
    bytearray.length(builtin.un_b_data(resolution_schedule_hash_data)) == 32,
    redeemer_total_count <= max_tx_size_derived_collection_item_count,
    scan_is_complete,
    builtin.slice_bytearray(0, stage_offset, witness.work_witness_cbor) == stage_prefix,
    builtin.slice_bytearray(stage_offset, 1, witness.work_witness_cbor) == #"01",
    script_sources_stage_zero_successor_work_is_exact(
      pre,
      witness,
      next_work_witness_cbor,
    ),
  }
}

pub fn script_sources_stage_one(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ScriptSourcesControlV1,
  door: field_door.MachineFieldDoorV1,
) -> Bool {
  let Pair(verified, witness_set) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  let redeemer_commitment = witness_set.redeemer_tx_wits_hash
  let empty_redeemer_commitment = native_tx_field_access_v1.empty_field_commitment
  let common_control_is_initial = and {
    control.source_total_count == control.source_count,
    control.resolved_item_peaks == [],
    control.replay_cursor == 0,
    control.spend_index == 0,
    control.purpose_count == 0,
    control.output_cursor == 0,
    control.output_count == 0,
    control.receive_scan == empty_receive_purpose_scan_control(),
  }
  if redeemer_commitment == empty_redeemer_commitment {
    and {
      common_control_is_initial,
      auxiliary == NoAuxiliaryWitness,
      control.pending_source_cbor == #"",
      control.redeemer_count == 0,
      control.redeemer_total_count == 0,
      control.redeemer_peaks == [],
      script_sources_control_successor_is_exact(
        pre,
        witness,
        ScriptSourcesControlV1 { ..control, stage: 2 },
      ),
    }
  } else if and {
    control.pending_source_cbor == #"",
    control.redeemer_total_count > 0,
    control.redeemer_count == control.redeemer_total_count,
  } {
    and {
      common_control_is_initial,
      auxiliary == NoAuxiliaryWitness,
      script_sources_control_successor_is_exact(
        pre,
        witness,
        ScriptSourcesControlV1 { ..control, stage: 2 },
      ),
    }
  } else if control.pending_source_cbor == #"" {
    expect TransactionRedeemerItemBeginWitness { carriage } = auxiliary
    let item = field_door.open_machine_field_item(
      door,
      verified,
      witness_set,
      8,
      control.redeemer_count,
      carriage,
    )
    let item_count = field_door.machine_field_item_count(item)
    let active_total_count = if control.redeemer_total_count == 0 {
      item_count
    } else {
      control.redeemer_total_count
    }
    let item_control = redeemer_item_proof_v1.initial_control_v1(
      redeemer_item_proof_v1.mode_data,
      control.redeemer_count,
      active_total_count,
      field_door.machine_field_item_length(item),
      field_door.machine_field_item_commitment(item),
      -1,
      -1,
    )
    and {
      common_control_is_initial,
      active_total_count > 0,
      active_total_count <= max_tx_size_derived_collection_item_count,
      item_count == active_total_count,
      script_sources_control_successor_is_exact(
        pre,
        witness,
        ScriptSourcesControlV1 {
          ..control,
          redeemer_total_count: active_total_count,
          pending_source_cbor: redeemer_item_proof_v1.hash_control_v1(
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
      common_control_is_initial,
      redeemer_item_proof_v1.control_is_well_formed(item_control),
      item_control.mode == redeemer_item_proof_v1.mode_data,
      item_control.item_index == control.redeemer_count,
      item_control.item_count == control.redeemer_total_count,
      redeemer_item_proof_v1.hash_control_v1(item_control) == control.pending_source_cbor,
    }
    when redeemer_item_proof_v1.step_v1(item_control, item_witness) is {
      None -> False
      Some(redeemer_item_proof_v1.RedeemerItemProofInvalid) -> and {
          current_matches,
          rejected_successor_is_exact(
            pre,
            witness.claimed_successor,
            reject_invalid_field_type,
          ),
        }
      Some(redeemer_item_proof_v1.RedeemerItemProofAdvanced { control: next }) ->
        if next.stage == redeemer_item_proof_v1.stage_terminal {
          and {
            current_matches,
            redeemer_item_proof_v1.finalize_v1(next) != None,
            script_sources_control_successor_is_exact(
              pre,
              witness,
              ScriptSourcesControlV1 {
                ..control,
                redeemer_count: control.redeemer_count + 1,
                redeemer_peaks: validation_merkle_v1.append_leaf(
                  control.redeemer_count,
                  control.redeemer_peaks,
                  script_proof_v1.redeemer_item_leaf_hash(
                    control.redeemer_count,
                    next.item_commitment,
                  ),
                ),
                pending_source_cbor: #"",
              },
            ),
          }
        } else {
          and {
            current_matches,
            script_sources_control_successor_is_exact(
              pre,
              witness,
              ScriptSourcesControlV1 {
                ..control,
                pending_source_cbor: redeemer_item_proof_v1.hash_control_v1(
                  next,
                ),
              },
            ),
          }
        }
    }
  }
}

pub fn script_sources_stage_two(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  and {
    auxiliary == NoAuxiliaryWitness,
    control.resolved_input_count > 0,
    control.replay_cursor == 0,
    control.resolved_item_peaks == [],
    control.replay_accumulator == initial_resolution_accumulator(),
    control.replay_remaining_schedule_hash == empty_resolution_schedule_hash(),
    control.spend_index == 0,
    control.purpose_count == 0,
    control.purpose_peaks == [],
    control.resolution_schedule_hash != empty_resolution_schedule_hash(),
    script_sources_control_successor_is_exact(
      pre,
      witness,
      ScriptSourcesControlV1 {
        ..control,
        stage: 3,
        replay_remaining_schedule_hash: control.resolution_schedule_hash,
      },
    ),
  }
}
