use aiken/builtin
use aiken/cbor
use aiken/primitive/bytearray
use midgard/bounded_item_v1
use midgard/cek_constant_v1
use midgard/cek_data_v1
use midgard/cek_machine_v1
use midgard/cek_proof_v1
use midgard/fraud_proofs/native_tx/compact.{
  decode_native_tx_compact_v1, verify_native_tx_proof_source_v1,
}
use midgard/native_script_scan_v1
use midgard/native_tx_field_access_v1
use midgard/redeemer_item_proof_v1
use midgard/rejection_reason_v1.{reject_plutus_script_invalid}
use midgard/script_context_v1
use midgard/script_proof_v1
use midgard/validation_machine/machine_types.{
  CekContextAssembleWitness, CekContextControlV1, CekContextFinalizeSpendWitness,
  CekContextFinalizeWitness, CekContextPartsControlV1, CekContextSeedWitness,
  CekCoreStepWitness, CekFinalContextControlV1, CekMintContextItemWitness,
  CekOutputContextItemWitness, CekRedeemerContextControlV1,
  CekRedeemerContextSelectWitness, CekResolvedContextItemWitness,
  CekSignerContextItemWitness, CekTxInfoAssemblyControlV1,
  CekTxInfoFinalizeWitness, CekWitnessControlV1, NativeExecutionScanWitness,
  NativeScriptsControlV1, NoAuxiliaryWitness, RedeemerItemStepWitness,
  RedeemerScanBeginWitness, TransactionFieldChunkWitness,
  ValidationAuxiliaryWitnessV1, ValidationOneStepWitnessV1,
}
use midgard/validation_machine/shared.{
  cek_machine_state_hash, finalize_cek_observer_items_v1,
  max_aggregate_field_preimage_bytes, max_tx_size_derived_collection_item_count,
  mint_asset_leaf_hash, prepend_cek_observer_item_v1, prepend_mint_asset_summary,
  purpose_kind_to_redeemer_tag, rejected_successor_is_exact,
}
use midgard/validation_machine/witness.{
  cek_context_control_from_cbor, cek_context_control_is_well_formed,
  cek_control_is_context_step_v1, cek_control_is_core_step_v1,
  cek_control_is_execution_selection_v1, cek_control_is_finish_v1,
  cek_redeemer_context_control_is_well_formed, cek_witness_control_v1,
  cek_witness_is_well_formed_v1, empty_data_summary_v1, empty_observer_items_v1,
  encode_cek_context_control_v1, encode_cek_witness_v1,
  encode_value_and_mint_witness_v1, hash_cek_context_parts_control_v1,
  hash_cek_final_context_control_v1, hash_cek_redeemer_context_control_v1,
  hash_cek_tx_info_assembly_control_v1, initial_cek_redeemer_context_control_v1,
  observer_context_is_pristine,
}
use midgard/validation_machine_field_door_v1 as field_door
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

fn verify_cek_core_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: NativeScriptsControlV1,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
  active_state_hash: ByteArray,
  execution_cpu_limit: Int,
  execution_memory_limit: Int,
  program_envelope_hash: ByteArray,
) -> Bool {
  when auxiliary is {
    CekCoreStepWitness { step } -> {
      let next_cpu = completed_cpu + step.post.cpu
      let next_memory = completed_memory + step.post.memory
      let budget_exceeded = or {
        step.post.cpu > execution_cpu_limit,
        step.post.memory > execution_memory_limit,
      }
      and {
        bytearray.length(active_state_hash) == 32,
        bytearray.length(program_envelope_hash) == 32,
        execution_cursor >= 0,
        execution_cursor < control.execution_count,
        execution_cpu_limit > 0,
        execution_memory_limit > 0,
        step.pre.execution_index == execution_cursor,
        cek_machine_state_hash(step.pre) == active_state_hash,
        pre.execution_cpu == completed_cpu + step.pre.cpu,
        pre.execution_memory == completed_memory + step.pre.memory,
        cek_machine_v1.verify_core_step_v1(step.pre, step.post, step.witness),
        if or {
          budget_exceeded,
          step.post.mode == cek_machine_v1.mode_halt_error,
        } {
          and {
            witness.claimed_successor.execution_cpu == next_cpu,
            witness.claimed_successor.execution_memory == next_memory,
            rejected_successor_is_exact(
              pre,
              witness.claimed_successor,
              reject_plutus_script_invalid,
            ),
          }
        } else if step.post.mode == cek_machine_v1.mode_halt_success {
          let next_cursor = execution_cursor + 1
          and {
            step.post.cpu <= execution_cpu_limit,
            step.post.memory <= execution_memory_limit,
            witness.claimed_successor.execution_cpu == next_cpu,
            witness.claimed_successor.execution_memory == next_memory,
            if next_cursor == control.execution_count {
              and {
                witness.claimed_successor.phase == validation_trace_v1.ValueAndMint,
                witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
                  validation_trace_v1.ValueAndMint,
                  pre.program_counter + 1,
                  encode_value_and_mint_witness_v1(control),
                ),
              }
            } else {
              and {
                witness.claimed_successor.phase == validation_trace_v1.Cek,
                witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
                  validation_trace_v1.Cek,
                  pre.program_counter + 1,
                  encode_cek_witness_v1(
                    control,
                    #"",
                    next_cursor,
                    next_cpu,
                    next_memory,
                    #"",
                    0,
                    0,
                    #"",
                  ),
                ),
              }
            },
          }
        } else {
          and {
            witness.claimed_successor.phase == validation_trace_v1.Cek,
            witness.claimed_successor.execution_cpu == next_cpu,
            witness.claimed_successor.execution_memory == next_memory,
            witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
              validation_trace_v1.Cek,
              pre.program_counter + 1,
              encode_cek_witness_v1(
                control,
                #"",
                execution_cursor,
                completed_cpu,
                completed_memory,
                cek_machine_state_hash(step.post),
                execution_cpu_limit,
                execution_memory_limit,
                program_envelope_hash,
              ),
            ),
          }
        },
      }
    }
    _ -> False
  }
}

fn cek_context_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  native_control: NativeScriptsControlV1,
  context_control: CekContextControlV1,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
) -> Bool {
  and {
    witness.claimed_successor.phase == validation_trace_v1.Cek,
    witness.claimed_successor.execution_cpu == pre.execution_cpu,
    witness.claimed_successor.execution_memory == pre.execution_memory,
    witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.Cek,
      pre.program_counter + 1,
      encode_cek_witness_v1(
        native_control,
        encode_cek_context_control_v1(context_control),
        execution_cursor,
        completed_cpu,
        completed_memory,
        #"",
        0,
        0,
        context_control.program_envelope_hash,
      ),
    ),
  }
}

fn cek_selection_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: NativeScriptsControlV1,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
  language_tag: Int,
  purpose_kind: Int,
  purpose_index: Int,
  script_hash: ByteArray,
  subject: ByteArray,
  redeemer_leaf: ByteArray,
  program_term_root: ByteArray,
  program_envelope_hash: ByteArray,
) -> Bool {
  if and {
    language_tag == 3,
    purpose_kind == 3,
  } {
    rejected_successor_is_exact(
      pre,
      witness.claimed_successor,
      reject_plutus_script_invalid,
    )
  } else if language_tag == 0 {
    let next_cursor = execution_cursor + 1
    if next_cursor == control.execution_count {
      and {
        witness.claimed_successor.phase == validation_trace_v1.ValueAndMint,
        witness.claimed_successor.execution_cpu == pre.execution_cpu,
        witness.claimed_successor.execution_memory == pre.execution_memory,
        witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
          validation_trace_v1.ValueAndMint,
          pre.program_counter + 1,
          encode_value_and_mint_witness_v1(control),
        ),
      }
    } else {
      and {
        witness.claimed_successor.phase == validation_trace_v1.Cek,
        witness.claimed_successor.execution_cpu == pre.execution_cpu,
        witness.claimed_successor.execution_memory == pre.execution_memory,
        witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
          validation_trace_v1.Cek,
          pre.program_counter + 1,
          encode_cek_witness_v1(
            control,
            #"",
            next_cursor,
            completed_cpu,
            completed_memory,
            #"",
            0,
            0,
            #"",
          ),
        ),
      }
    }
  } else {
    cek_context_successor_is_exact(
      pre,
      witness,
      control,
      CekContextControlV1 {
        stage: 0,
        language_tag,
        program_term_root,
        program_envelope_hash,
        purpose_kind,
        purpose_index,
        script_hash,
        subject,
        redeemer_leaf,
        redeemer_context_control_hash: #"",
        execution_memory_limit: 0,
        execution_cpu_limit: 0,
        reference_items: cek_data_v1.empty_data_list_summary_v1(),
        spend_items: cek_data_v1.empty_data_list_summary_v1(),
        output_items: cek_data_v1.empty_data_list_summary_v1(),
        signer_items: cek_data_v1.empty_data_list_summary_v1(),
        observer_count: 0,
        observer_items: empty_observer_items_v1(language_tag),
        previous_observer: #"",
        observer_summary: empty_data_summary_v1(),
        mint_cursor: 0,
        current_mint_policy: #"",
        current_mint_assets: cek_data_v1.empty_data_pair_summary_v1(),
        mint_policies: cek_data_v1.empty_data_pair_summary_v1(),
        mint_summary: empty_data_summary_v1(),
      },
      execution_cursor,
      completed_cpu,
      completed_memory,
    )
  }
}

fn first_source_chunk_identity_matches(
  origin_kind: Int,
  source_key: ByteArray,
  proof: bounded_item_v1.ChunkProofV1,
) -> Bool {
  if origin_kind == 0 {
    expect Some(source_index_data) = cbor.deserialise(source_key)
    let source_index = builtin.un_i_data(source_index_data)
    and {
      source_index >= 0,
      cbor.serialise(source_index) == source_key,
      proof.field_index == 6,
      proof.item_index == source_index,
    }
  } else if origin_kind == 1 {
    expect Some(source_key_data) = cbor.deserialise(source_key)
    expect [_, output_index_data] = builtin.un_list_data(source_key_data)
    let output_index = builtin.un_i_data(output_index_data)
    and {
      output_index >= 0,
      output_index <= 65535,
      proof.field_index == 2,
      proof.item_index == output_index,
    }
  } else {
    False
  }
}

pub fn verify_cek_execution_selection(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: NativeScriptsControlV1,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
) -> Bool {
  expect
      NativeExecutionScanWitness {
        execution_index,
        language_tag,
        purpose_kind,
        purpose_index,
        script_hash,
        subject,
        purpose_siblings,
        source_index,
        origin_kind,
        source_key,
        script_total_length,
        script_item_commitment,
        source_siblings,
        redeemer_leaf,
        execution_siblings,
        first_chunk_proof,
      }
    = auxiliary
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(
    purpose_kind,
    purpose_index,
    script_hash,
    subject,
  )
  let source_leaf = script_proof_v1.source_descriptor_leaf_hash(
    origin_kind,
    source_key,
    language_tag,
    script_hash,
    script_total_length,
    script_item_commitment,
  )
  let execution_leaf = script_proof_v1.execution_leaf_hash(
    language_tag,
    purpose_leaf,
    source_leaf,
    redeemer_leaf,
  )
  expect Some(header) = native_script_scan_v1.versioned_script_header_v1(
    first_chunk_proof.chunk,
    script_total_length,
  )
  if and {
    execution_index == execution_cursor,
    execution_cursor < control.execution_count,
    control.execution_count == control.purpose_count,
    script_total_length > 0,
    script_total_length <= max_aggregate_field_preimage_bytes,
    bytearray.length(script_item_commitment) == 32,
    first_chunk_proof.chunk_index == 0,
    first_chunk_proof.total_length == script_total_length,
    first_source_chunk_identity_matches(
      origin_kind,
      source_key,
      first_chunk_proof,
    ),
    bounded_item_v1.verify_chunk(script_item_commitment, first_chunk_proof),
    header.language_tag == language_tag,
    validation_merkle_v1.verify_membership(
      control.purpose_count,
      control.purpose_peaks,
      execution_index,
      purpose_leaf,
      purpose_siblings,
    ),
    validation_merkle_v1.verify_membership(
      control.source_count,
      control.source_peaks,
      source_index,
      source_leaf,
      source_siblings,
    ),
    validation_merkle_v1.verify_membership(
      control.execution_count,
      control.execution_peaks,
      execution_index,
      execution_leaf,
      execution_siblings,
    ),
  } {
    if language_tag == 0 {
      and {
        redeemer_leaf == #"",
        cek_selection_successor_is_exact(
          pre,
          witness,
          control,
          execution_cursor,
          completed_cpu,
          completed_memory,
          language_tag,
          purpose_kind,
          purpose_index,
          script_hash,
          subject,
          redeemer_leaf,
          #"",
          #"",
        ),
      }
    } else if and {
      or {
        language_tag == 3,
        language_tag == 128,
      },
      bytearray.length(redeemer_leaf) == 32,
      header.payload_length <= cek_proof_v1.max_program_envelope_cbor_bytes,
      header.payload_offset + header.payload_length <= bytearray.length(
        first_chunk_proof.chunk,
      ),
    } {
      let envelope_cbor = builtin.slice_bytearray(
        header.payload_offset,
        header.payload_length,
        first_chunk_proof.chunk,
      )
      when cek_proof_v1.inspect_program_envelope_v1(envelope_cbor) is {
        None -> False
        Some(envelope) ->
          cek_selection_successor_is_exact(
            pre,
            witness,
            control,
            execution_cursor,
            completed_cpu,
            completed_memory,
            language_tag,
            purpose_kind,
            purpose_index,
            script_hash,
            subject,
            redeemer_leaf,
            envelope.term_root,
            cek_proof_v1.hash_program_envelope_v1(
              1,
              1,
              0,
              envelope.term_root,
              envelope.node_count,
              envelope.material_byte_length,
            ),
          )
      }
    } else {
      False
    }
  } else {
    False
  }
}

fn verify_cek_redeemer_selection(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  native_control: NativeScriptsControlV1,
  context_control: CekContextControlV1,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
) -> Bool {
  if context_control.redeemer_context_control_hash == #"" {
    expect
        RedeemerScanBeginWitness {
          item_index,
          item_count,
          total_length,
          item_commitment,
          siblings,
        }
      = auxiliary
    let leaf = script_proof_v1.redeemer_item_leaf_hash(
      item_index,
      item_commitment,
    )
    let item_control = redeemer_item_proof_v1.initial_control_v1(
      redeemer_item_proof_v1.mode_descriptor,
      item_index,
      item_count,
      total_length,
      item_commitment,
      purpose_kind_to_redeemer_tag(context_control.purpose_kind),
      context_control.purpose_index,
    )
    and {
      context_control.stage == 0,
      item_count == native_control.redeemer_count,
      leaf == context_control.redeemer_leaf,
      validation_merkle_v1.verify_membership(
        native_control.redeemer_count,
        native_control.redeemer_peaks,
        item_index,
        leaf,
        siblings,
      ),
      cek_context_successor_is_exact(
        pre,
        witness,
        native_control,
        CekContextControlV1 {
          ..context_control,
          redeemer_context_control_hash: redeemer_item_proof_v1.hash_control_v1(
            item_control,
          ),
        },
        execution_cursor,
        completed_cpu,
        completed_memory,
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
    let leaf = script_proof_v1.redeemer_item_leaf_hash(
      item_control.item_index,
      item_control.item_commitment,
    )
    let current_matches = and {
      context_control.stage == 0,
      item_control.mode == redeemer_item_proof_v1.mode_descriptor,
      item_control.item_count == native_control.redeemer_count,
      item_control.expected_purpose_tag == purpose_kind_to_redeemer_tag(
        context_control.purpose_kind,
      ),
      item_control.expected_pointer_index == context_control.purpose_index,
      leaf == context_control.redeemer_leaf,
      redeemer_item_proof_v1.hash_control_v1(item_control) == context_control.redeemer_context_control_hash,
    }
    when redeemer_item_proof_v1.step_v1(item_control, item_witness) is {
      None -> False
      Some(redeemer_item_proof_v1.RedeemerItemProofInvalid) -> False
      Some(redeemer_item_proof_v1.RedeemerItemProofAdvanced { control: next }) ->
        if next.stage == redeemer_item_proof_v1.stage_terminal {
          let redeemer_context = initial_cek_redeemer_context_control_v1()
          and {
            current_matches,
            cek_context_successor_is_exact(
              pre,
              witness,
              native_control,
              CekContextControlV1 {
                ..context_control,
                stage: 1,
                redeemer_context_control_hash: hash_cek_redeemer_context_control_v1(
                  redeemer_context,
                ),
                execution_memory_limit: next.execution_memory,
                execution_cpu_limit: next.execution_steps,
              },
              execution_cursor,
              completed_cpu,
              completed_memory,
            ),
          }
        } else {
          and {
            current_matches,
            cek_context_successor_is_exact(
              pre,
              witness,
              native_control,
              CekContextControlV1 {
                ..context_control,
                redeemer_context_control_hash: redeemer_item_proof_v1.hash_control_v1(
                  next,
                ),
              },
              execution_cursor,
              completed_cpu,
              completed_memory,
            ),
          }
        }
    }
  }
}

pub fn verify_cek_reference_context_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  native_control: NativeScriptsControlV1,
  context_control: CekContextControlV1,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
) -> Bool {
  let reference_count = native_control.resolved_input_count - native_control.spend_input_count
  if context_control.reference_items.length == reference_count {
    and {
      auxiliary == NoAuxiliaryWitness,
      cek_context_successor_is_exact(
        pre,
        witness,
        native_control,
        CekContextControlV1 { ..context_control, stage: 2 },
        execution_cursor,
        completed_cpu,
        completed_memory,
      ),
    }
  } else {
    expect
        CekResolvedContextItemWitness {
          source_kind,
          item_index,
          key,
          descriptor_cbor,
          siblings,
        }
      = auxiliary
    expect Some(next_items) = script_context_v1.prepend_resolved_descriptor_tx_in_info_v1(
      native_control.resolved_input_count,
      native_control.resolved_item_peaks,
      native_control.spend_input_count,
      source_kind,
      item_index,
      key,
      descriptor_cbor,
      siblings,
      context_control.language_tag == 128,
      context_control.reference_items,
    )
    and {
      source_kind == 1,
      cek_context_successor_is_exact(
        pre,
        witness,
        native_control,
        CekContextControlV1 { ..context_control, reference_items: next_items },
        execution_cursor,
        completed_cpu,
        completed_memory,
      ),
    }
  }
}

pub fn verify_cek_spend_context_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  native_control: NativeScriptsControlV1,
  context_control: CekContextControlV1,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
) -> Bool {
  if context_control.spend_items.length == native_control.spend_input_count {
    and {
      auxiliary == NoAuxiliaryWitness,
      cek_context_successor_is_exact(
        pre,
        witness,
        native_control,
        CekContextControlV1 { ..context_control, stage: 3 },
        execution_cursor,
        completed_cpu,
        completed_memory,
      ),
    }
  } else {
    expect
        CekResolvedContextItemWitness {
          source_kind,
          item_index,
          key,
          descriptor_cbor,
          siblings,
        }
      = auxiliary
    expect Some(next_items) = script_context_v1.prepend_resolved_descriptor_tx_in_info_v1(
      native_control.resolved_input_count,
      native_control.resolved_item_peaks,
      native_control.spend_input_count,
      source_kind,
      item_index,
      key,
      descriptor_cbor,
      siblings,
      context_control.language_tag == 128,
      context_control.spend_items,
    )
    and {
      source_kind == 0,
      cek_context_successor_is_exact(
        pre,
        witness,
        native_control,
        CekContextControlV1 { ..context_control, spend_items: next_items },
        execution_cursor,
        completed_cpu,
        completed_memory,
      ),
    }
  }
}

pub fn verify_cek_output_context_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  native_control: NativeScriptsControlV1,
  context_control: CekContextControlV1,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
) -> Bool {
  if context_control.output_items.length == native_control.output_count {
    and {
      auxiliary == NoAuxiliaryWitness,
      cek_context_successor_is_exact(
        pre,
        witness,
        native_control,
        CekContextControlV1 { ..context_control, stage: 4 },
        execution_cursor,
        completed_cpu,
        completed_memory,
      ),
    }
  } else {
    expect
        CekOutputContextItemWitness { output_index, descriptor_cbor, siblings }
      = auxiliary
    expect Some(next_items) = script_context_v1.prepend_output_descriptor_v1(
      native_control.output_count,
      native_control.output_descriptor_peaks,
      output_index,
      descriptor_cbor,
      siblings,
      context_control.language_tag == 128,
      context_control.output_items,
    )
    cek_context_successor_is_exact(
      pre,
      witness,
      native_control,
      CekContextControlV1 { ..context_control, output_items: next_items },
      execution_cursor,
      completed_cpu,
      completed_memory,
    )
  }
}

fn verify_cek_signer_context_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  native_control: NativeScriptsControlV1,
  context_control: CekContextControlV1,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
) -> Bool {
  if context_control.signer_items.length == native_control.signer_count {
    and {
      auxiliary == NoAuxiliaryWitness,
      cek_context_successor_is_exact(
        pre,
        witness,
        native_control,
        CekContextControlV1 { ..context_control, stage: 5 },
        execution_cursor,
        completed_cpu,
        completed_memory,
      ),
    }
  } else {
    expect
        CekSignerContextItemWitness {
          peaks,
          signer_index,
          signer_hash,
          siblings,
        }
      = auxiliary
    expect Some(next_items) = script_context_v1.prepend_signer_v1(
      native_control.signer_count,
      native_control.signer_frontier_commitment,
      peaks,
      signer_index,
      signer_hash,
      siblings,
      context_control.signer_items,
    )
    cek_context_successor_is_exact(
      pre,
      witness,
      native_control,
      CekContextControlV1 { ..context_control, signer_items: next_items },
      execution_cursor,
      completed_cpu,
      completed_memory,
    )
  }
}

pub fn verify_cek_observer_context_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  native_control: NativeScriptsControlV1,
  context_control: CekContextControlV1,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
  door: field_door.MachineFieldDoorV1,
) -> Bool {
  let Pair(verified_source, cek_observer_witness_set) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    native_control.compact_cbor,
    native_control.witness_set_compact_cbor,
    native_control.field_preimage_lengths_cbor,
  )
  let observer_commitment = verified_source.tx_compact.body.required_observers_hash
  let empty_observer_commitment = native_tx_field_access_v1.empty_field_commitment
  let midgard_encoding = context_control.language_tag == 128
  and {
    verified_source.version == 1,
    if observer_commitment == empty_observer_commitment {
      and {
        auxiliary == NoAuxiliaryWitness,
        observer_context_is_pristine(context_control),
        cek_context_successor_is_exact(
          pre,
          witness,
          native_control,
          CekContextControlV1 {
            ..context_control,
            stage: 6,
            observer_summary: finalize_cek_observer_items_v1(
              context_control.observer_items,
              midgard_encoding,
            ),
          },
          execution_cursor,
          completed_cpu,
          completed_memory,
        ),
      }
    } else if and {
      context_control.observer_count > 0,
      context_control.observer_items.length == context_control.observer_count,
    } {
      and {
        auxiliary == NoAuxiliaryWitness,
        cek_context_successor_is_exact(
          pre,
          witness,
          native_control,
          CekContextControlV1 {
            ..context_control,
            stage: 6,
            observer_summary: finalize_cek_observer_items_v1(
              context_control.observer_items,
              midgard_encoding,
            ),
          },
          execution_cursor,
          completed_cpu,
          completed_memory,
        ),
      }
    } else {
      expect TransactionFieldChunkWitness { field_index, item_index, carriage }
        = auxiliary
      // The CEK observer list is built back to front, so the index this step is
      // allowed to read is derived from how many items it has already
      // accumulated. The door is opened at that derived index; `item_index` from
      // the evidence is checked against it rather than used.
      let provisional_count = if context_control.observer_count == 0 {
        field_door.machine_field_count(
          door,
          verified_source,
          cek_observer_witness_set,
          3,
          carriage,
        )
      } else {
        context_control.observer_count
      }
      let expected_index = provisional_count - context_control.observer_items.length - 1
      let item = field_door.open_machine_field_item(
        door,
        verified_source,
        cek_observer_witness_set,
        3,
        expected_index,
        carriage,
      )
      let active_count = field_door.machine_field_item_count(item)
      let observer_hash = field_door.machine_field_item_bytes(item)
      let next_items = prepend_cek_observer_item_v1(
        observer_hash,
        midgard_encoding,
        context_control.observer_items,
      )
      and {
        active_count > 0,
        active_count == provisional_count,
        active_count <= max_tx_size_derived_collection_item_count,
        expected_index >= 0,
        field_index == 3,
        item_index == expected_index,
        field_door.machine_field_item_length(item) == 28,
        if context_control.observer_items.length == 0 {
          context_control.previous_observer == #""
        } else {
          bytearray.compare(observer_hash, context_control.previous_observer) == Less
        },
        cek_context_successor_is_exact(
          pre,
          witness,
          native_control,
          CekContextControlV1 {
            ..context_control,
            observer_count: active_count,
            observer_items: next_items,
            previous_observer: observer_hash,
          },
          execution_cursor,
          completed_cpu,
          completed_memory,
        ),
      }
    },
  }
}

fn verify_cek_mint_context_init(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  native_control: NativeScriptsControlV1,
  context_control: CekContextControlV1,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
) -> Bool {
  and {
    auxiliary == NoAuxiliaryWitness,
    if native_control.mint_count == 0 {
      cek_context_successor_is_exact(
        pre,
        witness,
        native_control,
        CekContextControlV1 {
          ..context_control,
          stage: 9,
          mint_summary: cek_data_v1.map_data_summary_v1(
            cek_data_v1.empty_data_pair_summary_v1(),
          ),
        },
        execution_cursor,
        completed_cpu,
        completed_memory,
      )
    } else {
      cek_context_successor_is_exact(
        pre,
        witness,
        native_control,
        CekContextControlV1 { ..context_control, stage: 8 },
        execution_cursor,
        completed_cpu,
        completed_memory,
      )
    },
  }
}

fn finalize_current_mint_policy(
  control: CekContextControlV1,
) -> cek_data_v1.DataSequenceSummaryV1 {
  expect bytearray.length(control.current_mint_policy) == 28
  cek_data_v1.prepend_data_pair_summary_v1(
    builtin.b_data(control.current_mint_policy)
      |> cek_data_v1.semantic_data_summary_v1,
    cek_data_v1.map_data_summary_v1(control.current_mint_assets),
    control.mint_policies,
  )
}

fn verify_cek_mint_context_item(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  native_control: NativeScriptsControlV1,
  context_control: CekContextControlV1,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
) -> Bool {
  if context_control.mint_cursor == native_control.mint_count {
    let final_policies = finalize_current_mint_policy(context_control)
    and {
      auxiliary == NoAuxiliaryWitness,
      cek_context_successor_is_exact(
        pre,
        witness,
        native_control,
        CekContextControlV1 {
          ..context_control,
          stage: 9,
          current_mint_policy: #"",
          current_mint_assets: cek_data_v1.empty_data_pair_summary_v1(),
          mint_policies: final_policies,
          mint_summary: cek_data_v1.map_data_summary_v1(final_policies),
        },
        execution_cursor,
        completed_cpu,
        completed_memory,
      ),
    }
  } else {
    expect
        CekMintContextItemWitness {
          mint_index,
          policy_id,
          asset_name,
          quantity,
          siblings,
        }
      = auxiliary
    let expected_index = native_control.mint_count - context_control.mint_cursor - 1
    let membership_verified = and {
      mint_index == expected_index,
      validation_merkle_v1.verify_membership(
        native_control.mint_count,
        native_control.mint_peaks,
        mint_index,
        mint_asset_leaf_hash(policy_id, asset_name, quantity),
        siblings,
      ),
    }
    if context_control.current_mint_policy == #"" {
      and {
        membership_verified,
        cek_context_successor_is_exact(
          pre,
          witness,
          native_control,
          CekContextControlV1 {
            ..context_control,
            mint_cursor: context_control.mint_cursor + 1,
            current_mint_policy: policy_id,
            current_mint_assets: prepend_mint_asset_summary(
              asset_name,
              quantity,
              context_control.current_mint_assets,
            ),
          },
          execution_cursor,
          completed_cpu,
          completed_memory,
        ),
      }
    } else if policy_id == context_control.current_mint_policy {
      and {
        membership_verified,
        cek_context_successor_is_exact(
          pre,
          witness,
          native_control,
          CekContextControlV1 {
            ..context_control,
            mint_cursor: context_control.mint_cursor + 1,
            current_mint_assets: prepend_mint_asset_summary(
              asset_name,
              quantity,
              context_control.current_mint_assets,
            ),
          },
          execution_cursor,
          completed_cpu,
          completed_memory,
        ),
      }
    } else {
      let next_policies = finalize_current_mint_policy(context_control)
      and {
        membership_verified,
        bytearray.compare(policy_id, context_control.current_mint_policy) == Less,
        cek_context_successor_is_exact(
          pre,
          witness,
          native_control,
          CekContextControlV1 {
            ..context_control,
            mint_cursor: context_control.mint_cursor + 1,
            current_mint_policy: policy_id,
            current_mint_assets: prepend_mint_asset_summary(
              asset_name,
              quantity,
              cek_data_v1.empty_data_pair_summary_v1(),
            ),
            mint_policies: next_policies,
          },
          execution_cursor,
          completed_cpu,
          completed_memory,
        ),
      }
    }
  }
}

fn cek_redeemer_context_main_successor(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  native_control: NativeScriptsControlV1,
  context_control: CekContextControlV1,
  next_redeemers: CekRedeemerContextControlV1,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
) -> Bool {
  let completed = next_redeemers.cursor == native_control.redeemer_count
  let next_context = if completed {
    expect next_redeemers.active_scan_hash == #""
    expect bytearray.length(next_redeemers.current_redeemer.root) == 32
    CekContextControlV1 {
      ..context_control,
      stage: 10,
      redeemer_context_control_hash: hash_cek_redeemer_context_control_v1(
        next_redeemers,
      ),
    }
  } else {
    CekContextControlV1 {
      ..context_control,
      redeemer_context_control_hash: hash_cek_redeemer_context_control_v1(
        next_redeemers,
      ),
    }
  }
  and {
    cek_redeemer_context_control_is_well_formed(
      native_control.redeemer_count,
      next_redeemers,
    ),
    cek_context_successor_is_exact(
      pre,
      witness,
      native_control,
      next_context,
      execution_cursor,
      completed_cpu,
      completed_memory,
    ),
  }
}

fn verify_cek_redeemer_data_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  native_control: NativeScriptsControlV1,
  context_control: CekContextControlV1,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
) -> Bool {
  when auxiliary is {
    CekRedeemerContextSelectWitness {
      control,
      item_index,
      item_count,
      total_length,
      item_commitment,
      redeemer_siblings,
      purpose_frontier_index,
      purpose_kind,
      purpose_index,
      script_hash,
      subject,
      purpose_siblings,
    } -> {
      let redeemer_leaf = script_proof_v1.redeemer_item_leaf_hash(
        item_index,
        item_commitment,
      )
      let purpose_leaf = script_proof_v1.purpose_leaf_hash(
        purpose_kind,
        purpose_index,
        script_hash,
        subject,
      )
      let current_matches = and {
        cek_redeemer_context_control_is_well_formed(
          native_control.redeemer_count,
          control,
        ),
        hash_cek_redeemer_context_control_v1(control) == context_control.redeemer_context_control_hash,
        control.active_scan_hash == #"",
      }
      let membership_matches = and {
        item_index == native_control.redeemer_count - control.cursor - 1,
        item_count == native_control.redeemer_count,
        validation_merkle_v1.verify_membership(
          native_control.redeemer_count,
          native_control.redeemer_peaks,
          item_index,
          redeemer_leaf,
          redeemer_siblings,
        ),
        purpose_frontier_index >= 0,
        purpose_frontier_index < native_control.purpose_count,
        validation_merkle_v1.verify_membership(
          native_control.purpose_count,
          native_control.purpose_peaks,
          purpose_frontier_index,
          purpose_leaf,
          purpose_siblings,
        ),
      }
      when
        script_context_v1.script_purpose_summary_v1(
          purpose_kind,
          script_hash,
          subject,
          context_control.language_tag == 128,
        )
      is {
        None ->
          if and {
            current_matches,
            membership_matches,
            purpose_kind == 3,
            context_control.language_tag == 3,
          } {
            let item_control = redeemer_item_proof_v1.initial_control_v1(
              redeemer_item_proof_v1.mode_descriptor,
              item_index,
              item_count,
              total_length,
              item_commitment,
              purpose_kind_to_redeemer_tag(purpose_kind),
              purpose_index,
            )
            cek_redeemer_context_main_successor(
              pre,
              witness,
              native_control,
              context_control,
              CekRedeemerContextControlV1 {
                ..control,
                active_scan_hash: redeemer_item_proof_v1.hash_control_v1(
                  item_control,
                ),
                active_redeemer_leaf: redeemer_leaf,
              },
              execution_cursor,
              completed_cpu,
              completed_memory,
            )
          } else {
            False
          }
        Some(purpose) -> {
          let item_control = redeemer_item_proof_v1.initial_control_v1(
            redeemer_item_proof_v1.mode_data,
            item_index,
            item_count,
            total_length,
            item_commitment,
            purpose_kind_to_redeemer_tag(purpose_kind),
            purpose_index,
          )
          and {
            current_matches,
            membership_matches,
            cek_redeemer_context_main_successor(
              pre,
              witness,
              native_control,
              context_control,
              CekRedeemerContextControlV1 {
                ..control,
                active_scan_hash: redeemer_item_proof_v1.hash_control_v1(
                  item_control,
                ),
                active_redeemer_leaf: redeemer_leaf,
                active_purpose: purpose,
              },
              execution_cursor,
              completed_cpu,
              completed_memory,
            ),
          }
        }
      }
    }
    RedeemerItemStepWitness {
      redeemer_control: Some(redeemer_control),
      control: item_control,
      witness: item_witness,
    } -> {
      let current_matches = and {
        cek_redeemer_context_control_is_well_formed(
          native_control.redeemer_count,
          redeemer_control,
        ),
        hash_cek_redeemer_context_control_v1(redeemer_control) == context_control.redeemer_context_control_hash,
        item_control.item_count == native_control.redeemer_count,
        item_control.item_index == native_control.redeemer_count - redeemer_control.cursor - 1,
        script_proof_v1.redeemer_item_leaf_hash(
          item_control.item_index,
          item_control.item_commitment,
        ) == redeemer_control.active_redeemer_leaf,
        redeemer_control.active_scan_hash == redeemer_item_proof_v1.hash_control_v1(
          item_control,
        ),
      }
      when redeemer_item_proof_v1.step_v1(item_control, item_witness) is {
        None -> False
        Some(redeemer_item_proof_v1.RedeemerItemProofInvalid) -> False
        Some(redeemer_item_proof_v1.RedeemerItemProofAdvanced { control: next }) -> {
          let next_redeemers = if next.stage == redeemer_item_proof_v1.stage_terminal {
            if next.mode == redeemer_item_proof_v1.mode_descriptor {
              CekRedeemerContextControlV1 {
                ..redeemer_control,
                cursor: redeemer_control.cursor + 1,
                active_scan_hash: #"",
                active_redeemer_leaf: #"",
                active_purpose: empty_data_summary_v1(),
              }
            } else {
              expect Some(summary) = redeemer_item_proof_v1.finalize_v1(next)
              let next_current = if redeemer_control.active_redeemer_leaf == context_control.redeemer_leaf {
                summary
              } else {
                redeemer_control.current_redeemer
              }
              CekRedeemerContextControlV1 {
                cursor: redeemer_control.cursor + 1,
                map_items: cek_data_v1.prepend_data_pair_summary_v1(
                  redeemer_control.active_purpose,
                  summary,
                  redeemer_control.map_items,
                ),
                active_scan_hash: #"",
                active_redeemer_leaf: #"",
                active_purpose: empty_data_summary_v1(),
                current_redeemer: next_current,
              }
            }
          } else {
            CekRedeemerContextControlV1 {
              ..redeemer_control,
              active_scan_hash: redeemer_item_proof_v1.hash_control_v1(next),
            }
          }
          and {
            current_matches,
            if next.mode == redeemer_item_proof_v1.mode_descriptor {
              and {
                item_control.expected_purpose_tag == 6,
                context_control.language_tag == 3,
                redeemer_control.active_purpose == empty_data_summary_v1(),
              }
            } else {
              bytearray.length(redeemer_control.active_purpose.root) == 32
            },
            cek_redeemer_context_main_successor(
              pre,
              witness,
              native_control,
              context_control,
              next_redeemers,
              execution_cursor,
              completed_cpu,
              completed_memory,
            ),
          }
        }
      }
    }
    _ -> False
  }
}

fn cek_context_execution_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  native_control: NativeScriptsControlV1,
  context_control: CekContextControlV1,
  context_summary: cek_data_v1.DataSummaryV1,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
) -> Bool {
  let context_value = cek_constant_v1.semantic_data_constant_root_v1(
    context_summary,
  )
  let context_term = cek_proof_v1.hash_context_constant_term_v1(context_value)
  let initial_state = cek_machine_v1.MachineStateV1 {
    mode: cek_machine_v1.mode_compute,
    execution_index: execution_cursor,
    focus_root: cek_proof_v1.hash_application_term_v1(
      context_control.program_term_root,
      context_term,
    ),
    environment_root: cek_proof_v1.empty_environment_root_v1,
    continuation_root: cek_proof_v1.empty_continuation_root_v1,
    auxiliary: 0,
    cpu: 0,
    memory: 0,
  }
  and {
    witness.claimed_successor.phase == validation_trace_v1.Cek,
    witness.claimed_successor.execution_cpu == pre.execution_cpu,
    witness.claimed_successor.execution_memory == pre.execution_memory,
    witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.Cek,
      pre.program_counter + 1,
      encode_cek_witness_v1(
        native_control,
        #"",
        execution_cursor,
        completed_cpu,
        completed_memory,
        cek_machine_v1.hash_state_v1(initial_state),
        context_control.execution_cpu_limit,
        context_control.execution_memory_limit,
        context_control.program_envelope_hash,
      ),
    ),
  }
}

fn completed_redeemer_context_matches(
  native_control: NativeScriptsControlV1,
  context_control: CekContextControlV1,
  redeemers: CekRedeemerContextControlV1,
) -> Bool {
  and {
    cek_redeemer_context_control_is_well_formed(
      native_control.redeemer_count,
      redeemers,
    ),
    hash_cek_redeemer_context_control_v1(redeemers) == context_control.redeemer_context_control_hash,
    redeemers.cursor == native_control.redeemer_count,
    redeemers.active_scan_hash == #"",
    bytearray.length(redeemers.current_redeemer.root) == 32,
  }
}

pub fn verify_cek_context_finalize(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  native_control: NativeScriptsControlV1,
  context_control: CekContextControlV1,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
) -> Bool {
  let midgard_encoding = context_control.language_tag == 128
  let Pair(redeemers, script_info) = if and {
    !midgard_encoding,
    context_control.purpose_kind == 0,
  } {
    expect
        CekContextFinalizeSpendWitness {
          redeemer_control,
          item_index,
          key,
          descriptor_cbor,
          siblings,
        }
      = auxiliary
    expect item_index == context_control.purpose_index
    expect key == context_control.subject
    expect
      validation_merkle_v1.verify_membership(
        native_control.resolved_input_count,
        native_control.resolved_item_peaks,
        item_index,
        script_proof_v1.resolved_context_item_leaf_hash(
          0,
          item_index,
          key,
          descriptor_cbor,
        ),
        siblings,
      )
    expect Some(summary) = script_context_v1.cardano_spend_script_info_from_descriptor_v1(
      context_control.subject,
      descriptor_cbor,
    )
    Pair(redeemer_control, summary)
  } else {
    expect CekContextFinalizeWitness { redeemer_control } = auxiliary
    expect Some(summary) = if midgard_encoding {
      script_context_v1.script_purpose_summary_v1(
        context_control.purpose_kind,
        context_control.script_hash,
        context_control.subject,
        True,
      )
    } else {
      script_context_v1.cardano_script_info_summary_v1(
        context_control.purpose_kind,
        context_control.script_hash,
        context_control.subject,
        None,
      )
    }
    Pair(redeemer_control, summary)
  }
  let parts = CekContextPartsControlV1 {
    redeemer_items: redeemers.map_items,
    redeemer: redeemers.current_redeemer,
    script_info,
  }
  and {
    completed_redeemer_context_matches(
      native_control,
      context_control,
      redeemers,
    ),
    cek_context_successor_is_exact(
      pre,
      witness,
      native_control,
      CekContextControlV1 {
        ..context_control,
        stage: 11,
        redeemer_context_control_hash: hash_cek_context_parts_control_v1(parts),
      },
      execution_cursor,
      completed_cpu,
      completed_memory,
    ),
  }
}

fn verify_cek_context_assemble(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  native_control: NativeScriptsControlV1,
  context_control: CekContextControlV1,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
) -> Bool {
  expect CekContextAssembleWitness { control } = auxiliary
  let assembly = CekTxInfoAssemblyControlV1 {
    tail_fields: script_context_v1.tx_info_tail_fields_summary_v1(
      context_control.language_tag == 128,
      context_control.observer_summary,
      context_control.signer_items,
      context_control.mint_summary,
      control.redeemer_items,
      pre.transaction_id,
    ),
    redeemer: control.redeemer,
    script_info: control.script_info,
  }
  and {
    hash_cek_context_parts_control_v1(control) == context_control.redeemer_context_control_hash,
    cek_context_successor_is_exact(
      pre,
      witness,
      native_control,
      CekContextControlV1 {
        ..context_control,
        stage: 12,
        redeemer_context_control_hash: hash_cek_tx_info_assembly_control_v1(
          assembly,
        ),
      },
      execution_cursor,
      completed_cpu,
      completed_memory,
    ),
  }
}

fn verify_cek_tx_info_finalize(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  native_control: NativeScriptsControlV1,
  context_control: CekContextControlV1,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
) -> Bool {
  expect CekTxInfoFinalizeWitness { control } = auxiliary
  let compact = decode_native_tx_compact_v1(native_control.compact_cbor)
  let tx_info = script_context_v1.tx_info_from_tail_summary_v1(
    context_control.language_tag == 128,
    context_control.spend_items,
    context_control.reference_items,
    context_control.output_items,
    compact.body.fee,
    compact.body.validity_interval_start,
    compact.body.validity_interval_end,
    context_control.observer_summary,
    context_control.mint_summary,
    control.tail_fields,
  )
  let final_context = CekFinalContextControlV1 {
    tx_info,
    redeemer: control.redeemer,
    script_info: control.script_info,
  }
  and {
    hash_cek_tx_info_assembly_control_v1(control) == context_control.redeemer_context_control_hash,
    cek_context_successor_is_exact(
      pre,
      witness,
      native_control,
      CekContextControlV1 {
        ..context_control,
        stage: 13,
        redeemer_context_control_hash: hash_cek_final_context_control_v1(
          final_context,
        ),
      },
      execution_cursor,
      completed_cpu,
      completed_memory,
    ),
  }
}

fn verify_cek_context_seed(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  native_control: NativeScriptsControlV1,
  context_control: CekContextControlV1,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
) -> Bool {
  expect CekContextSeedWitness { control } = auxiliary
  let context = script_context_v1.script_context_summary_v1(
    control.tx_info,
    control.redeemer,
    control.script_info,
  )
  and {
    hash_cek_final_context_control_v1(control) == context_control.redeemer_context_control_hash,
    cek_context_execution_successor_is_exact(
      pre,
      witness,
      native_control,
      context_control,
      context,
      execution_cursor,
      completed_cpu,
      completed_memory,
    ),
  }
}

fn verify_cek_context_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  native_control: NativeScriptsControlV1,
  context_control: CekContextControlV1,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
  door: field_door.MachineFieldDoorV1,
) -> Bool {
  if context_control.stage == 0 {
    verify_cek_redeemer_selection(
      pre,
      witness,
      auxiliary,
      native_control,
      context_control,
      execution_cursor,
      completed_cpu,
      completed_memory,
    )
  } else if context_control.stage == 1 {
    verify_cek_reference_context_step(
      pre,
      witness,
      auxiliary,
      native_control,
      context_control,
      execution_cursor,
      completed_cpu,
      completed_memory,
    )
  } else if context_control.stage == 2 {
    verify_cek_spend_context_step(
      pre,
      witness,
      auxiliary,
      native_control,
      context_control,
      execution_cursor,
      completed_cpu,
      completed_memory,
    )
  } else if context_control.stage == 3 {
    verify_cek_output_context_step(
      pre,
      witness,
      auxiliary,
      native_control,
      context_control,
      execution_cursor,
      completed_cpu,
      completed_memory,
    )
  } else if context_control.stage == 4 {
    verify_cek_signer_context_step(
      pre,
      witness,
      auxiliary,
      native_control,
      context_control,
      execution_cursor,
      completed_cpu,
      completed_memory,
    )
  } else if context_control.stage == 5 {
    verify_cek_observer_context_step(
      pre,
      witness,
      auxiliary,
      native_control,
      context_control,
      execution_cursor,
      completed_cpu,
      completed_memory,
      door,
    )
  } else if context_control.stage == 6 {
    verify_cek_mint_context_init(
      pre,
      witness,
      auxiliary,
      native_control,
      context_control,
      execution_cursor,
      completed_cpu,
      completed_memory,
    )
  } else if context_control.stage == 7 {
    False
  } else if context_control.stage == 8 {
    verify_cek_mint_context_item(
      pre,
      witness,
      auxiliary,
      native_control,
      context_control,
      execution_cursor,
      completed_cpu,
      completed_memory,
    )
  } else if context_control.stage == 9 {
    verify_cek_redeemer_data_step(
      pre,
      witness,
      auxiliary,
      native_control,
      context_control,
      execution_cursor,
      completed_cpu,
      completed_memory,
    )
  } else if context_control.stage == 10 {
    verify_cek_context_finalize(
      pre,
      witness,
      auxiliary,
      native_control,
      context_control,
      execution_cursor,
      completed_cpu,
      completed_memory,
    )
  } else if context_control.stage == 11 {
    verify_cek_context_assemble(
      pre,
      witness,
      auxiliary,
      native_control,
      context_control,
      execution_cursor,
      completed_cpu,
      completed_memory,
    )
  } else if context_control.stage == 12 {
    verify_cek_tx_info_finalize(
      pre,
      witness,
      auxiliary,
      native_control,
      context_control,
      execution_cursor,
      completed_cpu,
      completed_memory,
    )
  } else if context_control.stage == 13 {
    verify_cek_context_seed(
      pre,
      witness,
      auxiliary,
      native_control,
      context_control,
      execution_cursor,
      completed_cpu,
      completed_memory,
    )
  } else {
    // Remaining scalar/dynamic fields, the outer context root, and initial
    // CEK application state must all be proven before execution can start.
    False
  }
}

fn cek_finish_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  cek: CekWitnessControlV1,
) -> Bool {
  let control = cek.control
  and {
    cek_control_is_finish_v1(cek),
    cek.execution_cpu_limit == 0,
    cek.execution_memory_limit == 0,
    cek.program_envelope_hash == #"",
    auxiliary == NoAuxiliaryWitness,
    witness.claimed_successor.phase == validation_trace_v1.ValueAndMint,
    witness.claimed_successor.execution_cpu == pre.execution_cpu,
    witness.claimed_successor.execution_memory == pre.execution_memory,
    witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.ValueAndMint,
      pre.program_counter + 1,
      encode_value_and_mint_witness_v1(control),
    ),
  }
}

fn cek_execution_selection_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  cek: CekWitnessControlV1,
) -> Bool {
  and {
    cek_control_is_execution_selection_v1(cek),
    cek.execution_cpu_limit == 0,
    cek.execution_memory_limit == 0,
    cek.program_envelope_hash == #"",
    verify_cek_execution_selection(
      pre,
      witness,
      auxiliary,
      cek.control,
      cek.execution_cursor,
      cek.completed_cpu,
      cek.completed_memory,
    ),
  }
}

fn cek_context_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  cek: CekWitnessControlV1,
  door: field_door.MachineFieldDoorV1,
) -> Bool {
  let control = cek.control
  let context_control = cek_context_control_from_cbor(cek.context_control_cbor)
  and {
    cek_control_is_context_step_v1(cek),
    cek.execution_cursor < control.execution_count,
    cek.execution_cpu_limit == 0,
    cek.execution_memory_limit == 0,
    cek.program_envelope_hash == context_control.program_envelope_hash,
    cek.context_control_cbor == encode_cek_context_control_v1(context_control),
    cek_context_control_is_well_formed(control, context_control),
    verify_cek_context_step(
      pre,
      witness,
      auxiliary,
      control,
      context_control,
      cek.execution_cursor,
      cek.completed_cpu,
      cek.completed_memory,
      door,
    ),
  }
}

fn cek_core_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  cek: CekWitnessControlV1,
) -> Bool {
  and {
    cek_control_is_core_step_v1(cek),
    cek.context_control_cbor == #"",
    verify_cek_core_step(
      pre,
      witness,
      auxiliary,
      cek.control,
      cek.execution_cursor,
      cek.completed_cpu,
      cek.completed_memory,
      cek.active_state_hash,
      cek.execution_cpu_limit,
      cek.execution_memory_limit,
      cek.program_envelope_hash,
    ),
  }
}

pub fn verify_cek(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  door: field_door.MachineFieldDoorV1,
) -> Bool {
  let cek = cek_witness_control_v1(witness)
  and {
    cek_witness_is_well_formed_v1(pre, witness, cek),
    if cek_control_is_core_step_v1(cek) {
      cek_core_v1(pre, witness, auxiliary, cek)
    } else if cek_control_is_context_step_v1(cek) {
      cek_context_v1(pre, witness, auxiliary, cek, door)
    } else if cek_control_is_finish_v1(cek) {
      cek_finish_v1(pre, witness, auxiliary, cek)
    } else {
      cek_execution_selection_v1(pre, witness, auxiliary, cek)
    },
  }
}

/// Per-kind cek semantic resolvers (R5 item 1). Each one re-runs the shared
/// cek control decode, pins its own discriminator and calls the same branch
/// the aggregate `verify_cek` would have taken, so the four resolvers
/// partition the cek step space exactly.
pub fn verify_cek_finish_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let cek = cek_witness_control_v1(witness)
  and {
    cek_witness_is_well_formed_v1(pre, witness, cek),
    cek_finish_v1(pre, witness, NoAuxiliaryWitness, cek),
  }
}

pub fn verify_cek_execution_selection_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  let cek = cek_witness_control_v1(witness)
  and {
    cek_witness_is_well_formed_v1(pre, witness, cek),
    cek_execution_selection_v1(pre, witness, auxiliary, cek),
  }
}

pub fn verify_cek_context_step_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  door: field_door.MachineFieldDoorV1,
) -> Bool {
  let cek = cek_witness_control_v1(witness)
  and {
    cek_witness_is_well_formed_v1(pre, witness, cek),
    // Pinned here as well as inside `cek_context_v1`: the branch decodes the
    // context control before it tests the discriminator, so without this
    // pin a non-context witness would abort on the empty control bytes
    // instead of being refused, and the four resolvers would not partition
    // the step space by a clean `False`.
    cek_control_is_context_step_v1(cek),
    cek_context_v1(pre, witness, auxiliary, cek, door),
  }
}

pub fn verify_cek_core_step_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  step: cek_machine_v1.CoreStepEvidenceV1,
) -> Bool {
  let cek = cek_witness_control_v1(witness)
  and {
    cek_witness_is_well_formed_v1(pre, witness, cek),
    cek_core_v1(pre, witness, CekCoreStepWitness { step }, cek),
  }
}
