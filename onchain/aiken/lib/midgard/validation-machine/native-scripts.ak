use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/bounded_item_v1
use midgard/fraud_proofs/native_tx/compact.{
  native_tx_proof_commitment_v1, verify_native_tx_proof_source_v1,
}
use midgard/native_script_scan_v1
use midgard/script_proof_v1
use midgard/validation_machine/machine_types.{
  NativeExecutionDescriptorWitness, NativeScriptsControlV1, NoAuxiliaryWitness,
  ValidationAuxiliaryWitnessV1, ValidationOneStepWitnessV1,
}
use midgard/validation_machine/shared.{max_aggregate_field_preimage_bytes}
use midgard/validation_machine/witness.{
  encode_native_scripts_control_v1, encode_phase_a_native_scripts_scan_witness,
  encode_script_integrity_witness_v1, native_scripts_control_from_witness,
  native_scripts_control_is_well_formed,
}
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

pub fn script_integrity_control_and_stage(
  work_witness_cbor: ByteArray,
) -> Pair<NativeScriptsControlV1, Int> {
  expect Some(data) = cbor.deserialise(work_witness_cbor)
  let fields = builtin.un_list_data(data)
  expect Some(native_control_cbor_data) = list.at(fields, 0)
  expect Some(stage_data) = list.at(fields, 1)
  Pair(
    native_scripts_control_from_witness(
      builtin.un_b_data(native_control_cbor_data),
    ),
    builtin.un_i_data(stage_data),
  )
}

fn native_scripts_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: NativeScriptsControlV1,
  next_language_bitmap: Int,
) -> Bool {
  let next_control = NativeScriptsControlV1 {
    ..control,
    execution_cursor: control.execution_cursor + 1,
    language_bitmap: next_language_bitmap,
  }
  and {
    witness.claimed_successor.phase == validation_trace_v1.NativeScripts,
    witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.NativeScripts,
      pre.program_counter + 1,
      encode_native_scripts_control_v1(next_control),
    ),
  }
}

fn verify_native_execution_descriptor_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: NativeScriptsControlV1,
) -> Bool {
  expect
      NativeExecutionDescriptorWitness {
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
        signer_peaks,
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
  if and {
    execution_index == control.execution_cursor,
    control.execution_count == control.purpose_count,
    script_total_length > 0,
    script_total_length <= max_aggregate_field_preimage_bytes,
    bytearray.length(script_item_commitment) == 32,
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
      expect Some(chunk_proof) = first_chunk_proof
      expect Some(header) = native_script_scan_v1.versioned_script_header_v1(
        chunk_proof.chunk,
        script_total_length,
      )
      let continuation_cbor = encode_native_scripts_control_v1(control)
      and {
        redeemer_leaf == #"",
        chunk_proof.chunk_index == 0,
        chunk_proof.total_length == script_total_length,
        bounded_item_v1.verify_chunk(script_item_commitment, chunk_proof),
        header.language_tag == 0,
        validation_merkle_v1.frontier_is_well_formed(
          control.signer_count,
          signer_peaks,
        ),
        validation_merkle_v1.frontier_commitment(
          control.signer_count,
          signer_peaks,
        ) == control.signer_frontier_commitment,
        witness.claimed_successor.phase == validation_trace_v1.PhaseANativeScripts,
        witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
          validation_trace_v1.PhaseANativeScripts,
          pre.program_counter + 1,
          encode_phase_a_native_scripts_scan_witness(
            control.compact_cbor,
            control.witness_set_compact_cbor,
            control.field_preimage_lengths_cbor,
            control.context_cbor,
            blake2b_256(continuation_cbor),
            1,
            1,
            0,
            0,
            script_total_length,
            script_item_commitment,
            header.payload_offset,
            #"",
            0,
            0,
            -1,
            control.signer_count,
            signer_peaks,
            continuation_cbor,
          ),
        ),
      }
    } else if and {
      or {
        language_tag == 3,
        language_tag == 128,
      },
      bytearray.length(redeemer_leaf) == 32,
      first_chunk_proof == None,
      signer_peaks == [],
    } {
      native_scripts_successor_is_exact(
        pre,
        witness,
        control,
        if language_tag == 3 {
          control.language_bitmap + if control.language_bitmap % 2 == 0 {
            1
          } else {
            0
          }
        } else {
          control.language_bitmap + if control.language_bitmap < 2 {
            2
          } else {
            0
          }
        },
      )
    } else {
      False
    }
  } else {
    False
  }
}

pub fn verify_native_scripts(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  let control = native_scripts_control_from_witness(witness.work_witness_cbor)
  let Pair(verified_source, _) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  and {
    verified_source.version == 1,
    native_tx_proof_commitment_v1(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
    ) == pre.transaction_commitment,
    validation_trace_v1.hash_validation_context(control.context_cbor) == pre.validation_context_hash,
    native_scripts_control_is_well_formed(control),
    witness.work_witness_cbor == encode_native_scripts_control_v1(control),
    if control.execution_cursor == control.execution_count {
      and {
        auxiliary == NoAuxiliaryWitness,
        witness.claimed_successor.phase == validation_trace_v1.ScriptIntegrity,
        witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
          validation_trace_v1.ScriptIntegrity,
          pre.program_counter + 1,
          encode_script_integrity_witness_v1(control),
        ),
      }
    } else {
      verify_native_execution_descriptor_step(pre, witness, auxiliary, control)
    },
  }
}

pub fn verify_native_scripts_terminal_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  verify_native_scripts(pre, witness, NoAuxiliaryWitness)
}

pub fn verify_native_scripts_native_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  execution_index: Int,
  purpose_kind: Int,
  purpose_index: Int,
  script_hash: ByteArray,
  subject: ByteArray,
  purpose_siblings: List<ByteArray>,
  source_index: Int,
  origin_kind: Int,
  source_key: ByteArray,
  script_total_length: Int,
  script_item_commitment: ByteArray,
  source_siblings: List<ByteArray>,
  redeemer_leaf: ByteArray,
  execution_siblings: List<ByteArray>,
  first_chunk_proof: bounded_item_v1.ChunkProofV1,
  signer_peaks: List<validation_merkle_v1.FrontierPeak>,
) -> Bool {
  verify_native_scripts(
    pre,
    witness,
    NativeExecutionDescriptorWitness {
      execution_index,
      language_tag: 0,
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
      first_chunk_proof: Some(first_chunk_proof),
      signer_peaks,
    },
  )
}

pub fn verify_native_scripts_effectful_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  execution_index: Int,
  language_tag: Int,
  purpose_kind: Int,
  purpose_index: Int,
  script_hash: ByteArray,
  subject: ByteArray,
  purpose_siblings: List<ByteArray>,
  source_index: Int,
  origin_kind: Int,
  source_key: ByteArray,
  script_total_length: Int,
  script_item_commitment: ByteArray,
  source_siblings: List<ByteArray>,
  redeemer_leaf: ByteArray,
  execution_siblings: List<ByteArray>,
) -> Bool {
  verify_native_scripts(
    pre,
    witness,
    NativeExecutionDescriptorWitness {
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
      first_chunk_proof: None,
      signer_peaks: [],
    },
  )
}
