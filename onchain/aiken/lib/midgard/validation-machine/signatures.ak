use aiken/cbor
use aiken/crypto.{blake2b_224, verify_ed25519_signature}
use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/compact.{verify_native_tx_proof_source_v1}
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_address_witness_cbor,
}
use midgard/native_tx_field_access_v1
use midgard/rejection_reason_v1.{
  reject_invalid_signature, reject_missing_required_witness,
}
use midgard/script_proof_v1
use midgard/validation_machine/machine_types.{
  NoAuxiliaryWitness, RequiredSignerItemWitness, SignaturesControlV1,
  SignerMembershipProof, SignerSetProofV1, TransactionFieldChunkWitness,
  ValidationAuxiliaryWitnessV1, ValidationOneStepWitnessV1,
}
use midgard/validation_machine/shared.{
  max_tx_size_derived_collection_item_count, rejected_successor_is_exact,
  signer_membership_is_valid, signer_non_membership_is_valid,
}
use midgard/validation_machine/witness.{
  encode_phase_a_native_scripts_scan_witness, encode_signatures_scan_witness,
  signatures_control_from_witness, signatures_control_is_bound,
}
use midgard/validation_machine_field_door_v1 as field_door
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

fn signatures_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  post: validation_trace_v1.ValidationMachineStateV1,
  control: SignaturesControlV1,
) -> Bool {
  and {
    post.phase == validation_trace_v1.Signatures,
    post.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.Signatures,
      pre.program_counter + 1,
      encode_signatures_scan_witness(
        control.compact_cbor,
        control.witness_set_compact_cbor,
        control.field_preimage_lengths_cbor,
        control.context_cbor,
        control.resolution_schedule_hash,
        control.stage,
        control.address_count,
        control.required_count,
        control.address_seen,
        control.required_seen,
        control.previous_order_key,
        control.previous_signer_hash,
        control.signer_count,
        control.signer_peaks,
        control.invalid_signature_seen,
      ),
    ),
  }
}

fn signatures_after_required_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  post: validation_trace_v1.ValidationMachineStateV1,
  control: SignaturesControlV1,
) -> Bool {
  if control.invalid_signature_seen == 1 {
    rejected_successor_is_exact(pre, post, reject_invalid_signature)
  } else {
    signatures_successor_is_exact(
      pre,
      post,
      SignaturesControlV1 { ..control, stage: 2 },
    )
  }
}

fn signature_address_order_key(
  signer_hash: ByteArray,
  witness_cbor: ByteArray,
  item_index: Int,
) -> ByteArray {
  signer_hash
    |> bytearray.concat(witness_cbor)
    |> bytearray.concat(cbor.serialise(item_index))
}

/// One §5.3 field-7 address-witness item.
///
/// `item_index` is the prover's to choose — the order this phase enforces is on
/// the derived `order_key`, which includes the index, not on the index itself.
fn verify_signature_address_item(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: SignaturesControlV1,
  door: field_door.MachineFieldDoorV1,
  field_index: Int,
  item_index: Int,
  carriage: native_tx_field_access_v1.FieldCarriageV1,
) -> Bool {
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
    7,
    item_index,
    carriage,
  )
  let item_count = field_door.machine_field_item_count(item)
  let item_bytes = field_door.machine_field_item_bytes(item)
  let active_count = if control.address_count == -1 {
    item_count
  } else {
    control.address_count
  }
  let address_witness = decode_midgard_address_witness_cbor(item_bytes)
  let signer_hash = blake2b_224(address_witness.verification_key)
  let order_key = signature_address_order_key(
    signer_hash,
    item_bytes,
    item_index,
  )
  let new_signer = control.previous_signer_hash != signer_hash
  let next_signer_count = if new_signer {
    control.signer_count + 1
  } else {
    control.signer_count
  }
  let next_signer_peaks = if new_signer {
    validation_merkle_v1.append_leaf(
      control.signer_count,
      control.signer_peaks,
      script_proof_v1.signer_leaf_hash(signer_hash),
    )
  } else {
    control.signer_peaks
  }
  let next_invalid_signature_seen = if and {
    control.invalid_signature_seen == 0,
    !verify_ed25519_signature(
      address_witness.verification_key,
      pre.transaction_id,
      address_witness.signature,
    ),
  } {
    1
  } else {
    control.invalid_signature_seen
  }
  let next_address_seen = control.address_seen + 1
  let next_control = if next_address_seen == active_count {
    SignaturesControlV1 {
      ..control,
      stage: 1,
      address_count: active_count,
      address_seen: next_address_seen,
      previous_order_key: #"",
      previous_signer_hash: #"",
      signer_count: next_signer_count,
      signer_peaks: next_signer_peaks,
      invalid_signature_seen: next_invalid_signature_seen,
    }
  } else {
    SignaturesControlV1 {
      ..control,
      address_count: active_count,
      address_seen: next_address_seen,
      previous_order_key: order_key,
      previous_signer_hash: signer_hash,
      signer_count: next_signer_count,
      signer_peaks: next_signer_peaks,
      invalid_signature_seen: next_invalid_signature_seen,
    }
  }
  and {
    signatures_control_is_bound(pre, witness, control, verified, witness_set),
    control.stage == 0,
    control.address_count != 0,
    field_index == 7,
    control.address_count == -1 || control.address_count == item_count,
    active_count > 0,
    active_count <= max_tx_size_derived_collection_item_count,
    control.address_seen < active_count,
    item_count == active_count,
    item_index >= 0,
    item_index < active_count,
    or {
      control.previous_order_key == #"",
      bytearray.compare(control.previous_order_key, order_key) == Less,
    },
    signatures_successor_is_exact(pre, witness.claimed_successor, next_control),
  }
}

fn verify_signatures_advance(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: SignaturesControlV1,
) -> Bool {
  let Pair(verified, witness_set) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  and {
    signatures_control_is_bound(pre, witness, control, verified, witness_set),
    if control.stage == 0 {
      and {
        control.address_count == 0,
        signatures_successor_is_exact(
          pre,
          witness.claimed_successor,
          SignaturesControlV1 { ..control, stage: 1 },
        ),
      }
    } else if control.stage == 1 {
      and {
        control.required_count == 0,
        signatures_after_required_successor_is_exact(
          pre,
          witness.claimed_successor,
          control,
        ),
      }
    } else {
      False
    },
  }
}

fn required_signer_membership_is_valid(
  signer_hash: ByteArray,
  control: SignaturesControlV1,
  proof: SignerSetProofV1,
) -> Bool {
  when proof is {
    SignerMembershipProof { peaks, signer_index, siblings } -> and {
        peaks == control.signer_peaks,
        signer_membership_is_valid(
          signer_hash,
          control.signer_count,
          validation_merkle_v1.frontier_commitment(
            control.signer_count,
            control.signer_peaks,
          ),
          peaks,
          signer_index,
          siblings,
        ),
      }
    _ -> False
  }
}

fn required_signer_non_membership_is_valid(
  signer_hash: ByteArray,
  control: SignaturesControlV1,
  proof: SignerSetProofV1,
) -> Bool {
  signer_non_membership_is_valid(
    signer_hash,
    control.signer_count,
    validation_merkle_v1.frontier_commitment(
      control.signer_count,
      control.signer_peaks,
    ),
    proof,
  )
}

fn verify_required_signer_item(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: SignaturesControlV1,
  door: field_door.MachineFieldDoorV1,
  carriage: native_tx_field_access_v1.FieldCarriageV1,
  signer_proof: SignerSetProofV1,
) -> Bool {
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
    4,
    control.required_seen,
    carriage,
  )
  let item_count = field_door.machine_field_item_count(item)
  let active_count = if control.required_count == -1 {
    item_count
  } else {
    control.required_count
  }
  let signer_hash = field_door.machine_field_item_bytes(item)
  let next_required_seen = control.required_seen + 1
  let next_control = SignaturesControlV1 {
    ..control,
    required_count: active_count,
    required_seen: next_required_seen,
  }
  let present = required_signer_membership_is_valid(
    signer_hash,
    control,
    signer_proof,
  )
  let absent = required_signer_non_membership_is_valid(
    signer_hash,
    control,
    signer_proof,
  )
  and {
    signatures_control_is_bound(pre, witness, control, verified, witness_set),
    control.stage == 1,
    control.required_count != 0,
    verified.tx_compact.body.required_signers_hash != native_tx_field_access_v1.empty_field_commitment,
    control.required_count == -1 || control.required_count == item_count,
    active_count > 0,
    active_count <= max_tx_size_derived_collection_item_count,
    control.required_seen < active_count,
    item_count == active_count,
    bytearray.length(signer_hash) == 28,
    if absent {
      rejected_successor_is_exact(
        pre,
        witness.claimed_successor,
        reject_missing_required_witness,
      )
    } else if present {
      if next_required_seen == active_count {
        signatures_after_required_successor_is_exact(
          pre,
          witness.claimed_successor,
          next_control,
        )
      } else {
        signatures_successor_is_exact(
          pre,
          witness.claimed_successor,
          next_control,
        )
      }
    } else {
      False
    },
  }
}

fn verify_signatures_handoff(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: SignaturesControlV1,
) -> Bool {
  let Pair(verified, witness_set) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  let script_count = if witness_set.script_tx_wits_hash == native_tx_field_access_v1.empty_field_commitment {
    0
  } else {
    -1
  }
  and {
    signatures_control_is_bound(pre, witness, control, verified, witness_set),
    control.stage == 2,
    witness.claimed_successor.phase == validation_trace_v1.PhaseANativeScripts,
    witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.PhaseANativeScripts,
      pre.program_counter + 1,
      encode_phase_a_native_scripts_scan_witness(
        control.compact_cbor,
        control.witness_set_compact_cbor,
        control.field_preimage_lengths_cbor,
        control.context_cbor,
        control.resolution_schedule_hash,
        0,
        script_count,
        0,
        0,
        0,
        #"",
        0,
        // stack_root
        #"",
        // stack_depth
        0,
        // node_count
        0,
        // result: the "not yet evaluated" sentinel. A stage-0 phase-A native
        // scan control MUST carry -1; `phase_a_native_control_is_bound` pins
        // -1 at every stage except 2, and `result == 0` at a completed stage
        // MEANS "this native script evaluated false -> reject". Emitting 0 here
        // asserted a verdict the machine had not computed and made the handoff
        // successor unusable. Matches the TypeScript reference builder
        // `resetPhaseANativeScriptsScanControl` (`result: -1`).
        -1,
        control.signer_count,
        control.signer_peaks,
        #"",
      ),
    ),
  }
}

pub fn verify_signatures(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  door: field_door.MachineFieldDoorV1,
) -> Bool {
  let control = signatures_control_from_witness(witness.work_witness_cbor)
  when auxiliary is {
    NoAuxiliaryWitness ->
      if control.stage == 2 {
        verify_signatures_handoff(pre, witness, control)
      } else {
        verify_signatures_advance(pre, witness, control)
      }
    TransactionFieldChunkWitness { field_index, item_index, carriage } ->
      verify_signature_address_item(
        pre,
        witness,
        control,
        door,
        field_index,
        item_index,
        carriage,
      )
    RequiredSignerItemWitness { carriage, signer_proof } ->
      verify_required_signer_item(
        pre,
        witness,
        control,
        door,
        carriage,
        signer_proof,
      )
    _ -> False
  }
}

pub fn verify_signatures_advance_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  verify_signatures_advance(
    pre,
    witness,
    signatures_control_from_witness(witness.work_witness_cbor),
  )
}

pub fn verify_signature_address_item_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  door: field_door.MachineFieldDoorV1,
  field_index: Int,
  item_index: Int,
  carriage: native_tx_field_access_v1.FieldCarriageV1,
) -> Bool {
  verify_signature_address_item(
    pre,
    witness,
    signatures_control_from_witness(witness.work_witness_cbor),
    door,
    field_index,
    item_index,
    carriage,
  )
}

pub fn verify_required_signer_item_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  door: field_door.MachineFieldDoorV1,
  carriage: native_tx_field_access_v1.FieldCarriageV1,
  signer_proof: SignerSetProofV1,
) -> Bool {
  verify_required_signer_item(
    pre,
    witness,
    signatures_control_from_witness(witness.work_witness_cbor),
    door,
    carriage,
    signer_proof,
  )
}

pub fn verify_signatures_handoff_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  verify_signatures_handoff(
    pre,
    witness,
    signatures_control_from_witness(witness.work_witness_cbor),
  )
}
