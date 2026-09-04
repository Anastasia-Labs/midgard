use aiken/primitive/bytearray
use midgard/bounded_item_v1
use midgard/fraud_proofs/native_tx/compact.{verify_native_tx_proof_source_v1}
use midgard/native_script_scan_v1
use midgard/native_tx_field_access_v1
use midgard/rejection_reason_v1.{
  reject_invalid_field_type, reject_native_script_depth,
  reject_native_script_invalid, reject_native_script_node_count,
}
use midgard/validation_machine/machine_types.{
  AuthenticatedNativeScriptTokenHeadV1, AuthenticatedNativeScriptWindowV1,
  NativeChunkWindowV1, NativeScriptFrameWitness, NativeScriptTokenWitness,
  NativeScriptsControlV1, NoAuxiliaryWitness, NoSignerSetProof,
  PhaseANativeScriptsControlV1, SignerMembershipProof, SignerSetProofV1,
  TransactionFieldChunkWitness, ValidationAuxiliaryWitnessV1,
  ValidationOneStepWitnessV1,
}
use midgard/validation_machine/shared.{
  max_aggregate_field_preimage_bytes, max_tx_size_derived_collection_item_count,
  rejected_successor_is_exact, signer_frontier_matches,
  signer_membership_is_valid, signer_non_membership_is_valid,
}
use midgard/validation_machine/witness.{
  encode_native_scripts_control_v1, encode_phase_a_native_scripts_scan_witness,
  encode_phase_a_script_preconditions_witness,
  native_scripts_control_from_witness, phase_a_native_control_from_witness,
  phase_a_native_control_is_bound, reset_phase_a_native_control,
}
use midgard/validation_machine_field_door_v1 as field_door
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

fn phase_a_native_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  post: validation_trace_v1.ValidationMachineStateV1,
  control: PhaseANativeScriptsControlV1,
) -> Bool {
  and {
    post.phase == validation_trace_v1.PhaseANativeScripts,
    post.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.PhaseANativeScripts,
      pre.program_counter + 1,
      encode_phase_a_native_scripts_scan_witness(
        control.compact_cbor,
        control.witness_set_compact_cbor,
        control.field_preimage_lengths_cbor,
        control.context_cbor,
        control.resolution_schedule_hash,
        control.stage,
        control.script_count,
        control.script_seen,
        control.contains_non_native_script,
        control.item_length,
        control.item_commitment,
        control.cursor,
        control.stack_root,
        control.stack_depth,
        control.node_count,
        control.result,
        control.signer_count,
        control.signer_peaks,
        control.continuation_cbor,
      ),
    ),
  }
}

fn phase_a_native_to_preconditions_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  post: validation_trace_v1.ValidationMachineStateV1,
  control: PhaseANativeScriptsControlV1,
) -> Bool {
  and {
    post.phase == validation_trace_v1.PhaseAScriptPreconditions,
    post.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.PhaseAScriptPreconditions,
      pre.program_counter + 1,
      encode_phase_a_script_preconditions_witness(
        control.compact_cbor,
        control.witness_set_compact_cbor,
        control.field_preimage_lengths_cbor,
        control.context_cbor,
        control.resolution_schedule_hash,
        control.signer_count,
        validation_merkle_v1.frontier_commitment(
          control.signer_count,
          control.signer_peaks,
        ),
        control.contains_non_native_script == 1,
        0,
        0,
        #"",
      ),
    ),
  }
}

fn phase_a_native_complete_script_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  post: validation_trace_v1.ValidationMachineStateV1,
  control: PhaseANativeScriptsControlV1,
  script_count: Int,
  script_seen: Int,
  contains_non_native_script: Int,
) -> Bool {
  if control.continuation_cbor != #"" {
    let native_control = native_scripts_control_from_witness(
      control.continuation_cbor,
    )
    let next_native_control = NativeScriptsControlV1 {
      ..native_control,
      execution_cursor: native_control.execution_cursor + 1,
    }
    and {
      script_count == 1,
      script_seen == 1,
      contains_non_native_script == 0,
      post.phase == validation_trace_v1.NativeScripts,
      post.work_root == validation_trace_v1.hash_work_witness(
        validation_trace_v1.NativeScripts,
        pre.program_counter + 1,
        encode_native_scripts_control_v1(next_native_control),
      ),
    }
  } else {
    let next_control = reset_phase_a_native_control(
      control,
      script_count,
      script_seen,
      contains_non_native_script,
    )
    if script_seen == script_count {
      phase_a_native_to_preconditions_is_exact(pre, post, next_control)
    } else {
      phase_a_native_successor_is_exact(pre, post, next_control)
    }
  }
}

fn verify_phase_a_native_advance_scan(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
) -> Bool {
  let Pair(verified, witness_set) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  and {
    phase_a_native_control_is_bound(
      pre,
      witness,
      control,
      verified,
      witness_set,
    ),
    control.stage == 0,
    control.script_count == 0,
    phase_a_native_to_preconditions_is_exact(
      pre,
      witness.claimed_successor,
      control,
    ),
  }
}

/// One §5.3 field-6 script item's head, and the decision whether to walk it.
///
/// This is one of the four sites that hand a *partially read* item to a later
/// step: `item_commitment` goes into the successor control and the token-head
/// scan opens chunk proofs against it. The commitment is now **derived** here
/// from bytes the door authenticated (`machine_field_item_commitment`) rather
/// than asserted by a prover's opening — see the door module's header on why
/// that continuation is not part of #592's defect.
fn verify_phase_a_native_item_scan(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
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
    6,
    control.script_seen,
    carriage,
  )
  let item_count = field_door.machine_field_item_count(item)
  let item_length = field_door.machine_field_item_length(item)
  let active_count = if control.script_count == -1 {
    item_count
  } else {
    control.script_count
  }
  let header = native_script_scan_v1.versioned_script_header_v1(
    field_door.machine_field_item_chunk(item, 0),
    item_length,
  )
  and {
    phase_a_native_control_is_bound(
      pre,
      witness,
      control,
      verified,
      witness_set,
    ),
    control.stage == 0,
    control.script_count != 0,
    control.script_count == -1 || control.script_count == item_count,
    active_count > 0,
    active_count <= max_tx_size_derived_collection_item_count,
    field_index == 6,
    item_index == control.script_seen,
    item_count == active_count,
    item_length > 0,
    item_length <= max_aggregate_field_preimage_bytes,
    when header is {
      None ->
        rejected_successor_is_exact(
          pre,
          witness.claimed_successor,
          reject_invalid_field_type,
        )
      Some(item_header) ->
        if item_header.language_tag == 0 {
          phase_a_native_successor_is_exact(
            pre,
            witness.claimed_successor,
            PhaseANativeScriptsControlV1 {
              ..control,
              stage: 1,
              script_count: active_count,
              item_length,
              item_commitment: field_door.machine_field_item_commitment(item),
              cursor: item_header.payload_offset,
            },
          )
        } else {
          phase_a_native_complete_script_is_exact(
            pre,
            witness.claimed_successor,
            control,
            active_count,
            control.script_seen + 1,
            1,
          )
        }
    },
  }
}

fn phase_a_native_chunk_window(
  control: PhaseANativeScriptsControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Option<NativeChunkWindowV1> {
  let expected_chunk_index = control.cursor / bounded_item_v1.chunk_bytes
  let count = bounded_item_v1.chunk_count(control.item_length)
  let current_matches = and {
    chunk_proof.field_index == 6,
    chunk_proof.item_index == control.script_seen,
    chunk_proof.total_length == control.item_length,
    chunk_proof.chunk_index == expected_chunk_index,
    bounded_item_v1.verify_chunk(control.item_commitment, chunk_proof),
  }
  if !current_matches {
    None
  } else if expected_chunk_index + 1 < count {
    when next_chunk_proof is {
      None -> None
      Some(next) ->
        if and {
          next.field_index == 6,
          next.item_index == control.script_seen,
          next.total_length == control.item_length,
          next.chunk_index == expected_chunk_index + 1,
          bounded_item_v1.verify_chunk(control.item_commitment, next),
        } {
          Some(
            NativeChunkWindowV1 {
              bytes: bytearray.concat(chunk_proof.chunk, next.chunk),
              offset: control.cursor - expected_chunk_index * bounded_item_v1.chunk_bytes,
            },
          )
        } else {
          None
        }
    }
  } else if next_chunk_proof == None {
    Some(
      NativeChunkWindowV1 {
        bytes: chunk_proof.chunk,
        offset: control.cursor - expected_chunk_index * bounded_item_v1.chunk_bytes,
      },
    )
  } else {
    None
  }
}

fn phase_a_native_signature_result(
  key_hash: ByteArray,
  control: PhaseANativeScriptsControlV1,
  proof: SignerSetProofV1,
) -> Option<Bool> {
  let frontier_commitment = validation_merkle_v1.frontier_commitment(
    control.signer_count,
    control.signer_peaks,
  )
  when proof is {
    SignerMembershipProof { peaks, signer_index, siblings } ->
      if signer_membership_is_valid(
        key_hash,
        control.signer_count,
        frontier_commitment,
        peaks,
        signer_index,
        siblings,
      ) {
        Some(True)
      } else {
        None
      }
    _ ->
      if signer_non_membership_is_valid(
        key_hash,
        control.signer_count,
        frontier_commitment,
        proof,
      ) {
        Some(False)
      } else {
        None
      }
  }
}

fn verify_phase_a_native_token_scan(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
  signer_proof: SignerSetProofV1,
  token_mode: Int,
) -> Bool {
  let Pair(verified, witness_set) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  let window = phase_a_native_chunk_window(
    control,
    chunk_proof,
    next_chunk_proof,
  )
  and {
    phase_a_native_control_is_bound(
      pre,
      witness,
      control,
      verified,
      witness_set,
    ),
    control.stage == 1,
    when window is {
      None -> False
      Some(authenticated) ->
        when
          native_script_scan_v1.token_at_v1(
            authenticated.bytes,
            authenticated.offset,
            control.cursor,
          )
        is {
          None ->
            rejected_successor_is_exact(
              pre,
              witness.claimed_successor,
              reject_invalid_field_type,
            )
          Some(token) -> {
            let next_node_count = control.node_count + 1
            if next_node_count > native_script_scan_v1.max_native_script_nodes {
              rejected_successor_is_exact(
                pre,
                witness.claimed_successor,
                reject_native_script_node_count,
              )
            } else if and {
              token.kind >= native_script_scan_v1.all_node,
              token.kind <= native_script_scan_v1.at_least_node,
              token.child_count > 0,
            } {
              when
                native_script_scan_v1.frame_for_token_v1(
                  token,
                  control.stack_root,
                )
              is {
                None -> False
                Some(frame) -> {
                  let next_depth = control.stack_depth + 1
                  if next_depth > native_script_scan_v1.max_native_script_depth {
                    rejected_successor_is_exact(
                      pre,
                      witness.claimed_successor,
                      reject_native_script_depth,
                    )
                  } else {
                    and {
                      token_mode != 1,
                      signer_proof == NoSignerSetProof,
                      token.next_offset <= control.item_length,
                      phase_a_native_successor_is_exact(
                        pre,
                        witness.claimed_successor,
                        PhaseANativeScriptsControlV1 {
                          ..control,
                          cursor: token.next_offset,
                          stack_root: native_script_scan_v1.hash_frame_v1(frame),
                          stack_depth: next_depth,
                          node_count: next_node_count,
                        },
                      ),
                    }
                  }
                }
              }
            } else {
              let result = if token.kind == native_script_scan_v1.signature_node {
                if token_mode != 0 {
                  phase_a_native_signature_result(
                    token.key_hash,
                    control,
                    signer_proof,
                  )
                } else {
                  None
                }
              } else if token.kind == native_script_scan_v1.after_node {
                if and {
                  token_mode != 1,
                  signer_proof == NoSignerSetProof,
                } {
                  Some(and {
                      verified.tx_compact.body.validity_interval_start >= 0,
                      verified.tx_compact.body.validity_interval_start >= token.slot,
                    })
                } else {
                  None
                }
              } else if token.kind == native_script_scan_v1.before_node {
                if and {
                  token_mode != 1,
                  signer_proof == NoSignerSetProof,
                } {
                  Some(and {
                      verified.tx_compact.body.validity_interval_end >= 0,
                      verified.tx_compact.body.validity_interval_end <= token.slot,
                    })
                } else {
                  None
                }
              } else if and {
                token_mode != 1,
                signer_proof == NoSignerSetProof,
              } {
                native_script_scan_v1.empty_container_result_v1(token)
              } else {
                None
              }
              when result is {
                None -> False
                Some(valid) -> and {
                    token.next_offset <= control.item_length,
                    phase_a_native_successor_is_exact(
                      pre,
                      witness.claimed_successor,
                      PhaseANativeScriptsControlV1 {
                        ..control,
                        stage: 2,
                        cursor: token.next_offset,
                        node_count: next_node_count,
                        result: if valid {
                          1
                        } else {
                          0
                        },
                      },
                    ),
                  }
              }
            }
          }
        }
    },
  }
}

fn verify_phase_a_native_frame_scan(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  frame: native_script_scan_v1.NativeScriptFrameV1,
) -> Bool {
  let Pair(verified, witness_set) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  and {
    phase_a_native_control_is_bound(
      pre,
      witness,
      control,
      verified,
      witness_set,
    ),
    control.stage == 2,
    bytearray.length(control.stack_root) == 32,
    control.stack_depth > 0,
    native_script_scan_v1.hash_frame_v1(frame) == control.stack_root,
    when native_script_scan_v1.apply_child_v1(frame, control.result == 1) is {
      None -> False
      Some(native_script_scan_v1.NativeFramePending { frame: next_frame }) ->
        phase_a_native_successor_is_exact(
          pre,
          witness.claimed_successor,
          PhaseANativeScriptsControlV1 {
            ..control,
            stage: 1,
            stack_root: native_script_scan_v1.hash_frame_v1(next_frame),
            result: -1,
          },
        )
      Some(native_script_scan_v1.NativeFrameComplete { tail, valid }) ->
        phase_a_native_successor_is_exact(
          pre,
          witness.claimed_successor,
          PhaseANativeScriptsControlV1 {
            ..control,
            stack_root: tail,
            stack_depth: control.stack_depth - 1,
            result: if valid {
              1
            } else {
              0
            },
          },
        )
    },
  }
}

fn verify_phase_a_native_finalize_scan(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
) -> Bool {
  let Pair(verified, witness_set) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  and {
    phase_a_native_control_is_bound(
      pre,
      witness,
      control,
      verified,
      witness_set,
    ),
    control.stage == 2,
    control.stack_root == #"",
    control.stack_depth == 0,
    if control.cursor != control.item_length {
      rejected_successor_is_exact(
        pre,
        witness.claimed_successor,
        reject_invalid_field_type,
      )
    } else if control.result == 0 {
      rejected_successor_is_exact(
        pre,
        witness.claimed_successor,
        reject_native_script_invalid,
      )
    } else {
      phase_a_native_complete_script_is_exact(
        pre,
        witness.claimed_successor,
        control,
        control.script_count,
        control.script_seen + 1,
        control.contains_non_native_script,
      )
    },
  }
}

pub fn verify_phase_a_native_scripts_scan(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  door: field_door.MachineFieldDoorV1,
) -> Bool {
  let control = phase_a_native_control_from_witness(witness.work_witness_cbor)
  when auxiliary is {
    NoAuxiliaryWitness ->
      if control.stage == 2 {
        verify_phase_a_native_finalize_scan(pre, witness, control)
      } else {
        verify_phase_a_native_advance_scan(pre, witness, control)
      }
    TransactionFieldChunkWitness { field_index, item_index, carriage } ->
      verify_phase_a_native_item_scan(
        pre,
        witness,
        control,
        door,
        field_index,
        item_index,
        carriage,
      )
    NativeScriptTokenWitness { chunk_proof, next_chunk_proof, signer_proof } ->
      if control.stage == 1 {
        and {
          signer_proof == NoSignerSetProof,
          verify_phase_a_native_token_head_scan_v1(
            pre,
            witness,
            control,
            chunk_proof,
            next_chunk_proof,
          ),
        }
      } else if control.stage == 3 {
        verify_phase_a_native_signature_payload_scan_v1(
          pre,
          witness,
          control,
          chunk_proof,
          next_chunk_proof,
          signer_proof,
        )
      } else if or {
        control.stage == 4,
        control.stage == 5,
      } {
        and {
          signer_proof == NoSignerSetProof,
          or {
            verify_phase_a_native_all_or_any_container_frame_payload_scan_v1(
              pre,
              witness,
              control,
              chunk_proof,
              next_chunk_proof,
            ),
            verify_phase_a_native_all_or_any_empty_container_payload_scan_v1(
              pre,
              witness,
              control,
              chunk_proof,
              next_chunk_proof,
            ),
          },
        }
      } else if control.stage == 6 {
        and {
          signer_proof == NoSignerSetProof,
          or {
            verify_phase_a_native_at_least_container_frame_payload_scan_v1(
              pre,
              witness,
              control,
              chunk_proof,
              next_chunk_proof,
            ),
            verify_phase_a_native_at_least_empty_container_payload_scan_v1(
              pre,
              witness,
              control,
              chunk_proof,
              next_chunk_proof,
            ),
          },
        }
      } else if or {
        control.stage == 7,
        control.stage == 8,
      } {
        and {
          signer_proof == NoSignerSetProof,
          verify_phase_a_native_timelock_payload_scan_v1(
            pre,
            witness,
            control,
            chunk_proof,
            next_chunk_proof,
          ),
        }
      } else {
        False
      }
    NativeScriptFrameWitness { frame } ->
      verify_phase_a_native_frame_scan(pre, witness, control, frame)
    _ -> False
  }
}

pub fn verify_phase_a_native_advance_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let control = phase_a_native_control_from_witness(witness.work_witness_cbor)
  if control.stage == 2 {
    verify_phase_a_native_finalize_scan(pre, witness, control)
  } else {
    verify_phase_a_native_advance_scan(pre, witness, control)
  }
}

pub fn verify_phase_a_native_item_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  door: field_door.MachineFieldDoorV1,
  field_index: Int,
  item_index: Int,
  carriage: native_tx_field_access_v1.FieldCarriageV1,
) -> Bool {
  verify_phase_a_native_item_scan(
    pre,
    witness,
    phase_a_native_control_from_witness(witness.work_witness_cbor),
    door,
    field_index,
    item_index,
    carriage,
  )
}

pub fn verify_phase_a_native_token_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
  signer_proof: SignerSetProofV1,
) -> Bool {
  verify_phase_a_native_token_scan(
    pre,
    witness,
    phase_a_native_control_from_witness(witness.work_witness_cbor),
    chunk_proof,
    next_chunk_proof,
    signer_proof,
    2,
  )
}

fn authenticated_phase_a_native_window_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Option<AuthenticatedNativeScriptWindowV1> {
  let Pair(verified, witness_set) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  if phase_a_native_control_is_bound(
    pre,
    witness,
    control,
    verified,
    witness_set,
  ) {
    when
      phase_a_native_chunk_window(control, chunk_proof, next_chunk_proof)
    is {
      None -> None
      Some(authenticated) ->
        Some(
          AuthenticatedNativeScriptWindowV1 { verified, window: authenticated },
        )
    }
  } else {
    None
  }
}

fn authenticated_phase_a_native_token_head_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Option<AuthenticatedNativeScriptTokenHeadV1> {
  when
    authenticated_phase_a_native_window_v1(
      pre,
      witness,
      control,
      chunk_proof,
      next_chunk_proof,
    )
  is {
    None -> None
    Some(authenticated) ->
      if control.stage != 1 {
        None
      } else {
        Some(
          AuthenticatedNativeScriptTokenHeadV1 {
            verified: authenticated.verified,
            window: authenticated.window,
            head: native_script_scan_v1.token_head_at_v1(
              authenticated.window.bytes,
              authenticated.window.offset,
            ),
          },
        )
      }
  }
}

fn verify_phase_a_native_token_head_scan_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Bool {
  when
    authenticated_phase_a_native_token_head_v1(
      pre,
      witness,
      control,
      chunk_proof,
      next_chunk_proof,
    )
  is {
    None -> False
    Some(authenticated) ->
      when authenticated.head is {
        None ->
          rejected_successor_is_exact(
            pre,
            witness.claimed_successor,
            reject_invalid_field_type,
          )
        Some(head) ->
          if !native_script_scan_v1.token_head_is_well_formed_v1(head) {
            rejected_successor_is_exact(
              pre,
              witness.claimed_successor,
              reject_invalid_field_type,
            )
          } else {
            let next_node_count = control.node_count + 1
            let next_offset = control.cursor + head.next_offset - authenticated.window.offset
            if next_node_count > native_script_scan_v1.max_native_script_nodes {
              rejected_successor_is_exact(
                pre,
                witness.claimed_successor,
                reject_native_script_node_count,
              )
            } else {
              and {
                next_offset <= control.item_length,
                phase_a_native_successor_is_exact(
                  pre,
                  witness.claimed_successor,
                  PhaseANativeScriptsControlV1 {
                    ..control,
                    stage: head.tag + 3,
                    cursor: next_offset,
                    node_count: next_node_count,
                  },
                ),
              }
            }
          }
      }
  }
}

fn phase_a_native_container_frame_payload_successor_is_exact_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  token: native_script_scan_v1.NativeScriptTokenV1,
) -> Bool {
  if token.child_count <= 0 {
    False
  } else {
    when
      native_script_scan_v1.frame_for_token_v1(token, control.stack_root)
    is {
      None -> False
      Some(frame) -> {
        let next_depth = control.stack_depth + 1
        if next_depth > native_script_scan_v1.max_native_script_depth {
          rejected_successor_is_exact(
            pre,
            witness.claimed_successor,
            reject_native_script_depth,
          )
        } else {
          and {
            token.next_offset <= control.item_length,
            phase_a_native_successor_is_exact(
              pre,
              witness.claimed_successor,
              PhaseANativeScriptsControlV1 {
                ..control,
                stage: 1,
                cursor: token.next_offset,
                stack_root: native_script_scan_v1.hash_frame_v1(frame),
                stack_depth: next_depth,
              },
            ),
          }
        }
      }
    }
  }
}

fn phase_a_native_empty_container_payload_successor_is_exact_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  token: native_script_scan_v1.NativeScriptTokenV1,
) -> Bool {
  if token.child_count != 0 {
    False
  } else {
    when native_script_scan_v1.empty_container_result_v1(token) is {
      None -> False
      Some(valid) -> and {
          token.next_offset <= control.item_length,
          phase_a_native_successor_is_exact(
            pre,
            witness.claimed_successor,
            PhaseANativeScriptsControlV1 {
              ..control,
              stage: 2,
              cursor: token.next_offset,
              result: if valid {
                1
              } else {
                0
              },
            },
          ),
        }
    }
  }
}

fn authenticated_phase_a_native_all_or_any_payload_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Option<Option<native_script_scan_v1.NativeScriptTokenV1>> {
  when
    authenticated_phase_a_native_window_v1(
      pre,
      witness,
      control,
      chunk_proof,
      next_chunk_proof,
    )
  is {
    None -> None
    Some(authenticated) ->
      if and {
        control.stage != 4,
        control.stage != 5,
      } {
        None
      } else {
        Some(
          native_script_scan_v1.all_or_any_payload_at_v1(
            authenticated.window.bytes,
            authenticated.window.offset,
            control.cursor,
            control.stage - 3,
          ),
        )
      }
  }
}

fn authenticated_phase_a_native_at_least_payload_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Option<Option<native_script_scan_v1.NativeScriptTokenV1>> {
  when
    authenticated_phase_a_native_window_v1(
      pre,
      witness,
      control,
      chunk_proof,
      next_chunk_proof,
    )
  is {
    None -> None
    Some(authenticated) ->
      if control.stage != 6 {
        None
      } else {
        Some(
          native_script_scan_v1.at_least_payload_at_v1(
            authenticated.window.bytes,
            authenticated.window.offset,
            control.cursor,
          ),
        )
      }
  }
}

fn verify_phase_a_native_all_or_any_container_frame_payload_scan_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Bool {
  when
    authenticated_phase_a_native_all_or_any_payload_v1(
      pre,
      witness,
      control,
      chunk_proof,
      next_chunk_proof,
    )
  is {
    None -> False
    Some(None) ->
      rejected_successor_is_exact(
        pre,
        witness.claimed_successor,
        reject_invalid_field_type,
      )
    Some(Some(token)) ->
      phase_a_native_container_frame_payload_successor_is_exact_v1(
        pre,
        witness,
        control,
        token,
      )
  }
}

fn verify_phase_a_native_all_or_any_empty_container_payload_scan_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Bool {
  when
    authenticated_phase_a_native_all_or_any_payload_v1(
      pre,
      witness,
      control,
      chunk_proof,
      next_chunk_proof,
    )
  is {
    None -> False
    Some(None) ->
      rejected_successor_is_exact(
        pre,
        witness.claimed_successor,
        reject_invalid_field_type,
      )
    Some(Some(token)) ->
      phase_a_native_empty_container_payload_successor_is_exact_v1(
        pre,
        witness,
        control,
        token,
      )
  }
}

fn verify_phase_a_native_at_least_container_frame_payload_scan_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Bool {
  when
    authenticated_phase_a_native_at_least_payload_v1(
      pre,
      witness,
      control,
      chunk_proof,
      next_chunk_proof,
    )
  is {
    None -> False
    Some(None) ->
      rejected_successor_is_exact(
        pre,
        witness.claimed_successor,
        reject_invalid_field_type,
      )
    Some(Some(token)) ->
      phase_a_native_container_frame_payload_successor_is_exact_v1(
        pre,
        witness,
        control,
        token,
      )
  }
}

fn verify_phase_a_native_at_least_empty_container_payload_scan_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Bool {
  when
    authenticated_phase_a_native_at_least_payload_v1(
      pre,
      witness,
      control,
      chunk_proof,
      next_chunk_proof,
    )
  is {
    None -> False
    Some(None) ->
      rejected_successor_is_exact(
        pre,
        witness.claimed_successor,
        reject_invalid_field_type,
      )
    Some(Some(token)) ->
      phase_a_native_empty_container_payload_successor_is_exact_v1(
        pre,
        witness,
        control,
        token,
      )
  }
}

fn phase_a_native_container_frame_successor_is_exact_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  token: native_script_scan_v1.NativeScriptTokenV1,
) -> Bool {
  let next_node_count = control.node_count + 1
  if next_node_count > native_script_scan_v1.max_native_script_nodes {
    rejected_successor_is_exact(
      pre,
      witness.claimed_successor,
      reject_native_script_node_count,
    )
  } else if token.child_count <= 0 {
    False
  } else {
    when
      native_script_scan_v1.frame_for_token_v1(token, control.stack_root)
    is {
      None -> False
      Some(frame) -> {
        let next_depth = control.stack_depth + 1
        if next_depth > native_script_scan_v1.max_native_script_depth {
          rejected_successor_is_exact(
            pre,
            witness.claimed_successor,
            reject_native_script_depth,
          )
        } else {
          and {
            token.next_offset <= control.item_length,
            phase_a_native_successor_is_exact(
              pre,
              witness.claimed_successor,
              PhaseANativeScriptsControlV1 {
                ..control,
                cursor: token.next_offset,
                stack_root: native_script_scan_v1.hash_frame_v1(frame),
                stack_depth: next_depth,
                node_count: next_node_count,
              },
            ),
          }
        }
      }
    }
  }
}

fn phase_a_native_empty_container_successor_is_exact_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  token: native_script_scan_v1.NativeScriptTokenV1,
) -> Bool {
  let next_node_count = control.node_count + 1
  if next_node_count > native_script_scan_v1.max_native_script_nodes {
    rejected_successor_is_exact(
      pre,
      witness.claimed_successor,
      reject_native_script_node_count,
    )
  } else if token.child_count != 0 {
    False
  } else {
    when native_script_scan_v1.empty_container_result_v1(token) is {
      None -> False
      Some(valid) -> and {
          token.next_offset <= control.item_length,
          phase_a_native_successor_is_exact(
            pre,
            witness.claimed_successor,
            PhaseANativeScriptsControlV1 {
              ..control,
              stage: 2,
              cursor: token.next_offset,
              node_count: next_node_count,
              result: if valid {
                1
              } else {
                0
              },
            },
          ),
        }
    }
  }
}

fn verify_phase_a_native_container_frame_token_scan_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Bool {
  when
    authenticated_phase_a_native_token_head_v1(
      pre,
      witness,
      control,
      chunk_proof,
      next_chunk_proof,
    )
  is {
    None -> False
    Some(authenticated) ->
      when authenticated.head is {
        None ->
          rejected_successor_is_exact(
            pre,
            witness.claimed_successor,
            reject_invalid_field_type,
          )
        Some(head) ->
          if and {
            head.tag >= native_script_scan_v1.all_node,
            head.tag <= native_script_scan_v1.at_least_node,
          } {
            when
              native_script_scan_v1.container_token_from_head_v1(
                authenticated.window.bytes,
                authenticated.window.offset,
                control.cursor,
                head,
              )
            is {
              None ->
                rejected_successor_is_exact(
                  pre,
                  witness.claimed_successor,
                  reject_invalid_field_type,
                )
              Some(token) ->
                phase_a_native_container_frame_successor_is_exact_v1(
                  pre,
                  witness,
                  control,
                  token,
                )
            }
          } else if or {
            head.tag == native_script_scan_v1.signature_node,
            head.tag == native_script_scan_v1.after_node,
            head.tag == native_script_scan_v1.before_node,
          } {
            False
          } else {
            rejected_successor_is_exact(
              pre,
              witness.claimed_successor,
              reject_invalid_field_type,
            )
          }
      }
  }
}

fn verify_phase_a_native_empty_container_token_scan_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Bool {
  when
    authenticated_phase_a_native_token_head_v1(
      pre,
      witness,
      control,
      chunk_proof,
      next_chunk_proof,
    )
  is {
    None -> False
    Some(authenticated) ->
      when authenticated.head is {
        None ->
          rejected_successor_is_exact(
            pre,
            witness.claimed_successor,
            reject_invalid_field_type,
          )
        Some(head) ->
          if and {
            head.tag >= native_script_scan_v1.all_node,
            head.tag <= native_script_scan_v1.at_least_node,
          } {
            when
              native_script_scan_v1.container_token_from_head_v1(
                authenticated.window.bytes,
                authenticated.window.offset,
                control.cursor,
                head,
              )
            is {
              None ->
                rejected_successor_is_exact(
                  pre,
                  witness.claimed_successor,
                  reject_invalid_field_type,
                )
              Some(token) ->
                phase_a_native_empty_container_successor_is_exact_v1(
                  pre,
                  witness,
                  control,
                  token,
                )
            }
          } else if or {
            head.tag == native_script_scan_v1.signature_node,
            head.tag == native_script_scan_v1.after_node,
            head.tag == native_script_scan_v1.before_node,
          } {
            False
          } else {
            rejected_successor_is_exact(
              pre,
              witness.claimed_successor,
              reject_invalid_field_type,
            )
          }
      }
  }
}

fn verify_phase_a_native_timelock_token_scan_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Bool {
  when
    authenticated_phase_a_native_token_head_v1(
      pre,
      witness,
      control,
      chunk_proof,
      next_chunk_proof,
    )
  is {
    None -> False
    Some(authenticated) ->
      when authenticated.head is {
        None ->
          rejected_successor_is_exact(
            pre,
            witness.claimed_successor,
            reject_invalid_field_type,
          )
        Some(head) ->
          if or {
            head.tag == native_script_scan_v1.after_node,
            head.tag == native_script_scan_v1.before_node,
          } {
            when
              native_script_scan_v1.timelock_token_from_head_v1(
                authenticated.window.bytes,
                authenticated.window.offset,
                control.cursor,
                head,
              )
            is {
              None ->
                rejected_successor_is_exact(
                  pre,
                  witness.claimed_successor,
                  reject_invalid_field_type,
                )
              Some(token) -> {
                let next_node_count = control.node_count + 1
                if next_node_count > native_script_scan_v1.max_native_script_nodes {
                  rejected_successor_is_exact(
                    pre,
                    witness.claimed_successor,
                    reject_native_script_node_count,
                  )
                } else {
                  let valid = if token.kind == native_script_scan_v1.after_node {
                    and {
                      authenticated.verified.tx_compact.body.validity_interval_start >= 0,
                      authenticated.verified.tx_compact.body.validity_interval_start >= token.slot,
                    }
                  } else {
                    and {
                      authenticated.verified.tx_compact.body.validity_interval_end >= 0,
                      authenticated.verified.tx_compact.body.validity_interval_end <= token.slot,
                    }
                  }
                  and {
                    token.next_offset <= control.item_length,
                    phase_a_native_successor_is_exact(
                      pre,
                      witness.claimed_successor,
                      PhaseANativeScriptsControlV1 {
                        ..control,
                        stage: 2,
                        cursor: token.next_offset,
                        node_count: next_node_count,
                        result: if valid {
                          1
                        } else {
                          0
                        },
                      },
                    ),
                  }
                }
              }
            }
          } else if and {
            head.tag >= native_script_scan_v1.signature_node,
            head.tag <= native_script_scan_v1.at_least_node,
          } {
            False
          } else {
            rejected_successor_is_exact(
              pre,
              witness.claimed_successor,
              reject_invalid_field_type,
            )
          }
      }
  }
}

fn verify_phase_a_native_signature_token_scan_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
  signer_proof: SignerSetProofV1,
) -> Bool {
  when
    authenticated_phase_a_native_token_head_v1(
      pre,
      witness,
      control,
      chunk_proof,
      next_chunk_proof,
    )
  is {
    None -> False
    Some(authenticated) ->
      when authenticated.head is {
        None ->
          rejected_successor_is_exact(
            pre,
            witness.claimed_successor,
            reject_invalid_field_type,
          )
        Some(head) ->
          if head.tag == native_script_scan_v1.signature_node {
            when
              native_script_scan_v1.signature_token_from_head_v1(
                authenticated.window.bytes,
                authenticated.window.offset,
                control.cursor,
                head,
              )
            is {
              None ->
                rejected_successor_is_exact(
                  pre,
                  witness.claimed_successor,
                  reject_invalid_field_type,
                )
              Some(token) -> {
                let next_node_count = control.node_count + 1
                if next_node_count > native_script_scan_v1.max_native_script_nodes {
                  rejected_successor_is_exact(
                    pre,
                    witness.claimed_successor,
                    reject_native_script_node_count,
                  )
                } else {
                  when
                    phase_a_native_signature_result(
                      token.key_hash,
                      control,
                      signer_proof,
                    )
                  is {
                    None -> False
                    Some(valid) -> and {
                        token.next_offset <= control.item_length,
                        phase_a_native_successor_is_exact(
                          pre,
                          witness.claimed_successor,
                          PhaseANativeScriptsControlV1 {
                            ..control,
                            stage: 2,
                            cursor: token.next_offset,
                            node_count: next_node_count,
                            result: if valid {
                              1
                            } else {
                              0
                            },
                          },
                        ),
                      }
                  }
                }
              }
            }
          } else if and {
            head.tag >= native_script_scan_v1.all_node,
            head.tag <= native_script_scan_v1.before_node,
          } {
            False
          } else {
            rejected_successor_is_exact(
              pre,
              witness.claimed_successor,
              reject_invalid_field_type,
            )
          }
      }
  }
}

fn verify_phase_a_native_timelock_payload_scan_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Bool {
  when
    authenticated_phase_a_native_window_v1(
      pre,
      witness,
      control,
      chunk_proof,
      next_chunk_proof,
    )
  is {
    None -> False
    Some(authenticated) ->
      if and {
        control.stage != 7,
        control.stage != 8,
      } {
        False
      } else {
        when
          native_script_scan_v1.timelock_payload_at_v1(
            authenticated.window.bytes,
            authenticated.window.offset,
            control.cursor,
            control.stage - 3,
          )
        is {
          None ->
            rejected_successor_is_exact(
              pre,
              witness.claimed_successor,
              reject_invalid_field_type,
            )
          Some(token) -> {
            let valid = if token.kind == native_script_scan_v1.after_node {
              and {
                authenticated.verified.tx_compact.body.validity_interval_start >= 0,
                authenticated.verified.tx_compact.body.validity_interval_start >= token.slot,
              }
            } else {
              and {
                authenticated.verified.tx_compact.body.validity_interval_end >= 0,
                authenticated.verified.tx_compact.body.validity_interval_end <= token.slot,
              }
            }
            and {
              token.next_offset <= control.item_length,
              phase_a_native_successor_is_exact(
                pre,
                witness.claimed_successor,
                PhaseANativeScriptsControlV1 {
                  ..control,
                  stage: 2,
                  cursor: token.next_offset,
                  result: if valid {
                    1
                  } else {
                    0
                  },
                },
              ),
            }
          }
        }
      }
  }
}

fn verify_phase_a_native_signature_payload_with_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
  signer_result: fn(ByteArray, PhaseANativeScriptsControlV1) -> Option<Bool>,
) -> Bool {
  when
    authenticated_phase_a_native_window_v1(
      pre,
      witness,
      control,
      chunk_proof,
      next_chunk_proof,
    )
  is {
    None -> False
    Some(authenticated) ->
      if control.stage != 3 {
        False
      } else {
        when
          native_script_scan_v1.signature_payload_at_v1(
            authenticated.window.bytes,
            authenticated.window.offset,
            control.cursor,
          )
        is {
          None ->
            rejected_successor_is_exact(
              pre,
              witness.claimed_successor,
              reject_invalid_field_type,
            )
          Some(token) ->
            when signer_result(token.key_hash, control) is {
              None -> False
              Some(valid) -> and {
                  token.next_offset <= control.item_length,
                  phase_a_native_successor_is_exact(
                    pre,
                    witness.claimed_successor,
                    PhaseANativeScriptsControlV1 {
                      ..control,
                      stage: 2,
                      cursor: token.next_offset,
                      result: if valid {
                        1
                      } else {
                        0
                      },
                    },
                  ),
                }
            }
        }
      }
  }
}

fn verify_phase_a_native_signature_payload_scan_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
  signer_proof: SignerSetProofV1,
) -> Bool {
  verify_phase_a_native_signature_payload_with_v1(
    pre,
    witness,
    control,
    chunk_proof,
    next_chunk_proof,
    fn(key_hash, bound_control) {
      phase_a_native_signature_result(key_hash, bound_control, signer_proof)
    },
  )
}

pub fn verify_phase_a_native_token_head_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Bool {
  verify_phase_a_native_token_head_scan_v1(
    pre,
    witness,
    phase_a_native_control_from_witness(witness.work_witness_cbor),
    chunk_proof,
    next_chunk_proof,
  )
}

pub fn verify_phase_a_native_all_or_any_container_frame_payload_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Bool {
  verify_phase_a_native_all_or_any_container_frame_payload_scan_v1(
    pre,
    witness,
    phase_a_native_control_from_witness(witness.work_witness_cbor),
    chunk_proof,
    next_chunk_proof,
  )
}

pub fn verify_phase_a_native_all_or_any_empty_container_payload_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Bool {
  verify_phase_a_native_all_or_any_empty_container_payload_scan_v1(
    pre,
    witness,
    phase_a_native_control_from_witness(witness.work_witness_cbor),
    chunk_proof,
    next_chunk_proof,
  )
}

pub fn verify_phase_a_native_at_least_container_frame_payload_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Bool {
  verify_phase_a_native_at_least_container_frame_payload_scan_v1(
    pre,
    witness,
    phase_a_native_control_from_witness(witness.work_witness_cbor),
    chunk_proof,
    next_chunk_proof,
  )
}

pub fn verify_phase_a_native_at_least_empty_container_payload_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Bool {
  verify_phase_a_native_at_least_empty_container_payload_scan_v1(
    pre,
    witness,
    phase_a_native_control_from_witness(witness.work_witness_cbor),
    chunk_proof,
    next_chunk_proof,
  )
}

pub fn verify_phase_a_native_timelock_payload_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Bool {
  verify_phase_a_native_timelock_payload_scan_v1(
    pre,
    witness,
    phase_a_native_control_from_witness(witness.work_witness_cbor),
    chunk_proof,
    next_chunk_proof,
  )
}

pub fn verify_phase_a_native_signature_payload_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
  signer_proof: SignerSetProofV1,
) -> Bool {
  verify_phase_a_native_signature_payload_scan_v1(
    pre,
    witness,
    phase_a_native_control_from_witness(witness.work_witness_cbor),
    chunk_proof,
    next_chunk_proof,
    signer_proof,
  )
}

pub fn verify_phase_a_native_signature_membership_payload_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
  peaks: List<validation_merkle_v1.FrontierPeak>,
  signer_index: Int,
  siblings: List<ByteArray>,
) -> Bool {
  verify_phase_a_native_signature_payload_with_v1(
    pre,
    witness,
    phase_a_native_control_from_witness(witness.work_witness_cbor),
    chunk_proof,
    next_chunk_proof,
    fn(key_hash, control) {
      let frontier_commitment = validation_merkle_v1.frontier_commitment(
        control.signer_count,
        control.signer_peaks,
      )
      if signer_membership_is_valid(
        key_hash,
        control.signer_count,
        frontier_commitment,
        peaks,
        signer_index,
        siblings,
      ) {
        Some(True)
      } else {
        None
      }
    },
  )
}

pub fn verify_phase_a_native_signature_empty_payload_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
  peaks: List<validation_merkle_v1.FrontierPeak>,
) -> Bool {
  verify_phase_a_native_signature_payload_with_v1(
    pre,
    witness,
    phase_a_native_control_from_witness(witness.work_witness_cbor),
    chunk_proof,
    next_chunk_proof,
    fn(_key_hash, control) {
      let frontier_commitment = validation_merkle_v1.frontier_commitment(
        control.signer_count,
        control.signer_peaks,
      )
      if and {
        control.signer_count == 0,
        signer_frontier_matches(
          control.signer_count,
          frontier_commitment,
          peaks,
        ),
      } {
        Some(False)
      } else {
        None
      }
    },
  )
}

pub fn verify_phase_a_native_signature_below_first_payload_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
  peaks: List<validation_merkle_v1.FrontierPeak>,
  first_signer_hash: ByteArray,
  siblings: List<ByteArray>,
) -> Bool {
  verify_phase_a_native_signature_payload_with_v1(
    pre,
    witness,
    phase_a_native_control_from_witness(witness.work_witness_cbor),
    chunk_proof,
    next_chunk_proof,
    fn(key_hash, control) {
      let frontier_commitment = validation_merkle_v1.frontier_commitment(
        control.signer_count,
        control.signer_peaks,
      )
      if and {
        bytearray.compare(key_hash, first_signer_hash) == Less,
        signer_membership_is_valid(
          first_signer_hash,
          control.signer_count,
          frontier_commitment,
          peaks,
          0,
          siblings,
        ),
      } {
        Some(False)
      } else {
        None
      }
    },
  )
}

pub fn verify_phase_a_native_signature_above_last_payload_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
  peaks: List<validation_merkle_v1.FrontierPeak>,
  last_signer_hash: ByteArray,
  siblings: List<ByteArray>,
) -> Bool {
  verify_phase_a_native_signature_payload_with_v1(
    pre,
    witness,
    phase_a_native_control_from_witness(witness.work_witness_cbor),
    chunk_proof,
    next_chunk_proof,
    fn(key_hash, control) {
      let frontier_commitment = validation_merkle_v1.frontier_commitment(
        control.signer_count,
        control.signer_peaks,
      )
      if and {
        control.signer_count > 0,
        bytearray.compare(last_signer_hash, key_hash) == Less,
        signer_membership_is_valid(
          last_signer_hash,
          control.signer_count,
          frontier_commitment,
          peaks,
          control.signer_count - 1,
          siblings,
        ),
      } {
        Some(False)
      } else {
        None
      }
    },
  )
}

pub fn verify_phase_a_native_signature_between_payload_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
  peaks: List<validation_merkle_v1.FrontierPeak>,
  lower_index: Int,
  lower_signer_hash: ByteArray,
  lower_siblings: List<ByteArray>,
  upper_signer_hash: ByteArray,
  upper_siblings: List<ByteArray>,
) -> Bool {
  verify_phase_a_native_signature_payload_with_v1(
    pre,
    witness,
    phase_a_native_control_from_witness(witness.work_witness_cbor),
    chunk_proof,
    next_chunk_proof,
    fn(key_hash, control) {
      let frontier_commitment = validation_merkle_v1.frontier_commitment(
        control.signer_count,
        control.signer_peaks,
      )
      if and {
        lower_index >= 0,
        lower_index + 1 < control.signer_count,
        bytearray.compare(lower_signer_hash, key_hash) == Less,
        bytearray.compare(key_hash, upper_signer_hash) == Less,
        signer_membership_is_valid(
          lower_signer_hash,
          control.signer_count,
          frontier_commitment,
          peaks,
          lower_index,
          lower_siblings,
        ),
        signer_membership_is_valid(
          upper_signer_hash,
          control.signer_count,
          frontier_commitment,
          peaks,
          lower_index + 1,
          upper_siblings,
        ),
      } {
        Some(False)
      } else {
        None
      }
    },
  )
}

pub fn verify_phase_a_native_container_frame_token_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Bool {
  verify_phase_a_native_container_frame_token_scan_v1(
    pre,
    witness,
    phase_a_native_control_from_witness(witness.work_witness_cbor),
    chunk_proof,
    next_chunk_proof,
  )
}

pub fn verify_phase_a_native_empty_container_token_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Bool {
  verify_phase_a_native_empty_container_token_scan_v1(
    pre,
    witness,
    phase_a_native_control_from_witness(witness.work_witness_cbor),
    chunk_proof,
    next_chunk_proof,
  )
}

pub fn verify_phase_a_native_timelock_token_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Bool {
  verify_phase_a_native_timelock_token_scan_v1(
    pre,
    witness,
    phase_a_native_control_from_witness(witness.work_witness_cbor),
    chunk_proof,
    next_chunk_proof,
  )
}

pub fn verify_phase_a_native_signature_token_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
  signer_proof: SignerSetProofV1,
) -> Bool {
  verify_phase_a_native_signature_token_scan_v1(
    pre,
    witness,
    phase_a_native_control_from_witness(witness.work_witness_cbor),
    chunk_proof,
    next_chunk_proof,
    signer_proof,
  )
}

pub fn verify_phase_a_native_frame_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  frame: native_script_scan_v1.NativeScriptFrameV1,
) -> Bool {
  verify_phase_a_native_frame_scan(
    pre,
    witness,
    phase_a_native_control_from_witness(witness.work_witness_cbor),
    frame,
  )
}
