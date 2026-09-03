use aiken/primitive/bytearray
use midgard/bounded_item_v1
use midgard/fraud_proofs/native_tx/codec.{
  decode_canonical_int_at, decode_definite_array_header_at,
}
use midgard/fraud_proofs/native_tx/compact.{verify_native_tx_proof_source_v1}
use midgard/fraud_proofs/native_tx/preimages.{
  canonical_bytes_key_precedes, decode_canonical_bytes_at,
  decode_canonical_map_header_at,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddress, NativeTxWitnessSetCompact, PubKeyCredential, ScriptCredential,
  VerifiedMidgardNativeTxCompact,
}
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_proof_v1
use midgard/ledger_output_v1
use midgard/native_tx_field_access_v1
use midgard/rejection_reason_v1.{
  reject_asset_count, reject_invalid_field_type, reject_invalid_output,
  reject_missing_required_witness, reject_native_script_depth,
  reject_native_script_node_count, reject_network_id_mismatch,
}
use midgard/script_proof_v1
use midgard/validation_machine/machine_types.{
  InputSignerAuthorizationV1, InputSignerAuthorized, InputSignerMissing,
  InputSignerProofMalformed, LedgerOutputProofBeginWitness,
  LedgerOutputProofFinalizeWitness, LedgerOutputProofStepWitness,
  MintChunkWindowV1, MintFoldAssetWitness, MintFoldControlV1, NoAuxiliaryWitness,
  NoSignerSetProof, ReceivePurposeScanControlV1, ResolvedInputReplayWitness,
  ScriptSourcesControlV1, SignerSetProofV1, TransactionFieldChunkWitness,
  TransactionRedeemerItemBeginWitness, ValidationAuxiliaryWitnessV1,
  ValidationOneStepWitnessV1,
}
use midgard/validation_machine/script_sources_early_stages.{
  script_sources_control_successor_is_exact,
}
use midgard/validation_machine/shared.{
  decode_validation_context, max_aggregate_field_preimage_bytes,
  max_serialized_output_preimage_bytes,
  max_tx_size_derived_collection_item_count, mint_asset_leaf_hash,
  payment_credential_signer_authorization, rejected_successor_is_exact,
}
use midgard/validation_machine/witness.{
  empty_receive_purpose_scan_control, empty_resolution_schedule_hash,
  encode_script_sources_witness, resolution_schedule_node_hash,
  resolved_input_accumulator_successor,
}
use midgard/validation_machine_field_door_v1 as field_door
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

fn replay_source_frontier(
  control: ScriptSourcesControlV1,
  source_kind: Int,
  key: ByteArray,
  descriptor: ledger_output_commitment_v1.LedgerOutputCommitmentV1,
) -> validation_merkle_v1.BuiltFrontier {
  if and {
    source_kind == 1,
    descriptor.reference_script_language != -1,
  } {
    validation_merkle_v1.BuiltFrontier {
      count: control.source_count + 1,
      peaks: validation_merkle_v1.append_leaf(
        control.source_count,
        control.source_peaks,
        script_proof_v1.reference_source_leaf_hash(
          key,
          descriptor.reference_script_language,
          descriptor.reference_script_hash,
          descriptor.reference_script_total_length,
          descriptor.reference_script_item_commitment,
        ),
      ),
    }
  } else {
    validation_merkle_v1.BuiltFrontier {
      count: control.source_count,
      peaks: control.source_peaks,
    }
  }
}

fn replay_purpose_frontier(
  control: ScriptSourcesControlV1,
  source_kind: Int,
  key: ByteArray,
  descriptor: ledger_output_commitment_v1.LedgerOutputCommitmentV1,
) -> validation_merkle_v1.BuiltFrontier {
  expect Some(address) = ledger_output_v1.decode_canonical_address_bytes(
    descriptor.address,
  )
  if source_kind == 0 {
    when address.payment_credential is {
      ScriptCredential(script_hash) ->
        validation_merkle_v1.BuiltFrontier {
          count: control.purpose_count + 1,
          peaks: validation_merkle_v1.append_leaf(
            control.purpose_count,
            control.purpose_peaks,
            script_proof_v1.purpose_leaf_hash(
              0,
              control.spend_index,
              script_hash,
              key,
            ),
          ),
        }
      PubKeyCredential(_) ->
        validation_merkle_v1.BuiltFrontier {
          count: control.purpose_count,
          peaks: control.purpose_peaks,
        }
    }
  } else {
    validation_merkle_v1.BuiltFrontier {
      count: control.purpose_count,
      peaks: control.purpose_peaks,
    }
  }
}

pub fn script_sources_replay_item(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
  source_kind: Int,
  key: ByteArray,
  next_schedule_hash: ByteArray,
  value: ByteArray,
) -> Bool {
  let descriptor = ledger_output_commitment_v1.decode(value)
  if resolution_schedule_node_hash(source_kind, key, next_schedule_hash) == control.replay_remaining_schedule_hash {
    let next_sources = replay_source_frontier(
      control,
      source_kind,
      key,
      descriptor,
    )
    let next_purposes = replay_purpose_frontier(
      control,
      source_kind,
      key,
      descriptor,
    )
    let next_resolved_item_peaks = validation_merkle_v1.append_leaf(
      control.replay_cursor,
      control.resolved_item_peaks,
      script_proof_v1.resolved_context_item_leaf_hash(
        source_kind,
        control.replay_cursor,
        key,
        value,
      ),
    )
    let next_control = encode_script_sources_witness(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
      control.context_cbor,
      control.resolved_input_count,
      control.resolved_inputs_accumulator,
      control.signer_count,
      control.signer_frontier_commitment,
      next_resolved_item_peaks,
      3,
      next_sources.count,
      next_sources.peaks,
      control.redeemer_count,
      control.redeemer_peaks,
      control.replay_cursor + 1,
      resolved_input_accumulator_successor(
        control.replay_accumulator,
        source_kind,
        key,
        value,
      ),
      next_schedule_hash,
      control.spend_index + if source_kind == 0 {
        1
      } else {
        0
      },
      next_purposes.count,
      next_purposes.peaks,
      control.output_cursor,
      control.output_count,
      control.output_peaks,
      control.output_total_count,
      control.receive_scan,
      next_sources.count,
      control.redeemer_total_count,
      control.observer_scan,
      control.mint_fold,
      control.resolution_schedule_hash,
    )
    and {
      witness.claimed_successor.phase == validation_trace_v1.ScriptSources,
      witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
        validation_trace_v1.ScriptSources,
        pre.program_counter + 1,
        next_control,
      ),
    }
  } else {
    False
  }
}

pub fn script_sources_stage_three(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  if control.replay_remaining_schedule_hash == empty_resolution_schedule_hash() {
    let next_control = encode_script_sources_witness(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
      control.context_cbor,
      control.resolved_input_count,
      control.resolved_inputs_accumulator,
      control.signer_count,
      control.signer_frontier_commitment,
      control.resolved_item_peaks,
      4,
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
    and {
      auxiliary == NoAuxiliaryWitness,
      control.replay_cursor == control.resolved_input_count,
      control.replay_accumulator == control.resolved_inputs_accumulator,
      witness.claimed_successor.phase == validation_trace_v1.ScriptSources,
      witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
        validation_trace_v1.ScriptSources,
        pre.program_counter + 1,
        next_control,
      ),
    }
  } else {
    when auxiliary is {
      ResolvedInputReplayWitness { source_kind, key, next_schedule_hash, value } ->
        script_sources_replay_item(
          pre,
          witness,
          control,
          source_kind,
          key,
          next_schedule_hash,
          value,
        )
      _ -> False
    }
  }
}

pub fn script_sources_stage_four(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ScriptSourcesControlV1,
  door: field_door.MachineFieldDoorV1,
) -> Bool {
  let Pair(verified_source, stage_four_witness_set) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  let output_commitment = verified_source.tx_compact.body.outputs_hash
  let empty_output_commitment = native_tx_field_access_v1.empty_field_commitment
  if output_commitment == empty_output_commitment {
    and {
      auxiliary == NoAuxiliaryWitness,
      control.output_cursor == 0,
      control.output_count == 0,
      control.output_total_count == 0,
      control.output_peaks == [],
      control.receive_scan == empty_receive_purpose_scan_control(),
      script_sources_control_successor_is_exact(
        pre,
        witness,
        ScriptSourcesControlV1 { ..control, stage: 5 },
      ),
    }
  } else if and {
    control.output_total_count > 0,
    control.output_count == control.output_total_count,
  } {
    and {
      auxiliary == NoAuxiliaryWitness,
      control.output_cursor == 0,
      control.receive_scan == empty_receive_purpose_scan_control(),
      script_sources_control_successor_is_exact(
        pre,
        witness,
        ScriptSourcesControlV1 { ..control, stage: 5 },
      ),
    }
  } else {
    // The fold consumes only the authenticated
    // (field_index, item_index, item_length, item_commitment) tuple. Under the
    // retired counted scheme `verify_item` bound that tuple into `outputs_hash`
    // — which §4 made unsatisfiable — and it is now bound the other way round:
    // the door authenticates the whole field-2 preimage against `outputs_hash`
    // and the item's length and `bounded_item_v1` commitment are *derived* from
    // it, so the tuple is authenticated by construction rather than by an
    // opening. What has not changed is why the item's bytes are not revealed
    // here: doing so re-proved only that an authenticated commitment has a
    // preimage — which canonicalDecode and the stage-5 LedgerOutputProof
    // traversal already establish for every output — while making stage-4
    // evidence grow with output size and unbuildable above the L1 envelope for
    // legal 16,384-byte outputs (C21-STAGE4-GAP). The carriage keeps stage-4
    // *redeemer* evidence O(1) in output size only under tiers 2–3, where the
    // preimage rides reference inputs; a tier-1 plan for a large field-2
    // preimage is refused by the L1 transaction size, not by this rule.
    expect TransactionRedeemerItemBeginWitness { carriage } = auxiliary
    let item = field_door.open_machine_field_item(
      door,
      verified_source,
      stage_four_witness_set,
      2,
      control.output_count,
      carriage,
    )
    let item_count = field_door.machine_field_item_count(item)
    let active_total_count = if control.output_total_count == 0 {
      item_count
    } else {
      control.output_total_count
    }
    and {
      control.output_cursor == 0,
      control.receive_scan == empty_receive_purpose_scan_control(),
      active_total_count > 0,
      active_total_count <= max_tx_size_derived_collection_item_count,
      item_count == active_total_count,
      field_door.machine_field_item_length(item) <= max_serialized_output_preimage_bytes,
      script_sources_control_successor_is_exact(
        pre,
        witness,
        ScriptSourcesControlV1 {
          ..control,
          output_count: control.output_count + 1,
          output_peaks: validation_merkle_v1.append_leaf(
            control.output_count,
            control.output_peaks,
            script_proof_v1.output_item_leaf_hash(
              control.output_count,
              field_door.machine_field_item_commitment(item),
            ),
          ),
          output_total_count: active_total_count,
        },
      ),
    }
  }
}

fn protected_output_authorization(
  address: MidgardAddress,
  signer_count: Int,
  signer_frontier_commitment: ByteArray,
  signer_proof: SignerSetProofV1,
) -> InputSignerAuthorizationV1 {
  if address.protected {
    payment_credential_signer_authorization(
      address.payment_credential,
      signer_count,
      signer_frontier_commitment,
      signer_proof,
    )
  } else if signer_proof == NoSignerSetProof {
    InputSignerAuthorized
  } else {
    InputSignerProofMalformed
  }
}

fn receive_source_successor(
  scan: ReceivePurposeScanControlV1,
  address: MidgardAddress,
) -> ReceivePurposeScanControlV1 {
  if address.protected {
    when address.payment_credential is {
      PubKeyCredential(_) -> scan
      ScriptCredential(script_hash) ->
        ReceivePurposeScanControlV1 {
          ..scan,
          source_count: scan.source_count + 1,
          source_peaks: validation_merkle_v1.append_leaf(
            scan.source_count,
            scan.source_peaks,
            script_proof_v1.purpose_leaf_hash(
              3,
              scan.source_count,
              script_hash,
              script_hash,
            ),
          ),
        }
    }
  } else {
    scan
  }
}

pub fn script_sources_output_proof_begin(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
  output_index: Int,
  total_length: Int,
  item_commitment: ByteArray,
  siblings: List<ByteArray>,
) -> Bool {
  and {
    control.output_proof == None,
    output_index == control.output_cursor,
    total_length > 0,
    bytearray.length(item_commitment) == 32,
    validation_merkle_v1.verify_membership(
      control.output_count,
      control.output_peaks,
      output_index,
      script_proof_v1.output_item_leaf_hash(output_index, item_commitment),
      siblings,
    ),
    script_sources_control_successor_is_exact(
      pre,
      witness,
      ScriptSourcesControlV1 {
        ..control,
        output_proof: Some(
          ledger_output_proof_v1.initial_control_v1(
            output_index,
            total_length,
            item_commitment,
          ),
        ),
      },
    ),
  }
}

pub fn script_sources_output_proof_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
  output_proof: ledger_output_proof_v1.LedgerOutputProofControlV1,
  proof_witness: ledger_output_proof_v1.LedgerOutputProofWitnessV1,
) -> Bool {
  when ledger_output_proof_v1.step_v1(output_proof, proof_witness) is {
    None -> False
    Some(result) ->
      when result is {
        ledger_output_proof_v1.LedgerOutputProofAdvanced {
          control: next_output_proof,
        } ->
          script_sources_control_successor_is_exact(
            pre,
            witness,
            ScriptSourcesControlV1 {
              ..control,
              output_proof: Some(next_output_proof),
            },
          )
        ledger_output_proof_v1.LedgerOutputProofInvalidOutput ->
          rejected_successor_is_exact(
            pre,
            witness.claimed_successor,
            reject_invalid_output,
          )
        ledger_output_proof_v1.LedgerOutputProofInvalidReferenceScript ->
          rejected_successor_is_exact(
            pre,
            witness.claimed_successor,
            reject_invalid_field_type,
          )
        ledger_output_proof_v1.LedgerOutputProofNativeScriptNodeLimit ->
          rejected_successor_is_exact(
            pre,
            witness.claimed_successor,
            reject_native_script_node_count,
          )
        ledger_output_proof_v1.LedgerOutputProofNativeScriptDepthLimit ->
          rejected_successor_is_exact(
            pre,
            witness.claimed_successor,
            reject_native_script_depth,
          )
      }
  }
}

pub fn script_sources_output_proof_finalize(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
  output_proof: ledger_output_proof_v1.LedgerOutputProofControlV1,
  descriptor_cbor: ByteArray,
  signer_proof: SignerSetProofV1,
) -> Bool {
  let descriptor = ledger_output_commitment_v1.decode(descriptor_cbor)
  if !ledger_output_proof_v1.descriptor_is_exact_v1(output_proof, descriptor) {
    False
  } else {
    expect Some(address) = ledger_output_v1.decode_canonical_address_bytes(
      descriptor.address,
    )
    let context = decode_validation_context(control.context_cbor)
    if address.network_id != context.expected_network_id {
      rejected_successor_is_exact(
        pre,
        witness.claimed_successor,
        reject_network_id_mismatch,
      )
    } else {
      when
        protected_output_authorization(
          address,
          control.signer_count,
          control.signer_frontier_commitment,
          signer_proof,
        )
      is {
        InputSignerMissing ->
          rejected_successor_is_exact(
            pre,
            witness.claimed_successor,
            reject_missing_required_witness,
          )
        InputSignerProofMalformed -> False
        InputSignerAuthorized ->
          script_sources_control_successor_is_exact(
            pre,
            witness,
            ScriptSourcesControlV1 {
              ..control,
              output_cursor: control.output_cursor + 1,
              receive_scan: receive_source_successor(
                ReceivePurposeScanControlV1 {
                  ..control.receive_scan,
                  descriptor_peaks: validation_merkle_v1.append_leaf(
                    control.output_cursor,
                    control.receive_scan.descriptor_peaks,
                    script_proof_v1.output_descriptor_leaf_hash(
                      control.output_cursor,
                      descriptor_cbor,
                    ),
                  ),
                },
                address,
              ),
              output_proof: None,
            },
          )
      }
    }
  }
}

pub fn script_sources_stage_five(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  if control.output_cursor == control.output_count {
    and {
      control.output_proof == None,
      auxiliary == NoAuxiliaryWitness,
      script_sources_control_successor_is_exact(
        pre,
        witness,
        ScriptSourcesControlV1 { ..control, stage: 6 },
      ),
    }
  } else {
    when control.output_proof is {
      None ->
        when auxiliary is {
          LedgerOutputProofBeginWitness {
            output_index,
            total_length,
            item_commitment,
            siblings,
          } ->
            script_sources_output_proof_begin(
              pre,
              witness,
              control,
              output_index,
              total_length,
              item_commitment,
              siblings,
            )
          _ -> False
        }
      Some(output_proof) ->
        if ledger_output_proof_v1.terminal_is_exact_v1(output_proof) {
          when auxiliary is {
            LedgerOutputProofFinalizeWitness { descriptor_cbor, signer_proof } ->
              script_sources_output_proof_finalize(
                pre,
                witness,
                control,
                output_proof,
                descriptor_cbor,
                signer_proof,
              )
            _ -> False
          }
        } else {
          when auxiliary is {
            LedgerOutputProofStepWitness { witness: proof_witness } ->
              script_sources_output_proof_step(
                pre,
                witness,
                control,
                output_proof,
                proof_witness,
              )
            _ -> False
          }
        }
    }
  }
}

fn mint_fold_chunk_window(
  control: MintFoldControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Option<MintChunkWindowV1> {
  let expected_chunk_index = control.item_cursor / bounded_item_v1.chunk_bytes
  let count = bounded_item_v1.chunk_count(control.item_length)
  let current_matches = and {
    chunk_proof.field_index == 5,
    chunk_proof.item_index == control.policy_cursor,
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
          next.field_index == 5,
          next.item_index == control.policy_cursor,
          next.total_length == control.item_length,
          next.chunk_index == expected_chunk_index + 1,
          bounded_item_v1.verify_chunk(control.item_commitment, next),
        } {
          Some(
            MintChunkWindowV1 {
              bytes: bytearray.concat(chunk_proof.chunk, next.chunk),
              offset: control.item_cursor - expected_chunk_index * bounded_item_v1.chunk_bytes,
              chunk_index: expected_chunk_index,
            },
          )
        } else {
          None
        }
    }
  } else if next_chunk_proof == None {
    Some(
      MintChunkWindowV1 {
        bytes: chunk_proof.chunk,
        offset: control.item_cursor - expected_chunk_index * bounded_item_v1.chunk_bytes,
        chunk_index: expected_chunk_index,
      },
    )
  } else {
    None
  }
}

fn script_sources_begin_mint_policy(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
  verified: VerifiedMidgardNativeTxCompact,
  witness_set: NativeTxWitnessSetCompact,
  door: field_door.MachineFieldDoorV1,
  field_index: Int,
  item_index: Int,
  carriage: native_tx_field_access_v1.FieldCarriageV1,
) -> Bool {
  let fold = control.mint_fold
  let item = field_door.open_machine_field_item(
    door,
    verified,
    witness_set,
    5,
    fold.policy_cursor,
    carriage,
  )
  let first_chunk = field_door.machine_field_item_chunk(item, 0)
  let item_length = field_door.machine_field_item_length(item)
  let Pair(policy_offset, item_count) = decode_definite_array_header_at(
    first_chunk,
    0,
  )
  expect item_count == 2
  let Pair(assets_header_offset, policy_id) = decode_canonical_bytes_at(
    first_chunk,
    policy_offset,
  )
  let Pair(assets_offset, asset_count) = decode_canonical_map_header_at(
    first_chunk,
    assets_header_offset,
  )
  let policy_item_count = field_door.machine_field_item_count(item)
  let active_policy_count = if fold.policy_count == -1 {
    policy_item_count
  } else {
    fold.policy_count
  }
  let common_is_valid = and {
    fold.active_policy == #"",
    fold.policy_cursor < active_policy_count,
    field_index == 5,
    item_index == fold.policy_cursor,
    policy_item_count == active_policy_count,
    policy_item_count <= max_tx_size_derived_collection_item_count,
    item_length > assets_offset,
    item_length <= max_aggregate_field_preimage_bytes,
    bytearray.length(policy_id) == 28,
    asset_count > 0,
    or {
      fold.policy_cursor == 0,
      bytearray.compare(fold.previous_policy, policy_id) == Less,
    },
  }
  if and {
    common_is_valid,
    fold.asset_count + asset_count > ledger_output_v1.max_distinct_asset_count,
  } {
    rejected_successor_is_exact(
      pre,
      witness.claimed_successor,
      reject_asset_count,
    )
  } else {
    and {
      common_is_valid,
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
              1,
              fold.policy_cursor,
              policy_id,
              policy_id,
            ),
          ),
          mint_fold: MintFoldControlV1 {
            ..fold,
            policy_count: active_policy_count,
            active_policy: policy_id,
            item_length,
            item_commitment: field_door.machine_field_item_commitment(item),
            item_cursor: assets_offset,
            assets_remaining: asset_count,
            policy_asset_cursor: 0,
            previous_asset: #"",
          },
        },
      ),
    }
  }
}

fn script_sources_fold_mint_asset(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Bool {
  let fold = control.mint_fold
  expect Some(window) = mint_fold_chunk_window(
    fold,
    chunk_proof,
    next_chunk_proof,
  )
  let Pair(quantity_offset, asset_name) = decode_canonical_bytes_at(
    window.bytes,
    window.offset,
  )
  let Pair(next_window_offset, quantity) = decode_canonical_int_at(
    window.bytes,
    quantity_offset,
  )
  let next_item_cursor = window.chunk_index * bounded_item_v1.chunk_bytes + next_window_offset
  let next_asset_count = fold.asset_count + 1
  let common_is_valid = and {
    bytearray.length(fold.active_policy) == 28,
    fold.assets_remaining > 0,
    fold.policy_asset_cursor >= 0,
    bytearray.length(asset_name) <= 32,
    quantity != 0,
    next_asset_count <= ledger_output_v1.max_distinct_asset_count,
    or {
      fold.policy_asset_cursor == 0,
      canonical_bytes_key_precedes(fold.previous_asset, asset_name),
    },
    if fold.assets_remaining == 1 {
      next_item_cursor == fold.item_length
    } else {
      next_item_cursor < fold.item_length
    },
  }
  let next_fold = if fold.assets_remaining == 1 {
    MintFoldControlV1 {
      ..fold,
      policy_cursor: fold.policy_cursor + 1,
      previous_policy: fold.active_policy,
      active_policy: #"",
      item_length: 0,
      item_commitment: #"",
      item_cursor: 0,
      assets_remaining: 0,
      policy_asset_cursor: 0,
      previous_asset: #"",
      asset_count: next_asset_count,
      asset_peaks: validation_merkle_v1.append_leaf(
        fold.asset_count,
        fold.asset_peaks,
        mint_asset_leaf_hash(fold.active_policy, asset_name, quantity),
      ),
    }
  } else {
    MintFoldControlV1 {
      ..fold,
      item_cursor: next_item_cursor,
      assets_remaining: fold.assets_remaining - 1,
      policy_asset_cursor: fold.policy_asset_cursor + 1,
      previous_asset: asset_name,
      asset_count: next_asset_count,
      asset_peaks: validation_merkle_v1.append_leaf(
        fold.asset_count,
        fold.asset_peaks,
        mint_asset_leaf_hash(fold.active_policy, asset_name, quantity),
      ),
    }
  }
  and {
    common_is_valid,
    script_sources_control_successor_is_exact(
      pre,
      witness,
      ScriptSourcesControlV1 { ..control, mint_fold: next_fold },
    ),
  }
}

pub fn script_sources_stage_six(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ScriptSourcesControlV1,
  door: field_door.MachineFieldDoorV1,
) -> Bool {
  let Pair(verified, stage_six_witness_set) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  let expected_commitment = verified.tx_compact.body.mint_hash
  let fold = control.mint_fold
  if fold.policy_count == -1 && expected_commitment == native_tx_field_access_v1.empty_field_commitment {
    and {
      auxiliary == NoAuxiliaryWitness,
      script_sources_control_successor_is_exact(
        pre,
        witness,
        ScriptSourcesControlV1 {
          ..control,
          stage: 7,
          output_cursor: 0,
          mint_fold: MintFoldControlV1 { ..fold, policy_count: 0 },
        },
      ),
    }
  } else if and {
    fold.policy_count >= 0,
    fold.policy_cursor == fold.policy_count,
    fold.active_policy == #"",
  } {
    and {
      auxiliary == NoAuxiliaryWitness,
      script_sources_control_successor_is_exact(
        pre,
        witness,
        ScriptSourcesControlV1 { ..control, stage: 7, output_cursor: 0 },
      ),
    }
  } else if fold.active_policy == #"" {
    expect TransactionFieldChunkWitness { field_index, item_index, carriage } = auxiliary
    script_sources_begin_mint_policy(
      pre,
      witness,
      control,
      verified,
      stage_six_witness_set,
      door,
      field_index,
      item_index,
      carriage,
    )
  } else {
    expect MintFoldAssetWitness { chunk_proof, next_chunk_proof } = auxiliary
    script_sources_fold_mint_asset(
      pre,
      witness,
      control,
      chunk_proof,
      next_chunk_proof,
    )
  }
}
