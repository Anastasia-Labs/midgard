use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use env
use midgard/fraud_proofs/native_tx/compact.{
  native_tx_proof_commitment_v1, verify_native_tx_proof_source_v1,
}
use midgard/fraud_proofs/native_tx/types.{NativeTxBodyCompact}
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_v1
use midgard/mpf_proof_v1
use midgard/rejection_reason_v1.{
  reject_asset_count, reject_min_ada, reject_value_not_preserved,
}
use midgard/script_proof_v1
use midgard/validation_machine/machine_types.{
  NoAuxiliaryWitness, ResolvedInputReplayWitness, ValidationAuxiliaryWitnessV1,
  ValidationOneStepWitnessV1, ValueAccumulatorAssetLimitExceeded,
  ValueAccumulatorMutationInvalid, ValueAccumulatorUpdateV1,
  ValueAccumulatorUpdated, ValueAccumulatorV1, ValueAndMintControlV1,
  ValueAssetMutationWitnessV1, ValueInputAssetWitness, ValueMintAssetWitness,
  ValueOutputAssetWitness, ValueOutputDescriptorWitness,
}
use midgard/validation_machine/shared.{
  mint_asset_leaf_hash, no_rejection_code_hash, output_meets_min_ada_v1,
  rejected_successor_is_exact, value_map_maximum_proof_steps,
}
use midgard/validation_machine/witness.{
  empty_resolution_schedule_hash, encode_ledger_delta_witness_v1,
  encode_value_and_mint_control_v1, initial_resolution_accumulator,
  initial_value_accumulator, native_scripts_control_is_well_formed,
  resolution_schedule_node_hash, resolved_input_accumulator_successor,
  value_and_mint_control_from_witness,
}
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

fn update_value_asset_delta_root(
  root: ByteArray,
  unit: ByteArray,
  quantity_delta: Int,
  mutation: ValueAssetMutationWitnessV1,
) -> Option<ByteArray> {
  if mutation.delta_was_present {
    let old_value = cbor.serialise(mutation.old_delta)
    let next_delta = mutation.old_delta + quantity_delta
    mpf_proof_v1.update_root(
      root,
      unit,
      old_value,
      cbor.serialise(next_delta),
      mutation.delta_proof,
    )
  } else {
    if mutation.old_delta != 0 {
      None
    } else {
      mpf_proof_v1.insert_root(
        root,
        unit,
        cbor.serialise(quantity_delta),
        mutation.delta_proof,
      )
    }
  }
}

fn apply_value_asset_mutation(
  accumulator: ValueAccumulatorV1,
  unit: ByteArray,
  quantity_delta: Int,
  mutation: ValueAssetMutationWitnessV1,
) -> ValueAccumulatorUpdateV1 {
  if or {
    bytearray.length(unit) < 28,
    bytearray.length(unit) > 60,
    quantity_delta == 0,
    !mpf_proof_v1.proof_has_at_most_steps(
      mutation.delta_proof,
      value_map_maximum_proof_steps,
    ),
  } {
    ValueAccumulatorMutationInvalid
  } else {
    when
      update_value_asset_delta_root(
        accumulator.asset_root,
        unit,
        quantity_delta,
        mutation,
      )
    is {
      None -> ValueAccumulatorMutationInvalid
      Some(next_asset_root) -> {
        let old_delta = if mutation.delta_was_present {
          mutation.old_delta
        } else {
          0
        }
        let next_delta = old_delta + quantity_delta
        let next_nonzero_count = if old_delta == 0 && next_delta != 0 {
          accumulator.nonzero_asset_count + 1
        } else if old_delta != 0 && next_delta == 0 {
          accumulator.nonzero_asset_count - 1
        } else {
          accumulator.nonzero_asset_count
        }
        if and {
          !mutation.delta_was_present,
          accumulator.seen_asset_count >= ledger_output_v1.max_distinct_asset_count,
        } {
          ValueAccumulatorAssetLimitExceeded
        } else if or {
          next_nonzero_count < 0,
          next_nonzero_count > accumulator.seen_asset_count + if mutation.delta_was_present {
            0
          } else {
            1
          },
        } {
          ValueAccumulatorMutationInvalid
        } else {
          ValueAccumulatorUpdated(
            ValueAccumulatorV1 {
              ..accumulator,
              asset_root: next_asset_root,
              seen_asset_count: accumulator.seen_asset_count + if mutation.delta_was_present {
                0
              } else {
                1
              },
              nonzero_asset_count: next_nonzero_count,
            },
          )
        }
      }
    }
  }
}

fn value_and_mint_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  next_control: ValueAndMintControlV1,
) -> Bool {
  and {
    witness.claimed_successor.phase == validation_trace_v1.ValueAndMint,
    witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.ValueAndMint,
      pre.program_counter + 1,
      encode_value_and_mint_control_v1(next_control),
    ),
  }
}

fn value_and_mint_stage_zero(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ValueAndMintControlV1,
) -> Bool {
  let native_control = control.native_control
  let next_control = ValueAndMintControlV1 {
    ..control,
    stage: 1,
    replay_schedule_hash: native_control.resolution_schedule_hash,
    replay_remaining_schedule_hash: native_control.resolution_schedule_hash,
  }
  and {
    auxiliary == NoAuxiliaryWitness,
    control.replay_schedule_hash == empty_resolution_schedule_hash(),
    control.replay_cursor == 0,
    control.replay_asset_cursor == 0,
    control.replay_value_hash == no_rejection_code_hash,
    control.replay_accumulator == initial_resolution_accumulator(),
    control.replay_remaining_schedule_hash == empty_resolution_schedule_hash(),
    control.output_cursor == 0,
    control.output_asset_cursor == 0,
    control.mint_cursor == 0,
    control.value_accumulator == initial_value_accumulator(),
    native_control.resolution_schedule_hash != empty_resolution_schedule_hash(),
    value_and_mint_successor_is_exact(pre, witness, next_control),
  }
}

fn value_and_mint_stage_one(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ValueAndMintControlV1,
) -> Bool {
  and {
    auxiliary == NoAuxiliaryWitness,
    value_and_mint_successor_is_exact(
      pre,
      witness,
      ValueAndMintControlV1 { ..control, stage: 2 },
    ),
  }
}

fn complete_value_input_replay(
  control: ValueAndMintControlV1,
  source_kind: Int,
  key: ByteArray,
  value: ByteArray,
  next_schedule_hash: ByteArray,
  next_value_accumulator: ValueAccumulatorV1,
) -> ValueAndMintControlV1 {
  ValueAndMintControlV1 {
    ..control,
    replay_cursor: control.replay_cursor + 1,
    replay_asset_cursor: 0,
    replay_value_hash: no_rejection_code_hash,
    replay_accumulator: resolved_input_accumulator_successor(
      control.replay_accumulator,
      source_kind,
      key,
      value,
    ),
    replay_remaining_schedule_hash: next_schedule_hash,
    value_accumulator: next_value_accumulator,
  }
}

fn value_and_mint_stage_two(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ValueAndMintControlV1,
) -> Bool {
  if control.replay_remaining_schedule_hash == empty_resolution_schedule_hash() {
    and {
      auxiliary == NoAuxiliaryWitness,
      control.replay_cursor == control.native_control.resolved_input_count,
      control.replay_asset_cursor == 0,
      control.replay_value_hash == no_rejection_code_hash,
      control.replay_accumulator == control.native_control.resolved_inputs_accumulator,
      value_and_mint_successor_is_exact(
        pre,
        witness,
        ValueAndMintControlV1 { ..control, stage: 3 },
      ),
    }
  } else if control.replay_asset_cursor == 0 {
    expect
        ResolvedInputReplayWitness {
          source_kind,
          key,
          next_schedule_hash,
          value,
        }
      = auxiliary
    if and {
      source_kind == 0 || source_kind == 1,
      resolution_schedule_node_hash(source_kind, key, next_schedule_hash) == control.replay_remaining_schedule_hash,
      control.replay_cursor < control.native_control.resolved_input_count,
    } {
      let descriptor = ledger_output_commitment_v1.decode(value)
      let next_value = if source_kind == 0 {
        ValueAccumulatorV1 {
          ..control.value_accumulator,
          lovelace_delta: control.value_accumulator.lovelace_delta + descriptor.lovelace,
        }
      } else {
        control.value_accumulator
      }
      let next_control = if or {
        source_kind == 1,
        descriptor.asset_count == 0,
      } {
        complete_value_input_replay(
          control,
          source_kind,
          key,
          value,
          next_schedule_hash,
          next_value,
        )
      } else {
        ValueAndMintControlV1 {
          ..control,
          replay_asset_cursor: 1,
          replay_value_hash: blake2b_256(value),
          value_accumulator: next_value,
        }
      }
      value_and_mint_successor_is_exact(pre, witness, next_control)
    } else {
      False
    }
  } else {
    expect
        ValueInputAssetWitness {
          source_kind,
          key,
          next_schedule_hash,
          descriptor_cbor,
          asset_index,
          policy_id,
          asset_name,
          quantity,
          asset_peaks,
          asset_siblings,
          mutation,
        }
      = auxiliary
    if and {
      source_kind == 0,
      resolution_schedule_node_hash(source_kind, key, next_schedule_hash) == control.replay_remaining_schedule_hash,
      control.replay_cursor < control.native_control.resolved_input_count,
      blake2b_256(descriptor_cbor) == control.replay_value_hash,
      asset_index == control.replay_asset_cursor - 1,
    } {
      let descriptor = ledger_output_commitment_v1.decode(descriptor_cbor)
      if ledger_output_commitment_v1.verify_asset_membership(
        descriptor,
        asset_index,
        policy_id,
        asset_name,
        quantity,
        asset_peaks,
        asset_siblings,
      ) {
        when
          apply_value_asset_mutation(
            control.value_accumulator,
            policy_id |> bytearray.concat(asset_name),
            quantity,
            mutation,
          )
        is {
          ValueAccumulatorAssetLimitExceeded ->
            rejected_successor_is_exact(
              pre,
              witness.claimed_successor,
              reject_asset_count,
            )
          ValueAccumulatorMutationInvalid -> False
          ValueAccumulatorUpdated(next_value) -> {
            let next_control = if control.replay_asset_cursor == descriptor.asset_count {
              complete_value_input_replay(
                control,
                source_kind,
                key,
                descriptor_cbor,
                next_schedule_hash,
                next_value,
              )
            } else {
              ValueAndMintControlV1 {
                ..control,
                replay_asset_cursor: control.replay_asset_cursor + 1,
                value_accumulator: next_value,
              }
            }
            value_and_mint_successor_is_exact(pre, witness, next_control)
          }
        }
      } else {
        False
      }
    } else {
      False
    }
  }
}

fn value_and_mint_stage_three(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ValueAndMintControlV1,
) -> Bool {
  if control.output_cursor == control.native_control.output_count {
    and {
      auxiliary == NoAuxiliaryWitness,
      control.output_asset_cursor == 0,
      control.replay_value_hash == no_rejection_code_hash,
      value_and_mint_successor_is_exact(
        pre,
        witness,
        ValueAndMintControlV1 { ..control, stage: 4 },
      ),
    }
  } else if control.output_asset_cursor == 0 {
    expect
        ValueOutputDescriptorWitness { output_index, descriptor_cbor, siblings }
      = auxiliary
    if and {
      output_index == control.output_cursor,
      validation_merkle_v1.verify_membership(
        control.native_control.output_count,
        control.native_control.output_descriptor_peaks,
        output_index,
        script_proof_v1.output_descriptor_leaf_hash(
          output_index,
          descriptor_cbor,
        ),
        siblings,
      ),
    } {
      let descriptor = ledger_output_commitment_v1.decode(descriptor_cbor)
      // E_MIN_ADA (#618 ruling 1; R8 of decision 0005; decision 0003 Q27).
      // The parameterized minimum-Ada floor, bound to a machine rejection code
      // at the first step that sees the whole output. Both operands are facts
      // of the descriptor the membership conjunct above just authenticated
      // against `output_descriptor_peaks`: `total_length` is the length of the
      // canonical serialized output (`descriptor_is_well_formed` in
      // lib/midgard/ledger-output-commitment-v1.ak binds it to the bounded item
      // the output scan chunked), and `lovelace` is that output's Ada. Neither
      // is re-derived here, so this step convicts on authenticated bytes alone.
      //
      // The floor is evaluated BEFORE the output's Ada is folded into the value
      // accumulator, and before the per-asset cursor opens: an under-funded
      // output is convictable from the single descriptor step, with no asset
      // steps and no dependence on the rest of the transaction. This is the
      // MIN-ADA-TX shape of technical-spec §5 (5-ledger-rules); MIN-ADA-UTXO,
      // the same floor over a member of the post-block UTxO set, is Q27's own
      // membership shape and is deliberately not reachable from here.
      //
      // The rate is `env.coins_per_utxo_byte`, a compiled deployment constant
      // (owner ruling 2026-08-23 on #627, option B) rather than a header field
      // or a context element: the rate that convicts a block is not the block
      // producer's to declare. Its TypeScript mirror is
      // `MIDGARD_CONSENSUS_LIMITS_V1.coinsPerUtxoByte`.
      if !output_meets_min_ada_v1(
        env.coins_per_utxo_byte,
        descriptor.total_length,
        descriptor.lovelace,
      ) {
        rejected_successor_is_exact(
          pre,
          witness.claimed_successor,
          reject_min_ada,
        )
      } else {
        let next_value = ValueAccumulatorV1 {
          ..control.value_accumulator,
          lovelace_delta: control.value_accumulator.lovelace_delta - descriptor.lovelace,
        }
        let next_control = if descriptor.asset_count == 0 {
          ValueAndMintControlV1 {
            ..control,
            output_cursor: control.output_cursor + 1,
            value_accumulator: next_value,
          }
        } else {
          ValueAndMintControlV1 {
            ..control,
            output_asset_cursor: 1,
            replay_value_hash: blake2b_256(descriptor_cbor),
            value_accumulator: next_value,
          }
        }
        value_and_mint_successor_is_exact(pre, witness, next_control)
      }
    } else {
      False
    }
  } else {
    expect
        ValueOutputAssetWitness {
          output_index,
          descriptor_cbor,
          asset_index,
          policy_id,
          asset_name,
          quantity,
          asset_peaks,
          asset_siblings,
          mutation,
        }
      = auxiliary
    if and {
      output_index == control.output_cursor,
      asset_index == control.output_asset_cursor - 1,
      blake2b_256(descriptor_cbor) == control.replay_value_hash,
    } {
      let descriptor = ledger_output_commitment_v1.decode(descriptor_cbor)
      if ledger_output_commitment_v1.verify_asset_membership(
        descriptor,
        asset_index,
        policy_id,
        asset_name,
        quantity,
        asset_peaks,
        asset_siblings,
      ) {
        when
          apply_value_asset_mutation(
            control.value_accumulator,
            policy_id |> bytearray.concat(asset_name),
            0 - quantity,
            mutation,
          )
        is {
          ValueAccumulatorAssetLimitExceeded ->
            rejected_successor_is_exact(
              pre,
              witness.claimed_successor,
              reject_asset_count,
            )
          ValueAccumulatorMutationInvalid -> False
          ValueAccumulatorUpdated(next_value) -> {
            let next_control = if control.output_asset_cursor == descriptor.asset_count {
              ValueAndMintControlV1 {
                ..control,
                output_cursor: control.output_cursor + 1,
                output_asset_cursor: 0,
                replay_value_hash: no_rejection_code_hash,
                value_accumulator: next_value,
              }
            } else {
              ValueAndMintControlV1 {
                ..control,
                output_asset_cursor: control.output_asset_cursor + 1,
                value_accumulator: next_value,
              }
            }
            value_and_mint_successor_is_exact(pre, witness, next_control)
          }
        }
      } else {
        False
      }
    } else {
      False
    }
  }
}

fn value_and_mint_stage_four(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ValueAndMintControlV1,
) -> Bool {
  let native_control = control.native_control
  if control.mint_cursor == native_control.mint_count {
    and {
      auxiliary == NoAuxiliaryWitness,
      value_and_mint_successor_is_exact(
        pre,
        witness,
        ValueAndMintControlV1 { ..control, stage: 5 },
      ),
    }
  } else {
    expect
        ValueMintAssetWitness {
          mint_index,
          policy_id,
          asset_name,
          quantity,
          siblings,
          mutation,
        }
      = auxiliary
    if and {
      mint_index == control.mint_cursor,
      bytearray.length(policy_id) == 28,
      bytearray.length(asset_name) <= 32,
      quantity != 0,
      validation_merkle_v1.verify_membership(
        native_control.mint_count,
        native_control.mint_peaks,
        mint_index,
        mint_asset_leaf_hash(policy_id, asset_name, quantity),
        siblings,
      ),
    } {
      when
        apply_value_asset_mutation(
          control.value_accumulator,
          policy_id |> bytearray.concat(asset_name),
          quantity,
          mutation,
        )
      is {
        ValueAccumulatorAssetLimitExceeded ->
          rejected_successor_is_exact(
            pre,
            witness.claimed_successor,
            reject_asset_count,
          )
        ValueAccumulatorMutationInvalid -> False
        ValueAccumulatorUpdated(next_value) ->
          value_and_mint_successor_is_exact(
            pre,
            witness,
            ValueAndMintControlV1 {
              ..control,
              mint_cursor: control.mint_cursor + 1,
              value_accumulator: next_value,
            },
          )
      }
    } else {
      False
    }
  }
}

fn value_and_mint_stage_five(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ValueAndMintControlV1,
  body: NativeTxBodyCompact,
) -> Bool {
  and {
    auxiliary == NoAuxiliaryWitness,
    control.replay_cursor == control.native_control.resolved_input_count,
    control.replay_asset_cursor == 0,
    control.replay_value_hash == no_rejection_code_hash,
    control.replay_accumulator == control.native_control.resolved_inputs_accumulator,
    control.replay_remaining_schedule_hash == empty_resolution_schedule_hash(),
    control.output_cursor == control.native_control.output_count,
    control.output_asset_cursor == 0,
    control.mint_cursor == control.native_control.mint_count,
    if or {
      control.value_accumulator.lovelace_delta - body.fee != 0,
      control.value_accumulator.nonzero_asset_count != 0,
    } {
      rejected_successor_is_exact(
        pre,
        witness.claimed_successor,
        reject_value_not_preserved,
      )
    } else {
      and {
        witness.claimed_successor.phase == validation_trace_v1.LedgerDelta,
        witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
          validation_trace_v1.LedgerDelta,
          pre.program_counter + 1,
          encode_ledger_delta_witness_v1(
            control.native_control,
            pre.prior_ledger_root,
            control.replay_schedule_hash,
          ),
        ),
      }
    },
  }
}

pub fn verify_value_and_mint(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  let control = value_and_mint_control_from_witness(witness.work_witness_cbor)
  let Pair(body, control_is_well_formed) = value_and_mint_verified_body_v1(
    pre,
    witness,
    control,
  )
  and {
    control_is_well_formed,
    if control.stage == 0 {
      value_and_mint_stage_zero(pre, witness, auxiliary, control)
    } else if control.stage == 1 {
      value_and_mint_stage_one(pre, witness, auxiliary, control)
    } else if control.stage == 2 {
      value_and_mint_stage_two(pre, witness, auxiliary, control)
    } else if control.stage == 3 {
      value_and_mint_stage_three(pre, witness, auxiliary, control)
    } else if control.stage == 4 {
      value_and_mint_stage_four(pre, witness, auxiliary, control)
    } else {
      value_and_mint_stage_five(pre, witness, auxiliary, control, body)
    },
  }
}

/// The checks every ValueAndMint step shares, single-sourced so the
/// aggregate `verify_value_and_mint` and the per-kind semantic resolvers
/// cannot drift: the native transaction proof source (whose `expect`s are
/// load-bearing, which is why every caller receives the verified body from
/// here rather than re-deriving it), the transaction commitment, the
/// validation context, the native control, every cursor and hash bound and
/// the exact re-encoding of the control.
fn value_and_mint_verified_body_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ValueAndMintControlV1,
) -> Pair<NativeTxBodyCompact, Bool> {
  let native_control = control.native_control
  let Pair(verified_source, _) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    native_control.compact_cbor,
    native_control.witness_set_compact_cbor,
    native_control.field_preimage_lengths_cbor,
  )
  Pair(verified_source.tx_compact.body, and {
      native_tx_proof_commitment_v1(
        native_control.compact_cbor,
        native_control.witness_set_compact_cbor,
        native_control.field_preimage_lengths_cbor,
      ) == pre.transaction_commitment,
      validation_trace_v1.hash_validation_context(native_control.context_cbor) == pre.validation_context_hash,
      native_scripts_control_is_well_formed(native_control),
      native_control.execution_cursor == native_control.execution_count,
      bytearray.length(control.replay_schedule_hash) == 32,
      bytearray.length(control.replay_value_hash) == 32,
      bytearray.length(control.replay_accumulator) == 32,
      bytearray.length(control.replay_remaining_schedule_hash) == 32,
      control.replay_cursor >= 0,
      control.replay_cursor <= native_control.resolved_input_count,
      control.replay_asset_cursor >= 0,
      control.replay_asset_cursor <= ledger_output_v1.max_distinct_asset_count,
      if or {
        control.stage == 2 && control.replay_asset_cursor > 0,
        control.stage == 3 && control.output_asset_cursor > 0,
      } {
        True
      } else {
        control.replay_value_hash == no_rejection_code_hash
      },
      control.output_cursor >= 0,
      control.output_cursor <= native_control.output_count,
      control.output_asset_cursor >= 0,
      control.output_asset_cursor <= ledger_output_v1.max_distinct_asset_count,
      bytearray.length(control.value_accumulator.asset_root) == 32,
      control.value_accumulator.seen_asset_count >= 0,
      control.value_accumulator.seen_asset_count <= ledger_output_v1.max_distinct_asset_count,
      control.value_accumulator.nonzero_asset_count >= 0,
      control.value_accumulator.nonzero_asset_count <= control.value_accumulator.seen_asset_count,
      control.mint_cursor >= 0,
      control.mint_cursor <= native_control.mint_count,
      control.stage >= 0,
      control.stage <= 5,
      witness.work_witness_cbor == encode_value_and_mint_control_v1(control),
    })
}

/// Per-kind ValueAndMint semantic resolvers (R5 item 1). Each one decodes
/// the control, runs the shared guard list, pins its stage and the auxiliary
/// discriminator the stage body branches on, and calls that stage body with
/// the auxiliary it reconstructs from the validator's action fields, so the
/// eleven resolvers partition the ValueAndMint step space exactly.
pub fn verify_value_and_mint_begin_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let control = value_and_mint_control_from_witness(witness.work_witness_cbor)
  let Pair(_, control_is_well_formed) = value_and_mint_verified_body_v1(
    pre,
    witness,
    control,
  )
  and {
    control_is_well_formed,
    control.stage == 0,
    value_and_mint_stage_zero(pre, witness, NoAuxiliaryWitness, control),
  }
}

pub fn verify_value_and_mint_replay_begin_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let control = value_and_mint_control_from_witness(witness.work_witness_cbor)
  let Pair(_, control_is_well_formed) = value_and_mint_verified_body_v1(
    pre,
    witness,
    control,
  )
  and {
    control_is_well_formed,
    control.stage == 1,
    value_and_mint_stage_one(pre, witness, NoAuxiliaryWitness, control),
  }
}

pub fn verify_value_and_mint_replay_input_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  source_kind: Int,
  key: ByteArray,
  next_schedule_hash: ByteArray,
  value: ByteArray,
) -> Bool {
  let control = value_and_mint_control_from_witness(witness.work_witness_cbor)
  let Pair(_, control_is_well_formed) = value_and_mint_verified_body_v1(
    pre,
    witness,
    control,
  )
  and {
    control_is_well_formed,
    control.stage == 2,
    control.replay_remaining_schedule_hash != empty_resolution_schedule_hash(),
    control.replay_asset_cursor == 0,
    value_and_mint_stage_two(
      pre,
      witness,
      ResolvedInputReplayWitness { source_kind, key, next_schedule_hash, value },
      control,
    ),
  }
}

pub fn verify_value_and_mint_replay_asset_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  source_kind: Int,
  key: ByteArray,
  next_schedule_hash: ByteArray,
  descriptor_cbor: ByteArray,
  asset_index: Int,
  policy_id: ByteArray,
  asset_name: ByteArray,
  quantity: Int,
  asset_peaks: List<validation_merkle_v1.FrontierPeak>,
  asset_siblings: List<ByteArray>,
  mutation: ValueAssetMutationWitnessV1,
) -> Bool {
  let control = value_and_mint_control_from_witness(witness.work_witness_cbor)
  let Pair(_, control_is_well_formed) = value_and_mint_verified_body_v1(
    pre,
    witness,
    control,
  )
  and {
    control_is_well_formed,
    control.stage == 2,
    control.replay_remaining_schedule_hash != empty_resolution_schedule_hash(),
    control.replay_asset_cursor > 0,
    value_and_mint_stage_two(
      pre,
      witness,
      ValueInputAssetWitness {
        source_kind,
        key,
        next_schedule_hash,
        descriptor_cbor,
        asset_index,
        policy_id,
        asset_name,
        quantity,
        asset_peaks,
        asset_siblings,
        mutation,
      },
      control,
    ),
  }
}

pub fn verify_value_and_mint_replay_finish_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let control = value_and_mint_control_from_witness(witness.work_witness_cbor)
  let Pair(_, control_is_well_formed) = value_and_mint_verified_body_v1(
    pre,
    witness,
    control,
  )
  and {
    control_is_well_formed,
    control.stage == 2,
    control.replay_remaining_schedule_hash == empty_resolution_schedule_hash(),
    value_and_mint_stage_two(pre, witness, NoAuxiliaryWitness, control),
  }
}

pub fn verify_value_and_mint_output_descriptor_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  output_index: Int,
  descriptor_cbor: ByteArray,
  siblings: List<ByteArray>,
) -> Bool {
  let control = value_and_mint_control_from_witness(witness.work_witness_cbor)
  let Pair(_, control_is_well_formed) = value_and_mint_verified_body_v1(
    pre,
    witness,
    control,
  )
  and {
    control_is_well_formed,
    control.stage == 3,
    control.output_cursor < control.native_control.output_count,
    control.output_asset_cursor == 0,
    value_and_mint_stage_three(
      pre,
      witness,
      ValueOutputDescriptorWitness { output_index, descriptor_cbor, siblings },
      control,
    ),
  }
}

pub fn verify_value_and_mint_output_asset_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  output_index: Int,
  descriptor_cbor: ByteArray,
  asset_index: Int,
  policy_id: ByteArray,
  asset_name: ByteArray,
  quantity: Int,
  asset_peaks: List<validation_merkle_v1.FrontierPeak>,
  asset_siblings: List<ByteArray>,
  mutation: ValueAssetMutationWitnessV1,
) -> Bool {
  let control = value_and_mint_control_from_witness(witness.work_witness_cbor)
  let Pair(_, control_is_well_formed) = value_and_mint_verified_body_v1(
    pre,
    witness,
    control,
  )
  and {
    control_is_well_formed,
    control.stage == 3,
    control.output_cursor < control.native_control.output_count,
    control.output_asset_cursor > 0,
    value_and_mint_stage_three(
      pre,
      witness,
      ValueOutputAssetWitness {
        output_index,
        descriptor_cbor,
        asset_index,
        policy_id,
        asset_name,
        quantity,
        asset_peaks,
        asset_siblings,
        mutation,
      },
      control,
    ),
  }
}

pub fn verify_value_and_mint_output_finish_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let control = value_and_mint_control_from_witness(witness.work_witness_cbor)
  let Pair(_, control_is_well_formed) = value_and_mint_verified_body_v1(
    pre,
    witness,
    control,
  )
  and {
    control_is_well_formed,
    control.stage == 3,
    control.output_cursor == control.native_control.output_count,
    value_and_mint_stage_three(pre, witness, NoAuxiliaryWitness, control),
  }
}

pub fn verify_value_and_mint_mint_asset_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  mint_index: Int,
  policy_id: ByteArray,
  asset_name: ByteArray,
  quantity: Int,
  siblings: List<ByteArray>,
  mutation: ValueAssetMutationWitnessV1,
) -> Bool {
  let control = value_and_mint_control_from_witness(witness.work_witness_cbor)
  let Pair(_, control_is_well_formed) = value_and_mint_verified_body_v1(
    pre,
    witness,
    control,
  )
  and {
    control_is_well_formed,
    control.stage == 4,
    control.mint_cursor < control.native_control.mint_count,
    value_and_mint_stage_four(
      pre,
      witness,
      ValueMintAssetWitness {
        mint_index,
        policy_id,
        asset_name,
        quantity,
        siblings,
        mutation,
      },
      control,
    ),
  }
}

pub fn verify_value_and_mint_mint_finish_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let control = value_and_mint_control_from_witness(witness.work_witness_cbor)
  let Pair(_, control_is_well_formed) = value_and_mint_verified_body_v1(
    pre,
    witness,
    control,
  )
  and {
    control_is_well_formed,
    control.stage == 4,
    control.mint_cursor == control.native_control.mint_count,
    value_and_mint_stage_four(pre, witness, NoAuxiliaryWitness, control),
  }
}

pub fn verify_value_and_mint_finalize_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let control = value_and_mint_control_from_witness(witness.work_witness_cbor)
  let Pair(body, control_is_well_formed) = value_and_mint_verified_body_v1(
    pre,
    witness,
    control,
  )
  and {
    control_is_well_formed,
    control.stage == 5,
    value_and_mint_stage_five(pre, witness, NoAuxiliaryWitness, control, body),
  }
}
