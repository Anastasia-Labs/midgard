//// Checks the script-source steps a TypeScript trace emits, one step at a
//// time, through the one-step dispatcher and through the split verifier the
//// deployment routes that step to. The vectors are produced by
//// demo/midgard-validation/tests/script-source-lookup.machine-rows.test.ts.

use aiken/cbor
use aiken/collection/list
use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/codec.{encode_definite_bytes}
use midgard/rejection_reason_v1.{RejectionReasonV1}
use midgard/validation_machine/machine_types.{
  NoAuxiliaryWitness, ResolvedInputReplayWitness, ScriptDiscoveryControlV1,
  ScriptSourceScanWitness, ValidationAuxiliaryWitnessV1,
  ValidationOneStepWitnessV1,
}
use midgard/validation_machine/script_sources as vm_script_sources
use midgard/validation_machine/script_sources_late_stages as late
use midgard/validation_machine/script_sources_middle_semantics as middle
use midgard/validation_machine/shared.{rejected_successor_is_exact}
use midgard/validation_machine/witness.{
  encode_script_discovery_control,
  script_sources_stage_nine_control_from_witness,
}
use midgard/validation_machine_field_door_v1.{MachineFieldDoorV1}
use midgard/validation_trace_v1.{
  ScriptSources, ValidationMachineStateV1, hash_work_witness,
}

pub type OneStepVector {
  stage: Int,
  pre: ByteArray,
  transition: ByteArray,
  auxiliary: ByteArray,
}

type DecodedStep {
  stage: Int,
  pre: ValidationMachineStateV1,
  transition: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
}

fn decode(vector: OneStepVector) -> DecodedStep {
  expect Some(pre_data) = cbor.deserialise(vector.pre)
  expect pre: ValidationMachineStateV1 = pre_data
  expect Some(transition_data) = cbor.deserialise(vector.transition)
  expect transition: ValidationOneStepWitnessV1 = transition_data
  expect Some(auxiliary_data) = cbor.deserialise(vector.auxiliary)
  expect auxiliary: ValidationAuxiliaryWitnessV1 = auxiliary_data
  DecodedStep { stage: vector.stage, pre, transition, auxiliary }
}

/// Stages 3, 9 and 11 read no committed field, so the door is never opened.
const unopened_door =
  MachineFieldDoorV1 { reference_inputs: [], certificate_policy_id: #"" }

fn split_verifier_accepts(step: DecodedStep) -> Bool {
  let DecodedStep { stage, pre, transition, auxiliary } = step
  when auxiliary is {
    NoAuxiliaryWitness ->
      if stage == 3 {
        middle.stage_three_finish(pre, transition)
      } else if stage == 9 {
        late.verify_script_sources_stage_nine_missing_semantics_v1(
          pre,
          transition,
        )
      } else {
        and {
          stage == 11,
          late.verify_script_sources_stage_eleven_finish_semantics_v1(
            pre,
            transition,
          ),
        }
      }
    ResolvedInputReplayWitness { source_kind, key, next_schedule_hash, value } ->
      and {
        stage == 3,
        middle.stage_three_replay(
          pre,
          transition,
          source_kind,
          key,
          next_schedule_hash,
          value,
        ),
      }
    ScriptSourceScanWitness {
      source_index,
      origin_kind,
      source_key,
      script_language_tag,
      script_hash,
      script_total_length,
      script_item_commitment,
      siblings,
    } ->
      if stage == 9 {
        or {
          late.verify_script_sources_stage_nine_mismatch_semantics_v1(
            pre,
            transition,
            source_index,
            origin_kind,
            source_key,
            script_language_tag,
            script_hash,
            script_total_length,
            script_item_commitment,
            siblings,
          ),
          and {
            script_language_tag == 0,
            late.verify_script_sources_stage_nine_native_match_semantics_v1(
              pre,
              transition,
              source_index,
              origin_kind,
              source_key,
              script_hash,
              script_total_length,
              script_item_commitment,
              siblings,
            ),
          },
          late.verify_script_sources_stage_nine_effectful_match_semantics_v1(
            pre,
            transition,
            source_index,
            origin_kind,
            source_key,
            script_language_tag,
            script_hash,
            script_total_length,
            script_item_commitment,
            siblings,
          ),
        }
      } else {
        and {
          stage == 11,
          late.verify_script_sources_stage_eleven_source_semantics_v1(
            pre,
            transition,
            source_index,
            origin_kind,
            source_key,
            script_language_tag,
            script_hash,
            script_total_length,
            script_item_commitment,
            siblings,
          ),
        }
      }
    _ -> False
  }
}

/// Every step verifies through the dispatcher and through its split verifier.
pub fn verifies_every_step(vectors: List<OneStepVector>) -> Bool {
  and {
    vectors != [],
    list.all(
      vectors,
      fn(vector) {
        let step = decode(vector)
        and {
          vm_script_sources.verify_script_sources(
            step.pre,
            step.transition,
            step.auxiliary,
            unopened_door,
          ),
          split_verifier_accepts(step),
        }
      },
    ),
  }
}

/// The last step is the machine's exact rejection for `reason`.
pub fn rejects_with(
  vectors: List<OneStepVector>,
  reason: RejectionReasonV1,
) -> Bool {
  expect Some(last) = list.last(vectors)
  let step = decode(last)
  rejected_successor_is_exact(
    step.pre,
    step.transition.claimed_successor,
    reason,
  )
}

/// The stage-9 mismatch verifier on `pre` and `transition`, opened at the
/// source the step's scan witness names.
fn mismatch_accepts(
  step: DecodedStep,
  pre: ValidationMachineStateV1,
  transition: ValidationOneStepWitnessV1,
) -> Bool {
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
    = step.auxiliary
  late.verify_script_sources_stage_nine_mismatch_semantics_v1(
    pre,
    transition,
    source_index,
    origin_kind,
    source_key,
    script_language_tag,
    script_hash,
    script_total_length,
    script_item_commitment,
    siblings,
  )
}

/// `work_witness_cbor` with its trailing discovery control `old` replaced by
/// `new`.
fn with_discovery(
  work_witness_cbor: ByteArray,
  old: ScriptDiscoveryControlV1,
  new: ScriptDiscoveryControlV1,
) -> ByteArray {
  let old_suffix = encode_definite_bytes(encode_script_discovery_control(old))
  let kept = bytearray.length(work_witness_cbor) - bytearray.length(old_suffix)
  expect bytearray.drop(work_witness_cbor, kept) == old_suffix
  bytearray.take(work_witness_cbor, kept)
    |> bytearray.concat(
        encode_definite_bytes(encode_script_discovery_control(new)),
      )
}

/// The step from `pre` that stays at stage 9 and moves the lookup cursor one
/// source further on.
fn skip_transition(
  pre: ValidationMachineStateV1,
  work_witness_cbor: ByteArray,
  discovery: ScriptDiscoveryControlV1,
) -> ValidationOneStepWitnessV1 {
  let next = with_discovery(
    work_witness_cbor,
    discovery,
    ScriptDiscoveryControlV1 {
      ..discovery,
      source_cursor: discovery.source_cursor + 1,
    },
  )
  ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: ValidationMachineStateV1 {
      ..pre,
      program_counter: pre.program_counter + 1,
      work_root: hash_work_witness(ScriptSources, pre.program_counter + 1, next),
    },
  }
}

/// Every stage-9 step whose scanned source has the script hash the lookup
/// wants refuses the stage-9 successor that moves past that source. The same
/// skip verifies once the lookup wants a different script hash, so the
/// refusal comes from the hash comparison alone.
pub fn refuses_skipping_the_match(vectors: List<OneStepVector>) -> Bool {
  let matches = vectors
    |> list.map(decode)
    |> list.filter(
        fn(step) {
          when step.auxiliary is {
            ScriptSourceScanWitness { script_hash, .. } -> and {
                step.stage == 9,
                script_hash == script_sources_stage_nine_control_from_witness(
                  step.transition.work_witness_cbor,
                ).discovery.current_script_hash,
              }
            _ -> False
          }
        },
      )
  and {
    matches != [],
    list.all(
      matches,
      fn(step) {
        let work = step.transition.work_witness_cbor
        let discovery = script_sources_stage_nine_control_from_witness(work).discovery
        let wanted = discovery.current_script_hash
        let other = ScriptDiscoveryControlV1 {
          ..discovery,
          current_script_hash: bytearray.push(
            bytearray.drop(wanted, 1),
            ( bytearray.at(wanted, 0) + 1 ) % 256,
          ),
        }
        let other_work = with_discovery(work, discovery, other)
        let other_pre = ValidationMachineStateV1 {
          ..step.pre,
          work_root: hash_work_witness(
            ScriptSources,
            step.pre.program_counter,
            other_work,
          ),
        }
        and {
          step.pre.work_root == hash_work_witness(
            ScriptSources,
            step.pre.program_counter,
            work,
          ),
          !mismatch_accepts(
            step,
            step.pre,
            skip_transition(step.pre, work, discovery),
          ),
          mismatch_accepts(
            step,
            other_pre,
            skip_transition(other_pre, other_work, other),
          ),
        }
      },
    ),
  }
}
