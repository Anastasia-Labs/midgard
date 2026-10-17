//// Checks the script-source steps and the ValueAndMint resolved-input replays
//// a TypeScript trace emits, one step at a time, through the one-step
//// dispatcher and through the split verifier the deployment routes that step
//// to. The vectors are produced by
//// demo/midgard-validation/tests/script-source-lookup.machine-rows.test.ts.

use aiken/cbor
use aiken/collection/list
use midgard/rejection_reason_v1.{RejectionReasonV1}
use midgard/validation_machine/machine_types.{
  NoAuxiliaryWitness, ResolvedInputReplayWitness, ScriptSourceScanWitness,
  ValidationAuxiliaryWitnessV1, ValidationOneStepWitnessV1,
}
use midgard/validation_machine/script_sources as vm_script_sources
use midgard/validation_machine/script_sources_late_stages as late
use midgard/validation_machine/script_sources_middle_semantics as middle
use midgard/validation_machine/shared.{rejected_successor_is_exact}
use midgard/validation_machine/value_and_mint as vm_value_and_mint
use midgard/validation_machine_field_door_v1.{MachineFieldDoorV1}
use midgard/validation_trace_v1.{ValidationMachineStateV1}

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

/// The stage a ValueAndMint resolved-input replay vector carries: its control
/// stage, which no exported ScriptSources step shares.
const value_replay_stage = 2

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
    ResolvedInputReplayWitness {
      source_kind,
      key,
      next_schedule_hash,
      value,
      proof,
      siblings,
    } ->
      if stage == value_replay_stage {
        vm_value_and_mint.verify_value_and_mint_replay_input_semantics_v1(
          pre,
          transition,
          source_kind,
          key,
          next_schedule_hash,
          value,
          siblings,
        )
      } else {
        and {
          stage == 3,
          middle.stage_three_replay(
            pre,
            transition,
            source_kind,
            key,
            next_schedule_hash,
            value,
            proof,
          ),
        }
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
          if step.stage == value_replay_stage {
            vm_value_and_mint.verify_value_and_mint(
              step.pre,
              step.transition,
              step.auxiliary,
            )
          } else {
            vm_script_sources.verify_script_sources(
              step.pre,
              step.transition,
              step.auxiliary,
              unopened_door,
            )
          },
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
