use midgard/validation_machine/canonical_decode.{
  verify_canonical_decode, verify_compact_binding,
}
use midgard/validation_machine/cek.{verify_cek}
use midgard/validation_machine/input_sets.{verify_input_sets}
use midgard/validation_machine/ledger_delta.{verify_ledger_delta}
use midgard/validation_machine/machine_types.{
  NoAuxiliaryWitness, ValidationAuxiliaryWitnessV1, ValidationOneStepEvidenceV1,
  ValidationOneStepWitnessV1,
}
use midgard/validation_machine/native_scripts.{verify_native_scripts}
use midgard/validation_machine/phase_a_native_scripts.{
  verify_phase_a_native_scripts_scan,
}
use midgard/validation_machine/phase_a_script_preconditions.{
  verify_phase_a_script_preconditions,
}
use midgard/validation_machine/resolve_inputs.{verify_resolve_inputs}
use midgard/validation_machine/script_integrity.{
  verify_script_integrity, verify_script_integrity_authentication,
  verify_script_integrity_compact, verify_script_integrity_finalize,
  verify_script_integrity_witness_set,
}
use midgard/validation_machine/script_sources.{verify_script_sources}
use midgard/validation_machine/shared.{structural_transition_is_valid}
use midgard/validation_machine/signatures.{verify_signatures}
use midgard/validation_machine/static_rules.{verify_static_rules}
use midgard/validation_machine/value_and_mint.{verify_value_and_mint}
use midgard/validation_machine_field_door_v1 as field_door
use midgard/validation_trace_v1

// Midgard V1 is deliberately closed over the instructions implemented here.
// Returning False for an unimplemented phase prevents a deployment from
// treating structural trace commitments as semantic L1 verification.
fn verify_one_step_with_auxiliary(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  door: field_door.MachineFieldDoorV1,
) -> Bool {
  and {
    structural_transition_is_valid(pre, witness),
    when pre.phase is {
      validation_trace_v1.CanonicalDecode ->
        verify_canonical_decode(pre, witness, auxiliary, door)
      validation_trace_v1.CompactBinding -> and {
          auxiliary == NoAuxiliaryWitness,
          verify_compact_binding(pre, witness),
        }
      validation_trace_v1.InputSets ->
        verify_input_sets(pre, witness, auxiliary, door)
      validation_trace_v1.StaticLedgerRules -> and {
          auxiliary == NoAuxiliaryWitness,
          verify_static_rules(pre, witness),
        }
      validation_trace_v1.ResolveInputs ->
        verify_resolve_inputs(pre, witness, auxiliary)
      validation_trace_v1.Signatures ->
        verify_signatures(pre, witness, auxiliary, door)
      validation_trace_v1.ValueAndMint ->
        verify_value_and_mint(pre, witness, auxiliary)
      validation_trace_v1.ScriptSources ->
        verify_script_sources(pre, witness, auxiliary, door)
      validation_trace_v1.NativeScripts ->
        verify_native_scripts(pre, witness, auxiliary)
      validation_trace_v1.PhaseANativeScripts ->
        verify_phase_a_native_scripts_scan(pre, witness, auxiliary, door)
      validation_trace_v1.PhaseAScriptPreconditions ->
        verify_phase_a_script_preconditions(pre, witness, auxiliary, door)
      validation_trace_v1.ScriptIntegrity ->
        verify_script_integrity(pre, witness, auxiliary)
      validation_trace_v1.Cek -> verify_cek(pre, witness, auxiliary, door)
      validation_trace_v1.LedgerDelta ->
        verify_ledger_delta(pre, witness, auxiliary)
      validation_trace_v1.Terminal -> False
    },
  }
}

pub fn verify_one_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  door: field_door.MachineFieldDoorV1,
) -> Bool {
  verify_one_step_with_auxiliary(pre, witness, NoAuxiliaryWitness, door)
}

pub fn verify_one_step_evidence(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
  door: field_door.MachineFieldDoorV1,
) -> Bool {
  verify_one_step_with_auxiliary(
    pre,
    evidence.transition,
    evidence.auxiliary,
    door,
  )
}

pub fn verify_canonical_decode_one_step_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
  door: field_door.MachineFieldDoorV1,
) -> Bool {
  and {
    pre.phase == validation_trace_v1.CanonicalDecode,
    structural_transition_is_valid(pre, evidence.transition),
    verify_canonical_decode(pre, evidence.transition, evidence.auxiliary, door),
  }
}

pub fn verify_canonical_decode_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
  door: field_door.MachineFieldDoorV1,
) -> Bool {
  verify_canonical_decode(pre, evidence.transition, evidence.auxiliary, door)
}

pub fn verify_compact_binding_one_step_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
) -> Bool {
  and {
    pre.phase == validation_trace_v1.CompactBinding,
    structural_transition_is_valid(pre, evidence.transition),
    evidence.auxiliary == NoAuxiliaryWitness,
    verify_compact_binding(pre, evidence.transition),
  }
}

/// Semantic half used after a resolver preparation step has authenticated the
/// common machine-state transition and the challenger successor commitment.
pub fn verify_compact_binding_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
) -> Bool {
  and {
    evidence.auxiliary == NoAuxiliaryWitness,
    verify_compact_binding(pre, evidence.transition),
  }
}

pub fn verify_static_ledger_rules_one_step_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
) -> Bool {
  and {
    pre.phase == validation_trace_v1.StaticLedgerRules,
    structural_transition_is_valid(pre, evidence.transition),
    evidence.auxiliary == NoAuxiliaryWitness,
    verify_static_rules(pre, evidence.transition),
  }
}

pub fn verify_static_ledger_rules_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
) -> Bool {
  and {
    evidence.auxiliary == NoAuxiliaryWitness,
    verify_static_rules(pre, evidence.transition),
  }
}

pub fn verify_input_sets_one_step_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
  door: field_door.MachineFieldDoorV1,
) -> Bool {
  and {
    pre.phase == validation_trace_v1.InputSets,
    structural_transition_is_valid(pre, evidence.transition),
    verify_input_sets(pre, evidence.transition, evidence.auxiliary, door),
  }
}

pub fn verify_input_sets_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
  door: field_door.MachineFieldDoorV1,
) -> Bool {
  verify_input_sets(pre, evidence.transition, evidence.auxiliary, door)
}

pub fn verify_signatures_one_step_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
  door: field_door.MachineFieldDoorV1,
) -> Bool {
  and {
    pre.phase == validation_trace_v1.Signatures,
    structural_transition_is_valid(pre, evidence.transition),
    verify_signatures(pre, evidence.transition, evidence.auxiliary, door),
  }
}

pub fn verify_phase_a_native_scripts_one_step_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
  door: field_door.MachineFieldDoorV1,
) -> Bool {
  and {
    pre.phase == validation_trace_v1.PhaseANativeScripts,
    structural_transition_is_valid(pre, evidence.transition),
    verify_phase_a_native_scripts_scan(
      pre,
      evidence.transition,
      evidence.auxiliary,
      door,
    ),
  }
}

pub fn verify_phase_a_native_scripts_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
  door: field_door.MachineFieldDoorV1,
) -> Bool {
  verify_phase_a_native_scripts_scan(
    pre,
    evidence.transition,
    evidence.auxiliary,
    door,
  )
}

pub fn verify_phase_a_script_preconditions_one_step_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
  door: field_door.MachineFieldDoorV1,
) -> Bool {
  and {
    pre.phase == validation_trace_v1.PhaseAScriptPreconditions,
    structural_transition_is_valid(pre, evidence.transition),
    verify_phase_a_script_preconditions(
      pre,
      evidence.transition,
      evidence.auxiliary,
      door,
    ),
  }
}

pub fn verify_phase_a_script_preconditions_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
  door: field_door.MachineFieldDoorV1,
) -> Bool {
  verify_phase_a_script_preconditions(
    pre,
    evidence.transition,
    evidence.auxiliary,
    door,
  )
}

pub fn verify_resolve_inputs_one_step_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
) -> Bool {
  and {
    pre.phase == validation_trace_v1.ResolveInputs,
    structural_transition_is_valid(pre, evidence.transition),
    verify_resolve_inputs(pre, evidence.transition, evidence.auxiliary),
  }
}

pub fn verify_script_sources_one_step_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
  door: field_door.MachineFieldDoorV1,
) -> Bool {
  and {
    pre.phase == validation_trace_v1.ScriptSources,
    structural_transition_is_valid(pre, evidence.transition),
    verify_script_sources(pre, evidence.transition, evidence.auxiliary, door),
  }
}

pub fn verify_native_scripts_one_step_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
) -> Bool {
  and {
    pre.phase == validation_trace_v1.NativeScripts,
    structural_transition_is_valid(pre, evidence.transition),
    verify_native_scripts(pre, evidence.transition, evidence.auxiliary),
  }
}

pub fn verify_script_integrity_one_step_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
) -> Bool {
  and {
    pre.phase == validation_trace_v1.ScriptIntegrity,
    structural_transition_is_valid(pre, evidence.transition),
    verify_script_integrity(pre, evidence.transition, evidence.auxiliary),
  }
}

pub fn verify_script_integrity_authentication_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  and {
    pre.phase == validation_trace_v1.ScriptIntegrity,
    structural_transition_is_valid(pre, witness),
    verify_script_integrity_authentication(pre, witness, NoAuxiliaryWitness),
  }
}

pub fn verify_script_integrity_compact_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  and {
    pre.phase == validation_trace_v1.ScriptIntegrity,
    structural_transition_is_valid(pre, witness),
    verify_script_integrity_compact(pre, witness, NoAuxiliaryWitness),
  }
}

pub fn verify_script_integrity_witness_set_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  and {
    pre.phase == validation_trace_v1.ScriptIntegrity,
    structural_transition_is_valid(pre, witness),
    verify_script_integrity_witness_set(pre, witness, NoAuxiliaryWitness),
  }
}

pub fn verify_script_integrity_finalize_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  and {
    pre.phase == validation_trace_v1.ScriptIntegrity,
    structural_transition_is_valid(pre, witness),
    verify_script_integrity_finalize(pre, witness, NoAuxiliaryWitness),
  }
}

pub fn verify_cek_one_step_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
  door: field_door.MachineFieldDoorV1,
) -> Bool {
  and {
    pre.phase == validation_trace_v1.Cek,
    structural_transition_is_valid(pre, evidence.transition),
    verify_cek(pre, evidence.transition, evidence.auxiliary, door),
  }
}

pub fn verify_value_and_mint_one_step_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
) -> Bool {
  and {
    pre.phase == validation_trace_v1.ValueAndMint,
    structural_transition_is_valid(pre, evidence.transition),
    verify_value_and_mint(pre, evidence.transition, evidence.auxiliary),
  }
}

pub fn verify_ledger_delta_one_step_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
) -> Bool {
  and {
    pre.phase == validation_trace_v1.LedgerDelta,
    structural_transition_is_valid(pre, evidence.transition),
    verify_ledger_delta(pre, evidence.transition, evidence.auxiliary),
  }
}
