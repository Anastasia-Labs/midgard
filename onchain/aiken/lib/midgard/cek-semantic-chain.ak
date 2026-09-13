use aiken/builtin
use aiken/cbor
use aiken/crypto.{ScriptHash, blake2b_256}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/cek_builtin_v1 as builtin_witness
use midgard/cek_machine_v1 as machine
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{continue}

// Prepared evidence is opaque between the binder and settle. Its typed opening
// is required only by those two endpoints, so arm hops do not decode the VM.
pub type CoreBound {
  prepared: Data,
  pre: machine.MachineStateV1,
  post: machine.MachineStateV1,
  witness_hash: ByteArray,
  arm: Int,
  group: Int,
  progress: Int,
  facts: Data,
}

pub type CoreDatum =
  ct.StepDatum<CoreBound>

pub type ArmAction {
  VerifyArm { input_index: Int, output_index: Int, witness: Data }
}

pub type ArmRedeemer =
  ct.StepRedeemer<ArmAction>

pub fn witness_is_bound(bound: CoreBound, witness: Data) -> Bool {
  and {
    blake2b_256(cbor.serialise(witness)) == bound.witness_hash,
    builtin.un_constr_data(witness).1st == bound.arm,
  }
}

pub fn continue_core(
  next_script_hash: ScriptHash,
  computation_thread_policy_id: PolicyId,
  step: CoreDatum,
  action: ArmAction,
  expected_progress: Int,
  next_facts: Data,
  own_out_ref: OutputReference,
  tx: Transaction,
) -> Bool {
  expect Some(bound) = step.data
  expect bound.progress == expected_progress
  expect witness_is_bound(bound, action.witness)
  let next: Data = CoreBound {
    ..bound,
    progress: bound.progress + 1,
    facts: next_facts,
  }
  continue(
    computation_thread_policy_id,
    step,
    action.input_index,
    action.output_index,
    own_out_ref,
    tx.inputs,
    tx.outputs,
    fn(_, _, _, _, output_hash, output_state) {
      output_hash == next_script_hash && output_state == next
    },
  )
}

pub fn core_group(mode: Int, witness: Data) -> Int {
  let Pair(arm, fields) = builtin.un_constr_data(witness)
  if mode == machine.mode_compute {
    expect arm >= 0 && arm <= 10 || arm == 40
    0
  } else if mode == machine.mode_lookup {
    expect arm == 11 || arm == 12
    1
  } else if mode == machine.mode_return {
    expect arm >= 13 && arm <= 27
    1
  } else if mode == machine.mode_case_select {
    expect arm == 28
    1
  } else if mode == machine.mode_case_apply {
    expect arm == 29
    1
  } else if mode == machine.mode_semantic_builtin {
    expect arm >= 33 && arm <= 35
    2
  } else {
    expect mode == machine.mode_builtin
    when arm is {
      30 -> {
        expect [tag_data, _, _] = fields
        expect tag: Int = tag_data
        if tag >= 29 && tag <= 69 && tag != 52 && tag != 53 {
          4
        } else {
          3
        }
      }
      31 -> 5
      32 -> 6
      36 -> 7
      37 -> 8
      38 -> 9
      39 -> 10
      _ -> fail
    }
  }
}

pub type BuiltinSuccess {
  tag: Int,
  arguments: List<builtin_witness.ValueWitnessV1>,
  result: builtin_witness.ValueWitnessV1,
  material: Data,
}

pub type BuiltinRoots {
  arguments_root: ByteArray,
  arguments_count: Int,
  result_root: ByteArray,
  builtin_root: ByteArray,
}

pub fn decode_builtin_success(arm: Int, raw: Data) -> BuiltinSuccess {
  expect Pair(tag, fields) = builtin.un_constr_data(raw)
  expect tag == arm
  let Pair(prefix, material) = if arm == 30 {
    expect [tag_data, arguments_data, result_data] = fields
    Pair([tag_data, arguments_data, result_data], builtin.constr_data(0, []))
  } else {
    expect arm == 31 || arm == 32
    expect [tag_data, arguments_data, result_data, material] = fields
    Pair([tag_data, arguments_data, result_data], material)
  }
  expect [tag_data, arguments_data, result_data] = prefix
  expect tag: Int = tag_data
  expect arguments: List<builtin_witness.ValueWitnessV1> = arguments_data
  expect result: builtin_witness.ValueWitnessV1 = result_data
  BuiltinSuccess { tag, arguments, result, material }
}

pub fn semantic_group(tag: Int) -> Int {
  if tag == 29 || tag == 30 {
    0
  } else if tag == 31 || tag == 32 || tag == 35 {
    1
  } else if tag == 33 || tag == 34 {
    2
  } else if tag == 36 {
    3
  } else if tag == 37 || tag == 42 {
    4
  } else if tag >= 47 && tag <= 51 {
    6
  } else if tag == 39 || tag == 40 || tag == 41 || tag == 44 || tag == 45 || tag == 46 {
    5
  } else {
    fail
  }
}

pub type SemanticRoots {
  roots: BuiltinRoots,
  arguments: List<builtin_witness.ValueWitnessV1>,
  result: builtin_witness.ValueWitnessV1,
}

pub fn semantic_item(bound: CoreBound, witness: Data) -> BuiltinSuccess {
  let raw = decode_builtin_success(bound.arm, witness)
  expect facts: SemanticRoots = bound.facts
  BuiltinSuccess { ..raw, arguments: facts.arguments, result: facts.result }
}

pub type SemanticArguments {
  arguments_root: ByteArray,
  arguments_count: Int,
  builtin_root: ByteArray,
  arguments: List<builtin_witness.ValueWitnessV1>,
}

pub type MapProjection {
  arguments: List<builtin_witness.ValueWitnessV1>,
  result: builtin_witness.ValueWitnessV1,
  budget: Data,
}

pub fn map_item(bound: CoreBound, witness: Data) -> BuiltinSuccess {
  let raw = decode_builtin_success(bound.arm, witness)
  expect facts: MapProjection = bound.facts
  BuiltinSuccess { ..raw, arguments: facts.arguments, result: facts.result }
}
