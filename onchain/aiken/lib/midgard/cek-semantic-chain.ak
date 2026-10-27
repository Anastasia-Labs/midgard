use aiken/builtin
use aiken/cbor
use aiken/crypto.{ScriptHash, blake2b_256}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/cek_builtin_v1 as builtin_witness
use midgard/cek_machine_v1 as machine
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{ProverOnly, continue_validation_trace}

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

/// The witness is the one the core step bound. The binder in
/// `cek-core-step-semantic-v1` is the only writer of a `CoreBound` (every arm
/// carries it on with `..bound`), and it sets `witness_hash` and `arm` from
/// the same witness, so a witness that hashes to `witness_hash` also carries
/// constructor `arm`; that is not checked again.
pub fn witness_is_bound(bound: CoreBound, witness: Data) -> Bool {
  blake2b_256(cbor.serialise(witness)) == bound.witness_hash
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
  continue_validation_trace(
    computation_thread_policy_id,
    step,
    action.input_index,
    action.output_index,
    own_out_ref,
    tx,
    ProverOnly,
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
    if arm == 41 {
      11
    } else {
      expect arm >= 13 && arm <= 27
      1
    }
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
      38 -> {
        // Uncompress (59, 66) fails in its own arm, which carries the point
        // checks; every other direct failure is the known failure arm's.
        expect [tag_data, _] = fields
        expect tag: Int = tag_data
        if tag == 59 || tag == 66 {
          12
        } else {
          9
        }
      }
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
  } else if tag == 51 {
    // serialiseData, over a Data argument of any size.
    10
  } else if tag >= 47 && tag <= 50 {
    6
  } else if tag == 39 || tag == 40 || tag == 41 || tag == 44 || tag == 45 || tag == 46 {
    5
  } else if tag >= 2 && tag <= 6 {
    // multiplyInteger and the division builtins over staged operands.
    9
  } else if tag >= 0 && tag <= 9 {
    // The linear integer builtins over staged operands.
    8
  } else if or {
    tag >= 10 && tag <= 25,
    tag == 53,
    tag == 71,
    tag == 72,
    tag >= 75 && tag <= 79,
    tag == 84,
    tag == 85,
    tag == 86,
  } {
    // The byte-string, string, bitwise and signature builtins over staged
    // operands.
    7
  } else if or {
    tag == 73,
    tag == 74,
    tag == 82,
    tag == 83,
  } {
    // integerToByteString, byteStringToInteger, shiftByteString and
    // rotateByteString past the direct carriage bound.
    11
  } else if tag == 80 {
    // writeBits over an index list of any length.
    12
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

/// A hop of an arm that runs as a loop over several transactions: the arm's
/// action, and the end of the range of work this hop covers.
pub type LoopAction {
  LoopArm { input_index: Int, output_index: Int, witness: Data, end: Int }
}

pub type LoopRedeemer =
  ct.StepRedeemer<LoopAction>

/// The facts a `CoreBound` carries while an arm's loop runs: the arm's own
/// facts (`inner`), where the next hop starts (`cursor`) and the loop's state.
/// They are constructor 1, so they never read as an arm's facts.
pub type LoopFacts {
  inner: Data,
  cursor: Int,
  state: Data,
}

fn loop_facts_data(facts: LoopFacts) -> Data {
  builtin.constr_data(
    1,
    [facts.inner, builtin.i_data(facts.cursor), facts.state],
  )
}

fn read_loop_facts(facts: Data) -> Option<LoopFacts> {
  let Pair(constructor, fields) = builtin.un_constr_data(facts)
  if constructor == 1 {
    expect [inner, cursor, state] = fields
    Some(LoopFacts { inner, cursor: builtin.un_i_data(cursor), state })
  } else {
    None
  }
}

/// One hop of an arm's loop at `expected_progress`. `advance` is given the
/// arm's facts, the cursor and state the previous hop left (`None` on the
/// first hop) and the hop's end. It returns the state after the hop, or
/// `None` once the loop has finished and every final check holds.
///
/// A hop that does not finish re-outputs the bound to the spending script
/// itself with the progress unchanged and the loop's facts; the hop that
/// finishes restores the arm's facts and continues to `next_script_hash`
/// exactly as `continue_core` does.
pub fn continue_loop(
  next_script_hash: ScriptHash,
  computation_thread_policy_id: PolicyId,
  step: CoreDatum,
  action: LoopAction,
  expected_progress: Int,
  own_out_ref: OutputReference,
  tx: Transaction,
  advance: fn(Data, Option<Pair<Int, Data>>, Int) -> Option<Data>,
) -> Bool {
  expect Some(bound) = step.data
  let LoopArm { input_index, output_index, witness, end } = action
  let Pair(inner, from) = when read_loop_facts(bound.facts) is {
    Some(facts) -> Pair(facts.inner, Some(Pair(facts.cursor, facts.state)))
    None -> Pair(bound.facts, None)
  }
  let arm = VerifyArm { input_index, output_index, witness }
  when advance(inner, from, end) is {
    None ->
      continue_core(
        next_script_hash,
        computation_thread_policy_id,
        step,
        arm,
        expected_progress,
        inner,
        own_out_ref,
        tx,
      )
    Some(state) -> {
      expect bound.progress == expected_progress
      expect witness_is_bound(bound, witness)
      let next: Data = CoreBound {
        ..bound,
        facts: loop_facts_data(LoopFacts { inner, cursor: end, state }),
      }
      continue_validation_trace(
        computation_thread_policy_id,
        step,
        input_index,
        output_index,
        own_out_ref,
        tx,
        ProverOnly,
        fn(input_hash, _, _, _, output_hash, output_state) {
          output_hash == input_hash && output_state == next
        },
      )
    }
  }
}
