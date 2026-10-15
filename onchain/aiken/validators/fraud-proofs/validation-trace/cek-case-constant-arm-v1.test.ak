//// Validator-level vectors for the case steps of the machine arm
//// (`cek_core_arm_machine`, group 1). Each runs the deployed validator on one
//// honest case step, so its execution units are those of a real dispute
//// transaction.

use aiken/builtin
use aiken/cbor
use aiken/crypto.{blake2b_256}
use fraud_proofs/validation_trace/cek_core_arm_machine as core_machine
use midgard/cek_builtin_v1
use midgard/cek_constant_v1
use midgard/cek_data_scan_v1
use midgard/cek_data_v1
use midgard/cek_machine_v1
use midgard/cek_proof_v1
use midgard/cek_semantic_chain as core_chain
use midgard/computation_thread as ct
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture

const h_01 = #"0101010101010101010101010101010101010101010101010101010101010101"

const h_02 = #"0202020202020202020202020202020202020202020202020202020202020202"

const ct_asset = "cek-case-thread"

fn state(
  mode: Int,
  focus_root: ByteArray,
  environment_root: ByteArray,
  continuation_root: ByteArray,
  auxiliary: Int,
) -> cek_machine_v1.MachineStateV1 {
  cek_machine_v1.MachineStateV1 {
    mode,
    execution_index: 0,
    focus_root,
    environment_root,
    continuation_root,
    auxiliary,
    cpu: 16100,
    memory: 200,
  }
}

fn run_machine_arm(
  pre: cek_machine_v1.MachineStateV1,
  post: cek_machine_v1.MachineStateV1,
  witness: cek_machine_v1.CoreStepWitnessV1,
) -> Bool {
  let witness_data: Data = witness
  let bound = core_chain.CoreBound {
    prepared: builtin.constr_data(0, []),
    pre,
    post,
    witness_hash: blake2b_256(cbor.serialise(witness_data)),
    arm: builtin.un_constr_data(witness_data).1st,
    group: 1,
    progress: 0,
    facts: builtin.constr_data(0, []),
  }
  let next: Data = core_chain.CoreBound { ..bound, progress: 1 }
  core_machine.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(bound),
      },
    ),
    ct.Continue(
      core_chain.VerifyArm {
        input_index: 0,
        output_index: 0,
        witness: witness_data,
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(ct_asset, next),
  )
}

/// Two branches, `h_01` then `h_02`, closed over the empty environment and
/// returning to the empty stack.
fn two_branches() -> ByteArray {
  cek_proof_v1.hash_sequence_node_v1(
    h_01,
    cek_proof_v1.hash_sequence_node_v1(
      h_02,
      cek_proof_v1.empty_sequence_root_v1,
      1,
    ),
    2,
  )
}

fn two_branch_frame() -> ByteArray {
  cek_proof_v1.hash_case_continuation_v1(
    2,
    two_branches(),
    cek_proof_v1.empty_environment_root_v1,
    cek_proof_v1.empty_continuation_root_v1,
  )
}

fn selecting(
  values_root: ByteArray,
  values_count: Int,
  tag: Int,
) -> cek_machine_v1.MachineStateV1 {
  state(
    cek_machine_v1.mode_case_select,
    two_branches(),
    values_root,
    cek_proof_v1.hash_case_select_continuation_v1(
      cek_proof_v1.empty_environment_root_v1,
      cek_proof_v1.empty_continuation_root_v1,
      values_count,
    ),
    tag,
  )
}

test machine_arm_selects_a_constructor_branch() {
  let empty = cek_proof_v1.empty_sequence_root_v1
  run_machine_arm(
    state(
      cek_machine_v1.mode_return,
      cek_proof_v1.hash_constr_value_v1(1, 0, empty),
      cek_proof_v1.empty_environment_root_v1,
      two_branch_frame(),
      0,
    ),
    selecting(empty, 0, 1),
    cek_machine_v1.ReturnCaseConstr {
      tag: 1,
      values_count: 0,
      values_root: empty,
      branches_count: 2,
      branches_root: two_branches(),
      captured_environment: cek_proof_v1.empty_environment_root_v1,
      tail: cek_proof_v1.empty_continuation_root_v1,
    },
  )
}

test machine_arm_halts_on_a_delay_scrutinee() {
  let environment = cek_proof_v1.empty_environment_root_v1
  let pre = state(
    cek_machine_v1.mode_return,
    cek_proof_v1.hash_delay_value_v1(h_02, environment),
    environment,
    two_branch_frame(),
    0,
  )
  run_machine_arm(
    pre,
    state(
      cek_machine_v1.mode_halt_error,
      cek_proof_v1.hash_error_term_v1(),
      environment,
      cek_proof_v1.empty_continuation_root_v1,
      cek_machine_v1.error_invalid_case_scrutinee,
    ),
    cek_machine_v1.ReturnCaseInvalid {
      value: cek_machine_v1.DelayValue { body: h_02, environment },
      branches_count: 2,
      branches_root: two_branches(),
      captured_environment: environment,
      tail: cek_proof_v1.empty_continuation_root_v1,
    },
  )
}

fn no_material() -> cek_builtin_v1.SemanticBuiltinWitnessV1 {
  cek_builtin_v1.SemanticBuiltinWitnessV1 {
    data_nodes: [],
    list_nodes: [],
    pair_nodes: [],
    scalar_preimages: [],
  }
}

fn inline(
  type_cbor: ByteArray,
  payload: Data,
) -> cek_constant_v1.ConstantWitnessV1 {
  cek_constant_v1.ConstantWitnessV1 {
    type_cbor,
    payload_cbor: cbor.serialise(payload),
  }
}

fn root(type_cbor: ByteArray, payload: Data) -> ByteArray {
  cek_constant_v1.constant_root_v1(inline(type_cbor, payload))
}

fn values(first: ByteArray, second: ByteArray) -> ByteArray {
  cek_proof_v1.hash_sequence_node_v1(
    second,
    cek_proof_v1.hash_sequence_node_v1(
      first,
      cek_proof_v1.empty_sequence_root_v1,
      1,
    ),
    2,
  )
}

/// One case step on a constant, through the two-branch frame.
fn run_case_constant(
  scrutinee: cek_builtin_v1.ValueWitnessV1,
  focus_root: ByteArray,
  material: cek_builtin_v1.SemanticBuiltinWitnessV1,
  post: cek_machine_v1.MachineStateV1,
) -> Bool {
  run_machine_arm(
    state(
      cek_machine_v1.mode_return,
      focus_root,
      cek_proof_v1.empty_environment_root_v1,
      two_branch_frame(),
      0,
    ),
    post,
    cek_machine_v1.ReturnCaseConstant {
      scrutinee,
      material,
      branches_count: 2,
      branches_root: two_branches(),
      captured_environment: cek_proof_v1.empty_environment_root_v1,
      tail: cek_proof_v1.empty_continuation_root_v1,
    },
  )
}

fn run_inline(
  type_cbor: ByteArray,
  payload: Data,
  post: cek_machine_v1.MachineStateV1,
) -> Bool {
  run_case_constant(
    cek_builtin_v1.ConstantValue(inline(type_cbor, payload)),
    root(type_cbor, payload),
    no_material(),
    post,
  )
}

fn halting(reason: Int) -> cek_machine_v1.MachineStateV1 {
  state(
    cek_machine_v1.mode_halt_error,
    cek_proof_v1.hash_error_term_v1(),
    cek_proof_v1.empty_environment_root_v1,
    cek_proof_v1.empty_continuation_root_v1,
    reason,
  )
}

fn small_integers(count: Int) -> List<Data> {
  if count == 0 {
    []
  } else {
    [builtin.i_data(count % 24), ..small_integers(count - 1)]
  }
}

test machine_arm_selects_an_integer_branch() {
  run_inline(
    #"9f00ff",
    builtin.i_data(1),
    selecting(cek_proof_v1.empty_sequence_root_v1, 0, 1),
  )
}

test machine_arm_halts_on_an_integer_past_the_branches() {
  run_inline(
    #"9f00ff",
    builtin.i_data(2),
    halting(cek_machine_v1.error_case_branch_missing),
  )
}

test machine_arm_selects_a_boolean_branch() {
  run_inline(
    #"9f04ff",
    builtin.constr_data(1, []),
    selecting(cek_proof_v1.empty_sequence_root_v1, 0, 1),
  )
}

test machine_arm_halts_on_a_bytestring_scrutinee() {
  run_inline(
    #"9f01ff",
    builtin.b_data(#"00"),
    halting(cek_machine_v1.error_invalid_case_scrutinee),
  )
}

test machine_arm_applies_a_list_branch_to_head_and_tail() {
  run_inline(
    #"9f0500ff",
    builtin.list_data([builtin.i_data(1), builtin.i_data(2)]),
    selecting(
      values(
        root(#"9f00ff", builtin.i_data(1)),
        root(#"9f0500ff", builtin.list_data([builtin.i_data(2)])),
      ),
      2,
      0,
    ),
  )
}

/// A 64-item list of one-byte integers, with the successor of its case step.
fn long_list() -> Pair<Data, cek_machine_v1.MachineStateV1> {
  let items = small_integers(64)
  expect [head, ..tail] = items
  Pair(
    builtin.list_data(items),
    selecting(
      values(root(#"9f00ff", head), root(#"9f0500ff", builtin.list_data(tail))),
      2,
      0,
    ),
  )
}

test machine_arm_applies_a_list_branch_to_a_long_list() {
  let Pair(list, post) = long_list()
  run_inline(#"9f0500ff", list, post)
}

// The cost of building the vector above without running the validator: the
// step itself costs the difference.
test machine_arm_long_list_vector_alone() {
  let Pair(list, post) = long_list()
  root(#"9f0500ff", list) != post.focus_root
}

test machine_arm_halts_on_a_pair_with_two_branches() {
  run_inline(
    #"9f060004ff",
    builtin.constr_data(0, [builtin.i_data(5), builtin.constr_data(1, [])]),
    halting(cek_machine_v1.error_case_branch_missing),
  )
}

test machine_arm_applies_a_list_branch_to_a_wide_list() {
  let bytes = builtin.append_bytearray(
    builtin.replicate_byte(4500, 42),
    builtin.replicate_byte(4500, 42),
  )
  let blob = cek_data_scan_v1.bounded_blob_root_v1(bytes)
  let head = cek_data_v1.bytes_data_summary_v1(9000, blob)
  let link = cek_data_v1.DataListNodeV1 {
    head: head.root,
    head_cbor_length: head.cbor_length,
    head_memory: head.memory,
    tail: cek_data_v1.empty_data_list_root_v1(),
    length: 1,
    payload_cbor_length: head.cbor_length,
    memory: head.memory,
  }
  let items = cek_data_v1.prepend_data_list_summary_v1(
    head,
    cek_data_v1.empty_data_list_summary_v1(),
  )
  let payload = cek_data_v1.list_data_summary_v1(items)
  run_case_constant(
    cek_builtin_v1.SemanticConstantValue {
      type_cbor: #"9f0508ff",
      payload,
      memory: items.memory,
    },
    cek_constant_v1.semantic_constant_root_v1(
      #"9f0508ff",
      payload,
      items.memory,
    ),
    cek_builtin_v1.SemanticBuiltinWitnessV1 {
      data_nodes: [
        cek_data_v1.ListData {
          items_count: 1,
          items_root: items.root,
          cbor_length: payload.cbor_length,
          memory: payload.memory,
        },
        cek_data_v1.BytesData {
          bytes_root: blob,
          bytes_length: 9000,
          cbor_length: head.cbor_length,
          memory: head.memory,
        },
      ],
      list_nodes: [link],
      pair_nodes: [],
      scalar_preimages: [],
    },
    selecting(
      values(
        cek_constant_v1.semantic_constant_root_v1(#"9f08ff", head, head.memory),
        root(#"9f0508ff", builtin.list_data([])),
      ),
      2,
      0,
    ),
  )
}
