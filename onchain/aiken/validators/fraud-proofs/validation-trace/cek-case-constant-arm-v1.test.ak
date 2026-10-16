//// Validator-level vectors for the case steps: the machine arm
//// (`cek_core_arm_machine`, group 1) for constructor and invalid scrutinees,
//// and the case-constant arm (`cek_core_arm_case_constant`, group 11) for
//// constant scrutinees. Each runs the deployed validator on one honest case
//// step, so its execution units are those of a real dispute transaction.

use aiken/builtin
use aiken/cbor
use aiken/crypto.{blake2b_256}
use fraud_proofs/validation_trace/cek_core_arm_case_constant as core_case_constant
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

// Each runner references one arm validator only, so a vector's execution
// units carry no code from the other arm.

/// Runs the machine arm (group 1) on one step bound to `group`.
fn run_machine_bound(
  group: Int,
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
    group,
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

/// Runs the case-constant arm (group 11) on one step bound to `group`.
fn run_case_constant_bound(
  group: Int,
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
    group,
    progress: 0,
    facts: builtin.constr_data(0, []),
  }
  let next: Data = core_chain.CoreBound { ..bound, progress: 1 }
  core_case_constant.main.spend(
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

/// Runs the machine arm on one step bound to the group `core_group` routes
/// it to.
fn run_machine_arm(
  pre: cek_machine_v1.MachineStateV1,
  post: cek_machine_v1.MachineStateV1,
  witness: cek_machine_v1.CoreStepWitnessV1,
) -> Bool {
  let witness_data: Data = witness
  run_machine_bound(
    core_chain.core_group(pre.mode, witness_data),
    pre,
    post,
    witness,
  )
}

/// Runs the case-constant arm on one step bound to the group `core_group`
/// routes it to.
fn run_case_constant_arm(
  pre: cek_machine_v1.MachineStateV1,
  post: cek_machine_v1.MachineStateV1,
  witness: cek_machine_v1.CoreStepWitnessV1,
) -> Bool {
  let witness_data: Data = witness
  run_case_constant_bound(
    core_chain.core_group(pre.mode, witness_data),
    pre,
    post,
    witness,
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
  run_case_constant_arm(
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

test case_constant_arm_selects_an_integer_branch() {
  run_inline(
    #"9f00ff",
    builtin.i_data(1),
    selecting(cek_proof_v1.empty_sequence_root_v1, 0, 1),
  )
}

test case_constant_arm_halts_on_an_integer_past_the_branches() {
  run_inline(
    #"9f00ff",
    builtin.i_data(2),
    halting(cek_machine_v1.error_case_branch_missing),
  )
}

test case_constant_arm_selects_a_boolean_branch() {
  run_inline(
    #"9f04ff",
    builtin.constr_data(1, []),
    selecting(cek_proof_v1.empty_sequence_root_v1, 0, 1),
  )
}

test case_constant_arm_halts_on_a_bytestring_scrutinee() {
  run_inline(
    #"9f01ff",
    builtin.b_data(#"00"),
    halting(cek_machine_v1.error_invalid_case_scrutinee),
  )
}

test case_constant_arm_applies_a_list_branch_to_head_and_tail() {
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

test case_constant_arm_applies_a_list_branch_to_a_long_list() {
  let Pair(list, post) = long_list()
  run_inline(#"9f0500ff", list, post)
}

// The cost of building the vector above without running the validator: the
// step itself costs the difference.
test case_constant_arm_long_list_vector_alone() {
  let Pair(list, post) = long_list()
  root(#"9f0500ff", list) != post.focus_root
}

/// `count` one-byte bytestrings, counting down to `#01`.
fn byte_items(count: Int) -> List<Data> {
  if count == 0 {
    []
  } else {
    [
      builtin.b_data(builtin.cons_bytearray(count, #"")),
      ..byte_items(count - 1)
    ]
  }
}

fn summary_link(
  head: cek_data_v1.DataSummaryV1,
  tail: cek_data_v1.DataSequenceSummaryV1,
) -> cek_data_v1.DataListNodeV1 {
  cek_data_v1.DataListNodeV1 {
    head: head.root,
    head_cbor_length: head.cbor_length,
    head_memory: head.memory,
    tail: tail.root,
    length: tail.length + 1,
    payload_cbor_length: head.cbor_length + tail.payload_cbor_length,
    memory: head.memory + tail.memory,
  }
}

type CommittedList {
  scrutinee: cek_builtin_v1.ValueWitnessV1,
  focus_root: ByteArray,
  material: cek_builtin_v1.SemanticBuiltinWitnessV1,
  post: cek_machine_v1.MachineStateV1,
}

/// `count` booleans, alternating and ending in `False`.
fn booleans(count: Int) -> List<Data> {
  if count == 0 {
    []
  } else {
    [builtin.constr_data(count % 2, []), ..booleans(count - 1)]
  }
}

/// `count` units.
fn units(count: Int) -> List<Data> {
  if count == 0 {
    []
  } else {
    [builtin.constr_data(0, []), ..units(count - 1)]
  }
}

/// The node of a unit, a boolean, an integer or a bytestring item.
fn item_node(item: Data) -> cek_data_v1.DataNodeV1 {
  let summary = cek_data_v1.semantic_data_summary_v1(item)
  builtin.choose_data(
    item,
    cek_data_v1.ConstrSmallData {
      constructor: builtin.un_constr_data(item).1st,
      fields_count: 0,
      fields_root: cek_data_v1.empty_data_list_summary_v1().root,
      cbor_length: summary.cbor_length,
      memory: summary.memory,
    },
    fail,
    fail,
    cek_data_v1.IntegerData {
      cbor_root: cek_data_v1.bounded_blob_root_v1(cbor.serialise(item)),
      cbor_length: summary.cbor_length,
      memory: summary.memory,
    },
    cek_data_v1.BytesData {
      bytes_root: cek_data_v1.bounded_blob_root_v1(builtin.un_b_data(item)),
      bytes_length: builtin.length_of_bytearray(builtin.un_b_data(item)),
      cbor_length: summary.cbor_length,
      memory: summary.memory,
    },
  )
}

/// `items` (at least two one-word items of the type `item_type`) as a list in
/// its committed form, read through its top node, its head node and its first
/// two links, with the successor of its case step.
fn committed_list(
  item_type: ByteArray,
  list_type: ByteArray,
  items: List<Data>,
) -> CommittedList {
  expect [head, second, ..rest] = items
  let rest_items = cek_data_v1.commit_data_list(rest)
  let second_summary = cek_data_v1.semantic_data_summary_v1(second)
  let tail_items = cek_data_v1.prepend_data_list_summary_v1(
    second_summary,
    rest_items,
  )
  let head_summary = cek_data_v1.semantic_data_summary_v1(head)
  let all_items = cek_data_v1.prepend_data_list_summary_v1(
    head_summary,
    tail_items,
  )
  let payload = cek_data_v1.list_data_summary_v1(all_items)
  CommittedList {
    // One word per item.
    scrutinee: cek_builtin_v1.SemanticConstantValue {
      type_cbor: list_type,
      payload,
      memory: all_items.length,
    },
    focus_root: root(list_type, builtin.list_data(items)),
    material: cek_builtin_v1.SemanticBuiltinWitnessV1 {
      data_nodes: [
        cek_data_v1.ListData {
          items_count: all_items.length,
          items_root: all_items.root,
          cbor_length: payload.cbor_length,
          memory: payload.memory,
        },
        item_node(head),
      ],
      list_nodes: [
        summary_link(head_summary, tail_items),
        summary_link(second_summary, rest_items),
      ],
      pair_nodes: [],
      scalar_preimages: [],
    },
    post: selecting(
      values(
        root(item_type, head),
        root(list_type, builtin.list_data([second, ..rest])),
      ),
      2,
      0,
    ),
  }
}

fn run_committed(list: CommittedList) -> Bool {
  run_case_constant(list.scrutinee, list.focus_root, list.material, list.post)
}

// Lists of 100 items of each type a committed list is read with are proved in
// their committed form, at a cost that does not grow with the list. Each
// vector's cost alone is measured beside it: the step costs the difference.

test case_constant_arm_applies_a_list_branch_to_a_committed_bytestring_list() {
  run_committed(committed_list(#"9f01ff", #"9f0501ff", byte_items(100)))
}

test case_constant_arm_committed_bytestring_list_vector_alone() {
  let list = committed_list(#"9f01ff", #"9f0501ff", byte_items(100))
  list.focus_root != list.post.focus_root
}

test case_constant_arm_applies_a_list_branch_to_a_committed_integer_list() {
  run_committed(committed_list(#"9f00ff", #"9f0500ff", small_integers(100)))
}

test case_constant_arm_committed_integer_list_vector_alone() {
  let list = committed_list(#"9f00ff", #"9f0500ff", small_integers(100))
  list.focus_root != list.post.focus_root
}

test case_constant_arm_applies_a_list_branch_to_a_committed_boolean_list() {
  run_committed(committed_list(#"9f04ff", #"9f0504ff", booleans(100)))
}

test case_constant_arm_committed_boolean_list_vector_alone() {
  let list = committed_list(#"9f04ff", #"9f0504ff", booleans(100))
  list.focus_root != list.post.focus_root
}

test case_constant_arm_applies_a_list_branch_to_a_committed_unit_list() {
  run_committed(committed_list(#"9f03ff", #"9f0503ff", units(100)))
}

test case_constant_arm_committed_unit_list_vector_alone() {
  let list = committed_list(#"9f03ff", #"9f0503ff", units(100))
  list.focus_root != list.post.focus_root
}

test case_constant_arm_halts_on_a_pair_with_two_branches() {
  run_inline(
    #"9f060004ff",
    builtin.constr_data(0, [builtin.i_data(5), builtin.constr_data(1, [])]),
    halting(cek_machine_v1.error_case_branch_missing),
  )
}

test case_constant_arm_applies_a_list_branch_to_a_wide_list() {
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

fn integer_case_witness() -> cek_machine_v1.CoreStepWitnessV1 {
  cek_machine_v1.ReturnCaseConstant {
    scrutinee: cek_builtin_v1.ConstantValue(
      inline(#"9f00ff", builtin.i_data(1)),
    ),
    material: no_material(),
    branches_count: 2,
    branches_root: two_branches(),
    captured_environment: cek_proof_v1.empty_environment_root_v1,
    tail: cek_proof_v1.empty_continuation_root_v1,
  }
}

fn integer_case_pre() -> cek_machine_v1.MachineStateV1 {
  state(
    cek_machine_v1.mode_return,
    root(#"9f00ff", builtin.i_data(1)),
    cek_proof_v1.empty_environment_root_v1,
    two_branch_frame(),
    0,
  )
}

// A case step on a constant belongs to group 11: the machine arm refuses it.
test machine_arm_refuses_a_case_constant_step() fail {
  run_machine_bound(
    1,
    integer_case_pre(),
    selecting(cek_proof_v1.empty_sequence_root_v1, 0, 1),
    integer_case_witness(),
  )
}

test case_constant_arm_refuses_a_wrong_successor() fail {
  run_case_constant_arm(
    integer_case_pre(),
    selecting(cek_proof_v1.empty_sequence_root_v1, 0, 0),
    integer_case_witness(),
  )
}

// The case-constant arm verifies only `ReturnCaseConstant` witnesses.
test case_constant_arm_refuses_a_constructor_step() fail {
  let empty = cek_proof_v1.empty_sequence_root_v1
  run_case_constant_bound(
    11,
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

// `core_group` routes a return step on a constant to group 11 and one on a
// constructor to group 1.
test core_group_routes_a_case_constant_step_to_group_11() {
  let witness: Data = integer_case_witness()
  core_chain.core_group(cek_machine_v1.mode_return, witness) == 11
}

test core_group_routes_a_constructor_case_step_to_group_1() {
  let witness: Data = cek_machine_v1.ReturnCaseConstr {
    tag: 1,
    values_count: 0,
    values_root: cek_proof_v1.empty_sequence_root_v1,
    branches_count: 2,
    branches_root: two_branches(),
    captured_environment: cek_proof_v1.empty_environment_root_v1,
    tail: cek_proof_v1.empty_continuation_root_v1,
  }
  core_chain.core_group(cek_machine_v1.mode_return, witness) == 1
}

// The case-constant arm requires the bound group to be its own.
test case_constant_arm_refuses_another_group() fail {
  run_case_constant_bound(
    1,
    integer_case_pre(),
    selecting(cek_proof_v1.empty_sequence_root_v1, 0, 1),
    integer_case_witness(),
  )
}
