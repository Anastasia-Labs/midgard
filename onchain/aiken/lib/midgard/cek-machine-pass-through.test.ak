use aiken/collection/list
use aiken/crypto.{blake2b_256}
use midgard/cek_builtin_v1
use midgard/cek_constant_v1
use midgard/cek_data_v1
use midgard/cek_machine_v1
use midgard/cek_proof_v1

// ifThenElse, chooseUnit, trace, chooseList and chooseData pass a branch
// through without reading it. Each branch and the result are presented by
// value root alone; the step binds every branch root through the pre-state
// builtin value, and the result root to the selected branch.

const h_01 = #"0101010101010101010101010101010101010101010101010101010101010101"

const h_02 = #"0202020202020202020202020202020202020202020202020202020202020202"

const h_03 = #"0303030303030303030303030303030303030303030303030303030303030303"

fn constant(
  type_cbor: ByteArray,
  payload_cbor: ByteArray,
) -> cek_builtin_v1.ValueWitnessV1 {
  cek_builtin_v1.ConstantValue(
    cek_constant_v1.ConstantWitnessV1 { type_cbor, payload_cbor },
  )
}

/// A bytestring branch held by its value root alone.
fn branch(payload_cbor: ByteArray) -> cek_builtin_v1.ValueWitnessV1 {
  cek_builtin_v1.OpaqueValue(
    cek_constant_v1.constant_root_v1(
      cek_constant_v1.ConstantWitnessV1 { type_cbor: #"9f01ff", payload_cbor },
    ),
  )
}

fn root_of(value: cek_builtin_v1.ValueWitnessV1) -> ByteArray {
  cek_builtin_v1.result_root_v1(value)
}

fn forged(value: cek_builtin_v1.ValueWitnessV1) -> cek_builtin_v1.ValueWitnessV1 {
  cek_builtin_v1.OpaqueValue(blake2b_256(root_of(value)))
}

fn builtin_pre(
  tag: Int,
  arguments: List<cek_builtin_v1.ValueWitnessV1>,
) -> cek_machine_v1.MachineStateV1 {
  let Pair(arguments_root, arguments_count) = cek_builtin_v1.arguments_root_v1(
    arguments,
  )
  cek_machine_v1.MachineStateV1 {
    mode: cek_machine_v1.mode_builtin,
    execution_index: 0,
    focus_root: cek_proof_v1.hash_builtin_value_v1(
      tag,
      0,
      arguments_count,
      arguments_root,
    ),
    environment_root: cek_proof_v1.empty_environment_root_v1,
    continuation_root: h_01,
    auxiliary: 0,
    cpu: 100,
    memory: 100,
  }
}

type PassThroughCase {
  tag: Int,
  arguments: List<cek_builtin_v1.ValueWitnessV1>,
  selected: Int,
  material: Option<cek_builtin_v1.SemanticBuiltinWitnessV1>,
}

fn if_then_else() -> PassThroughCase {
  PassThroughCase {
    tag: 26,
    arguments: [
      constant(#"9f04ff", #"d87a80"),
      branch(#"4101"),
      branch(#"4102"),
    ],
    selected: 1,
    material: None,
  }
}

fn choose_unit() -> PassThroughCase {
  PassThroughCase {
    tag: 27,
    arguments: [constant(#"9f03ff", #"d87980"), branch(#"4101")],
    selected: 1,
    material: None,
  }
}

fn trace_message() -> PassThroughCase {
  PassThroughCase {
    tag: 28,
    arguments: [constant(#"9f02ff", #"416d"), branch(#"4101")],
    selected: 1,
    material: None,
  }
}

fn choose_list() -> PassThroughCase {
  let items = cek_data_v1.empty_data_list_summary_v1()
  PassThroughCase {
    tag: 31,
    arguments: [constant(#"9f0500ff", #"80"), branch(#"4101"), branch(#"4102")],
    selected: 1,
    material: Some(
      cek_builtin_v1.SemanticBuiltinWitnessV1 {
        data_nodes: [
          cek_data_v1.ListData {
            items_count: 0,
            items_root: items.root,
            cbor_length: cek_data_v1.list_data_summary_v1(items).cbor_length,
            memory: 4 + items.memory,
          },
        ],
        list_nodes: [],
        pair_nodes: [],
        scalar_preimages: [],
      },
    ),
  }
}

fn choose_data() -> PassThroughCase {
  PassThroughCase {
    tag: 36,
    arguments: [
      constant(#"9f08ff", #"01"),
      branch(#"4101"),
      branch(#"4102"),
      branch(#"4103"),
      branch(#"4104"),
      branch(#"4105"),
    ],
    selected: 4,
    material: Some(
      cek_builtin_v1.SemanticBuiltinWitnessV1 {
        data_nodes: [
          cek_data_v1.IntegerData {
            cbor_root: cek_data_v1.bounded_blob_root_v1(#"01"),
            cbor_length: 1,
            memory: 4 + cek_data_v1.integer_memory_size_v1(1),
          },
        ],
        list_nodes: [],
        pair_nodes: [],
        scalar_preimages: [],
      },
    ),
  }
}

/// Verifies the step whose pre-state binds the case's own arguments, presented
/// with `arguments` and `result`. The post-state follows the presentation, so
/// only the root bindings can refuse it.
fn proves(
  case: PassThroughCase,
  arguments: List<cek_builtin_v1.ValueWitnessV1>,
  result: cek_builtin_v1.ValueWitnessV1,
) -> Bool {
  let pre = builtin_pre(case.tag, case.arguments)
  let budget = cek_builtin_v1.direct_builtin_budget_v1(case.tag, arguments)
  let post = cek_machine_v1.exact_state(
    pre,
    cek_machine_v1.mode_return,
    root_of(result),
    cek_proof_v1.empty_environment_root_v1,
    pre.continuation_root,
    0,
    budget.cpu,
    budget.memory,
  )
  cek_machine_v1.verify_core_step_v1(
    pre,
    post,
    when case.material is {
      None ->
        cek_machine_v1.ExecuteBuiltinDirect { tag: case.tag, arguments, result }
      Some(material) ->
        cek_machine_v1.ExecuteBuiltinSemantic {
          tag: case.tag,
          arguments,
          result,
          material,
        }
    },
  )
}

fn selected_of(case: PassThroughCase) -> cek_builtin_v1.ValueWitnessV1 {
  expect Some(selected) = list.at(case.arguments, case.selected)
  selected
}

fn proves_honestly(case: PassThroughCase) -> Bool {
  proves(case, case.arguments, selected_of(case))
}

/// The branch at `index` replaced by a forged root; a forged selected branch
/// is also the result, so the selection still agrees.
fn proves_forged_branch(case: PassThroughCase, index: Int) -> Bool {
  expect Some(original) = list.at(case.arguments, index)
  let arguments = list.indexed_map(
    case.arguments,
    fn(at, argument) {
      if at == index {
        forged(original)
      } else {
        argument
      }
    },
  )
  proves(
    case,
    arguments,
    if index == case.selected {
      forged(original)
    } else {
      selected_of(case)
    },
  )
}

fn proves_result_of(case: PassThroughCase, index: Int) -> Bool {
  expect Some(other) = list.at(case.arguments, index)
  proves(case, case.arguments, cek_builtin_v1.OpaqueValue(root_of(other)))
}

fn proves_forged_result(case: PassThroughCase) -> Bool {
  proves(case, case.arguments, forged(selected_of(case)))
}

test if_then_else_passes_opaque_branches_through() {
  proves_honestly(if_then_else())
}

test if_then_else_refuses_a_forged_selected_branch() fail {
  proves_forged_branch(if_then_else(), 1)
}

test if_then_else_refuses_a_forged_unselected_branch() fail {
  proves_forged_branch(if_then_else(), 2)
}

test if_then_else_refuses_the_unselected_branch_as_result() fail {
  proves_result_of(if_then_else(), 2)
}

test if_then_else_refuses_a_forged_result() fail {
  proves_forged_result(if_then_else())
}

test choose_unit_passes_an_opaque_branch_through() {
  proves_honestly(choose_unit())
}

test choose_unit_refuses_a_forged_branch() fail {
  proves_forged_branch(choose_unit(), 1)
}

test choose_unit_refuses_the_unit_as_result() fail {
  proves_result_of(choose_unit(), 0)
}

test choose_unit_refuses_a_forged_result() fail {
  proves_forged_result(choose_unit())
}

test trace_passes_an_opaque_branch_through() {
  proves_honestly(trace_message())
}

test trace_refuses_a_forged_branch() fail {
  proves_forged_branch(trace_message(), 1)
}

test trace_refuses_the_message_as_result() fail {
  proves_result_of(trace_message(), 0)
}

test trace_refuses_a_forged_result() fail {
  proves_forged_result(trace_message())
}

test choose_list_passes_opaque_branches_through() {
  proves_honestly(choose_list())
}

test choose_list_refuses_a_forged_selected_branch() fail {
  proves_forged_branch(choose_list(), 1)
}

test choose_list_refuses_a_forged_unselected_branch() fail {
  proves_forged_branch(choose_list(), 2)
}

test choose_list_refuses_the_unselected_branch_as_result() fail {
  proves_result_of(choose_list(), 2)
}

test choose_list_refuses_a_forged_result() fail {
  proves_forged_result(choose_list())
}

test choose_data_passes_opaque_branches_through() {
  proves_honestly(choose_data())
}

test choose_data_refuses_a_forged_constructor_branch() fail {
  proves_forged_branch(choose_data(), 1)
}

test choose_data_refuses_a_forged_map_branch() fail {
  proves_forged_branch(choose_data(), 2)
}

test choose_data_refuses_a_forged_list_branch() fail {
  proves_forged_branch(choose_data(), 3)
}

test choose_data_refuses_a_forged_integer_branch() fail {
  proves_forged_branch(choose_data(), 4)
}

test choose_data_refuses_a_forged_bytes_branch() fail {
  proves_forged_branch(choose_data(), 5)
}

test choose_data_refuses_the_constructor_branch_as_result() fail {
  proves_result_of(choose_data(), 1)
}

test choose_data_refuses_the_map_branch_as_result() fail {
  proves_result_of(choose_data(), 2)
}

test choose_data_refuses_the_list_branch_as_result() fail {
  proves_result_of(choose_data(), 3)
}

test choose_data_refuses_the_bytes_branch_as_result() fail {
  proves_result_of(choose_data(), 5)
}

test choose_data_refuses_a_forged_result() fail {
  proves_forged_result(choose_data())
}

fn failure_post(
  pre: cek_machine_v1.MachineStateV1,
) -> cek_machine_v1.MachineStateV1 {
  cek_machine_v1.exact_state(
    pre,
    cek_machine_v1.mode_halt_error,
    cek_proof_v1.hash_error_term_v1(),
    cek_proof_v1.empty_environment_root_v1,
    cek_proof_v1.empty_continuation_root_v1,
    cek_machine_v1.error_builtin_failure,
    0,
    0,
  )
}

/// A division by zero whose pre-state binds `[1, 0]`, presented with a
/// different dividend that fails the same way.
fn failure_proves(dividend: ByteArray) -> Bool {
  let pre = builtin_pre(
    3,
    [constant(#"9f00ff", #"01"), constant(#"9f00ff", #"00")],
  )
  cek_machine_v1.verify_core_step_v1(
    pre,
    failure_post(pre),
    cek_machine_v1.ExecuteBuiltinFailure {
      tag: 3,
      arguments: [constant(#"9f00ff", dividend), constant(#"9f00ff", #"00")],
    },
  )
}

test builtin_failure_proves_the_arguments_the_focus_binds() {
  failure_proves(#"01")
}

test builtin_failure_refuses_arguments_the_focus_does_not_bind() fail {
  failure_proves(#"02")
}

fn type_failure_arguments(
  body: ByteArray,
) -> List<cek_builtin_v1.RuntimeValueWitnessV1> {
  [
    cek_builtin_v1.RuntimeLambdaValue {
      body,
      environment: cek_proof_v1.empty_environment_root_v1,
    },
    cek_builtin_v1.RuntimeConstantValue(
      cek_constant_v1.ConstantWitnessV1 {
        type_cbor: #"9f00ff",
        payload_cbor: #"01",
      },
    ),
  ]
}

/// An addition applied to a lambda whose pre-state binds the body `h_02`,
/// presented with the lambda body `body`.
fn type_failure_proves(body: ByteArray) -> Bool {
  let Pair(arguments_root, arguments_count) = cek_builtin_v1.runtime_arguments_root_v1(
    type_failure_arguments(h_02),
  )
  let pre = cek_machine_v1.MachineStateV1 {
    ..builtin_pre(0, []),
    focus_root: cek_proof_v1.hash_builtin_value_v1(
      0,
      0,
      arguments_count,
      arguments_root,
    ),
  }
  cek_machine_v1.verify_core_step_v1(
    pre,
    failure_post(pre),
    cek_machine_v1.ExecuteBuiltinTypeFailure {
      tag: 0,
      arguments: type_failure_arguments(body),
    },
  )
}

test builtin_type_failure_proves_the_arguments_the_focus_binds() {
  type_failure_proves(h_02)
}

test builtin_type_failure_refuses_arguments_the_focus_does_not_bind() fail {
  type_failure_proves(h_03)
}
