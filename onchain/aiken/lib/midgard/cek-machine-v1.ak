use aiken/builtin
use aiken/cbor
use aiken/primitive/bytearray
use midgard/cek_builtin_v1
use midgard/cek_constant_v1
use midgard/cek_data_v1
use midgard/cek_proof_v1
use midgard/fraud_proofs/native_tx/codec.{
  encode_definite_array_header, encode_definite_bytes,
}

pub const mode_compute = 0

pub const mode_return = 1

pub const mode_lookup = 2

pub const mode_builtin = 3

pub const mode_halt_success = 4

pub const mode_halt_error = 5

pub const mode_case_select = 6

pub const mode_case_apply = 7

pub const mode_semantic_builtin = 8

pub const error_explicit = 0

pub const error_unbound_variable = 1

pub const error_invalid_application = 2

pub const error_invalid_force = 3

pub const error_nonconstant_halt = 4

pub const error_invalid_case_scrutinee = 5

pub const error_case_branch_missing = 6

pub const error_builtin_failure = 7

const machine_step_cpu = 16000

const machine_step_memory = 100

pub type MachineStateV1 {
  mode: Int,
  execution_index: Int,
  focus_root: ByteArray,
  environment_root: ByteArray,
  continuation_root: ByteArray,
  auxiliary: Int,
  cpu: Int,
  memory: Int,
}

pub type EnvironmentSummaryV1 {
  EmptyEnvironmentSummary
  NonEmptyEnvironmentSummary { value: ByteArray, tail: ByteArray, length: Int }
}

pub type MapConversionControlV1 {
  tag: Int,
  result_root: ByteArray,
  source_root: ByteArray,
  source_remaining: Int,
  source_payload_cbor_length: Int,
  source_memory: Int,
  destination_root: ByteArray,
  destination_remaining: Int,
  destination_payload_cbor_length: Int,
  destination_memory: Int,
  budget_cpu: Int,
  budget_memory: Int,
}

pub type MapConversionStartWitnessV1 {
  source_node: cek_data_v1.DataNodeV1,
  source_list: Option<cek_data_v1.DataListNodeV1>,
  source_pairs: Option<cek_data_v1.DataPairNodeV1>,
  result_node: cek_data_v1.DataNodeV1,
  result_list: Option<cek_data_v1.DataListNodeV1>,
  result_pairs: Option<cek_data_v1.DataPairNodeV1>,
}

pub type ValueWitnessV1 {
  ConstantValue {
    type_root: ByteArray,
    payload_root: ByteArray,
    payload_length: Int,
    semantic_root: ByteArray,
    memory: Int,
  }
  LambdaValue { body: ByteArray, environment: ByteArray }
  DelayValue { body: ByteArray, environment: ByteArray }
  ConstrValue { tag: Int, values_count: Int, values_root: ByteArray }
  BuiltinValue {
    tag: Int,
    forces_remaining: Int,
    arguments_count: Int,
    arguments_root: ByteArray,
  }
  BlsMillerLoopValue { expression_root: ByteArray }
}

pub type CoreStepWitnessV1 {
  ComputeVariable { index: Int }
  ComputeConstant { value: cek_constant_v1.ConstantWitnessV1 }
  ComputeLambda { body: ByteArray }
  ComputeDelay { body: ByteArray }
  ComputeApplication { function: ByteArray, argument: ByteArray }
  ComputeForce { term: ByteArray }
  ComputeError
  ComputeBuiltin { tag: Int }
  ComputeConstrEmpty { tag: Int }
  ComputeConstrNonEmpty {
    tag: Int,
    terms_count: Int,
    first_term: ByteArray,
    remaining_terms_root: ByteArray,
  }
  ComputeCase {
    scrutinee: ByteArray,
    branches_count: Int,
    branches_root: ByteArray,
  }
  LookupEnvironment { value: ByteArray, tail: ByteArray, length: Int }
  LookupEmptyEnvironment
  ReturnEmptyContinuation { value: ValueWitnessV1 }
  ReturnApplyArgument {
    argument: ByteArray,
    captured_environment: ByteArray,
    tail: ByteArray,
  }
  ReturnApplyLambda {
    body: ByteArray,
    closure_environment: ByteArray,
    closure_summary: EnvironmentSummaryV1,
    tail: ByteArray,
  }
  ReturnApplyBuiltin {
    tag: Int,
    forces_remaining: Int,
    arguments_count: Int,
    arguments_root: ByteArray,
    tail: ByteArray,
  }
  ReturnApplyInvalid { function: ValueWitnessV1, tail: ByteArray }
  ReturnApplyValueLambda {
    argument: ByteArray,
    body: ByteArray,
    closure_environment: ByteArray,
    closure_summary: EnvironmentSummaryV1,
    tail: ByteArray,
  }
  ReturnApplyValueBuiltin {
    argument: ByteArray,
    tag: Int,
    forces_remaining: Int,
    arguments_count: Int,
    arguments_root: ByteArray,
    tail: ByteArray,
  }
  ReturnApplyValueInvalid {
    argument: ByteArray,
    function: ValueWitnessV1,
    tail: ByteArray,
  }
  ReturnForceDelay {
    body: ByteArray,
    closure_environment: ByteArray,
    tail: ByteArray,
  }
  ReturnForceBuiltin {
    tag: Int,
    forces_remaining: Int,
    arguments_count: Int,
    arguments_root: ByteArray,
    tail: ByteArray,
  }
  ReturnForceInvalid { value: ValueWitnessV1, tail: ByteArray }
  ReturnConstrNext {
    tag: Int,
    remaining_terms_count: Int,
    next_term: ByteArray,
    remaining_terms_tail: ByteArray,
    values_count: Int,
    values_root: ByteArray,
    captured_environment: ByteArray,
    tail: ByteArray,
  }
  ReturnConstrDone {
    tag: Int,
    values_count: Int,
    values_root: ByteArray,
    captured_environment: ByteArray,
    tail: ByteArray,
  }
  ReturnCaseConstr {
    tag: Int,
    values_count: Int,
    values_root: ByteArray,
    branches_count: Int,
    branches_root: ByteArray,
    captured_environment: ByteArray,
    tail: ByteArray,
  }
  ReturnCaseInvalid {
    value: ValueWitnessV1,
    branches_count: Int,
    branches_root: ByteArray,
    captured_environment: ByteArray,
    tail: ByteArray,
  }
  SelectCaseBranch {
    branch: ByteArray,
    remaining_branches_root: ByteArray,
    length: Int,
    captured_environment: ByteArray,
    tail: ByteArray,
    values_count: Int,
  }
  ApplyCaseValue {
    value: ByteArray,
    remaining_values_root: ByteArray,
    length: Int,
    captured_environment: ByteArray,
    built_continuation: ByteArray,
  }
  ExecuteBuiltinDirect {
    tag: Int,
    arguments: List<cek_builtin_v1.ValueWitnessV1>,
    result: cek_builtin_v1.ValueWitnessV1,
  }
  ExecuteBuiltinSemantic {
    tag: Int,
    arguments: List<cek_builtin_v1.ValueWitnessV1>,
    result: cek_builtin_v1.ValueWitnessV1,
    material: cek_builtin_v1.SemanticBuiltinWitnessV1,
  }
  StartBuiltinMapConversion {
    tag: Int,
    arguments: List<cek_builtin_v1.ValueWitnessV1>,
    result: cek_builtin_v1.ValueWitnessV1,
    material: MapConversionStartWitnessV1,
  }
  StepBuiltinListToMap {
    control: MapConversionControlV1,
    source: cek_data_v1.DataListNodeV1,
    pair: cek_data_v1.DataNodeV1,
    first: cek_data_v1.DataListNodeV1,
    second: cek_data_v1.DataListNodeV1,
    key: cek_data_v1.DataNodeV1,
    value: cek_data_v1.DataNodeV1,
    destination: cek_data_v1.DataPairNodeV1,
  }
  StepBuiltinMapToList {
    control: MapConversionControlV1,
    source: cek_data_v1.DataPairNodeV1,
    destination: cek_data_v1.DataListNodeV1,
    pair: cek_data_v1.DataNodeV1,
    first: cek_data_v1.DataListNodeV1,
    second: cek_data_v1.DataListNodeV1,
    key: cek_data_v1.DataNodeV1,
    value: cek_data_v1.DataNodeV1,
  }
  FinishBuiltinMapConversion { control: MapConversionControlV1 }
  ExecuteBuiltinSemanticFailure {
    tag: Int,
    arguments: List<cek_builtin_v1.ValueWitnessV1>,
    material: cek_builtin_v1.SemanticBuiltinWitnessV1,
  }
  ExecuteBuiltinBlsFinal {
    left_root: ByteArray,
    right_root: ByteArray,
    left: cek_builtin_v1.BlsExpressionWitnessV1,
    right: cek_builtin_v1.BlsExpressionWitnessV1,
    result: cek_builtin_v1.ValueWitnessV1,
  }
  ExecuteBuiltinFailure {
    tag: Int,
    arguments: List<cek_builtin_v1.ValueWitnessV1>,
  }
  ExecuteBuiltinTypeFailure {
    tag: Int,
    arguments: List<cek_builtin_v1.RuntimeValueWitnessV1>,
  }
  ComputeContextConstant { value_root: ByteArray }
}

pub type CoreStepEvidenceV1 {
  pre: MachineStateV1,
  post: MachineStateV1,
  witness: CoreStepWitnessV1,
}

fn root_is_hash(root: ByteArray) -> Bool {
  bytearray.length(root) == 32
}

pub fn state_is_well_formed(state: MachineStateV1) -> Bool {
  and {
    state.mode >= mode_compute,
    state.mode <= mode_semantic_builtin,
    state.execution_index >= 0,
    state.execution_index <= 4294967295,
    root_is_hash(state.focus_root),
    root_is_hash(state.environment_root),
    root_is_hash(state.continuation_root),
    state.auxiliary >= 0,
    state.auxiliary <= 18446744073709551615,
    state.cpu >= 0,
    state.cpu <= 18446744073709551615,
    state.memory >= 0,
    state.memory <= 18446744073709551615,
  }
}

pub fn hash_state_v1(state: MachineStateV1) -> ByteArray {
  expect state_is_well_formed(state)
  cek_proof_v1.hash_machine_state_v1(
    state.mode,
    state.execution_index,
    state.focus_root,
    state.environment_root,
    state.continuation_root,
    state.auxiliary,
    state.cpu,
    state.memory,
  )
}

pub fn builtin_force_count(tag: Int) -> Int {
  expect tag >= 0
  expect tag <= cek_proof_v1.max_builtin_tag
  if or {
    tag == 29,
    tag == 30,
    tag == 31,
  } {
    2
  } else if or {
    tag == 26,
    tag == 27,
    tag == 28,
    tag == 32,
    tag == 33,
    tag == 34,
    tag == 35,
    tag == 36,
  } {
    1
  } else {
    0
  }
}

pub fn builtin_argument_count(tag: Int) -> Int {
  expect tag >= 0
  expect tag <= cek_proof_v1.max_builtin_tag
  if tag == 36 {
    6
  } else if or {
    tag == 12,
    tag == 21,
    tag == 26,
    tag == 31,
    tag == 52,
    tag == 53,
    tag == 73,
    tag == 75,
    tag == 76,
    tag == 77,
    tag == 80,
  } {
    3
  } else if or {
    tag <= 11,
    tag == 14,
    tag == 15,
    tag == 16,
    tag == 17,
    tag == 22,
    tag == 23,
    tag == 27,
    tag == 28,
    tag == 32,
    tag == 37,
    tag == 47,
    tag == 48,
    tag == 54,
    tag == 56,
    tag == 57,
    tag == 58,
    tag == 61,
    tag == 63,
    tag == 64,
    tag == 65,
    tag == 68,
    tag == 69,
    tag == 70,
    tag == 74,
    tag == 79,
    tag == 81,
    tag == 82,
    tag == 83,
  } {
    2
  } else {
    1
  }
}

fn value_witness_hash(value: ValueWitnessV1) -> ByteArray {
  when value is {
    ConstantValue {
      type_root,
      payload_root,
      payload_length,
      semantic_root,
      memory,
    } ->
      cek_proof_v1.hash_constant_value_v1(
        type_root,
        payload_root,
        payload_length,
        semantic_root,
        memory,
      )
    LambdaValue { body, environment } ->
      cek_proof_v1.hash_lambda_value_v1(body, environment)
    DelayValue { body, environment } ->
      cek_proof_v1.hash_delay_value_v1(body, environment)
    ConstrValue { tag, values_count, values_root } ->
      cek_proof_v1.hash_constr_value_v1(tag, values_count, values_root)
    BuiltinValue { tag, forces_remaining, arguments_count, arguments_root } ->
      cek_proof_v1.hash_builtin_value_v1(
        tag,
        forces_remaining,
        arguments_count,
        arguments_root,
      )
    BlsMillerLoopValue { expression_root } ->
      cek_proof_v1.hash_bls_miller_loop_value_v1(expression_root)
  }
}

fn value_is_constant(value: ValueWitnessV1) -> Bool {
  when value is {
    ConstantValue { .. } -> True
    _ -> False
  }
}

fn value_is_lambda_or_builtin(value: ValueWitnessV1) -> Bool {
  when value is {
    LambdaValue { .. } -> True
    BuiltinValue { .. } -> True
    _ -> False
  }
}

fn value_is_delay_or_forceable_builtin(value: ValueWitnessV1) -> Bool {
  when value is {
    DelayValue { .. } -> True
    BuiltinValue { forces_remaining, .. } -> forces_remaining > 0
    _ -> False
  }
}

fn value_is_constr(value: ValueWitnessV1) -> Bool {
  when value is {
    ConstrValue { .. } -> True
    _ -> False
  }
}

fn linked_sequence_tail_is_well_formed(tail: ByteArray, length: Int) -> Bool {
  if length == 1 {
    tail == cek_proof_v1.empty_sequence_root_v1
  } else {
    tail != cek_proof_v1.empty_sequence_root_v1
  }
}

fn linked_sequence_root_is_well_formed(root: ByteArray, count: Int) -> Bool {
  if count == 0 {
    root == cek_proof_v1.empty_sequence_root_v1
  } else {
    root != cek_proof_v1.empty_sequence_root_v1
  }
}

fn environment_summary_matches(
  root: ByteArray,
  summary: EnvironmentSummaryV1,
) -> Bool {
  when summary is {
    EmptyEnvironmentSummary -> root == cek_proof_v1.empty_environment_root_v1
    NonEmptyEnvironmentSummary { value, tail, length } -> and {
        length > 0,
        root == cek_proof_v1.hash_environment_node_v1(value, tail, length),
        if length == 1 {
          tail == cek_proof_v1.empty_environment_root_v1
        } else {
          tail != cek_proof_v1.empty_environment_root_v1
        },
      }
  }
}

fn environment_summary_length(summary: EnvironmentSummaryV1) -> Int {
  when summary is {
    EmptyEnvironmentSummary -> 0
    NonEmptyEnvironmentSummary { length, .. } -> length
  }
}

fn exact_state(
  pre: MachineStateV1,
  mode: Int,
  focus_root: ByteArray,
  environment_root: ByteArray,
  continuation_root: ByteArray,
  auxiliary: Int,
  cpu_delta: Int,
  memory_delta: Int,
) -> MachineStateV1 {
  MachineStateV1 {
    mode,
    execution_index: pre.execution_index,
    focus_root,
    environment_root,
    continuation_root,
    auxiliary,
    cpu: pre.cpu + cpu_delta,
    memory: pre.memory + memory_delta,
  }
}

const map_conversion_control_domain =
  #"4d69646761726443656b4d6170436f6e76657273696f6e436f6e74726f6c5631"

pub fn encode_map_conversion_control_v1(
  control: MapConversionControlV1,
) -> ByteArray {
  encode_definite_array_header(12)
    |> bytearray.concat(cbor.serialise(control.tag))
    |> bytearray.concat(encode_definite_bytes(control.result_root))
    |> bytearray.concat(encode_definite_bytes(control.source_root))
    |> bytearray.concat(cbor.serialise(control.source_remaining))
    |> bytearray.concat(cbor.serialise(control.source_payload_cbor_length))
    |> bytearray.concat(cbor.serialise(control.source_memory))
    |> bytearray.concat(encode_definite_bytes(control.destination_root))
    |> bytearray.concat(cbor.serialise(control.destination_remaining))
    |> bytearray.concat(cbor.serialise(control.destination_payload_cbor_length))
    |> bytearray.concat(cbor.serialise(control.destination_memory))
    |> bytearray.concat(cbor.serialise(control.budget_cpu))
    |> bytearray.concat(cbor.serialise(control.budget_memory))
}

fn map_conversion_control_is_well_formed_v1(
  control: MapConversionControlV1,
) -> Bool {
  and {
    or {
      control.tag == 38,
      control.tag == 43,
    },
    root_is_hash(control.result_root),
    root_is_hash(control.source_root),
    root_is_hash(control.destination_root),
    control.source_remaining >= 0,
    control.source_remaining == control.destination_remaining,
    control.source_payload_cbor_length >= 0,
    control.source_memory >= 0,
    control.destination_payload_cbor_length >= 0,
    control.destination_memory >= 0,
    control.budget_cpu >= 0,
    control.budget_memory >= 0,
    if control.source_remaining == 0 {
      and {
        control.source_payload_cbor_length == 0,
        control.source_memory == 0,
        control.destination_payload_cbor_length == 0,
        control.destination_memory == 0,
        if control.tag == 38 {
          and {
            control.source_root == cek_data_v1.empty_data_list_root_v1(),
            control.destination_root == cek_data_v1.empty_data_pair_root_v1(),
          }
        } else {
          and {
            control.source_root == cek_data_v1.empty_data_pair_root_v1(),
            control.destination_root == cek_data_v1.empty_data_list_root_v1(),
          }
        },
      }
    } else {
      True
    },
  }
}

pub fn hash_map_conversion_control_v1(
  control: MapConversionControlV1,
) -> ByteArray {
  expect map_conversion_control_is_well_formed_v1(control)
  map_conversion_control_domain
    |> bytearray.concat(encode_map_conversion_control_v1(control))
    |> builtin.blake2b_256
}

fn data_summary_from_node_v1(
  node: cek_data_v1.DataNodeV1,
) -> cek_data_v1.DataSummaryV1 {
  cek_data_v1.DataSummaryV1 {
    root: cek_data_v1.hash_data_node_v1(node),
    cbor_length: cek_data_v1.data_node_cbor_length_v1(node),
    memory: cek_data_v1.data_node_memory_v1(node),
  }
}

fn list_sequence_from_node_v1(
  node: cek_data_v1.DataNodeV1,
) -> cek_data_v1.DataSequenceSummaryV1 {
  expect cek_data_v1.ListData { items_count, items_root, cbor_length, memory } = node
  cek_data_v1.DataSequenceSummaryV1 {
    root: items_root,
    length: items_count,
    payload_cbor_length: cbor_length - if items_count == 0 {
      1
    } else {
      2
    },
    memory: memory - 4,
  }
}

fn map_sequence_from_node_v1(
  node: cek_data_v1.DataNodeV1,
) -> cek_data_v1.DataSequenceSummaryV1 {
  expect  
      cek_data_v1.MapData { entries_count, entries_root, cbor_length, memory }
    = node
  let header_length = if entries_count < 24 {
    1
  } else if entries_count <= 255 {
    2
  } else if entries_count <= 65535 {
    3
  } else {
    5
  }
  cek_data_v1.DataSequenceSummaryV1 {
    root: entries_root,
    length: entries_count,
    payload_cbor_length: cbor_length - header_length,
    memory: memory - 4,
  }
}

fn exact_compute_successor(
  pre: MachineStateV1,
  mode: Int,
  focus_root: ByteArray,
  environment_root: ByteArray,
  continuation_root: ByteArray,
  auxiliary: Int,
) -> MachineStateV1 {
  exact_state(
    pre,
    mode,
    focus_root,
    environment_root,
    continuation_root,
    auxiliary,
    machine_step_cpu,
    machine_step_memory,
  )
}

fn error_successor(pre: MachineStateV1, reason: Int) -> MachineStateV1 {
  exact_state(
    pre,
    mode_halt_error,
    cek_proof_v1.hash_error_term_v1(),
    cek_proof_v1.empty_environment_root_v1,
    cek_proof_v1.empty_continuation_root_v1,
    reason,
    0,
    0,
  )
}

fn verify_compute_step(
  pre: MachineStateV1,
  post: MachineStateV1,
  witness: CoreStepWitnessV1,
) -> Bool {
  when witness is {
    ComputeVariable { index } -> and {
        pre.focus_root == cek_proof_v1.hash_variable_term_v1(index),
        post == exact_compute_successor(
          pre,
          mode_lookup,
          pre.environment_root,
          pre.environment_root,
          pre.continuation_root,
          index,
        ),
      }
    ComputeConstant { value } -> {
      let value_root = cek_constant_v1.constant_root_v1(value)
      and {
        pre.focus_root == cek_proof_v1.hash_constant_term_v1(value_root),
        post == exact_compute_successor(
          pre,
          mode_return,
          value_root,
          cek_proof_v1.empty_environment_root_v1,
          pre.continuation_root,
          0,
        ),
      }
    }
    ComputeContextConstant { value_root } -> and {
        pre.focus_root == cek_proof_v1.hash_context_constant_term_v1(value_root),
        post == exact_compute_successor(
          pre,
          mode_return,
          value_root,
          cek_proof_v1.empty_environment_root_v1,
          pre.continuation_root,
          0,
        ),
      }
    ComputeLambda { body } -> {
      let value_root = cek_proof_v1.hash_lambda_value_v1(
        body,
        pre.environment_root,
      )
      and {
        pre.focus_root == cek_proof_v1.hash_lambda_term_v1(body),
        post == exact_compute_successor(
          pre,
          mode_return,
          value_root,
          cek_proof_v1.empty_environment_root_v1,
          pre.continuation_root,
          0,
        ),
      }
    }
    ComputeDelay { body } -> {
      let value_root = cek_proof_v1.hash_delay_value_v1(
        body,
        pre.environment_root,
      )
      and {
        pre.focus_root == cek_proof_v1.hash_delay_term_v1(body),
        post == exact_compute_successor(
          pre,
          mode_return,
          value_root,
          cek_proof_v1.empty_environment_root_v1,
          pre.continuation_root,
          0,
        ),
      }
    }
    ComputeApplication { function, argument } -> {
      let continuation_root = cek_proof_v1.hash_apply_argument_continuation_v1(
        argument,
        pre.environment_root,
        pre.continuation_root,
      )
      and {
        pre.focus_root == cek_proof_v1.hash_application_term_v1(
          function,
          argument,
        ),
        post == exact_compute_successor(
          pre,
          mode_compute,
          function,
          pre.environment_root,
          continuation_root,
          0,
        ),
      }
    }
    ComputeForce { term } -> {
      let continuation_root = cek_proof_v1.hash_force_continuation_v1(
        pre.continuation_root,
      )
      and {
        pre.focus_root == cek_proof_v1.hash_force_term_v1(term),
        post == exact_compute_successor(
          pre,
          mode_compute,
          term,
          pre.environment_root,
          continuation_root,
          0,
        ),
      }
    }
    ComputeError -> and {
        pre.focus_root == cek_proof_v1.hash_error_term_v1(),
        post == error_successor(pre, error_explicit),
      }
    ComputeBuiltin { tag } -> {
      let value_root = cek_proof_v1.hash_builtin_value_v1(
        tag,
        builtin_force_count(tag),
        0,
        cek_proof_v1.empty_sequence_root_v1,
      )
      and {
        pre.focus_root == cek_proof_v1.hash_builtin_term_v1(tag),
        post == exact_compute_successor(
          pre,
          mode_return,
          value_root,
          cek_proof_v1.empty_environment_root_v1,
          pre.continuation_root,
          0,
        ),
      }
    }
    ComputeConstrEmpty { tag } -> {
      let value_root = cek_proof_v1.hash_constr_value_v1(
        tag,
        0,
        cek_proof_v1.empty_sequence_root_v1,
      )
      and {
        pre.focus_root == cek_proof_v1.hash_constr_term_v1(
          tag,
          0,
          cek_proof_v1.empty_sequence_root_v1,
        ),
        post == exact_compute_successor(
          pre,
          mode_return,
          value_root,
          cek_proof_v1.empty_environment_root_v1,
          pre.continuation_root,
          0,
        ),
      }
    }
    ComputeConstrNonEmpty { tag, terms_count, first_term, remaining_terms_root } -> {
      let terms_root = cek_proof_v1.hash_sequence_node_v1(
        first_term,
        remaining_terms_root,
        terms_count,
      )
      let continuation_root = cek_proof_v1.hash_constr_continuation_v1(
        tag,
        terms_count - 1,
        remaining_terms_root,
        0,
        cek_proof_v1.empty_sequence_root_v1,
        pre.environment_root,
        pre.continuation_root,
      )
      and {
        terms_count > 0,
        linked_sequence_tail_is_well_formed(remaining_terms_root, terms_count),
        pre.focus_root == cek_proof_v1.hash_constr_term_v1(
          tag,
          terms_count,
          terms_root,
        ),
        post == exact_compute_successor(
          pre,
          mode_compute,
          first_term,
          pre.environment_root,
          continuation_root,
          0,
        ),
      }
    }
    ComputeCase { scrutinee, branches_count, branches_root } -> {
      let continuation_root = cek_proof_v1.hash_case_continuation_v1(
        branches_count,
        branches_root,
        pre.environment_root,
        pre.continuation_root,
      )
      and {
        branches_count >= 0,
        linked_sequence_root_is_well_formed(branches_root, branches_count),
        pre.focus_root == cek_proof_v1.hash_case_term_v1(
          scrutinee,
          branches_count,
          branches_root,
        ),
        post == exact_compute_successor(
          pre,
          mode_compute,
          scrutinee,
          pre.environment_root,
          continuation_root,
          0,
        ),
      }
    }
    _ -> False
  }
}

fn verify_lookup_step(
  pre: MachineStateV1,
  post: MachineStateV1,
  witness: CoreStepWitnessV1,
) -> Bool {
  when witness is {
    LookupEnvironment { value, tail, length } ->
      if and {
        length > 0,
        pre.focus_root == cek_proof_v1.hash_environment_node_v1(
          value,
          tail,
          length,
        ),
        pre.environment_root == pre.focus_root,
        if length == 1 {
          tail == cek_proof_v1.empty_environment_root_v1
        } else {
          tail != cek_proof_v1.empty_environment_root_v1
        },
      } {
        if pre.auxiliary == 0 {
          post == exact_state(
            pre,
            mode_return,
            value,
            cek_proof_v1.empty_environment_root_v1,
            pre.continuation_root,
            0,
            0,
            0,
          )
        } else {
          post == exact_state(
            pre,
            mode_lookup,
            tail,
            tail,
            pre.continuation_root,
            pre.auxiliary - 1,
            0,
            0,
          )
        }
      } else {
        False
      }
    LookupEmptyEnvironment -> and {
        pre.focus_root == cek_proof_v1.empty_environment_root_v1,
        pre.environment_root == cek_proof_v1.empty_environment_root_v1,
        post == error_successor(pre, error_unbound_variable),
      }
    _ -> False
  }
}

fn verify_return_empty(
  pre: MachineStateV1,
  post: MachineStateV1,
  value: ValueWitnessV1,
) -> Bool {
  and {
    pre.continuation_root == cek_proof_v1.empty_continuation_root_v1,
    pre.focus_root == value_witness_hash(value),
    post == if value_is_constant(value) {
      exact_state(
        pre,
        mode_halt_success,
        pre.focus_root,
        cek_proof_v1.empty_environment_root_v1,
        cek_proof_v1.empty_continuation_root_v1,
        0,
        0,
        0,
      )
    } else {
      error_successor(pre, error_nonconstant_halt)
    },
  }
}

fn verify_return_apply_argument(
  pre: MachineStateV1,
  post: MachineStateV1,
  argument: ByteArray,
  captured_environment: ByteArray,
  tail: ByteArray,
) -> Bool {
  and {
    pre.continuation_root == cek_proof_v1.hash_apply_argument_continuation_v1(
      argument,
      captured_environment,
      tail,
    ),
    post == exact_state(
      pre,
      mode_compute,
      argument,
      captured_environment,
      cek_proof_v1.hash_apply_function_continuation_v1(pre.focus_root, tail),
      0,
      0,
      0,
    ),
  }
}

fn verify_return_apply_lambda(
  pre: MachineStateV1,
  post: MachineStateV1,
  body: ByteArray,
  closure_environment: ByteArray,
  closure_summary: EnvironmentSummaryV1,
  tail: ByteArray,
) -> Bool {
  let function_value = cek_proof_v1.hash_lambda_value_v1(
    body,
    closure_environment,
  )
  let next_environment = cek_proof_v1.hash_environment_node_v1(
    pre.focus_root,
    closure_environment,
    environment_summary_length(closure_summary) + 1,
  )
  and {
    environment_summary_matches(closure_environment, closure_summary),
    pre.continuation_root == cek_proof_v1.hash_apply_function_continuation_v1(
      function_value,
      tail,
    ),
    post == exact_state(
      pre,
      mode_compute,
      body,
      next_environment,
      tail,
      0,
      0,
      0,
    ),
  }
}

fn verify_return_apply_builtin(
  pre: MachineStateV1,
  post: MachineStateV1,
  tag: Int,
  forces_remaining: Int,
  arguments_count: Int,
  arguments_root: ByteArray,
  tail: ByteArray,
) -> Bool {
  let function_value = cek_proof_v1.hash_builtin_value_v1(
    tag,
    forces_remaining,
    arguments_count,
    arguments_root,
  )
  let next_arguments_count = arguments_count + 1
  let required_arguments = builtin_argument_count(tag)
  let next_arguments_root = cek_proof_v1.hash_sequence_node_v1(
    pre.focus_root,
    arguments_root,
    next_arguments_count,
  )
  let next_value = cek_proof_v1.hash_builtin_value_v1(
    tag,
    forces_remaining,
    next_arguments_count,
    next_arguments_root,
  )
  and {
    forces_remaining == 0,
    arguments_count >= 0,
    arguments_count < required_arguments,
    pre.continuation_root == cek_proof_v1.hash_apply_function_continuation_v1(
      function_value,
      tail,
    ),
    post == exact_state(
      pre,
      if next_arguments_count == required_arguments {
        mode_builtin
      } else {
        mode_return
      },
      next_value,
      cek_proof_v1.empty_environment_root_v1,
      tail,
      0,
      0,
      0,
    ),
  }
}

fn verify_return_apply_invalid(
  pre: MachineStateV1,
  post: MachineStateV1,
  function: ValueWitnessV1,
  tail: ByteArray,
) -> Bool {
  let function_value = value_witness_hash(function)
  and {
    !value_is_lambda_or_builtin(function),
    pre.continuation_root == cek_proof_v1.hash_apply_function_continuation_v1(
      function_value,
      tail,
    ),
    post == error_successor(pre, error_invalid_application),
  }
}

fn verify_return_apply_value_lambda(
  pre: MachineStateV1,
  post: MachineStateV1,
  argument: ByteArray,
  body: ByteArray,
  closure_environment: ByteArray,
  closure_summary: EnvironmentSummaryV1,
  tail: ByteArray,
) -> Bool {
  let function_value = cek_proof_v1.hash_lambda_value_v1(
    body,
    closure_environment,
  )
  let next_environment = cek_proof_v1.hash_environment_node_v1(
    argument,
    closure_environment,
    environment_summary_length(closure_summary) + 1,
  )
  and {
    environment_summary_matches(closure_environment, closure_summary),
    pre.focus_root == function_value,
    pre.continuation_root == cek_proof_v1.hash_apply_value_continuation_v1(
      argument,
      tail,
    ),
    post == exact_state(
      pre,
      mode_compute,
      body,
      next_environment,
      tail,
      0,
      0,
      0,
    ),
  }
}

fn verify_return_apply_value_builtin(
  pre: MachineStateV1,
  post: MachineStateV1,
  argument: ByteArray,
  tag: Int,
  forces_remaining: Int,
  arguments_count: Int,
  arguments_root: ByteArray,
  tail: ByteArray,
) -> Bool {
  let function_value = cek_proof_v1.hash_builtin_value_v1(
    tag,
    forces_remaining,
    arguments_count,
    arguments_root,
  )
  let next_arguments_count = arguments_count + 1
  let required_arguments = builtin_argument_count(tag)
  let next_arguments_root = cek_proof_v1.hash_sequence_node_v1(
    argument,
    arguments_root,
    next_arguments_count,
  )
  let next_value = cek_proof_v1.hash_builtin_value_v1(
    tag,
    forces_remaining,
    next_arguments_count,
    next_arguments_root,
  )
  and {
    forces_remaining == 0,
    arguments_count >= 0,
    arguments_count < required_arguments,
    pre.focus_root == function_value,
    pre.continuation_root == cek_proof_v1.hash_apply_value_continuation_v1(
      argument,
      tail,
    ),
    post == exact_state(
      pre,
      if next_arguments_count == required_arguments {
        mode_builtin
      } else {
        mode_return
      },
      next_value,
      cek_proof_v1.empty_environment_root_v1,
      tail,
      0,
      0,
      0,
    ),
  }
}

fn verify_return_apply_value_invalid(
  pre: MachineStateV1,
  post: MachineStateV1,
  argument: ByteArray,
  function: ValueWitnessV1,
  tail: ByteArray,
) -> Bool {
  and {
    !value_is_lambda_or_builtin(function),
    pre.focus_root == value_witness_hash(function),
    pre.continuation_root == cek_proof_v1.hash_apply_value_continuation_v1(
      argument,
      tail,
    ),
    post == error_successor(pre, error_invalid_application),
  }
}

fn verify_return_force_delay(
  pre: MachineStateV1,
  post: MachineStateV1,
  body: ByteArray,
  closure_environment: ByteArray,
  tail: ByteArray,
) -> Bool {
  and {
    pre.focus_root == cek_proof_v1.hash_delay_value_v1(
      body,
      closure_environment,
    ),
    pre.continuation_root == cek_proof_v1.hash_force_continuation_v1(tail),
    post == exact_state(
      pre,
      mode_compute,
      body,
      closure_environment,
      tail,
      0,
      0,
      0,
    ),
  }
}

fn verify_return_force_builtin(
  pre: MachineStateV1,
  post: MachineStateV1,
  tag: Int,
  forces_remaining: Int,
  arguments_count: Int,
  arguments_root: ByteArray,
  tail: ByteArray,
) -> Bool {
  let value = cek_proof_v1.hash_builtin_value_v1(
    tag,
    forces_remaining,
    arguments_count,
    arguments_root,
  )
  let next_value = cek_proof_v1.hash_builtin_value_v1(
    tag,
    forces_remaining - 1,
    arguments_count,
    arguments_root,
  )
  and {
    forces_remaining > 0,
    forces_remaining <= builtin_force_count(tag),
    pre.focus_root == value,
    pre.continuation_root == cek_proof_v1.hash_force_continuation_v1(tail),
    post == exact_state(
      pre,
      mode_return,
      next_value,
      cek_proof_v1.empty_environment_root_v1,
      tail,
      0,
      0,
      0,
    ),
  }
}

fn verify_return_force_invalid(
  pre: MachineStateV1,
  post: MachineStateV1,
  value: ValueWitnessV1,
  tail: ByteArray,
) -> Bool {
  and {
    !value_is_delay_or_forceable_builtin(value),
    pre.focus_root == value_witness_hash(value),
    pre.continuation_root == cek_proof_v1.hash_force_continuation_v1(tail),
    post == error_successor(pre, error_invalid_force),
  }
}

fn verify_return_constr_next(
  pre: MachineStateV1,
  post: MachineStateV1,
  tag: Int,
  remaining_terms_count: Int,
  next_term: ByteArray,
  remaining_terms_tail: ByteArray,
  values_count: Int,
  values_root: ByteArray,
  captured_environment: ByteArray,
  tail: ByteArray,
) -> Bool {
  let remaining_terms_root = cek_proof_v1.hash_sequence_node_v1(
    next_term,
    remaining_terms_tail,
    remaining_terms_count,
  )
  let next_values_count = values_count + 1
  let next_values_root = cek_proof_v1.hash_sequence_node_v1(
    pre.focus_root,
    values_root,
    next_values_count,
  )
  let next_continuation = cek_proof_v1.hash_constr_continuation_v1(
    tag,
    remaining_terms_count - 1,
    remaining_terms_tail,
    next_values_count,
    next_values_root,
    captured_environment,
    tail,
  )
  and {
    remaining_terms_count > 0,
    values_count >= 0,
    linked_sequence_root_is_well_formed(values_root, values_count),
    linked_sequence_tail_is_well_formed(
      remaining_terms_tail,
      remaining_terms_count,
    ),
    pre.continuation_root == cek_proof_v1.hash_constr_continuation_v1(
      tag,
      remaining_terms_count,
      remaining_terms_root,
      values_count,
      values_root,
      captured_environment,
      tail,
    ),
    post == exact_state(
      pre,
      mode_compute,
      next_term,
      captured_environment,
      next_continuation,
      0,
      0,
      0,
    ),
  }
}

fn verify_return_constr_done(
  pre: MachineStateV1,
  post: MachineStateV1,
  tag: Int,
  values_count: Int,
  values_root: ByteArray,
  captured_environment: ByteArray,
  tail: ByteArray,
) -> Bool {
  let next_values_count = values_count + 1
  let next_values_root = cek_proof_v1.hash_sequence_node_v1(
    pre.focus_root,
    values_root,
    next_values_count,
  )
  and {
    values_count >= 0,
    linked_sequence_root_is_well_formed(values_root, values_count),
    pre.continuation_root == cek_proof_v1.hash_constr_continuation_v1(
      tag,
      0,
      cek_proof_v1.empty_sequence_root_v1,
      values_count,
      values_root,
      captured_environment,
      tail,
    ),
    post == exact_state(
      pre,
      mode_return,
      cek_proof_v1.hash_constr_value_v1(
        tag,
        next_values_count,
        next_values_root,
      ),
      cek_proof_v1.empty_environment_root_v1,
      tail,
      0,
      0,
      0,
    ),
  }
}

fn verify_return_case_constr(
  pre: MachineStateV1,
  post: MachineStateV1,
  tag: Int,
  values_count: Int,
  values_root: ByteArray,
  branches_count: Int,
  branches_root: ByteArray,
  captured_environment: ByteArray,
  tail: ByteArray,
) -> Bool {
  and {
    values_count >= 0,
    branches_count >= 0,
    linked_sequence_root_is_well_formed(values_root, values_count),
    linked_sequence_root_is_well_formed(branches_root, branches_count),
    pre.focus_root == cek_proof_v1.hash_constr_value_v1(
      tag,
      values_count,
      values_root,
    ),
    pre.continuation_root == cek_proof_v1.hash_case_continuation_v1(
      branches_count,
      branches_root,
      captured_environment,
      tail,
    ),
    post == if and {
      tag >= 0,
      tag < branches_count,
    } {
      exact_state(
        pre,
        mode_case_select,
        branches_root,
        values_root,
        cek_proof_v1.hash_case_select_continuation_v1(
          captured_environment,
          tail,
          values_count,
        ),
        tag,
        0,
        0,
      )
    } else {
      error_successor(pre, error_case_branch_missing)
    },
  }
}

fn verify_return_case_invalid(
  pre: MachineStateV1,
  post: MachineStateV1,
  value: ValueWitnessV1,
  branches_count: Int,
  branches_root: ByteArray,
  captured_environment: ByteArray,
  tail: ByteArray,
) -> Bool {
  and {
    !value_is_constr(value),
    branches_count >= 0,
    linked_sequence_root_is_well_formed(branches_root, branches_count),
    pre.focus_root == value_witness_hash(value),
    pre.continuation_root == cek_proof_v1.hash_case_continuation_v1(
      branches_count,
      branches_root,
      captured_environment,
      tail,
    ),
    post == error_successor(pre, error_invalid_case_scrutinee),
  }
}

fn verify_return_step(
  pre: MachineStateV1,
  post: MachineStateV1,
  witness: CoreStepWitnessV1,
) -> Bool {
  when witness is {
    ReturnEmptyContinuation { value } -> verify_return_empty(pre, post, value)
    ReturnApplyArgument { argument, captured_environment, tail } ->
      verify_return_apply_argument(
        pre,
        post,
        argument,
        captured_environment,
        tail,
      )
    ReturnApplyLambda { body, closure_environment, closure_summary, tail } ->
      verify_return_apply_lambda(
        pre,
        post,
        body,
        closure_environment,
        closure_summary,
        tail,
      )
    ReturnApplyBuiltin {
      tag,
      forces_remaining,
      arguments_count,
      arguments_root,
      tail,
    } ->
      verify_return_apply_builtin(
        pre,
        post,
        tag,
        forces_remaining,
        arguments_count,
        arguments_root,
        tail,
      )
    ReturnApplyInvalid { function, tail } ->
      verify_return_apply_invalid(pre, post, function, tail)
    ReturnApplyValueLambda {
      argument,
      body,
      closure_environment,
      closure_summary,
      tail,
    } ->
      verify_return_apply_value_lambda(
        pre,
        post,
        argument,
        body,
        closure_environment,
        closure_summary,
        tail,
      )
    ReturnApplyValueBuiltin {
      argument,
      tag,
      forces_remaining,
      arguments_count,
      arguments_root,
      tail,
    } ->
      verify_return_apply_value_builtin(
        pre,
        post,
        argument,
        tag,
        forces_remaining,
        arguments_count,
        arguments_root,
        tail,
      )
    ReturnApplyValueInvalid { argument, function, tail } ->
      verify_return_apply_value_invalid(pre, post, argument, function, tail)
    ReturnForceDelay { body, closure_environment, tail } ->
      verify_return_force_delay(pre, post, body, closure_environment, tail)
    ReturnForceBuiltin {
      tag,
      forces_remaining,
      arguments_count,
      arguments_root,
      tail,
    } ->
      verify_return_force_builtin(
        pre,
        post,
        tag,
        forces_remaining,
        arguments_count,
        arguments_root,
        tail,
      )
    ReturnForceInvalid { value, tail } ->
      verify_return_force_invalid(pre, post, value, tail)
    ReturnConstrNext {
      tag,
      remaining_terms_count,
      next_term,
      remaining_terms_tail,
      values_count,
      values_root,
      captured_environment,
      tail,
    } ->
      verify_return_constr_next(
        pre,
        post,
        tag,
        remaining_terms_count,
        next_term,
        remaining_terms_tail,
        values_count,
        values_root,
        captured_environment,
        tail,
      )
    ReturnConstrDone {
      tag,
      values_count,
      values_root,
      captured_environment,
      tail,
    } ->
      verify_return_constr_done(
        pre,
        post,
        tag,
        values_count,
        values_root,
        captured_environment,
        tail,
      )
    ReturnCaseConstr {
      tag,
      values_count,
      values_root,
      branches_count,
      branches_root,
      captured_environment,
      tail,
    } ->
      verify_return_case_constr(
        pre,
        post,
        tag,
        values_count,
        values_root,
        branches_count,
        branches_root,
        captured_environment,
        tail,
      )
    ReturnCaseInvalid {
      value,
      branches_count,
      branches_root,
      captured_environment,
      tail,
    } ->
      verify_return_case_invalid(
        pre,
        post,
        value,
        branches_count,
        branches_root,
        captured_environment,
        tail,
      )
    _ -> False
  }
}

fn verify_case_select_step(
  pre: MachineStateV1,
  post: MachineStateV1,
  witness: CoreStepWitnessV1,
) -> Bool {
  when witness is {
    SelectCaseBranch {
      branch,
      remaining_branches_root,
      length,
      captured_environment,
      tail,
      values_count,
    } -> {
      let work_root = cek_proof_v1.hash_case_select_continuation_v1(
        captured_environment,
        tail,
        values_count,
      )
      and {
        length > 0,
        values_count >= 0,
        pre.auxiliary >= 0,
        pre.auxiliary < length,
        linked_sequence_tail_is_well_formed(remaining_branches_root, length),
        linked_sequence_root_is_well_formed(pre.environment_root, values_count),
        pre.focus_root == cek_proof_v1.hash_sequence_node_v1(
          branch,
          remaining_branches_root,
          length,
        ),
        pre.continuation_root == work_root,
        post == if pre.auxiliary > 0 {
          exact_state(
            pre,
            mode_case_select,
            remaining_branches_root,
            pre.environment_root,
            work_root,
            pre.auxiliary - 1,
            0,
            0,
          )
        } else if values_count == 0 {
          exact_state(
            pre,
            mode_compute,
            branch,
            captured_environment,
            tail,
            0,
            0,
            0,
          )
        } else {
          exact_state(
            pre,
            mode_case_apply,
            pre.environment_root,
            branch,
            cek_proof_v1.hash_case_apply_continuation_v1(
              captured_environment,
              tail,
            ),
            values_count,
            0,
            0,
          )
        },
      }
    }
    _ -> False
  }
}

fn verify_case_apply_step(
  pre: MachineStateV1,
  post: MachineStateV1,
  witness: CoreStepWitnessV1,
) -> Bool {
  when witness is {
    ApplyCaseValue {
      value,
      remaining_values_root,
      length,
      captured_environment,
      built_continuation,
    } -> {
      let next_continuation = cek_proof_v1.hash_apply_value_continuation_v1(
        value,
        built_continuation,
      )
      and {
        length > 0,
        pre.auxiliary == length,
        linked_sequence_tail_is_well_formed(remaining_values_root, length),
        pre.focus_root == cek_proof_v1.hash_sequence_node_v1(
          value,
          remaining_values_root,
          length,
        ),
        pre.continuation_root == cek_proof_v1.hash_case_apply_continuation_v1(
          captured_environment,
          built_continuation,
        ),
        post == if length == 1 {
          exact_state(
            pre,
            mode_compute,
            pre.environment_root,
            captured_environment,
            next_continuation,
            0,
            0,
            0,
          )
        } else {
          exact_state(
            pre,
            mode_case_apply,
            remaining_values_root,
            pre.environment_root,
            cek_proof_v1.hash_case_apply_continuation_v1(
              captured_environment,
              next_continuation,
            ),
            length - 1,
            0,
            0,
          )
        },
      }
    }
    _ -> False
  }
}

fn verify_builtin_direct_step(
  pre: MachineStateV1,
  post: MachineStateV1,
  tag: Int,
  arguments: List<cek_builtin_v1.ValueWitnessV1>,
  result: cek_builtin_v1.ValueWitnessV1,
) -> Bool {
  let Pair(arguments_root, arguments_count) = cek_builtin_v1.arguments_root_v1(
    arguments,
  )
  let builtin_root = cek_proof_v1.hash_builtin_value_v1(
    tag,
    0,
    arguments_count,
    arguments_root,
  )
  let budget = cek_builtin_v1.direct_builtin_budget_v1(tag, arguments)
  and {
    arguments_count == builtin_argument_count(tag),
    pre.focus_root == builtin_root,
    cek_builtin_v1.verify_direct_builtin_v1(
      tag,
      builtin_root,
      arguments,
      result,
    ),
    post == exact_state(
      pre,
      mode_return,
      cek_builtin_v1.result_root_v1(result),
      cek_proof_v1.empty_environment_root_v1,
      pre.continuation_root,
      0,
      budget.cpu,
      budget.memory,
    ),
  }
}

fn verify_builtin_map_conversion_start_v1(
  pre: MachineStateV1,
  post: MachineStateV1,
  tag: Int,
  arguments: List<cek_builtin_v1.ValueWitnessV1>,
  result: cek_builtin_v1.ValueWitnessV1,
  material: MapConversionStartWitnessV1,
) -> Bool {
  expect [source] = arguments
  expect or {
      tag == 38,
      tag == 43,
    }
  let Pair(arguments_root, arguments_count) = cek_builtin_v1.arguments_root_v1(
    arguments,
  )
  let builtin_root = cek_proof_v1.hash_builtin_value_v1(
    tag,
    0,
    arguments_count,
    arguments_root,
  )
  let source_payload = cek_builtin_v1.semantic_constant_payload_v1(source)
  let result_payload = cek_builtin_v1.semantic_constant_payload_v1(result)
  expect source_payload == data_summary_from_node_v1(material.source_node)
  expect result_payload == data_summary_from_node_v1(material.result_node)
  expect
    cek_data_v1.verify_data_node_v1(
      material.source_node,
      material.source_list,
      material.source_pairs,
    )
  expect
    cek_data_v1.verify_data_node_v1(
      material.result_node,
      material.result_list,
      material.result_pairs,
    )
  let Pair(source_sequence, destination_sequence) = if tag == 38 {
    expect
      cek_builtin_v1.semantic_constant_type_v1(source) == cek_constant_v1.ListConstant {
        element: cek_constant_v1.PairConstant {
          first: cek_constant_v1.DataConstant,
          second: cek_constant_v1.DataConstant,
        },
      }
    expect
      cek_builtin_v1.semantic_constant_type_v1(result) == cek_constant_v1.DataConstant
    let source_sequence = list_sequence_from_node_v1(material.source_node)
    let destination_sequence = map_sequence_from_node_v1(material.result_node)
    expect
      cek_builtin_v1.semantic_constant_memory_v1(source) == source_sequence.memory - source_sequence.length * 4
    expect
      cek_builtin_v1.semantic_constant_memory_v1(result) == destination_sequence.memory + 4
    Pair(source_sequence, destination_sequence)
  } else {
    expect
      cek_builtin_v1.semantic_constant_type_v1(source) == cek_constant_v1.DataConstant
    expect
      cek_builtin_v1.semantic_constant_type_v1(result) == cek_constant_v1.ListConstant {
        element: cek_constant_v1.PairConstant {
          first: cek_constant_v1.DataConstant,
          second: cek_constant_v1.DataConstant,
        },
      }
    let source_sequence = map_sequence_from_node_v1(material.source_node)
    let destination_sequence = list_sequence_from_node_v1(material.result_node)
    expect
      cek_builtin_v1.semantic_constant_memory_v1(source) == source_sequence.memory + 4
    expect
      cek_builtin_v1.semantic_constant_memory_v1(result) == destination_sequence.memory - destination_sequence.length * 4
    Pair(source_sequence, destination_sequence)
  }
  expect source_sequence.length == destination_sequence.length
  let budget = cek_builtin_v1.direct_builtin_budget_v1(tag, arguments)
  let control = MapConversionControlV1 {
    tag,
    result_root: cek_builtin_v1.result_root_v1(result),
    source_root: source_sequence.root,
    source_remaining: source_sequence.length,
    source_payload_cbor_length: source_sequence.payload_cbor_length,
    source_memory: source_sequence.memory,
    destination_root: destination_sequence.root,
    destination_remaining: destination_sequence.length,
    destination_payload_cbor_length: destination_sequence.payload_cbor_length,
    destination_memory: destination_sequence.memory,
    budget_cpu: budget.cpu,
    budget_memory: budget.memory,
  }
  and {
    arguments_count == builtin_argument_count(tag),
    pre.focus_root == builtin_root,
    post == exact_state(
      pre,
      mode_semantic_builtin,
      hash_map_conversion_control_v1(control),
      cek_proof_v1.empty_environment_root_v1,
      pre.continuation_root,
      0,
      0,
      0,
    ),
  }
}

fn verify_builtin_bls_final_step(
  pre: MachineStateV1,
  post: MachineStateV1,
  left_root: ByteArray,
  right_root: ByteArray,
  left: cek_builtin_v1.BlsExpressionWitnessV1,
  right: cek_builtin_v1.BlsExpressionWitnessV1,
  result: cek_builtin_v1.ValueWitnessV1,
) -> Bool {
  let budget = cek_builtin_v1.direct_builtin_budget_v1(
    70,
    [
      cek_builtin_v1.BlsMillerLoopValue(left_root),
      cek_builtin_v1.BlsMillerLoopValue(right_root),
    ],
  )
  and {
    cek_builtin_v1.verify_direct_bls_final_roots_v1(
      pre.focus_root,
      left_root,
      right_root,
      left,
      right,
      result,
    ),
    post == exact_state(
      pre,
      mode_return,
      cek_builtin_v1.result_root_v1(result),
      cek_proof_v1.empty_environment_root_v1,
      pre.continuation_root,
      0,
      budget.cpu,
      budget.memory,
    ),
  }
}

fn verify_builtin_semantic_step(
  pre: MachineStateV1,
  post: MachineStateV1,
  tag: Int,
  arguments: List<cek_builtin_v1.ValueWitnessV1>,
  result: cek_builtin_v1.ValueWitnessV1,
  material: cek_builtin_v1.SemanticBuiltinWitnessV1,
) -> Bool {
  let Pair(arguments_root, arguments_count) = cek_builtin_v1.arguments_root_v1(
    arguments,
  )
  let builtin_root = cek_proof_v1.hash_builtin_value_v1(
    tag,
    0,
    arguments_count,
    arguments_root,
  )
  let budget = cek_builtin_v1.direct_builtin_budget_v1(tag, arguments)
  and {
    arguments_count == builtin_argument_count(tag),
    pre.focus_root == builtin_root,
    cek_builtin_v1.verify_semantic_builtin_v1(
      tag,
      builtin_root,
      arguments,
      result,
      material,
    ),
    post == exact_state(
      pre,
      mode_return,
      cek_builtin_v1.result_root_v1(result),
      cek_proof_v1.empty_environment_root_v1,
      pre.continuation_root,
      0,
      budget.cpu,
      budget.memory,
    ),
  }
}

fn verify_builtin_failure_step(
  pre: MachineStateV1,
  post: MachineStateV1,
  tag: Int,
  arguments: List<cek_builtin_v1.ValueWitnessV1>,
) -> Bool {
  let Pair(arguments_root, arguments_count) = cek_builtin_v1.arguments_root_v1(
    arguments,
  )
  let builtin_root = cek_proof_v1.hash_builtin_value_v1(
    tag,
    0,
    arguments_count,
    arguments_root,
  )
  let budget = cek_builtin_v1.direct_builtin_failure_budget_v1(tag, arguments)
  and {
    pre.focus_root == builtin_root,
    cek_builtin_v1.verify_direct_builtin_failure_v1(
      tag,
      builtin_root,
      arguments,
    ),
    post == exact_state(
      pre,
      mode_halt_error,
      cek_proof_v1.hash_error_term_v1(),
      cek_proof_v1.empty_environment_root_v1,
      cek_proof_v1.empty_continuation_root_v1,
      error_builtin_failure,
      budget.cpu,
      budget.memory,
    ),
  }
}

fn verify_builtin_semantic_failure_step(
  pre: MachineStateV1,
  post: MachineStateV1,
  tag: Int,
  arguments: List<cek_builtin_v1.ValueWitnessV1>,
  material: cek_builtin_v1.SemanticBuiltinWitnessV1,
) -> Bool {
  let Pair(arguments_root, arguments_count) = cek_builtin_v1.arguments_root_v1(
    arguments,
  )
  let builtin_root = cek_proof_v1.hash_builtin_value_v1(
    tag,
    0,
    arguments_count,
    arguments_root,
  )
  and {
    arguments_count == builtin_argument_count(tag),
    pre.focus_root == builtin_root,
    cek_builtin_v1.verify_semantic_builtin_failure_v1(
      tag,
      builtin_root,
      arguments,
      material,
    ),
    post == exact_state(
      pre,
      mode_halt_error,
      cek_proof_v1.hash_error_term_v1(),
      cek_proof_v1.empty_environment_root_v1,
      cek_proof_v1.empty_continuation_root_v1,
      error_builtin_failure,
      0,
      0,
    ),
  }
}

fn verify_builtin_type_failure_step(
  pre: MachineStateV1,
  post: MachineStateV1,
  tag: Int,
  arguments: List<cek_builtin_v1.RuntimeValueWitnessV1>,
) -> Bool {
  let Pair(arguments_root, arguments_count) = cek_builtin_v1.runtime_arguments_root_v1(
    arguments,
  )
  let builtin_root = cek_proof_v1.hash_builtin_value_v1(
    tag,
    0,
    arguments_count,
    arguments_root,
  )
  and {
    pre.focus_root == builtin_root,
    cek_builtin_v1.verify_builtin_type_failure_v1(tag, builtin_root, arguments),
    post == exact_state(
      pre,
      mode_halt_error,
      cek_proof_v1.hash_error_term_v1(),
      cek_proof_v1.empty_environment_root_v1,
      cek_proof_v1.empty_continuation_root_v1,
      error_builtin_failure,
      0,
      0,
    ),
  }
}

fn pair_wrapper_is_exact_v1(
  pair: cek_data_v1.DataNodeV1,
  first: cek_data_v1.DataListNodeV1,
  second: cek_data_v1.DataListNodeV1,
  key: cek_data_v1.DataNodeV1,
  value: cek_data_v1.DataNodeV1,
) -> Bool {
  and {
    cek_data_v1.verify_data_node_v1(pair, Some(first), None),
    when pair is {
      cek_data_v1.ConstrSmallData { constructor, fields_count, .. } -> and {
          constructor == 0,
          fields_count == 2,
        }
      _ -> False
    },
    cek_data_v1.verify_data_list_link_v1(first, key, Some(second)),
    cek_data_v1.verify_data_list_link_v1(second, value, None),
  }
}

fn verify_builtin_list_to_map_step_v1(
  pre: MachineStateV1,
  post: MachineStateV1,
  control: MapConversionControlV1,
  source: cek_data_v1.DataListNodeV1,
  pair: cek_data_v1.DataNodeV1,
  first: cek_data_v1.DataListNodeV1,
  second: cek_data_v1.DataListNodeV1,
  key: cek_data_v1.DataNodeV1,
  value: cek_data_v1.DataNodeV1,
  destination: cek_data_v1.DataPairNodeV1,
) -> Bool {
  expect control.tag == 38
  expect control.source_remaining > 0
  let key_summary = data_summary_from_node_v1(key)
  let value_summary = data_summary_from_node_v1(value)
  let pair_summary = data_summary_from_node_v1(pair)
  let next_control = MapConversionControlV1 {
    ..control,
    source_root: source.tail,
    source_remaining: control.source_remaining - 1,
    source_payload_cbor_length: control.source_payload_cbor_length - source.head_cbor_length,
    source_memory: control.source_memory - source.head_memory,
    destination_root: destination.tail,
    destination_remaining: control.destination_remaining - 1,
    destination_payload_cbor_length: control.destination_payload_cbor_length - destination.key_cbor_length - destination.value_cbor_length,
    destination_memory: control.destination_memory - destination.key_memory - destination.value_memory,
  }
  and {
    pre.focus_root == hash_map_conversion_control_v1(control),
    cek_data_v1.hash_data_list_node_v1(source) == control.source_root,
    source.length == control.source_remaining,
    source.payload_cbor_length == control.source_payload_cbor_length,
    source.memory == control.source_memory,
    source.head == pair_summary.root,
    source.head_cbor_length == pair_summary.cbor_length,
    source.head_memory == pair_summary.memory,
    pair_wrapper_is_exact_v1(pair, first, second, key, value),
    cek_data_v1.hash_data_pair_node_v1(destination) == control.destination_root,
    destination.length == control.destination_remaining,
    destination.payload_cbor_length == control.destination_payload_cbor_length,
    destination.memory == control.destination_memory,
    destination.key == key_summary.root,
    destination.key_cbor_length == key_summary.cbor_length,
    destination.key_memory == key_summary.memory,
    destination.value == value_summary.root,
    destination.value_cbor_length == value_summary.cbor_length,
    destination.value_memory == value_summary.memory,
    post == exact_state(
      pre,
      mode_semantic_builtin,
      hash_map_conversion_control_v1(next_control),
      pre.environment_root,
      pre.continuation_root,
      0,
      0,
      0,
    ),
  }
}

fn verify_builtin_map_to_list_step_v1(
  pre: MachineStateV1,
  post: MachineStateV1,
  control: MapConversionControlV1,
  source: cek_data_v1.DataPairNodeV1,
  destination: cek_data_v1.DataListNodeV1,
  pair: cek_data_v1.DataNodeV1,
  first: cek_data_v1.DataListNodeV1,
  second: cek_data_v1.DataListNodeV1,
  key: cek_data_v1.DataNodeV1,
  value: cek_data_v1.DataNodeV1,
) -> Bool {
  expect control.tag == 43
  expect control.source_remaining > 0
  let key_summary = data_summary_from_node_v1(key)
  let value_summary = data_summary_from_node_v1(value)
  let pair_summary = data_summary_from_node_v1(pair)
  let next_control = MapConversionControlV1 {
    ..control,
    source_root: source.tail,
    source_remaining: control.source_remaining - 1,
    source_payload_cbor_length: control.source_payload_cbor_length - source.key_cbor_length - source.value_cbor_length,
    source_memory: control.source_memory - source.key_memory - source.value_memory,
    destination_root: destination.tail,
    destination_remaining: control.destination_remaining - 1,
    destination_payload_cbor_length: control.destination_payload_cbor_length - destination.head_cbor_length,
    destination_memory: control.destination_memory - destination.head_memory,
  }
  and {
    pre.focus_root == hash_map_conversion_control_v1(control),
    cek_data_v1.hash_data_pair_node_v1(source) == control.source_root,
    source.length == control.source_remaining,
    source.payload_cbor_length == control.source_payload_cbor_length,
    source.memory == control.source_memory,
    source.key == key_summary.root,
    source.key_cbor_length == key_summary.cbor_length,
    source.key_memory == key_summary.memory,
    source.value == value_summary.root,
    source.value_cbor_length == value_summary.cbor_length,
    source.value_memory == value_summary.memory,
    pair_wrapper_is_exact_v1(pair, first, second, key, value),
    cek_data_v1.hash_data_list_node_v1(destination) == control.destination_root,
    destination.length == control.destination_remaining,
    destination.payload_cbor_length == control.destination_payload_cbor_length,
    destination.memory == control.destination_memory,
    destination.head == pair_summary.root,
    destination.head_cbor_length == pair_summary.cbor_length,
    destination.head_memory == pair_summary.memory,
    post == exact_state(
      pre,
      mode_semantic_builtin,
      hash_map_conversion_control_v1(next_control),
      pre.environment_root,
      pre.continuation_root,
      0,
      0,
      0,
    ),
  }
}

fn verify_builtin_map_conversion_finish_v1(
  pre: MachineStateV1,
  post: MachineStateV1,
  control: MapConversionControlV1,
) -> Bool {
  and {
    map_conversion_control_is_well_formed_v1(control),
    control.source_remaining == 0,
    pre.focus_root == hash_map_conversion_control_v1(control),
    post == exact_state(
      pre,
      mode_return,
      control.result_root,
      cek_proof_v1.empty_environment_root_v1,
      pre.continuation_root,
      0,
      control.budget_cpu,
      control.budget_memory,
    ),
  }
}

fn verify_semantic_builtin_control_step_v1(
  pre: MachineStateV1,
  post: MachineStateV1,
  witness: CoreStepWitnessV1,
) -> Bool {
  when witness is {
    StepBuiltinListToMap {
      control,
      source,
      pair,
      first,
      second,
      key,
      value,
      destination,
    } ->
      verify_builtin_list_to_map_step_v1(
        pre,
        post,
        control,
        source,
        pair,
        first,
        second,
        key,
        value,
        destination,
      )
    StepBuiltinMapToList {
      control,
      source,
      destination,
      pair,
      first,
      second,
      key,
      value,
    } ->
      verify_builtin_map_to_list_step_v1(
        pre,
        post,
        control,
        source,
        destination,
        pair,
        first,
        second,
        key,
        value,
      )
    FinishBuiltinMapConversion { control } ->
      verify_builtin_map_conversion_finish_v1(pre, post, control)
    _ -> False
  }
}

fn verify_builtin_step(
  pre: MachineStateV1,
  post: MachineStateV1,
  witness: CoreStepWitnessV1,
) -> Bool {
  when witness is {
    ExecuteBuiltinDirect { tag, arguments, result } ->
      verify_builtin_direct_step(pre, post, tag, arguments, result)
    ExecuteBuiltinSemantic { tag, arguments, result, material } ->
      verify_builtin_semantic_step(pre, post, tag, arguments, result, material)
    StartBuiltinMapConversion { tag, arguments, result, material } ->
      verify_builtin_map_conversion_start_v1(
        pre,
        post,
        tag,
        arguments,
        result,
        material,
      )
    ExecuteBuiltinSemanticFailure { tag, arguments, material } ->
      verify_builtin_semantic_failure_step(pre, post, tag, arguments, material)
    ExecuteBuiltinBlsFinal { left_root, right_root, left, right, result } ->
      verify_builtin_bls_final_step(
        pre,
        post,
        left_root,
        right_root,
        left,
        right,
        result,
      )
    ExecuteBuiltinFailure { tag, arguments } ->
      verify_builtin_failure_step(pre, post, tag, arguments)
    ExecuteBuiltinTypeFailure { tag, arguments } ->
      verify_builtin_type_failure_step(pre, post, tag, arguments)
    _ -> False
  }
}

pub fn verify_core_step_v1(
  pre: MachineStateV1,
  post: MachineStateV1,
  witness: CoreStepWitnessV1,
) -> Bool {
  and {
    state_is_well_formed(pre),
    state_is_well_formed(post),
    pre.execution_index == post.execution_index,
    if pre.mode == mode_compute {
      verify_compute_step(pre, post, witness)
    } else if pre.mode == mode_lookup {
      verify_lookup_step(pre, post, witness)
    } else if pre.mode == mode_return {
      verify_return_step(pre, post, witness)
    } else if pre.mode == mode_case_select {
      verify_case_select_step(pre, post, witness)
    } else if pre.mode == mode_case_apply {
      verify_case_apply_step(pre, post, witness)
    } else if pre.mode == mode_builtin {
      verify_builtin_step(pre, post, witness)
    } else if pre.mode == mode_semantic_builtin {
      verify_semantic_builtin_control_step_v1(pre, post, witness)
    } else {
      False
    },
  }
}
