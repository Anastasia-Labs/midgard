use aiken/builtin
use aiken/cbor
use aiken/collection/list
use midgard/cek_builtin_v1
use midgard/cek_constant_v1
use midgard/cek_cost_v1
use midgard/cek_data_v1
use midgard/cek_machine_v1
use midgard/cek_proof_v1

const h_01 = #"0101010101010101010101010101010101010101010101010101010101010101"

const h_02 = #"0202020202020202020202020202020202020202020202020202020202020202"

const h_03 = #"0303030303030303030303030303030303030303030303030303030303030303"

test canonical_machine_mode_and_error_tags_match_typescript() {
  and {
    [
      cek_machine_v1.mode_compute,
      cek_machine_v1.mode_return,
      cek_machine_v1.mode_lookup,
      cek_machine_v1.mode_builtin,
      cek_machine_v1.mode_halt_success,
      cek_machine_v1.mode_halt_error,
      cek_machine_v1.mode_case_select,
      cek_machine_v1.mode_case_apply,
      cek_machine_v1.mode_semantic_builtin,
    ] == [0, 1, 2, 3, 4, 5, 6, 7, 8],
    [
      cek_machine_v1.error_explicit,
      cek_machine_v1.error_unbound_variable,
      cek_machine_v1.error_invalid_application,
      cek_machine_v1.error_invalid_force,
      cek_machine_v1.error_nonconstant_halt,
      cek_machine_v1.error_invalid_case_scrutinee,
      cek_machine_v1.error_case_branch_missing,
      cek_machine_v1.error_builtin_failure,
    ] == [0, 1, 2, 3, 4, 5, 6, 7],
  }
}

test offchain_core_step_data_abi_vector() {
  let pre = cek_machine_v1.MachineStateV1 {
    mode: cek_machine_v1.mode_compute,
    execution_index: 2,
    focus_root: cek_proof_v1.hash_application_term_v1(h_01, h_02),
    environment_root: cek_proof_v1.empty_environment_root_v1,
    continuation_root: cek_proof_v1.empty_continuation_root_v1,
    auxiliary: 0,
    cpu: 10,
    memory: 11,
  }
  let post = cek_machine_v1.MachineStateV1 {
    mode: cek_machine_v1.mode_compute,
    execution_index: 2,
    focus_root: h_01,
    environment_root: cek_proof_v1.empty_environment_root_v1,
    continuation_root: h_03,
    auxiliary: 0,
    cpu: 16010,
    memory: 111,
  }
  cbor.serialise(
    cek_machine_v1.CoreStepEvidenceV1 {
      pre,
      post,
      witness: cek_machine_v1.ComputeApplication {
        function: h_01,
        argument: h_02,
      },
    },
  ) == #"d8799fd8799f000258202a37aa5b923cf90c6f3c8849e8fe2b28adcda97ccd736af6bf35b8312035f43158200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582053163c160dcec15695dabe0bccf6afc7f0e12db206392865db2feb0497ac838b000a0bffd8799f00025820010101010101010101010101010101010101010101010101010101010101010158200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a75820030303030303030303030303030303030303030303030303030303030303030300193e8a186fffd87d9f5820010101010101010101010101010101010101010101010101010101010101010158200202020202020202020202020202020202020202020202020202020202020202ffff"
}

fn state(
  mode: Int,
  focus_root: ByteArray,
  environment_root: ByteArray,
  continuation_root: ByteArray,
  auxiliary: Int,
  cpu: Int,
  memory: Int,
) -> cek_machine_v1.MachineStateV1 {
  cek_machine_v1.MachineStateV1 {
    mode,
    execution_index: 0,
    focus_root,
    environment_root,
    continuation_root,
    auxiliary,
    cpu,
    memory,
  }
}

test semantic_map_conversion_scans_one_pair_then_finishes() {
  let key_cbor = cbor.serialise(builtin.i_data(1))
  let key = cek_data_v1.integer_data_summary_v1(
    1,
    cek_proof_v1.hash_blob_chunk_v1(key_cbor),
  )
  let key_node = cek_data_v1.IntegerData {
    cbor_root: cek_proof_v1.hash_blob_chunk_v1(key_cbor),
    cbor_length: key.cbor_length,
    memory: key.memory,
  }
  let value = cek_data_v1.bytes_data_summary_v1(
    1,
    cek_proof_v1.hash_blob_chunk_v1(#"aa"),
  )
  let value_node = cek_data_v1.BytesData {
    bytes_root: cek_proof_v1.hash_blob_chunk_v1(#"aa"),
    bytes_length: 1,
    cbor_length: value.cbor_length,
    memory: value.memory,
  }
  let second = cek_data_v1.DataListNodeV1 {
    head: value.root,
    head_cbor_length: value.cbor_length,
    head_memory: value.memory,
    tail: cek_data_v1.empty_data_list_root_v1(),
    length: 1,
    payload_cbor_length: value.cbor_length,
    memory: value.memory,
  }
  let first = cek_data_v1.DataListNodeV1 {
    head: key.root,
    head_cbor_length: key.cbor_length,
    head_memory: key.memory,
    tail: cek_data_v1.hash_data_list_node_v1(second),
    length: 2,
    payload_cbor_length: key.cbor_length + value.cbor_length,
    memory: key.memory + value.memory,
  }
  let pair_sequence = cek_data_v1.prepend_data_list_summary_v1(
    key,
    cek_data_v1.prepend_data_list_summary_v1(
      value,
      cek_data_v1.empty_data_list_summary_v1(),
    ),
  )
  let pair_summary = cek_data_v1.small_constr_data_summary_v1(0, pair_sequence)
  let pair_node = cek_data_v1.ConstrSmallData {
    constructor: 0,
    fields_count: 2,
    fields_root: pair_sequence.root,
    cbor_length: pair_summary.cbor_length,
    memory: pair_summary.memory,
  }
  let source = cek_data_v1.DataListNodeV1 {
    head: pair_summary.root,
    head_cbor_length: pair_summary.cbor_length,
    head_memory: pair_summary.memory,
    tail: cek_data_v1.empty_data_list_root_v1(),
    length: 1,
    payload_cbor_length: pair_summary.cbor_length,
    memory: pair_summary.memory,
  }
  let destination = cek_data_v1.DataPairNodeV1 {
    key: key.root,
    key_cbor_length: key.cbor_length,
    key_memory: key.memory,
    value: value.root,
    value_cbor_length: value.cbor_length,
    value_memory: value.memory,
    tail: cek_data_v1.empty_data_pair_root_v1(),
    length: 1,
    payload_cbor_length: key.cbor_length + value.cbor_length,
    memory: key.memory + value.memory,
  }
  let control = cek_machine_v1.MapConversionControlV1 {
    tag: 38,
    result_root: h_03,
    source_root: cek_data_v1.hash_data_list_node_v1(source),
    source_remaining: 1,
    source_payload_cbor_length: source.payload_cbor_length,
    source_memory: source.memory,
    destination_root: cek_data_v1.hash_data_pair_node_v1(destination),
    destination_remaining: 1,
    destination_payload_cbor_length: destination.payload_cbor_length,
    destination_memory: destination.memory,
    budget_cpu: 111,
    budget_memory: 222,
  }
  let completed = cek_machine_v1.MapConversionControlV1 {
    ..control,
    source_root: cek_data_v1.empty_data_list_root_v1(),
    source_remaining: 0,
    source_payload_cbor_length: 0,
    source_memory: 0,
    destination_root: cek_data_v1.empty_data_pair_root_v1(),
    destination_remaining: 0,
    destination_payload_cbor_length: 0,
    destination_memory: 0,
  }
  let pre = state(
    cek_machine_v1.mode_semantic_builtin,
    cek_machine_v1.hash_map_conversion_control_v1(control),
    cek_proof_v1.empty_environment_root_v1,
    h_02,
    0,
    10,
    20,
  )
  let scanned = state(
    cek_machine_v1.mode_semantic_builtin,
    cek_machine_v1.hash_map_conversion_control_v1(completed),
    cek_proof_v1.empty_environment_root_v1,
    h_02,
    0,
    10,
    20,
  )
  let finished = state(
    cek_machine_v1.mode_return,
    h_03,
    cek_proof_v1.empty_environment_root_v1,
    h_02,
    0,
    121,
    242,
  )
  and {
    cek_machine_v1.verify_core_step_v1(
      pre,
      scanned,
      cek_machine_v1.StepBuiltinListToMap {
        control,
        source,
        pair: pair_node,
        first,
        second,
        key: key_node,
        value: value_node,
        destination,
      },
    ),
    cek_machine_v1.verify_core_step_v1(
      scanned,
      finished,
      cek_machine_v1.FinishBuiltinMapConversion { control: completed },
    ),
  }
}

fn integer(payload_cbor: ByteArray) -> cek_builtin_v1.ValueWitnessV1 {
  cek_builtin_v1.ConstantValue(
    cek_constant_v1.ConstantWitnessV1 { type_cbor: #"9f00ff", payload_cbor },
  )
}

fn boolean(payload_cbor: ByteArray) -> cek_builtin_v1.ValueWitnessV1 {
  cek_builtin_v1.ConstantValue(
    cek_constant_v1.ConstantWitnessV1 { type_cbor: #"9f04ff", payload_cbor },
  )
}

fn bytes(payload_cbor: ByteArray) -> cek_builtin_v1.ValueWitnessV1 {
  cek_builtin_v1.ConstantValue(
    cek_constant_v1.ConstantWitnessV1 { type_cbor: #"9f01ff", payload_cbor },
  )
}

fn g1_generator() -> cek_constant_v1.ConstantWitnessV1 {
  cek_constant_v1.ConstantWitnessV1 {
    type_cbor: #"9f09ff",
    payload_cbor: #"583097f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb",
  }
}

fn g2_generator() -> cek_constant_v1.ConstantWitnessV1 {
  cek_constant_v1.ConstantWitnessV1 {
    type_cbor: #"9f0aff",
    payload_cbor: #"5f584093e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc510515820c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8ff",
  }
}

fn miller_loop_generator_expression() -> cek_builtin_v1.BlsExpressionWitnessV1 {
  cek_builtin_v1.BlsMillerLoopExpression {
    g1: g1_generator(),
    g2: g2_generator(),
  }
}

fn double_miller_loop_expression() -> cek_builtin_v1.BlsExpressionWitnessV1 {
  cek_builtin_v1.BlsMultiplyExpression {
    left: miller_loop_generator_expression(),
    right: miller_loop_generator_expression(),
  }
}

fn triple_miller_loop_expression() -> cek_builtin_v1.BlsExpressionWitnessV1 {
  cek_builtin_v1.BlsMultiplyExpression {
    left: double_miller_loop_expression(),
    right: miller_loop_generator_expression(),
  }
}

fn five_miller_loop_expression() -> cek_builtin_v1.BlsExpressionWitnessV1 {
  cek_builtin_v1.BlsMultiplyExpression {
    left: triple_miller_loop_expression(),
    right: double_miller_loop_expression(),
  }
}

test application_and_lambda_reduction_are_exact() {
  let application = cek_proof_v1.hash_application_term_v1(
    cek_proof_v1.hash_lambda_term_v1(h_01),
    cek_proof_v1.hash_constant_term_v1(h_02),
  )
  let pre = state(
    cek_machine_v1.mode_compute,
    application,
    cek_proof_v1.empty_environment_root_v1,
    cek_proof_v1.empty_continuation_root_v1,
    0,
    100,
    100,
  )
  let apply_continuation = cek_proof_v1.hash_apply_argument_continuation_v1(
    cek_proof_v1.hash_constant_term_v1(h_02),
    cek_proof_v1.empty_environment_root_v1,
    cek_proof_v1.empty_continuation_root_v1,
  )
  let after_application = state(
    cek_machine_v1.mode_compute,
    cek_proof_v1.hash_lambda_term_v1(h_01),
    cek_proof_v1.empty_environment_root_v1,
    apply_continuation,
    0,
    16100,
    200,
  )
  let lambda_value = cek_proof_v1.hash_lambda_value_v1(
    h_01,
    cek_proof_v1.empty_environment_root_v1,
  )
  let after_lambda = state(
    cek_machine_v1.mode_return,
    lambda_value,
    cek_proof_v1.empty_environment_root_v1,
    apply_continuation,
    0,
    32100,
    300,
  )
  let function_continuation = cek_proof_v1.hash_apply_function_continuation_v1(
    lambda_value,
    cek_proof_v1.empty_continuation_root_v1,
  )
  let before_argument = state(
    cek_machine_v1.mode_compute,
    cek_proof_v1.hash_constant_term_v1(h_02),
    cek_proof_v1.empty_environment_root_v1,
    function_continuation,
    0,
    32100,
    300,
  )
  and {
    cek_machine_v1.verify_core_step_v1(
      pre,
      after_application,
      cek_machine_v1.ComputeApplication {
        function: cek_proof_v1.hash_lambda_term_v1(h_01),
        argument: cek_proof_v1.hash_constant_term_v1(h_02),
      },
    ),
    cek_machine_v1.verify_core_step_v1(
      after_application,
      after_lambda,
      cek_machine_v1.ComputeLambda { body: h_01 },
    ),
    cek_machine_v1.verify_core_step_v1(
      after_lambda,
      before_argument,
      cek_machine_v1.ReturnApplyArgument {
        argument: cek_proof_v1.hash_constant_term_v1(h_02),
        captured_environment: cek_proof_v1.empty_environment_root_v1,
        tail: cek_proof_v1.empty_continuation_root_v1,
      },
    ),
  }
}

test debruijn_lookup_walks_an_authenticated_environment() {
  let tail = cek_proof_v1.hash_environment_node_v1(
    h_01,
    cek_proof_v1.empty_environment_root_v1,
    1,
  )
  let environment = cek_proof_v1.hash_environment_node_v1(h_02, tail, 2)
  let pre = state(
    cek_machine_v1.mode_compute,
    cek_proof_v1.hash_variable_term_v1(1),
    environment,
    cek_proof_v1.empty_continuation_root_v1,
    0,
    100,
    100,
  )
  let lookup = state(
    cek_machine_v1.mode_lookup,
    environment,
    environment,
    cek_proof_v1.empty_continuation_root_v1,
    1,
    16100,
    200,
  )
  let lookup_tail = state(
    cek_machine_v1.mode_lookup,
    tail,
    tail,
    cek_proof_v1.empty_continuation_root_v1,
    0,
    16100,
    200,
  )
  let found = state(
    cek_machine_v1.mode_return,
    h_01,
    cek_proof_v1.empty_environment_root_v1,
    cek_proof_v1.empty_continuation_root_v1,
    0,
    16100,
    200,
  )
  and {
    cek_machine_v1.verify_core_step_v1(
      pre,
      lookup,
      cek_machine_v1.ComputeVariable { index: 1 },
    ),
    cek_machine_v1.verify_core_step_v1(
      lookup,
      lookup_tail,
      cek_machine_v1.LookupEnvironment { value: h_02, tail, length: 2 },
    ),
    cek_machine_v1.verify_core_step_v1(
      lookup_tail,
      found,
      cek_machine_v1.LookupEnvironment {
        value: h_01,
        tail: cek_proof_v1.empty_environment_root_v1,
        length: 1,
      },
    ),
  }
}

test builtin_force_and_application_enter_builtin_micro_machine() {
  let tag = 26
  let initial_value = cek_proof_v1.hash_builtin_value_v1(
    tag,
    1,
    0,
    cek_proof_v1.empty_sequence_root_v1,
  )
  let force_tail = cek_proof_v1.empty_continuation_root_v1
  let force_continuation = cek_proof_v1.hash_force_continuation_v1(force_tail)
  let pre_force = state(
    cek_machine_v1.mode_return,
    initial_value,
    cek_proof_v1.empty_environment_root_v1,
    force_continuation,
    0,
    16100,
    200,
  )
  let forced_value = cek_proof_v1.hash_builtin_value_v1(
    tag,
    0,
    0,
    cek_proof_v1.empty_sequence_root_v1,
  )
  let post_force = state(
    cek_machine_v1.mode_return,
    forced_value,
    cek_proof_v1.empty_environment_root_v1,
    force_tail,
    0,
    16100,
    200,
  )
  let function_continuation = cek_proof_v1.hash_apply_function_continuation_v1(
    cek_proof_v1.hash_builtin_value_v1(
      18,
      0,
      0,
      cek_proof_v1.empty_sequence_root_v1,
    ),
    force_tail,
  )
  let pre_apply = state(
    cek_machine_v1.mode_return,
    h_03,
    cek_proof_v1.empty_environment_root_v1,
    function_continuation,
    0,
    16100,
    200,
  )
  let arguments_root = cek_proof_v1.hash_sequence_node_v1(
    h_03,
    cek_proof_v1.empty_sequence_root_v1,
    1,
  )
  let post_apply = state(
    cek_machine_v1.mode_builtin,
    cek_proof_v1.hash_builtin_value_v1(18, 0, 1, arguments_root),
    cek_proof_v1.empty_environment_root_v1,
    force_tail,
    0,
    16100,
    200,
  )
  and {
    cek_machine_v1.verify_core_step_v1(
      pre_force,
      post_force,
      cek_machine_v1.ReturnForceBuiltin {
        tag,
        forces_remaining: 1,
        arguments_count: 0,
        arguments_root: cek_proof_v1.empty_sequence_root_v1,
        tail: force_tail,
      },
    ),
    cek_machine_v1.verify_core_step_v1(
      pre_apply,
      post_apply,
      cek_machine_v1.ReturnApplyBuiltin {
        tag: 18,
        forces_remaining: 0,
        arguments_count: 0,
        arguments_root: cek_proof_v1.empty_sequence_root_v1,
        tail: force_tail,
      },
    ),
  }
}

test direct_builtin_executes_with_exact_semantics_and_budget() {
  let arguments = [integer(#"01"), integer(#"1880")]
  let result = integer(#"1881")
  let Pair(arguments_root, arguments_count) = cek_builtin_v1.arguments_root_v1(
    arguments,
  )
  let builtin_root = cek_proof_v1.hash_builtin_value_v1(
    0,
    0,
    arguments_count,
    arguments_root,
  )
  let pre = state(
    cek_machine_v1.mode_builtin,
    builtin_root,
    cek_proof_v1.empty_environment_root_v1,
    cek_proof_v1.empty_continuation_root_v1,
    0,
    100,
    100,
  )
  let post = state(
    cek_machine_v1.mode_return,
    cek_builtin_v1.result_root_v1(result),
    cek_proof_v1.empty_environment_root_v1,
    cek_proof_v1.empty_continuation_root_v1,
    0,
    101728,
    103,
  )
  and {
    cek_machine_v1.verify_core_step_v1(
      pre,
      post,
      cek_machine_v1.ExecuteBuiltinDirect { tag: 0, arguments, result },
    ),
    !cek_machine_v1.verify_core_step_v1(
      pre,
      cek_machine_v1.MachineStateV1 { ..post, cpu: post.cpu - 1 },
      cek_machine_v1.ExecuteBuiltinDirect { tag: 0, arguments, result },
    ),
  }
}

test builtin_failure_halts_with_the_authenticated_failure_code() {
  let arguments = [integer(#"01"), integer(#"00")]
  let Pair(arguments_root, arguments_count) = cek_builtin_v1.arguments_root_v1(
    arguments,
  )
  let pre = state(
    cek_machine_v1.mode_builtin,
    cek_proof_v1.hash_builtin_value_v1(3, 0, arguments_count, arguments_root),
    cek_proof_v1.empty_environment_root_v1,
    h_01,
    0,
    100,
    100,
  )
  let post = state(
    cek_machine_v1.mode_halt_error,
    cek_proof_v1.hash_error_term_v1(),
    cek_proof_v1.empty_environment_root_v1,
    cek_proof_v1.empty_continuation_root_v1,
    cek_machine_v1.error_builtin_failure,
    100,
    100,
  )
  cek_machine_v1.verify_core_step_v1(
    pre,
    post,
    cek_machine_v1.ExecuteBuiltinFailure { tag: 3, arguments },
  )
}

test semantic_builtin_failure_halts_without_revealing_the_data_value() {
  let entries = cek_data_v1.empty_data_pair_summary_v1()
  let summary = cek_data_v1.map_data_summary_v1(entries)
  let node = cek_data_v1.MapData {
    entries_count: 0,
    entries_root: entries.root,
    cbor_length: summary.cbor_length,
    memory: summary.memory,
  }
  let arguments = [
    cek_builtin_v1.SemanticConstantValue {
      type_cbor: #"9f08ff",
      payload: summary,
      memory: summary.memory,
    },
  ]
  let Pair(arguments_root, arguments_count) = cek_builtin_v1.arguments_root_v1(
    arguments,
  )
  let pre = state(
    cek_machine_v1.mode_builtin,
    cek_proof_v1.hash_builtin_value_v1(42, 0, arguments_count, arguments_root),
    cek_proof_v1.empty_environment_root_v1,
    h_01,
    0,
    100,
    100,
  )
  let post = state(
    cek_machine_v1.mode_halt_error,
    cek_proof_v1.hash_error_term_v1(),
    cek_proof_v1.empty_environment_root_v1,
    cek_proof_v1.empty_continuation_root_v1,
    cek_machine_v1.error_builtin_failure,
    100,
    100,
  )
  cek_machine_v1.verify_core_step_v1(
    pre,
    post,
    cek_machine_v1.ExecuteBuiltinSemanticFailure {
      tag: 42,
      arguments,
      material: cek_builtin_v1.SemanticBuiltinWitnessV1 {
        data_nodes: [node],
        list_nodes: [],
        pair_nodes: [],
        scalar_preimages: [],
      },
    },
  )
}

test builtin_type_failure_halts_without_charging() {
  let arguments = [
    cek_builtin_v1.RuntimeLambdaValue {
      body: h_02,
      environment: cek_proof_v1.empty_environment_root_v1,
    },
    cek_builtin_v1.RuntimeConstantValue(
      cek_constant_v1.ConstantWitnessV1 {
        type_cbor: #"9f00ff",
        payload_cbor: #"01",
      },
    ),
  ]
  let Pair(arguments_root, arguments_count) = cek_builtin_v1.runtime_arguments_root_v1(
    arguments,
  )
  let pre = state(
    cek_machine_v1.mode_builtin,
    cek_proof_v1.hash_builtin_value_v1(0, 0, arguments_count, arguments_root),
    cek_proof_v1.empty_environment_root_v1,
    h_01,
    0,
    100,
    100,
  )
  let post = state(
    cek_machine_v1.mode_halt_error,
    cek_proof_v1.hash_error_term_v1(),
    cek_proof_v1.empty_environment_root_v1,
    cek_proof_v1.empty_continuation_root_v1,
    cek_machine_v1.error_builtin_failure,
    100,
    100,
  )
  cek_machine_v1.verify_core_step_v1(
    pre,
    post,
    cek_machine_v1.ExecuteBuiltinTypeFailure { tag: 0, arguments },
  )
}

test paid_secp_failure_halts_with_the_exact_builtin_budget() {
  let arguments = [
    bytes(
      #"5821020000000000000000000000000000000000000000000000000000000000000000",
    ),
    bytes(
      #"58200000000000000000000000000000000000000000000000000000000000000000",
    ),
    bytes(
      #"584000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
    ),
  ]
  let Pair(arguments_root, arguments_count) = cek_builtin_v1.arguments_root_v1(
    arguments,
  )
  let budget = cek_builtin_v1.direct_builtin_budget_v1(52, arguments)
  let pre = state(
    cek_machine_v1.mode_builtin,
    cek_proof_v1.hash_builtin_value_v1(52, 0, arguments_count, arguments_root),
    cek_proof_v1.empty_environment_root_v1,
    h_01,
    0,
    100,
    100,
  )
  let post = state(
    cek_machine_v1.mode_halt_error,
    cek_proof_v1.hash_error_term_v1(),
    cek_proof_v1.empty_environment_root_v1,
    cek_proof_v1.empty_continuation_root_v1,
    cek_machine_v1.error_builtin_failure,
    100 + budget.cpu,
    100 + budget.memory,
  )
  cek_machine_v1.verify_core_step_v1(
    pre,
    post,
    cek_machine_v1.ExecuteBuiltinFailure { tag: 52, arguments },
  )
}

test bls_final_builtin_executes_from_authenticated_expressions() {
  let g1 = g1_generator()
  let g2 = g2_generator()
  let expression = cek_builtin_v1.BlsMillerLoopExpression { g1, g2 }
  let expression_root = cek_proof_v1.hash_bls_miller_loop_expression_v1(
    cek_constant_v1.constant_root_v1(g1),
    cek_constant_v1.constant_root_v1(g2),
  )
  let arguments = [
    cek_builtin_v1.BlsMillerLoopValue(expression_root),
    cek_builtin_v1.BlsMillerLoopValue(expression_root),
  ]
  let Pair(arguments_root, arguments_count) = cek_builtin_v1.arguments_root_v1(
    arguments,
  )
  let pre = state(
    cek_machine_v1.mode_builtin,
    cek_proof_v1.hash_builtin_value_v1(70, 0, arguments_count, arguments_root),
    cek_proof_v1.empty_environment_root_v1,
    cek_proof_v1.empty_continuation_root_v1,
    0,
    100,
    100,
  )
  let result = boolean(#"d87a80")
  let post = state(
    cek_machine_v1.mode_return,
    cek_builtin_v1.result_root_v1(result),
    cek_proof_v1.empty_environment_root_v1,
    cek_proof_v1.empty_continuation_root_v1,
    0,
    333849814,
    101,
  )
  cek_machine_v1.verify_core_step_v1(
    pre,
    post,
    cek_machine_v1.ExecuteBuiltinBlsFinal {
      left_root: expression_root,
      right_root: expression_root,
      left: expression,
      right: expression,
      result,
    },
  )
}

test ten_leaf_bls_final_transition_fits_the_l1_execution_reserve() {
  let expression = five_miller_loop_expression()
  let leaf_root = cek_proof_v1.hash_bls_miller_loop_expression_v1(
    cek_constant_v1.constant_root_v1(g1_generator()),
    cek_constant_v1.constant_root_v1(g2_generator()),
  )
  let double_root = cek_proof_v1.hash_bls_multiply_expression_v1(
    leaf_root,
    leaf_root,
  )
  let triple_root = cek_proof_v1.hash_bls_multiply_expression_v1(
    double_root,
    leaf_root,
  )
  let expression_root = cek_proof_v1.hash_bls_multiply_expression_v1(
    triple_root,
    double_root,
  )
  let arguments = [
    cek_builtin_v1.BlsMillerLoopValue(expression_root),
    cek_builtin_v1.BlsMillerLoopValue(expression_root),
  ]
  let Pair(arguments_root, arguments_count) = cek_builtin_v1.arguments_root_v1(
    arguments,
  )
  let pre = state(
    cek_machine_v1.mode_builtin,
    cek_proof_v1.hash_builtin_value_v1(70, 0, arguments_count, arguments_root),
    cek_proof_v1.empty_environment_root_v1,
    cek_proof_v1.empty_continuation_root_v1,
    0,
    100,
    100,
  )
  let result = boolean(#"d87a80")
  let post = state(
    cek_machine_v1.mode_return,
    cek_builtin_v1.result_root_v1(result),
    cek_proof_v1.empty_environment_root_v1,
    cek_proof_v1.empty_continuation_root_v1,
    0,
    333849814,
    101,
  )
  cek_machine_v1.verify_core_step_v1(
    pre,
    post,
    cek_machine_v1.ExecuteBuiltinBlsFinal {
      left_root: expression_root,
      right_root: expression_root,
      left: expression,
      right: expression,
      result,
    },
  )
}

test nonconstant_empty_stack_halts_as_error() {
  let lambda_value = cek_proof_v1.hash_lambda_value_v1(
    h_01,
    cek_proof_v1.empty_environment_root_v1,
  )
  let pre = state(
    cek_machine_v1.mode_return,
    lambda_value,
    cek_proof_v1.empty_environment_root_v1,
    cek_proof_v1.empty_continuation_root_v1,
    0,
    16100,
    200,
  )
  let post = state(
    cek_machine_v1.mode_halt_error,
    cek_proof_v1.hash_error_term_v1(),
    cek_proof_v1.empty_environment_root_v1,
    cek_proof_v1.empty_continuation_root_v1,
    cek_machine_v1.error_nonconstant_halt,
    16100,
    200,
  )
  cek_machine_v1.verify_core_step_v1(
    pre,
    post,
    cek_machine_v1.ReturnEmptyContinuation {
      value: cek_machine_v1.LambdaValue {
        body: h_01,
        environment: cek_proof_v1.empty_environment_root_v1,
      },
    },
  )
}

test wrong_successor_budget_fails_closed() {
  let value = cek_constant_v1.ConstantWitnessV1 {
    type_cbor: #"9f00ff",
    payload_cbor: #"01",
  }
  let value_root = cek_constant_v1.constant_root_v1(value)
  let pre = state(
    cek_machine_v1.mode_compute,
    cek_proof_v1.hash_constant_term_v1(value_root),
    cek_proof_v1.empty_environment_root_v1,
    cek_proof_v1.empty_continuation_root_v1,
    0,
    100,
    100,
  )
  let wrong = state(
    cek_machine_v1.mode_return,
    value_root,
    cek_proof_v1.empty_environment_root_v1,
    cek_proof_v1.empty_continuation_root_v1,
    0,
    16099,
    200,
  )
  !cek_machine_v1.verify_core_step_v1(
    pre,
    wrong,
    cek_machine_v1.ComputeConstant { value },
  )
}

test source_and_runtime_context_constants_use_distinct_exact_steps() {
  let source_value = cek_constant_v1.ConstantWitnessV1 {
    type_cbor: #"9f00ff",
    payload_cbor: #"01",
  }
  let source_root = cek_constant_v1.constant_root_v1(source_value)
  let source_pre = state(
    cek_machine_v1.mode_compute,
    cek_proof_v1.hash_constant_term_v1(source_root),
    cek_proof_v1.empty_environment_root_v1,
    cek_proof_v1.empty_continuation_root_v1,
    0,
    0,
    0,
  )
  let source_post = state(
    cek_machine_v1.mode_return,
    source_root,
    cek_proof_v1.empty_environment_root_v1,
    cek_proof_v1.empty_continuation_root_v1,
    0,
    16000,
    100,
  )
  let context_pre = state(
    cek_machine_v1.mode_compute,
    cek_proof_v1.hash_context_constant_term_v1(h_01),
    cek_proof_v1.empty_environment_root_v1,
    cek_proof_v1.empty_continuation_root_v1,
    0,
    0,
    0,
  )
  let context_post = state(
    cek_machine_v1.mode_return,
    h_01,
    cek_proof_v1.empty_environment_root_v1,
    cek_proof_v1.empty_continuation_root_v1,
    0,
    16000,
    100,
  )
  and {
    cek_machine_v1.verify_core_step_v1(
      source_pre,
      source_post,
      cek_machine_v1.ComputeConstant { value: source_value },
    ),
    cek_machine_v1.verify_core_step_v1(
      context_pre,
      context_post,
      cek_machine_v1.ComputeContextConstant { value_root: h_01 },
    ),
    !cek_machine_v1.verify_core_step_v1(
      source_pre,
      source_post,
      cek_machine_v1.ComputeContextConstant { value_root: source_root },
    ),
  }
}

test constructor_fields_stream_into_a_reversed_authenticated_value_list() {
  let empty = cek_proof_v1.empty_sequence_root_v1
  let term_01 = cek_proof_v1.hash_constant_term_v1(h_01)
  let term_02 = cek_proof_v1.hash_constant_term_v1(h_02)
  let term_tail = cek_proof_v1.hash_sequence_node_v1(term_02, empty, 1)
  let terms_root = cek_proof_v1.hash_sequence_node_v1(term_01, term_tail, 2)
  let pre = state(
    cek_machine_v1.mode_compute,
    cek_proof_v1.hash_constr_term_v1(7, 2, terms_root),
    cek_proof_v1.empty_environment_root_v1,
    cek_proof_v1.empty_continuation_root_v1,
    0,
    100,
    100,
  )
  let first_frame = cek_proof_v1.hash_constr_continuation_v1(
    7,
    1,
    term_tail,
    0,
    empty,
    cek_proof_v1.empty_environment_root_v1,
    cek_proof_v1.empty_continuation_root_v1,
  )
  let computing_first = state(
    cek_machine_v1.mode_compute,
    term_01,
    cek_proof_v1.empty_environment_root_v1,
    first_frame,
    0,
    16100,
    200,
  )
  let first_values = cek_proof_v1.hash_sequence_node_v1(h_01, empty, 1)
  let final_frame = cek_proof_v1.hash_constr_continuation_v1(
    7,
    0,
    empty,
    1,
    first_values,
    cek_proof_v1.empty_environment_root_v1,
    cek_proof_v1.empty_continuation_root_v1,
  )
  let returned_first = state(
    cek_machine_v1.mode_return,
    h_01,
    cek_proof_v1.empty_environment_root_v1,
    first_frame,
    0,
    16100,
    200,
  )
  let computing_second = state(
    cek_machine_v1.mode_compute,
    term_02,
    cek_proof_v1.empty_environment_root_v1,
    final_frame,
    0,
    16100,
    200,
  )
  let returned_second = state(
    cek_machine_v1.mode_return,
    h_02,
    cek_proof_v1.empty_environment_root_v1,
    final_frame,
    0,
    16100,
    200,
  )
  let final_values = cek_proof_v1.hash_sequence_node_v1(h_02, first_values, 2)
  let returned_constr = state(
    cek_machine_v1.mode_return,
    cek_proof_v1.hash_constr_value_v1(7, 2, final_values),
    cek_proof_v1.empty_environment_root_v1,
    cek_proof_v1.empty_continuation_root_v1,
    0,
    16100,
    200,
  )
  and {
    cek_machine_v1.verify_core_step_v1(
      pre,
      computing_first,
      cek_machine_v1.ComputeConstrNonEmpty {
        tag: 7,
        terms_count: 2,
        first_term: term_01,
        remaining_terms_root: term_tail,
      },
    ),
    cek_machine_v1.verify_core_step_v1(
      returned_first,
      computing_second,
      cek_machine_v1.ReturnConstrNext {
        tag: 7,
        remaining_terms_count: 1,
        next_term: term_02,
        remaining_terms_tail: empty,
        values_count: 0,
        values_root: empty,
        captured_environment: cek_proof_v1.empty_environment_root_v1,
        tail: cek_proof_v1.empty_continuation_root_v1,
      },
    ),
    cek_machine_v1.verify_core_step_v1(
      returned_second,
      returned_constr,
      cek_machine_v1.ReturnConstrDone {
        tag: 7,
        values_count: 1,
        values_root: first_values,
        captured_environment: cek_proof_v1.empty_environment_root_v1,
        tail: cek_proof_v1.empty_continuation_root_v1,
      },
    ),
  }
}

test case_selection_and_constructor_argument_application_are_streamed() {
  let empty = cek_proof_v1.empty_sequence_root_v1
  let branch_tail = cek_proof_v1.hash_sequence_node_v1(h_02, empty, 1)
  let branches_root = cek_proof_v1.hash_sequence_node_v1(h_01, branch_tail, 2)
  // Constructor values are committed in reverse evaluation order.
  let value_tail = cek_proof_v1.hash_sequence_node_v1(h_01, empty, 1)
  let values_root = cek_proof_v1.hash_sequence_node_v1(h_02, value_tail, 2)
  let tail = cek_proof_v1.empty_continuation_root_v1
  let environment = cek_proof_v1.empty_environment_root_v1
  let case_frame = cek_proof_v1.hash_case_continuation_v1(
    2,
    branches_root,
    environment,
    tail,
  )
  let pre = state(
    cek_machine_v1.mode_return,
    cek_proof_v1.hash_constr_value_v1(1, 2, values_root),
    cek_proof_v1.empty_environment_root_v1,
    case_frame,
    0,
    16100,
    200,
  )
  let select_work = cek_proof_v1.hash_case_select_continuation_v1(
    environment,
    tail,
    2,
  )
  let selecting_first = state(
    cek_machine_v1.mode_case_select,
    branches_root,
    values_root,
    select_work,
    1,
    16100,
    200,
  )
  let selecting_second = state(
    cek_machine_v1.mode_case_select,
    branch_tail,
    values_root,
    select_work,
    0,
    16100,
    200,
  )
  let case_apply_work = cek_proof_v1.hash_case_apply_continuation_v1(
    environment,
    tail,
  )
  let applying_values = state(
    cek_machine_v1.mode_case_apply,
    values_root,
    h_02,
    case_apply_work,
    2,
    16100,
    200,
  )
  let applied_last_value = cek_proof_v1.hash_apply_value_continuation_v1(
    h_02,
    tail,
  )
  let applying_first_value = state(
    cek_machine_v1.mode_case_apply,
    value_tail,
    h_02,
    cek_proof_v1.hash_case_apply_continuation_v1(
      environment,
      applied_last_value,
    ),
    1,
    16100,
    200,
  )
  let final_continuation = cek_proof_v1.hash_apply_value_continuation_v1(
    h_01,
    applied_last_value,
  )
  let computing_branch = state(
    cek_machine_v1.mode_compute,
    h_02,
    environment,
    final_continuation,
    0,
    16100,
    200,
  )
  and {
    cek_machine_v1.verify_core_step_v1(
      pre,
      selecting_first,
      cek_machine_v1.ReturnCaseConstr {
        tag: 1,
        values_count: 2,
        values_root,
        branches_count: 2,
        branches_root,
        captured_environment: environment,
        tail,
      },
    ),
    cek_machine_v1.verify_core_step_v1(
      selecting_first,
      selecting_second,
      cek_machine_v1.SelectCaseBranch {
        branch: h_01,
        remaining_branches_root: branch_tail,
        length: 2,
        captured_environment: environment,
        tail,
        values_count: 2,
      },
    ),
    cek_machine_v1.verify_core_step_v1(
      selecting_second,
      applying_values,
      cek_machine_v1.SelectCaseBranch {
        branch: h_02,
        remaining_branches_root: empty,
        length: 1,
        captured_environment: environment,
        tail,
        values_count: 2,
      },
    ),
    cek_machine_v1.verify_core_step_v1(
      applying_values,
      applying_first_value,
      cek_machine_v1.ApplyCaseValue {
        value: h_02,
        remaining_values_root: value_tail,
        length: 2,
        captured_environment: environment,
        built_continuation: tail,
      },
    ),
    cek_machine_v1.verify_core_step_v1(
      applying_first_value,
      computing_branch,
      cek_machine_v1.ApplyCaseValue {
        value: h_01,
        remaining_values_root: empty,
        length: 1,
        captured_environment: environment,
        built_continuation: applied_last_value,
      },
    ),
  }
}

// Issue #521 regression pins. `cek_machine_v1.MachineValueWitnessV1` used to be
// named `ValueWitnessV1`, colliding with `cek_builtin_v1.ValueWitnessV1` in
// stock aiken v1.1.22's generated-decoder cache, which is keyed by the local
// type name only. The two acceptance sets are arity-disjoint at every shared
// constructor tag, so the shared decoder was wrong in both directions: it
// admitted malformed machine witnesses and rejected well-formed ones. This
// first test forces both decoders into one compiled program, which is the
// only shape in which `aiken check` can observe the collision at all.
test machine_and_builtin_value_witness_decoders_stay_distinct() {
  expect Some(machine_data) = cbor.deserialise(#"d87b9f41004111ff")
  expect machine_value: cek_machine_v1.MachineValueWitnessV1 = machine_data
  expect Some(builtin_data) = cbor.deserialise(#"d87b9f4100ff")
  expect builtin_value: cek_builtin_v1.ValueWitnessV1 = builtin_data
  and {
    machine_value == cek_machine_v1.DelayValue {
      body: #"00",
      environment: #"11",
    },
    builtin_value == cek_builtin_v1.OpaqueValue(#"00"),
  }
}

test machine_value_witness_accepts_a_well_formed_delay_value() {
  expect Some(data) = cbor.deserialise(#"d87b9f41004111ff")
  expect decoded: cek_machine_v1.MachineValueWitnessV1 = data
  decoded == cek_machine_v1.DelayValue { body: #"00", environment: #"11" }
}

test machine_value_witness_accepts_a_well_formed_constr_value() {
  expect Some(data) = cbor.deserialise(#"d87c9f010241aaff")
  expect decoded: cek_machine_v1.MachineValueWitnessV1 = data
  decoded == cek_machine_v1.ConstrValue {
    tag: 1,
    values_count: 2,
    values_root: #"aa",
  }
}

test machine_value_witness_rejects_a_one_field_delay_value() fail {
  expect Some(data) = cbor.deserialise(#"d87b9f4100ff")
  expect decoded: cek_machine_v1.MachineValueWitnessV1 = data
  cbor.serialise(decoded) == cbor.serialise(decoded)
}

test machine_value_witness_rejects_a_one_field_constr_value() fail {
  expect Some(data) = cbor.deserialise(#"d87c9f4100ff")
  expect decoded: cek_machine_v1.MachineValueWitnessV1 = data
  cbor.serialise(decoded) == cbor.serialise(decoded)
}

// ---------------------------------------------------------------------------
// One successor per builtin pre-state. Each case takes one mode_builtin
// pre-state and offers it to two arms; exactly one may accept.
// ---------------------------------------------------------------------------

// Map-conversion start steps emitted by the TypeScript trace builder (the
// map_round_trip_empty and map_round_trip_constant core-step goldens).

const empty_unmap_step_cbor =
  #"d8799fd8799f03005820db8d5a689fe906712a09ba0941da41a7cbcef8c12ad9e7ac3d9099b6dbc2cdff58200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a758206ea68031c2800dfd45b3732e9fd615f19c97b062659999d3ad6ba181af284ddb001a0001f400190320ffd8799f080058208fce9cebbf56200bd4ab72bc735b970257695a871e2c7bbd0bf582aa27dc23db58200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a758206ea68031c2800dfd45b3732e9fd615f19c97b062659999d3ad6ba181af284ddb001a0001f400190320ffd905199f182b9fd87a9f439f08ffd8799f5820ee74a59f1ed4e11ab8a5035f44d49fdb4b7a77e51fc7f9bb4013d73037afb0e00104ff04ffffd8799fd8799f469f05060808ff4180ffffd8799fd87b9f005820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba20104ffd87a80d87a80d87c9f0058208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa560104ffd87a80d87a80ffffff"

const empty_unmap_step_pre =
  #"666633bf962f455935c3feb6358cb23b51dba024dbb817b73920abb3a0eb4a77"

const empty_map_step_cbor =
  #"d8799fd8799f030058203dbaddfafb4c4475e45ba4c3f53a37be05bf4f4f4c06aa1f4f2ed6928f03467d58200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582053163c160dcec15695dabe0bccf6afc7f0e12db206392865db2feb0497ac838b001a0002542f190340ffd8799f080058206fffa27ed4081de72e24c0168dcc4da3b9c33c07e33bb8aefd704b16d8d939c958200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582053163c160dcec15695dabe0bccf6afc7f0e12db206392865db2feb0497ac838b001a0002542f190340ffd905199f18269fd8799fd8799f469f05060808ff4180ffffffd8799fd8799f439f08ff41a0ffffd8799fd87c9f0058208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa560104ffd87a80d87a80d87b9f005820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba20104ffd87a80d87a80ffffff"

const empty_map_step_pre =
  #"18ce67eb56a8d08bd9ebf5ead6c90b938a0f129ac35d67cf234a3da6c25106ff"

const narrow_unmap_step_cbor =
  #"d8799fd8799f03005820ad578ff37b0e0997b877836bce3bb95a585307f584ee4283515e74bfa1c11ec158200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a758206ea68031c2800dfd45b3732e9fd615f19c97b062659999d3ad6ba181af284ddb001a0001f400190320ffd8799f0800582016bcaac883d2e351622eaf95064d6225d2ba65ec2c9244c8216eae6123a3327658200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a758206ea68031c2800dfd45b3732e9fd615f19c97b062659999d3ad6ba181af284ddb001a0001f400190320ffd905199f182b9fd8799fd8799f439f08ff4fa2014a2a2a2a2a2a2a2a2a2a2a0203ffffffd8799fd8799f469f05060808ff58189fd8799f014a2a2a2a2a2a2a2a2a2a2affd8799f0203ffffffffd8799fd87b9f0258207f0b53a382ba4d740072bd848b43ba84da5a9378251525b023c19ffd2fc034ec0f1821ffd87a80d8799fd8799f58206102804ee42d85218a31c7dda323bccc1ad4b31a9f3f3880449bdfca06a307dc010558206a157bf5ce93d07c5a59dce62821eba629fb8e3575c5d8ad9a4ee13342b9af250b0e58205770570155d35aedd507d7c4daa9303465bbbf69b8e9a0b604a9c0ee6dcdab61020e181dffffd87c9f025820bd51d81d895064134d8646595d0d2fed86beec8533146d3bc0c74c7211c6f1e218181829ffd8799fd8799f582027b5c54b5b7982aa9796d5b64906da1b91aa41077e3f976625245f9dda2f1635101758208d0163902197d2399d06cd3a2ec5b0a22f6b5b3c67cff731eaa39663f9cc487802161825ffffd87a80ffffff"

const narrow_unmap_step_pre =
  #"9a07040d5c1117e72eb7205713aed14e80a2b2c041f15ccbf9fe6da620f4a6c4"

const narrow_map_step_cbor =
  #"d8799fd8799f0300582093bdef27562393576c7d08a738ea3f21bbb37a5e2541bdb304451c456c22b0d458200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582053163c160dcec15695dabe0bccf6afc7f0e12db206392865db2feb0497ac838b001a0002542f190340ffd8799f080058204df8c150dca87799f4c14e096b03eaf1d409287b51d3e04ea1fb11d6fa53fe1f58200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582053163c160dcec15695dabe0bccf6afc7f0e12db206392865db2feb0497ac838b001a0002542f190340ffd905199f18269fd8799fd8799f469f05060808ff58189fd8799f014a2a2a2a2a2a2a2a2a2a2affd8799f0203ffffffffffd8799fd8799f439f08ff4fa2014a2a2a2a2a2a2a2a2a2a2a0203ffffd8799fd87c9f025820bd51d81d895064134d8646595d0d2fed86beec8533146d3bc0c74c7211c6f1e218181829ffd8799fd8799f582027b5c54b5b7982aa9796d5b64906da1b91aa41077e3f976625245f9dda2f1635101758208d0163902197d2399d06cd3a2ec5b0a22f6b5b3c67cff731eaa39663f9cc487802161825ffffd87a80d87b9f0258207f0b53a382ba4d740072bd848b43ba84da5a9378251525b023c19ffd2fc034ec0f1821ffd87a80d8799fd8799f58206102804ee42d85218a31c7dda323bccc1ad4b31a9f3f3880449bdfca06a307dc010558206a157bf5ce93d07c5a59dce62821eba629fb8e3575c5d8ad9a4ee13342b9af250b0e58205770570155d35aedd507d7c4daa9303465bbbf69b8e9a0b604a9c0ee6dcdab61020e181dffffffffff"

const narrow_map_step_pre =
  #"eb3fc40544dd958605634539161030268cfcbce4cf7ad5fd24df6a2a1e5eff95"

fn decode_core_step(
  evidence_cbor: ByteArray,
) -> cek_machine_v1.CoreStepEvidenceV1 {
  expect Some(data) = cbor.deserialise(evidence_cbor)
  expect evidence: cek_machine_v1.CoreStepEvidenceV1 = data
  evidence
}

// A context-derived source carries its payload by root; the direct arm reads
// it inline, so the same committed value is presented with it revealed.
fn revealed_source(
  value: cek_builtin_v1.ValueWitnessV1,
  payload: Option<Data>,
) -> cek_builtin_v1.ValueWitnessV1 {
  when payload is {
    None -> value
    Some(payload) -> {
      expect cek_builtin_v1.SemanticConstantValue { type_cbor, .. } = value
      cek_builtin_v1.ConstantValue(
        cek_constant_v1.ConstantWitnessV1 {
          type_cbor,
          payload_cbor: cbor.serialise(payload),
        },
      )
    }
  }
}

// The map-conversion start is accepted and the direct arm, offering the
// conversion's own result at its own budget, is refused.
fn map_conversion_has_one_arm(
  evidence_cbor: ByteArray,
  pre_hash: ByteArray,
  expected_tag: Int,
  source_payload: Option<Data>,
) -> Bool {
  let evidence = decode_core_step(evidence_cbor)
  expect cek_machine_v1.StartBuiltinMapConversion { tag, arguments, result, .. }
    = evidence.witness
  expect [source] = arguments
  let direct_arguments = [revealed_source(source, source_payload)]
  let pre = evidence.pre
  let budget = cek_builtin_v1.direct_builtin_budget_v1(tag, direct_arguments)
  let direct_post = cek_machine_v1.exact_state(
    pre,
    cek_machine_v1.mode_return,
    cek_builtin_v1.result_root_v1(result),
    cek_proof_v1.empty_environment_root_v1,
    pre.continuation_root,
    0,
    budget.cpu,
    budget.memory,
  )
  and {
    tag == expected_tag,
    cek_machine_v1.hash_state_v1(pre) == pre_hash,
    cek_machine_v1.verify_core_step_v1(pre, evidence.post, evidence.witness),
    cek_builtin_v1.arguments_root_v1(direct_arguments) == cek_builtin_v1.arguments_root_v1(
      arguments,
    ),
    direct_post != evidence.post,
    !cek_machine_v1.verify_core_step_v1(
      pre,
      direct_post,
      cek_machine_v1.ExecuteBuiltinDirect {
        tag,
        arguments: direct_arguments,
        result,
      },
    ),
    !cek_builtin_v1.verify_direct_structured_semantics(
      tag,
      direct_arguments,
      result,
    ),
  }
}

test empty_unmap_data_has_only_the_map_conversion_arm() {
  map_conversion_has_one_arm(
    empty_unmap_step_cbor,
    empty_unmap_step_pre,
    43,
    Some(builtin.map_data([])),
  )
}

test empty_map_data_has_only_the_map_conversion_arm() {
  map_conversion_has_one_arm(empty_map_step_cbor, empty_map_step_pre, 38, None)
}

test narrow_unmap_data_has_only_the_map_conversion_arm() {
  map_conversion_has_one_arm(
    narrow_unmap_step_cbor,
    narrow_unmap_step_pre,
    43,
    None,
  )
}

test narrow_map_data_has_only_the_map_conversion_arm() {
  map_conversion_has_one_arm(
    narrow_map_step_cbor,
    narrow_map_step_pre,
    38,
    None,
  )
}

fn integer_constant(value: Int) -> cek_constant_v1.ConstantWitnessV1 {
  cek_constant_v1.ConstantWitnessV1 {
    type_cbor: #"9f00ff",
    payload_cbor: cbor.serialise(builtin.i_data(value)),
  }
}

fn bytes_constant(value: ByteArray) -> cek_constant_v1.ConstantWitnessV1 {
  cek_constant_v1.ConstantWitnessV1 {
    type_cbor: #"9f01ff",
    payload_cbor: cbor.serialise(builtin.b_data(value)),
  }
}

fn repeated_byte(byte: Int, count: Int) -> ByteArray {
  if count == 0 {
    #""
  } else {
    builtin.cons_bytearray(byte, repeated_byte(byte, count - 1))
  }
}

fn builtin_pre(
  tag: Int,
  arguments: List<cek_constant_v1.ConstantWitnessV1>,
) -> cek_machine_v1.MachineStateV1 {
  let Pair(root, count) = cek_builtin_v1.runtime_arguments_root_v1(
    list.map(arguments, cek_builtin_v1.RuntimeConstantValue),
  )
  state(
    cek_machine_v1.mode_builtin,
    cek_proof_v1.hash_builtin_value_v1(tag, 0, count, root),
    cek_proof_v1.empty_environment_root_v1,
    h_03,
    0,
    10,
    11,
  )
}

fn builtin_failure_post(
  pre: cek_machine_v1.MachineStateV1,
  cpu: Int,
  memory: Int,
) -> cek_machine_v1.MachineStateV1 {
  cek_machine_v1.exact_state(
    pre,
    cek_machine_v1.mode_halt_error,
    cek_proof_v1.hash_error_term_v1(),
    cek_proof_v1.empty_environment_root_v1,
    cek_proof_v1.empty_continuation_root_v1,
    cek_machine_v1.error_builtin_failure,
    cpu,
    memory,
  )
}

fn type_failure_step(
  tag: Int,
  arguments: List<cek_constant_v1.ConstantWitnessV1>,
) -> Bool {
  let pre = builtin_pre(tag, arguments)
  cek_machine_v1.verify_core_step_v1(
    pre,
    builtin_failure_post(pre, 0, 0),
    cek_machine_v1.ExecuteBuiltinTypeFailure {
      tag,
      arguments: list.map(arguments, cek_builtin_v1.RuntimeConstantValue),
    },
  )
}

// The known-failure arm charges the builtin's own budget; it applies only when
// every argument has the kind the builtin reads.
fn known_failure_step(
  tag: Int,
  pre_arguments: List<cek_constant_v1.ConstantWitnessV1>,
  arguments: List<cek_builtin_v1.ValueWitnessV1>,
  budget: cek_cost_v1.BuiltinBudgetV1,
) -> Bool {
  let pre = builtin_pre(tag, pre_arguments)
  cek_machine_v1.verify_core_step_v1(
    pre,
    builtin_failure_post(pre, budget.cpu, budget.memory),
    cek_machine_v1.ExecuteBuiltinFailure { tag, arguments },
  )
}

fn charged_known_failure_step(
  tag: Int,
  arguments: List<cek_constant_v1.ConstantWitnessV1>,
) -> Bool {
  let direct = list.map(arguments, cek_builtin_v1.ConstantValue)
  known_failure_step(
    tag,
    arguments,
    direct,
    cek_builtin_v1.direct_builtin_budget_v1(tag, direct),
  )
}

fn bytes_dividend_by_zero() -> List<cek_constant_v1.ConstantWitnessV1> {
  [bytes_constant(#"aa"), integer_constant(0)]
}

fn integer_message_hash_to_group() -> List<cek_constant_v1.ConstantWitnessV1> {
  [integer_constant(7), bytes_constant(repeated_byte(7, 256))]
}

test ill_typed_division_by_zero_is_a_type_failure_step() {
  and {
    type_failure_step(4, bytes_dividend_by_zero()),
    type_failure_step(5, bytes_dividend_by_zero()),
    type_failure_step(6, bytes_dividend_by_zero()),
    type_failure_step(58, integer_message_hash_to_group()),
    type_failure_step(65, integer_message_hash_to_group()),
  }
}

test ill_typed_quotient_by_zero_is_not_a_known_failure_step() fail {
  charged_known_failure_step(4, bytes_dividend_by_zero())
}

test ill_typed_remainder_by_zero_is_not_a_known_failure_step() fail {
  charged_known_failure_step(5, bytes_dividend_by_zero())
}

test ill_typed_mod_by_zero_is_not_a_known_failure_step() fail {
  charged_known_failure_step(6, bytes_dividend_by_zero())
}

test ill_typed_g1_hash_to_group_is_not_a_known_failure_step() fail {
  charged_known_failure_step(58, integer_message_hash_to_group())
}

test ill_typed_g2_hash_to_group_is_not_a_known_failure_step() fail {
  charged_known_failure_step(65, integer_message_hash_to_group())
}

fn wide_message_hash_to_group() -> List<cek_constant_v1.ConstantWitnessV1> {
  [bytes_constant(repeated_byte(9, 64)), bytes_constant(repeated_byte(7, 256))]
}

// The budget of the same failure with the message sized as one word.
fn one_word_message_budget() -> cek_cost_v1.BuiltinBudgetV1 {
  let direct = list.map(
    wide_message_hash_to_group(),
    cek_builtin_v1.ConstantValue,
  )
  expect [_, dst_size] = cek_builtin_v1.builtin_cost_sizes_v1(58, direct)
  cek_cost_v1.builtin_budget_v1(58, [1, dst_size])
}

// The revealed message is wider than one word; that is the size the failure
// pays.
test revealed_hash_to_group_failure_pays_the_revealed_size() {
  let direct = list.map(
    wide_message_hash_to_group(),
    cek_builtin_v1.ConstantValue,
  )
  let charged = cek_builtin_v1.direct_builtin_budget_v1(58, direct)
  expect [message_size, _] = cek_builtin_v1.builtin_cost_sizes_v1(58, direct)
  and {
    message_size > 1,
    charged != one_word_message_budget(),
    known_failure_step(58, wide_message_hash_to_group(), direct, charged),
  }
}

// Presented opaquely, the same message has no size and so no budget.
test opaque_hash_to_group_failure_is_not_a_known_failure_step() fail {
  expect [message, dst] = wide_message_hash_to_group()
  known_failure_step(
    58,
    wide_message_hash_to_group(),
    [
      cek_builtin_v1.OpaqueValue(cek_constant_v1.constant_root_v1(message)),
      cek_builtin_v1.ConstantValue(dst),
    ],
    one_word_message_budget(),
  )
}

// mkCons of a well-typed item and list succeeds whichever presentation carries
// the committed values, so the type-failure arm refuses every presentation.
test mk_cons_of_well_typed_values_has_only_the_success_arm() {
  let item = integer_constant(1)
  let items = cek_constant_v1.ConstantWitnessV1 {
    type_cbor: #"9f0500ff",
    payload_cbor: #"80",
  }
  let result = cek_builtin_v1.ConstantValue(
    cek_constant_v1.ConstantWitnessV1 {
      type_cbor: #"9f0500ff",
      payload_cbor: cbor.serialise(builtin.list_data([builtin.i_data(1)])),
    },
  )
  let direct = [
    cek_builtin_v1.ConstantValue(item),
    cek_builtin_v1.ConstantValue(items),
  ]
  let pre = builtin_pre(32, [item, items])
  let budget = cek_builtin_v1.direct_builtin_budget_v1(32, direct)
  let success_post = cek_machine_v1.exact_state(
    pre,
    cek_machine_v1.mode_return,
    cek_builtin_v1.result_root_v1(result),
    cek_proof_v1.empty_environment_root_v1,
    pre.continuation_root,
    0,
    budget.cpu,
    budget.memory,
  )
  let Pair(item_payload, item_memory) = cek_constant_v1.constant_semantic_projection(
    item,
  )
  let Pair(items_payload, items_memory) = cek_constant_v1.constant_semantic_projection(
    items,
  )
  let semantic_item = [
    cek_builtin_v1.RuntimeSemanticConstantValue {
      type_cbor: item.type_cbor,
      payload: item_payload,
      memory: item_memory,
    },
    cek_builtin_v1.RuntimeConstantValue(items),
  ]
  let semantic_list = [
    cek_builtin_v1.RuntimeConstantValue(item),
    cek_builtin_v1.RuntimeSemanticConstantValue {
      type_cbor: items.type_cbor,
      payload: items_payload,
      memory: items_memory,
    },
  ]
  let type_failure_post = builtin_failure_post(pre, 0, 0)
  and {
    cek_machine_v1.verify_core_step_v1(
      pre,
      success_post,
      cek_machine_v1.ExecuteBuiltinDirect { tag: 32, arguments: direct, result },
    ),
    cek_builtin_v1.runtime_arguments_root_v1(semantic_item) == cek_builtin_v1.arguments_root_v1(
      direct,
    ),
    cek_builtin_v1.runtime_arguments_root_v1(semantic_list) == cek_builtin_v1.arguments_root_v1(
      direct,
    ),
    !cek_machine_v1.verify_core_step_v1(
      pre,
      type_failure_post,
      cek_machine_v1.ExecuteBuiltinTypeFailure {
        tag: 32,
        arguments: semantic_item,
      },
    ),
    !cek_machine_v1.verify_core_step_v1(
      pre,
      type_failure_post,
      cek_machine_v1.ExecuteBuiltinTypeFailure {
        tag: 32,
        arguments: semantic_list,
      },
    ),
  }
}

// Every newly recognized failure has exactly one authenticated halted state.
fn builtin_failure_is_exact(
  tag: Int,
  arguments: List<cek_constant_v1.ConstantWitnessV1>,
  cpu: Int,
  memory: Int,
) -> Bool {
  let pre = builtin_pre(tag, arguments)
  let direct = list.map(arguments, cek_builtin_v1.ConstantValue)
  let post = builtin_failure_post(pre, cpu, memory)
  let witness = cek_machine_v1.ExecuteBuiltinFailure { tag, arguments: direct }
  and {
    cek_machine_v1.verify_core_step_v1(pre, post, witness),
    !cek_machine_v1.verify_core_step_v1(
      pre,
      cek_machine_v1.MachineStateV1 { ..post, cpu: post.cpu + 1 },
      witness,
    ),
    !cek_machine_v1.verify_core_step_v1(
      pre,
      cek_machine_v1.MachineStateV1 { ..post, memory: post.memory + 1 },
      witness,
    ),
    !cek_machine_v1.verify_core_step_v1(
      pre,
      cek_machine_v1.MachineStateV1 {
        ..post,
        auxiliary: cek_machine_v1.error_explicit,
      },
      witness,
    ),
    !cek_machine_v1.verify_core_step_v1(
      pre,
      cek_machine_v1.MachineStateV1 { ..post, focus_root: h_02 },
      witness,
    ),
    !cek_machine_v1.verify_core_step_v1(
      pre,
      cek_machine_v1.MachineStateV1 { ..post, environment_root: h_02 },
      witness,
    ),
    !cek_machine_v1.verify_core_step_v1(
      pre,
      cek_machine_v1.MachineStateV1 { ..post, continuation_root: h_02 },
      witness,
    ),
    !cek_machine_v1.verify_core_step_v1(
      cek_machine_v1.MachineStateV1 { ..pre, focus_root: h_02 },
      post,
      witness,
    ),
  }
}

test int64_builtin_failures_halt_with_zero_charge_and_exact_state() {
  list.all(
    [9223372036854775808, -9223372036854775809],
    fn(amount) {
      and {
        builtin_failure_is_exact(
          12,
          [
            integer_constant(amount),
            integer_constant(1),
            bytes_constant(#"abcd"),
          ],
          0,
          0,
        ),
        builtin_failure_is_exact(
          12,
          [
            integer_constant(0),
            integer_constant(amount),
            bytes_constant(#"abcd"),
          ],
          0,
          0,
        ),
        builtin_failure_is_exact(
          82,
          [bytes_constant(#"abcd"), integer_constant(amount)],
          0,
          0,
        ),
        builtin_failure_is_exact(
          83,
          [bytes_constant(#"abcd"), integer_constant(amount)],
          0,
          0,
        ),
      }
    },
  )
}

test ecdsa_scalar_failures_halt_with_exact_paid_state() {
  let n = 115792089237316195423570985008687907852837564279074904382605163141518161494337
  list.all(
    [Pair(n, 1), Pair(1, n)],
    fn(scalars) {
      builtin_failure_is_exact(
        52,
        [
          bytes_constant(
            #"0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798",
          ),
          bytes_constant(
            #"0000000000000000000000000000000000000000000000000000000000000000",
          ),
          bytes_constant(
            builtin.append_bytearray(
              builtin.integer_to_bytearray(True, 32, scalars.1st),
              builtin.integer_to_bytearray(True, 32, scalars.2nd),
            ),
          ),
        ],
        43053543,
        10,
      )
    },
  )
}

test honest_int64_builtin_applications_reject_failure_successors() {
  list.all(
    [9223372036854775807, -9223372036854775808],
    fn(amount) {
      and {
        !known_failure_step(
          12,
          [
            integer_constant(amount),
            integer_constant(1),
            bytes_constant(#"abcd"),
          ],
          list.map(
            [
              integer_constant(amount),
              integer_constant(1),
              bytes_constant(#"abcd"),
            ],
            cek_builtin_v1.ConstantValue,
          ),
          cek_cost_v1.BuiltinBudgetV1 { cpu: 0, memory: 0 },
        ),
        !known_failure_step(
          82,
          [bytes_constant(#"abcd"), integer_constant(amount)],
          list.map(
            [bytes_constant(#"abcd"), integer_constant(amount)],
            cek_builtin_v1.ConstantValue,
          ),
          cek_cost_v1.BuiltinBudgetV1 { cpu: 0, memory: 0 },
        ),
        !known_failure_step(
          83,
          [bytes_constant(#"abcd"), integer_constant(amount)],
          list.map(
            [bytes_constant(#"abcd"), integer_constant(amount)],
            cek_builtin_v1.ConstantValue,
          ),
          cek_cost_v1.BuiltinBudgetV1 { cpu: 0, memory: 0 },
        ),
      }
    },
  )
}

test malformed_ed25519_lengths_have_one_paid_failure_successor() {
  let valid_key = #"d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a"
  let signature = builtin.integer_to_bytearray(True, 64, 0)
  let short_key_arguments = [
    bytes_constant(builtin.slice_bytearray(0, 31, valid_key)),
    bytes_constant(#""),
    bytes_constant(signature),
  ]
  let short_signature_arguments = [
    bytes_constant(valid_key),
    bytes_constant(#""),
    bytes_constant(builtin.slice_bytearray(0, 63, signature)),
  ]
  and {
    builtin_failure_is_exact(21, short_key_arguments, 53398444, 10),
    builtin_failure_is_exact(21, short_signature_arguments, 53398444, 10),
    !known_failure_step(
      21,
      short_key_arguments,
      list.map(short_key_arguments, cek_builtin_v1.ConstantValue),
      cek_cost_v1.BuiltinBudgetV1 { cpu: 0, memory: 0 },
    ),
    !known_failure_step(
      21,
      short_signature_arguments,
      list.map(short_signature_arguments, cek_builtin_v1.ConstantValue),
      cek_cost_v1.BuiltinBudgetV1 { cpu: 0, memory: 0 },
    ),
  }
}
