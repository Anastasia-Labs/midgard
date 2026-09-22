use aiken/builtin
use aiken/cbor
use midgard/cek_builtin_v1
use midgard/cek_constant_v1
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
