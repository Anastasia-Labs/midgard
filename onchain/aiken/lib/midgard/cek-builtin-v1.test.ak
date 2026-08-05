use aiken/builtin
use aiken/cbor
use midgard/cek_builtin_v1
use midgard/cek_constant_v1
use midgard/cek_cost_v1
use midgard/cek_data_scan_v1
use midgard/cek_data_v1
use midgard/cek_proof_v1

fn constant(
  type_cbor: ByteArray,
  payload_cbor: ByteArray,
) -> cek_builtin_v1.ValueWitnessV1 {
  cek_builtin_v1.ConstantValue(
    cek_constant_v1.ConstantWitnessV1 { type_cbor, payload_cbor },
  )
}

fn integer(payload_cbor: ByteArray) -> cek_builtin_v1.ValueWitnessV1 {
  constant(#"9f00ff", payload_cbor)
}

fn bytes(payload_cbor: ByteArray) -> cek_builtin_v1.ValueWitnessV1 {
  constant(#"9f01ff", payload_cbor)
}

fn boolean(payload_cbor: ByteArray) -> cek_builtin_v1.ValueWitnessV1 {
  constant(#"9f04ff", payload_cbor)
}

fn builtin_root(
  tag: Int,
  arguments: List<cek_builtin_v1.ValueWitnessV1>,
) -> ByteArray {
  let Pair(root, count) = cek_builtin_v1.arguments_root_v1(arguments)
  cek_proof_v1.hash_builtin_value_v1(tag, 0, count, root)
}

fn runtime_constant(
  type_cbor: ByteArray,
  payload_cbor: ByteArray,
) -> cek_builtin_v1.RuntimeValueWitnessV1 {
  cek_builtin_v1.RuntimeConstantValue(
    cek_constant_v1.ConstantWitnessV1 { type_cbor, payload_cbor },
  )
}

fn runtime_builtin_root(
  tag: Int,
  arguments: List<cek_builtin_v1.RuntimeValueWitnessV1>,
) -> ByteArray {
  let Pair(root, count) = cek_builtin_v1.runtime_arguments_root_v1(arguments)
  cek_proof_v1.hash_builtin_value_v1(tag, 0, count, root)
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

fn triple_miller_loop_expression() -> cek_builtin_v1.BlsExpressionWitnessV1 {
  cek_builtin_v1.BlsMultiplyExpression {
    left: cek_builtin_v1.BlsMultiplyExpression {
      left: miller_loop_generator_expression(),
      right: miller_loop_generator_expression(),
    },
    right: miller_loop_generator_expression(),
  }
}

fn double_miller_loop_expression() -> cek_builtin_v1.BlsExpressionWitnessV1 {
  cek_builtin_v1.BlsMultiplyExpression {
    left: miller_loop_generator_expression(),
    right: miller_loop_generator_expression(),
  }
}

fn five_miller_loop_expression() -> cek_builtin_v1.BlsExpressionWitnessV1 {
  cek_builtin_v1.BlsMultiplyExpression {
    left: triple_miller_loop_expression(),
    right: double_miller_loop_expression(),
  }
}

fn six_miller_loop_expression() -> cek_builtin_v1.BlsExpressionWitnessV1 {
  cek_builtin_v1.BlsMultiplyExpression {
    left: triple_miller_loop_expression(),
    right: triple_miller_loop_expression(),
  }
}

test add_integer_direct_step_is_authenticated() {
  let arguments = [integer(#"1829"), integer(#"01")]
  cek_builtin_v1.verify_direct_builtin_v1(
    0,
    builtin_root(0, arguments),
    arguments,
    integer(#"182a"),
  )
}

test add_integer_direct_budget_matches_reference_cek() {
  let arguments = [integer(#"01"), integer(#"1880")]
  and {
    cek_builtin_v1.verify_direct_builtin_v1(
      0,
      builtin_root(0, arguments),
      arguments,
      integer(#"1881"),
    ),
    cek_builtin_v1.direct_builtin_budget_v1(0, arguments) == cek_cost_v1.BuiltinBudgetV1 {
      cpu: 101628,
      memory: 3,
    },
  }
}

test empty_trace_message_uses_the_reference_zero_size_quirk() {
  let arguments = [constant(#"9f02ff", #"40"), integer(#"01")]
  and {
    cek_builtin_v1.builtin_cost_sizes_v1(28, arguments) == [0, 1],
    cek_builtin_v1.direct_builtin_budget_v1(28, arguments) == cek_cost_v1.BuiltinBudgetV1 {
      cpu: 59498,
      memory: 32,
    },
  }
}

test division_by_zero_failure_is_authenticated_without_charging() {
  let arguments = [integer(#"01"), integer(#"00")]
  and {
    cek_builtin_v1.verify_direct_builtin_failure_v1(
      3,
      builtin_root(3, arguments),
      arguments,
    ),
    cek_builtin_v1.direct_builtin_failure_budget_v1(3, arguments) == cek_cost_v1.BuiltinBudgetV1 {
      cpu: 0,
      memory: 0,
    },
  }
}

test quotient_by_zero_failure_charges_before_the_operation_fails() {
  let arguments = [integer(#"01"), integer(#"00")]
  and {
    cek_builtin_v1.verify_direct_builtin_failure_v1(
      4,
      builtin_root(4, arguments),
      arguments,
    ),
    cek_builtin_v1.direct_builtin_failure_budget_v1(4, arguments) == cek_builtin_v1.direct_builtin_budget_v1(
      4,
      arguments,
    ),
  }
}

test invalid_utf8_failure_is_authenticated_without_decoding() {
  let arguments = [bytes(#"42c0af")]
  and {
    !cek_builtin_v1.is_valid_utf8_v1(#"c0af"),
    cek_builtin_v1.is_valid_utf8_v1(#"f09f9880"),
    cek_builtin_v1.verify_direct_builtin_failure_v1(
      25,
      builtin_root(25, arguments),
      arguments,
    ),
  }
}

test integer_to_bytes_size_failure_charges_the_pinned_builtin_cost() {
  let arguments = [boolean(#"d87a80"), integer(#"01"), integer(#"190100")]
  and {
    cek_builtin_v1.verify_direct_builtin_failure_v1(
      73,
      builtin_root(73, arguments),
      arguments,
    ),
    cek_builtin_v1.direct_builtin_failure_budget_v1(73, arguments) == cek_cost_v1.BuiltinBudgetV1 {
      cpu: 1351512,
      memory: 1,
    },
  }
}

test malformed_ecdsa_public_key_failure_is_paid_and_authenticated() {
  let arguments = [
    bytes(
      #"58210479be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798",
    ),
    bytes(
      #"58200000000000000000000000000000000000000000000000000000000000000000",
    ),
    bytes(
      #"584000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
    ),
  ]
  and {
    cek_builtin_v1.verify_direct_builtin_failure_v1(
      52,
      builtin_root(52, arguments),
      arguments,
    ),
    cek_builtin_v1.direct_builtin_failure_budget_v1(52, arguments) == cek_builtin_v1.direct_builtin_budget_v1(
      52,
      arguments,
    ),
  }
}

test malformed_schnorr_x_coordinate_failure_is_paid_and_authenticated() {
  let arguments = [
    bytes(
      #"5820ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
    ),
    bytes(
      #"58200000000000000000000000000000000000000000000000000000000000000000",
    ),
    bytes(
      #"584000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
    ),
  ]
  and {
    cek_builtin_v1.verify_direct_builtin_failure_v1(
      53,
      builtin_root(53, arguments),
      arguments,
    ),
    cek_builtin_v1.direct_builtin_failure_budget_v1(53, arguments) == cek_builtin_v1.direct_builtin_budget_v1(
      53,
      arguments,
    ),
  }
}

test non_residue_ecdsa_x_coordinate_has_a_bounded_failure_proof() {
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
  cek_builtin_v1.verify_direct_builtin_failure_v1(
    52,
    builtin_root(52, arguments),
    arguments,
  )
}

test malformed_bls_g1_compression_header_failure_is_paid() {
  let arguments = [
    bytes(
      #"5830000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
    ),
  ]
  and {
    cek_builtin_v1.verify_direct_builtin_failure_v1(
      60,
      builtin_root(60, arguments),
      arguments,
    ),
    cek_builtin_v1.direct_builtin_failure_budget_v1(60, arguments) == cek_builtin_v1.direct_builtin_budget_v1(
      60,
      arguments,
    ),
  }
}

test malformed_bls_g2_compression_header_failure_is_paid() {
  let arguments = [
    bytes(
      #"5f58400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000058200000000000000000000000000000000000000000000000000000000000000000ff",
    ),
  ]
  and {
    cek_builtin_v1.verify_direct_builtin_failure_v1(
      67,
      builtin_root(67, arguments),
      arguments,
    ),
    cek_builtin_v1.direct_builtin_failure_budget_v1(67, arguments) == cek_builtin_v1.direct_builtin_budget_v1(
      67,
      arguments,
    ),
  }
}

test off_curve_bls_g1_encoding_has_a_bounded_paid_failure_proof() {
  let arguments = [
    bytes(
      #"5830800000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001",
    ),
  ]
  and {
    cek_builtin_v1.verify_direct_builtin_failure_v1(
      60,
      builtin_root(60, arguments),
      arguments,
    ),
    cek_builtin_v1.direct_builtin_failure_budget_v1(60, arguments) == cek_builtin_v1.direct_builtin_budget_v1(
      60,
      arguments,
    ),
  }
}

test canonical_bls_infinity_encoding_is_not_a_failure() fail {
  let arguments = [
    bytes(
      #"5830c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
    ),
  ]
  cek_builtin_v1.verify_direct_builtin_failure_v1(
    60,
    builtin_root(60, arguments),
    arguments,
  )
}

test valid_ecdsa_public_key_with_invalid_signature_returns_false() {
  let arguments = [
    bytes(
      #"58210279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798",
    ),
    bytes(
      #"58200000000000000000000000000000000000000000000000000000000000000000",
    ),
    bytes(
      #"584000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
    ),
  ]
  cek_builtin_v1.verify_direct_builtin_v1(
    52,
    builtin_root(52, arguments),
    arguments,
    boolean(#"d87980"),
  )
}

test builtin_closure_type_failure_is_authenticated_without_charging() {
  let arguments = [
    cek_builtin_v1.RuntimeLambdaValue {
      body: #"1111111111111111111111111111111111111111111111111111111111111111",
      environment: cek_proof_v1.empty_environment_root_v1,
    },
    runtime_constant(#"9f00ff", #"01"),
  ]
  cek_builtin_v1.verify_builtin_type_failure_v1(
    0,
    runtime_builtin_root(0, arguments),
    arguments,
  )
}

test mk_cons_rejects_an_incongruent_element_type() {
  let arguments = [
    runtime_constant(#"9f01ff", #"4101"),
    runtime_constant(#"9f0500ff", #"9f01ff"),
  ]
  cek_builtin_v1.verify_builtin_type_failure_v1(
    32,
    runtime_builtin_root(32, arguments),
    arguments,
  )
}

// V-9 (#532). This was `test ... fail` over a single call to
// `verify_builtin_type_failure_v1`, a three-conjunct production `and {}`, so it
// went green as soon as ANY conjunct was false and the name could not establish
// which guard fired. Single-conjunct mutation controls (each conjunct
// neutralized to `True` in turn, aiken-fork v1.1.23, this exact selector):
//
//   conjunct 1  arguments_count == builtin_argument_count_v1(tag)   -> pass
//   conjunct 2  builtin_value_root == hash_builtin_value_v1(...)    -> pass
//   conjunct 3  !builtin_arguments_are_well_typed_v1(tag, args)     -> FAIL
//
// Only conjunct 3 flips the test, so conjuncts 1 and 2 are TRUE here and the
// well-typedness guard is the sole falsifier. Each disposition is now pinned in
// its own non-`fail` conjunct below, and the ill-typed differential shows the
// same arity and the same authenticated argument root DO produce a type failure
// once the ifThenElse Boolean slot stops being a Boolean — which isolates
// conjunct 3 as the only thing this vector exercises.
test arbitrary_control_branch_values_are_not_type_failures() {
  let delay = cek_builtin_v1.RuntimeDelayValue {
    body: #"1111111111111111111111111111111111111111111111111111111111111111",
    environment: cek_proof_v1.empty_environment_root_v1,
  }
  let constr = cek_builtin_v1.RuntimeConstrValue {
    tag: 0,
    values_count: 0,
    values_root: cek_proof_v1.empty_sequence_root_v1,
  }
  let arguments = [runtime_constant(#"9f04ff", #"d87a80"), delay, constr]
  let builtin_value_root = runtime_builtin_root(26, arguments)
  let Pair(arguments_root, arguments_count) = cek_builtin_v1.runtime_arguments_root_v1(
    arguments,
  )
  // Same arity, same shape, but the ifThenElse Boolean slot carries an Integer.
  let ill_typed = [runtime_constant(#"9f00ff", #"00"), delay, constr]
  and {
    // Conjunct 1 (argument arity) holds.
    arguments_count == cek_cost_v1.builtin_argument_count_v1(26),
    // Conjunct 2 (authenticated argument root) holds.
    builtin_value_root == cek_proof_v1.hash_builtin_value_v1(
      26,
      0,
      arguments_count,
      arguments_root,
    ),
    // Conjunct 3 (`!well_typed`) is the sole falsifier: a delayed term and a
    // constructor value are legal ifThenElse branches, so this is not a type
    // failure.
    !cek_builtin_v1.verify_builtin_type_failure_v1(
      26,
      builtin_value_root,
      arguments,
    ),
    // Differential: only the branch-value typing changed, and the same
    // production call now authenticates a type failure.
    cek_builtin_v1.verify_builtin_type_failure_v1(
      26,
      runtime_builtin_root(26, ill_typed),
      ill_typed,
    ),
  }
}

test append_bytearray_direct_step_is_authenticated() {
  let arguments = [bytes(#"42aabb"), bytes(#"41cc")]
  cek_builtin_v1.verify_direct_builtin_v1(
    10,
    builtin_root(10, arguments),
    arguments,
    bytes(#"43aabbcc"),
  )
}

test if_then_else_selects_an_opaque_value_without_interpreting_it() {
  let selected = cek_builtin_v1.OpaqueValue(
    #"1111111111111111111111111111111111111111111111111111111111111111",
  )
  let rejected = cek_builtin_v1.OpaqueValue(
    #"2222222222222222222222222222222222222222222222222222222222222222",
  )
  let arguments = [boolean(#"d87a80"), selected, rejected]
  cek_builtin_v1.verify_direct_builtin_v1(
    26,
    builtin_root(26, arguments),
    arguments,
    selected,
  )
}

test list_head_preserves_the_authenticated_element_type() {
  let source = constant(#"9f0500ff", #"9f0102ff")
  let arguments = [source]
  cek_builtin_v1.verify_direct_builtin_v1(
    33,
    builtin_root(33, arguments),
    arguments,
    integer(#"01"),
  )
}

test i_data_and_un_i_data_round_trip_semantically() {
  let to_data_arguments = [integer(#"182a")]
  let data_value = constant(#"9f08ff", #"182a")
  and {
    cek_builtin_v1.verify_direct_builtin_v1(
      40,
      builtin_root(40, to_data_arguments),
      to_data_arguments,
      data_value,
    ),
    cek_builtin_v1.verify_direct_builtin_v1(
      45,
      builtin_root(45, [data_value]),
      [data_value],
      integer(#"182a"),
    ),
  }
}

test miller_loop_expression_and_final_verify_are_authenticated() {
  let g1 = g1_generator()
  let g2 = g2_generator()
  let expression_root = cek_proof_v1.hash_bls_miller_loop_expression_v1(
    cek_constant_v1.constant_root_v1(g1),
    cek_constant_v1.constant_root_v1(g2),
  )
  let arguments = [
    cek_builtin_v1.BlsMillerLoopValue(expression_root),
    cek_builtin_v1.BlsMillerLoopValue(expression_root),
  ]
  and {
    cek_builtin_v1.verify_direct_builtin_v1(
      68,
      builtin_root(
        68,
        [cek_builtin_v1.ConstantValue(g1), cek_builtin_v1.ConstantValue(g2)],
      ),
      [cek_builtin_v1.ConstantValue(g1), cek_builtin_v1.ConstantValue(g2)],
      cek_builtin_v1.BlsMillerLoopValue(expression_root),
    ),
    cek_builtin_v1.verify_direct_bls_final_v1(
      builtin_root(70, arguments),
      cek_builtin_v1.BlsMillerLoopExpression { g1, g2 },
      cek_builtin_v1.BlsMillerLoopExpression { g1, g2 },
      boolean(#"d87a80"),
    ),
  }
}

test six_leaf_miller_loop_proof_fits_the_l1_execution_reserve() {
  let expression = triple_miller_loop_expression()
  let leaf_root = cek_proof_v1.hash_bls_miller_loop_expression_v1(
    cek_constant_v1.constant_root_v1(g1_generator()),
    cek_constant_v1.constant_root_v1(g2_generator()),
  )
  let expression_root = cek_proof_v1.hash_bls_multiply_expression_v1(
    cek_proof_v1.hash_bls_multiply_expression_v1(leaf_root, leaf_root),
    leaf_root,
  )
  let arguments = [
    cek_builtin_v1.BlsMillerLoopValue(expression_root),
    cek_builtin_v1.BlsMillerLoopValue(expression_root),
  ]
  cek_builtin_v1.verify_direct_bls_final_v1(
    builtin_root(70, arguments),
    expression,
    expression,
    boolean(#"d87a80"),
  )
}

test ten_leaf_miller_loop_proof_fits_the_l1_execution_reserve() {
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
  cek_builtin_v1.verify_direct_bls_final_v1(
    builtin_root(70, arguments),
    expression,
    expression,
    boolean(#"d87a80"),
  )
}

test ten_leaf_root_bound_final_proof_fits_the_l1_execution_reserve() {
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
  cek_builtin_v1.verify_direct_bls_final_roots_v1(
    builtin_root(70, arguments),
    expression_root,
    expression_root,
    expression,
    expression,
    boolean(#"d87a80"),
  )
}

test twelve_leaf_miller_loop_proof_exceeding_the_reserve_fails_closed() fail {
  let expression = six_miller_loop_expression()
  let leaf_root = cek_proof_v1.hash_bls_miller_loop_expression_v1(
    cek_constant_v1.constant_root_v1(g1_generator()),
    cek_constant_v1.constant_root_v1(g2_generator()),
  )
  let triple_root = cek_proof_v1.hash_bls_multiply_expression_v1(
    cek_proof_v1.hash_bls_multiply_expression_v1(leaf_root, leaf_root),
    leaf_root,
  )
  let expression_root = cek_proof_v1.hash_bls_multiply_expression_v1(
    triple_root,
    triple_root,
  )
  let arguments = [
    cek_builtin_v1.BlsMillerLoopValue(expression_root),
    cek_builtin_v1.BlsMillerLoopValue(expression_root),
  ]
  cek_builtin_v1.verify_direct_bls_final_v1(
    builtin_root(70, arguments),
    expression,
    expression,
    boolean(#"d87a80"),
  )
}

test unsupported_direct_builtin_stays_closed() fail {
  cek_builtin_v1.verify_direct_builtin_v1(
    70,
    builtin_root(70, []),
    [],
    boolean(#"d87980"),
  )
}

test wrong_builtin_result_fails_closed() fail {
  let arguments = [integer(#"1829"), integer(#"01")]
  cek_builtin_v1.verify_direct_builtin_v1(
    0,
    builtin_root(0, arguments),
    arguments,
    integer(#"182b"),
  )
}

type LargeSemanticConstructorFixture {
  source: cek_builtin_v1.ValueWitnessV1,
  node: cek_data_v1.DataNodeV1,
  child_node: cek_data_v1.DataNodeV1,
  fields_node: cek_data_v1.DataListNodeV1,
  fields: cek_data_v1.DataSequenceSummaryV1,
}

fn large_semantic_constructor_fixture() -> LargeSemanticConstructorFixture {
  let bytes = builtin.append_bytearray(
    builtin.replicate_byte(4500, 42),
    builtin.replicate_byte(4500, 42),
  )
  let child = cek_data_v1.bytes_data_summary_v1(
    9000,
    cek_data_scan_v1.bounded_blob_root_v1(bytes),
  )
  let child_node = cek_data_v1.BytesData {
    bytes_root: cek_data_scan_v1.bounded_blob_root_v1(bytes),
    bytes_length: 9000,
    cbor_length: child.cbor_length,
    memory: child.memory,
  }
  let fields_node = cek_data_v1.DataListNodeV1 {
    head: child.root,
    head_cbor_length: child.cbor_length,
    head_memory: child.memory,
    tail: cek_data_v1.empty_data_list_root_v1(),
    length: 1,
    payload_cbor_length: child.cbor_length,
    memory: child.memory,
  }
  let fields = cek_data_v1.prepend_data_list_summary_v1(
    child,
    cek_data_v1.empty_data_list_summary_v1(),
  )
  let summary = cek_data_v1.small_constr_data_summary_v1(0, fields)
  let node = cek_data_v1.ConstrSmallData {
    constructor: 0,
    fields_count: fields.length,
    fields_root: fields.root,
    cbor_length: summary.cbor_length,
    memory: summary.memory,
  }
  LargeSemanticConstructorFixture {
    source: cek_builtin_v1.SemanticConstantValue {
      type_cbor: #"9f08ff",
      payload: summary,
      memory: summary.memory,
    },
    node,
    child_node,
    fields_node,
    fields,
  }
}

test semantic_choose_data_inspects_a_large_context_root_locally() {
  let fixture = large_semantic_constructor_fixture()
  let selected = integer(#"00")
  let arguments = [
    fixture.source,
    selected,
    integer(#"01"),
    integer(#"02"),
    integer(#"03"),
    integer(#"04"),
  ]
  cek_builtin_v1.verify_semantic_builtin_v1(
    36,
    builtin_root(36, arguments),
    arguments,
    selected,
    cek_builtin_v1.SemanticBuiltinWitnessV1 {
      data_nodes: [fixture.node],
      list_nodes: [fixture.fields_node],
      pair_nodes: [],
      scalar_preimages: [],
    },
  )
}

test semantic_unconstr_data_returns_the_exact_large_typed_pair() {
  let fixture = large_semantic_constructor_fixture()
  let constructor_cbor = cbor.serialise(builtin.i_data(0))
  let constructor = cek_data_v1.integer_data_summary_v1(
    0,
    cek_data_scan_v1.bounded_blob_root_v1(constructor_cbor),
  )
  let fields = cek_data_v1.list_data_summary_v1(fixture.fields)
  let result_payload = cek_data_v1.small_constr_data_summary_v1(
    0,
    cek_data_v1.prepend_data_list_summary_v1(
      constructor,
      cek_data_v1.prepend_data_list_summary_v1(
        fields,
        cek_data_v1.empty_data_list_summary_v1(),
      ),
    ),
  )
  let result = cek_builtin_v1.SemanticConstantValue {
    type_cbor: #"9f06000508ff",
    payload: result_payload,
    memory: constructor.memory - 4 + fixture.fields.memory,
  }
  let arguments = [fixture.source]
  cek_builtin_v1.verify_semantic_builtin_v1(
    42,
    builtin_root(42, arguments),
    arguments,
    result,
    cek_builtin_v1.SemanticBuiltinWitnessV1 {
      data_nodes: [fixture.node],
      list_nodes: [fixture.fields_node],
      pair_nodes: [],
      scalar_preimages: [],
    },
  )
}

test semantic_fst_pair_extracts_the_constructor_without_the_large_field() {
  let fixture = large_semantic_constructor_fixture()
  let constructor_cbor = cbor.serialise(builtin.i_data(0))
  let constructor = cek_data_v1.integer_data_summary_v1(
    0,
    cek_data_scan_v1.bounded_blob_root_v1(constructor_cbor),
  )
  let constructor_node = cek_data_v1.IntegerData {
    cbor_root: cek_data_scan_v1.bounded_blob_root_v1(constructor_cbor),
    cbor_length: constructor.cbor_length,
    memory: constructor.memory,
  }
  let fields = cek_data_v1.list_data_summary_v1(fixture.fields)
  let fields_data_node = cek_data_v1.ListData {
    items_count: fixture.fields.length,
    items_root: fixture.fields.root,
    cbor_length: fields.cbor_length,
    memory: fields.memory,
  }
  let second_link = cek_data_v1.DataListNodeV1 {
    head: fields.root,
    head_cbor_length: fields.cbor_length,
    head_memory: fields.memory,
    tail: cek_data_v1.empty_data_list_root_v1(),
    length: 1,
    payload_cbor_length: fields.cbor_length,
    memory: fields.memory,
  }
  let first_link = cek_data_v1.DataListNodeV1 {
    head: constructor.root,
    head_cbor_length: constructor.cbor_length,
    head_memory: constructor.memory,
    tail: cek_data_v1.hash_data_list_node_v1(second_link),
    length: 2,
    payload_cbor_length: constructor.cbor_length + fields.cbor_length,
    memory: constructor.memory + fields.memory,
  }
  let payload = cek_data_v1.small_constr_data_summary_v1(
    0,
    cek_data_v1.prepend_data_list_summary_v1(
      constructor,
      cek_data_v1.prepend_data_list_summary_v1(
        fields,
        cek_data_v1.empty_data_list_summary_v1(),
      ),
    ),
  )
  let payload_node = cek_data_v1.ConstrSmallData {
    constructor: 0,
    fields_count: 2,
    fields_root: cek_data_v1.hash_data_list_node_v1(first_link),
    cbor_length: payload.cbor_length,
    memory: payload.memory,
  }
  let source = cek_builtin_v1.SemanticConstantValue {
    type_cbor: #"9f06000508ff",
    payload,
    memory: constructor.memory - 4 + fixture.fields.memory,
  }
  let result = cek_builtin_v1.SemanticConstantValue {
    type_cbor: #"9f00ff",
    payload: constructor,
    memory: constructor.memory - 4,
  }
  let arguments = [source]
  cek_builtin_v1.verify_semantic_builtin_v1(
    29,
    builtin_root(29, arguments),
    arguments,
    result,
    cek_builtin_v1.SemanticBuiltinWitnessV1 {
      data_nodes: [payload_node, constructor_node, fields_data_node],
      list_nodes: [first_link, second_link],
      pair_nodes: [],
      scalar_preimages: [],
    },
  )
}

test semantic_head_list_extracts_a_9000_byte_data_child_by_root() {
  let fixture = large_semantic_constructor_fixture()
  let list_payload = cek_data_v1.list_data_summary_v1(fixture.fields)
  let source_node = cek_data_v1.ListData {
    items_count: fixture.fields.length,
    items_root: fixture.fields.root,
    cbor_length: list_payload.cbor_length,
    memory: list_payload.memory,
  }
  let source = cek_builtin_v1.SemanticConstantValue {
    type_cbor: #"9f0508ff",
    payload: list_payload,
    memory: fixture.fields.memory,
  }
  let child = cek_data_v1.DataSummaryV1 {
    root: cek_data_v1.hash_data_node_v1(fixture.child_node),
    cbor_length: cek_data_v1.data_node_cbor_length_v1(fixture.child_node),
    memory: cek_data_v1.data_node_memory_v1(fixture.child_node),
  }
  let result = cek_builtin_v1.SemanticConstantValue {
    type_cbor: #"9f08ff",
    payload: child,
    memory: child.memory,
  }
  let arguments = [source]
  cek_builtin_v1.verify_semantic_builtin_v1(
    33,
    builtin_root(33, arguments),
    arguments,
    result,
    cek_builtin_v1.SemanticBuiltinWitnessV1 {
      data_nodes: [source_node, fixture.child_node],
      list_nodes: [fixture.fields_node],
      pair_nodes: [],
      scalar_preimages: [],
    },
  )
}

test semantic_wrong_data_variant_failure_is_authenticated_locally() {
  let entries = cek_data_v1.empty_data_pair_summary_v1()
  let summary = cek_data_v1.map_data_summary_v1(entries)
  let node = cek_data_v1.MapData {
    entries_count: 0,
    entries_root: entries.root,
    cbor_length: summary.cbor_length,
    memory: summary.memory,
  }
  let source = cek_builtin_v1.SemanticConstantValue {
    type_cbor: #"9f08ff",
    payload: summary,
    memory: summary.memory,
  }
  let arguments = [source]
  cek_builtin_v1.verify_semantic_builtin_failure_v1(
    42,
    builtin_root(42, arguments),
    arguments,
    cek_builtin_v1.SemanticBuiltinWitnessV1 {
      data_nodes: [node],
      list_nodes: [],
      pair_nodes: [],
      scalar_preimages: [],
    },
  )
}
