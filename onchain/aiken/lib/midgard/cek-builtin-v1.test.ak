use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/fuzz
use aiken/math
use aiken/primitive/bytearray
use midgard/cek_builtin_v1
use midgard/cek_constant_v1
use midgard/cek_cost_v1
use midgard/cek_data_v1
use midgard/cek_proof_v1
use midgard/cek_staged_blob_v1.{flat_root_v1}

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
  verify_direct_with_split_parity(0, arguments, integer(#"182a"))
}

test add_integer_direct_budget_matches_reference_cek() {
  let arguments = [integer(#"01"), integer(#"1880")]
  and {
    verify_direct_with_split_parity(0, arguments, integer(#"1881")),
    cek_builtin_v1.direct_builtin_budget_v1(0, arguments) == cek_cost_v1.BuiltinBudgetV1 {
      cpu: 101208,
      memory: 2,
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
    cek_builtin_v1.authenticated_builtin_failure_budget(3, arguments) == cek_cost_v1.BuiltinBudgetV1 {
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
    cek_builtin_v1.authenticated_builtin_failure_budget(4, arguments) == cek_builtin_v1.direct_builtin_budget_v1(
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
    cek_builtin_v1.authenticated_builtin_failure_budget(73, arguments) == cek_cost_v1.BuiltinBudgetV1 {
      cpu: 1322607,
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
    cek_builtin_v1.authenticated_builtin_failure_budget(52, arguments) == cek_builtin_v1.direct_builtin_budget_v1(
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
    cek_builtin_v1.authenticated_builtin_failure_budget(53, arguments) == cek_builtin_v1.direct_builtin_budget_v1(
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
      59,
      builtin_root(59, arguments),
      arguments,
    ),
    cek_builtin_v1.authenticated_builtin_failure_budget(59, arguments) == cek_builtin_v1.direct_builtin_budget_v1(
      59,
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
      66,
      builtin_root(66, arguments),
      arguments,
    ),
    cek_builtin_v1.authenticated_builtin_failure_budget(66, arguments) == cek_builtin_v1.direct_builtin_budget_v1(
      66,
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
      59,
      builtin_root(59, arguments),
      arguments,
    ),
    cek_builtin_v1.authenticated_builtin_failure_budget(59, arguments) == cek_builtin_v1.direct_builtin_budget_v1(
      59,
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
    59,
    builtin_root(59, arguments),
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
  verify_direct_with_split_parity(52, arguments, boolean(#"d87980"))
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
  verify_direct_with_split_parity(10, arguments, bytes(#"43aabbcc"))
}

test if_then_else_selects_an_opaque_value_without_interpreting_it() {
  let selected = cek_builtin_v1.OpaqueValue(
    #"1111111111111111111111111111111111111111111111111111111111111111",
  )
  let rejected = cek_builtin_v1.OpaqueValue(
    #"2222222222222222222222222222222222222222222222222222222222222222",
  )
  let arguments = [boolean(#"d87a80"), selected, rejected]
  verify_direct_with_split_parity(26, arguments, selected)
}

test list_head_preserves_the_authenticated_element_type() {
  let source = constant(#"9f0500ff", #"9f0102ff")
  let arguments = [source]
  verify_direct_with_split_parity(33, arguments, integer(#"01"))
}

test i_data_and_un_i_data_round_trip_semantically() {
  let to_data_arguments = [integer(#"182a")]
  let data_value = constant(#"9f08ff", #"182a")
  and {
    verify_direct_with_split_parity(40, to_data_arguments, data_value),
    verify_direct_with_split_parity(45, [data_value], integer(#"182a")),
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
    verify_direct_with_split_parity(
      68,
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

/// The bls12_381_finalVerify budget does not depend on the roots of its two
/// Miller-loop results, so a step may take it as one value computed for any
/// two roots.
test final_verify_budget_is_the_same_for_every_pair_of_roots(
  roots via fuzz.both(fuzz.bytearray_fixed(32), fuzz.bytearray_fixed(32)),
) {
  let (left, right) = roots
  let zero = #"0000000000000000000000000000000000000000000000000000000000000000"
  cek_builtin_v1.direct_builtin_budget_v1(
    70,
    [
      cek_builtin_v1.BlsMillerLoopValue(left),
      cek_builtin_v1.BlsMillerLoopValue(right),
    ],
  ) == cek_builtin_v1.direct_builtin_budget_v1(
    70,
    [
      cek_builtin_v1.BlsMillerLoopValue(zero),
      cek_builtin_v1.BlsMillerLoopValue(zero),
    ],
  )
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
  expect
    cek_builtin_v1.verify_bls_expression_roots(
      expression,
      expression,
      expression_root,
      expression_root,
    )
  expect
    cek_builtin_v1.verify_authenticated_bls_final(
      expression,
      expression,
      boolean(#"d87a80"),
    )
  expect
    !cek_builtin_v1.verify_bls_expression_roots(
      expression,
      expression,
      #"00",
      expression_root,
    )
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
  verify_direct_with_split_parity(70, [], boolean(#"d87980"))
}

test wrong_builtin_result_fails_closed() fail {
  let arguments = [integer(#"1829"), integer(#"01")]
  verify_direct_with_split_parity(0, arguments, integer(#"182b"))
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
    cek_proof_v1.bounded_blob_root_v1(bytes),
  )
  let child_node = cek_data_v1.BytesData {
    bytes_root: cek_proof_v1.bounded_blob_root_v1(bytes),
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
  verify_semantic_with_split_parity(
    36,
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
    cek_proof_v1.bounded_blob_root_v1(constructor_cbor),
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
  verify_semantic_with_split_parity(
    42,
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
    cek_proof_v1.bounded_blob_root_v1(constructor_cbor),
  )
  let constructor_node = cek_data_v1.IntegerData {
    cbor_root: cek_proof_v1.bounded_blob_root_v1(constructor_cbor),
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
  verify_semantic_with_split_parity(
    29,
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
  verify_semantic_with_split_parity(
    33,
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
    arguments,
    cek_builtin_v1.SemanticBuiltinWitnessV1 {
      data_nodes: [node],
      list_nodes: [],
      pair_nodes: [],
      scalar_preimages: [],
    },
  )
}

const largest_constr_index = 18446744073709551615

const smallest_oversized_constr_index = 18446744073709551616

fn empty_data_fields() -> cek_builtin_v1.ValueWitnessV1 {
  constant(#"9f0508ff", #"80")
}

fn empty_data_fields_node() -> cek_data_v1.DataNodeV1 {
  cek_data_v1.ListData {
    items_count: 0,
    items_root: cek_data_v1.empty_data_list_root_v1(),
    cbor_length: 1,
    memory: 4,
  }
}

fn direct_integer(value: Int) -> cek_builtin_v1.ValueWitnessV1 {
  integer(cbor.serialise(builtin.i_data(value)))
}

fn semantic_integer_node(value: Int) -> cek_data_v1.DataNodeV1 {
  let raw = cbor.serialise(builtin.i_data(value))
  cek_data_v1.IntegerData {
    cbor_root: cek_proof_v1.bounded_blob_root_v1(raw),
    cbor_length: bytearray.length(raw),
    memory: 4 + cek_data_v1.integer_memory_size_v1(value),
  }
}

fn semantic_integer(value: Int) -> cek_builtin_v1.ValueWitnessV1 {
  let raw = cbor.serialise(builtin.i_data(value))
  let summary = cek_data_v1.integer_data_summary_v1(
    value,
    cek_proof_v1.bounded_blob_root_v1(raw),
  )
  cek_builtin_v1.SemanticConstantValue {
    type_cbor: #"9f00ff",
    payload: summary,
    memory: summary.memory - 4,
  }
}

fn constr_data_result(index: Int) -> cek_builtin_v1.ValueWitnessV1 {
  let raw = cbor.serialise(builtin.i_data(index))
  let summary = cek_data_v1.large_constr_data_summary_from_cbor_v1(
    cek_proof_v1.bounded_blob_root_v1(raw),
    bytearray.length(raw),
    4 + cek_data_v1.integer_memory_size_v1(index),
    cek_data_v1.empty_data_list_summary_v1(),
  )
  cek_builtin_v1.SemanticConstantValue {
    type_cbor: #"9f08ff",
    payload: summary,
    memory: summary.memory,
  }
}

fn constr_data_succeeds(
  index: cek_builtin_v1.ValueWitnessV1,
  index_nodes: List<cek_data_v1.DataNodeV1>,
  scalar_preimages: List<ByteArray>,
  value: Int,
) -> Bool {
  let arguments = [index, empty_data_fields()]
  cek_builtin_v1.verify_semantic_builtin_v1(
    37,
    arguments,
    constr_data_result(value),
    cek_builtin_v1.SemanticBuiltinWitnessV1 {
      data_nodes: list.concat(index_nodes, [empty_data_fields_node()]),
      list_nodes: [],
      pair_nodes: [],
      scalar_preimages,
    },
  )
}

fn constr_data_fails(
  index: cek_builtin_v1.ValueWitnessV1,
  data_nodes: List<cek_data_v1.DataNodeV1>,
  scalar_preimages: List<ByteArray>,
) -> Bool {
  let arguments = [index, empty_data_fields()]
  cek_builtin_v1.verify_semantic_builtin_failure_v1(
    37,
    arguments,
    cek_builtin_v1.SemanticBuiltinWitnessV1 {
      data_nodes,
      list_nodes: [],
      pair_nodes: [],
      scalar_preimages,
    },
  )
}

fn leaf(value: Int) -> ByteArray {
  cbor.serialise(builtin.i_data(value))
}

test constr_data_accepts_the_largest_unsigned_64_bit_index() {
  constr_data_succeeds(
    direct_integer(largest_constr_index),
    [],
    [],
    largest_constr_index,
  )
}

test semantic_constr_data_accepts_the_largest_unsigned_64_bit_index() {
  constr_data_succeeds(
    semantic_integer(largest_constr_index),
    [semantic_integer_node(largest_constr_index)],
    [leaf(largest_constr_index)],
    largest_constr_index,
  )
}

test constr_data_cannot_succeed_at_minus_one() fail {
  constr_data_succeeds(
    semantic_integer(-1),
    [semantic_integer_node(-1)],
    [leaf(-1)],
    -1,
  )
}

test constr_data_fails_provably_at_two_to_the_64() {
  constr_data_fails(
    semantic_integer(smallest_oversized_constr_index),
    [semantic_integer_node(smallest_oversized_constr_index)],
    [],
  )
}

test constr_data_fails_provably_at_minus_one() {
  constr_data_fails(direct_integer(-1), [semantic_integer_node(-1)], [leaf(-1)])
}

test semantic_constr_data_fails_provably_at_minus_one() {
  constr_data_fails(
    semantic_integer(-1),
    [semantic_integer_node(-1)],
    [leaf(-1)],
  )
}

test constr_data_failure_is_refused_at_the_largest_index() fail {
  constr_data_fails(
    semantic_integer(largest_constr_index),
    [semantic_integer_node(largest_constr_index)],
    [leaf(largest_constr_index)],
  )
}

test constr_data_failure_is_refused_at_zero() fail {
  constr_data_fails(direct_integer(0), [semantic_integer_node(0)], [leaf(0)])
}

test one_word_constr_data_index_cannot_fail_from_memory() fail {
  constr_data_fails(semantic_integer(-1), [semantic_integer_node(-1)], [])
}

// The old aggregate remains the oracle while physical core hops are introduced.
fn verify_direct_with_split_parity(
  tag: Int,
  arguments: List<cek_builtin_v1.ValueWitnessV1>,
  result: cek_builtin_v1.ValueWitnessV1,
) -> Bool {
  let aggregate = cek_builtin_v1.verify_direct_builtin_v1(
    tag,
    arguments,
    result,
  )
  let semantics = if tag >= 29 && tag <= 69 && tag != 52 && tag != 53 {
    cek_builtin_v1.verify_direct_structured_semantics(tag, arguments, result)
  } else {
    cek_builtin_v1.verify_direct_scalar_semantics(tag, arguments, result)
  }
  expect semantics == aggregate
  aggregate
}

fn verify_semantic_with_split_parity(
  tag: Int,
  arguments: List<cek_builtin_v1.ValueWitnessV1>,
  result: cek_builtin_v1.ValueWitnessV1,
  material: cek_builtin_v1.SemanticBuiltinWitnessV1,
) -> Bool {
  let aggregate = cek_builtin_v1.verify_semantic_builtin_v1(
    tag,
    arguments,
    result,
    material,
  )
  let compact_arguments = list.map(
    arguments,
    cek_builtin_v1.compact_semantic_value,
  )
  let compact_result = cek_builtin_v1.compact_semantic_value(result)
  expect
    cek_builtin_v1.arguments_root_v1(arguments) == cek_builtin_v1.arguments_root_v1(
      compact_arguments,
    )
  expect
    cek_builtin_v1.result_root_v1(result) == cek_builtin_v1.result_root_v1(
      compact_result,
    )
  expect
    cek_builtin_v1.verify_semantic_builtin_v1(
      tag,
      compact_arguments,
      compact_result,
      material,
    ) == aggregate
  let semantics = if tag == 29 || tag == 30 {
    cek_builtin_v1.verify_semantic_pair_v1(tag, arguments, result, material)
  } else if tag == 31 || tag == 32 || tag == 35 {
    cek_builtin_v1.verify_semantic_list_construct(
      tag,
      arguments,
      result,
      material,
    )
  } else if tag == 33 || tag == 34 {
    cek_builtin_v1.verify_semantic_list_select(tag, arguments, result, material)
  } else if tag == 36 {
    cek_builtin_v1.verify_semantic_choose_data_v1(arguments, result, material)
  } else if tag == 37 || tag == 42 {
    cek_builtin_v1.verify_semantic_data_construct(
      tag,
      arguments,
      result,
      material,
    )
  } else if tag >= 47 && tag <= 51 {
    cek_builtin_v1.verify_semantic_data_misc(tag, arguments, result, material)
  } else {
    cek_builtin_v1.verify_semantic_data_scalar(tag, arguments, result, material)
  }
  expect semantics == aggregate
  aggregate
}

test bls_root_hop_refuses_twelve_leaves() {
  let expression = six_miller_loop_expression()
  !cek_builtin_v1.verify_bls_expression_roots(expression, expression, #"", #"")
}

// The MlResult leaf cap is a proof limit, not a Cardano rule: eleven leaves
// fail the root hop (the validator expects it, so nothing is certified) and
// abort the final-verify arm, which has no failure successor to fall into.
test bls_root_hop_refuses_eleven_leaves() {
  let left = six_miller_loop_expression()
  let right = five_miller_loop_expression()
  !cek_builtin_v1.verify_bls_expression_roots(left, right, #"", #"")
}

fn bls_expression_root(
  expression: cek_builtin_v1.BlsExpressionWitnessV1,
) -> ByteArray {
  when expression is {
    cek_builtin_v1.BlsMillerLoopExpression { g1, g2 } ->
      cek_proof_v1.hash_bls_miller_loop_expression_v1(
        cek_constant_v1.constant_root_v1(g1),
        cek_constant_v1.constant_root_v1(g2),
      )
    cek_builtin_v1.BlsMultiplyExpression { left, right } ->
      cek_proof_v1.hash_bls_multiply_expression_v1(
        bls_expression_root(left),
        bls_expression_root(right),
      )
  }
}

fn unequal_final_verify(
  left: cek_builtin_v1.BlsExpressionWitnessV1,
  right: cek_builtin_v1.BlsExpressionWitnessV1,
) -> Bool {
  let arguments = [
    cek_builtin_v1.BlsMillerLoopValue(bls_expression_root(left)),
    cek_builtin_v1.BlsMillerLoopValue(bls_expression_root(right)),
  ]
  cek_builtin_v1.verify_direct_bls_final_v1(
    builtin_root(70, arguments),
    left,
    right,
    boolean(#"d87980"),
  )
}

test nine_leaf_unequal_final_verify_is_certified() {
  unequal_final_verify(
    five_miller_loop_expression(),
    cek_builtin_v1.BlsMultiplyExpression {
      left: double_miller_loop_expression(),
      right: double_miller_loop_expression(),
    },
  )
}

test eleven_leaf_unequal_final_verify_aborts() fail {
  unequal_final_verify(
    six_miller_loop_expression(),
    five_miller_loop_expression(),
  )
}

// The 9,215 B revealed-payload cap is a proof limit, not a Cardano rule. At
// the cap each arm certifies; one byte over, the success arms, the known
// failure arm and the type failure arm all abort, so the limit never becomes
// a failure successor. Plutus CBOR chunks a byte string above 64 B, so a
// payload of L bytes is L + 2 * ceil(L / 64) + 2 bytes.
fn zero_bytes_constant(length: Int) -> ByteArray {
  let half = length / 2
  cbor.serialise(
    builtin.b_data(
      bytearray.concat(
        bytearray.from_int_big_endian(0, half),
        bytearray.from_int_big_endian(0, length - half),
      ),
    ),
  )
}

fn length_of_bytes_at(length: Int) -> Bool {
  let payload = zero_bytes_constant(length)
  let result = integer(cbor.serialise(builtin.i_data(length)))
  let arguments = [bytes(payload)]
  verify_direct_with_split_parity(13, arguments, result)
}

fn negative_index_failure_at(length: Int) -> Bool {
  let arguments = [bytes(zero_bytes_constant(length)), integer(#"20")]
  cek_builtin_v1.verify_direct_builtin_failure_v1(
    14,
    builtin_root(14, arguments),
    arguments,
  )
}

fn add_integer_type_failure_at(length: Int) -> Bool {
  let arguments = [
    runtime_constant(#"9f01ff", zero_bytes_constant(length)),
    runtime_constant(#"9f00ff", #"01"),
  ]
  cek_builtin_v1.verify_builtin_type_failure_v1(
    0,
    runtime_builtin_root(0, arguments),
    arguments,
  )
}

test direct_arms_certify_at_the_revealed_payload_cap() {
  // lengthOfByteString: 9,210 B argument + 3 B result.
  expect bytearray.length(zero_bytes_constant(8930)) == 9212
  // indexByteString and the addInteger type failure: 9,214 B + 1 B.
  expect bytearray.length(zero_bytes_constant(8932)) == 9214
  and {
    length_of_bytes_at(8930),
    negative_index_failure_at(8932),
    add_integer_type_failure_at(8932),
  }
}

test direct_success_one_byte_over_the_cap_aborts() fail {
  expect bytearray.length(zero_bytes_constant(8931)) == 9213
  length_of_bytes_at(8931)
}

test direct_scalar_semantics_one_byte_over_the_cap_aborts() fail {
  let arguments = [bytes(zero_bytes_constant(8931))]
  cek_builtin_v1.verify_direct_scalar_semantics(
    13,
    arguments,
    integer(cbor.serialise(builtin.i_data(8931))),
  )
}

test known_failure_one_byte_over_the_cap_aborts() fail {
  expect bytearray.length(zero_bytes_constant(8933)) == 9215
  negative_index_failure_at(8933)
}

test type_failure_one_byte_over_the_cap_aborts() fail {
  expect bytearray.length(zero_bytes_constant(8933)) == 9215
  add_integer_type_failure_at(8933)
}

fn repeated_byte(byte: Int, count: Int) -> ByteArray {
  if count == 0 {
    #""
  } else {
    builtin.cons_bytearray(byte, repeated_byte(byte, count - 1))
  }
}

fn one_pair_map() -> cek_builtin_v1.ValueWitnessV1 {
  constant(
    #"9f08ff",
    cbor.serialise(
      builtin.map_data([Pair(builtin.i_data(1), builtin.i_data(2))]),
    ),
  )
}

fn one_pair_list() -> cek_builtin_v1.ValueWitnessV1 {
  constant(
    #"9f05060808ff",
    cbor.serialise(
      builtin.list_data(
        [builtin.constr_data(0, [builtin.i_data(1), builtin.i_data(2)])],
      ),
    ),
  )
}

// mapData and unMapData have exactly one successor: the map-conversion start.
// The direct arm refuses both even when the conversion it claims is correct.
test map_conversions_have_no_direct_arm() {
  let map_arguments = [one_pair_list()]
  let unmap_arguments = [one_pair_map()]
  and {
    !verify_direct_with_split_parity(38, map_arguments, one_pair_map()),
    !verify_direct_with_split_parity(43, unmap_arguments, one_pair_list()),
  }
}

fn ill_typed_failure_has_one_arm(
  tag: Int,
  arguments: List<cek_builtin_v1.ValueWitnessV1>,
  runtime: List<cek_builtin_v1.RuntimeValueWitnessV1>,
) -> Bool {
  and {
    cek_builtin_v1.runtime_arguments_root_v1(runtime) == cek_builtin_v1.arguments_root_v1(
      arguments,
    ),
    !cek_builtin_v1.verify_direct_builtin_failure_v1(
      tag,
      builtin_root(tag, arguments),
      arguments,
    ),
    cek_builtin_v1.verify_builtin_type_failure_v1(
      tag,
      runtime_builtin_root(tag, runtime),
      runtime,
    ),
  }
}

// A division whose dividend is not an integer is a type failure at zero cost,
// never a paid division-by-zero failure.
test ill_typed_division_is_only_a_type_failure() {
  let dividend = cbor.serialise(builtin.b_data(#"aa"))
  let divisor = cbor.serialise(builtin.i_data(0))
  let arguments = [bytes(dividend), integer(divisor)]
  let runtime = [
    runtime_constant(#"9f01ff", dividend),
    runtime_constant(#"9f00ff", divisor),
  ]
  and {
    ill_typed_failure_has_one_arm(4, arguments, runtime),
    ill_typed_failure_has_one_arm(5, arguments, runtime),
    ill_typed_failure_has_one_arm(6, arguments, runtime),
  }
}

// Likewise for hash-to-group with an integer message.
test ill_typed_hash_to_group_is_only_a_type_failure() {
  let message = cbor.serialise(builtin.i_data(7))
  let dst = cbor.serialise(builtin.b_data(repeated_byte(7, 256)))
  let arguments = [integer(message), bytes(dst)]
  let runtime = [
    runtime_constant(#"9f00ff", message),
    runtime_constant(#"9f01ff", dst),
  ]
  and {
    ill_typed_failure_has_one_arm(60, arguments, runtime),
    ill_typed_failure_has_one_arm(67, arguments, runtime),
  }
}

// consByteString with an out-of-range byte and a non-bytes suffix is a type
// failure, never a paid known failure.
test ill_typed_cons_byte_string_is_only_a_type_failure() {
  let byte = cbor.serialise(builtin.i_data(300))
  let suffix = cbor.serialise(builtin.i_data(7))
  ill_typed_failure_has_one_arm(
    11,
    [integer(byte), integer(suffix)],
    [runtime_constant(#"9f00ff", byte), runtime_constant(#"9f00ff", suffix)],
  )
}

// writeBits is a known failure only over a list of integers and a boolean;
// the same out-of-range index in a list of Data, or with a non-boolean bit,
// is a type failure.
test ill_typed_write_bits_is_only_a_type_failure() {
  let source = cbor.serialise(builtin.b_data(#"00"))
  let indices = cbor.serialise(builtin.list_data([builtin.i_data(9)]))
  let set = #"d87a80"
  let well_typed = [bytes(source), constant(#"9f0500ff", indices), boolean(set)]
  and {
    cek_builtin_v1.verify_direct_builtin_failure_v1(
      80,
      builtin_root(80, well_typed),
      well_typed,
    ),
    ill_typed_failure_has_one_arm(
      80,
      [bytes(source), constant(#"9f0508ff", indices), boolean(set)],
      [
        runtime_constant(#"9f01ff", source),
        runtime_constant(#"9f0508ff", indices),
        runtime_constant(#"9f04ff", set),
      ],
    ),
    ill_typed_failure_has_one_arm(
      80,
      [bytes(source), constant(#"9f0500ff", indices), bytes(source)],
      [
        runtime_constant(#"9f01ff", source),
        runtime_constant(#"9f0500ff", indices),
        runtime_constant(#"9f01ff", source),
      ],
    ),
  }
}

fn opaque_dividend_arguments() -> List<cek_builtin_v1.ValueWitnessV1> {
  [
    cek_builtin_v1.OpaqueValue(
      cek_constant_v1.constant_root_v1(
        cek_constant_v1.ConstantWitnessV1 {
          type_cbor: #"9f00ff",
          payload_cbor: cbor.serialise(builtin.i_data(5)),
        },
      ),
    ),
    integer(#"00"),
  ]
}

// A sized argument presented opaquely has no budget, so the failure it hides
// is proved only by revealing it.
test revealed_dividend_is_a_known_failure() {
  let revealed = [integer(cbor.serialise(builtin.i_data(5))), integer(#"00")]
  and {
    builtin_root(4, opaque_dividend_arguments()) == builtin_root(4, revealed),
    cek_builtin_v1.verify_direct_builtin_failure_v1(
      4,
      builtin_root(4, revealed),
      revealed,
    ),
  }
}

test opaque_dividend_is_not_a_known_failure() fail {
  let hidden = opaque_dividend_arguments()
  cek_builtin_v1.verify_direct_builtin_failure_v1(
    4,
    builtin_root(4, hidden),
    hidden,
  )
}

test opaque_value_has_no_memory_size() fail {
  cek_builtin_v1.value_memory_size_v1(
    cek_builtin_v1.OpaqueValue(
      #"1111111111111111111111111111111111111111111111111111111111111111",
    ),
  ) >= 0
}

// mkCons is typed by the committed runtime type, so a semantic presentation of
// a well-typed item or list is not a type failure.
test mk_cons_is_typed_by_the_committed_type_in_any_presentation() {
  let item = cek_constant_v1.ConstantWitnessV1 {
    type_cbor: #"9f00ff",
    payload_cbor: #"01",
  }
  let items = cek_constant_v1.ConstantWitnessV1 {
    type_cbor: #"9f0500ff",
    payload_cbor: #"80",
  }
  let Pair(item_payload, item_memory) = cek_constant_v1.constant_semantic_projection(
    item,
  )
  let Pair(items_payload, items_memory) = cek_constant_v1.constant_semantic_projection(
    items,
  )
  let constant_arguments = [
    cek_builtin_v1.RuntimeConstantValue(item),
    cek_builtin_v1.RuntimeConstantValue(items),
  ]
  let semantic_item = [
    cek_builtin_v1.RuntimeSemanticConstantValue {
      type_cbor: #"9f00ff",
      payload: item_payload,
      memory: item_memory,
    },
    cek_builtin_v1.RuntimeConstantValue(items),
  ]
  let semantic_list = [
    cek_builtin_v1.RuntimeConstantValue(item),
    cek_builtin_v1.RuntimeSemanticConstantValue {
      type_cbor: #"9f0500ff",
      payload: items_payload,
      memory: items_memory,
    },
  ]
  let root = runtime_builtin_root(32, constant_arguments)
  and {
    runtime_builtin_root(32, semantic_item) == root,
    runtime_builtin_root(32, semantic_list) == root,
    cek_builtin_v1.builtin_arguments_are_well_typed_v1(32, constant_arguments),
    cek_builtin_v1.builtin_arguments_are_well_typed_v1(32, semantic_item),
    cek_builtin_v1.builtin_arguments_are_well_typed_v1(32, semantic_list),
    !cek_builtin_v1.verify_builtin_type_failure_v1(32, root, semantic_item),
    !cek_builtin_v1.verify_builtin_type_failure_v1(32, root, semantic_list),
  }
}

// An out-of-range index over a source that is not a byte string: tags 14
// (indexByteString) and 79 (readBit) read the index first, and a negative
// index must not let the source go untyped.
fn integer_source_negative_index() -> List<cek_builtin_v1.ValueWitnessV1> {
  [integer(cbor.serialise(builtin.i_data(5))), integer(#"20")]
}

fn integer_source_negative_index_runtime() -> List<
  cek_builtin_v1.RuntimeValueWitnessV1,
> {
  [
    runtime_constant(#"9f00ff", cbor.serialise(builtin.i_data(5))),
    runtime_constant(#"9f00ff", #"20"),
  ]
}

fn known_failure_of_integer_source(tag: Int) -> Bool {
  cek_builtin_v1.verify_direct_builtin_failure_v1(
    tag,
    builtin_root(tag, integer_source_negative_index()),
    integer_source_negative_index(),
  )
}

test ill_typed_index_byte_string_source_is_not_a_known_failure() fail {
  known_failure_of_integer_source(14)
}

test ill_typed_read_bit_source_is_not_a_known_failure() fail {
  known_failure_of_integer_source(79)
}

test ill_typed_index_source_is_a_type_failure() {
  let runtime = integer_source_negative_index_runtime()
  and {
    cek_builtin_v1.runtime_arguments_root_v1(runtime) == cek_builtin_v1.arguments_root_v1(
      integer_source_negative_index(),
    ),
    cek_builtin_v1.verify_builtin_type_failure_v1(
      14,
      runtime_builtin_root(14, runtime),
      runtime,
    ),
    cek_builtin_v1.verify_builtin_type_failure_v1(
      79,
      runtime_builtin_root(79, runtime),
      runtime,
    ),
  }
}

// The same negative index over a byte string is a known failure.
test negative_index_over_bytes_is_a_known_failure() {
  let arguments = [bytes(cbor.serialise(builtin.b_data(#"aa"))), integer(#"20")]
  and {
    cek_builtin_v1.verify_direct_builtin_failure_v1(
      14,
      builtin_root(14, arguments),
      arguments,
    ),
    cek_builtin_v1.verify_direct_builtin_failure_v1(
      79,
      builtin_root(79, arguments),
      arguments,
    ),
  }
}

// Cardano converts these operands to Int64 before costing the builtin.
fn integer_value(value: Int) -> cek_builtin_v1.ValueWitnessV1 {
  integer(cbor.serialise(builtin.i_data(value)))
}

fn int64_failure(tag: Int, amount: Int, length_operand: Bool) -> Bool {
  let arguments = if tag == 12 {
    if length_operand {
      [integer_value(0), integer_value(amount), bytes(#"42abcd")]
    } else {
      [integer_value(amount), integer_value(1), bytes(#"42abcd")]
    }
  } else {
    [bytes(#"42abcd"), integer_value(amount)]
  }
  cek_builtin_v1.verify_direct_builtin_failure_v1(
    tag,
    builtin_root(tag, arguments),
    arguments,
  )
}

test int64_slice_start_and_length_failures_are_authenticated_without_charging() {
  list.all(
    [9223372036854775808, -9223372036854775809],
    fn(amount) {
      and {
        int64_failure(12, amount, False),
        int64_failure(12, amount, True),
      }
    },
  )
}

test int64_shift_and_rotate_failures_are_authenticated_without_charging() {
  list.all(
    [82, 83],
    fn(tag) {
      list.all(
        [9223372036854775808, -9223372036854775809],
        fn(amount) { int64_failure(tag, amount, False) },
      )
    },
  )
}

test int64_endpoints_are_not_builtin_failures() {
  list.all(
    [9223372036854775807, -9223372036854775808],
    fn(amount) {
      and {
        !int64_failure(12, amount, False),
        !int64_failure(12, amount, True),
        !int64_failure(82, amount, False),
        !int64_failure(83, amount, False),
      }
    },
  )
}

fn ecdsa_scalar_arguments(r: Int, s: Int) -> List<cek_builtin_v1.ValueWitnessV1> {
  [
    bytes(
      cbor.serialise(
        builtin.b_data(
          #"0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798",
        ),
      ),
    ),
    bytes(
      cbor.serialise(
        builtin.b_data(
          #"0000000000000000000000000000000000000000000000000000000000000000",
        ),
      ),
    ),
    bytes(
      cbor.serialise(
        builtin.b_data(
          builtin.append_bytearray(
            builtin.integer_to_bytearray(True, 32, r),
            builtin.integer_to_bytearray(True, 32, s),
          ),
        ),
      ),
    ),
  ]
}

test ecdsa_overflowing_r_or_s_is_a_paid_authenticated_failure() {
  let n = 115792089237316195423570985008687907852837564279074904382605163141518161494337
  list.all(
    [Pair(n, 1), Pair(1, n), Pair(n + 1, 1), Pair(1, n + 1)],
    fn(scalars) {
      let arguments = ecdsa_scalar_arguments(scalars.1st, scalars.2nd)
      and {
        cek_builtin_v1.verify_direct_builtin_failure_v1(
          52,
          builtin_root(52, arguments),
          arguments,
        ),
        cek_builtin_v1.authenticated_builtin_failure_budget(52, arguments) == cek_cost_v1.BuiltinBudgetV1 {
          cpu: 43053543,
          memory: 10,
        },
      }
    },
  )
}

test ecdsa_zero_and_high_s_are_false_results_not_builtin_failures() {
  let n = 115792089237316195423570985008687907852837564279074904382605163141518161494337
  list.all(
    [Pair(0, 1), Pair(1, 0), Pair(n - 1, 1), Pair(1, n - 1)],
    fn(scalars) {
      let arguments = ecdsa_scalar_arguments(scalars.1st, scalars.2nd)
      and {
        !cek_builtin_v1.verify_direct_builtin_failure_v1(
          52,
          builtin_root(52, arguments),
          arguments,
        ),
        cek_builtin_v1.verify_direct_scalar_semantics(
          52,
          arguments,
          boolean(#"d87980"),
        ),
      }
    },
  )
}

test ed25519_malformed_lengths_fail_but_invalid_signatures_return_false() {
  let key = #"d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a"
  let signature = #"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
  let arguments = [
    bytes(cbor.serialise(builtin.b_data(key))),
    bytes(#"40"),
    bytes(cbor.serialise(builtin.b_data(signature))),
  ]
  let short_key_arguments = [
    bytes(#"40"),
    bytes(#"40"),
    bytes(cbor.serialise(builtin.b_data(signature))),
  ]
  let short_signature_arguments = [
    bytes(cbor.serialise(builtin.b_data(key))),
    bytes(#"40"),
    bytes(#"40"),
  ]
  and {
    cek_builtin_v1.verify_direct_builtin_failure_v1(
      21,
      builtin_root(21, short_key_arguments),
      short_key_arguments,
    ),
    cek_builtin_v1.verify_direct_builtin_failure_v1(
      21,
      builtin_root(21, short_signature_arguments),
      short_signature_arguments,
    ),
    !cek_builtin_v1.verify_direct_builtin_failure_v1(
      21,
      builtin_root(21, arguments),
      arguments,
    ),
    cek_builtin_v1.verify_direct_scalar_semantics(
      21,
      arguments,
      boolean(#"d87980"),
    ),
    cek_builtin_v1.authenticated_builtin_failure_budget(21, short_key_arguments) == cek_cost_v1.BuiltinBudgetV1 {
      cpu: 53398444,
      memory: 10,
    },
  }
}

test cardano_shift_over_int64_fails(source via fuzz.bytearray()) fail {
  builtin.shift_bytearray(source, 9223372036854775808) == #""
}

test cardano_rotate_below_int64_fails(source via fuzz.bytearray()) fail {
  builtin.rotate_bytearray(source, -9223372036854775809) == #""
}

// The successful slice arm explicitly excludes Cardano's failed Int64 decode,
// even though the pinned Aiken test evaluator clamps this operand.
test out_of_int64_slice_cannot_claim_a_successful_empty_result() fail {
  cek_builtin_v1.verify_direct_scalar_semantics(
    12,
    [integer_value(9223372036854775808), integer_value(1), bytes(#"42abcd")],
    bytes(#"40"),
  )
}

test int64_failure_tags_reject_ill_typed_sources() {
  let slice_arguments = [
    integer_value(9223372036854775808),
    integer_value(1),
    integer_value(0),
  ]
  let shift_arguments = [integer_value(0), integer_value(9223372036854775808)]
  let slice_runtime = [
    runtime_constant(#"9f00ff", #"1b8000000000000000"),
    runtime_constant(#"9f00ff", #"01"),
    runtime_constant(#"9f00ff", #"00"),
  ]
  let shift_runtime = [
    runtime_constant(#"9f00ff", #"00"),
    runtime_constant(#"9f00ff", #"1b8000000000000000"),
  ]
  and {
    ill_typed_failure_has_one_arm(12, slice_arguments, slice_runtime),
    ill_typed_failure_has_one_arm(82, shift_arguments, shift_runtime),
    ill_typed_failure_has_one_arm(83, shift_arguments, shift_runtime),
  }
}

test out_of_int64_shift_cannot_claim_a_successful_zero_result() fail {
  cek_builtin_v1.verify_direct_scalar_semantics(
    82,
    [bytes(#"42abcd"), integer_value(9223372036854775808)],
    bytes(#"420000"),
  )
}

test out_of_int64_rotate_cannot_claim_a_successful_result() fail {
  cek_builtin_v1.verify_direct_scalar_semantics(
    83,
    [bytes(#"42abcd"), integer_value(-9223372036854775809)],
    bytes(#"42abcd"),
  )
}

test overflowing_ecdsa_scalar_cannot_claim_a_successful_false_result() fail {
  let arguments = ecdsa_scalar_arguments(
    115792089237316195423570985008687907852837564279074904382605163141518161494337,
    1,
  )
  cek_builtin_v1.verify_direct_scalar_semantics(
    52,
    arguments,
    boolean(#"d87980"),
  )
}

test int64_endpoints_succeed_with_exact_slice_shift_rotate_results() {
  and {
    cek_builtin_v1.verify_direct_scalar_semantics(
      12,
      [integer_value(9223372036854775807), integer_value(1), bytes(#"42abcd")],
      bytes(#"40"),
    ),
    cek_builtin_v1.verify_direct_scalar_semantics(
      12,
      [integer_value(-9223372036854775808), integer_value(1), bytes(#"42abcd")],
      bytes(#"41ab"),
    ),
    cek_builtin_v1.verify_direct_scalar_semantics(
      12,
      [integer_value(0), integer_value(9223372036854775807), bytes(#"42abcd")],
      bytes(#"42abcd"),
    ),
    cek_builtin_v1.verify_direct_scalar_semantics(
      12,
      [integer_value(0), integer_value(-9223372036854775808), bytes(#"42abcd")],
      bytes(#"40"),
    ),
    cek_builtin_v1.verify_direct_scalar_semantics(
      82,
      [bytes(#"42abcd"), integer_value(9223372036854775807)],
      bytes(#"420000"),
    ),
    cek_builtin_v1.verify_direct_scalar_semantics(
      82,
      [bytes(#"42abcd"), integer_value(-9223372036854775808)],
      bytes(#"420000"),
    ),
    cek_builtin_v1.verify_direct_scalar_semantics(
      83,
      [bytes(#"42abcd"), integer_value(9223372036854775807)],
      bytes(#"42d5e6"),
    ),
    cek_builtin_v1.verify_direct_scalar_semantics(
      83,
      [bytes(#"42abcd"), integer_value(-9223372036854775808)],
      bytes(#"42abcd"),
    ),
  }
}

test malformed_ed25519_lengths_charge_the_exact_builtin_budget() {
  let key = #"d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a"
  list.all(
    [Pair(31, 64), Pair(32, 63), Pair(32, 65), Pair(0, 0)],
    fn(lengths) {
      let signature = if lengths.2nd == 65 {
        builtin.cons_bytearray(0, builtin.integer_to_bytearray(True, 64, 0))
      } else if lengths.2nd == 0 {
        #""
      } else {
        builtin.integer_to_bytearray(True, lengths.2nd, 0)
      }
      let arguments = [
        bytes(
          cbor.serialise(
            builtin.b_data(builtin.slice_bytearray(0, lengths.1st, key)),
          ),
        ),
        bytes(#"40"),
        bytes(cbor.serialise(builtin.b_data(signature))),
      ]
      and {
        cek_builtin_v1.verify_direct_builtin_failure_v1(
          21,
          builtin_root(21, arguments),
          arguments,
        ),
        cek_builtin_v1.authenticated_builtin_failure_budget(21, arguments) == cek_cost_v1.BuiltinBudgetV1 {
          cpu: 53398444,
          memory: 10,
        },
      }
    },
  )
}

test ed25519_charges_message_words_and_keeps_true_false_successes() {
  let key = bytes(
    #"5820d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a",
  )
  let signature = bytes(
    cbor.serialise(builtin.b_data(builtin.integer_to_bytearray(True, 64, 0))),
  )
  let valid_signature = bytes(
    #"5840e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e065224901555fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b",
  )
  and {
    cek_builtin_v1.verify_direct_scalar_semantics(
      21,
      [key, bytes(#"40"), valid_signature],
      boolean(#"d87a80"),
    ),
    list.all(
      [
        Pair(0, 53398444), Pair(1, 53398444), Pair(8, 53398444),
        Pair(9, 53412777), Pair(16, 53412777), Pair(17, 53427110),
      ],
      fn(example) {
        let message = if example.1st == 0 {
          #""
        } else {
          builtin.integer_to_bytearray(True, example.1st, 0)
        }
        let arguments = [
          key,
          bytes(cbor.serialise(builtin.b_data(message))),
          signature,
        ]
        and {
          cek_builtin_v1.direct_builtin_budget_v1(21, arguments) == cek_cost_v1.BuiltinBudgetV1 {
            cpu: example.2nd,
            memory: 10,
          },
          cek_builtin_v1.verify_direct_scalar_semantics(
            21,
            arguments,
            boolean(#"d87980"),
          ),
          !cek_builtin_v1.verify_direct_builtin_failure_v1(
            21,
            builtin_root(21, arguments),
            arguments,
          ),
        }
      },
    ),
  }
}

// serialiseData over Data holding a constructor index of 128 or more and
// integers outside 64 bits: the semantic arm decodes the argument constant and
// the revealed serialisation with the Cardano Data decoder.
fn wide_data() -> Data {
  builtin.constr_data(
    0,
    [
      builtin.constr_data(128, [builtin.i_data(1)]),
      builtin.constr_data(18446744073709551615, []),
      builtin.i_data(18446744073709551616),
      builtin.i_data(-18446744073709551617),
    ],
  )
}

fn serialise_data_verifies(
  source: Data,
  revealed: ByteArray,
  serialised: ByteArray,
) -> Bool {
  let arguments = [constant(#"9f08ff", cbor.serialise(source))]
  verify_semantic_with_split_parity(
    51,
    arguments,
    bytes(cbor.serialise(builtin.b_data(serialised))),
    cek_builtin_v1.SemanticBuiltinWitnessV1 {
      data_nodes: [],
      list_nodes: [],
      pair_nodes: [],
      scalar_preimages: [revealed],
    },
  )
}

test semantic_serialise_data_handles_wide_constructors_and_integers() {
  let raw = cbor.serialise(wide_data())
  serialise_data_verifies(wide_data(), raw, raw)
}

test semantic_serialise_data_refuses_a_different_result() {
  let raw = cbor.serialise(wide_data())
  let other = cbor.serialise(builtin.constr_data(0, [builtin.i_data(1)]))
  !serialise_data_verifies(wide_data(), raw, other)
}

test semantic_serialise_data_refuses_a_revealed_serialisation_of_other_data() fail {
  let other = cbor.serialise(builtin.constr_data(0, [builtin.i_data(1)]))
  serialise_data_verifies(wide_data(), other, other)
}

fn serialise_data_split_arm(source: Data) -> Bool {
  let raw = cbor.serialise(source)
  cek_builtin_v1.verify_semantic_data_misc(
    51,
    [constant(#"9f08ff", raw)],
    bytes(cbor.serialise(builtin.b_data(raw))),
    cek_builtin_v1.SemanticBuiltinWitnessV1 {
      data_nodes: [],
      list_nodes: [],
      pair_nodes: [],
      scalar_preimages: [raw],
    },
  )
}

// The production split arm alone, on the wide Data above.
test semantic_data_misc_serialises_wide_data() {
  serialise_data_split_arm(wide_data())
}

// The production split arm alone, on Data of the same shape within 64 bits.
test semantic_data_misc_serialises_data_within_64_bits() {
  serialise_data_split_arm(
    builtin.constr_data(
      0,
      [
        builtin.constr_data(127, [builtin.i_data(1)]),
        builtin.constr_data(6, []),
        builtin.i_data(18446744073709551615),
        builtin.i_data(-18446744073709551616),
      ],
    ),
  )
}

// Integer constants on both sides of the 64-bit head boundary, read by the
// direct addInteger arm. Each sum crosses from a major-type head to a bignum.
fn add_integer_verifies(left: Int, right: Int, sum: Int) -> Bool {
  let arguments = [integer_value(left), integer_value(right)]
  verify_direct_with_split_parity(0, arguments, integer_value(sum))
}

test add_integer_reads_constants_across_the_64_bit_boundary() {
  expect
    cbor.serialise(builtin.i_data(18446744073709551615)) == #"1bffffffffffffffff"
  expect
    cbor.serialise(builtin.i_data(18446744073709551616)) == #"c249010000000000000000"
  expect
    cbor.serialise(builtin.i_data(-18446744073709551616)) == #"3bffffffffffffffff"
  expect
    cbor.serialise(builtin.i_data(-18446744073709551617)) == #"c349010000000000000000"
  and {
    add_integer_verifies(18446744073709551615, 1, 18446744073709551616),
    add_integer_verifies(18446744073709551616, -1, 18446744073709551615),
    add_integer_verifies(-18446744073709551616, -1, -18446744073709551617),
    add_integer_verifies(-18446744073709551617, 1, -18446744073709551616),
  }
}

// No claimed sum pairs -2^64 with -2^64 - 1: the pinned evaluator's Data
// equality does not separate them.
test add_integer_refuses_a_wrong_sum_across_the_64_bit_boundary() {
  and {
    !add_integer_verifies(18446744073709551615, 1, 18446744073709551615),
    !add_integer_verifies(18446744073709551616, -1, 18446744073709551616),
    !add_integer_verifies(-18446744073709551616, -1, -18446744073709551618),
    !add_integer_verifies(-18446744073709551617, 1, -18446744073709551615),
  }
}

// The IntegerData leaf over the CBOR `raw`, rooted by its flat root (the
// bounded blob root up to 9,215 bytes).
fn integer_leaf_node(memory: Int, raw: ByteArray) -> cek_data_v1.DataNodeV1 {
  cek_data_v1.IntegerData {
    cbor_root: flat_root_v1(raw),
    cbor_length: bytearray.length(raw),
    memory,
  }
}

fn node_summary(node: cek_data_v1.DataNodeV1) -> cek_data_v1.DataSummaryV1 {
  cek_data_v1.DataSummaryV1 {
    root: cek_data_v1.hash_data_node_v1(node),
    cbor_length: cek_data_v1.data_node_cbor_length_v1(node),
    memory: cek_data_v1.data_node_memory_v1(node),
  }
}

// The material of iData and unIData: the leaf node alone.
fn leaf_witness(
  node: cek_data_v1.DataNodeV1,
) -> cek_builtin_v1.SemanticBuiltinWitnessV1 {
  cek_builtin_v1.SemanticBuiltinWitnessV1 {
    data_nodes: [node],
    list_nodes: [],
    pair_nodes: [],
    scalar_preimages: [],
  }
}

// unIData over a semantic Data value whose only node is the leaf.
fn un_i_data_leaf_verifies(value: Int, raw: ByteArray) -> Bool {
  un_i_data_leaf_with_memory_verifies(
    4 + cek_constant_v1.integer_memory_size_v1(value),
    raw,
  )
}

fn un_i_data_leaf_with_memory_verifies(memory: Int, raw: ByteArray) -> Bool {
  let node = integer_leaf_node(memory, raw)
  let summary = node_summary(node)
  let source = cek_builtin_v1.SemanticConstantValue {
    type_cbor: #"9f08ff",
    payload: summary,
    memory: summary.memory,
  }
  let result = cek_builtin_v1.SemanticConstantValue {
    type_cbor: #"9f00ff",
    payload: summary,
    memory: summary.memory - 4,
  }
  verify_semantic_with_split_parity(45, [source], result, leaf_witness(node))
}

// iData from an integer constant: the leaf must be that integer's leaf.
fn i_data_leaf_verifies(value: Int, raw: ByteArray) -> Bool {
  let argument = integer_value(value)
  let payload = cek_data_v1.integer_data_summary_v1(
    value,
    cek_proof_v1.bounded_blob_root_v1(raw),
  )
  let node = integer_leaf_node(
    4 + cek_constant_v1.integer_memory_size_v1(value),
    raw,
  )
  verify_semantic_with_split_parity(
    40,
    [argument],
    cek_builtin_v1.SemanticConstantValue {
      type_cbor: #"9f08ff",
      payload,
      memory: payload.memory,
    },
    leaf_witness(node),
  )
}

fn integer_leaf_verifies(value: Int, raw: ByteArray) -> Bool {
  expect cbor.serialise(builtin.i_data(value)) == raw
  and {
    un_i_data_leaf_verifies(value, raw),
    i_data_leaf_verifies(value, raw),
  }
}

test integer_leaf_with_largest_unsigned_head_verifies() {
  integer_leaf_verifies(18446744073709551615, #"1bffffffffffffffff")
}

test integer_leaf_with_smallest_positive_bignum_verifies() {
  integer_leaf_verifies(18446744073709551616, #"c249010000000000000000")
}

test integer_leaf_with_largest_negative_head_verifies() {
  integer_leaf_verifies(-18446744073709551616, #"3bffffffffffffffff")
}

test integer_leaf_with_smallest_negative_bignum_verifies() {
  integer_leaf_verifies(-18446744073709551617, #"c349010000000000000000")
}

// The leaf must be the integer constant's own leaf.
test integer_leaf_for_a_different_integer_fails_closed() fail {
  i_data_leaf_verifies(18446744073709551616, #"c349010000000000000000")
}

// unIData reads the leaf's committed fields only. That every I leaf is over
// canonical CBOR (so never behind a long tag 2 or tag 3 head) is checked where
// leaves are created: `cek-integer-leaf-sites.test.ak`.

// serialiseData over constructor 127 (tag 1400) and 128 (tag 102), through the
// production split arm.
test semantic_data_misc_serialises_constructor_127() {
  expect cbor.serialise(builtin.constr_data(127, [])) == #"d9057880"
  serialise_data_split_arm(builtin.constr_data(127, []))
}

test semantic_data_misc_serialises_constructor_128() {
  expect cbor.serialise(builtin.constr_data(128, [])) == #"d86682188080"
  serialise_data_split_arm(builtin.constr_data(128, []))
}

fn serialise_data_split_arm_reveals(source: Data, revealed: ByteArray) -> Bool {
  cek_builtin_v1.verify_semantic_data_misc(
    51,
    [constant(#"9f08ff", cbor.serialise(source))],
    bytes(cbor.serialise(builtin.b_data(revealed))),
    cek_builtin_v1.SemanticBuiltinWitnessV1 {
      data_nodes: [],
      list_nodes: [],
      pair_nodes: [],
      scalar_preimages: [revealed],
    },
  )
}

test semantic_data_misc_refuses_a_long_tag_2_head() fail {
  serialise_data_split_arm_reveals(
    builtin.i_data(18446744073709551616),
    #"d80249010000000000000000",
  )
}

test semantic_data_misc_refuses_constructor_102_with_a_negative_index() fail {
  serialise_data_split_arm_reveals(builtin.constr_data(0, []), #"d866822080")
}

test semantic_data_misc_refuses_constructor_102_with_a_bignum_index() fail {
  serialise_data_split_arm_reveals(
    builtin.constr_data(1, []),
    #"d86682c2410180",
  )
}

// The largest integers within the direct payload bound (9,215 bytes): 8,932
// magnitude bytes.
test integer_leaf_at_the_direct_payload_bound_verifies() {
  let largest = math.pow2(71456) - 1
  let positive = cbor.serialise(builtin.i_data(largest))
  let negative = cbor.serialise(builtin.i_data(-largest - 1))
  expect bytearray.length(positive) == 9215
  expect bytearray.length(negative) == 9215
  and {
    un_i_data_leaf_verifies(largest, positive),
    un_i_data_leaf_verifies(-largest - 1, negative),
  }
}

// One magnitude byte more has no bounded blob root; its leaf is rooted by the
// flat root, and unIData reads it the same way.
test integer_leaf_beyond_the_direct_payload_bound_has_no_bounded_root() fail {
  let raw = cbor.serialise(builtin.i_data(math.pow2(71456)))
  bytearray.length(cek_proof_v1.bounded_blob_root_v1(raw)) == 32
}

test integer_leaf_beyond_the_direct_payload_bound_verifies() {
  let positive = cbor.serialise(builtin.i_data(math.pow2(71456)))
  let negative = cbor.serialise(builtin.i_data(-math.pow2(71456) - 1))
  expect bytearray.length(positive) == 9216
  expect bytearray.length(negative) == 9216
  and {
    un_i_data_leaf_verifies(math.pow2(71456), positive),
    un_i_data_leaf_verifies(-math.pow2(71456) - 1, negative),
  }
}

// equalsData compares Data by value. -6 is 25 and -5 is 24.
fn equals_data_verifies(
  left: ByteArray,
  right: ByteArray,
  claimed: ByteArray,
) -> Bool {
  let arguments = [constant(#"9f08ff", left), constant(#"9f08ff", right)]
  verify_semantic_with_split_parity(
    47,
    arguments,
    boolean(claimed),
    cek_builtin_v1.SemanticBuiltinWitnessV1 {
      data_nodes: [],
      list_nodes: [],
      pair_nodes: [],
      scalar_preimages: [],
    },
  )
}

test equals_data_matches_an_integer_with_itself() {
  equals_data_verifies(#"25", #"25", #"d87a80")
}

test equals_data_separates_minus_six_from_minus_five() {
  equals_data_verifies(#"25", #"24", #"d87980")
}

test equals_data_refuses_a_wrong_verdict() {
  and {
    !equals_data_verifies(#"25", #"25", #"d87980"),
    !equals_data_verifies(#"25", #"24", #"d87a80"),
  }
}

// The step compares Data by value on both sides of the 64-bit head boundary.
test equals_data_separates_minus_two_to_the_64_from_its_predecessor() {
  and {
    equals_data_verifies(
      #"3bffffffffffffffff",
      #"c349010000000000000000",
      #"d87980",
    ),
    !equals_data_verifies(
      #"3bffffffffffffffff",
      #"c349010000000000000000",
      #"d87a80",
    ),
  }
}

// SH-2: the BLS builtins carry Cardano's flat tags (Plutus `Builtins.hs`):
// 58/65 compress, 59/66 uncompress, 60/67 hashToGroup. The hashToGroup
// points are RFC 9380 hash_to_curve of "midgard" under the BLS signature
// suites' DSTs, computed independently of this module.
fn h2g_message() -> cek_builtin_v1.ValueWitnessV1 {
  bytes(#"476d696467617264")
}

test bls_g1_builtins_follow_cardano_flat_tags() {
  let compressed = bytes(
    #"583097f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb",
  )
  let point = cek_builtin_v1.ConstantValue(g1_generator())
  let h2g_arguments = [
    h2g_message(),
    bytes(
      #"582b424c535f5349475f424c53313233383147315f584d443a5348412d3235365f535357555f524f5f4e554c5f",
    ),
  ]
  let hashed = constant(
    #"9f09ff",
    #"58308148482e08a92312cbd0a869e03ced121bf66f29306e281b0f894701f5e3f1242fa6cfdd3afbb2620cb9bfc50ed7dd52",
  )
  and {
    verify_direct_with_split_parity(58, [point], compressed),
    verify_direct_with_split_parity(59, [compressed], point),
    verify_direct_with_split_parity(60, h2g_arguments, hashed),
  }
}

test bls_g2_builtins_follow_cardano_flat_tags() {
  let compressed = bytes(
    #"5f584093e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc510515820c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8ff",
  )
  let point = cek_builtin_v1.ConstantValue(g2_generator())
  let h2g_arguments = [
    h2g_message(),
    bytes(
      #"582b424c535f5349475f424c53313233383147325f584d443a5348412d3235365f535357555f524f5f4e554c5f",
    ),
  ]
  let hashed = constant(
    #"9f0aff",
    #"5f584089ac93813e1f13d1f1a75d2c790cbc435330bdfcaa9f42a609aa5563b8bab0edbc269b186eb3c3c9a6bdf9fd525c34e7167f474f0e648c0c3eeab5df3eebf3ff58207c39f72addeb2d3368a44d56f5dca28fca97fe1184b365e3c8a14e226b6188b1ff",
  )
  and {
    verify_direct_with_split_parity(65, [point], compressed),
    verify_direct_with_split_parity(66, [compressed], point),
    verify_direct_with_split_parity(67, h2g_arguments, hashed),
  }
}

// The library's numbering (58 hashToGroup, 59 compress) is not Cardano's: a
// G1 compress claimed at 59 is refused.
test bls_g1_compress_is_refused_at_the_library_tag() fail {
  let compressed = bytes(
    #"583097f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb",
  )
  let point = cek_builtin_v1.ConstantValue(g1_generator())
  cek_builtin_v1.verify_direct_builtin_v1(59, [point], compressed)
}

// Variant E bounds a CInteger argument to [-2^262143, 2^262143 - 1] and a
// CByteString argument to 65,536 B; out of bound, Cardano fails the call. Every
// success arm of a bounded builtin reads its operands from revealed payloads
// under the 9,215 B cap, so one past either bound is refused at that cap and
// never certified. An arm that admits operands beyond the cap must bound them
// itself. The roots are left empty: the cap is checked before them, and a
// bignum payload cannot be decoded by today's reader.

// integerToByteString stops at 8,192 B, so longer zero runs are appended.
fn zero_run(length: Int) -> ByteArray {
  if length <= 8192 {
    bytearray.from_int_big_endian(0, length)
  } else {
    bytearray.concat(
      bytearray.from_int_big_endian(0, 8192),
      zero_run(length - 8192),
    )
  }
}

// 0x80 followed by 32,767 zero bytes: 2^(8 * 32767 + 7) = 2^262143.
fn power_of_two_262143() -> Int {
  bytearray.to_int_big_endian(bytearray.push(zero_run(32767), 128))
}

fn out_of_bound_integer_addition(operand: Int) -> Bool {
  let arguments = [
    integer(cbor.serialise(builtin.i_data(operand))),
    integer(cbor.serialise(builtin.i_data(0))),
  ]
  cek_builtin_v1.verify_direct_builtin_v1(
    0,
    arguments,
    integer(cbor.serialise(builtin.i_data(operand))),
  )
}

test add_integer_success_is_refused_one_past_the_upper_bound() fail {
  out_of_bound_integer_addition(power_of_two_262143())
}

test add_integer_success_is_refused_one_past_the_lower_bound() fail {
  out_of_bound_integer_addition(-power_of_two_262143() - 1)
}

test append_bytes_success_is_refused_one_past_the_bound() fail {
  let left = cbor.serialise(builtin.b_data(zero_run(65537)))
  let arguments = [bytes(left), bytes(cbor.serialise(builtin.b_data(#"")))]
  cek_builtin_v1.verify_direct_builtin_v1(10, arguments, bytes(left))
}

// ECDSA takes a 33 B key, a 32 B message and a 64 B signature; any other
// length fails the call (PV 11 leaves secp256k1 unchanged). The failure arm
// certifies each such call, and the success arm aborts on it.
fn ecdsa_with_lengths(
  key: ByteArray,
  message: ByteArray,
  signature: ByteArray,
) -> List<cek_builtin_v1.ValueWitnessV1> {
  [
    bytes(cbor.serialise(builtin.b_data(key))),
    bytes(cbor.serialise(builtin.b_data(message))),
    bytes(cbor.serialise(builtin.b_data(signature))),
  ]
}

const ecdsa_x =
  #"79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"

fn ecdsa_message() -> ByteArray {
  bytearray.from_int_big_endian(0, 32)
}

fn ecdsa_signature() -> ByteArray {
  bytearray.concat(
    builtin.integer_to_bytearray(True, 32, 1),
    builtin.integer_to_bytearray(True, 32, 1),
  )
}

fn uncompressed_key_arguments() -> List<cek_builtin_v1.ValueWitnessV1> {
  let y = #"483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8"
  ecdsa_with_lengths(
    bytearray.push(bytearray.concat(ecdsa_x, y), 4),
    ecdsa_message(),
    ecdsa_signature(),
  )
}

test ecdsa_length_edges_are_certified_failures() {
  let key = bytearray.push(ecdsa_x, 2)
  let message = ecdsa_message()
  let signature = ecdsa_signature()
  list.all(
    [
      uncompressed_key_arguments(),
      ecdsa_with_lengths(key, bytearray.drop(message, 1), signature),
      ecdsa_with_lengths(key, bytearray.push(message, 0), signature),
      ecdsa_with_lengths(key, message, bytearray.drop(signature, 1)),
      ecdsa_with_lengths(key, message, bytearray.push(signature, 0)),
    ],
    fn(arguments) {
      cek_builtin_v1.verify_direct_builtin_failure_v1(
        52,
        builtin_root(52, arguments),
        arguments,
      )
    },
  )
}

test ecdsa_uncompressed_key_success_aborts() fail {
  cek_builtin_v1.verify_direct_scalar_semantics(
    52,
    uncompressed_key_arguments(),
    boolean(#"d87980"),
  )
}

fn byte_string(value: ByteArray) -> cek_builtin_v1.ValueWitnessV1 {
  bytes(cbor.serialise(builtin.b_data(value)))
}

fn counting_bytes(length: Int) -> ByteArray {
  list.foldr(
    list.range(0, length - 1),
    #"",
    fn(byte, tail) { builtin.cons_bytearray(byte, tail) },
  )
}

/// BIP-340 test vector key (bitcoin/bips bip-0340/test-vectors.csv, 15-18).
const schnorr_key =
  #"778caa53b4393ac467774d09497a87224bf9fab6f6e68b23086497324d6fd117"

/// Valid BIP-340 signatures under `schnorr_key`, as message and signature.
/// Vector 15: the empty message.
fn schnorr_signed_empty() -> Pair<ByteArray, ByteArray> {
  Pair(
    #"",
    #"71535db165ecd9fbbc046e5ffaea61186bb6ad436732fccc25291a55895464cf6069ce26bf03466228f19a3a62db8a649f2d560fac652827d1af0574e427ab63",
  )
}

/// Signed with the vectors' secret key and an all-zero aux_rand.
fn schnorr_signed_31() -> Pair<ByteArray, ByteArray> {
  Pair(
    counting_bytes(31),
    #"6279da0776f05a84e74bd0400f577f22de28eab33c6cd0a69319b9097e912d4360c3c32b1252eb9b55186684198886abfceaf124f57b2088e809133ddd24d34a",
  )
}

/// Signed with the vectors' secret key and an all-zero aux_rand.
fn schnorr_signed_33() -> Pair<ByteArray, ByteArray> {
  Pair(
    counting_bytes(33),
    #"fe02a88f7b6ad46d2a02f678741b1e03ff93ce930235a2c63f644e0d4ee9129d6fdb9aef937e53a826791b584e2392c6135386cca3009b31c7cab9cd3475e7d7",
  )
}

/// Vector 18: 100 bytes of 0x99.
fn schnorr_signed_100() -> Pair<ByteArray, ByteArray> {
  Pair(
    repeated_byte(153, 100),
    #"403b12b0d8555a344175ea7ec746566303321e5dbfa8be6f091635163eca79a8585ed3e3170807e7c03b720fc54c7b23897fcba0e9d0b4a06894cfd249f22367",
  )
}

/// 1,000 bytes of 0x99, signed with the vectors' secret key and an all-zero
/// aux_rand.
fn schnorr_signed_1000() -> Pair<ByteArray, ByteArray> {
  Pair(
    repeated_byte(153, 1000),
    #"f1d320d7de4d5ff365a8ce6cf0ad580b9358f96f46dfbe141e22128b75919c3e47b4420db40806ad1cf6bf0fcade8a2d51634dedb7e118e9ae896dd4d6a504bb",
  )
}

/// Vectors 15 to 18 (messages of 0, 1, 17 and 100 bytes) and signatures over
/// 31, 33 and 1,000 bytes.
fn schnorr_signed_messages() -> List<Pair<ByteArray, ByteArray>> {
  [
    schnorr_signed_empty(),
    Pair(
      #"11",
      #"08a20a0afef64124649232e0693c583ab1b9934ae63b4c3511f3ae1134c6a303ea3173bfea6683bd101fa5aa5dbc1996fe7cacfc5a577d33ec14564cec2bacbf",
    ),
    Pair(
      #"0102030405060708090a0b0c0d0e0f1011",
      #"5130f39a4059b43bc7cac09a19ece52b5d8699d1a71e3c52da9afdb6b50ac370c4a482b77bf960f8681540e25b6771ece1e5a37fd80e5a51897c5566a97ea5a5",
    ),
    schnorr_signed_31(),
    schnorr_signed_33(),
    schnorr_signed_100(),
    schnorr_signed_1000(),
  ]
}

fn schnorr_arguments(
  key: ByteArray,
  message: ByteArray,
  signature: ByteArray,
) -> List<cek_builtin_v1.ValueWitnessV1> {
  [byte_string(key), byte_string(message), byte_string(signature)]
}

test schnorr_verifies_messages_of_any_length() {
  list.all(
    schnorr_signed_messages(),
    fn(signed) {
      let arguments = schnorr_arguments(schnorr_key, signed.1st, signed.2nd)
      and {
        cek_builtin_v1.verify_direct_scalar_semantics(
          53,
          arguments,
          boolean(#"d87a80"),
        ),
        !cek_builtin_v1.verify_direct_builtin_failure_v1(
          53,
          builtin_root(53, arguments),
          arguments,
        ),
      }
    },
  )
}

test schnorr_signature_over_another_message_returns_false() {
  let arguments = schnorr_arguments(
    schnorr_key,
    schnorr_signed_33().1st,
    schnorr_signed_31().2nd,
  )
  and {
    cek_builtin_v1.verify_direct_scalar_semantics(
      53,
      arguments,
      boolean(#"d87980"),
    ),
    !cek_builtin_v1.verify_direct_scalar_semantics(
      53,
      arguments,
      boolean(#"d87a80"),
    ),
    !cek_builtin_v1.verify_direct_builtin_failure_v1(
      53,
      builtin_root(53, arguments),
      arguments,
    ),
  }
}

test schnorr_1000_byte_message_under_another_signature_returns_false() {
  let arguments = schnorr_arguments(
    schnorr_key,
    schnorr_signed_1000().1st,
    schnorr_signed_100().2nd,
  )
  and {
    cek_builtin_v1.verify_direct_scalar_semantics(
      53,
      arguments,
      boolean(#"d87980"),
    ),
    !cek_builtin_v1.verify_direct_scalar_semantics(
      53,
      arguments,
      boolean(#"d87a80"),
    ),
    !cek_builtin_v1.verify_direct_builtin_failure_v1(
      53,
      builtin_root(53, arguments),
      arguments,
    ),
  }
}

test schnorr_1000_byte_message_cannot_claim_false() fail {
  let Pair(message, signature) = schnorr_signed_1000()
  cek_builtin_v1.verify_direct_scalar_semantics(
    53,
    schnorr_arguments(schnorr_key, message, signature),
    boolean(#"d87980"),
  )
}

test schnorr_charges_message_words() {
  let Pair(empty, empty_signature) = schnorr_signed_empty()
  let Pair(hundred, hundred_signature) = schnorr_signed_100()
  let Pair(thousand, thousand_signature) = schnorr_signed_1000()
  and {
    cek_builtin_v1.direct_builtin_budget_v1(
      53,
      schnorr_arguments(schnorr_key, empty, empty_signature),
    ) == cek_cost_v1.BuiltinBudgetV1 { cpu: 43600591, memory: 10 },
    cek_builtin_v1.direct_builtin_budget_v1(
      53,
      schnorr_arguments(schnorr_key, hundred, hundred_signature),
    ) == cek_cost_v1.BuiltinBudgetV1 { cpu: 43916287, memory: 10 },
    cek_builtin_v1.direct_builtin_budget_v1(
      53,
      schnorr_arguments(schnorr_key, thousand, thousand_signature),
    ) == cek_cost_v1.BuiltinBudgetV1 { cpu: 46862783, memory: 10 },
  }
}

test schnorr_wrong_key_or_signature_length_fails_at_any_message_length() {
  let key = schnorr_key
  list.all(
    schnorr_signed_messages(),
    fn(signed) {
      let signature = signed.2nd
      list.all(
        [
          schnorr_arguments(
            builtin.slice_bytearray(0, 31, key),
            signed.1st,
            signature,
          ),
          schnorr_arguments(
            builtin.cons_bytearray(0, key),
            signed.1st,
            signature,
          ),
          schnorr_arguments(
            key,
            signed.1st,
            builtin.slice_bytearray(0, 63, signature),
          ),
          schnorr_arguments(
            key,
            signed.1st,
            builtin.cons_bytearray(0, signature),
          ),
        ],
        fn(arguments) {
          cek_builtin_v1.verify_direct_builtin_failure_v1(
            53,
            builtin_root(53, arguments),
            arguments,
          )
        },
      )
    },
  )
}

test schnorr_short_key_cannot_claim_a_successful_result() fail {
  let Pair(hundred, hundred_signature) = schnorr_signed_100()
  cek_builtin_v1.verify_direct_scalar_semantics(
    53,
    schnorr_arguments(
      builtin.slice_bytearray(0, 31, schnorr_key),
      hundred,
      hundred_signature,
    ),
    boolean(#"d87980"),
  )
}

test schnorr_ill_typed_message_is_not_a_known_failure() fail {
  let arguments = [
    byte_string(builtin.slice_bytearray(0, 31, schnorr_key)),
    integer(#"01"),
    byte_string(#""),
  ]
  cek_builtin_v1.verify_direct_builtin_failure_v1(
    53,
    builtin_root(53, arguments),
    arguments,
  )
}

test ed25519_ill_typed_message_is_not_a_known_failure() fail {
  let arguments = [byte_string(#""), integer(#"01"), byte_string(#"")]
  cek_builtin_v1.verify_direct_builtin_failure_v1(
    21,
    builtin_root(21, arguments),
    arguments,
  )
}

// Variant E also bounds three operands the 9,215 B cap leaves open: a writeBits
// input at 4,096 B, a shift or rotate amount at Int64 (the bound the failure
// arm proves), and a BLS12-381 scalarMul scalar at [-2^4095, 2^4095 - 1]. Each
// success arm certifies at its bound and is refused one past it, with a result
// that would be right if the operand were not refused.

// Setting bit 0 of `length` zero bytes, written out without writeBits.
fn write_bit_zero_succeeds_at(length: Int) -> Bool {
  let source = cbor.serialise(builtin.b_data(zero_run(length)))
  let indices = cbor.serialise(builtin.list_data([builtin.i_data(0)]))
  let arguments = [
    bytes(source),
    constant(#"9f0500ff", indices),
    boolean(#"d87a80"),
  ]
  let written = bytearray.concat(zero_run(length - 1), #"01")
  cek_builtin_v1.verify_direct_builtin_v1(
    80,
    arguments,
    bytes(cbor.serialise(builtin.b_data(written))),
  )
}

test write_bits_success_certifies_a_4096_byte_input() {
  write_bit_zero_succeeds_at(4096)
}

test write_bits_success_is_refused_one_past_4096_bytes() fail {
  write_bit_zero_succeeds_at(4097)
}

// Shifting one byte by eight bits or more leaves zero; rotating 0xff leaves it.
fn shift_or_rotate_succeeds(tag: Int, amount: Int) -> Bool {
  let source = if tag == 82 {
    #"01"
  } else {
    #"ff"
  }
  let shifted = if tag == 82 {
    #"00"
  } else {
    #"ff"
  }
  let arguments = [
    bytes(cbor.serialise(builtin.b_data(source))),
    integer(cbor.serialise(builtin.i_data(amount))),
  ]
  cek_builtin_v1.verify_direct_builtin_v1(
    tag,
    arguments,
    bytes(cbor.serialise(builtin.b_data(shifted))),
  )
}

test shift_and_rotate_success_certifies_int64_amounts() {
  and {
    shift_or_rotate_succeeds(82, 9223372036854775807),
    shift_or_rotate_succeeds(82, -9223372036854775808),
    shift_or_rotate_succeeds(83, 9223372036854775807),
    shift_or_rotate_succeeds(83, -9223372036854775808),
  }
}

test shift_success_is_refused_one_past_int64_max() fail {
  shift_or_rotate_succeeds(82, 9223372036854775808)
}

test shift_success_is_refused_one_past_int64_min() fail {
  shift_or_rotate_succeeds(82, -9223372036854775809)
}

test rotate_success_is_refused_one_past_int64_max() fail {
  shift_or_rotate_succeeds(83, 9223372036854775808)
}

test rotate_success_is_refused_one_past_int64_min() fail {
  shift_or_rotate_succeeds(83, -9223372036854775809)
}

// 0x80 followed by 511 zero bytes: 2^(8 * 511 + 7) = 2^4095.
fn power_of_two_4095() -> Int {
  bytearray.to_int_big_endian(bytearray.push(zero_run(511), 128))
}

test bls_scalar_bound_is_variant_e_bound() {
  and {
    cek_builtin_v1.bls_scalar_in_bound_v1(power_of_two_4095() - 1),
    cek_builtin_v1.bls_scalar_in_bound_v1(-power_of_two_4095()),
    !cek_builtin_v1.bls_scalar_in_bound_v1(power_of_two_4095()),
    !cek_builtin_v1.bls_scalar_in_bound_v1(-power_of_two_4095() - 1),
  }
}

// A scalar past 64 bits is a CBOR bignum, which today's constant reader cannot
// decode (the pinned evaluator aborts on it), so the arm is driven here with
// small scalars only; the bound itself is pinned above.
fn scalar_mul_succeeds(tag: Int, scalar: Int) -> Bool {
  let Pair(point, product) = if tag == 56 {
    let generator = g1_generator()
    Pair(
      generator,
      builtin.bls12_381_g1_scalar_mul(
        scalar,
        builtin.un_b_data(cbor_payload(generator.payload_cbor))
          |> builtin.bls12_381_g1_uncompress,
      )
        |> builtin.bls12_381_g1_compress,
    )
  } else {
    let generator = g2_generator()
    Pair(
      generator,
      builtin.bls12_381_g2_scalar_mul(
        scalar,
        builtin.un_b_data(cbor_payload(generator.payload_cbor))
          |> builtin.bls12_381_g2_uncompress,
      )
        |> builtin.bls12_381_g2_compress,
    )
  }
  let arguments = [
    integer(cbor.serialise(builtin.i_data(scalar))),
    cek_builtin_v1.ConstantValue(point),
  ]
  cek_builtin_v1.verify_direct_builtin_v1(
    tag,
    arguments,
    constant(point.type_cbor, cbor.serialise(builtin.b_data(product))),
  )
}

fn cbor_payload(payload_cbor: ByteArray) -> Data {
  expect Some(payload) = cbor.deserialise(payload_cbor)
  payload
}

test scalar_mul_success_certifies_in_bound_scalars() {
  and {
    scalar_mul_succeeds(56, 7),
    scalar_mul_succeeds(56, -7),
    scalar_mul_succeeds(63, 7),
    scalar_mul_succeeds(63, -7),
  }
}

// Each builtin's own charge at the 64-bit and constructor-128 boundaries:
// Cardano's PV 11 total for the program in
// cek-program.cardano-boundaries.vectors.ts, less the 100/100 start-up and
// its 16000/100 machine steps (five for a two-argument builtin, three for a
// one-argument one). An integer is two 64-bit words exactly from |x| = 2^64.
fn charges(
  tag: Int,
  arguments: List<cek_builtin_v1.ValueWitnessV1>,
  cpu: Int,
  memory: Int,
) -> Bool {
  cek_builtin_v1.direct_builtin_budget_v1(tag, arguments) == cek_cost_v1.BuiltinBudgetV1 {
    cpu,
    memory,
  }
}

fn data_value(value: Data) -> cek_builtin_v1.ValueWitnessV1 {
  constant(#"9f08ff", cbor.serialise(value))
}

test add_integer_charges_cardano_words_across_the_64_bit_boundary() {
  and {
    charges(
      0,
      [integer_value(18446744073709551615), integer_value(1)],
      101208,
      2,
    ),
    charges(
      0,
      [integer_value(18446744073709551615), integer_value(0)],
      101208,
      2,
    ),
    charges(
      0,
      [integer_value(18446744073709551616), integer_value(-1)],
      101628,
      3,
    ),
    charges(
      0,
      [integer_value(-18446744073709551616), integer_value(-1)],
      101628,
      3,
    ),
    charges(
      0,
      [integer_value(-18446744073709551616), integer_value(0)],
      101628,
      3,
    ),
    charges(
      0,
      [integer_value(-18446744073709551617), integer_value(1)],
      101628,
      3,
    ),
  }
}

test equals_data_charges_cardano_words_across_the_64_bit_boundary() {
  and {
    charges(
      47,
      [
        data_value(builtin.i_data(-18446744073709551616)),
        data_value(builtin.i_data(-18446744073709551617)),
      ],
      1061822,
      1,
    ),
    charges(
      47,
      [
        data_value(builtin.i_data(18446744073709551616)),
        data_value(builtin.i_data(18446744073709551616)),
      ],
      1061822,
      1,
    ),
    charges(
      47,
      [
        data_value(builtin.i_data(18446744073709551615)),
        data_value(builtin.i_data(18446744073709551616)),
      ],
      1034543,
      1,
    ),
    charges(
      47,
      [
        data_value(builtin.constr_data(127, [])),
        data_value(builtin.constr_data(128, [])),
      ],
      1007264,
      1,
    ),
  }
}

test serialise_data_charges_cardano_words_at_constructor_128() {
  and {
    charges(51, [data_value(builtin.constr_data(128, []))], 1808754, 8),
    charges(
      51,
      [
        data_value(
          builtin.constr_data(128, [builtin.i_data(18446744073709551616)]),
        ),
      ],
      3088626,
      20,
    ),
    charges(
      51,
      [
        data_value(
          builtin.constr_data(127, [builtin.i_data(18446744073709551615)]),
        ),
      ],
      2875314,
      18,
    ),
  }
}

test un_i_data_charges_a_constant_across_the_64_bit_boundary() {
  and {
    charges(45, [data_value(builtin.i_data(18446744073709551615))], 20744, 32),
    charges(45, [data_value(builtin.i_data(18446744073709551616))], 20744, 32),
    charges(45, [data_value(builtin.i_data(-18446744073709551616))], 20744, 32),
    charges(45, [data_value(builtin.i_data(-18446744073709551617))], 20744, 32),
  }
}

// unConstrData of constructor 128 (tag 102) and of 127 (tag 1400), then
// fstPair: Cardano returns the index. The program
// `[(force (force (builtin fstPair))) [(builtin unConstrData) (con data _)]]`
// costs 278583/864, of which seven machine steps and the start-up are
// 112100/800.
fn un_constr_data_pair(index: Int) -> cek_builtin_v1.ValueWitnessV1 {
  constant(
    #"9f06000508ff",
    cbor.serialise(
      builtin.constr_data(0, [builtin.i_data(index), builtin.list_data([])]),
    ),
  )
}

fn un_constr_data_then_fst_pair_verifies(index: Int) -> Bool {
  let source = [data_value(builtin.constr_data(index, []))]
  let pair = [un_constr_data_pair(index)]
  let unconstr = cek_builtin_v1.direct_builtin_budget_v1(42, source)
  let fst = cek_builtin_v1.direct_builtin_budget_v1(29, pair)
  and {
    verify_direct_with_split_parity(42, source, un_constr_data_pair(index)),
    verify_direct_with_split_parity(29, pair, integer_value(index)),
    unconstr.cpu + fst.cpu == 166483,
    unconstr.memory + fst.memory == 64,
  }
}

test un_constr_data_reads_constructors_127_and_128() {
  expect cbor.serialise(builtin.constr_data(128, [])) == #"d86682188080"
  expect cbor.serialise(builtin.i_data(128)) == #"1880"
  and {
    un_constr_data_then_fst_pair_verifies(127),
    un_constr_data_then_fst_pair_verifies(128),
  }
}

test un_constr_data_refuses_a_wrong_index_at_constructor_128() {
  let source = [data_value(builtin.constr_data(128, []))]
  !verify_direct_with_split_parity(42, source, un_constr_data_pair(127))
}

test un_constr_data_refuses_constructor_102_with_a_negative_index() fail {
  let source = [constant(#"9f08ff", #"d866822080")]
  verify_direct_with_split_parity(42, source, un_constr_data_pair(0))
}

test un_constr_data_refuses_constructor_102_with_a_bignum_index() fail {
  let source = [constant(#"9f08ff", #"d86682c2410180")]
  verify_direct_with_split_parity(42, source, un_constr_data_pair(1))
}

// mkCons over a list item presented by its semantic summary. The summary
// memory is not in the item's value root, so the arm binds it to the item's
// constant memory by element type; any other element type must be revealed.
type ListItemFixture {
  list_type: ByteArray,
  item: cek_builtin_v1.ValueWitnessV1,
  source: cek_builtin_v1.ValueWitnessV1,
  tail: cek_data_v1.DataSequenceSummaryV1,
  material: cek_builtin_v1.SemanticBuiltinWitnessV1,
}

fn list_item_fixture(
  element_cbor: ByteArray,
  item: Data,
  items: List<Data>,
) -> ListItemFixture {
  let source_cbor = cbor.serialise(builtin.list_data(items))
  let tail = cek_data_v1.commit_data_list(items)
  let list_nodes = when items is {
    [] -> []
    [head, ..rest] -> {
      let head_summary = cek_data_v1.semantic_data_summary_v1(head)
      [
        cek_data_v1.DataListNodeV1 {
          head: head_summary.root,
          head_cbor_length: head_summary.cbor_length,
          head_memory: head_summary.memory,
          tail: cek_data_v1.commit_data_list(rest).root,
          length: tail.length,
          payload_cbor_length: tail.payload_cbor_length,
          memory: tail.memory,
        },
      ]
    }
  }
  let list_type = #"9f05"
    |> bytearray.concat(element_cbor)
    |> bytearray.concat(#"ff")
  ListItemFixture {
    list_type,
    item: constant(
      #"9f" |> bytearray.concat(element_cbor) |> bytearray.concat(#"ff"),
      cbor.serialise(item),
    ),
    source: constant(list_type, source_cbor),
    tail,
    material: cek_builtin_v1.SemanticBuiltinWitnessV1 {
      data_nodes: [
        cek_data_v1.ListData {
          items_count: tail.length,
          items_root: tail.root,
          cbor_length: bytearray.length(source_cbor),
          memory: 4 + tail.memory,
        },
      ],
      list_nodes,
      pair_nodes: [],
      scalar_preimages: [],
    },
  }
}

fn mk_cons_result(
  fixture: ListItemFixture,
  item: cek_builtin_v1.ValueWitnessV1,
) -> cek_builtin_v1.ValueWitnessV1 {
  cek_builtin_v1.SemanticConstantValue {
    type_cbor: fixture.list_type,
    payload: cek_data_v1.list_data_summary_v1(
      cek_data_v1.prepend_data_list_summary_v1(
        cek_builtin_v1.semantic_constant_payload_v1(item),
        fixture.tail,
      ),
    ),
    memory: cek_builtin_v1.semantic_constant_memory_v1(item) + cek_builtin_v1.semantic_constant_memory_v1(
      fixture.source,
    ),
  }
}

/// The item presented by its semantic summary, with the summary memory moved
/// by `delta`.
fn presented_item(
  fixture: ListItemFixture,
  delta: Int,
) -> cek_builtin_v1.ValueWitnessV1 {
  expect cek_builtin_v1.SemanticConstantValue { type_cbor, payload, memory } = cek_builtin_v1.compact_semantic_value(
    fixture.item,
  )
  cek_builtin_v1.SemanticConstantValue {
    type_cbor,
    payload: cek_data_v1.DataSummaryV1 {
      ..payload,
      memory: payload.memory + delta,
    },
    memory,
  }
}

/// mkCons over the presented item, in the direct arm and the split chain.
fn mk_cons_presented(fixture: ListItemFixture, delta: Int) -> Bool {
  let item = presented_item(fixture, delta)
  verify_semantic_with_split_parity(
    32,
    [item, fixture.source],
    mk_cons_result(fixture, item),
    fixture.material,
  )
}

/// mkCons in the direct arm and the split list hop, each given the arguments
/// as presented, so a revealed source is not compacted.
fn mk_cons_in_both_arms(
  fixture: ListItemFixture,
  item: cek_builtin_v1.ValueWitnessV1,
) -> Pair<Bool, Bool> {
  let arguments = [item, fixture.source]
  let result = mk_cons_result(fixture, item)
  Pair(
    cek_builtin_v1.verify_semantic_builtin_v1(
      32,
      arguments,
      result,
      fixture.material,
    ),
    cek_builtin_v1.verify_semantic_list_construct(
      32,
      arguments,
      result,
      fixture.material,
    ),
  )
}

fn summary_memory_words(item: cek_builtin_v1.ValueWitnessV1) -> Int {
  cek_builtin_v1.semantic_constant_payload_v1(item).memory - cek_builtin_v1.semantic_constant_memory_v1(
    item,
  )
}

fn data_item_fixture() -> ListItemFixture {
  list_item_fixture(
    #"08",
    builtin.i_data(0),
    [builtin.i_data(1), builtin.i_data(2)],
  )
}

fn integer_item_fixture() -> ListItemFixture {
  list_item_fixture(
    #"00",
    builtin.i_data(70000),
    [builtin.i_data(1), builtin.i_data(2)],
  )
}

fn bytes_item_fixture() -> ListItemFixture {
  list_item_fixture(
    #"01",
    builtin.b_data(#"2a2a2a2a2a2a2a2a2a"),
    [builtin.b_data(#"01")],
  )
}

fn data_pair_item_fixture() -> ListItemFixture {
  list_item_fixture(
    #"060808",
    builtin.constr_data(0, [builtin.i_data(1), builtin.b_data(#"02")]),
    [builtin.constr_data(0, [builtin.i_data(3), builtin.i_data(4)])],
  )
}

fn unit_item_fixture() -> ListItemFixture {
  list_item_fixture(
    #"03",
    builtin.constr_data(0, []),
    [builtin.constr_data(0, [])],
  )
}

fn boolean_item_fixture() -> ListItemFixture {
  list_item_fixture(
    #"04",
    builtin.constr_data(1, []),
    [builtin.constr_data(0, [])],
  )
}

fn string_item_fixture() -> ListItemFixture {
  list_item_fixture(#"02", builtin.b_data(#"6162"), [builtin.b_data(#"63")])
}

fn integer_list_item_fixture() -> ListItemFixture {
  list_item_fixture(
    #"0500",
    builtin.list_data([builtin.i_data(1)]),
    [builtin.list_data([builtin.i_data(2)])],
  )
}

fn integer_pair_item_fixture() -> ListItemFixture {
  list_item_fixture(
    #"060000",
    builtin.constr_data(0, [builtin.i_data(1), builtin.i_data(2)]),
    [builtin.constr_data(0, [builtin.i_data(3), builtin.i_data(4)])],
  )
}

test mk_cons_accepts_a_presented_data_item_whose_summary_memory_matches_its_memory() {
  let fixture = data_item_fixture()
  and {
    summary_memory_words(presented_item(fixture, 0)) == 0,
    mk_cons_presented(fixture, 0),
  }
}

test mk_cons_refuses_a_presented_data_item_whose_summary_memory_is_one_more() {
  !mk_cons_presented(data_item_fixture(), 1)
}

test mk_cons_refuses_a_presented_data_item_whose_summary_memory_is_one_less() {
  !mk_cons_presented(data_item_fixture(), -1)
}

test mk_cons_accepts_a_presented_integer_item_whose_summary_memory_matches_its_memory() {
  let fixture = integer_item_fixture()
  and {
    summary_memory_words(presented_item(fixture, 0)) == 4,
    mk_cons_presented(fixture, 0),
  }
}

test mk_cons_refuses_a_presented_integer_item_whose_summary_memory_is_one_more() {
  !mk_cons_presented(integer_item_fixture(), 1)
}

test mk_cons_refuses_a_presented_integer_item_whose_summary_memory_is_one_less() {
  !mk_cons_presented(integer_item_fixture(), -1)
}

test mk_cons_accepts_a_presented_bytes_item_whose_summary_memory_matches_its_memory() {
  let fixture = bytes_item_fixture()
  and {
    summary_memory_words(presented_item(fixture, 0)) == 4,
    mk_cons_presented(fixture, 0),
  }
}

test mk_cons_refuses_a_presented_bytes_item_whose_summary_memory_is_one_more() {
  !mk_cons_presented(bytes_item_fixture(), 1)
}

test mk_cons_refuses_a_presented_bytes_item_whose_summary_memory_is_one_less() {
  !mk_cons_presented(bytes_item_fixture(), -1)
}

test mk_cons_accepts_a_presented_data_pair_item_whose_summary_memory_matches_its_memory() {
  let fixture = data_pair_item_fixture()
  and {
    summary_memory_words(presented_item(fixture, 0)) == 4,
    mk_cons_presented(fixture, 0),
  }
}

test mk_cons_refuses_a_presented_data_pair_item_whose_summary_memory_is_one_more() {
  !mk_cons_presented(data_pair_item_fixture(), 1)
}

test mk_cons_refuses_a_presented_data_pair_item_whose_summary_memory_is_one_less() {
  !mk_cons_presented(data_pair_item_fixture(), -1)
}

test mk_cons_accepts_a_presented_unit_item_whose_summary_memory_matches_its_memory() {
  let fixture = unit_item_fixture()
  and {
    summary_memory_words(presented_item(fixture, 0)) == 3,
    mk_cons_presented(fixture, 0),
  }
}

test mk_cons_refuses_a_presented_unit_item_whose_summary_memory_is_one_more() {
  !mk_cons_presented(unit_item_fixture(), 1)
}

test mk_cons_refuses_a_presented_unit_item_whose_summary_memory_is_one_less() {
  !mk_cons_presented(unit_item_fixture(), -1)
}

test mk_cons_accepts_a_presented_boolean_item_whose_summary_memory_matches_its_memory() {
  let fixture = boolean_item_fixture()
  and {
    summary_memory_words(presented_item(fixture, 0)) == 3,
    mk_cons_presented(fixture, 0),
  }
}

test mk_cons_refuses_a_presented_boolean_item_whose_summary_memory_is_one_more() {
  !mk_cons_presented(boolean_item_fixture(), 1)
}

test mk_cons_refuses_a_presented_boolean_item_whose_summary_memory_is_one_less() {
  !mk_cons_presented(boolean_item_fixture(), -1)
}

test mk_cons_refuses_an_item_whose_summary_memory_differs_from_its_memory() {
  let fixture = data_item_fixture()
  and {
    mk_cons_presented(fixture, 0),
    !mk_cons_presented(fixture, 1),
    !mk_cons_presented(fixture, -1),
    !mk_cons_presented(fixture, 100),
  }
}

/// A presented item of an element type with no summary memory relation, at
/// its own summary memory and at each relation a bound element type uses.
fn mk_cons_refuses_the_presented_item(fixture: ListItemFixture) -> Bool {
  let words = summary_memory_words(presented_item(fixture, 0))
  list.all(
    [0, -words, 3 - words, 4 - words],
    fn(delta) {
      mk_cons_in_both_arms(fixture, presented_item(fixture, delta)) == Pair(
        False,
        False,
      )
    },
  )
}

test mk_cons_refuses_a_presented_string_item_at_any_summary_memory() {
  mk_cons_refuses_the_presented_item(string_item_fixture())
}

test mk_cons_refuses_a_presented_integer_list_item_at_any_summary_memory() {
  mk_cons_refuses_the_presented_item(integer_list_item_fixture())
}

test mk_cons_refuses_a_presented_integer_pair_item_at_any_summary_memory() {
  mk_cons_refuses_the_presented_item(integer_pair_item_fixture())
}

test mk_cons_accepts_a_revealed_item_of_any_element_type() {
  list.all(
    [
      data_item_fixture(),
      integer_item_fixture(),
      bytes_item_fixture(),
      data_pair_item_fixture(),
      unit_item_fixture(),
      boolean_item_fixture(),
      string_item_fixture(),
      integer_list_item_fixture(),
      integer_pair_item_fixture(),
    ],
    fn(fixture) {
      mk_cons_in_both_arms(fixture, fixture.item) == Pair(True, True)
    },
  )
}

/// The split roots hop compacts every argument before the list hop reads it
/// (cek-core-builtin-roots.ak), so the list hop sees both compacted.
fn mk_cons_from_its_split_presentation(fixture: ListItemFixture) -> Bool {
  let arguments = list.map(
    [fixture.item, fixture.source],
    cek_builtin_v1.compact_semantic_value,
  )
  and {
    verify_semantic_with_split_parity(
      32,
      [fixture.item, fixture.source],
      mk_cons_result(fixture, fixture.item),
      fixture.material,
    ),
    cek_builtin_v1.verify_semantic_list_construct(
      32,
      arguments,
      mk_cons_result(fixture, fixture.item),
      fixture.material,
    ),
  }
}

test mk_cons_over_a_unit_list_is_proved_from_its_split_presentation() {
  mk_cons_from_its_split_presentation(unit_item_fixture())
}

test mk_cons_over_a_bool_list_is_proved_from_its_split_presentation() {
  mk_cons_from_its_split_presentation(boolean_item_fixture())
}

test mk_cons_over_a_data_list_is_proved_from_its_split_presentation() {
  mk_cons_from_its_split_presentation(data_item_fixture())
}

test mk_cons_over_an_integer_list_is_proved_from_its_split_presentation() {
  mk_cons_from_its_split_presentation(integer_item_fixture())
}

test mk_cons_over_a_data_pair_list_is_proved_from_its_split_presentation() {
  mk_cons_from_its_split_presentation(data_pair_item_fixture())
}

// Builtin arguments and results that are, or end in, an empty byte string.
// Each charge is Cardano's PV 11 total for the one-builtin program in
// cek-program.data-constant-spelling.test.ts, less the 100/100 start-up and
// its 16000/100 machine steps (five for a two-argument builtin, three for a
// one-argument one).
fn empty_bytes_list() -> Data {
  builtin.list_data([builtin.b_data(#"")])
}

test data_builtins_charge_cardano_budgets_over_an_ending_empty_byte_string() {
  and {
    charges(46, [data_value(builtin.b_data(#""))], 20142, 32),
    charges(41, [bytes(#"40")], 11183, 32),
    charges(44, [data_value(empty_bytes_list())], 25933, 32),
    charges(
      43,
      [
        data_value(
          builtin.map_data([Pair(builtin.i_data(0), builtin.b_data(#""))]),
        ),
      ],
      24623,
      32,
    ),
    charges(
      42,
      [data_value(builtin.constr_data(1, [builtin.b_data(#"")]))],
      24588,
      32,
    ),
    charges(51, [data_value(builtin.b_data(#""))], 2022066, 10),
    charges(51, [data_value(empty_bytes_list())], 2875314, 18),
    charges(
      51,
      [
        data_value(
          builtin.map_data([Pair(builtin.i_data(0), builtin.b_data(#""))]),
        ),
      ],
      3941874,
      28,
    ),
    charges(
      47,
      [data_value(builtin.b_data(#"")), data_value(builtin.b_data(#""))],
      1034543,
      1,
    ),
    charges(
      47,
      [data_value(empty_bytes_list()), data_value(empty_bytes_list())],
      1143659,
      1,
    ),
  }
}

test data_builtins_read_and_return_an_ending_empty_byte_string() {
  let empty = [data_value(builtin.b_data(#""))]
  let list = [data_value(empty_bytes_list())]
  let constructor = [data_value(builtin.constr_data(1, [builtin.b_data(#"")]))]
  and {
    verify_direct_with_split_parity(
      41,
      [bytes(#"40")],
      constant(#"9f08ff", #"40"),
    ),
    verify_direct_with_split_parity(46, empty, bytes(#"40")),
    verify_direct_with_split_parity(44, list, constant(#"9f0508ff", #"9f40ff")),
    verify_direct_with_split_parity(
      42,
      constructor,
      constant(#"9f06000508ff", #"d8799f019f40ffff"),
    ),
    verify_direct_with_split_parity(51, list, bytes(#"439f40ff")),
  }
}

test data_builtins_reveal_an_ending_empty_byte_string() {
  and {
    serialise_data_verifies(builtin.b_data(#""), #"40", #"40"),
    serialise_data_verifies(empty_bytes_list(), #"9f40ff", #"9f40ff"),
    serialise_data_split_arm(builtin.b_data(#"")),
    serialise_data_split_arm(empty_bytes_list()),
    equals_data_verifies(#"40", #"40", #"d87a80"),
    equals_data_verifies(#"9f40ff", #"9f40ff", #"d87a80"),
    equals_data_verifies(#"40", #"9f40ff", #"d87980"),
  }
}

test un_b_data_refuses_a_byte_string_claiming_one_byte_past_the_end() fail {
  let source = [constant(#"9f08ff", #"41")]
  verify_direct_with_split_parity(46, source, bytes(#"40"))
}

test serialise_data_refuses_a_revealed_byte_string_claiming_one_byte_past_the_end() fail {
  serialise_data_verifies(builtin.b_data(#""), #"41", #"40")
}
