//// Transaction-level rows for the staged conversion semantics step (arm 31,
//// group 5, semantics index 9): byteStringToInteger over an input bound
//// inline or from staging outputs, of at most 65,536 bytes. The step must
//// accept exactly the integer Cardano returns, in either byte order.

use aiken/builtin
use aiken/cbor
use aiken/collection/list
use fraud_proofs/validation_trace/cek_core_builtin_budget as budget
use fraud_proofs/validation_trace/cek_core_staged_bytes as staged_bytes
use fraud_proofs/validation_trace/cek_core_staged_conversion as staged_conversion
use fraud_proofs/validation_trace/cek_core_staged_integer as staged_integer
use midgard/cek_builtin_v1.{SemanticConstantValue, ValueWitnessV1}
use midgard/cek_machine_v1 as machine
use midgard/cek_staged_arm_v1.{
  BytesLeafOperand, InlineOperand, OperandSourceV1, SmallIntegerOperand,
}
use midgard/cek_staged_blob_v1.{flat_root_of_pieces_v1, flat_root_v1}
use midgard/cek_staged_integer_cbor_v1.{encode_integer_cbor_pieces_v1}
use midgard/cek_staged_integer_v1.{
  CanonicalIntegerV1, integer_leaf_from_cbor_v1, magnitude_words_v1,
}
use midgard/fraud_proofs/cek_staged_fixture_v1.{
  StagedStepV1, StagingV1, boolean_value, budget_step, bytes_value,
  integer_type_cbor, integer_value, material, no_staging, other_input, pattern,
  semantic_pieces, stage, stage_pieces, staged, staged_step, staged_step_at,
  text, zeros,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture

// ## Spending

fn spend(step: StagedStepV1) -> Bool {
  staged_conversion.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    step.datum,
    step.redeemer,
    fixture.own_out_ref(),
    step.transaction,
  )
}

/// byteStringToInteger over `input` in byte order `big_endian`, with the
/// input staged.
fn conversion_step(
  big_endian: Bool,
  input: ByteArray,
  result: ValueWitnessV1,
) -> StagedStepV1 {
  let staging = stage([input])
  staged_step(
    74,
    [boolean_value(big_endian), bytes_value(input, False)],
    result,
    material(staging, [staged(0)]),
    staging.reference_inputs,
  )
}

/// The integer Cardano returns for `input`, of at most 32,768 magnitude
/// bytes.
fn converted(big_endian: Bool, input: ByteArray) -> ValueWitnessV1 {
  integer_value(builtin.bytearray_to_integer(big_endian, input))
}

/// The integer Cardano returns for `input`, whose magnitude is
/// `magnitude_bytes` bytes, rooted from its CBOR as Cardano serialises it
/// (which must fit one byte string).
fn converted_long(
  big_endian: Bool,
  input: ByteArray,
  magnitude_bytes: Int,
) -> ValueWitnessV1 {
  let integer_cbor = cbor.serialise(
    builtin.bytearray_to_integer(big_endian, input),
  )
  let words = ( magnitude_bytes + 7 ) / 8
  SemanticConstantValue {
    type_cbor: integer_type_cbor,
    payload: integer_leaf_from_cbor_v1(
      flat_root_v1(integer_cbor),
      builtin.length_of_bytearray(integer_cbor),
      words,
    ),
    memory: words,
  }
}

/// The non-negative integer whose minimal big-endian bytes are `argument`,
/// rooted from the pieces of its CBOR, which can be longer than one byte
/// string.
fn integer_of_argument(argument: ByteArray) -> ValueWitnessV1 {
  let canonical = CanonicalIntegerV1 { negative: False, argument }
  let pieces = encode_integer_cbor_pieces_v1(canonical)
  let words = magnitude_words_v1(canonical)
  SemanticConstantValue {
    type_cbor: integer_type_cbor,
    payload: integer_leaf_from_cbor_v1(
      flat_root_of_pieces_v1(pieces),
      list.foldl(
        pieces,
        0,
        fn(piece, total) { total + builtin.length_of_bytearray(piece) },
      ),
      words,
    ),
    memory: words,
  }
}

/// `bytes` in reverse order: blocks of up to 4,096 bytes, each reversed by
/// the builtins, joined last block first.
fn reversed(bytes: ByteArray) -> ByteArray {
  let length = builtin.length_of_bytearray(bytes)
  if length == 0 {
    #""
  } else {
    let block = if length < 4096 {
      length
    } else {
      4096
    }
    builtin.append_bytearray(
      reversed(builtin.slice_bytearray(block, length - block, bytes)),
      builtin.integer_to_bytearray(
        True,
        block,
        builtin.bytearray_to_integer(
          False,
          builtin.slice_bytearray(0, block, bytes),
        ),
      ),
    )
  }
}

/// The honest step for `input` in byte order `big_endian` verifies.
fn converts(big_endian: Bool, input: ByteArray) -> Bool {
  spend(conversion_step(big_endian, input, converted(big_endian, input)))
}

fn verifies(
  arguments: List<ValueWitnessV1>,
  result: ValueWitnessV1,
  staging: StagingV1,
  sources: List<OperandSourceV1>,
) -> Bool {
  spend(
    staged_step(
      74,
      arguments,
      result,
      material(staging, sources),
      staging.reference_inputs,
    ),
  )
}

/// An input whose first byte is non-zero.
fn input(length: Int, shift: Int) -> ByteArray {
  builtin.cons_bytearray(0x5a, pattern(length - 1, shift))
}

// ## Honest steps verify

test converts_big_endian_input_past_the_direct_bound() {
  converts(True, input(9216, 3))
}

test converts_little_endian_input_past_the_direct_bound() {
  converts(False, input(9216, 3))
}

test converts_big_endian_input_of_49_000_bytes() {
  let bytes = input(49000, 5)
  spend(conversion_step(True, bytes, converted_long(True, bytes, 49000)))
}

test converts_little_endian_input_of_49_000_bytes() {
  let bytes = reversed(input(49000, 5))
  spend(conversion_step(False, bytes, converted_long(False, bytes, 49000)))
}

test converts_big_endian_input_at_the_argument_bound() {
  spend(step_at_the_bound(True))
}

test converts_little_endian_input_at_the_argument_bound() {
  spend(step_at_the_bound(False))
}

test converts_little_endian_input_at_the_argument_bound_with_zero_top_bytes() {
  // Little-endian, the zeros are the most significant bytes.
  let magnitude = input(64000, 7)
  spend(
    conversion_step(
      False,
      reversed(builtin.append_bytearray(zeros(1536), magnitude)),
      integer_of_argument(magnitude),
    ),
  )
}

test the_piece_rooted_integer_agrees_with_the_serialised_one() {
  let bytes = input(49000, 5)
  and {
    integer_of_argument(bytes) == converted_long(True, bytes, 49000),
    integer_of_argument(bytes) == converted_long(False, reversed(bytes), 49000),
    integer_of_argument(input(30000, 1)) == converted(True, input(30000, 1)),
  }
}

test converts_input_of_one_limb_and_one_byte_in_both_orders() {
  let bytes = input(513, 2)
  converts(True, bytes) && converts(False, bytes)
}

test converts_big_endian_input_with_leading_zero_bytes() {
  converts(True, builtin.append_bytearray(zeros(1500), input(9216, 4)))
}

test converts_little_endian_input_with_trailing_zero_bytes() {
  converts(False, builtin.append_bytearray(input(9216, 4), zeros(1500)))
}

test converts_little_endian_input_whose_leading_bytes_are_zero() {
  converts(False, builtin.append_bytearray(zeros(1500), input(9216, 4)))
}

test converts_zero_input_to_zero_in_both_orders() {
  let bytes = zeros(9216)
  and {
    spend(conversion_step(True, bytes, integer_value(0))),
    spend(conversion_step(False, bytes, integer_value(0))),
  }
}

test converts_an_empty_inline_input_to_zero() {
  verifies(
    [boolean_value(True), bytes_value(#"", False)],
    integer_value(0),
    no_staging(),
    [InlineOperand { bytes: #"" }],
  )
}

test agrees_with_the_direct_route_on_inline_input() {
  list.all(
    [#"01", #"ff00", #"00ff", pattern(600, 1), pattern(4000, 9)],
    fn(bytes) {
      list.all(
        [True, False],
        fn(big_endian) {
          verifies(
            [boolean_value(big_endian), bytes_value(bytes, False)],
            converted(big_endian, bytes),
            no_staging(),
            [InlineOperand { bytes }],
          )
        },
      )
    },
  )
}

test ignores_reference_inputs_the_material_does_not_name() {
  let bytes = input(9216, 1)
  let staging = stage([bytes])
  verifies(
    [boolean_value(True), bytes_value(bytes, False)],
    converted(True, bytes),
    StagingV1 {
      ..staging,
      reference_inputs: list.concat(
        staging.reference_inputs,
        [other_input(250)],
      ),
    },
    [staged(0)],
  )
}

// ## Wrong results are refused

test refuses_the_integer_of_the_other_byte_order() fail {
  let bytes = input(9216, 3)
  spend(conversion_step(True, bytes, converted(False, bytes)))
}

test refuses_a_result_one_greater() fail {
  let bytes = input(9216, 3)
  spend(
    conversion_step(
      False,
      bytes,
      integer_value(builtin.bytearray_to_integer(False, bytes) + 1),
    ),
  )
}

test refuses_a_negative_result() fail {
  let bytes = input(9216, 3)
  spend(
    conversion_step(
      True,
      bytes,
      integer_value(-builtin.bytearray_to_integer(True, bytes)),
    ),
  )
}

test refuses_a_byte_string_result() fail {
  let bytes = input(9216, 3)
  spend(conversion_step(True, bytes, bytes_value(bytes, False)))
}

test refuses_an_endianness_that_is_not_a_boolean() fail {
  let bytes = input(9216, 3)
  let staging = stage([bytes])
  verifies(
    [integer_value(1), bytes_value(bytes, False)],
    converted(True, bytes),
    staging,
    [staged(0)],
  )
}

// ## Binding

test refuses_input_past_the_argument_bound() fail {
  let pieces = [input(65536, 2), #"01"]
  verifies(
    [boolean_value(True), semantic_pieces(pieces, False)],
    integer_value(0),
    stage_pieces([pieces]),
    [staged(0)],
  )
}

test refuses_input_bound_by_its_leaf_alone() fail {
  let bytes = input(9216, 3)
  verifies(
    [boolean_value(True), bytes_value(bytes, False)],
    converted(True, bytes),
    no_staging(),
    [BytesLeafOperand { root: flat_root_of_pieces_v1([bytes]), length: 9216 }],
  )
}

test refuses_staged_bytes_that_differ_from_the_input() fail {
  let bytes = input(9216, 3)
  let other = input(9216, 4)
  let staging = stage([other])
  verifies(
    [boolean_value(True), bytes_value(bytes, False)],
    converted(True, bytes),
    staging,
    [staged(0)],
  )
}

test refuses_a_string_input() fail {
  let bytes = text(9216)
  let staging = stage([bytes])
  verifies(
    [boolean_value(True), bytes_value(bytes, True)],
    converted(True, bytes),
    staging,
    [staged(0)],
  )
}

test refuses_a_source_for_the_endianness() fail {
  let bytes = input(9216, 3)
  let staging = stage([bytes])
  verifies(
    [boolean_value(True), bytes_value(bytes, False)],
    converted(True, bytes),
    staging,
    [SmallIntegerOperand { value: 1 }, staged(0)],
  )
}

// ## Routing

/// The budget hop for builtin `tag` over `arguments`, with the step it
/// continues to at semantics index `index`.
fn routes_to(tag: Int, arguments: List<ValueWitnessV1>, index: Int) -> Bool {
  let step = budget_step(tag, arguments, integer_value(1))
  budget.main.spend(
    list.map(
      list.range(0, 9),
      fn(i) {
        if i == index {
          fixture.next_step_script_hash
        } else {
          fixture.step_script_hash
        }
      },
    ),
    fixture.computation_thread_policy_id,
    step.datum,
    step.redeemer,
    fixture.own_out_ref(),
    step.transaction,
  )
}

test the_budget_hop_routes_a_conversion_here_at_the_bound() {
  list.all(
    [True, False],
    fn(big_endian) {
      routes_to(
        74,
        [boolean_value(big_endian), bytes_value(input(65536, 1), False)],
        9,
      )
    },
  )
}

test the_budget_hop_does_not_route_a_conversion_to_the_byte_string_semantics() fail {
  routes_to(74, [boolean_value(True), bytes_value(input(65536, 1), False)], 7)
}

test the_byte_string_semantics_step_refuses_a_conversion() fail {
  let step = conversion_step(
    True,
    input(9216, 3),
    converted(True, input(9216, 3)),
  )
  staged_bytes.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    step.datum,
    step.redeemer,
    fixture.own_out_ref(),
    step.transaction,
  )
}

test the_integer_semantics_step_refuses_a_conversion() fail {
  let step = conversion_step(
    True,
    input(9216, 3),
    converted(True, input(9216, 3)),
  )
  staged_integer.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    step.datum,
    step.redeemer,
    fixture.own_out_ref(),
    step.transaction,
  )
}

test refuses_a_step_before_the_budget_hop() fail {
  let bytes = input(9216, 3)
  let staging = stage([bytes])
  spend(
    staged_step_at(
      74,
      [boolean_value(True), bytes_value(bytes, False)],
      converted(True, bytes),
      material(staging, [staged(0)]),
      staging.reference_inputs,
      machine.mode_builtin,
      5,
      2,
    ),
  )
}

test refuses_a_step_in_another_group() fail {
  let bytes = input(9216, 3)
  let staging = stage([bytes])
  spend(
    staged_step_at(
      74,
      [boolean_value(True), bytes_value(bytes, False)],
      converted(True, bytes),
      material(staging, [staged(0)]),
      staging.reference_inputs,
      machine.mode_builtin,
      2,
      3,
    ),
  )
}

// ## Maximum shape

/// byteStringToInteger over a staged input of 65,536 bytes whose most
/// significant byte is non-zero in the given order.
fn step_at_the_bound(big_endian: Bool) -> StagedStepV1 {
  let bytes = input(65536, 7)
  if big_endian {
    conversion_step(True, bytes, integer_of_argument(bytes))
  } else {
    conversion_step(False, reversed(bytes), integer_of_argument(bytes))
  }
}

fn built(step: StagedStepV1) -> Bool {
  when step.datum is {
    Some(_) -> True
    None -> False
  }
}

test staged_conversion_big_endian_at_the_argument_bound() {
  spend(step_at_the_bound(True))
}

test staged_conversion_big_endian_fixture_only_at_the_argument_bound() {
  built(step_at_the_bound(True))
}

test staged_conversion_little_endian_at_the_argument_bound() {
  spend(step_at_the_bound(False))
}

test staged_conversion_little_endian_fixture_only_at_the_argument_bound() {
  built(step_at_the_bound(False))
}
