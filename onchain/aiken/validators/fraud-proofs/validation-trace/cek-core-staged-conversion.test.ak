//// Transaction-level rows for the staged conversion semantics step (arm 31,
//// group 5, semantics index 11): byteStringToInteger over an input bound
//// inline or from staging outputs, of at most 65,536 bytes, and
//// shiftByteString and rotateByteString over an input of any length. The
//// step must accept exactly the integer Cardano returns, in either byte
//// order, and exactly the bytes a shift or rotation returns.

use aiken/builtin
use aiken/cbor
use aiken/collection/list
use fraud_proofs/validation_trace/cek_core_builtin_budget as budget
use fraud_proofs/validation_trace/cek_core_staged_bytes as staged_bytes
use fraud_proofs/validation_trace/cek_core_staged_conversion as staged_conversion
use fraud_proofs/validation_trace/cek_core_staged_integer as staged_integer
use midgard/cek_builtin_v1.{SemanticConstantValue, ValueWitnessV1}
use midgard/cek_data_v1
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
  StagedStepV1, StagingV1, boolean_value, budget_step, bytes_type_cbor,
  bytes_value, flip_last_byte, integer_type_cbor, integer_value, material,
  no_staging, other_input, pattern, semantic_pieces, stage, stage_pieces, staged,
  staged_step, staged_step_at, text, zeros,
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

/// byteStringToInteger over the staged input `pieces`, whose value is 7,
/// claiming 7.
fn claims_seven(big_endian: Bool, pieces: List<ByteArray>) -> Bool {
  verifies(
    [boolean_value(big_endian), semantic_pieces(pieces, False)],
    integer_value(7),
    stage_pieces([pieces]),
    [staged(0)],
  )
}

test converts_input_of_pieces_at_the_argument_bound() {
  and {
    claims_seven(True, [zeros(65535), #"07"]),
    claims_seven(False, [#"07", zeros(65535)]),
    claims_seven(True, [zeros(30000), zeros(35535), #"07"]),
  }
}

// The rows below claim the input's own value, so only the input's bound
// refuses them.

test refuses_big_endian_input_past_the_argument_bound() fail {
  claims_seven(True, [zeros(65536), #"07"])
}

test refuses_little_endian_input_past_the_argument_bound() fail {
  claims_seven(False, [#"07", zeros(65536)])
}

test refuses_input_of_three_pieces_past_the_argument_bound() fail {
  claims_seven(True, [zeros(30000), zeros(35536), #"07"])
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

/// byteStringToInteger over the argument `bytes` with `other`, of the same
/// length, staged in its place, claiming the value of `other`, so only the
/// binding of the staged bytes to the argument refuses it.
fn stages_other_bytes(big_endian: Bool) -> Bool {
  let bytes = input(9216, 3)
  let other = input(9216, 4)
  verifies(
    [boolean_value(big_endian), bytes_value(bytes, False)],
    converted(big_endian, other),
    stage([other]),
    [staged(0)],
  )
}

test refuses_staged_bytes_that_differ_from_the_input() fail {
  stages_other_bytes(True)
}

test refuses_little_endian_staged_bytes_that_differ_from_the_input() fail {
  stages_other_bytes(False)
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
      list.range(0, 11),
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
        11,
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

// ## shiftByteString and rotateByteString

/// shiftByteString (82) or rotateByteString (83) over the staged input
/// `pieces`, by the revealed `amount`, claiming `result`.
fn shift_step(
  tag: Int,
  pieces: List<ByteArray>,
  amount: Int,
  result: ValueWitnessV1,
) -> StagedStepV1 {
  let staging = stage_pieces([pieces])
  staged_step(
    tag,
    [semantic_pieces(pieces, False), integer_value(amount)],
    result,
    material(staging, [staged(0), SmallIntegerOperand { value: amount }]),
    staging.reference_inputs,
  )
}

/// What Cardano returns for builtin `tag` over `bytes` by `amount`.
fn shifted(tag: Int, bytes: ByteArray, amount: Int) -> ByteArray {
  if tag == 82 {
    builtin.shift_bytearray(bytes, amount)
  } else {
    builtin.rotate_bytearray(bytes, amount)
  }
}

/// The honest step for builtin `tag` over the staged `bytes` by `amount`
/// verifies.
fn shifts(tag: Int, bytes: ByteArray, amount: Int) -> Bool {
  spend(
    shift_step(
      tag,
      [bytes],
      amount,
      bytes_value(shifted(tag, bytes, amount), False),
    ),
  )
}

/// The honest step for builtin `tag` over the inline `bytes` by `amount`
/// verifies.
fn shifts_inline(tag: Int, bytes: ByteArray, amount: Int) -> Bool {
  spend(
    staged_step(
      tag,
      [bytes_value(bytes, False), integer_value(amount)],
      bytes_value(shifted(tag, bytes, amount), False),
      material(
        no_staging(),
        [InlineOperand { bytes }, SmallIntegerOperand { value: amount }],
      ),
      [],
    ),
  )
}

/// Amounts that shift an input of `length` bytes by none, part of a byte,
/// whole bytes, across a chunk, by all of it and past it, either way.
fn amounts(length: Int) -> List<Int> {
  let towards_start = [
    0,
    1,
    7,
    8,
    9,
    8 * 4095 + 3,
    8 * length - 1,
    8 * length,
    8 * length + 13,
    9223372036854775807,
  ]
  list.concat(
    towards_start,
    [
      -9223372036854775808,
      ..list.map(list.drop(towards_start, 1), fn(n) { -n })
    ],
  )
}

fn shifts_by_each_amount(tag: Int, bytes: ByteArray) -> Bool {
  list.all(
    amounts(builtin.length_of_bytearray(bytes)),
    fn(amount) { shifts(tag, bytes, amount) },
  )
}

test shifts_a_staged_input_past_the_direct_bound_by_each_amount() {
  shifts_by_each_amount(82, pattern(9216, 3))
}

test rotates_a_staged_input_past_the_direct_bound_by_each_amount() {
  shifts_by_each_amount(83, pattern(9216, 3))
}

test shifts_and_rotates_a_staged_input_at_the_argument_bound() {
  let bytes = pattern(65536, 5)
  list.all(
    [82, 83],
    fn(tag) {
      list.all(
        [3, -3, 8 * 12345 + 5, -(8 * 12345 + 5), 8 * 65536 - 2],
        fn(amount) { shifts(tag, bytes, amount) },
      )
    },
  )
}

test shifts_and_rotates_inline_input_by_each_amount() {
  list.all(
    [#"", #"01", #"80ff", pattern(600, 1), pattern(4000, 9)],
    fn(bytes) {
      list.all(
        [82, 83],
        fn(tag) {
          list.all(
            amounts(builtin.length_of_bytearray(bytes)),
            fn(amount) { shifts_inline(tag, bytes, amount) },
          )
        },
      )
    },
  )
}

/// An input of 70,000 bytes, longer than one byte string can be, as its
/// two pieces.
fn first_piece() -> ByteArray {
  pattern(65536, 1)
}

fn second_piece() -> ByteArray {
  pattern(4464, 7)
}

fn moves_whole_pieces(tag: Int, amount: Int, result: List<ByteArray>) -> Bool {
  spend(
    shift_step(
      tag,
      [first_piece(), second_piece()],
      amount,
      semantic_pieces(result, False),
    ),
  )
}

test shifts_an_input_past_the_argument_bound_by_whole_pieces() {
  and {
    moves_whole_pieces(82, 8 * 65536, [second_piece(), zeros(65536)]),
    moves_whole_pieces(82, -8 * 4464, [zeros(4464), first_piece()]),
  }
}

test rotates_an_input_past_the_argument_bound_by_whole_pieces() {
  and {
    moves_whole_pieces(83, 8 * 65536, [second_piece(), first_piece()]),
    moves_whole_pieces(83, -8 * 4464, [second_piece(), first_piece()]),
    moves_whole_pieces(83, 8 * 70000 * 3, [first_piece(), second_piece()]),
  }
}

test ignores_reference_inputs_the_shift_material_does_not_name() {
  let bytes = pattern(9216, 1)
  let staging = stage([bytes])
  spend(
    staged_step(
      82,
      [bytes_value(bytes, False), integer_value(11)],
      bytes_value(builtin.shift_bytearray(bytes, 11), False),
      material(staging, [staged(0), SmallIntegerOperand { value: 11 }]),
      list.concat(staging.reference_inputs, [other_input(250)]),
    ),
  )
}

// ### Wrong results are refused

test refuses_a_shift_by_another_amount() fail {
  let bytes = pattern(9216, 3)
  spend(
    shift_step(
      82,
      [bytes],
      11,
      bytes_value(builtin.shift_bytearray(bytes, 12), False),
    ),
  )
}

test refuses_a_rotation_by_another_amount() fail {
  let bytes = pattern(9216, 3)
  spend(
    shift_step(
      83,
      [bytes],
      -11,
      bytes_value(builtin.rotate_bytearray(bytes, -12), False),
    ),
  )
}

test refuses_the_rotation_for_a_shift() fail {
  let bytes = pattern(9216, 3)
  spend(
    shift_step(
      82,
      [bytes],
      8 * 100 + 3,
      bytes_value(builtin.rotate_bytearray(bytes, 8 * 100 + 3), False),
    ),
  )
}

test refuses_the_shift_for_a_rotation() fail {
  let bytes = pattern(9216, 3)
  spend(
    shift_step(
      83,
      [bytes],
      -(8 * 100 + 3),
      bytes_value(builtin.shift_bytearray(bytes, -(8 * 100 + 3)), False),
    ),
  )
}

test refuses_a_shift_with_its_last_byte_changed() fail {
  let bytes = pattern(9216, 3)
  spend(
    shift_step(
      82,
      [bytes],
      5,
      bytes_value(flip_last_byte(builtin.shift_bytearray(bytes, 5)), False),
    ),
  )
}

test refuses_a_shift_one_byte_longer() fail {
  let bytes = pattern(9216, 3)
  spend(
    shift_step(
      82,
      [bytes],
      8,
      bytes_value(
        builtin.append_bytearray(builtin.shift_bytearray(bytes, 8), #"00"),
        False,
      ),
    ),
  )
}

test refuses_a_string_result_for_a_shift() fail {
  let bytes = pattern(9216, 3)
  spend(shift_step(82, [bytes], 0, bytes_value(text(9216), True)))
}

test refuses_a_shift_past_int64() fail {
  let bytes = pattern(9216, 3)
  spend(
    shift_step(
      82,
      [bytes],
      9223372036854775808,
      bytes_value(zeros(9216), False),
    ),
  )
}

test refuses_a_rotation_past_int64() fail {
  let bytes = pattern(9216, 3)
  spend(
    shift_step(83, [bytes], -9223372036854775809, bytes_value(bytes, False)),
  )
}

// ### Binding

test refuses_an_amount_other_than_the_committed_one() fail {
  let bytes = pattern(9216, 3)
  let staging = stage([bytes])
  spend(
    staged_step(
      82,
      [bytes_value(bytes, False), integer_value(11)],
      bytes_value(builtin.shift_bytearray(bytes, 12), False),
      material(staging, [staged(0), SmallIntegerOperand { value: 12 }]),
      staging.reference_inputs,
    ),
  )
}

test refuses_a_shift_with_an_extra_source() fail {
  let bytes = pattern(9216, 3)
  let staging = stage([bytes])
  spend(
    staged_step(
      82,
      [bytes_value(bytes, False), integer_value(11)],
      bytes_value(builtin.shift_bytearray(bytes, 11), False),
      material(
        staging,
        [
          staged(0),
          SmallIntegerOperand { value: 11 },
          SmallIntegerOperand { value: 11 },
        ],
      ),
      staging.reference_inputs,
    ),
  )
}

test refuses_a_shift_input_bound_by_its_leaf_alone() fail {
  let bytes = pattern(9216, 3)
  spend(
    staged_step(
      82,
      [bytes_value(bytes, False), integer_value(11)],
      bytes_value(builtin.shift_bytearray(bytes, 11), False),
      material(
        no_staging(),
        [
          BytesLeafOperand {
            root: flat_root_of_pieces_v1([bytes]),
            length: 9216,
          },
          SmallIntegerOperand { value: 11 },
        ],
      ),
      [],
    ),
  )
}

test refuses_staged_bytes_that_differ_from_the_shift_input() fail {
  let bytes = pattern(9216, 3)
  let staging = stage([pattern(9216, 4)])
  spend(
    staged_step(
      83,
      [bytes_value(bytes, False), integer_value(11)],
      bytes_value(builtin.rotate_bytearray(bytes, 11), False),
      material(staging, [staged(0), SmallIntegerOperand { value: 11 }]),
      staging.reference_inputs,
    ),
  )
}

test refuses_a_string_shift_input() fail {
  let bytes = text(9216)
  let staging = stage([bytes])
  spend(
    staged_step(
      82,
      [bytes_value(bytes, True), integer_value(0)],
      bytes_value(bytes, False),
      material(staging, [staged(0), SmallIntegerOperand { value: 0 }]),
      staging.reference_inputs,
    ),
  )
}

test refuses_a_staged_amount() fail {
  let bytes = pattern(9216, 3)
  let staging = stage([bytes])
  spend(
    staged_step(
      82,
      [bytes_value(bytes, False), integer_value(0)],
      bytes_value(bytes, False),
      material(staging, [staged(0), staged(0)]),
      staging.reference_inputs,
    ),
  )
}

// ### Routing

test the_budget_hop_routes_shifts_and_rotations_here() {
  list.all(
    [82, 83],
    fn(tag) {
      and {
        routes_to(
          tag,
          [bytes_value(pattern(65536, 1), False), integer_value(-3)],
          11,
        ),
        routes_to(
          tag,
          [semantic_pieces(max_shape_pieces(), False), integer_value(8003)],
          11,
        ),
      }
    },
  )
}

test the_budget_hop_does_not_route_a_shift_to_the_byte_string_semantics() fail {
  routes_to(82, [bytes_value(pattern(65536, 1), False), integer_value(3)], 7)
}

test the_byte_string_semantics_step_refuses_a_rotation() fail {
  let bytes = pattern(9216, 3)
  let step = shift_step(
    83,
    [bytes],
    3,
    bytes_value(builtin.rotate_bytearray(bytes, 3), False),
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

// ### Maximum shape

/// The longest input an honest script can shift or rotate within the
/// transaction budget is 370,340 bytes (serialiseData over copies of a
/// 65,536-byte string). This input is 91 full chunks, 372,645 bytes: byte i
/// is (i + 1) mod 256.
fn max_shape_pieces() -> List<ByteArray> {
  let piece = pattern(65536, 1)
  [piece, piece, piece, piece, piece, pattern(44965, 1)]
}

/// The bytes result of 372,645 bytes whose flat root is `root`, which the
/// validation package computes from Cardano's result.
fn max_shape_result(root: ByteArray) -> ValueWitnessV1 {
  SemanticConstantValue {
    type_cbor: bytes_type_cbor,
    payload: cek_data_v1.bytes_data_summary_v1(372645, root),
    memory: cek_data_v1.bytes_memory_size_v1(372645),
  }
}

fn max_shape_step(tag: Int, amount: Int, root: ByteArray) -> StagedStepV1 {
  shift_step(tag, max_shape_pieces(), amount, max_shape_result(root))
}

const shifted_left_root =
  #"5034610bed6f5f61831b2b18a36af6dacebda3d37d912465fcde996bd101fd22"

const shifted_right_root =
  #"95db54de631587287faf289955ea51272dc14d56481ab7ad6738a010e1b5884e"

const rotated_root =
  #"ea39496b399eb92fd8fb0e179ce948e191566ff75413d957c53fbab497c8c0d7"

test staged_shift_left_at_the_maximum_shape() {
  spend(max_shape_step(82, 8003, shifted_left_root))
}

test staged_shift_left_fixture_only_at_the_maximum_shape() {
  built(max_shape_step(82, 8003, shifted_left_root))
}

test staged_shift_right_at_the_maximum_shape() {
  spend(max_shape_step(82, -8003, shifted_right_root))
}

test staged_shift_right_fixture_only_at_the_maximum_shape() {
  built(max_shape_step(82, -8003, shifted_right_root))
}

test staged_rotate_at_the_maximum_shape() {
  spend(max_shape_step(83, 1600005, rotated_root))
}

test staged_rotate_fixture_only_at_the_maximum_shape() {
  built(max_shape_step(83, 1600005, rotated_root))
}

test refuses_a_rotation_at_the_maximum_shape_rooted_as_the_shift() fail {
  spend(max_shape_step(83, 8003, shifted_left_root))
}
