//// Transaction-level rows for the staged integer semantics step (arm 31,
//// group 5, semantics index 8): addInteger, subtractInteger, equalsInteger,
//// lessThanInteger and lessThanEqualsInteger over operands bound by their
//// canonical CBOR (inline or from staging outputs) or by their leaf. The step
//// must accept exactly the results Cardano returns.

use aiken/builtin
use aiken/cbor
use aiken/collection/list
use fraud_proofs/validation_trace/cek_core_builtin_budget as budget
use fraud_proofs/validation_trace/cek_core_staged_integer as staged_integer
use midgard/cek_builtin_v1.{SemanticConstantValue, ValueWitnessV1}
use midgard/cek_machine_v1 as machine
use midgard/cek_staged_arm_v1.{
  InlineOperand, IntegerLeafOperand, OperandSourceV1,
}
use midgard/cek_staged_blob_v1.{flat_root_v1}
use midgard/cek_staged_integer_cbor_v1.{
  encode_integer_cbor_bulk_v1, parse_integer_cbor_bulk_v1,
}
use midgard/cek_staged_integer_v1.{CanonicalIntegerV1, magnitude_words_v1}
use midgard/fraud_proofs/cek_staged_fixture_v1.{
  StagedStepV1, StagingV1, boolean_value, budget_step, bytes_type_cbor,
  bytes_value, integer_value, material, no_staging, pattern, power_of_two,
  semantic_integer_cbor, stage, staged, staged_step, staged_step_at, zeros,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture

// ## Spending

fn spend(step: StagedStepV1) -> Bool {
  staged_integer.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    step.datum,
    step.redeemer,
    fixture.own_out_ref(),
    step.transaction,
  )
}

fn verifies(
  tag: Int,
  arguments: List<ValueWitnessV1>,
  result: ValueWitnessV1,
  staging: StagingV1,
  sources: List<OperandSourceV1>,
) -> Bool {
  spend(
    staged_step(
      tag,
      arguments,
      result,
      material(staging, sources),
      staging.reference_inputs,
    ),
  )
}

/// Builtin `tag` over `left` and `right`, both staged as canonical CBOR.
fn staged_pair(tag: Int, left: Int, right: Int, result: ValueWitnessV1) -> Bool {
  verifies(
    tag,
    [integer_value(left), integer_value(right)],
    result,
    stage([cbor.serialise(left), cbor.serialise(right)]),
    [staged(0), staged(1)],
  )
}

/// Builtin `tag` over a staged `left` and an inline `right`.
fn staged_and_inline(
  tag: Int,
  left: Int,
  right: Int,
  result: ValueWitnessV1,
) -> Bool {
  verifies(
    tag,
    [integer_value(left), integer_value(right)],
    result,
    stage([cbor.serialise(left)]),
    [staged(0), InlineOperand { bytes: cbor.serialise(right) }],
  )
}

/// The leaf preimage of `value`.
fn leaf(value: Int) -> OperandSourceV1 {
  let integer_cbor = cbor.serialise(value)
  IntegerLeafOperand {
    cbor_root: flat_root_v1(integer_cbor),
    cbor_length: builtin.length_of_bytearray(integer_cbor),
    words: magnitude_words_v1(parse_integer_cbor_bulk_v1(integer_cbor)),
  }
}

/// A positive integer of `bytes` magnitude bytes.
fn magnitude(bytes: Int, shift: Int) -> Int {
  builtin.bytearray_to_integer(True, pattern(bytes, shift))
}

/// 2^262142 - 1: 32,768 magnitude bytes, all 0xff below a 0x3f top byte.
fn half() -> Int {
  power_of_two(262142) - 1
}

/// 2^262143 - 1, the largest integer argument.
fn top() -> Int {
  half() + half() + 1
}

/// -2^262143, the smallest.
fn bottom() -> Int {
  -top() - 1
}

/// 2^262143, one past the largest. The test machine refuses arithmetic on
/// it, so it is built from its bytes.
fn past_top() -> Int {
  power_of_two(262143)
}

/// The canonical CBOR of -2^262143 - 1, one past the smallest (CBOR argument
/// 2^262143).
fn past_bottom_cbor() -> ByteArray {
  encode_integer_cbor_bulk_v1(
    CanonicalIntegerV1 {
      negative: True,
      argument: builtin.cons_bytearray(0x80, zeros(32767)),
    },
  )
}

/// An integer past the range of the bounded builtins (32,769 magnitude
/// bytes), given by `shift`.
fn huge(shift: Int) -> Int {
  magnitude(32769, shift)
}

// ## Honest steps verify

test adds_staged_operands_of_9216_bytes() {
  let left = magnitude(9216, 1)
  let right = magnitude(9216, 2)
  staged_pair(0, left, right, integer_value(left + right))
}

test subtracts_staged_operands_of_16_kilobytes_to_a_negative_result() {
  let left = magnitude(16384, 3)
  let right = magnitude(16384, 5)
  staged_pair(1, left, right, integer_value(left - right))
}

test adds_staged_operands_of_32_kilobytes_with_mixed_signs() {
  let left = magnitude(32768, 1)
  let right = -magnitude(32768, 2)
  staged_pair(0, left, right, integer_value(left + right))
}

test adds_with_a_carry_through_every_limb_at_the_argument_bound() {
  staged_pair(0, half(), half(), integer_value(half() + half()))
}

test adds_negative_operands_at_the_argument_bound() {
  staged_pair(0, -half(), -half(), integer_value(-half() - half()))
}

test subtracts_to_zero() {
  let value = magnitude(9216, 4)
  staged_pair(1, value, value, integer_value(0))
}

test adds_a_small_inline_operand_to_a_staged_operand() {
  let left = magnitude(16384, 1)
  staged_and_inline(0, left, 7, integer_value(left + 7))
}

test subtracts_a_staged_operand_from_a_small_inline_operand() {
  let right = magnitude(9216, 1)
  verifies(
    1,
    [integer_value(7), integer_value(right)],
    integer_value(7 - right),
    stage([cbor.serialise(right)]),
    [InlineOperand { bytes: cbor.serialise(7) }, staged(0)],
  )
}

test admits_the_largest_argument_and_result() {
  staged_and_inline(0, top(), 0, integer_value(top()))
}

test admits_the_smallest_argument_and_result() {
  staged_and_inline(1, bottom(), 0, integer_value(bottom()))
}

test reaches_the_largest_result_from_operands_in_range() {
  staged_pair(0, half() + 1, half(), integer_value(top()))
}

test reaches_the_smallest_result_from_operands_in_range() {
  staged_pair(1, -half() - 1, half() + 1, integer_value(bottom()))
}

test orders_staged_operands_at_the_argument_bound() {
  let left = magnitude(32768, 1)
  let right = magnitude(32768, 2)
  and {
    staged_pair(8, left, right, boolean_value(True)),
    staged_pair(8, right, left, boolean_value(False)),
    staged_pair(8, left, left, boolean_value(False)),
    staged_pair(9, left, left, boolean_value(True)),
    staged_pair(9, right, left, boolean_value(False)),
  }
}

test orders_negative_below_positive() {
  let value = magnitude(9216, 1)
  and {
    staged_pair(8, -value, value, boolean_value(True)),
    staged_pair(9, value, -value, boolean_value(False)),
    staged_pair(8, -value - 1, -value, boolean_value(True)),
  }
}

test equates_integers_past_the_argument_range_by_leaf() {
  let value = huge(1)
  let other = huge(2)
  and {
    verifies(
      7,
      [integer_value(value), integer_value(value)],
      boolean_value(True),
      no_staging(),
      [leaf(value), leaf(value)],
    ),
    verifies(
      7,
      [integer_value(value), integer_value(other)],
      boolean_value(False),
      no_staging(),
      [leaf(value), leaf(other)],
    ),
  }
}

test equates_staged_integers() {
  let value = magnitude(16384, 6)
  and {
    staged_pair(7, value, value, boolean_value(True)),
    staged_pair(7, value, -value, boolean_value(False)),
  }
}

/// Steps whose operands are all within the direct carriage bound verify on
/// this route too, with the result the direct route computes.
test agrees_with_the_direct_route_on_inline_operands() {
  let left = magnitude(4000, 1)
  let right = -magnitude(3000, 2)
  let inline = fn(tag, result) {
    verifies(
      tag,
      [integer_value(left), integer_value(right)],
      result,
      no_staging(),
      [
        InlineOperand { bytes: cbor.serialise(left) },
        InlineOperand { bytes: cbor.serialise(right) },
      ],
    )
  }
  and {
    inline(0, integer_value(left + right)),
    inline(1, integer_value(left - right)),
    inline(7, boolean_value(False)),
    inline(8, boolean_value(False)),
    inline(9, boolean_value(False)),
  }
}

// ## Wrong results are refused

test refuses_a_sum_missing_its_top_carry() fail {
  staged_pair(0, half(), half(), integer_value(half() + half() - half() - 1))
}

test refuses_a_sum_off_by_one() fail {
  staged_pair(0, half(), half(), integer_value(half() + half() + 1))
}

test refuses_a_difference_of_the_wrong_sign() fail {
  let left = magnitude(16384, 3)
  let right = magnitude(16384, 5)
  staged_pair(1, left, right, integer_value(right - left))
}

test refuses_a_wrong_comparison() fail {
  let left = magnitude(32768, 1)
  let right = magnitude(32768, 2)
  staged_pair(8, left, right, boolean_value(False))
}

test refuses_a_wrong_equality() fail {
  let value = huge(1)
  let other = huge(2)
  verifies(
    7,
    [integer_value(value), integer_value(other)],
    boolean_value(True),
    no_staging(),
    [leaf(value), leaf(other)],
  )
}

test refuses_a_sum_typed_as_a_byte_string() fail {
  let left = magnitude(9216, 1)
  let right = magnitude(9216, 2)
  staged_pair(0, left, right, bytes_value(cbor.serialise(left + right), False))
}

// ## Argument range (a result past it is a success)

test refuses_an_argument_one_past_the_largest() fail {
  staged_and_inline(0, past_top(), -1, integer_value(top()))
}

test refuses_an_argument_one_past_the_smallest() fail {
  let left_cbor = past_bottom_cbor()
  verifies(
    0,
    [semantic_integer_cbor(left_cbor), integer_value(1)],
    integer_value(bottom()),
    stage([left_cbor]),
    [staged(0), InlineOperand { bytes: cbor.serialise(1) }],
  )
}

test refuses_an_argument_of_32769_magnitude_bytes() fail {
  let value = magnitude(32769, 1)
  staged_and_inline(1, value, value, integer_value(0))
}

test refuses_an_order_of_an_argument_past_the_range() fail {
  staged_and_inline(8, past_top(), 0, boolean_value(False))
}

test admits_a_sum_one_past_the_largest() {
  staged_and_inline(0, top(), 1, integer_value(past_top()))
}

test admits_a_difference_one_past_the_smallest() {
  staged_and_inline(1, bottom(), 1, semantic_integer_cbor(past_bottom_cbor()))
}

test refuses_a_wrong_sum_one_past_the_largest() fail {
  staged_and_inline(0, top(), 1, integer_value(top()))
}

// ## Binding

test refuses_a_sum_given_leaf_operands() fail {
  let left = magnitude(9216, 1)
  let right = magnitude(9216, 2)
  verifies(
    0,
    [integer_value(left), integer_value(right)],
    integer_value(left + right),
    no_staging(),
    [leaf(left), leaf(right)],
  )
}

test refuses_staged_bytes_of_another_integer() fail {
  let left = magnitude(9216, 1)
  let right = magnitude(9216, 2)
  verifies(
    0,
    [integer_value(left), integer_value(right)],
    integer_value(left + 1 + right),
    stage([cbor.serialise(left + 1), cbor.serialise(right)]),
    [staged(0), staged(1)],
  )
}

test refuses_operands_given_in_swapped_order() fail {
  let left = magnitude(16384, 3)
  let right = magnitude(16384, 5)
  verifies(
    1,
    [integer_value(left), integer_value(right)],
    integer_value(right - left),
    stage([cbor.serialise(left), cbor.serialise(right)]),
    [staged(1), staged(0)],
  )
}

test refuses_a_non_canonical_inline_encoding() fail {
  verifies(
    0,
    [integer_value(7), integer_value(1)],
    integer_value(8),
    no_staging(),
    [
      InlineOperand { bytes: #"1807" },
      InlineOperand { bytes: cbor.serialise(1) },
    ],
  )
}

test refuses_a_leaf_with_a_wrong_word_count() fail {
  let value = huge(1)
  expect IntegerLeafOperand { cbor_root, cbor_length, words } = leaf(value)
  verifies(
    7,
    [integer_value(value), integer_value(value)],
    boolean_value(True),
    no_staging(),
    [
      IntegerLeafOperand { cbor_root, cbor_length, words: words + 1 },
      leaf(value),
    ],
  )
}

test refuses_a_leaf_with_a_wrong_cbor_length() fail {
  let value = huge(1)
  expect IntegerLeafOperand { cbor_root, cbor_length, words } = leaf(value)
  verifies(
    7,
    [integer_value(value), integer_value(value)],
    boolean_value(True),
    no_staging(),
    [
      IntegerLeafOperand { cbor_root, cbor_length: cbor_length - 1, words },
      leaf(value),
    ],
  )
}

test refuses_a_staged_operand_whose_memory_differs_from_its_words() fail {
  let left = magnitude(9216, 1)
  let right = magnitude(9216, 2)
  expect SemanticConstantValue { type_cbor, payload, memory } = semantic_integer_cbor(
    cbor.serialise(left),
  )
  verifies(
    0,
    [
      SemanticConstantValue { type_cbor, payload, memory: memory + 1 },
      integer_value(right),
    ],
    integer_value(left + right),
    stage([cbor.serialise(left), cbor.serialise(right)]),
    [staged(0), staged(1)],
  )
}

test refuses_a_leaf_operand_whose_memory_differs_from_its_words() fail {
  let value = huge(1)
  expect SemanticConstantValue { type_cbor, payload, memory } = semantic_integer_cbor(
    cbor.serialise(value),
  )
  verifies(
    7,
    [
      SemanticConstantValue { type_cbor, payload, memory: memory + 1 },
      integer_value(value),
    ],
    boolean_value(True),
    no_staging(),
    [leaf(value), leaf(value)],
  )
}

test refuses_an_integer_operand_typed_as_a_byte_string() fail {
  let value = magnitude(9216, 1)
  expect SemanticConstantValue { payload, memory, .. } = semantic_integer_cbor(
    cbor.serialise(value),
  )
  verifies(
    7,
    [
      SemanticConstantValue { type_cbor: bytes_type_cbor, payload, memory },
      integer_value(value),
    ],
    boolean_value(True),
    stage([cbor.serialise(value)]),
    [staged(0), staged(0)],
  )
}

test refuses_a_memory_matched_by_a_wrong_leaf_word_count() fail {
  let value = huge(1)
  expect IntegerLeafOperand { cbor_root, cbor_length, words } = leaf(value)
  expect SemanticConstantValue { type_cbor, payload, memory } = semantic_integer_cbor(
    cbor.serialise(value),
  )
  verifies(
    7,
    [
      SemanticConstantValue { type_cbor, payload, memory: memory + 1 },
      integer_value(value),
    ],
    boolean_value(True),
    no_staging(),
    [
      IntegerLeafOperand { cbor_root, cbor_length, words: words + 1 },
      leaf(value),
    ],
  )
}

// ## Routing

/// The budget hop for builtin `tag` over `arguments`, with the step it
/// continues to at semantics index `index`.
fn routes_to(tag: Int, arguments: List<ValueWitnessV1>, index: Int) -> Bool {
  let step = budget_step(tag, arguments, boolean_value(True))
  budget.main.spend(
    list.map(
      list.range(0, 8),
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

test the_budget_hop_charges_and_routes_every_builtin_here_at_the_bound() {
  let large = integer_value(top())
  let past = integer_value(huge(1))
  list.all(
    [
      Pair(0, [large, large]), Pair(1, [large, large]), Pair(7, [past, past]),
      Pair(8, [large, large]), Pair(9, [large, large]),
    ],
    fn(row) {
      let Pair(tag, arguments) = row
      routes_to(tag, arguments, 8)
    },
  )
}

test the_budget_hop_does_not_route_an_integer_builtin_to_the_byte_string_semantics() fail {
  routes_to(0, [integer_value(top()), integer_value(top())], 7)
}

test refuses_a_step_in_another_group() fail {
  let left = magnitude(9216, 1)
  let right = magnitude(9216, 2)
  let staging = stage([cbor.serialise(left), cbor.serialise(right)])
  spend(
    staged_step_at(
      0,
      [integer_value(left), integer_value(right)],
      integer_value(left + right),
      material(staging, [staged(0), staged(1)]),
      staging.reference_inputs,
      machine.mode_builtin,
      2,
      3,
    ),
  )
}

test refuses_a_byte_string_builtin() fail {
  let bytes = pattern(9216, 9)
  verifies(
    18,
    [bytes_value(bytes, False)],
    bytes_value(builtin.sha2_256(bytes), False),
    stage([bytes]),
    [staged(0)],
  )
}

// -2 * 2 and -2 - 2 agree, so only the tag tells them apart.
test refuses_a_multiplication() fail {
  staged_pair(2, -2, 2, integer_value(-4))
}

test refuses_a_step_before_the_budget_hop() fail {
  let left = magnitude(9216, 1)
  let right = magnitude(9216, 2)
  let staging = stage([cbor.serialise(left), cbor.serialise(right)])
  spend(
    staged_step_at(
      0,
      [integer_value(left), integer_value(right)],
      integer_value(left + right),
      material(staging, [staged(0), staged(1)]),
      staging.reference_inputs,
      machine.mode_builtin,
      5,
      2,
    ),
  )
}
