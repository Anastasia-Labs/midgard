//// Transaction-level rows for the staged product semantics step (arm 31,
//// group 5, semantics index 9): multiplyInteger, divideInteger,
//// quotientInteger, remainderInteger and modInteger over operands bound by
//// their canonical CBOR, inline or from staging outputs. A division also
//// carries its quotient's canonical CBOR. The step must accept exactly the
//// results Cardano returns.

use aiken/builtin
use aiken/cbor
use aiken/collection/list
use fraud_proofs/validation_trace/cek_core_builtin_budget as budget
use fraud_proofs/validation_trace/cek_core_staged_product as staged_product
use midgard/cek_builtin_v1.{ValueWitnessV1}
use midgard/cek_machine_v1 as machine
use midgard/cek_staged_arm_v1.{
  InlineOperand, IntegerLeafOperand, OperandSourceV1, SmallIntegerOperand,
}
use midgard/cek_staged_blob_v1.{flat_root_v1}
use midgard/cek_staged_integer_cbor_v1.{
  encode_integer_cbor_bulk_v1, parse_integer_cbor_bulk_v1,
}
use midgard/cek_staged_integer_v1.{CanonicalIntegerV1, magnitude_words_v1}
use midgard/fraud_proofs/cek_staged_fixture_v1.{
  StagedStepV1, StagingV1, boolean_value, budget_step, flip_last_byte,
  integer_value, material, no_staging, pattern, power_of_two,
  semantic_integer_cbor, stage, staged, staged_step, staged_step_at, zeros,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture

// ## Spending

fn spend(step: StagedStepV1) -> Bool {
  staged_product.main.spend(
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

/// multiplyInteger over `left` and `right`, both staged.
fn multiplies(left: Int, right: Int, result: ValueWitnessV1) -> Bool {
  verifies(
    2,
    [integer_value(left), integer_value(right)],
    result,
    stage([cbor.serialise(left), cbor.serialise(right)]),
    [staged(0), staged(1)],
  )
}

/// Division `tag` of `dividend` by `divisor` with quotient `quotient`, all
/// three staged.
fn divides(
  tag: Int,
  dividend: Int,
  divisor: Int,
  quotient: Int,
  result: ValueWitnessV1,
) -> Bool {
  verifies(
    tag,
    [integer_value(dividend), integer_value(divisor)],
    result,
    stage(
      [
        cbor.serialise(dividend),
        cbor.serialise(divisor),
        cbor.serialise(quotient),
      ],
    ),
    [staged(0), staged(1), staged(2)],
  )
}

/// The quotient division `tag` takes: floored for divideInteger and
/// modInteger, truncated for quotientInteger and remainderInteger.
fn quotient_of(tag: Int, dividend: Int, divisor: Int) -> Int {
  if tag == 4 || tag == 5 {
    builtin.quotient_integer(dividend, divisor)
  } else {
    builtin.divide_integer(dividend, divisor)
  }
}

/// Cardano's result of division `tag`.
fn division(tag: Int, dividend: Int, divisor: Int) -> Int {
  if tag == 3 {
    builtin.divide_integer(dividend, divisor)
  } else if tag == 4 {
    builtin.quotient_integer(dividend, divisor)
  } else if tag == 5 {
    builtin.remainder_integer(dividend, divisor)
  } else {
    builtin.mod_integer(dividend, divisor)
  }
}

/// Division `tag` of `dividend` by `divisor` with Cardano's quotient and
/// result.
fn divides_correctly(tag: Int, dividend: Int, divisor: Int) -> Bool {
  divides(
    tag,
    dividend,
    divisor,
    quotient_of(tag, dividend, divisor),
    integer_value(division(tag, dividend, divisor)),
  )
}

/// Every sign combination of `dividend` and `divisor` (both positive).
fn signs(dividend: Int, divisor: Int) -> List<Pair<Int, Int>> {
  [
    Pair(dividend, divisor),
    Pair(-dividend, divisor),
    Pair(dividend, -divisor),
    Pair(-dividend, -divisor),
  ]
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

/// `value`'s canonical CBOR with the last byte of its CBOR argument changed:
/// another integer of the same size and sign.
fn other_of_same_size(value: Int) -> ValueWitnessV1 {
  let integer = parse_integer_cbor_bulk_v1(cbor.serialise(value))
  semantic_integer_cbor(
    encode_integer_cbor_bulk_v1(
      CanonicalIntegerV1 {
        ..integer,
        argument: flip_last_byte(integer.argument),
      },
    ),
  )
}

// ## multiplyInteger

test multiplies_staged_operands_in_every_sign_combination() {
  let left = magnitude(9216, 1)
  let right = magnitude(4000, 2)
  list.all(
    signs(left, right),
    fn(row) {
      let Pair(a, b) = row
      multiplies(a, b, integer_value(a * b))
    },
  )
}

test multiplies_with_a_carry_through_every_limb() {
  let all_ones = power_of_two(32768) - 1
  multiplies(all_ones, all_ones, integer_value(all_ones * all_ones))
}

test multiplies_zero_by_a_negative_operand_to_a_canonical_zero() {
  let negative = -magnitude(9216, 3)
  and {
    multiplies(0, negative, integer_value(0)),
    multiplies(negative, 0, integer_value(0)),
  }
}

test multiplies_a_staged_operand_by_a_small_inline_operand() {
  let left = magnitude(16384, 1)
  verifies(
    2,
    [integer_value(left), integer_value(-3)],
    integer_value(left * -3),
    stage([cbor.serialise(left)]),
    [staged(0), InlineOperand { bytes: cbor.serialise(-3) }],
  )
}

test admits_the_largest_and_smallest_arguments_of_a_product() {
  and {
    multiplies(top(), 1, integer_value(top())),
    multiplies(-1, bottom(), integer_value(past_top())),
  }
}

test admits_a_product_past_the_largest() {
  multiplies(top(), 2, integer_value(top() * 2))
}

test refuses_a_product_of_the_wrong_sign() fail {
  let left = magnitude(9216, 1)
  let right = magnitude(4000, 2)
  multiplies(-left, right, integer_value(left * right))
}

test refuses_a_product_missing_its_top_bit() fail {
  let all_ones = power_of_two(32768) - 1
  let product = all_ones * all_ones
  multiplies(all_ones, all_ones, integer_value(product - power_of_two(65535)))
}

test refuses_a_product_off_by_one() fail {
  let left = magnitude(9216, 1)
  let right = magnitude(4000, 2)
  multiplies(left, right, integer_value(left * right + 1))
}

test refuses_a_wrong_product_past_the_largest() fail {
  multiplies(top(), 2, other_of_same_size(top() * 2))
}

test refuses_a_multiplicand_one_past_the_largest() fail {
  multiplies(past_top(), 1, integer_value(past_top()))
}

test refuses_a_multiplier_one_past_the_largest() fail {
  multiplies(1, past_top(), integer_value(past_top()))
}

test refuses_a_multiplicand_one_past_the_smallest() fail {
  let left_cbor = past_bottom_cbor()
  verifies(
    2,
    [semantic_integer_cbor(left_cbor), integer_value(1)],
    semantic_integer_cbor(left_cbor),
    stage([left_cbor]),
    [staged(0), InlineOperand { bytes: cbor.serialise(1) }],
  )
}

test refuses_a_product_with_a_quotient_source() fail {
  let left = magnitude(9216, 1)
  verifies(
    2,
    [integer_value(left), integer_value(2)],
    integer_value(left * 2),
    stage([cbor.serialise(left), cbor.serialise(2), cbor.serialise(0)]),
    [staged(0), staged(1), staged(2)],
  )
}

test refuses_a_product_given_leaf_operands() fail {
  let left = magnitude(9216, 1)
  let right = magnitude(9216, 2)
  verifies(
    2,
    [integer_value(left), integer_value(right)],
    integer_value(left * right),
    no_staging(),
    [leaf(left), leaf(right)],
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

// ## Divisions

test divides_in_every_sign_combination_with_a_remainder() {
  let dividend = magnitude(9216, 1)
  let divisor = magnitude(4000, 2)
  list.all(
    [3, 4, 5, 6],
    fn(tag) {
      list.all(
        signs(dividend, divisor),
        fn(row) {
          let Pair(a, b) = row
          divides_correctly(tag, a, b)
        },
      )
    },
  )
}

test divides_in_every_sign_combination_without_a_remainder() {
  let divisor = magnitude(4000, 2)
  let dividend = divisor * magnitude(5000, 3)
  list.all(
    [3, 4, 5, 6],
    fn(tag) {
      list.all(
        signs(dividend, divisor),
        fn(row) {
          let Pair(a, b) = row
          divides_correctly(tag, a, b)
        },
      )
    },
  )
}

test divides_a_smaller_dividend_to_a_zero_quotient() {
  let dividend = magnitude(4000, 1)
  let divisor = magnitude(9216, 2)
  list.all(
    [3, 4, 5, 6],
    fn(tag) {
      list.all(
        signs(dividend, divisor),
        fn(row) {
          let Pair(a, b) = row
          divides_correctly(tag, a, b)
        },
      )
    },
  )
}

test admits_a_remainder_one_below_the_divisor() {
  let divisor = magnitude(4000, 2)
  let dividend = divisor * 5 + divisor - 1
  and {
    divides_correctly(5, dividend, divisor),
    divides_correctly(5, -dividend, divisor),
    divides_correctly(6, dividend, divisor),
    divides_correctly(6, dividend, -divisor),
  }
}

test divides_a_small_inline_dividend_by_a_staged_divisor() {
  let divisor = magnitude(16384, 1)
  verifies(
    6,
    [integer_value(-7), integer_value(divisor)],
    integer_value(divisor - 7),
    stage([cbor.serialise(divisor)]),
    [
      InlineOperand { bytes: cbor.serialise(-7) },
      staged(0),
      InlineOperand { bytes: cbor.serialise(-1) },
    ],
  )
}

test admits_the_largest_and_smallest_arguments_of_a_division() {
  and {
    divides_correctly(3, top(), 3),
    divides_correctly(6, bottom(), 7),
    divides_correctly(4, 7, bottom()),
    divides_correctly(5, -7, top()),
  }
}

test admits_a_quotient_one_past_the_largest() {
  and {
    divides(3, bottom(), -1, past_top(), integer_value(past_top())),
    divides(4, bottom(), -1, past_top(), integer_value(past_top())),
    divides(5, bottom(), -1, past_top(), integer_value(0)),
    divides(6, bottom(), -1, past_top(), integer_value(0)),
  }
}

test refuses_a_remainder_equal_to_the_divisor() fail {
  let divisor = magnitude(4000, 2)
  let dividend = divisor * 6
  divides(3, dividend, divisor, 5, integer_value(5))
}

test refuses_a_quotient_result_one_above() fail {
  let dividend = magnitude(9216, 1)
  let divisor = magnitude(4000, 2)
  let quotient = dividend / divisor + 1
  divides(3, dividend, divisor, quotient, integer_value(quotient))
}

test refuses_a_quotient_result_one_below() fail {
  let dividend = -magnitude(9216, 1)
  let divisor = magnitude(4000, 2)
  let quotient = builtin.quotient_integer(dividend, divisor) - 1
  divides(4, dividend, divisor, quotient, integer_value(quotient))
}

test refuses_a_remainder_from_a_quotient_one_above() fail {
  let dividend = magnitude(9216, 1)
  let divisor = magnitude(4000, 2)
  let quotient = builtin.quotient_integer(dividend, divisor) + 1
  divides(
    5,
    dividend,
    divisor,
    quotient,
    integer_value(dividend - quotient * divisor),
  )
}

test refuses_a_remainder_from_a_quotient_one_below() fail {
  let dividend = magnitude(9216, 1)
  let divisor = -magnitude(4000, 2)
  let quotient = dividend / divisor - 1
  divides(
    6,
    dividend,
    divisor,
    quotient,
    integer_value(dividend - quotient * divisor),
  )
}

test refuses_a_wrong_quotient_result_with_the_right_quotient() fail {
  let dividend = magnitude(9216, 1)
  let divisor = magnitude(4000, 2)
  let quotient = builtin.quotient_integer(dividend, divisor)
  divides(4, dividend, divisor, quotient, integer_value(quotient + 1))
}

test refuses_a_truncated_quotient_for_divide_integer() fail {
  let dividend = -magnitude(9216, 1)
  let divisor = magnitude(4000, 2)
  let quotient = builtin.quotient_integer(dividend, divisor)
  divides(3, dividend, divisor, quotient, integer_value(quotient))
}

test refuses_a_floored_quotient_for_quotient_integer() fail {
  let dividend = -magnitude(9216, 1)
  let divisor = magnitude(4000, 2)
  let quotient = builtin.divide_integer(dividend, divisor)
  divides(4, dividend, divisor, quotient, integer_value(quotient))
}

test refuses_a_floored_remainder_for_remainder_integer() fail {
  let dividend = magnitude(9216, 1)
  let divisor = -magnitude(4000, 2)
  divides(
    5,
    dividend,
    divisor,
    builtin.divide_integer(dividend, divisor),
    integer_value(builtin.mod_integer(dividend, divisor)),
  )
}

test refuses_a_truncated_remainder_for_mod_integer() fail {
  let dividend = magnitude(9216, 1)
  let divisor = -magnitude(4000, 2)
  divides(
    6,
    dividend,
    divisor,
    builtin.quotient_integer(dividend, divisor),
    integer_value(builtin.remainder_integer(dividend, divisor)),
  )
}

test refuses_a_wrong_remainder_with_the_right_quotient() fail {
  let dividend = magnitude(9216, 1)
  let divisor = magnitude(4000, 2)
  divides(
    6,
    dividend,
    divisor,
    dividend / divisor,
    integer_value(builtin.mod_integer(dividend, divisor) + 1),
  )
}

test refuses_division_by_zero_with_divide_integer() fail {
  divides(3, magnitude(9216, 1), 0, 0, integer_value(0))
}

test refuses_division_by_zero_with_quotient_integer() fail {
  divides(4, magnitude(9216, 1), 0, 0, integer_value(0))
}

test refuses_division_by_zero_with_remainder_integer() fail {
  let dividend = magnitude(9216, 1)
  divides(5, dividend, 0, 0, integer_value(dividend))
}

test refuses_division_by_zero_with_mod_integer() fail {
  let dividend = magnitude(9216, 1)
  divides(6, dividend, 0, 0, integer_value(dividend))
}

test refuses_a_dividend_one_past_the_largest() fail {
  divides(3, past_top(), 1, past_top(), integer_value(past_top()))
}

test refuses_a_dividend_one_past_the_smallest() fail {
  let dividend_cbor = past_bottom_cbor()
  verifies(
    4,
    [semantic_integer_cbor(dividend_cbor), integer_value(1)],
    semantic_integer_cbor(dividend_cbor),
    stage([dividend_cbor, dividend_cbor]),
    [staged(0), InlineOperand { bytes: cbor.serialise(1) }, staged(1)],
  )
}

test refuses_a_divisor_one_past_the_largest() fail {
  divides(5, 1, past_top(), 0, integer_value(1))
}

test refuses_a_divisor_one_past_the_smallest() fail {
  let divisor_cbor = past_bottom_cbor()
  verifies(
    6,
    [integer_value(-1), semantic_integer_cbor(divisor_cbor)],
    integer_value(-1),
    stage([divisor_cbor]),
    [
      InlineOperand { bytes: cbor.serialise(-1) },
      staged(0),
      InlineOperand { bytes: cbor.serialise(0) },
    ],
  )
}

test refuses_a_division_without_a_quotient() fail {
  let dividend = magnitude(9216, 1)
  let divisor = magnitude(4000, 2)
  verifies(
    3,
    [integer_value(dividend), integer_value(divisor)],
    integer_value(dividend / divisor),
    stage([cbor.serialise(dividend), cbor.serialise(divisor)]),
    [staged(0), staged(1)],
  )
}

test refuses_a_division_with_a_second_quotient() fail {
  let dividend = magnitude(9216, 1)
  let divisor = magnitude(4000, 2)
  let quotient = dividend / divisor
  verifies(
    6,
    [integer_value(dividend), integer_value(divisor)],
    integer_value(dividend % divisor),
    stage(
      [
        cbor.serialise(dividend),
        cbor.serialise(divisor),
        cbor.serialise(quotient),
      ],
    ),
    [staged(0), staged(1), staged(2), staged(2)],
  )
}

test refuses_a_quotient_given_by_its_leaf() fail {
  let dividend = magnitude(9216, 1)
  let divisor = magnitude(4000, 2)
  let quotient = dividend / divisor
  verifies(
    3,
    [integer_value(dividend), integer_value(divisor)],
    integer_value(quotient),
    stage([cbor.serialise(dividend), cbor.serialise(divisor)]),
    [staged(0), staged(1), leaf(quotient)],
  )
}

test refuses_a_quotient_given_as_a_small_integer() fail {
  let dividend = magnitude(9216, 1)
  let divisor = magnitude(9000, 2)
  verifies(
    5,
    [integer_value(dividend), integer_value(divisor)],
    integer_value(dividend % divisor),
    stage([cbor.serialise(dividend), cbor.serialise(divisor)]),
    [staged(0), staged(1), SmallIntegerOperand { value: dividend / divisor }],
  )
}

test refuses_a_non_canonical_quotient() fail {
  verifies(
    6,
    [integer_value(70), integer_value(9)],
    integer_value(7),
    no_staging(),
    [
      InlineOperand { bytes: cbor.serialise(70) },
      InlineOperand { bytes: cbor.serialise(9) },
      InlineOperand { bytes: #"1807" },
    ],
  )
}

// ## The direct route agrees

/// Steps whose operands are all within the direct carriage bound verify on
/// this route too, with the result the direct route computes.
test agrees_with_the_direct_route_on_inline_operands() {
  let left = magnitude(3000, 1)
  let right = -magnitude(1000, 2)
  let inline = fn(tag, sources, result) {
    and {
      verifies(
        tag,
        [integer_value(left), integer_value(right)],
        integer_value(result),
        no_staging(),
        [
          InlineOperand { bytes: cbor.serialise(left) },
          InlineOperand { bytes: cbor.serialise(right) },
          ..sources
        ],
      ),
      cek_builtin_v1.verify_direct_scalar_semantics(
        tag,
        [integer_value(left), integer_value(right)],
        integer_value(result),
      ),
    }
  }
  let quotient = fn(tag) {
    [InlineOperand { bytes: cbor.serialise(quotient_of(tag, left, right)) }]
  }
  and {
    inline(2, [], left * right),
    inline(3, quotient(3), division(3, left, right)),
    inline(4, quotient(4), division(4, left, right)),
    inline(5, quotient(5), division(5, left, right)),
    inline(6, quotient(6), division(6, left, right)),
  }
}

// ## Routing

/// The budget hop for builtin `tag` over `arguments`, with the step it
/// continues to at semantics index `index`.
fn routes_to(tag: Int, arguments: List<ValueWitnessV1>, index: Int) -> Bool {
  let step = budget_step(tag, arguments, boolean_value(True))
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

test the_budget_hop_charges_and_routes_every_builtin_here_at_the_bound() {
  let large = integer_value(top())
  list.all([2, 3, 4, 5, 6], fn(tag) { routes_to(tag, [large, large], 9) })
}

test the_budget_hop_does_not_route_a_product_to_the_linear_integer_semantics() fail {
  routes_to(2, [integer_value(top()), integer_value(top())], 8)
}

test the_budget_hop_does_not_route_a_sum_here() fail {
  routes_to(0, [integer_value(top()), integer_value(top())], 9)
}

// With a zero quotient, the dividend is its own remainder, so only the tag
// keeps a sum out.
test refuses_a_linear_integer_builtin() fail {
  let left = magnitude(4000, 1)
  let right = magnitude(9216, 2)
  verifies(
    0,
    [integer_value(left), integer_value(right)],
    integer_value(left + right),
    stage([cbor.serialise(left), cbor.serialise(right), cbor.serialise(0)]),
    [staged(0), staged(1), staged(2)],
  )
}

test refuses_a_step_in_another_group() fail {
  let left = magnitude(9216, 1)
  let right = magnitude(9216, 2)
  let staging = stage([cbor.serialise(left), cbor.serialise(right)])
  spend(
    staged_step_at(
      2,
      [integer_value(left), integer_value(right)],
      integer_value(left * right),
      material(staging, [staged(0), staged(1)]),
      staging.reference_inputs,
      machine.mode_builtin,
      2,
      3,
    ),
  )
}

test refuses_a_step_before_the_budget_hop() fail {
  let left = magnitude(9216, 1)
  let right = magnitude(9216, 2)
  let staging = stage([cbor.serialise(left), cbor.serialise(right)])
  spend(
    staged_step_at(
      2,
      [integer_value(left), integer_value(right)],
      integer_value(left * right),
      material(staging, [staged(0), staged(1)]),
      staging.reference_inputs,
      machine.mode_builtin,
      5,
      2,
    ),
  )
}
