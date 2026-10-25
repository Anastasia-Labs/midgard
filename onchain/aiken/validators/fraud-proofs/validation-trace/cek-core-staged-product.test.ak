//// Transaction-level rows for the staged product semantics step (arm 31,
//// group 5, semantics index 9): multiplyInteger, divideInteger,
//// quotientInteger, remainderInteger and modInteger over operands bound by
//// their canonical CBOR, inline or from staging outputs. A division also
//// carries its quotient's canonical CBOR. The step runs as a loop over the
//// columns of a product, a range of columns per transaction, and must accept
//// exactly the results Cardano returns.

use aiken/builtin
use aiken/cbor
use aiken/collection/list
use cardano/address
use cardano/transaction.{InlineDatum, Input, Output, Transaction}
use fraud_proofs/validation_trace/cek_core_builtin_budget as budget
use fraud_proofs/validation_trace/cek_core_staged_product as staged_product
use midgard/cek_builtin_v1.{SemanticConstantValue, ValueWitnessV1}
use midgard/cek_semantic_chain as chain
use midgard/cek_staged_arm_v1.{
  InlineOperand, IntegerLeafOperand, OperandSourceV1, SmallIntegerOperand,
}
use midgard/cek_staged_blob_v1.{flat_root_of_pieces_v1, flat_root_v1}
use midgard/cek_staged_integer_cbor_v1.{
  encode_integer_cbor_bulk_v1, integer_cbor_pieces_from_magnitude_v1,
  parse_integer_cbor_bulk_v1,
}
use midgard/cek_staged_integer_v1.{
  CanonicalIntegerV1, SignedLimbsV1, integer_leaf_from_cbor_v1,
  magnitude_words_v1, mul_limbs_v1, signed_limbs_v1,
}
use midgard/cek_staged_product_v1.{ProductStateV1, advance_product_v1}
use midgard/computation_thread as ct
use midgard/fraud_proofs/cek_staged_fixture_v1.{
  LoopStepV1, StagingV1, arm_facts, arm_witness, boolean_value, budget_step,
  flip_last_byte, integer_type_cbor, integer_value, loop_facts, loop_step,
  material, no_staging, pattern, power_of_two, semantic_integer_cbor, stage,
  stage_pieces, staged, zeros,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture

// ## The loop

fn spend(step: LoopStepV1) -> Bool {
  staged_product.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    step.datum,
    step.redeemer,
    fixture.own_out_ref(),
    step.transaction,
  )
}

/// A step of builtin `tag`: its witness, the facts the roots hop committed and
/// the reference inputs its hops read.
type CaseV1 {
  witness: Data,
  facts: Data,
  reference_inputs: List<Input>,
}

fn case_of(
  tag: Int,
  arguments: List<ValueWitnessV1>,
  result: ValueWitnessV1,
  staging: StagingV1,
  sources: List<OperandSourceV1>,
) -> CaseV1 {
  CaseV1 {
    witness: arm_witness(tag, arguments, result, material(staging, sources)),
    facts: arm_facts(arguments, result),
    reference_inputs: staging.reference_inputs,
  }
}

/// The state a hop over [cursor, end) of `case` leaves, from `from`.
fn state_after(case: CaseV1, from: Option<Pair<Int, Data>>, end: Int) -> Data {
  expect Some(state) = advance_product_v1(
    31,
    case.facts,
    case.witness,
    case.reference_inputs,
    from,
    end,
  )
  state
}

fn hop_step(
  case: CaseV1,
  from: Option<Pair<Int, Data>>,
  end: Int,
  next: Option<Data>,
) -> LoopStepV1 {
  loop_step(case.witness, case.facts, case.reference_inputs, from, end, next)
}

fn hop(
  case: CaseV1,
  from: Option<Pair<Int, Data>>,
  end: Int,
  next: Option<Data>,
) -> Bool {
  spend(hop_step(case, from, end, next))
}

/// `case`'s hops ending at `ends`, from `from`; the last of `ends` ends the
/// loop when `finish` holds. Each hop's output carries the state the same hop
/// leaves for `reference`.
fn hops(
  case: CaseV1,
  reference: CaseV1,
  from: Option<Pair<Int, Data>>,
  ends: List<Int>,
  finish: Bool,
) -> Bool {
  when ends is {
    [] -> True
    [end, ..rest] ->
      if finish && rest == [] {
        hop(case, from, end, None)
      } else {
        let state = state_after(reference, from, end)
        and {
          hop(case, from, end, Some(state)),
          hops(case, reference, Some(Pair(end, state)), rest, finish),
        }
      }
  }
}

/// The whole loop of `case` over the hops ending at `ends`.
fn completes(case: CaseV1, ends: List<Int>) -> Bool {
  hops(case, case, None, ends, True)
}

/// Hops ending at 1, about half way and at `count`: a first, a middle and a
/// last hop when there are enough columns.
fn ends_of(count: Int) -> List<Int> {
  if count >= 3 {
    [1, count / 2 + 1, count]
  } else {
    list.range(1, count)
  }
}

fn limb_count(integer_cbor: ByteArray) -> Int {
  list.length(
    signed_limbs_v1(parse_integer_cbor_bulk_v1(integer_cbor)).magnitude,
  )
}

/// The column count of a product of `a` by `b`.
fn product_columns(a: ByteArray, b: ByteArray) -> Int {
  let count = limb_count(a) + limb_count(b)
  if count == 0 {
    1
  } else {
    count
  }
}

/// The column count of a division of `a` by `b` with quotient `q`.
fn division_columns(a: ByteArray, b: ByteArray, q: ByteArray) -> Int {
  let quotient_product = limb_count(q) + limb_count(b)
  let dividend = limb_count(a)
  if dividend > quotient_product {
    dividend
  } else if quotient_product == 0 {
    1
  } else {
    quotient_product
  }
}

// ## Values

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
fn other_of_same_size(value: Int) -> ByteArray {
  let integer = parse_integer_cbor_bulk_v1(cbor.serialise(value))
  encode_integer_cbor_bulk_v1(
    CanonicalIntegerV1 { ..integer, argument: flip_last_byte(integer.argument) },
  )
}

/// The integer whose canonical CBOR is `pieces`, root-only, with `words`
/// memory words.
fn semantic_integer_pieces(
  pieces: List<ByteArray>,
  words: Int,
) -> ValueWitnessV1 {
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

/// The leaf preimage of `value`.
fn leaf(value: Int) -> OperandSourceV1 {
  let integer_cbor = cbor.serialise(value)
  IntegerLeafOperand {
    cbor_root: flat_root_v1(integer_cbor),
    cbor_length: builtin.length_of_bytearray(integer_cbor),
    words: magnitude_words_v1(parse_integer_cbor_bulk_v1(integer_cbor)),
  }
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

// ## multiplyInteger

/// multiplyInteger over `a` and `b` (staged) with the result `product` (its
/// canonical CBOR, staged).
fn product_case(a: Int, b: Int, product: ByteArray) -> CaseV1 {
  case_of(
    2,
    [integer_value(a), integer_value(b)],
    semantic_integer_cbor(product),
    stage([cbor.serialise(a), cbor.serialise(b), product]),
    [staged(0), staged(1), staged(2)],
  )
}

/// multiplyInteger over a first, a middle and a last hop.
fn multiplies(a: Int, b: Int, product: ByteArray) -> Bool {
  completes(
    product_case(a, b, product),
    ends_of(product_columns(cbor.serialise(a), cbor.serialise(b))),
  )
}

/// multiplyInteger in one hop.
fn multiplies_at_once(a: Int, b: Int, product: ByteArray) -> Bool {
  completes(
    product_case(a, b, product),
    [product_columns(cbor.serialise(a), cbor.serialise(b))],
  )
}

test multiplies_staged_operands_in_every_sign_combination() {
  let left = magnitude(9216, 1)
  let right = magnitude(4000, 2)
  list.all(
    signs(left, right),
    fn(row) {
      let Pair(a, b) = row
      and {
        multiplies(a, b, cbor.serialise(a * b)),
        multiplies_at_once(a, b, cbor.serialise(a * b)),
      }
    },
  )
}

test multiplies_small_operands_in_one_column() {
  list.all(
    signs(7, 3),
    fn(row) {
      let Pair(a, b) = row
      multiplies(a, b, cbor.serialise(a * b))
    },
  )
}

test multiplies_with_a_carry_through_every_limb() {
  let all_ones = power_of_two(32768) - 1
  multiplies(all_ones, all_ones, cbor.serialise(all_ones * all_ones))
}

test multiplies_zero_and_a_negative_operand_to_a_canonical_zero() {
  let negative = -magnitude(9216, 3)
  and {
    multiplies(0, negative, cbor.serialise(0)),
    multiplies(negative, 0, cbor.serialise(0)),
    multiplies(0, 0, cbor.serialise(0)),
  }
}

test multiplies_to_minus_one_and_to_a_negative_power_of_256() {
  let power = power_of_two(32768)
  and {
    multiplies(-1, 1, cbor.serialise(-1)),
    multiplies(-power, 1, cbor.serialise(-power)),
    multiplies(power, -256, cbor.serialise(power * -256)),
  }
}

test multiplies_a_staged_operand_by_a_small_inline_operand() {
  let left = magnitude(16384, 1)
  let product = left * -3
  completes(
    case_of(
      2,
      [integer_value(left), integer_value(-3)],
      integer_value(product),
      stage([cbor.serialise(left), cbor.serialise(product)]),
      [staged(0), InlineOperand { bytes: cbor.serialise(-3) }, staged(1)],
    ),
    [1, 33],
  )
}

test admits_the_largest_and_smallest_arguments_of_a_product() {
  and {
    multiplies(top(), 1, cbor.serialise(top())),
    multiplies(-1, bottom(), cbor.serialise(past_top())),
  }
}

test admits_a_product_past_the_largest() {
  multiplies(top(), 2, cbor.serialise(top() * 2))
}

test refuses_a_product_of_the_wrong_sign() fail {
  let left = magnitude(9216, 1)
  let right = magnitude(4000, 2)
  multiplies_at_once(-left, right, cbor.serialise(left * right))
}

test refuses_a_negative_zero_product() fail {
  multiplies_at_once(0, -5, cbor.serialise(-1))
}

test refuses_a_product_missing_its_top_bit() fail {
  let all_ones = power_of_two(32768) - 1
  let product = all_ones * all_ones
  multiplies_at_once(
    all_ones,
    all_ones,
    cbor.serialise(product - power_of_two(65535)),
  )
}

test refuses_a_product_off_by_one() fail {
  let left = magnitude(9216, 1)
  let right = magnitude(4000, 2)
  multiplies_at_once(left, right, cbor.serialise(left * right + 1))
}

test refuses_a_negative_product_off_by_one() fail {
  let left = magnitude(9216, 1)
  let right = magnitude(4000, 2)
  multiplies_at_once(-left, right, cbor.serialise(-left * right - 1))
}

// The product of two one-limb operands has two columns; the claimed result
// matches it in both and has one more limb.
test refuses_a_product_with_a_limb_past_its_columns() fail {
  let left = magnitude(512, 1)
  let right = magnitude(512, 2)
  multiplies_at_once(
    left,
    right,
    cbor.serialise(left * right + power_of_two(8192)),
  )
}

test refuses_a_wrong_product_past_the_largest() fail {
  multiplies_at_once(top(), 2, other_of_same_size(top() * 2))
}

test refuses_a_product_whose_leaf_has_the_wrong_memory() fail {
  let left = magnitude(9216, 1)
  let product = cbor.serialise(left * 2)
  let words = magnitude_words_v1(parse_integer_cbor_bulk_v1(product)) + 1
  completes(
    case_of(
      2,
      [integer_value(left), integer_value(2)],
      semantic_integer_pieces([product], words),
      stage([cbor.serialise(left), product]),
      [staged(0), InlineOperand { bytes: cbor.serialise(2) }, staged(1)],
    ),
    [19],
  )
}

test refuses_a_multiplicand_one_past_the_largest() fail {
  multiplies_at_once(past_top(), 1, cbor.serialise(past_top()))
}

test refuses_a_multiplier_one_past_the_largest() fail {
  multiplies_at_once(1, past_top(), cbor.serialise(past_top()))
}

test refuses_a_multiplicand_one_past_the_smallest() fail {
  let left_cbor = past_bottom_cbor()
  completes(
    case_of(
      2,
      [semantic_integer_cbor(left_cbor), integer_value(1)],
      semantic_integer_cbor(left_cbor),
      stage([left_cbor, left_cbor]),
      [staged(0), InlineOperand { bytes: cbor.serialise(1) }, staged(1)],
    ),
    [65],
  )
}

test refuses_a_product_with_a_quotient_source() fail {
  let left = magnitude(9216, 1)
  let product = cbor.serialise(left * 2)
  completes(
    case_of(
      2,
      [integer_value(left), integer_value(2)],
      semantic_integer_cbor(product),
      stage(
        [cbor.serialise(left), cbor.serialise(2), cbor.serialise(0), product],
      ),
      [staged(0), staged(1), staged(2), staged(3)],
    ),
    [19],
  )
}

test refuses_a_product_without_its_result() fail {
  let left = magnitude(9216, 1)
  completes(
    case_of(
      2,
      [integer_value(left), integer_value(2)],
      integer_value(left * 2),
      stage([cbor.serialise(left), cbor.serialise(2)]),
      [staged(0), staged(1)],
    ),
    [19],
  )
}

test refuses_a_product_given_leaf_operands() fail {
  let left = magnitude(9216, 1)
  let right = magnitude(9216, 2)
  let product = cbor.serialise(left * right)
  completes(
    case_of(
      2,
      [integer_value(left), integer_value(right)],
      semantic_integer_cbor(product),
      stage([product]),
      [leaf(left), leaf(right), staged(0)],
    ),
    [36],
  )
}

test refuses_a_product_given_its_result_leaf() fail {
  let left = magnitude(9216, 1)
  completes(
    case_of(
      2,
      [integer_value(left), integer_value(2)],
      integer_value(left * 2),
      stage([cbor.serialise(left)]),
      [staged(0), InlineOperand { bytes: cbor.serialise(2) }, leaf(left * 2)],
    ),
    [19],
  )
}

// ## The largest product

/// The canonical CBOR of top()^2 (65,536 magnitude bytes), as pieces: the
/// test machine holds no byte string that long.
fn largest_product_pieces() -> List<ByteArray> {
  let limbs = signed_limbs_v1(parse_integer_cbor_bulk_v1(cbor.serialise(top()))).magnitude
  integer_cbor_pieces_from_magnitude_v1(
    SignedLimbsV1 { negative: False, magnitude: mul_limbs_v1(limbs, limbs) },
  )
}

/// multiplyInteger of top() by itself with the result `pieces`.
fn largest_product_case(pieces: List<ByteArray>) -> CaseV1 {
  let a = cbor.serialise(top())
  case_of(
    2,
    [integer_value(top()), integer_value(top())],
    semantic_integer_pieces(pieces, 8192),
    stage_pieces([[a], [a], pieces]),
    [staged(0), staged(1), staged(2)],
  )
}

/// `pieces` with the byte at offset `at` of their concatenation changed.
fn change_byte(pieces: List<ByteArray>, at: Int) -> List<ByteArray> {
  expect [piece, ..rest] = pieces
  let length = builtin.length_of_bytearray(piece)
  if at >= length {
    [piece, ..change_byte(rest, at - length)]
  } else {
    let byte = builtin.index_bytearray(piece, at)
    [
      builtin.append_bytearray(
        builtin.slice_bytearray(0, at, piece),
        builtin.cons_bytearray(
          if byte == 0 {
            1
          } else {
            byte - 1
          },
          builtin.slice_bytearray(at + 1, length - at - 1, piece),
        ),
      ),
      ..rest
    ]
  }
}

// The product of two 4,096-word arguments has 128 columns of 512 bytes.
test completes_the_loop_for_the_product_of_the_largest_arguments() {
  completes(largest_product_case(largest_product_pieces()), [1, 64, 127, 128])
}

/// top()^2 with a byte of its top limb changed (CBOR offset 100, in the
/// argument's second 64-byte segment).
fn largest_product_with_a_wrong_top_limb() -> CaseV1 {
  largest_product_case(change_byte(largest_product_pieces(), 100))
}

test a_wrong_top_limb_of_the_largest_product_passes_the_lower_hops() {
  hops(
    largest_product_with_a_wrong_top_limb(),
    largest_product_case(largest_product_pieces()),
    None,
    [1, 64, 127],
    False,
  )
}

test refuses_a_wrong_top_limb_of_the_largest_product_at_its_hop() fail {
  hops(
    largest_product_with_a_wrong_top_limb(),
    largest_product_case(largest_product_pieces()),
    None,
    [1, 64, 127, 128],
    True,
  )
}

test refuses_a_hop_of_the_largest_product_that_hands_on_a_wrong_carry() fail {
  let case = largest_product_case(largest_product_pieces())
  let first = state_after(case, None, 1)
  expect state: ProductStateV1 = state_after(case, Some(Pair(1, first)), 64)
  let wrong: Data = ProductStateV1 { ..state, carry: state.carry + 1 }
  hop(case, Some(Pair(1, first)), 64, Some(wrong))
}

// ## Cursor and end

fn mid_case() -> CaseV1 {
  let a = magnitude(9216, 1)
  let b = -magnitude(4000, 2)
  product_case(a, b, cbor.serialise(a * b))
}

// 18 + 8 limbs.
const mid_count = 26

test the_mid_product_completes_hop_by_hop() {
  completes(mid_case(), list.range(1, mid_count))
}

test refuses_a_hop_that_ends_at_its_cursor() fail {
  let case = mid_case()
  let state = state_after(case, None, 5)
  hop(case, Some(Pair(5, state)), 5, Some(state))
}

test refuses_a_first_hop_that_ends_at_zero() fail {
  let case = mid_case()
  hop(case, None, 0, Some(state_after(case, None, 1)))
}

test refuses_a_hop_past_the_last_column() fail {
  let case = mid_case()
  // The state a hop past the last column of a product leaves: every column
  // past the product is zero.
  let clear: Data = ProductStateV1 {
    carry: 0,
    borrow: 0,
    order: 0,
    increment: 0,
  }
  hop(
    case,
    Some(Pair(5, state_after(case, None, 5))),
    mid_count + 1,
    Some(clear),
  )
}

test refuses_a_loop_that_stops_before_its_last_column() fail {
  let case = mid_case()
  let state = state_after(case, None, 5)
  hop(case, Some(Pair(5, state)), mid_count - 1, None)
}

test refuses_a_last_hop_that_carries_on() fail {
  let case = mid_case()
  let state = state_after(case, None, 5)
  hop(case, Some(Pair(5, state)), mid_count, Some(state))
}

/// The hop over [5, 10) of `mid_case` whose output names `cursor` as where
/// the next hop starts.
fn hop_handing_on_cursor(cursor: Int) -> Bool {
  let case = mid_case()
  let from = Some(Pair(5, state_after(case, None, 5)))
  let state = state_after(case, from, 10)
  let step = hop_step(case, from, 10, Some(state))
  expect Some(datum) = step.datum
  expect Some(bound) = datum.data
  let output: Data = chain.CoreBound {
    ..bound,
    facts: loop_facts(case.facts, cursor, state),
  }
  spend(
    LoopStepV1 {
      ..step,
      transaction: Transaction {
        ..step.transaction,
        outputs: fixture.continue_same_step_l1_tx_v1("cek-core-staged", output).outputs,
      },
    },
  )
}

test hands_on_the_hop_end_as_the_next_cursor() {
  hop_handing_on_cursor(10)
}

test refuses_a_hop_whose_output_is_at_another_script() fail {
  let case = mid_case()
  let step = hop_step(case, None, 5, Some(state_after(case, None, 5)))
  expect [output] = step.transaction.outputs
  spend(
    LoopStepV1 {
      ..step,
      transaction: Transaction {
        ..step.transaction,
        outputs: [
          Output {
            ..output,
            address: address.from_script(fixture.next_step_script_hash),
          },
        ],
      },
    },
  )
}

test refuses_a_hop_that_skips_a_column() fail {
  hop_handing_on_cursor(11)
}

test refuses_a_hop_that_hands_on_an_earlier_cursor() fail {
  hop_handing_on_cursor(9)
}

// ## Divisions

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

/// Division `tag` of `a` by `b` with quotient `q` and result `result` (its
/// canonical CBOR), all staged.
fn division_case(tag: Int, a: Int, b: Int, q: Int, result: ByteArray) -> CaseV1 {
  let operands = [cbor.serialise(a), cbor.serialise(b), cbor.serialise(q)]
  case_of(
    tag,
    [integer_value(a), integer_value(b)],
    semantic_integer_cbor(result),
    if tag <= 4 {
      stage(operands)
    } else {
      stage(list.concat(operands, [result]))
    },
    if tag <= 4 {
      [staged(0), staged(1), staged(2)]
    } else {
      [staged(0), staged(1), staged(2), staged(3)]
    },
  )
}

fn columns_of(a: Int, b: Int, q: Int) -> Int {
  division_columns(cbor.serialise(a), cbor.serialise(b), cbor.serialise(q))
}

/// Division `tag` over a first, a middle and a last hop.
fn divides(tag: Int, a: Int, b: Int, q: Int, result: ByteArray) -> Bool {
  completes(division_case(tag, a, b, q, result), ends_of(columns_of(a, b, q)))
}

/// Division `tag` in one hop.
fn divides_at_once(tag: Int, a: Int, b: Int, q: Int, result: ByteArray) -> Bool {
  completes(division_case(tag, a, b, q, result), [columns_of(a, b, q)])
}

/// Division `tag` of `a` by `b` with Cardano's quotient and result.
fn divides_correctly(tag: Int, a: Int, b: Int) -> Bool {
  divides(
    tag,
    a,
    b,
    quotient_of(tag, a, b),
    cbor.serialise(division(tag, a, b)),
  )
}

/// Division `tag` of `a` by `b` with Cardano's quotient moved by `delta`, and
/// the result that quotient gives.
fn divides_with_quotient_off_by(tag: Int, a: Int, b: Int, delta: Int) -> Bool {
  let q = quotient_of(tag, a, b) + delta
  divides_at_once(
    tag,
    a,
    b,
    q,
    cbor.serialise(
      if tag <= 4 {
        q
      } else {
        a - q * b
      },
    ),
  )
}

/// 2^4096 - 1: one limb, every byte 0xff.
fn below_a_limb_boundary() -> Int {
  builtin.bytearray_to_integer(True, builtin.replicate_byte(512, 255))
}

fn every_sign_of_every_division(dividend: Int, divisor: Int) -> Bool {
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

test divides_in_every_sign_combination_with_a_remainder() {
  every_sign_of_every_division(magnitude(9216, 1), magnitude(4000, 2))
}

test divides_in_every_sign_combination_without_a_remainder() {
  let divisor = magnitude(4000, 2)
  every_sign_of_every_division(divisor * magnitude(5000, 3), divisor)
}

test divides_a_smaller_dividend_to_a_zero_quotient() {
  every_sign_of_every_division(magnitude(4000, 1), magnitude(9216, 2))
}

test divides_a_dividend_one_below_a_limb_boundary() {
  every_sign_of_every_division(below_a_limb_boundary(), 2)
}

// Over a divisor of the other sign, a zero quotient of 2^4096 - 1 leaves the
// remainder's one limb at 1, below the divisor, and a borrow out of the top
// column.
test refuses_a_quotient_leaving_a_borrow_out_of_the_top_column() fail {
  divides_at_once(3, below_a_limb_boundary(), -2, 0, cbor.serialise(0))
}

test refuses_a_modulus_from_a_quotient_leaving_a_borrow_out_of_the_top_column() fail {
  divides_at_once(6, below_a_limb_boundary(), -2, 0, cbor.serialise(-1))
}

test divides_small_operands_in_every_sign_combination() {
  and {
    every_sign_of_every_division(7, 2),
    every_sign_of_every_division(8, 2),
    every_sign_of_every_division(0, 3),
  }
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
  completes(
    case_of(
      6,
      [integer_value(-7), integer_value(divisor)],
      integer_value(divisor - 7),
      stage([cbor.serialise(divisor), cbor.serialise(divisor - 7)]),
      [
        InlineOperand { bytes: cbor.serialise(-7) },
        staged(0),
        InlineOperand { bytes: cbor.serialise(-1) },
        staged(1),
      ],
    ),
    [1, 32, 33],
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
    divides(3, bottom(), -1, past_top(), cbor.serialise(past_top())),
    divides(4, bottom(), -1, past_top(), cbor.serialise(past_top())),
    divides(5, bottom(), -1, past_top(), cbor.serialise(0)),
    divides(6, bottom(), -1, past_top(), cbor.serialise(0)),
  }
}

test refuses_a_remainder_equal_to_the_divisor() fail {
  let divisor = magnitude(4000, 2)
  let dividend = divisor * 6
  divides_at_once(3, dividend, divisor, 5, cbor.serialise(5))
}

test refuses_a_wrong_quotient_result_with_the_right_quotient() fail {
  let dividend = magnitude(9216, 1)
  let divisor = magnitude(4000, 2)
  let quotient = builtin.quotient_integer(dividend, divisor)
  divides_at_once(4, dividend, divisor, quotient, cbor.serialise(quotient + 1))
}

test refuses_a_truncated_quotient_for_divide_integer() fail {
  let dividend = -magnitude(9216, 1)
  let divisor = magnitude(4000, 2)
  let quotient = builtin.quotient_integer(dividend, divisor)
  divides_at_once(3, dividend, divisor, quotient, cbor.serialise(quotient))
}

test refuses_a_floored_quotient_for_quotient_integer() fail {
  let dividend = -magnitude(9216, 1)
  let divisor = magnitude(4000, 2)
  let quotient = builtin.divide_integer(dividend, divisor)
  divides_at_once(4, dividend, divisor, quotient, cbor.serialise(quotient))
}

test refuses_a_floored_remainder_for_remainder_integer() fail {
  let dividend = magnitude(9216, 1)
  let divisor = -magnitude(4000, 2)
  divides_at_once(
    5,
    dividend,
    divisor,
    builtin.divide_integer(dividend, divisor),
    cbor.serialise(builtin.mod_integer(dividend, divisor)),
  )
}

test refuses_a_truncated_remainder_for_mod_integer() fail {
  let dividend = magnitude(9216, 1)
  let divisor = -magnitude(4000, 2)
  divides_at_once(
    6,
    dividend,
    divisor,
    builtin.quotient_integer(dividend, divisor),
    cbor.serialise(builtin.remainder_integer(dividend, divisor)),
  )
}

test refuses_a_wrong_remainder_with_the_right_quotient() fail {
  let dividend = magnitude(9216, 1)
  let divisor = magnitude(4000, 2)
  divides_at_once(
    6,
    dividend,
    divisor,
    dividend / divisor,
    cbor.serialise(builtin.mod_integer(dividend, divisor) + 1),
  )
}

test refuses_a_remainder_of_the_other_sign() fail {
  // remainderInteger(7, -2) is 1; -1 has the same magnitude.
  divides_at_once(5, 7, -2, -3, cbor.serialise(-1))
}

test refuses_a_modulus_of_the_other_sign() fail {
  // modInteger(7, -2) is -1; 1 has the same magnitude.
  divides_at_once(6, 7, -2, -4, cbor.serialise(1))
}

// A quotient of the wrong sign with the right magnitude leaves a remainder
// below the divisor in magnitude.
test refuses_a_quotient_of_the_wrong_sign() fail {
  divides_at_once(4, -7, 2, 3, cbor.serialise(3))
}

test refuses_a_remainder_from_a_quotient_of_the_wrong_sign() fail {
  divides_at_once(5, -7, 2, 3, cbor.serialise(-1))
}

// quotientInteger(7, 2) with quotient 4 leaves -1, below 2 in magnitude.
test refuses_a_quotient_leaving_a_negative_remainder_for_a_positive_dividend() fail {
  divides_at_once(4, 7, 2, 4, cbor.serialise(4))
}

test refuses_a_remainder_from_a_quotient_leaving_the_other_sign() fail {
  divides_at_once(5, 7, 2, 4, cbor.serialise(-1))
}

// divideInteger(-7, 2) with quotient -3 leaves -1, below 2 in magnitude.
test refuses_a_floored_quotient_one_toward_zero() fail {
  divides_at_once(3, -7, 2, -3, cbor.serialise(-3))
}

test refuses_a_modulus_from_a_quotient_one_toward_zero() fail {
  divides_at_once(6, -7, 2, -3, cbor.serialise(-1))
}

test admits_a_zero_modulus_of_operands_of_different_signs() {
  and {
    divides_correctly(6, -8, 2),
    divides_correctly(6, 8, -2),
    divides_correctly(3, -8, 2),
    divides_correctly(3, 8, -2),
  }
}

// ### A quotient off by one in each sign combination

test refuses_q_above_divide_integer() fail {
  divides_with_quotient_off_by(3, 7, 2, 1)
}

test refuses_q_above_divide_integer_negative_dividend() fail {
  divides_with_quotient_off_by(3, -7, 2, 1)
}

test refuses_q_above_divide_integer_negative_divisor() fail {
  divides_with_quotient_off_by(3, 7, -2, 1)
}

test refuses_q_above_divide_integer_both_negative() fail {
  divides_with_quotient_off_by(3, -7, -2, 1)
}

test refuses_q_below_divide_integer() fail {
  divides_with_quotient_off_by(3, 7, 2, -1)
}

test refuses_q_below_divide_integer_negative_dividend() fail {
  divides_with_quotient_off_by(3, -7, 2, -1)
}

test refuses_q_below_divide_integer_negative_divisor() fail {
  divides_with_quotient_off_by(3, 7, -2, -1)
}

test refuses_q_below_divide_integer_both_negative() fail {
  divides_with_quotient_off_by(3, -7, -2, -1)
}

test refuses_q_above_quotient_integer() fail {
  divides_with_quotient_off_by(4, 7, 2, 1)
}

test refuses_q_above_quotient_integer_negative_dividend() fail {
  divides_with_quotient_off_by(4, -7, 2, 1)
}

test refuses_q_above_quotient_integer_negative_divisor() fail {
  divides_with_quotient_off_by(4, 7, -2, 1)
}

test refuses_q_above_quotient_integer_both_negative() fail {
  divides_with_quotient_off_by(4, -7, -2, 1)
}

test refuses_q_below_quotient_integer() fail {
  divides_with_quotient_off_by(4, 7, 2, -1)
}

test refuses_q_below_quotient_integer_negative_dividend() fail {
  divides_with_quotient_off_by(4, -7, 2, -1)
}

test refuses_q_below_quotient_integer_negative_divisor() fail {
  divides_with_quotient_off_by(4, 7, -2, -1)
}

test refuses_q_below_quotient_integer_both_negative() fail {
  divides_with_quotient_off_by(4, -7, -2, -1)
}

test refuses_q_above_remainder_integer() fail {
  divides_with_quotient_off_by(5, 7, 2, 1)
}

test refuses_q_above_remainder_integer_negative_dividend() fail {
  divides_with_quotient_off_by(5, -7, 2, 1)
}

test refuses_q_above_remainder_integer_negative_divisor() fail {
  divides_with_quotient_off_by(5, 7, -2, 1)
}

test refuses_q_above_remainder_integer_both_negative() fail {
  divides_with_quotient_off_by(5, -7, -2, 1)
}

test refuses_q_below_remainder_integer() fail {
  divides_with_quotient_off_by(5, 7, 2, -1)
}

test refuses_q_below_remainder_integer_negative_dividend() fail {
  divides_with_quotient_off_by(5, -7, 2, -1)
}

test refuses_q_below_remainder_integer_negative_divisor() fail {
  divides_with_quotient_off_by(5, 7, -2, -1)
}

test refuses_q_below_remainder_integer_both_negative() fail {
  divides_with_quotient_off_by(5, -7, -2, -1)
}

test refuses_q_above_mod_integer() fail {
  divides_with_quotient_off_by(6, 7, 2, 1)
}

test refuses_q_above_mod_integer_negative_dividend() fail {
  divides_with_quotient_off_by(6, -7, 2, 1)
}

test refuses_q_above_mod_integer_negative_divisor() fail {
  divides_with_quotient_off_by(6, 7, -2, 1)
}

test refuses_q_above_mod_integer_both_negative() fail {
  divides_with_quotient_off_by(6, -7, -2, 1)
}

test refuses_q_below_mod_integer() fail {
  divides_with_quotient_off_by(6, 7, 2, -1)
}

test refuses_q_below_mod_integer_negative_dividend() fail {
  divides_with_quotient_off_by(6, -7, 2, -1)
}

test refuses_q_below_mod_integer_negative_divisor() fail {
  divides_with_quotient_off_by(6, 7, -2, -1)
}

test refuses_q_below_mod_integer_both_negative() fail {
  divides_with_quotient_off_by(6, -7, -2, -1)
}

// ### A 32 KB dividend over a 17,000-byte divisor

fn large_dividend() -> Int {
  top()
}

fn large_divisor() -> Int {
  magnitude(17000, 3)
}

/// The large division `tag` with the divisor of sign `negative`, Cardano's
/// quotient moved by `delta` and the result that quotient gives, over a
/// first, a middle and a last hop.
fn divides_large(tag: Int, negative: Bool, delta: Int) -> Bool {
  let a = large_dividend()
  let b = if negative {
    -large_divisor()
  } else {
    large_divisor()
  }
  let q = quotient_of(tag, a, b) + delta
  let result = if tag <= 4 {
    q
  } else {
    expect delta == 0
    division(tag, a, b)
  }
  let case = division_case(tag, a, b, q, cbor.serialise(result))
  let count = columns_of(a, b, q)
  completes(case, [1, count / 2, count])
}

test divides_a_large_dividend_by_a_large_divisor_of_either_sign() {
  list.all(
    [3, 4, 5, 6],
    fn(tag) { divides_large(tag, False, 0) && divides_large(tag, True, 0) },
  )
}

test refuses_a_large_quotient_one_above() fail {
  divides_large(4, False, 1)
}

test refuses_a_large_quotient_one_below() fail {
  divides_large(4, False, -1)
}

test refuses_a_large_floored_quotient_one_above() fail {
  divides_large(3, True, 1)
}

test refuses_a_large_floored_quotient_one_below() fail {
  divides_large(3, True, -1)
}

// ### Division by zero

test refuses_division_by_zero_with_divide_integer() fail {
  divides_at_once(3, magnitude(9216, 1), 0, 0, cbor.serialise(0))
}

test refuses_division_by_zero_with_quotient_integer() fail {
  divides_at_once(4, magnitude(9216, 1), 0, 0, cbor.serialise(0))
}

test refuses_division_by_zero_with_remainder_integer() fail {
  let dividend = magnitude(9216, 1)
  divides_at_once(5, dividend, 0, 0, cbor.serialise(dividend))
}

test refuses_division_by_zero_with_mod_integer() fail {
  let dividend = magnitude(9216, 1)
  divides_at_once(6, dividend, 0, 0, cbor.serialise(dividend))
}

test refuses_division_of_zero_by_zero() fail {
  divides_at_once(4, 0, 0, 0, cbor.serialise(0))
}

test refuses_division_by_zero_with_a_large_quotient() fail {
  let dividend = magnitude(9216, 1)
  divides_at_once(5, dividend, 0, magnitude(9216, 2), cbor.serialise(dividend))
}

// ### Arguments and sources

test refuses_a_dividend_one_past_the_largest() fail {
  divides_at_once(3, past_top(), 1, past_top(), cbor.serialise(past_top()))
}

test refuses_a_dividend_one_past_the_smallest() fail {
  let dividend_cbor = past_bottom_cbor()
  completes(
    case_of(
      4,
      [semantic_integer_cbor(dividend_cbor), integer_value(1)],
      semantic_integer_cbor(dividend_cbor),
      stage([dividend_cbor, dividend_cbor]),
      [staged(0), InlineOperand { bytes: cbor.serialise(1) }, staged(1)],
    ),
    [65],
  )
}

test refuses_a_divisor_one_past_the_largest() fail {
  divides_at_once(5, 1, past_top(), 0, cbor.serialise(1))
}

test refuses_a_divisor_one_past_the_smallest() fail {
  let divisor_cbor = past_bottom_cbor()
  completes(
    case_of(
      6,
      [integer_value(-1), semantic_integer_cbor(divisor_cbor)],
      integer_value(-1),
      stage([divisor_cbor]),
      [
        InlineOperand { bytes: cbor.serialise(-1) },
        staged(0),
        InlineOperand { bytes: cbor.serialise(0) },
        InlineOperand { bytes: cbor.serialise(-1) },
      ],
    ),
    [65],
  )
}

test refuses_a_division_without_a_quotient() fail {
  let dividend = magnitude(9216, 1)
  let divisor = magnitude(4000, 2)
  completes(
    case_of(
      3,
      [integer_value(dividend), integer_value(divisor)],
      integer_value(dividend / divisor),
      stage([cbor.serialise(dividend), cbor.serialise(divisor)]),
      [staged(0), staged(1)],
    ),
    [18],
  )
}

test refuses_a_quotient_division_with_a_result_source() fail {
  let dividend = magnitude(9216, 1)
  let divisor = magnitude(4000, 2)
  let quotient = dividend / divisor
  completes(
    case_of(
      3,
      [integer_value(dividend), integer_value(divisor)],
      integer_value(quotient),
      stage(
        [
          cbor.serialise(dividend),
          cbor.serialise(divisor),
          cbor.serialise(quotient),
        ],
      ),
      [staged(0), staged(1), staged(2), staged(2)],
    ),
    [18],
  )
}

test refuses_a_remainder_division_without_its_result() fail {
  let dividend = magnitude(9216, 1)
  let divisor = magnitude(4000, 2)
  completes(
    case_of(
      6,
      [integer_value(dividend), integer_value(divisor)],
      integer_value(dividend % divisor),
      stage(
        [
          cbor.serialise(dividend),
          cbor.serialise(divisor),
          cbor.serialise(dividend / divisor),
        ],
      ),
      [staged(0), staged(1), staged(2)],
    ),
    [18],
  )
}

test refuses_a_quotient_given_by_its_leaf() fail {
  let dividend = magnitude(9216, 1)
  let divisor = magnitude(4000, 2)
  let quotient = dividend / divisor
  completes(
    case_of(
      3,
      [integer_value(dividend), integer_value(divisor)],
      integer_value(quotient),
      stage([cbor.serialise(dividend), cbor.serialise(divisor)]),
      [staged(0), staged(1), leaf(quotient)],
    ),
    [18],
  )
}

test refuses_a_quotient_given_as_a_small_integer() fail {
  let dividend = magnitude(9216, 1)
  let divisor = magnitude(9000, 2)
  completes(
    case_of(
      5,
      [integer_value(dividend), integer_value(divisor)],
      integer_value(dividend % divisor),
      stage(
        [
          cbor.serialise(dividend),
          cbor.serialise(divisor),
          cbor.serialise(dividend % divisor),
        ],
      ),
      [
        staged(0),
        staged(1),
        SmallIntegerOperand { value: dividend / divisor },
        staged(2),
      ],
    ),
    [18],
  )
}

test refuses_a_non_canonical_quotient() fail {
  completes(
    case_of(
      6,
      [integer_value(70), integer_value(9)],
      integer_value(7),
      no_staging(),
      [
        InlineOperand { bytes: cbor.serialise(70) },
        InlineOperand { bytes: cbor.serialise(9) },
        InlineOperand { bytes: #"1807" },
        InlineOperand { bytes: cbor.serialise(7) },
      ],
    ),
    [1],
  )
}

test refuses_a_non_canonical_remainder() fail {
  completes(
    case_of(
      6,
      [integer_value(70), integer_value(9)],
      semantic_integer_pieces([#"1807"], 1),
      no_staging(),
      [
        InlineOperand { bytes: cbor.serialise(70) },
        InlineOperand { bytes: cbor.serialise(9) },
        InlineOperand { bytes: cbor.serialise(7) },
        InlineOperand { bytes: #"1807" },
      ],
    ),
    [1],
  )
}

// ## The direct route agrees

/// Steps whose operands are all within the direct carriage bound verify on
/// this route too, with the result the direct route computes.
test agrees_with_the_direct_route_on_inline_operands() {
  let left = magnitude(3000, 1)
  let right = -magnitude(1000, 2)
  let inline = fn(tag, sources, result) {
    let count = if tag == 2 {
      8
    } else {
      6
    }
    and {
      completes(
        case_of(
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
        [1, 3, count],
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
  let remainder = fn(tag) {
    [
      InlineOperand { bytes: cbor.serialise(quotient_of(tag, left, right)) },
      InlineOperand { bytes: cbor.serialise(division(tag, left, right)) },
    ]
  }
  and {
    inline(
      2,
      [InlineOperand { bytes: cbor.serialise(left * right) }],
      left * right,
    ),
    inline(3, quotient(3), division(3, left, right)),
    inline(4, quotient(4), division(4, left, right)),
    inline(5, remainder(5), division(5, left, right)),
    inline(6, remainder(6), division(6, left, right)),
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
  completes(
    case_of(
      0,
      [integer_value(left), integer_value(right)],
      integer_value(left + right),
      stage(
        [
          cbor.serialise(left),
          cbor.serialise(right),
          cbor.serialise(0),
          cbor.serialise(left + right),
        ],
      ),
      [staged(0), staged(1), staged(2), staged(3)],
    ),
    [18],
  )
}

/// The one-hop step of `mid_case` with its bound moved to `group` and
/// `progress`.
fn mid_step_at(group: Int, progress: Int) -> Bool {
  let step = hop_step(mid_case(), None, mid_count, None)
  expect Some(datum) = step.datum
  expect Some(bound) = datum.data
  spend(
    LoopStepV1 {
      ..step,
      datum: Some(
        ct.StepDatum {
          ..datum,
          data: Some(chain.CoreBound { ..bound, group, progress }),
        },
      ),
    },
  )
}

test the_mid_product_completes_in_one_hop() {
  mid_step_at(5, 3)
}

test refuses_a_step_in_another_group() fail {
  mid_step_at(2, 3)
}

test refuses_a_step_before_the_budget_hop() fail {
  mid_step_at(5, 2)
}

// ## The result's leaf

test refuses_a_staged_product_whose_result_leaf_is_another_integer() fail {
  let a = magnitude(9216, 1)
  let b = -magnitude(4000, 2)
  let case = case_of(
    2,
    [integer_value(a), integer_value(b)],
    semantic_integer_cbor(other_of_same_size(a * b)),
    stage([cbor.serialise(a), cbor.serialise(b), cbor.serialise(a * b)]),
    [staged(0), staged(1), staged(2)],
  )
  completes(case, [product_columns(cbor.serialise(a), cbor.serialise(b))])
}

test refuses_an_inline_product_whose_result_leaf_is_another_integer() fail {
  let case = case_of(
    2,
    [integer_value(3), integer_value(5)],
    integer_value(16),
    no_staging(),
    [
      InlineOperand { bytes: cbor.serialise(3) },
      InlineOperand { bytes: cbor.serialise(5) },
      InlineOperand { bytes: cbor.serialise(15) },
    ],
  )
  completes(case, [2])
}

fn remainder_with_another_leaf(tag: Int) -> Bool {
  let a = -magnitude(9216, 1)
  let b = magnitude(4000, 2)
  let q = quotient_of(tag, a, b)
  let r = division(tag, a, b)
  let case = case_of(
    tag,
    [integer_value(a), integer_value(b)],
    semantic_integer_cbor(other_of_same_size(r)),
    stage(
      [
        cbor.serialise(a),
        cbor.serialise(b),
        cbor.serialise(q),
        cbor.serialise(r),
      ],
    ),
    [staged(0), staged(1), staged(2), staged(3)],
  )
  completes(case, [columns_of(a, b, q)])
}

test refuses_a_remainder_whose_result_leaf_is_another_integer() fail {
  remainder_with_another_leaf(5)
}

test refuses_a_modulus_whose_result_leaf_is_another_integer() fail {
  remainder_with_another_leaf(6)
}

// A negative result's CBOR spells |n| - 1, so the one carried into a limb
// past the last column would make a zero product read as -2^4096.
test refuses_a_zero_product_spelled_as_minus_the_limb_base() fail {
  multiplies_at_once(0, -5, cbor.serialise(-power_of_two(4096)))
}

// ## The result's CBOR

/// multiplyInteger of a 9,216-byte operand by 2 whose result is spelled
/// `product` (the canonical CBOR of the product with a byte changed).
fn doubles_to(product: ByteArray) -> Bool {
  let left = magnitude(9216, 1)
  completes(
    case_of(
      2,
      [integer_value(left), integer_value(2)],
      semantic_integer_pieces(
        [product],
        magnitude_words_v1(parse_integer_cbor_bulk_v1(cbor.serialise(left * 2))),
      ),
      stage([cbor.serialise(left), product]),
      [staged(0), InlineOperand { bytes: cbor.serialise(2) }, staged(1)],
    ),
    [19],
  )
}

fn doubled() -> ByteArray {
  cbor.serialise(magnitude(9216, 1) * 2)
}

test admits_the_doubled_product() {
  doubles_to(doubled())
}

test refuses_a_product_whose_cbor_ends_without_a_break() fail {
  let product = doubled()
  doubles_to(
    builtin.append_bytearray(
      builtin.slice_bytearray(
        0,
        builtin.length_of_bytearray(product) - 1,
        product,
      ),
      #"00",
    ),
  )
}

// Offset 68 is the head of the CBOR argument's second 64-byte segment.
test refuses_a_product_with_a_wrong_segment_head() fail {
  expect [product] = change_byte([doubled()], 68)
  doubles_to(product)
}

/// The hop of `mid_case` over columns [0, 5) with its bound, and the bound
/// its output carries, moved to `progress`.
fn mid_hop_at(progress: Int) -> Bool {
  let case = mid_case()
  let state = state_after(case, None, 5)
  let step = hop_step(case, None, 5, Some(state))
  expect Some(datum) = step.datum
  expect Some(bound) = datum.data
  let moved = chain.CoreBound { ..bound, progress }
  let next: Data = chain.CoreBound {
    ..moved,
    facts: loop_facts(case.facts, 5, state),
  }
  let output_datum: ct.StepDatum<Data> = ct.StepDatum {
    fraud_prover: datum.fraud_prover,
    data: Some(next),
  }
  let output_data: Data = output_datum
  expect [output] = step.transaction.outputs
  spend(
    LoopStepV1 {
      datum: Some(ct.StepDatum { ..datum, data: Some(moved) }),
      redeemer: step.redeemer,
      transaction: Transaction {
        ..step.transaction,
        outputs: [Output { ..output, datum: InlineDatum(output_data) }],
      },
    },
  )
}

test the_mid_product_hands_on_its_first_hop() {
  mid_hop_at(3)
}

test refuses_a_hop_before_the_budget_hop() fail {
  mid_hop_at(2)
}

/// The first hop of `mid_case` over columns [0, 5) given `witness` and
/// `reference_inputs` in place of the ones its bound holds.
fn mid_hop_given(witness: Data, reference_inputs: List<Input>) -> Bool {
  let case = mid_case()
  let step = hop_step(case, None, 5, Some(state_after(case, None, 5)))
  spend(
    LoopStepV1 {
      ..step,
      redeemer: ct.Continue(
        chain.LoopArm { input_index: 0, output_index: 0, witness, end: 5 },
      ),
      transaction: Transaction { ..step.transaction, reference_inputs },
    },
  )
}

test the_mid_product_hops_with_its_bound_witness() {
  let case = mid_case()
  mid_hop_given(case.witness, case.reference_inputs)
}

// The same operands and result, staged in another order: a witness the bound
// does not hold.
test refuses_a_hop_given_another_witness() fail {
  let a = magnitude(9216, 1)
  let b = -magnitude(4000, 2)
  let other = case_of(
    2,
    [integer_value(a), integer_value(b)],
    semantic_integer_cbor(cbor.serialise(a * b)),
    stage([cbor.serialise(b), cbor.serialise(a), cbor.serialise(a * b)]),
    [staged(1), staged(0), staged(2)],
  )
  mid_hop_given(other.witness, other.reference_inputs)
}
