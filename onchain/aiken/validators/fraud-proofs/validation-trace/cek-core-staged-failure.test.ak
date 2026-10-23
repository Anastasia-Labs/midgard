//// Transaction-level rows for the staged failure step (arm 36, group 7,
//// progress 1): a byte-string, string or integer builtin that fails on an
//// argument past protocol version 11's bounds, on an index outside its byte
//// string, or on bytes that are not UTF-8. The step must accept exactly the
//// failures Cardano raises, and the roots hop must route these builtins to
//// it.

use aiken/builtin
use aiken/cbor
use aiken/collection/list
use cardano/transaction.{Transaction}
use fraud_proofs/validation_trace/cek_core_semantic_failure_roots as failure_roots
use fraud_proofs/validation_trace/cek_core_staged_failure as staged_failure
use midgard/cek_builtin_v1.{ValueWitnessV1}
use midgard/cek_cost_v1 as cost
use midgard/cek_machine_v1 as machine
use midgard/cek_proof_v1 as proof
use midgard/cek_semantic_chain as chain
use midgard/cek_staged_arm_v1.{
  BytesLeafOperand, InlineOperand, IntegerLeafOperand, OperandSourceV1,
  SmallIntegerOperand, StagedMaterialV1, verify_staged_failure_v1,
}
use midgard/cek_staged_blob_v1.{flat_root_of_pieces_v1, flat_root_v1}
use midgard/cek_staged_integer_cbor_v1.{
  encode_integer_cbor_bulk_v1, parse_integer_cbor_bulk_v1,
}
use midgard/cek_staged_integer_v1.{CanonicalIntegerV1, magnitude_words_v1}
use midgard/computation_thread as ct
use midgard/fraud_proofs/cek_staged_fixture_v1.{
  StagedStepV1, StagingV1, boolean_value, bytes_value, charged_failure_step,
  compact, failure_step, failure_step_at, integer_value, material, no_staging,
  pattern, power_of_two, semantic_integer_cbor, semantic_pieces, stage, staged,
  text, zeros,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture

// ## Spending

fn spend(step: StagedStepV1) -> Bool {
  staged_failure.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    step.datum,
    step.redeemer,
    fixture.own_out_ref(),
    step.transaction,
  )
}

/// Spends the failure step for `tag` over `arguments`, carried compacted as
/// the roots hop commits them, with `sources` over `staging`.
fn fails(
  tag: Int,
  arguments: List<ValueWitnessV1>,
  staging: StagingV1,
  sources: List<OperandSourceV1>,
) -> Bool {
  spend(
    failure_step(
      tag,
      list.map(arguments, compact),
      material(staging, sources),
      staging.reference_inputs,
    ),
  )
}

/// Spends the failure step deciding from the committed arguments alone.
fn fails_on_arguments(tag: Int, arguments: List<ValueWitnessV1>) -> Bool {
  fails(tag, arguments, no_staging(), [])
}

/// The staged failure check itself, which a refused step fails on: True when
/// it decides that `tag` fails over `arguments`.
fn decides(
  tag: Int,
  arguments: List<ValueWitnessV1>,
  staging: StagingV1,
  sources: List<OperandSourceV1>,
) -> Bool {
  verify_staged_failure_v1(
    tag,
    list.map(arguments, compact),
    material(staging, sources),
    staging.reference_inputs,
  )
}

fn decides_on_arguments(tag: Int, arguments: List<ValueWitnessV1>) -> Bool {
  decides(tag, arguments, no_staging(), [])
}

// ## Values

/// A byte string of 65,537 bytes, one past the bound, as the roots hop
/// commits it.
fn past_bound_bytes() -> ValueWitnessV1 {
  semantic_pieces([pattern(65536, 0), #"01"], False)
}

/// A byte string of exactly 65,536 bytes.
fn at_bound_bytes() -> ValueWitnessV1 {
  semantic_pieces([pattern(65536, 0)], False)
}

fn small_bytes() -> ValueWitnessV1 {
  bytes_value(#"0102", False)
}

/// The arguments of byte-string builtin `tag` with `bytes` as every
/// byte-string argument and small in-range integers elsewhere.
fn bytes_arguments(tag: Int, bytes: ValueWitnessV1) -> List<ValueWitnessV1> {
  if tag == 11 {
    [integer_value(1), bytes]
  } else if tag == 12 {
    [integer_value(0), integer_value(1), bytes]
  } else if tag == 14 {
    [bytes, integer_value(0)]
  } else if tag == 10 || tag >= 15 && tag <= 17 {
    [bytes, small_bytes()]
  } else {
    [bytes]
  }
}

/// The byte-string builtins with a bounded byte-string argument.
const bounded_bytes_tags =
  [10, 11, 12, 14, 15, 16, 17, 18, 19, 20, 25, 71, 72, 86]

/// A source binding `bytes` by its leaf alone.
fn bytes_leaf(bytes: ByteArray) -> OperandSourceV1 {
  BytesLeafOperand {
    root: flat_root_v1(bytes),
    length: builtin.length_of_bytearray(bytes),
  }
}

/// The leaf preimage of `value`.
fn integer_leaf(value: Int) -> OperandSourceV1 {
  let integer_cbor = cbor.serialise(value)
  IntegerLeafOperand {
    cbor_root: flat_root_v1(integer_cbor),
    cbor_length: builtin.length_of_bytearray(integer_cbor),
    words: magnitude_words_v1(parse_integer_cbor_bulk_v1(integer_cbor)),
  }
}

/// 2^262142 - 1.
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

/// 2^262143, one past the largest (4,096 words, like the largest).
fn past_top_cbor() -> ByteArray {
  cbor.serialise(power_of_two(262143))
}

/// -2^262143 - 1, one past the smallest (CBOR argument 2^262143).
fn past_bottom_cbor() -> ByteArray {
  encode_integer_cbor_bulk_v1(
    CanonicalIntegerV1 {
      negative: True,
      argument: builtin.cons_bytearray(0x80, zeros(32767)),
    },
  )
}

/// 2^262144, the smallest positive integer of 4,097 words.
fn over_band_cbor() -> ByteArray {
  encode_integer_cbor_bulk_v1(
    CanonicalIntegerV1 {
      negative: False,
      argument: builtin.cons_bytearray(1, zeros(32768)),
    },
  )
}

/// The integer of `bytes` magnitude bytes, past the direct carriage bound
/// when `bytes` is over 9,215.
fn magnitude(bytes: Int) -> Int {
  builtin.bytearray_to_integer(True, pattern(bytes, 1))
}

// ## Byte-string arguments past 65,536 bytes

test proves_every_bounded_byte_string_builtin_fails_past_65536_bytes() {
  list.all(
    bounded_bytes_tags,
    fn(tag) {
      fails_on_arguments(tag, bytes_arguments(tag, past_bound_bytes()))
    },
  )
}

test refuses_every_bounded_byte_string_builtin_at_65536_bytes() {
  and {
    list.all(
      bounded_bytes_tags,
      fn(tag) {
        or {
          tag == 14,
          tag == 25,
          !decides_on_arguments(tag, bytes_arguments(tag, at_bound_bytes())),
        }
      },
    ),
    !index_decides(pattern(65536, 0), 65535),
    !decode_decides(text(65536), 0),
  }
}

test refuses_a_step_of_index_at_65536_bytes_without_an_index() fail {
  fails_on_arguments(14, bytes_arguments(14, at_bound_bytes()))
}

test proves_append_fails_on_a_second_argument_past_65536_bytes() {
  fails_on_arguments(10, [small_bytes(), past_bound_bytes()])
}

test refuses_the_unbounded_byte_string_and_string_builtins() {
  let long_string = semantic_pieces([pattern(65536, 0), #"01"], True)
  !or {
    decides_on_arguments(13, [past_bound_bytes()]),
    decides_on_arguments(22, [long_string, long_string]),
    decides_on_arguments(23, [long_string, long_string]),
    decides_on_arguments(24, [long_string]),
    decides_on_arguments(21, [past_bound_bytes(), small_bytes(), small_bytes()]),
  }
}

test refuses_a_step_that_fails_on_no_bound() fail {
  fails_on_arguments(13, [past_bound_bytes()])
}

// ## Integer arguments of a byte-string builtin past one word

test proves_cons_fails_on_a_byte_of_two_words() {
  fails_on_arguments(11, [integer_value(power_of_two(64)), small_bytes()])
}

test refuses_cons_on_a_byte_of_one_word() {
  !decides_on_arguments(
    11,
    [integer_value(power_of_two(64) - 1), small_bytes()],
  )
}

test proves_cons_fails_on_a_byte_past_the_direct_carriage_bound() {
  fails_on_arguments(11, [integer_value(magnitude(9300)), small_bytes()])
}

test proves_slice_fails_on_a_start_or_length_of_two_words() {
  and {
    fails_on_arguments(
      12,
      [integer_value(power_of_two(64)), integer_value(1), small_bytes()],
    ),
    fails_on_arguments(
      12,
      [integer_value(0), integer_value(-power_of_two(64) - 1), small_bytes()],
    ),
  }
}

test refuses_slice_on_a_start_and_length_of_one_word() {
  !decides_on_arguments(
    12,
    [
      integer_value(power_of_two(63)),
      integer_value(-power_of_two(64) + 1),
      small_bytes(),
    ],
  )
}

test proves_index_fails_on_an_index_past_the_direct_carriage_bound() {
  fails_on_arguments(
    14,
    [bytes_value(pattern(1000, 0), False), integer_value(magnitude(8930))],
  )
}

// ## indexByteString from the committed length

/// indexByteString of `bytes` (by its leaf) at `index`.
fn index_fails(bytes: ByteArray, index: Int) -> Bool {
  fails(
    14,
    [bytes_value(bytes, False), integer_value(index)],
    no_staging(),
    [bytes_leaf(bytes), SmallIntegerOperand { value: index }],
  )
}

fn index_decides(bytes: ByteArray, index: Int) -> Bool {
  decides(
    14,
    [bytes_value(bytes, False), integer_value(index)],
    no_staging(),
    [bytes_leaf(bytes), SmallIntegerOperand { value: index }],
  )
}

test proves_index_fails_at_and_past_the_length() {
  let bytes = pattern(20000, 3)
  and {
    index_fails(bytes, 20000),
    index_fails(bytes, 20001),
    index_fails(bytes, power_of_two(63)),
    index_fails(bytes, power_of_two(64) - 1),
  }
}

test proves_index_fails_below_zero() {
  let bytes = pattern(20000, 3)
  and {
    index_fails(bytes, -1),
    index_fails(bytes, -power_of_two(63) - 1),
  }
}

test proves_index_fails_on_the_empty_byte_string() {
  index_fails(#"", 0)
}

test refuses_index_within_the_length() {
  let bytes = pattern(20000, 3)
  !or {
    index_decides(bytes, 0),
    index_decides(bytes, 19999),
  }
}

test refuses_a_step_indexing_within_the_length() fail {
  index_fails(pattern(20000, 3), 19999)
}

test refuses_an_index_other_than_the_committed_one() fail {
  let bytes = pattern(20000, 3)
  fails(
    14,
    [bytes_value(bytes, False), integer_value(5)],
    no_staging(),
    [bytes_leaf(bytes), SmallIntegerOperand { value: 20000 }],
  )
}

test refuses_a_length_other_than_the_committed_one() fail {
  let bytes = pattern(20000, 3)
  fails(
    14,
    [bytes_value(bytes, False), integer_value(20000)],
    no_staging(),
    [
      BytesLeafOperand { root: flat_root_v1(bytes), length: 20001 },
      SmallIntegerOperand { value: 20000 },
    ],
  )
}

test refuses_a_leaf_of_other_bytes() fail {
  let bytes = pattern(20000, 3)
  fails(
    14,
    [bytes_value(bytes, False), integer_value(19000)],
    no_staging(),
    [bytes_leaf(pattern(19000, 3)), SmallIntegerOperand { value: 19000 }],
  )
}

// ## decodeUtf8

/// decodeUtf8 of `bytes` staged, with the valid prefix ending at `valid`.
fn decode_fails(bytes: ByteArray, valid: Int) -> Bool {
  fails(
    25,
    [bytes_value(bytes, False)],
    stage([bytes]),
    [staged(0), SmallIntegerOperand { value: valid }],
  )
}

fn decode_decides(bytes: ByteArray, valid: Int) -> Bool {
  decides(
    25,
    [bytes_value(bytes, False)],
    stage([bytes]),
    [staged(0), SmallIntegerOperand { value: valid }],
  )
}

/// `text(length)` with `bytes` written at `at`.
fn with_bytes(length: Int, at: Int, bytes: ByteArray) -> ByteArray {
  let base = text(length)
  let size = builtin.length_of_bytearray(bytes)
  builtin.append_bytearray(
    builtin.slice_bytearray(0, at, base),
    builtin.append_bytearray(
      bytes,
      builtin.slice_bytearray(at + size, length - at - size, base),
    ),
  )
}

test proves_decode_fails_on_a_byte_that_starts_no_character() {
  and {
    decode_fails(with_bytes(10000, 5000, #"ff"), 5000),
    decode_fails(with_bytes(10000, 5000, #"80"), 5000),
    decode_fails(with_bytes(10000, 0, #"c0"), 0),
  }
}

test proves_decode_fails_on_overlong_and_surrogate_sequences() {
  and {
    decode_fails(with_bytes(10000, 7000, #"c080"), 7000),
    decode_fails(with_bytes(10000, 7000, #"e08080"), 7000),
    decode_fails(with_bytes(10000, 7000, #"eda080"), 7000),
    decode_fails(with_bytes(10000, 7000, #"f4908080"), 7000),
  }
}

test proves_decode_fails_on_a_truncated_last_character() {
  let bytes = builtin.append_bytearray(text(65534), #"e282")
  decode_fails(bytes, 65534)
}

test proves_decode_fails_after_multi_byte_characters() {
  let bytes = builtin.append_bytearray(
    #"e282acf09f9880",
    with_bytes(9000, 10, #"e2"),
  )
  decode_fails(bytes, 17)
}

test proves_decode_fails_on_an_inline_operand() {
  let bytes = with_bytes(100, 50, #"ff")
  fails(
    25,
    [bytes_value(bytes, False)],
    no_staging(),
    [InlineOperand { bytes }, SmallIntegerOperand { value: 50 }],
  )
}

test refuses_decode_at_a_valid_character() {
  !or {
    decode_decides(with_bytes(10000, 5000, #"e282ac"), 5000),
    decode_decides(with_bytes(10000, 5000, #"f09f9880"), 5000),
    decode_decides(with_bytes(10000, 5000, #"c3a9"), 5000),
  }
}

test refuses_decode_at_a_valid_character_before_a_multi_byte_one() {
  !or {
    decode_decides(with_bytes(10000, 5000, #"c3a9"), 4999),
    decode_decides(with_bytes(10000, 5000, #"c3a9e282ac"), 5000),
    decode_decides(with_bytes(10000, 5000, #"e282acf09f9880"), 5000),
  }
}

test refuses_decode_before_the_invalid_byte() {
  !decode_decides(with_bytes(10000, 5000, #"ff"), 4999)
}

test refuses_a_step_decoding_valid_text() fail {
  decode_fails(text(10000), 0)
}

test refuses_decode_with_an_invalid_prefix() fail {
  decode_fails(with_bytes(10000, 5000, #"ff"), 5001)
}

test refuses_decode_inside_a_valid_character() fail {
  decode_fails(with_bytes(10000, 5000, #"c3a9"), 5001)
}

test refuses_the_decode_proof_for_another_builtin() fail {
  let bytes = with_bytes(10000, 5000, #"ff")
  fails(
    18,
    [bytes_value(bytes, False)],
    stage([bytes]),
    [staged(0), SmallIntegerOperand { value: 5000 }],
  )
}

test refuses_decode_past_the_end_of_valid_text() fail {
  decode_fails(text(10000), 10000)
}

test refuses_decode_by_the_leaf_alone() fail {
  let bytes = with_bytes(10000, 5000, #"ff")
  fails(
    25,
    [bytes_value(bytes, False)],
    no_staging(),
    [bytes_leaf(bytes), SmallIntegerOperand { value: 5000 }],
  )
}

// ## Integer arguments outside [-2^262143, 2^262143 - 1]

/// The bounded integer builtins.
const bounded_integer_tags = [0, 1, 2, 3, 4, 5, 6, 8, 9]

test proves_every_bounded_integer_builtin_fails_past_4096_words() {
  let huge = semantic_integer_cbor(over_band_cbor())
  list.all(
    bounded_integer_tags,
    fn(tag) {
      and {
        fails_on_arguments(tag, [huge, integer_value(1)]),
        fails_on_arguments(tag, [integer_value(1), huge]),
      }
    },
  )
}

test refuses_equals_integer_past_4096_words() {
  let huge = semantic_integer_cbor(over_band_cbor())
  !decides_on_arguments(7, [huge, huge])
}

test refuses_a_step_of_equals_integer_past_4096_words() fail {
  let huge = semantic_integer_cbor(over_band_cbor())
  fails_on_arguments(7, [huge, huge])
}

/// Builtin `tag` over the staged integer of `integer_cbor` and 1.
fn band_fails(tag: Int, integer_cbor: ByteArray) -> Bool {
  fails(
    tag,
    [semantic_integer_cbor(integer_cbor), integer_value(1)],
    stage([integer_cbor]),
    [staged(0), InlineOperand { bytes: cbor.serialise(1) }],
  )
}

fn band_decides(tag: Int, integer_cbor: ByteArray) -> Bool {
  decides(
    tag,
    [semantic_integer_cbor(integer_cbor), integer_value(1)],
    stage([integer_cbor]),
    [staged(0), InlineOperand { bytes: cbor.serialise(1) }],
  )
}

test proves_the_bounded_integer_builtins_fail_one_past_the_largest() {
  list.all(bounded_integer_tags, fn(tag) { band_fails(tag, past_top_cbor()) })
}

test proves_the_bounded_integer_builtins_fail_one_past_the_smallest() {
  list.all(
    bounded_integer_tags,
    fn(tag) { band_fails(tag, past_bottom_cbor()) },
  )
}

test proves_a_past_range_second_argument_from_an_inline_source() {
  let integer_cbor = past_top_cbor()
  fails(
    0,
    [integer_value(1), semantic_integer_cbor(integer_cbor)],
    no_staging(),
    [integer_leaf(1), InlineOperand { bytes: integer_cbor }],
  )
}

test refuses_the_largest_and_smallest_arguments() {
  !list.any(
    bounded_integer_tags,
    fn(tag) {
      or {
        band_decides(tag, cbor.serialise(top())),
        band_decides(tag, cbor.serialise(bottom())),
      }
    },
  )
}

test refuses_a_step_at_the_largest_argument() fail {
  band_fails(0, cbor.serialise(top()))
}

test refuses_a_past_range_argument_by_its_leaf_alone() {
  let integer_cbor = past_top_cbor()
  !decides(
    0,
    [semantic_integer_cbor(integer_cbor), integer_value(1)],
    no_staging(),
    [
      IntegerLeafOperand {
        cbor_root: flat_root_v1(integer_cbor),
        cbor_length: builtin.length_of_bytearray(integer_cbor),
        words: 4096,
      },
      integer_leaf(1),
    ],
  )
}

test refuses_other_bytes_for_a_past_range_argument() fail {
  fails(
    0,
    [semantic_integer_cbor(past_top_cbor()), integer_value(1)],
    stage([past_bottom_cbor()]),
    [staged(0), integer_leaf(1)],
  )
}

// ## Division by a zero divisor: one arm for each dividend

/// The direct failure arm proves division builtin `tag` fails on `dividend`,
/// compacted as the roots hop commits it, and a revealed zero divisor. The
/// step is the one the staged failure hop is given: both argument forms have
/// the same root.
fn direct_divides_by_zero(tag: Int, dividend: ValueWitnessV1) -> Bool {
  let arguments = [compact(dividend), integer_value(0)]
  let Pair(root, count) = cek_builtin_v1.arguments_root_v1(arguments)
  expect
    cek_builtin_v1.arguments_root_v1(list.map(arguments, compact)) == Pair(
      root,
      count,
    )
  cek_builtin_v1.verify_direct_builtin_failure_v1(
    tag,
    proof.hash_builtin_value_v1(tag, 0, count, root),
    arguments,
  )
}

/// Spends the staged failure hop for division builtin `tag` over the staged
/// integer of `integer_cbor` and a zero divisor, the post-state charging the
/// builtin's cost when `charged` and nothing otherwise.
fn staged_divides_by_zero(
  tag: Int,
  integer_cbor: ByteArray,
  charged: Bool,
) -> Bool {
  let arguments = list.map(
    [semantic_integer_cbor(integer_cbor), integer_value(0)],
    compact,
  )
  let budget = if charged {
    cek_builtin_v1.direct_builtin_budget_v1(tag, arguments)
  } else {
    cost.BuiltinBudgetV1 { cpu: 0, memory: 0 }
  }
  let staging = stage([integer_cbor])
  spend(
    charged_failure_step(
      tag,
      arguments,
      material(staging, [staged(0), integer_leaf(0)]),
      staging.reference_inputs,
      budget.cpu,
      budget.memory,
    ),
  )
}

const division_tags = [3, 4, 5, 6]

/// 2^262079, the smallest positive integer of 4,096 words is 2^262080.
fn largest_word_count_below_the_band() -> Int {
  power_of_two(262079)
}

test proves_a_division_by_zero_past_the_range_at_the_staged_failure_hop_only() {
  list.all(
    division_tags,
    fn(tag) {
      and {
        !direct_divides_by_zero(tag, semantic_integer_cbor(over_band_cbor())),
        !direct_divides_by_zero(tag, semantic_integer_cbor(past_top_cbor())),
        !direct_divides_by_zero(tag, semantic_integer_cbor(past_bottom_cbor())),
        staged_divides_by_zero(tag, over_band_cbor(), False),
        staged_divides_by_zero(tag, past_top_cbor(), False),
        staged_divides_by_zero(tag, past_bottom_cbor(), False),
      }
    },
  )
}

test proves_a_charged_division_by_zero_of_4096_words_at_the_staged_failure_hop_only() {
  list.all(
    division_tags,
    fn(tag) {
      and {
        !direct_divides_by_zero(tag, integer_value(top())),
        !direct_divides_by_zero(tag, integer_value(bottom())),
        staged_divides_by_zero(tag, cbor.serialise(top()), True),
        staged_divides_by_zero(tag, cbor.serialise(bottom()), True),
      }
    },
  )
}

test proves_a_division_by_zero_below_4096_words_at_the_direct_failure_arm() {
  let dividend = largest_word_count_below_the_band()
  expect
    magnitude_words_v1(parse_integer_cbor_bulk_v1(cbor.serialise(dividend))) == 4095
  list.all(
    division_tags,
    fn(tag) { direct_divides_by_zero(tag, integer_value(dividend)) },
  )
}

test refuses_a_charged_step_below_4096_words() fail {
  staged_divides_by_zero(
    3,
    cbor.serialise(largest_word_count_below_the_band()),
    True,
  )
}

test refuses_an_uncharged_step_below_4096_words() fail {
  staged_divides_by_zero(
    3,
    cbor.serialise(largest_word_count_below_the_band()),
    False,
  )
}

test refuses_an_uncharged_step_in_the_range() fail {
  staged_divides_by_zero(4, cbor.serialise(top()), False)
}

test refuses_a_charged_step_past_the_range() fail {
  staged_divides_by_zero(5, past_top_cbor(), True)
}

test refuses_a_charged_step_past_4096_words() fail {
  staged_divides_by_zero(6, over_band_cbor(), True)
}

test refuses_a_charged_step_on_a_divisor_other_than_zero() fail {
  let integer_cbor = cbor.serialise(top())
  let arguments = list.map(
    [semantic_integer_cbor(integer_cbor), integer_value(1)],
    compact,
  )
  let budget = cek_builtin_v1.direct_builtin_budget_v1(3, arguments)
  let staging = stage([integer_cbor])
  spend(
    charged_failure_step(
      3,
      arguments,
      material(staging, [staged(0), integer_leaf(1)]),
      staging.reference_inputs,
      budget.cpu,
      budget.memory,
    ),
  )
}

test refuses_a_charged_step_with_the_dividend_by_its_leaf_alone() fail {
  let integer_cbor = cbor.serialise(top())
  let arguments = list.map(
    [semantic_integer_cbor(integer_cbor), integer_value(0)],
    compact,
  )
  let budget = cek_builtin_v1.direct_builtin_budget_v1(3, arguments)
  spend(
    charged_failure_step(
      3,
      arguments,
      material(no_staging(), [integer_leaf(top()), integer_leaf(0)]),
      [],
      budget.cpu,
      budget.memory,
    ),
  )
}

test refuses_a_charged_step_with_another_charge() fail {
  let integer_cbor = cbor.serialise(top())
  let arguments = list.map(
    [semantic_integer_cbor(integer_cbor), integer_value(0)],
    compact,
  )
  let budget = cek_builtin_v1.direct_builtin_budget_v1(3, arguments)
  let staging = stage([integer_cbor])
  spend(
    charged_failure_step(
      3,
      arguments,
      material(staging, [staged(0), integer_leaf(0)]),
      staging.reference_inputs,
      budget.cpu + 1,
      budget.memory,
    ),
  )
}

test refuses_a_charged_multiplication_by_zero() fail {
  let integer_cbor = cbor.serialise(top())
  let arguments = list.map(
    [semantic_integer_cbor(integer_cbor), integer_value(0)],
    compact,
  )
  let budget = cek_builtin_v1.direct_builtin_budget_v1(2, arguments)
  let staging = stage([integer_cbor])
  spend(
    charged_failure_step(
      2,
      arguments,
      material(staging, [staged(0), integer_leaf(0)]),
      staging.reference_inputs,
      budget.cpu,
      budget.memory,
    ),
  )
}

test refuses_a_charged_post_for_a_bound_failure() fail {
  let huge = semantic_integer_cbor(over_band_cbor())
  let arguments = list.map([huge, integer_value(1)], compact)
  spend(
    charged_failure_step(0, arguments, material(no_staging(), []), [], 1, 0),
  )
}

// ## Cost at the largest shapes
//
// Each spend is paired with its fixture alone: the spend's cost on L1 is the
// difference.

/// decodeUtf8 of 65,536 staged bytes whose last character is cut short: the
/// prefix decoded is 65,534 bytes.
fn decode_step_at_the_bound() -> StagedStepV1 {
  let bytes = builtin.append_bytearray(text(65534), #"e282")
  let staging = stage([bytes])
  failure_step(
    25,
    [compact(bytes_value(bytes, False))],
    material(staging, [staged(0), SmallIntegerOperand { value: 65534 }]),
    staging.reference_inputs,
  )
}

/// addInteger over two staged integers of 4,096 words, the largest and one
/// past it: both are bound to their CBOR.
fn band_step_at_the_bound() -> StagedStepV1 {
  let staging = stage([cbor.serialise(top()), past_top_cbor()])
  failure_step(
    0,
    [
      compact(semantic_integer_cbor(cbor.serialise(top()))),
      compact(semantic_integer_cbor(past_top_cbor())),
    ],
    material(staging, [staged(0), staged(1)]),
    staging.reference_inputs,
  )
}

/// divideInteger of the largest staged integer by zero, charged.
fn division_step_at_the_bound() -> StagedStepV1 {
  let integer_cbor = cbor.serialise(top())
  let arguments = list.map(
    [semantic_integer_cbor(integer_cbor), integer_value(0)],
    compact,
  )
  let budget = cek_builtin_v1.direct_builtin_budget_v1(3, arguments)
  let staging = stage([integer_cbor])
  charged_failure_step(
    3,
    arguments,
    material(staging, [staged(0), integer_leaf(0)]),
    staging.reference_inputs,
    budget.cpu,
    budget.memory,
  )
}

fn built(step: StagedStepV1) -> Bool {
  when step.datum is {
    Some(_) -> True
    None -> False
  }
}

test staged_failure_decode_at_the_byte_string_bound() {
  spend(decode_step_at_the_bound())
}

test staged_failure_decode_fixture_only_at_the_byte_string_bound() {
  built(decode_step_at_the_bound())
}

test staged_failure_band_at_the_integer_bound() {
  spend(band_step_at_the_bound())
}

test staged_failure_band_fixture_only_at_the_integer_bound() {
  built(band_step_at_the_bound())
}

test staged_failure_division_at_the_integer_bound() {
  spend(division_step_at_the_bound())
}

test staged_failure_division_fixture_only_at_the_integer_bound() {
  built(division_step_at_the_bound())
}

// ## The step's shape

fn shaped(arm: Int, group: Int, progress: Int) -> Bool {
  let step_material: Data = StagedMaterialV1 {
    runs: no_staging().runs,
    sources: [],
  }
  spend(
    failure_step_at(
      10,
      [past_bound_bytes(), small_bytes()],
      step_material,
      [],
      arm,
      group,
      progress,
    ),
  )
}

test accepts_the_failure_step_shape() {
  shaped(36, 7, 1)
}

test refuses_another_witness_constructor() fail {
  shaped(31, 7, 1)
}

test refuses_another_group() fail {
  shaped(36, 5, 1)
}

test refuses_another_progress() fail {
  shaped(36, 7, 0)
}

test refuses_a_step_in_another_mode() fail {
  let step = failure_step(
    10,
    [past_bound_bytes(), small_bytes()],
    material(no_staging(), []),
    [],
  )
  expect Some(datum) = step.datum
  expect Some(bound) = datum.data
  let pre = machine.MachineStateV1 { ..bound.pre, mode: machine.mode_return }
  spend(
    StagedStepV1 {
      ..step,
      datum: Some(
        ct.StepDatum { ..datum, data: Some(chain.CoreBound { ..bound, pre }) },
      ),
    },
  )
}

// ## Routing at the roots hop

const other_hop_hash =
  #"5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f5f"

/// Spends the roots hop for an arm 36 step of `tag` over `arguments`, the
/// continuation paying to the fixture's next step: that is the staged
/// failure hop's hash when `to_staged`, the material hop's otherwise, and the
/// other hop has another hash.
fn roots_route(
  tag: Int,
  arguments: List<ValueWitnessV1>,
  to_staged: Bool,
) -> Bool {
  roots_route_charging(tag, arguments, to_staged, 0)
}

/// `roots_route` with a post-state that charges `cpu`.
fn roots_route_charging(
  tag: Int,
  arguments: List<ValueWitnessV1>,
  to_staged: Bool,
  cpu: Int,
) -> Bool {
  let Pair(root, count) = cek_builtin_v1.arguments_root_v1(arguments)
  expect count == cost.builtin_argument_count_v1(tag)
  let pre = machine.MachineStateV1 {
    mode: machine.mode_builtin,
    execution_index: 0,
    focus_root: proof.hash_builtin_value_v1(tag, 0, count, root),
    environment_root: #"",
    continuation_root: #"",
    auxiliary: 0,
    cpu: 0,
    memory: 0,
  }
  let post = machine.exact_state(
    pre,
    machine.mode_halt_error,
    proof.hash_error_term_v1(),
    proof.empty_environment_root_v1,
    proof.empty_continuation_root_v1,
    machine.error_builtin_failure,
    cpu,
    0,
  )
  let step_material: Data = StagedMaterialV1 {
    runs: no_staging().runs,
    sources: [],
  }
  let step = failure_step_at(tag, arguments, step_material, [], 36, 7, 0)
  expect Some(datum) = step.datum
  expect Some(bound) = datum.data
  let at_roots = chain.CoreBound { ..bound, pre, post }
  let next: Data = chain.CoreBound { ..at_roots, progress: 1 }
  let Pair(material_hop, staged_hop) = if to_staged {
    Pair(other_hop_hash, fixture.next_step_script_hash)
  } else {
    Pair(fixture.next_step_script_hash, other_hop_hash)
  }
  failure_roots.main.spend(
    material_hop,
    staged_hop,
    fixture.computation_thread_policy_id,
    Some(ct.StepDatum { ..datum, data: Some(at_roots) }),
    step.redeemer,
    fixture.own_out_ref(),
    Transaction {
      ..step.transaction,
      outputs: fixture.continue_mid_step_l1_tx_v1("cek-core-staged", next).outputs,
    },
  )
}

test routes_the_byte_string_builtins_to_the_staged_failure_hop() {
  roots_route(10, [past_bound_bytes(), small_bytes()], True)
}

test does_not_route_the_byte_string_builtins_to_the_material_hop() fail {
  roots_route(10, [past_bound_bytes(), small_bytes()], False)
}

test routes_decode_and_the_later_hash_builtins_to_the_staged_failure_hop() {
  and {
    roots_route(25, [past_bound_bytes()], True),
    roots_route(71, [past_bound_bytes()], True),
    roots_route(72, [past_bound_bytes()], True),
    roots_route(86, [past_bound_bytes()], True),
  }
}

test routes_the_integer_builtins_to_the_staged_failure_hop() {
  roots_route(
    0,
    [semantic_integer_cbor(over_band_cbor()), integer_value(1)],
    True,
  )
}

test routes_the_data_builtins_to_the_material_hop() {
  roots_route(45, [integer_value(1)], False)
}

test does_not_route_the_data_builtins_to_the_staged_failure_hop() fail {
  roots_route(45, [integer_value(1)], True)
}

test does_not_route_decode_to_the_material_hop() fail {
  roots_route(25, [past_bound_bytes()], False)
}

test routes_the_first_builtin_after_decode_to_the_material_hop() {
  roots_route(26, [integer_value(1), integer_value(1), integer_value(1)], False)
}

test does_not_route_the_first_builtin_after_decode_to_the_staged_failure_hop() fail {
  roots_route(26, [integer_value(1), integer_value(1), integer_value(1)], True)
}

test routes_head_and_tail_list_to_the_material_hop() {
  and {
    roots_route(33, [integer_value(1)], False),
    roots_route(34, [integer_value(1)], False),
  }
}

test does_not_route_head_list_to_the_staged_failure_hop() fail {
  roots_route(33, [integer_value(1)], True)
}

test does_not_route_tail_list_to_the_staged_failure_hop() fail {
  roots_route(34, [integer_value(1)], True)
}

test leaves_a_charged_post_on_the_staged_route_to_the_staged_failure_hop() {
  roots_route_charging(
    3,
    [semantic_integer_cbor(over_band_cbor()), integer_value(0)],
    True,
    1,
  )
}

test refuses_a_charged_post_on_the_material_route() fail {
  roots_route_charging(45, [integer_value(1)], False, 1)
}

// ## Signatures and byteStringToInteger
//
// verifyEd25519Signature and verifySchnorrSecp256k1Signature bound only their
// message, and fail on it before costing. A key or signature of the wrong
// length (or, for BIP-340, a key off the curve) beside a message in bound
// fails after costing, whatever the key's or signature's size.
// byteStringToInteger bounds its input.

/// The arguments of signature builtin `tag` of a key, message and signature.
fn signature_arguments(
  key: ByteArray,
  message: ValueWitnessV1,
  signature: ByteArray,
) -> List<ValueWitnessV1> {
  [bytes_value(key, False), message, bytes_value(signature, False)]
}

const signature_tags = [21, 53]

/// An x coordinate on the curve (x = 1), as a BIP-340 key.
fn on_curve_key() -> ByteArray {
  builtin.append_bytearray(zeros(31), #"01")
}

/// An x coordinate off the curve (x = 2^256 - 1, past the field prime).
fn off_curve_key() -> ByteArray {
  builtin.replicate_byte(32, 0xff)
}

test proves_the_signature_builtins_fail_on_a_message_past_65536_bytes() {
  list.all(
    signature_tags,
    fn(tag) {
      and {
        fails_on_arguments(
          tag,
          signature_arguments(on_curve_key(), past_bound_bytes(), zeros(64)),
        ),
        // The bound is checked before the lengths.
        fails_on_arguments(
          tag,
          signature_arguments(zeros(31), past_bound_bytes(), zeros(65)),
        ),
      }
    },
  )
}

test refuses_the_signature_builtins_at_65536_bytes() {
  list.all(
    signature_tags,
    fn(tag) {
      !decides_on_arguments(
        tag,
        signature_arguments(zeros(31), at_bound_bytes(), zeros(64)),
      )
    },
  )
}

test refuses_a_key_or_signature_past_65536_bytes_as_a_bound_failure() {
  list.all(signature_tags, fn(tag) { !or {
        decides_on_arguments(
          tag,
          [past_bound_bytes(), small_bytes(), bytes_value(zeros(64), False)],
        ),
        decides_on_arguments(
          tag,
          [bytes_value(zeros(32), False), small_bytes(), past_bound_bytes()],
        ),
      } })
}

test proves_byte_string_to_integer_fails_on_an_input_past_65536_bytes() {
  and {
    fails_on_arguments(74, [boolean_value(True), past_bound_bytes()]),
    fails_on_arguments(74, [boolean_value(False), past_bound_bytes()]),
  }
}

test refuses_byte_string_to_integer_at_65536_bytes() {
  !decides_on_arguments(74, [boolean_value(False), at_bound_bytes()])
}

/// Spends the staged failure hop for signature builtin `tag` over `key`,
/// `message` and `signature`, each bound by its leaf, charging `cpu`.
fn signature_fails_charging(
  tag: Int,
  key: ByteArray,
  message: ByteArray,
  signature: ByteArray,
  cpu: Int,
  memory: Int,
) -> Bool {
  signature_fails_from(
    tag,
    [bytes_leaf(key), bytes_leaf(message), bytes_leaf(signature)],
    key,
    message,
    signature,
    cpu,
    memory,
  )
}

fn signature_fails_from(
  tag: Int,
  sources: List<OperandSourceV1>,
  key: ByteArray,
  message: ByteArray,
  signature: ByteArray,
  cpu: Int,
  memory: Int,
) -> Bool {
  spend(
    charged_failure_step(
      tag,
      list.map(
        [
          bytes_value(key, False),
          bytes_value(message, False),
          bytes_value(signature, False),
        ],
        compact,
      ),
      material(no_staging(), sources),
      [],
      cpu,
      memory,
    ),
  )
}

/// The charge the builtin's cost row gives.
fn signature_charge(
  tag: Int,
  key: ByteArray,
  message: ByteArray,
  signature: ByteArray,
) -> cost.BuiltinBudgetV1 {
  cek_builtin_v1.direct_builtin_budget_v1(
    tag,
    list.map(
      [
        bytes_value(key, False),
        bytes_value(message, False),
        bytes_value(signature, False),
      ],
      compact,
    ),
  )
}

/// Spends the staged failure hop charged by the cost row, with the key
/// revealed inline when `inline_key`.
fn signature_fails(
  tag: Int,
  key: ByteArray,
  message: ByteArray,
  signature: ByteArray,
  inline_key: Bool,
) -> Bool {
  let budget = signature_charge(tag, key, message, signature)
  signature_fails_from(
    tag,
    [
      if inline_key {
        InlineOperand(key)
      } else {
        bytes_leaf(key)
      },
      bytes_leaf(message),
      bytes_leaf(signature),
    ],
    key,
    message,
    signature,
    budget.cpu,
    budget.memory,
  )
}

test proves_a_malformed_key_or_signature_beside_a_staged_message_charged() {
  let message = pattern(65536, 3)
  list.all(
    signature_tags,
    fn(tag) {
      and {
        signature_fails(tag, zeros(31), message, zeros(64), False),
        signature_fails(tag, zeros(33), message, zeros(64), False),
        signature_fails(tag, on_curve_key(), message, zeros(63), False),
        signature_fails(tag, on_curve_key(), message, zeros(65), False),
        signature_fails(tag, zeros(31), pattern(9216, 1), zeros(65), False),
      }
    },
  )
}

/// Spends the staged failure hop for signature builtin `tag` over a key or
/// signature of 65,537 bytes (the key when `long_key`), bound by its leaf,
/// beside a one-byte message, charged by the cost row.
fn long_key_or_signature_fails(tag: Int, long_key: Bool) -> Bool {
  let long_source = BytesLeafOperand {
    root: flat_root_of_pieces_v1([pattern(65536, 0), #"01"]),
    length: 65537,
  }
  let Pair(key, key_source) = if long_key {
    Pair(past_bound_bytes(), long_source)
  } else {
    Pair(bytes_value(on_curve_key(), False), bytes_leaf(on_curve_key()))
  }
  let Pair(signature, signature_source) = if long_key {
    Pair(bytes_value(zeros(64), False), bytes_leaf(zeros(64)))
  } else {
    Pair(past_bound_bytes(), long_source)
  }
  let arguments = list.map([key, bytes_value(#"01", False), signature], compact)
  let budget = cek_builtin_v1.direct_builtin_budget_v1(tag, arguments)
  spend(
    charged_failure_step(
      tag,
      arguments,
      material(no_staging(), [key_source, bytes_leaf(#"01"), signature_source]),
      [],
      budget.cpu,
      budget.memory,
    ),
  )
}

test proves_a_key_or_signature_past_65536_bytes_charged() {
  list.all(
    signature_tags,
    fn(tag) {
      and {
        long_key_or_signature_fails(tag, True),
        long_key_or_signature_fails(tag, False),
      }
    },
  )
}

test proves_schnorr_fails_on_a_key_off_the_curve_charged() {
  signature_fails(53, off_curve_key(), pattern(65536, 3), zeros(64), True)
}

test refuses_ed25519_on_well_formed_lengths() fail {
  signature_fails(21, off_curve_key(), pattern(65536, 3), zeros(64), True)
}

test refuses_schnorr_on_a_key_on_the_curve() fail {
  signature_fails(53, on_curve_key(), pattern(65536, 3), zeros(64), True)
}

test refuses_schnorr_off_the_curve_by_the_key_leaf_alone() fail {
  signature_fails(53, off_curve_key(), pattern(65536, 3), zeros(64), False)
}

test refuses_a_charged_step_with_another_charge_for_a_malformed_key() fail {
  let message = pattern(65536, 3)
  let budget = signature_charge(21, zeros(31), message, zeros(64))
  signature_fails_charging(
    21,
    zeros(31),
    message,
    zeros(64),
    budget.cpu + 1,
    budget.memory,
  )
}

test refuses_an_uncharged_step_for_a_malformed_key() fail {
  signature_fails_charging(21, zeros(31), pattern(65536, 3), zeros(64), 0, 0)
}

test refuses_a_charged_step_on_a_message_past_65536_bytes() fail {
  let arguments = signature_arguments(zeros(31), past_bound_bytes(), zeros(64))
    |> list.map(compact)
  let budget = cek_builtin_v1.direct_builtin_budget_v1(21, arguments)
  spend(
    charged_failure_step(
      21,
      arguments,
      material(
        no_staging(),
        [
          bytes_leaf(zeros(31)),
          BytesLeafOperand {
            root: flat_root_of_pieces_v1([pattern(65536, 0), #"01"]),
            length: 65537,
          },
          bytes_leaf(zeros(64)),
        ],
      ),
      [],
      budget.cpu,
      budget.memory,
    ),
  )
}

test refuses_a_charged_ecdsa_step() fail {
  let message = pattern(65536, 3)
  let budget = signature_charge(52, zeros(32), message, zeros(64))
  signature_fails_charging(
    52,
    zeros(32),
    message,
    zeros(64),
    budget.cpu,
    budget.memory,
  )
}

test refuses_a_charged_byte_string_to_integer_step() fail {
  let arguments = list.map(
    [boolean_value(False), bytes_value(#"01", False)],
    compact,
  )
  let budget = cek_builtin_v1.direct_builtin_budget_v1(74, arguments)
  spend(
    charged_failure_step(
      74,
      arguments,
      material(no_staging(), [bytes_leaf(#"01"), bytes_leaf(#"01")]),
      [],
      budget.cpu,
      budget.memory,
    ),
  )
}

test refuses_a_charged_step_on_a_message_of_another_type() fail {
  let arguments = list.map(
    [
      bytes_value(zeros(31), False),
      integer_value(1),
      bytes_value(zeros(64), False),
    ],
    compact,
  )
  let budget = cek_builtin_v1.direct_builtin_budget_v1(21, arguments)
  spend(
    charged_failure_step(
      21,
      arguments,
      material(
        no_staging(),
        [bytes_leaf(zeros(31)), integer_leaf(1), bytes_leaf(zeros(64))],
      ),
      [],
      budget.cpu,
      budget.memory,
    ),
  )
}

/// A malformed key beside an inline message is also proved by the direct
/// failure arm; both arms post the same charge.
test proves_a_malformed_key_beside_an_inline_message_with_one_charge() {
  let message = pattern(100, 5)
  list.all(
    signature_tags,
    fn(tag) {
      let arguments = [
        bytes_value(zeros(31), False),
        bytes_value(message, False),
        bytes_value(zeros(64), False),
      ]
      let Pair(root, count) = cek_builtin_v1.arguments_root_v1(arguments)
      and {
        cek_builtin_v1.verify_direct_builtin_failure_v1(
          tag,
          proof.hash_builtin_value_v1(tag, 0, count, root),
          arguments,
        ),
        cek_builtin_v1.direct_builtin_budget_v1(tag, arguments) == signature_charge(
          tag,
          zeros(31),
          message,
          zeros(64),
        ),
        signature_fails(tag, zeros(31), message, zeros(64), False),
      }
    },
  )
}

test routes_schnorr_and_byte_string_to_integer_to_the_staged_failure_hop() {
  and {
    roots_route(
      53,
      signature_arguments(on_curve_key(), past_bound_bytes(), zeros(64)),
      True,
    ),
    roots_route(74, [boolean_value(True), past_bound_bytes()], True),
  }
}

test does_not_route_schnorr_to_the_material_hop() fail {
  roots_route(
    53,
    signature_arguments(on_curve_key(), past_bound_bytes(), zeros(64)),
    False,
  )
}

test does_not_route_byte_string_to_integer_to_the_material_hop() fail {
  roots_route(74, [boolean_value(True), past_bound_bytes()], False)
}

test leaves_a_charged_signature_post_on_the_staged_route() {
  roots_route_charging(
    53,
    signature_arguments(off_curve_key(), at_bound_bytes(), zeros(64)),
    True,
    1,
  )
}

/// verifySchnorrSecp256k1Signature of a key off the curve beside a message of
/// 65,536 bytes: the key is revealed and checked against the curve.
fn signature_step_at_the_bound() -> StagedStepV1 {
  let key = off_curve_key()
  let message = pattern(65536, 3)
  let budget = signature_charge(53, key, message, zeros(64))
  charged_failure_step(
    53,
    list.map(
      [
        bytes_value(key, False),
        bytes_value(message, False),
        bytes_value(zeros(64), False),
      ],
      compact,
    ),
    material(
      no_staging(),
      [InlineOperand(key), bytes_leaf(message), bytes_leaf(zeros(64))],
    ),
    [],
    budget.cpu,
    budget.memory,
  )
}

test staged_failure_signature_at_the_byte_string_bound() {
  spend(signature_step_at_the_bound())
}

test staged_failure_signature_fixture_only_at_the_byte_string_bound() {
  built(signature_step_at_the_bound())
}
