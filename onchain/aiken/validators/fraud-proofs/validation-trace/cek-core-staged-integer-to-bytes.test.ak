//// Transaction-level rows for integerToByteString (73) at the staged
//// conversion semantics step (arm 31, group 5, semantics index 9): an input
//// integer bound inline or from staging outputs by its canonical CBOR, a width
//// revealed as a machine integer, and the endianness the committed Boolean.
//// The step must accept exactly the bytes Cardano returns, and the budget hop
//// must charge exactly what Cardano charges.

use aiken/builtin
use aiken/cbor
use aiken/collection/list
use fraud_proofs/validation_trace/cek_core_builtin_budget as budget
use fraud_proofs/validation_trace/cek_core_staged_conversion as staged_conversion
use midgard/cek_builtin_v1.{ValueWitnessV1}
use midgard/cek_staged_arm_v1.{
  InlineOperand, IntegerLeafOperand, OperandSourceV1, SmallIntegerOperand,
}
use midgard/cek_staged_blob_v1.{flat_root_v1}
use midgard/cek_staged_integer_cbor_v1.{parse_integer_cbor_bulk_v1}
use midgard/cek_staged_integer_v1.{magnitude_words_v1}
use midgard/fraud_proofs/cek_staged_fixture_v1.{
  StagedStepV1, StagingV1, boolean_value, budget_step, bytes_value,
  integer_value, material, no_staging, other_input, pattern, stage, staged,
  staged_step,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture

// ## Steps

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

/// The non-negative integer of `size` big-endian bytes, the first 0x5a.
fn magnitude(size: Int) -> Int {
  builtin.bytearray_to_integer(
    True,
    builtin.cons_bytearray(0x5a, pattern(size - 1, 3)),
  )
}

fn arguments(big_endian: Bool, width: Int, value: Int) -> List<ValueWitnessV1> {
  [boolean_value(big_endian), integer_value(width), integer_value(value)]
}

/// integerToByteString over `step_arguments` claiming `result`, its sources
/// read against `staging`.
fn step(
  step_arguments: List<ValueWitnessV1>,
  result: ByteArray,
  sources: List<OperandSourceV1>,
  staging: StagingV1,
) -> StagedStepV1 {
  staged_step(
    73,
    step_arguments,
    bytes_value(result, False),
    material(staging, sources),
    staging.reference_inputs,
  )
}

/// integerToByteString of `value` at `width` in byte order `big_endian`,
/// claiming `result`, with the input's CBOR staged.
fn claiming(
  big_endian: Bool,
  width: Int,
  value: Int,
  result: ByteArray,
) -> StagedStepV1 {
  step(
    arguments(big_endian, width, value),
    result,
    [SmallIntegerOperand { value: width }, staged(0)],
    stage([cbor.serialise(value)]),
  )
}

/// The honest step verifies.
fn converts(big_endian: Bool, width: Int, value: Int) -> Bool {
  spend(
    claiming(
      big_endian,
      width,
      value,
      builtin.integer_to_bytearray(big_endian, width, value),
    ),
  )
}

fn in_both_orders(width: Int, value: Int) -> Bool {
  converts(True, width, value) && converts(False, width, value)
}

/// The honest step with the input's CBOR inline.
fn converts_inline(big_endian: Bool, width: Int, value: Int) -> Bool {
  spend(
    step(
      arguments(big_endian, width, value),
      builtin.integer_to_bytearray(big_endian, width, value),
      [
        SmallIntegerOperand { value: width },
        InlineOperand { bytes: cbor.serialise(value) },
      ],
      no_staging(),
    ),
  )
}

// ## Honest steps verify

test converts_the_widest_input_at_width_zero() {
  in_both_orders(0, magnitude(8192))
}

test converts_the_widest_input_at_the_widest_width() {
  in_both_orders(8192, magnitude(8192))
}

test pads_an_input_to_the_widest_width() {
  list.all(
    [733, 4096, 8191],
    fn(size) { in_both_orders(8192, magnitude(size)) },
  )
}

test converts_an_input_at_its_own_width_and_one_past_it() {
  list.all(
    [4464, 5000],
    fn(size) {
      in_both_orders(size, magnitude(size)) && in_both_orders(
        size + 1,
        magnitude(size),
      )
    },
  )
}

test converts_zero_and_one_inline() {
  list.all(
    [0, 1],
    fn(value) {
      list.all(
        [0, 1, 8192],
        fn(width) {
          converts_inline(True, width, value) && converts_inline(
            False,
            width,
            value,
          )
        },
      )
    },
  )
}

test converts_an_inline_input() {
  converts_inline(False, 0, magnitude(2000)) && converts_inline(
    True,
    8192,
    magnitude(2000),
  )
}

test ignores_reference_inputs_the_material_does_not_name() {
  let value = magnitude(8192)
  let staging = stage([cbor.serialise(value)])
  spend(
    step(
      arguments(False, 0, value),
      builtin.integer_to_bytearray(False, 0, value),
      [SmallIntegerOperand { value: 0 }, staged(0)],
      StagingV1 {
        ..staging,
        reference_inputs: list.concat(
          staging.reference_inputs,
          [other_input(250)],
        ),
      },
    ),
  )
}

// ## Wrong results are refused

test refuses_the_bytes_of_the_other_byte_order() fail {
  let value = magnitude(8192)
  spend(claiming(True, 0, value, builtin.integer_to_bytearray(False, 0, value)))
}

test refuses_bytes_padded_to_another_width() fail {
  let value = magnitude(4096)
  spend(
    claiming(True, 8192, value, builtin.integer_to_bytearray(True, 8191, value)),
  )
}

test refuses_unpadded_bytes_at_a_width() fail {
  let value = magnitude(4096)
  spend(
    claiming(False, 8192, value, builtin.integer_to_bytearray(False, 0, value)),
  )
}

test refuses_a_changed_byte() fail {
  let value = magnitude(8192)
  let honest = builtin.integer_to_bytearray(True, 0, value)
  spend(
    claiming(
      True,
      0,
      value,
      builtin.cons_bytearray(0x5b, builtin.slice_bytearray(1, 8191, honest)),
    ),
  )
}

// ## Failures are not successes

test refuses_an_input_wider_than_its_width() fail {
  let value = magnitude(8192)
  spend(
    claiming(
      True,
      8191,
      value,
      builtin.slice_bytearray(
        1,
        8191,
        builtin.integer_to_bytearray(True, 0, value),
      ),
    ),
  )
}

test refuses_a_width_past_the_widest() fail {
  let value = magnitude(733)
  spend(
    claiming(
      True,
      8193,
      value,
      builtin.append_bytearray(
        #"00",
        builtin.integer_to_bytearray(True, 8192, value),
      ),
    ),
  )
}

test refuses_a_negative_input() fail {
  let value = magnitude(8192)
  spend(claiming(True, 0, -value, builtin.integer_to_bytearray(True, 0, value)))
}

test refuses_a_negative_input_as_the_bytes_of_its_encoding() fail {
  // The canonical encoding of -n carries the bytes of n - 1.
  let value = magnitude(8192)
  spend(
    claiming(True, 0, -value, builtin.integer_to_bytearray(True, 0, value - 1)),
  )
}

// ## Binding

test refuses_an_input_bound_by_its_leaf_alone() fail {
  let value = magnitude(8192)
  let integer_cbor = cbor.serialise(value)
  spend(
    step(
      arguments(True, 0, value),
      builtin.integer_to_bytearray(True, 0, value),
      [
        SmallIntegerOperand { value: 0 },
        IntegerLeafOperand {
          cbor_root: flat_root_v1(integer_cbor),
          cbor_length: builtin.length_of_bytearray(integer_cbor),
          words: magnitude_words_v1(parse_integer_cbor_bulk_v1(integer_cbor)),
        },
      ],
      no_staging(),
    ),
  )
}

test refuses_a_staged_input_other_than_the_argument() fail {
  let value = magnitude(8192)
  spend(
    step(
      arguments(True, 0, value),
      builtin.integer_to_bytearray(True, 0, value + 1),
      [SmallIntegerOperand { value: 0 }, staged(0)],
      stage([cbor.serialise(value + 1)]),
    ),
  )
}

test refuses_a_revealed_width_other_than_the_argument() fail {
  let value = magnitude(4096)
  spend(
    step(
      arguments(True, 8192, value),
      builtin.integer_to_bytearray(True, 8191, value),
      [SmallIntegerOperand { value: 8191 }, staged(0)],
      stage([cbor.serialise(value)]),
    ),
  )
}

test refuses_an_endianness_that_is_not_a_boolean() fail {
  let value = magnitude(8192)
  spend(
    step(
      [integer_value(1), integer_value(0), integer_value(value)],
      builtin.integer_to_bytearray(False, 0, value),
      [SmallIntegerOperand { value: 0 }, staged(0)],
      stage([cbor.serialise(value)]),
    ),
  )
}

test refuses_a_byte_string_input() fail {
  let bytes = builtin.integer_to_bytearray(True, 0, magnitude(8192))
  spend(
    step(
      [boolean_value(True), integer_value(0), bytes_value(bytes, False)],
      bytes,
      [SmallIntegerOperand { value: 0 }, staged(0)],
      stage([bytes]),
    ),
  )
}

// ## The budget hop

/// The budget hop for integerToByteString over `step_arguments` claiming
/// `result`, charged as Cardano charges the plain arguments, with the step
/// it continues to at semantics index `index`.
fn budget_routes(
  step_arguments: List<ValueWitnessV1>,
  result: ByteArray,
  index: Int,
) -> Bool {
  let hop = budget_step(73, step_arguments, bytes_value(result, False))
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
    hop.datum,
    hop.redeemer,
    fixture.own_out_ref(),
    hop.transaction,
  )
}

fn charges_as_cardano(big_endian: Bool, width: Int, value: Int) -> Bool {
  budget_routes(
    arguments(big_endian, width, value),
    builtin.integer_to_bytearray(big_endian, width, value),
    9,
  )
}

test the_budget_hop_charges_as_cardano_and_routes_here() {
  list.all(
    [
      Pair(0, 8192), Pair(8192, 8192), Pair(8192, 733), Pair(4464, 4464),
      Pair(4465, 4464), Pair(1, 1), Pair(9, 1),
    ],
    fn(row) {
      let Pair(width, size) = row
      charges_as_cardano(True, width, magnitude(size)) && charges_as_cardano(
        False,
        width,
        magnitude(size),
      )
    },
  ) && charges_as_cardano(True, 0, 0) && charges_as_cardano(False, 8192, 0)
}

test the_budget_hop_does_not_route_a_conversion_to_the_byte_string_semantics() fail {
  let value = magnitude(8192)
  budget_routes(
    arguments(True, 0, value),
    builtin.integer_to_bytearray(True, 0, value),
    7,
  )
}

test the_budget_hop_refuses_the_charge_of_another_width() fail {
  // Cardano charges the width 8,192 memory of 1,024 words; a result of 8,191
  // bytes is charged the same, so the result must differ by a word.
  let value = magnitude(4096)
  budget_routes(
    arguments(True, 8192, value),
    builtin.integer_to_bytearray(True, 8184, value),
    9,
  )
}

// ## Maximum shape

/// integerToByteString of the widest input (1,024 words) at the widest width,
/// little-endian, with its CBOR staged.
fn widest_step() -> StagedStepV1 {
  let value = magnitude(8192)
  claiming(False, 8192, value, builtin.integer_to_bytearray(False, 8192, value))
}

fn built(staged_step: StagedStepV1) -> Bool {
  when staged_step.datum is {
    Some(_) -> True
    None -> False
  }
}

test staged_integer_to_bytes_at_the_maximum_shape() {
  spend(widest_step())
}

test staged_integer_to_bytes_fixture_only_at_the_maximum_shape() {
  built(widest_step())
}
