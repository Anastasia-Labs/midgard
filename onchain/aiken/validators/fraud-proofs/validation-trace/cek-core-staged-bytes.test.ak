//// Transaction-level rows for the staged byte-string and string semantics
//// step (arm 31, group 5, semantics index 7). Each row spends the step with
//// a builtin's arguments, its claimed result and the material that binds the
//// arguments (inline bytes, staging outputs read as reference inputs, or
//// leaf preimages), and the step must accept exactly the results Cardano
//// returns.

use aiken/builtin
use aiken/collection/list
use cardano/transaction.{Input, Transaction}
use fraud_proofs/validation_trace/cek_core_builtin_budget as budget
use fraud_proofs/validation_trace/cek_core_semantic_data_misc as data_misc
use fraud_proofs/validation_trace/cek_core_staged_bytes as staged_bytes
use midgard/cek_builtin_v1.{SemanticConstantValue, ValueWitnessV1}
use midgard/cek_data_v1.{DataSummaryV1}
use midgard/cek_machine_v1 as machine
use midgard/cek_staged_arm_v1.{
  BytesLeafOperand, InlineOperand, OperandSourceV1, SmallIntegerOperand,
}
use midgard/cek_staged_blob_v1.{
  StagedRunsV1, flat_root_of_pieces_v1, flat_root_v1,
}
use midgard/fraud_proofs/cek_staged_fixture_v1.{
  StagedStepV1, StagingV1, boolean_value, budget_step, bytes_value, chunks_of,
  compact, flip_last_byte, integer_value, material, no_staging, other_input,
  pattern, semantic_bytes, semantic_pieces, stage, stage_pieces, staged,
  staged_step, staged_step_at, staging_input, text,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture

// ## Spending

fn spend(step: StagedStepV1) -> Bool {
  staged_bytes.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    step.datum,
    step.redeemer,
    fixture.own_out_ref(),
    step.transaction,
  )
}

/// Spends the step for `tag` over `arguments` claiming `result`, with
/// `sources` over `staging`.
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

/// A unary builtin over one staged operand `bytes`.
fn unary_staged(tag: Int, bytes: ByteArray, result: ValueWitnessV1) -> Bool {
  verifies(
    tag,
    [bytes_value(bytes, False)],
    result,
    stage([bytes]),
    [staged(0)],
  )
}

/// A binary builtin over two staged operands.
fn binary_staged(
  tag: Int,
  left: ByteArray,
  right: ByteArray,
  string: Bool,
  result: ValueWitnessV1,
) -> Bool {
  verifies(
    tag,
    [bytes_value(left, string), bytes_value(right, string)],
    result,
    stage([left, right]),
    [staged(0), staged(1)],
  )
}

fn leaf(bytes: ByteArray) -> OperandSourceV1 {
  leaf_of_pieces([bytes])
}

fn leaf_of_pieces(pieces: List<ByteArray>) -> OperandSourceV1 {
  BytesLeafOperand {
    root: flat_root_of_pieces_v1(pieces),
    length: list.foldl(
      pieces,
      0,
      fn(piece, total) { total + builtin.length_of_bytearray(piece) },
    ),
  }
}

fn digest(bytes: ByteArray) -> ValueWitnessV1 {
  bytes_value(bytes, False)
}

// ## Honest steps verify

test appends_a_small_inline_operand_to_a_staged_operand() {
  let left = pattern(100, 3)
  let right = pattern(9216, 5)
  verifies(
    10,
    [bytes_value(left, False), bytes_value(right, False)],
    bytes_value(builtin.append_bytearray(left, right), False),
    stage([right]),
    [InlineOperand { bytes: left }, staged(0)],
  )
}

test appends_two_staged_operands_at_the_argument_bound() {
  let left = pattern(65536, 1)
  let right = pattern(65536, 2)
  binary_staged(10, left, right, False, semantic_pieces([left, right], False))
}

test hashes_a_staged_operand_at_the_argument_bound_with_sha2() {
  let bytes = pattern(65536, 9)
  unary_staged(18, bytes, digest(builtin.sha2_256(bytes)))
}

test hashes_a_staged_operand_with_sha3() {
  let bytes = pattern(9216, 9)
  unary_staged(19, bytes, digest(builtin.sha3_256(bytes)))
}

test hashes_a_staged_operand_with_blake2b_256() {
  let bytes = pattern(16384, 9)
  unary_staged(20, bytes, digest(builtin.blake2b_256(bytes)))
}

test hashes_a_staged_operand_with_keccak_256() {
  let bytes = pattern(32768, 9)
  unary_staged(71, bytes, digest(builtin.keccak_256(bytes)))
}

test hashes_a_staged_operand_with_blake2b_224() {
  let bytes = pattern(9216, 4)
  unary_staged(72, bytes, digest(builtin.blake2b_224(bytes)))
}

test hashes_a_staged_operand_with_ripemd_160() {
  let bytes = pattern(9216, 6)
  unary_staged(86, bytes, digest(builtin.ripemd_160(bytes)))
}

test conses_a_byte_onto_a_staged_operand() {
  let bytes = pattern(9216, 2)
  verifies(
    11,
    [integer_value(7), bytes_value(bytes, False)],
    bytes_value(builtin.cons_bytearray(7, bytes), False),
    stage([bytes]),
    [SmallIntegerOperand { value: 7 }, staged(0)],
  )
}

test slices_a_staged_operand_at_the_argument_bound() {
  let bytes = pattern(65536, 8)
  verifies(
    12,
    [integer_value(100), integer_value(20000), bytes_value(bytes, False)],
    bytes_value(builtin.slice_bytearray(100, 20000, bytes), False),
    stage([bytes]),
    [
      SmallIntegerOperand { value: 100 },
      SmallIntegerOperand { value: 20000 },
      staged(0),
    ],
  )
}

test measures_an_operand_past_the_argument_bound_from_its_leaf() {
  let pieces = [pattern(65536, 8), pattern(4464, 0)]
  verifies(
    13,
    [semantic_pieces(pieces, False)],
    integer_value(70000),
    no_staging(),
    [leaf_of_pieces(pieces)],
  )
}

test indexes_the_last_byte_of_a_staged_operand() {
  let bytes = pattern(9216, 1)
  verifies(
    14,
    [bytes_value(bytes, False), integer_value(9215)],
    integer_value(builtin.index_bytearray(bytes, 9215)),
    stage([bytes]),
    [staged(0), SmallIntegerOperand { value: 9215 }],
  )
}

test compares_equal_operands_at_the_argument_bound_by_leaf() {
  let bytes = pattern(65536, 3)
  verifies(
    15,
    [bytes_value(bytes, False), bytes_value(bytes, False)],
    boolean_value(True),
    no_staging(),
    [leaf(bytes), leaf(bytes)],
  )
}

test compares_unequal_operands_by_leaf() {
  let left = pattern(65536, 3)
  let right = flip_last_byte(left)
  verifies(
    15,
    [bytes_value(left, False), bytes_value(right, False)],
    boolean_value(False),
    no_staging(),
    [leaf(left), leaf(right)],
  )
}

test orders_two_staged_operands_strictly() {
  let left = pattern(9216, 3)
  let right = flip_last_byte(left)
  binary_staged(
    16,
    left,
    right,
    False,
    boolean_value(builtin.less_than_bytearray(left, right)),
  )
}

test orders_two_staged_operands_at_the_argument_bound() {
  let left = pattern(65536, 3)
  let right = pattern(65536, 4)
  binary_staged(
    17,
    left,
    right,
    False,
    boolean_value(builtin.less_than_equals_bytearray(left, right)),
  )
}

test verifies_every_bounded_builtin_at_the_argument_bound() {
  let bytes = pattern(65536, 5)
  let words = text(65536)
  and {
    verifies(
      11,
      [integer_value(7), bytes_value(bytes, False)],
      semantic_pieces([#"07", bytes], False),
      stage([bytes]),
      [SmallIntegerOperand { value: 7 }, staged(0)],
    ),
    verifies(
      14,
      [bytes_value(bytes, False), integer_value(65535)],
      integer_value(builtin.index_bytearray(bytes, 65535)),
      stage([bytes]),
      [staged(0), SmallIntegerOperand { value: 65535 }],
    ),
    unary_staged(19, bytes, digest(builtin.sha3_256(bytes))),
    unary_staged(20, bytes, digest(builtin.blake2b_256(bytes))),
    unary_staged(71, bytes, digest(builtin.keccak_256(bytes))),
    unary_staged(72, bytes, digest(builtin.blake2b_224(bytes))),
    unary_staged(86, bytes, digest(builtin.ripemd_160(bytes))),
    unary_staged(25, words, bytes_value(words, True)),
  }
}

test appends_two_staged_strings_past_the_argument_bound() {
  let left = text(40000)
  let right = text(40001)
  binary_staged(22, left, right, True, semantic_pieces([left, right], True))
}

test compares_strings_past_the_argument_bound_by_leaf() {
  let pieces = [text(65536), text(10)]
  verifies(
    23,
    [semantic_pieces(pieces, True), semantic_pieces(pieces, True)],
    boolean_value(True),
    no_staging(),
    [leaf_of_pieces(pieces), leaf_of_pieces(pieces)],
  )
}

test encodes_a_string_past_the_argument_bound() {
  let pieces = [text(65536), text(10)]
  verifies(
    24,
    [semantic_pieces(pieces, True)],
    semantic_pieces(pieces, False),
    no_staging(),
    [leaf_of_pieces(pieces)],
  )
}

test decodes_a_staged_operand_to_a_string() {
  let bytes = text(9216)
  unary_staged(25, bytes, bytes_value(bytes, True))
}

test operands_with_equal_bytes_may_share_staging_outputs() {
  let bytes = pattern(9216, 3)
  let staging = stage([bytes])
  verifies(
    17,
    [bytes_value(bytes, False), bytes_value(bytes, False)],
    boolean_value(True),
    staging,
    [staged(0), staged(0)],
  )
}

/// Steps whose operands are all within the direct carriage bound verify on
/// this route too, with the result the direct route computes.
test agrees_with_the_direct_route_on_inline_operands() {
  let left = pattern(1000, 1)
  let right = pattern(8000, 2)
  and {
    verifies(
      10,
      [bytes_value(left, False), bytes_value(right, False)],
      bytes_value(builtin.append_bytearray(left, right), False),
      no_staging(),
      [InlineOperand { bytes: left }, InlineOperand { bytes: right }],
    ),
    verifies(
      18,
      [bytes_value(right, False)],
      digest(builtin.sha2_256(right)),
      no_staging(),
      [InlineOperand { bytes: right }],
    ),
    verifies(
      13,
      [bytes_value(left, False)],
      integer_value(1000),
      no_staging(),
      [InlineOperand { bytes: left }],
    ),
    verifies(
      16,
      [bytes_value(left, False), bytes_value(right, False)],
      boolean_value(builtin.less_than_bytearray(left, right)),
      no_staging(),
      [InlineOperand { bytes: left }, InlineOperand { bytes: right }],
    ),
  }
}

// ## Wrong results are refused

test refuses_a_wrong_digest() fail {
  let bytes = pattern(65536, 9)
  unary_staged(18, bytes, digest(builtin.sha3_256(bytes)))
}

test refuses_a_concatenation_with_its_last_byte_changed() fail {
  let left = pattern(65536, 1)
  let right = pattern(65536, 2)
  binary_staged(
    10,
    left,
    right,
    False,
    semantic_pieces([left, flip_last_byte(right)], False),
  )
}

test refuses_a_wrong_comparison() fail {
  let left = pattern(9216, 3)
  let right = flip_last_byte(left)
  binary_staged(
    16,
    left,
    right,
    False,
    boolean_value(!builtin.less_than_bytearray(left, right)),
  )
}

test refuses_a_wrong_length() fail {
  let bytes = pattern(20000, 8)
  verifies(
    13,
    [bytes_value(bytes, False)],
    integer_value(20001),
    no_staging(),
    [leaf(bytes)],
  )
}

test refuses_a_byte_string_result_typed_as_a_string() fail {
  let left = pattern(100, 3)
  let right = pattern(9216, 5)
  verifies(
    10,
    [bytes_value(left, False), bytes_value(right, False)],
    semantic_bytes(builtin.append_bytearray(left, right), True),
    stage([right]),
    [InlineOperand { bytes: left }, staged(0)],
  )
}

// ## Argument bounds

test refuses_an_append_with_an_operand_past_the_argument_bound() fail {
  let left = pattern(65536, 1)
  let right = [pattern(65536, 2), #"00"]
  verifies(
    10,
    [bytes_value(left, False), semantic_pieces(right, False)],
    semantic_pieces([left, ..right], False),
    stage_pieces([[left], right]),
    [staged(0), staged(1)],
  )
}

test refuses_a_hash_of_an_operand_past_the_argument_bound() fail {
  let pieces = [pattern(65536, 9), #"00"]
  verifies(
    18,
    [semantic_pieces(pieces, False)],
    digest(builtin.sha2_256(pattern(65536, 9))),
    stage_pieces([pieces]),
    [staged(0)],
  )
}

test refuses_an_equality_of_operands_past_the_argument_bound() fail {
  let pieces = [pattern(65536, 3), #"00"]
  verifies(
    15,
    [semantic_pieces(pieces, False), semantic_pieces(pieces, False)],
    boolean_value(True),
    no_staging(),
    [leaf_of_pieces(pieces), leaf_of_pieces(pieces)],
  )
}

test refuses_a_slice_of_an_operand_past_the_argument_bound() fail {
  let bytes = pattern(65536, 8)
  let pieces = [bytes, #"00"]
  verifies(
    12,
    [integer_value(0), integer_value(10), semantic_pieces(pieces, False)],
    bytes_value(builtin.slice_bytearray(0, 10, bytes), False),
    stage_pieces([pieces]),
    [
      SmallIntegerOperand { value: 0 },
      SmallIntegerOperand { value: 10 },
      staged(0),
    ],
  )
}

/// A one-operand builtin `tag` over an operand one byte past the argument
/// bound, staged, claiming `result`.
fn unary_past_bound(tag: Int, result: ValueWitnessV1) -> Bool {
  let pieces = [text(65536), #"41"]
  verifies(
    tag,
    [semantic_pieces(pieces, False)],
    result,
    stage_pieces([pieces]),
    [staged(0)],
  )
}

test refuses_a_sha3_256_past_the_argument_bound() fail {
  unary_past_bound(19, digest(builtin.sha3_256(text(65536))))
}

test refuses_a_blake2b_256_past_the_argument_bound() fail {
  unary_past_bound(20, digest(builtin.blake2b_256(text(65536))))
}

test refuses_a_keccak_256_past_the_argument_bound() fail {
  unary_past_bound(71, digest(builtin.keccak_256(text(65536))))
}

test refuses_a_blake2b_224_past_the_argument_bound() fail {
  unary_past_bound(72, digest(builtin.blake2b_224(text(65536))))
}

test refuses_a_ripemd_160_past_the_argument_bound() fail {
  unary_past_bound(86, digest(builtin.ripemd_160(text(65536))))
}

test refuses_a_decoding_past_the_argument_bound() fail {
  unary_past_bound(25, semantic_pieces([text(65536), #"41"], True))
}

test refuses_a_cons_onto_an_operand_past_the_argument_bound() fail {
  let pieces = [pattern(65536, 2), #"00"]
  verifies(
    11,
    [integer_value(7), semantic_pieces(pieces, False)],
    semantic_pieces([#"07", ..pieces], False),
    stage_pieces([pieces]),
    [SmallIntegerOperand { value: 7 }, staged(0)],
  )
}

test refuses_an_index_into_an_operand_past_the_argument_bound() fail {
  let pieces = [pattern(65536, 2), #"00"]
  verifies(
    14,
    [semantic_pieces(pieces, False), integer_value(0)],
    integer_value(2),
    stage_pieces([pieces]),
    [staged(0), SmallIntegerOperand { value: 0 }],
  )
}

test refuses_an_order_of_operands_past_the_argument_bound() fail {
  let pieces = [pattern(65536, 3), #"00"]
  verifies(
    17,
    [semantic_pieces(pieces, False), semantic_pieces(pieces, False)],
    boolean_value(True),
    stage_pieces([pieces]),
    [staged(0), staged(0)],
  )
}

test refuses_a_strict_order_with_one_operand_past_the_argument_bound() fail {
  let left = pattern(10000, 3)
  let right = [pattern(65536, 3), #"00"]
  verifies(
    16,
    [bytes_value(left, False), semantic_pieces(right, False)],
    boolean_value(True),
    stage_pieces([[left], right]),
    [staged(0), staged(1)],
  )
}

test refuses_a_cons_of_a_value_past_a_byte() fail {
  let bytes = pattern(9216, 2)
  verifies(
    11,
    [integer_value(256), bytes_value(bytes, False)],
    bytes_value(builtin.cons_bytearray(0, bytes), False),
    stage([bytes]),
    [SmallIntegerOperand { value: 256 }, staged(0)],
  )
}

// ## Binding

/// The honest append row, with the staging and material passed through
/// `tweak` before the spend.
fn append_with(
  tweak: fn(StagingV1, List<OperandSourceV1>) ->
    Pair<StagingV1, List<OperandSourceV1>>,
) -> Bool {
  let left = pattern(9216, 1)
  let right = pattern(20000, 2)
  let Pair(staging, sources) = tweak(
    stage([left, right]),
    [staged(0), staged(1)],
  )
  verifies(
    10,
    [bytes_value(left, False), bytes_value(right, False)],
    semantic_pieces([left, right], False),
    staging,
    sources,
  )
}

test binds_the_untweaked_append() {
  append_with(fn(staging, sources) { Pair(staging, sources) })
}

test refuses_a_staging_output_missing_from_the_reference_inputs() fail {
  append_with(
    fn(staging, sources) {
      expect [first, _dropped, ..rest] = staging.reference_inputs
      Pair(StagingV1 { ..staging, reference_inputs: [first, ..rest] }, sources)
    },
  )
}

test refuses_staging_references_out_of_ledger_order() fail {
  append_with(
    fn(staging, sources) {
      expect [a, b, ..rest] = staging.runs.refs
      Pair(
        StagingV1 {
          ..staging,
          runs: StagedRunsV1 { ..staging.runs, refs: [b, a, ..rest] },
        },
        sources,
      )
    },
  )
}

test refuses_an_operand_naming_a_run_outside_the_references() fail {
  append_with(
    fn(staging, sources) {
      expect [first, second] = staging.runs.operands
      Pair(
        StagingV1 {
          ..staging,
          runs: StagedRunsV1 {
            ..staging.runs,
            operands: [first, list.concat(second, [99])],
          },
        },
        sources,
      )
    },
  )
}

test refuses_an_operand_naming_another_operands_runs() fail {
  append_with(fn(staging, _) { Pair(staging, [staged(0), staged(0)]) })
}

test refuses_operands_given_in_swapped_order() fail {
  append_with(fn(staging, _) { Pair(staging, [staged(1), staged(0)]) })
}

test refuses_a_staging_output_with_a_changed_chunk() fail {
  append_with(
    fn(staging, sources) {
      expect [other, first, ..rest] = staging.reference_inputs
      expect [chunk, ..chunks] = chunks_of(pattern(9216, 1))
      Pair(
        StagingV1 {
          ..staging,
          reference_inputs: [
            other,
            Input {
              ..staging_input(10, [flip_last_byte(chunk), ..chunks]),
              output_reference: first.output_reference,
            },
            ..rest
          ],
        },
        sources,
      )
    },
  )
}

test refuses_a_staging_output_with_an_oversized_chunk() fail {
  let bytes = pattern(9216, 1)
  let staging = stage([bytes])
  expect [other, first] = staging.reference_inputs
  verifies(
    18,
    [bytes_value(bytes, False)],
    digest(builtin.sha2_256(bytes)),
    StagingV1 {
      ..staging,
      reference_inputs: [
        other,
        Input {
          ..staging_input(
            10,
            [
              builtin.slice_bytearray(0, 4096, bytes),
              builtin.slice_bytearray(4096, 5120, bytes),
            ],
          ),
          output_reference: first.output_reference,
        },
      ],
    },
    [staged(0)],
  )
}

test refuses_a_leaf_preimage_one_byte_longer() fail {
  let bytes = pattern(20000, 8)
  verifies(
    13,
    [bytes_value(bytes, False)],
    integer_value(20001),
    no_staging(),
    [BytesLeafOperand { root: flat_root_v1(bytes), length: 20001 }],
  )
}

test refuses_a_leaf_preimage_one_byte_shorter() fail {
  let bytes = pattern(20000, 8)
  verifies(
    13,
    [bytes_value(bytes, False)],
    integer_value(19999),
    no_staging(),
    [BytesLeafOperand { root: flat_root_v1(bytes), length: 19999 }],
  )
}

test refuses_an_operand_whose_memory_differs_from_its_length() fail {
  let bytes = pattern(20000, 8)
  expect SemanticConstantValue { type_cbor, payload, memory } = semantic_bytes(
    bytes,
    False,
  )
  verifies(
    13,
    [SemanticConstantValue { type_cbor, payload, memory: memory + 1 }],
    integer_value(20000),
    no_staging(),
    [leaf(bytes)],
  )
}

test refuses_inline_bytes_that_differ_from_the_operand() fail {
  let left = pattern(100, 3)
  let right = pattern(9216, 5)
  verifies(
    10,
    [bytes_value(left, False), bytes_value(right, False)],
    bytes_value(builtin.append_bytearray(flip_last_byte(left), right), False),
    stage([right]),
    [InlineOperand { bytes: flip_last_byte(left) }, staged(0)],
  )
}

test refuses_a_string_operand_bound_as_a_byte_string() fail {
  verifies(
    25,
    [bytes_value("abcd", True)],
    bytes_value("abcd", True),
    no_staging(),
    [InlineOperand { bytes: "abcd" }],
  )
}

test refuses_a_small_integer_that_differs_from_its_argument() fail {
  let bytes = pattern(9216, 1)
  verifies(
    14,
    [bytes_value(bytes, False), integer_value(9215)],
    integer_value(builtin.index_bytearray(bytes, 0)),
    stage([bytes]),
    [staged(0), SmallIntegerOperand { value: 0 }],
  )
}

test refuses_an_encoding_whose_payload_differs_from_the_operand() fail {
  let pieces = [text(65536), text(10)]
  expect SemanticConstantValue { type_cbor, payload, memory } = semantic_pieces(
    pieces,
    False,
  )
  verifies(
    24,
    [semantic_pieces(pieces, True)],
    SemanticConstantValue {
      type_cbor,
      payload: DataSummaryV1 { ..payload, cbor_length: payload.cbor_length + 1 },
      memory,
    },
    no_staging(),
    [leaf_of_pieces(pieces)],
  )
}

test refuses_a_source_list_longer_than_the_arguments() fail {
  let bytes = pattern(9216, 9)
  verifies(
    18,
    [bytes_value(bytes, False)],
    digest(builtin.sha2_256(bytes)),
    stage([bytes]),
    [staged(0), staged(0)],
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
  let bytes = bytes_value(pattern(65536, 1), False)
  let words = bytes_value(text(65536), True)
  let long = semantic_pieces([pattern(65536, 1), pattern(4464, 0)], False)
  let small = integer_value(7)
  list.all(
    [
      Pair(10, [bytes, bytes]),
      Pair(11, [small, bytes]),
      Pair(12, [small, small, bytes]),
      Pair(13, [long]),
      Pair(14, [bytes, small]),
      Pair(15, [bytes, bytes]),
      Pair(16, [bytes, bytes]),
      Pair(17, [bytes, bytes]),
      Pair(18, [bytes]),
      Pair(19, [bytes]),
      Pair(20, [bytes]),
      Pair(71, [bytes]),
      Pair(72, [bytes]),
      Pair(86, [bytes]),
      Pair(22, [words, words]),
      Pair(23, [words, words]),
      Pair(24, [words]),
      Pair(25, [bytes_value(text(65536), False)]),
    ],
    fn(row) {
      let Pair(tag, arguments) = row
      routes_to(tag, arguments, 7)
    },
  )
}

test the_budget_hop_does_not_route_a_byte_string_builtin_to_the_integer_semantics() fail {
  routes_to(18, [bytes_value(pattern(65536, 1), False)], 8)
}

test refuses_an_integer_builtin() fail {
  verifies(
    0,
    [integer_value(1), integer_value(2)],
    integer_value(3),
    no_staging(),
    [SmallIntegerOperand { value: 1 }, SmallIntegerOperand { value: 2 }],
  )
}

test refuses_a_step_before_the_budget_hop() fail {
  let bytes = pattern(9216, 9)
  let staging = stage([bytes])
  spend(
    staged_step_at(
      18,
      [bytes_value(bytes, False)],
      digest(builtin.sha2_256(bytes)),
      material(staging, [staged(0)]),
      staging.reference_inputs,
      machine.mode_builtin,
      5,
      2,
    ),
  )
}

test refuses_a_step_in_another_group() fail {
  let bytes = pattern(9216, 9)
  let staging = stage([bytes])
  spend(
    staged_step_at(
      18,
      [bytes_value(bytes, False)],
      digest(builtin.sha2_256(bytes)),
      material(staging, [staged(0)]),
      staging.reference_inputs,
      machine.mode_builtin,
      2,
      3,
    ),
  )
}

test the_data_semantics_step_refuses_a_byte_string_builtin() fail {
  let bytes = pattern(9216, 9)
  let staging = stage([bytes])
  let step = staged_step(
    18,
    [bytes_value(bytes, False)],
    digest(builtin.sha2_256(bytes)),
    material(staging, [staged(0)]),
    staging.reference_inputs,
  )
  data_misc.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    step.datum,
    step.redeemer,
    fixture.own_out_ref(),
    step.transaction,
  )
}

test ignores_reference_inputs_the_material_does_not_name() {
  let bytes = pattern(9216, 9)
  let staging = stage([bytes])
  verifies(
    18,
    [bytes_value(bytes, False)],
    digest(builtin.sha2_256(bytes)),
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

test compacts_to_the_committed_form() {
  // The fixture's root-only constant for a value within the direct bound is
  // the form the roots hop commits for the inline constant.
  let bytes = pattern(8000, 4)
  compact(bytes_value(bytes, False)) == semantic_bytes(bytes, False)
}

// ## Signatures over staged messages
//
// verifyEd25519Signature and verifySchnorrSecp256k1Signature over a message
// past the direct carriage bound, bound through its staging runs; the key
// and signature are inline. Keys and signatures are fixed vectors over
// `pattern` messages.

const ed25519_key =
  #"ea4a6c63e29c520abef5507b132ec5f9954776aebebe7b92421eea691446d22c"

const schnorr_key =
  #"56b328b30c8bf5839e24058747879408bdb36241dc9c2e7c619faa12b2920967"

/// The signature of `tag`'s key over `pattern(65536, 3)`.
fn signature_at_the_bound(tag: Int) -> ByteArray {
  if tag == 21 {
    #"04ed5fc52f4a8159c2ad55d0e9f0ff67c4f3164646bd7c34ca24ffcd4b1d893d9dfab324d99afc373aa9cca144cd681fc43c94fff1449567fcf8c6ad57704807"
  } else {
    #"5f88e4bb68977fe0cc5f49a5583e4c14d45767ff6f031dfbd46a664dccb7009b464344b921a72ec1c44edd9ae892654d683d3e67fe3cf7149e74aa20c22c17ca"
  }
}

/// The signature of `tag`'s key over `pattern(9216, 1)`.
fn signature_past_the_direct_bound(tag: Int) -> ByteArray {
  if tag == 21 {
    #"d961b8397dbf30f76137bd920c359287df486c4cc3429ac75d66c07ed190ee71f35222fdd9d67e7418ab5b6c93099e6ffa30f5ffb50460695f1239386e9b030d"
  } else {
    #"66e5eecf14219364f9540e4f1dbd92696b35baec085c75995e0f2cbe34f9668e7809385bdb78b1f74d522e2bdebbaca97625af6f261f4a78771141bea1b6bbcf"
  }
}

fn signature_key(tag: Int) -> ByteArray {
  if tag == 21 {
    ed25519_key
  } else {
    schnorr_key
  }
}

/// Spends the step for signature builtin `tag` over `key`, the staged
/// `message` and `signature`, claiming `verified`.
fn signature_verifies(
  tag: Int,
  key: ByteArray,
  message: ByteArray,
  signature: ByteArray,
  verified: Bool,
) -> Bool {
  verifies(
    tag,
    [
      bytes_value(key, False),
      bytes_value(message, False),
      bytes_value(signature, False),
    ],
    boolean_value(verified),
    stage([message]),
    [
      InlineOperand { bytes: key },
      staged(0),
      InlineOperand { bytes: signature },
    ],
  )
}

const signature_tags = [21, 53]

test verifies_a_signature_over_a_staged_message_at_the_argument_bound() {
  list.all(
    signature_tags,
    fn(tag) {
      signature_verifies(
        tag,
        signature_key(tag),
        pattern(65536, 3),
        signature_at_the_bound(tag),
        True,
      )
    },
  )
}

test verifies_a_signature_over_a_message_past_the_direct_bound() {
  list.all(
    signature_tags,
    fn(tag) {
      signature_verifies(
        tag,
        signature_key(tag),
        pattern(9216, 1),
        signature_past_the_direct_bound(tag),
        True,
      )
    },
  )
}

test rejects_a_signature_over_another_message() {
  list.all(
    signature_tags,
    fn(tag) {
      and {
        signature_verifies(
          tag,
          signature_key(tag),
          pattern(65536, 4),
          signature_at_the_bound(tag),
          False,
        ),
        signature_verifies(
          tag,
          signature_key(tag),
          pattern(65536, 3),
          flip_last_byte(signature_at_the_bound(tag)),
          False,
        ),
      }
    },
  )
}

test refuses_a_claimed_verification_of_a_wrong_signature() fail {
  signature_verifies(
    53,
    schnorr_key,
    pattern(65536, 4),
    signature_at_the_bound(53),
    True,
  )
}

test refuses_a_claimed_rejection_of_a_valid_signature() fail {
  signature_verifies(
    21,
    ed25519_key,
    pattern(65536, 3),
    signature_at_the_bound(21),
    False,
  )
}

test refuses_a_signature_over_a_message_past_the_argument_bound() fail {
  let pieces = [pattern(65536, 3), #"01"]
  let staging = stage_pieces([pieces])
  verifies(
    21,
    [
      bytes_value(ed25519_key, False),
      semantic_pieces(pieces, False),
      bytes_value(signature_at_the_bound(21), False),
    ],
    boolean_value(False),
    staging,
    [
      InlineOperand { bytes: ed25519_key },
      staged(0),
      InlineOperand { bytes: signature_at_the_bound(21) },
    ],
  )
}

test refuses_a_schnorr_key_off_the_curve() fail {
  signature_verifies(
    53,
    builtin.replicate_byte(32, 0xff),
    pattern(65536, 3),
    signature_at_the_bound(53),
    False,
  )
}

test refuses_an_ed25519_key_of_31_bytes() fail {
  signature_verifies(
    21,
    builtin.slice_bytearray(0, 31, ed25519_key),
    pattern(65536, 3),
    signature_at_the_bound(21),
    False,
  )
}

test refuses_a_message_bound_by_its_leaf_alone() fail {
  let message = pattern(65536, 3)
  verifies(
    53,
    [
      bytes_value(schnorr_key, False),
      bytes_value(message, False),
      bytes_value(signature_at_the_bound(53), False),
    ],
    boolean_value(True),
    no_staging(),
    [
      InlineOperand { bytes: schnorr_key },
      leaf(message),
      InlineOperand { bytes: signature_at_the_bound(53) },
    ],
  )
}

test the_budget_hop_routes_the_signature_builtins_here() {
  list.all(
    signature_tags,
    fn(tag) {
      routes_to(
        tag,
        [
          bytes_value(signature_key(tag), False),
          bytes_value(pattern(65536, 3), False),
          bytes_value(signature_at_the_bound(tag), False),
        ],
        7,
      )
    },
  )
}

test the_budget_hop_does_not_route_schnorr_to_the_integer_semantics() fail {
  routes_to(
    53,
    [
      bytes_value(schnorr_key, False),
      bytes_value(pattern(65536, 3), False),
      bytes_value(signature_at_the_bound(53), False),
    ],
    8,
  )
}

/// verifySchnorrSecp256k1Signature over a staged message of 65,536 bytes.
fn schnorr_step_at_the_bound() -> StagedStepV1 {
  let message = pattern(65536, 3)
  let staging = stage([message])
  staged_step(
    53,
    [
      bytes_value(schnorr_key, False),
      bytes_value(message, False),
      bytes_value(signature_at_the_bound(53), False),
    ],
    boolean_value(True),
    material(
      staging,
      [
        InlineOperand { bytes: schnorr_key },
        staged(0),
        InlineOperand { bytes: signature_at_the_bound(53) },
      ],
    ),
    staging.reference_inputs,
  )
}

fn built(step: StagedStepV1) -> Bool {
  when step.datum is {
    Some(_) -> True
    None -> False
  }
}

test staged_bytes_schnorr_at_the_argument_bound() {
  spend(schnorr_step_at_the_bound())
}

test staged_bytes_schnorr_fixture_only_at_the_argument_bound() {
  built(schnorr_step_at_the_bound())
}
