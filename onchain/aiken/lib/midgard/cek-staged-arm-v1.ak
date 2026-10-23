//// The staged semantic arms: byte-string, string and integer builtins whose
//// operands are bound to their committed leaves through bytes the material
//// reveals inline or reads from staging outputs among the transaction's
//// reference inputs.
////
//// Every argument and the result are checked in the compacted form the roots
//// hop committed (a type, a Data summary of the payload and a constant-level
//// memory), so an argument within the direct carriage bound and one beyond it
//// are bound the same way. Each argument has one source in the material:
////
//// - `InlineOperand`: the bytes (a byte string's or string's bytes, an
////   integer's canonical CBOR), re-hashed to the argument's leaf;
//// - `StagedOperand`: the same bytes as staging runs, named by the index of
////   their run list in `runs.operands`;
//// - `BytesLeafOperand` / `IntegerLeafOperand`: only the leaf preimage, for
////   the builtins that read no more than an operand's length or identity;
//// - `SmallIntegerOperand`: the value of an integer argument of a byte-string
////   builtin (a byte, an index or a slice bound), re-encoded to its leaf.
////
//// Whatever the source, the bytes are bound to the argument's committed leaf
//// and its constant-level memory is checked against them. The arms admit
//// operands beyond the direct carriage bound, so they check protocol version
//// 11's argument bounds themselves before binding: every byte-string argument
//// of a bounded builtin is at most 65,536 bytes, and every integer argument of
//// the integer builtins (but equalsInteger) is in [-2^262143, 2^262143 - 1].
//// A success past a bound is refused. The bounds are on arguments only: a
//// result past the integer range is a success, as on L1.

use aiken/builtin
use aiken/cbor
use aiken/collection/list
use cardano/transaction.{Input}
use midgard/cek_builtin_v1.{
  BoolResult, BytesResult, IntegerResult, ScalarResultV1, SemanticConstantValue,
  StringResult, ValueWitnessV1,
}
use midgard/cek_constant_v1
use midgard/cek_data_v1
use midgard/cek_semantic_chain as chain
use midgard/cek_staged_blob_v1.{
  StagedRunsV1, bind_operand_v1, collect_runs_v1, concat_balanced_v1,
  flat_root_of_pieces_v1, flat_root_v1,
}
use midgard/cek_staged_integer_cbor_v1.{
  compare_canonical_integers_v1, encode_integer_cbor_bulk_v1,
  parse_integer_cbor_bulk_v1,
}
use midgard/cek_staged_integer_v1.{
  CanonicalIntegerV1, SignedLimbsV1, add_signed_v1, from_limbs_v1,
  integer_leaf_from_cbor_v1, magnitude_words_v1, signed_limbs_v1, sub_limbs_v1,
  subtract_signed_v1,
}

pub type OperandSourceV1 {
  InlineOperand { bytes: ByteArray }
  StagedOperand { operand: Int }
  BytesLeafOperand { root: ByteArray, length: Int }
  IntegerLeafOperand { cbor_root: ByteArray, cbor_length: Int, words: Int }
  SmallIntegerOperand { value: Int }
}

/// The material of a staged step: the staging outputs it reads and one source
/// per builtin argument, in argument order.
pub type StagedMaterialV1 {
  runs: StagedRunsV1,
  sources: List<OperandSourceV1>,
}

/// The canonical CBOR length of a byte string of 65,536 bytes, the longest
/// byte-string argument protocol version 11 admits. The length is strictly
/// increasing in the byte count, so a leaf at or under it is in bound.
pub const max_bounded_bytes_cbor_length = 67586

/// The CBOR argument bytes of the integers at either end of the range.
const max_bounded_integer_argument_bytes = 32768

// The type CBOR `decode_constant_type_v1` accepts is canonical, so a type is
// recognised by comparing its CBOR with that type's one encoding.
const integer_type_cbor =
  cek_constant_v1.encode_constant_type_v1(cek_constant_v1.IntegerConstant)

const bytes_type_cbor =
  cek_constant_v1.encode_constant_type_v1(cek_constant_v1.ByteStringConstant)

const string_type_cbor =
  cek_constant_v1.encode_constant_type_v1(cek_constant_v1.StringConstant)

/// The compacted Boolean constant `value`, as the roots hop commits it.
pub fn boolean_result_v1(value: Bool) -> ValueWitnessV1 {
  if value {
    true_result
  } else {
    false_result
  }
}

const true_result: ValueWitnessV1 = boolean_constant(builtin.constr_data(1, []))

const false_result: ValueWitnessV1 =
  boolean_constant(builtin.constr_data(0, []))

fn boolean_constant(payload: Data) -> ValueWitnessV1 {
  SemanticConstantValue {
    type_cbor: cek_constant_v1.encode_constant_type_v1(
      cek_constant_v1.BooleanConstant,
    ),
    payload: cek_data_v1.semantic_data_summary_v1(payload),
    memory: cek_constant_v1.constant_payload_memory_size_v1(
      cek_constant_v1.BooleanConstant,
      payload,
    ),
  }
}

/// The 64-bit bounds of a machine integer.
const int64_min = -9223372036854775808

const int64_max = 9223372036854775807

/// The compacted Integer constant `value`, which must be a machine integer
/// (its magnitude is then one 64-bit word).
pub fn small_integer_result_v1(value: Int) -> ValueWitnessV1 {
  expect value >= int64_min && value <= int64_max
  let integer_cbor = cbor.serialise(value)
  SemanticConstantValue {
    type_cbor: integer_type_cbor,
    payload: integer_leaf_from_cbor_v1(
      flat_root_v1(integer_cbor),
      builtin.length_of_bytearray(integer_cbor),
      1,
    ),
    memory: 1,
  }
}

/// A byte-string or string operand bound to its committed leaf: the flat root
/// and length of its bytes, and the bytes as pieces when its source reveals
/// them.
type BoundBytesV1 {
  root: ByteArray,
  length: Int,
  pieces: Option<List<ByteArray>>,
}

type ArmInputsV1 {
  tag: Int,
  arguments: List<Pair<ValueWitnessV1, OperandSourceV1>>,
  result: ValueWitnessV1,
  runs: List<List<ByteArray>>,
  operands: List<List<Int>>,
}

fn arm_inputs_v1(
  bound: chain.CoreBound,
  witness: Data,
  reference_inputs: List<Input>,
) -> ArmInputsV1 {
  let raw = chain.decode_builtin_success(bound.arm, witness)
  expect facts: chain.SemanticRoots = bound.facts
  expect material: StagedMaterialV1 = raw.material
  ArmInputsV1 {
    tag: raw.tag,
    arguments: zip_arguments_v1(facts.arguments, material.sources),
    result: facts.result,
    runs: collect_runs_v1(reference_inputs, material.runs.refs),
    operands: material.runs.operands,
  }
}

fn zip_arguments_v1(
  arguments: List<ValueWitnessV1>,
  sources: List<OperandSourceV1>,
) -> List<Pair<ValueWitnessV1, OperandSourceV1>> {
  when arguments is {
    [] -> {
      expect [] = sources
      []
    }
    [argument, ..rest] -> {
      expect [source, ..sources_rest] = sources
      [Pair(argument, source), ..zip_arguments_v1(rest, sources_rest)]
    }
  }
}

/// The bytes of a staged operand, as its chunks in order, and their flat root.
fn staged_operand_v1(
  inputs: ArmInputsV1,
  operand: Int,
) -> Pair<ByteArray, List<ByteArray>> {
  expect Some(indices) = list.at(inputs.operands, operand)
  let staged = bind_operand_v1(inputs.runs, indices)
  Pair(staged.root, staged.chunks)
}

fn bytes_memory_v1(string: Bool, length: Int) -> Int {
  if string {
    length / 4
  } else {
    cek_data_v1.bytes_memory_size_v1(length)
  }
}

fn bind_bytes_v1(
  inputs: ArmInputsV1,
  argument: Pair<ValueWitnessV1, OperandSourceV1>,
  string: Bool,
  bounded: Bool,
) -> BoundBytesV1 {
  let Pair(value, source) = argument
  expect SemanticConstantValue { type_cbor, payload, memory } = value
  expect
    type_cbor == if string {
      string_type_cbor
    } else {
      bytes_type_cbor
    }
  expect or {
      !bounded,
      payload.cbor_length <= max_bounded_bytes_cbor_length,
    }
  let Pair(root, pieces) = when source is {
    InlineOperand { bytes } -> Pair(flat_root_v1(bytes), Some([bytes]))
    StagedOperand { operand } -> {
      let Pair(root, chunks) = staged_operand_v1(inputs, operand)
      Pair(root, Some(chunks))
    }
    BytesLeafOperand { root, .. } -> Pair(root, None)
    _ -> fail
  }
  let length = when source is {
    BytesLeafOperand { length, .. } -> length
    _ -> {
      expect Some(revealed) = pieces
      pieces_length_v1(revealed)
    }
  }
  expect cek_data_v1.bytes_data_summary_v1(length, root) == payload
  expect memory == bytes_memory_v1(string, length)
  BoundBytesV1 { root, length, pieces }
}

/// The value of an integer argument of a byte-string builtin.
fn small_integer_v1(argument: Pair<ValueWitnessV1, OperandSourceV1>) -> Int {
  let Pair(value, source) = argument
  expect SmallIntegerOperand { value: integer } = source
  expect small_integer_result_v1(integer) == value
  integer
}

fn pieces_length_v1(pieces: List<ByteArray>) -> Int {
  list.foldl(
    pieces,
    0,
    fn(piece, total) { total + builtin.length_of_bytearray(piece) },
  )
}

/// True when `result` (compacted) is the byte string or string `pieces` join to.
fn result_is_pieces_v1(
  result: ValueWitnessV1,
  string: Bool,
  pieces: List<ByteArray>,
) -> Bool {
  let length = pieces_length_v1(pieces)
  let root = when pieces is {
    [whole] -> flat_root_v1(whole)
    _ -> flat_root_of_pieces_v1(pieces)
  }
  result == SemanticConstantValue {
    type_cbor: if string {
      string_type_cbor
    } else {
      bytes_type_cbor
    },
    payload: cek_data_v1.bytes_data_summary_v1(length, root),
    memory: bytes_memory_v1(string, length),
  }
}

fn result_is_v1(result: ValueWitnessV1, expected: ScalarResultV1) -> Bool {
  when expected is {
    BytesResult(pieces) -> result_is_pieces_v1(result, False, pieces)
    StringResult(pieces) -> result_is_pieces_v1(result, True, pieces)
    IntegerResult(value) -> result == small_integer_result_v1(value)
    BoolResult(value) -> result == boolean_result_v1(value)
  }
}

/// The byte-string and string builtins (tags 10 to 20, 22 to 25, 71, 72 and
/// 86) over staged operands.
pub fn verify_staged_bytes_v1(
  bound: chain.CoreBound,
  witness: Data,
  reference_inputs: List<Input>,
) -> Bool {
  let inputs = arm_inputs_v1(bound, witness, reference_inputs)
  let tag = inputs.tag
  let string = cek_builtin_v1.string_operands_v1(tag)
  // lengthOfByteString and the string builtins take operands of any length.
  let bounded = tag != 13 && !string
  let Pair(byte_arguments, integer_arguments) = cek_builtin_v1.scalar_operands_v1(
    tag,
    inputs.arguments,
  )
  let operands = list.map(
    byte_arguments,
    fn(argument) { bind_bytes_v1(inputs, argument, string, bounded) },
  )
  if tag == 13 {
    expect [operand] = operands
    result_is_v1(inputs.result, IntegerResult(operand.length))
  } else if tag == 15 || tag == 23 {
    // Equal bytes have equal flat roots, and unequal bytes unequal ones.
    expect [left, right] = operands
    result_is_v1(inputs.result, BoolResult(left.root == right.root))
  } else if tag == 24 {
    // encodeUtf8 keeps the bytes, so the result has the operand's leaf.
    expect [operand] = operands
    inputs.result == SemanticConstantValue {
      type_cbor: bytes_type_cbor,
      payload: cek_data_v1.bytes_data_summary_v1(operand.length, operand.root),
      memory: bytes_memory_v1(False, operand.length),
    }
  } else {
    result_is_v1(
      inputs.result,
      cek_builtin_v1.bytes_relation_v1(
        tag,
        list.map(
          operands,
          fn(operand) {
            expect Some(pieces) = operand.pieces
            pieces
          },
        ),
        list.map(integer_arguments, small_integer_v1),
      ),
    )
  }
}

/// An integer operand: its committed leaf's root, and its canonical CBOR read
/// when its source reveals it.
fn bind_integer_v1(
  inputs: ArmInputsV1,
  argument: Pair<ValueWitnessV1, OperandSourceV1>,
  bounded: Bool,
) -> Pair<ByteArray, Option<CanonicalIntegerV1>> {
  let Pair(value, source) = argument
  expect SemanticConstantValue { type_cbor, payload, memory } = value
  expect type_cbor == integer_type_cbor
  // A bounded builtin needs the operand's CBOR, whose range is checked below;
  // a leaf alone does not serve it.
  when source is {
    IntegerLeafOperand { cbor_root, cbor_length, words } -> {
      expect integer_leaf_from_cbor_v1(cbor_root, cbor_length, words) == payload
      expect memory == words
      Pair(payload.root, None)
    }
    _ -> {
      let Pair(cbor_root, integer_cbor) = when source is {
        InlineOperand { bytes } -> Pair(flat_root_v1(bytes), bytes)
        StagedOperand { operand } -> {
          let Pair(root, chunks) = staged_operand_v1(inputs, operand)
          Pair(root, concat_balanced_v1(chunks))
        }
        _ -> fail
      }
      let integer = parse_integer_cbor_bulk_v1(integer_cbor)
      let words = magnitude_words_v1(integer)
      expect
        integer_leaf_from_cbor_v1(
          cbor_root,
          builtin.length_of_bytearray(integer_cbor),
          words,
        ) == payload
      expect memory == words
      expect or {
          !bounded,
          integer_in_range_v1(integer),
        }
      Pair(payload.root, Some(integer))
    }
  }
}

/// True when the integer is in [-2^262143, 2^262143 - 1]: its CBOR argument
/// (n, or -n - 1 when negative) is under 2^262143.
fn integer_in_range_v1(integer: CanonicalIntegerV1) -> Bool {
  let size = builtin.length_of_bytearray(integer.argument)
  or {
    size < max_bounded_integer_argument_bytes,
    and {
      size == max_bounded_integer_argument_bytes,
      builtin.index_bytearray(integer.argument, 0) <= 0x7f,
    },
  }
}

fn canonical_of_signed_v1(integer: SignedLimbsV1) -> CanonicalIntegerV1 {
  let SignedLimbsV1 { negative, magnitude } = integer
  if negative {
    CanonicalIntegerV1 {
      negative: True,
      argument: from_limbs_v1(sub_limbs_v1(magnitude, [1])),
    }
  } else {
    CanonicalIntegerV1 { negative: False, argument: from_limbs_v1(magnitude) }
  }
}

/// True when `result` (compacted) is the integer `integer`.
fn result_is_integer_v1(result: ValueWitnessV1, integer: SignedLimbsV1) -> Bool {
  let canonical = canonical_of_signed_v1(integer)
  let integer_cbor = encode_integer_cbor_bulk_v1(canonical)
  let words = magnitude_words_v1(canonical)
  result == SemanticConstantValue {
    type_cbor: integer_type_cbor,
    payload: integer_leaf_from_cbor_v1(
      flat_root_v1(integer_cbor),
      builtin.length_of_bytearray(integer_cbor),
      words,
    ),
    memory: words,
  }
}

/// The linear integer builtins (addInteger, subtractInteger, equalsInteger,
/// lessThanInteger, lessThanEqualsInteger) over staged operands.
pub fn verify_staged_integer_v1(
  bound: chain.CoreBound,
  witness: Data,
  reference_inputs: List<Input>,
) -> Bool {
  let inputs = arm_inputs_v1(bound, witness, reference_inputs)
  let tag = inputs.tag
  // equalsInteger takes operands of any size.
  let bounded = tag != 7
  expect [left_argument, right_argument] = inputs.arguments
  let Pair(left_root, left) = bind_integer_v1(inputs, left_argument, bounded)
  let Pair(right_root, right) = bind_integer_v1(inputs, right_argument, bounded)
  if tag == 7 {
    // Bound leaves are canonical, so equal values have equal leaves.
    inputs.result == boolean_result_v1(left_root == right_root)
  } else {
    expect Some(left_integer) = left
    expect Some(right_integer) = right
    if tag == 8 || tag == 9 {
      inputs.result == boolean_result_v1(
        cek_builtin_v1.integer_order_relation_v1(
          tag,
          compare_canonical_integers_v1(left_integer, right_integer),
        ),
      )
    } else {
      let left_limbs = signed_limbs_v1(left_integer)
      let right_limbs = signed_limbs_v1(right_integer)
      result_is_integer_v1(
        inputs.result,
        if tag == 0 {
          add_signed_v1(left_limbs, right_limbs)
        } else {
          expect tag == 1
          subtract_signed_v1(left_limbs, right_limbs)
        },
      )
    }
  }
}
