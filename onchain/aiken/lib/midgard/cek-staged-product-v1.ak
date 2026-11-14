//// multiplyInteger and the division builtins (divideInteger, quotientInteger,
//// remainderInteger, modInteger) over staged operands, verified as a loop
//// over the columns of a product, a contiguous range of columns per hop.
////
//// Magnitudes are taken in 512-byte limbs, least significant first. Column k
//// of a product x * y is the sum of x[i] * y[j] over i + j = k; adding the
//// carry from column k - 1 gives the product's limb k and the carry into
//// column k + 1. A hop covers columns [cursor, end) and leaves the cursor and
//// the carries in the loop's state, so no hop holds more than one range of
//// columns at once.
////
//// - multiplyInteger: the product is |a| * |b|. Each limb is compared with the
////   limb of the claimed result that the result's CBOR spells at that place.
//// - The divisions carry the quotient q's canonical CBOR as one more source.
////   The columns are those of |q| * |b|, and the same pass computes the
////   remainder's magnitude limb by limb with a borrow: |a| - |q * b|, except
////   for divideInteger and modInteger over operands of different signs, where
////   it is |q * b| - |a|. It also compares that magnitude with |b| from the
////   least significant limb up. The final hop requires no borrow and a
////   remainder below |b|, so a zero divisor never finishes. q must be zero or
////   have the sign of a * b. quotientInteger and divideInteger return q, whose
////   CBOR is bound to the result's leaf. remainderInteger and modInteger also
////   carry the result's CBOR, which is compared with the remainder limb by
////   limb, and is zero or has the dividend's sign (remainderInteger) or the
////   divisor's (modInteger).
////
//// Every source is bound to its leaf on the first hop. Later hops read the
//// same bytes again (inline in the bound witness, or from the same staging
//// outputs, which no transaction can change) without hashing them.
//// Both arguments are bounded; q and the result are not.

use aiken/builtin
use aiken/collection/list
use cardano/transaction.{Input}
use midgard/cek_builtin_v1.{SemanticConstantValue, ValueWitnessV1}
use midgard/cek_semantic_chain as chain
use midgard/cek_staged_arm_v1.{
  ArmInputsV1, InlineOperand, OperandSourceV1, StagedMaterialV1, StagedOperand,
  bind_integer_v1, integer_type_cbor,
}
use midgard/cek_staged_blob_v1.{
  bind_operand_v1, collect_runs_v1, concat_balanced_v1, flat_root_v1,
  operand_chunks_v1,
}
use midgard/cek_staged_integer_cbor_v1.{
  full_heads_hold, parse_integer_cbor_bulk_v1, payloads,
}
use midgard/cek_staged_integer_v1.{
  CanonicalIntegerV1, SignedLimbsV1, integer_leaf_from_cbor_v1, limb_bytes,
  to_limbs_v1,
}

/// The loop's state between hops: the product's carry into the next column,
/// the remainder's borrow, the remainder against the divisor so far (-1 below,
/// 0 equal, 1 above), and the one carried into the claimed result's next limb
/// when that result is negative (its CBOR spells |n| - 1).
pub type ProductStateV1 {
  carry: Int,
  borrow: Int,
  order: Int,
  increment: Int,
}

/// The claimed result as a hop reads it: its sign, its CBOR argument's byte
/// length, its CBOR as pieces and where its segments lie. A result whose
/// argument fits one definite string (`full` is -1) is held as that argument.
type ClaimedV1 {
  negative: Bool,
  length: Int,
  pieces: List<ByteArray>,
  full: Int,
  tail_at: Int,
  words: Int,
}

/// What a hop reads: the factors of the column products (x is |a| for
/// multiplyInteger and |q| for the divisions; y is |b|, most significant limb
/// first), the claimed result and the remainder's direction. `base` is
/// 2^4096, one past the largest limb.
type HopV1 {
  base: Int,
  tag: Int,
  x: List<Int>,
  x_count: Int,
  y_reversed: List<Int>,
  y_count: Int,
  claimed: Option<ClaimedV1>,
  reversed: Bool,
}

fn min_v1(left: Int, right: Int) -> Int {
  if left < right {
    left
  } else {
    right
  }
}

fn max_v1(left: Int, right: Int) -> Int {
  if left > right {
    left
  } else {
    right
  }
}

/// Bytes [from, from + size) of the concatenation of `pieces`.
fn window_v1(pieces: List<ByteArray>, from: Int, size: Int) -> ByteArray {
  expect [piece, ..rest] = pieces
  let length = builtin.length_of_bytearray(piece)
  if from >= length {
    window_v1(rest, from - length, size)
  } else if from + size <= length {
    builtin.slice_bytearray(from, size, piece)
  } else {
    builtin.append_bytearray(
      builtin.slice_bytearray(from, length - from, piece),
      window_v1(rest, 0, size - length + from),
    )
  }
}

/// The bytes a source reveals: inline, or its staging outputs' chunks.
fn source_pieces_v1(
  inputs: ArmInputsV1,
  source: OperandSourceV1,
) -> List<ByteArray> {
  when source is {
    InlineOperand { bytes } -> [bytes]
    StagedOperand { operand } -> {
      expect Some(indices) = list.at(inputs.operands, operand)
      operand_chunks_v1(inputs.runs, indices)
    }
    _ -> fail
  }
}

/// 256 squared `times` times.
fn squared_v1(value: Int, times: Int) -> Int {
  if times == 0 {
    value
  } else {
    squared_v1(value * value, times - 1)
  }
}

/// A magnitude's limbs plus one.
fn plus_one_v1(base: Int, limbs: List<Int>) -> List<Int> {
  when limbs is {
    [] -> [1]
    [limb, ..rest] ->
      if limb + 1 == base {
        [0, ..plus_one_v1(base, rest)]
      } else {
        [limb + 1, ..rest]
      }
  }
}

/// An integer from `source`: bound to `value`'s leaf on the first hop when
/// `value` is given, and read again from the same bytes otherwise.
fn integer_v1(
  base: Int,
  inputs: ArmInputsV1,
  value: Option<ValueWitnessV1>,
  source: OperandSourceV1,
  first: Bool,
  bounded: Bool,
) -> SignedLimbsV1 {
  let integer = when value is {
    Some(value) ->
      if first {
        let Pair(_, integer) = bind_integer_v1(
          inputs,
          Pair(value, source),
          bounded,
        )
        expect Some(integer) = integer
        integer
      } else {
        read_integer_v1(inputs, source)
      }
    None -> read_integer_v1(inputs, source)
  }
  let limbs = to_limbs_v1(integer.argument)
  SignedLimbsV1 {
    negative: integer.negative,
    magnitude: if integer.negative {
      plus_one_v1(base, limbs)
    } else {
      limbs
    },
  }
}

fn read_integer_v1(
  inputs: ArmInputsV1,
  source: OperandSourceV1,
) -> CanonicalIntegerV1 {
  parse_integer_cbor_bulk_v1(
    concat_balanced_v1(source_pieces_v1(inputs, source)),
  )
}

/// The claimed result, from `source`. On the first hop its bytes are bound to
/// the result's leaf; on every hop the fixed bytes of its CBOR (the opening,
/// the last segment's head, the break and the argument's first byte) are
/// checked, and each hop checks the heads of the segments it reads.
fn claimed_v1(
  inputs: ArmInputsV1,
  source: OperandSourceV1,
  first: Bool,
) -> ClaimedV1 {
  let pieces = source_pieces_v1(inputs, source)
  expect SemanticConstantValue { type_cbor, payload, memory } = inputs.result
  let size = payload.cbor_length
  expect or {
      !first,
      and {
        type_cbor == integer_type_cbor,
        payload == when source is {
          InlineOperand { bytes } ->
            integer_leaf_from_cbor_v1(
              flat_root_v1(bytes),
              builtin.length_of_bytearray(bytes),
              memory,
            )
          _ -> {
            expect StagedOperand { operand } = source
            expect Some(indices) = list.at(inputs.operands, operand)
            let staged = bind_operand_v1(inputs.runs, indices)
            integer_leaf_from_cbor_v1(staged.root, staged.length, memory)
          }
        },
      },
    }
  if size < 71 {
    let integer = parse_integer_cbor_bulk_v1(concat_balanced_v1(pieces))
    ClaimedV1 {
      negative: integer.negative,
      length: builtin.length_of_bytearray(integer.argument),
      pieces: [integer.argument],
      full: -1,
      tail_at: 0,
      words: memory,
    }
  } else {
    let opening = window_v1(pieces, 0, 2)
    let between = size - 3
    let full = ( between - 2 ) / 66
    let last_at = 2 + 66 * full
    let last = between - 66 * full
    let head = window_v1(pieces, last_at, 2)
    let Pair(tail, tail_at) = if last <= 24 {
      expect builtin.index_bytearray(head, 0) == 0x3f + last
      Pair(last - 1, last_at + 1)
    } else {
      expect builtin.index_bytearray(head, 0) == 0x58
      expect builtin.index_bytearray(head, 1) == last - 2
      expect last >= 26
      expect last <= 66
      Pair(last - 2, last_at + 2)
    }
    expect window_v1(pieces, size - 1, 1) == #"ff"
    expect builtin.index_bytearray(window_v1(pieces, 4, 1), 0) > 0
    ClaimedV1 {
      negative: if opening == #"c35f" {
        True
      } else {
        expect opening == #"c25f"
        False
      },
      length: 64 * full + tail,
      pieces,
      full,
      tail_at,
      words: memory,
    }
  }
}

/// Argument bytes [from, to) of the claimed result.
fn argument_bytes_v1(claimed: ClaimedV1, from: Int, to: Int) -> ByteArray {
  if claimed.full < 0 {
    expect [argument] = claimed.pieces
    builtin.slice_bytearray(from, to - from, argument)
  } else {
    let split = 64 * claimed.full
    let front = if from < split {
      let first = from / 64
      let segments = ( min_v1(to, split) + 63 ) / 64 - first
      let cbor = window_v1(claimed.pieces, 66 * first, 66 * segments + 2)
      expect full_heads_hold(cbor, segments)
      builtin.slice_bytearray(
        from - 64 * first,
        min_v1(to, split) - from,
        concat_balanced_v1(payloads(cbor, 0, segments, [])),
      )
    } else {
      #""
    }
    if to > split {
      let start = max_v1(from, split)
      builtin.append_bytearray(
        front,
        window_v1(claimed.pieces, claimed.tail_at + start - split, to - start),
      )
    } else {
      front
    }
  }
}

/// The claimed result's CBOR argument limbs `first` to `end - 1`, least
/// significant first; the limbs past the argument are left out.
fn claimed_limbs_v1(claimed: ClaimedV1, first: Int, end: Int) -> List<Int> {
  let to = claimed.length - limb_bytes * first
  if to <= 0 {
    []
  } else {
    to_limbs_v1(
      argument_bytes_v1(
        claimed,
        max_v1(0, claimed.length - limb_bytes * end),
        to,
      ),
    )
  }
}

fn split_limb(limbs: List<Int>) -> Pair<Int, List<Int>> {
  when limbs is {
    [] -> Pair(0, [])
    [limb, ..rest] -> Pair(limb, rest)
  }
}

/// The sum of x[i] * y[k - i] over the first `count` of `x` and of
/// `y_reversed`.
fn dot_v1(x: List<Int>, y_reversed: List<Int>, count: Int, total: Int) -> Int {
  if count == 0 {
    total
  } else {
    expect [left, ..x_rest] = x
    expect [right, ..y_rest] = y_reversed
    dot_v1(x_rest, y_rest, count - 1, total + left * right)
  }
}

/// Column k of x * y.
fn column_v1(hop: HopV1, k: Int) -> Int {
  let low = max_v1(0, k - hop.y_count + 1)
  let high = min_v1(k, hop.x_count - 1)
  if low > high {
    0
  } else {
    dot_v1(
      list.drop(hop.x, low),
      list.drop(hop.y_reversed, hop.y_count - 1 - k + low),
      high - low + 1,
      0,
    )
  }
}

/// Cardano's memory words of an integer whose CBOR argument has `length`
/// bytes, for a negative one whose argument is all 0xff (`all_ones`) as well.
fn words_v1(length: Int, negative: Bool, all_ones: Bool) -> Int {
  if and {
    negative,
    length % 8 == 0,
    all_ones,
  } {
    length / 8 + 1
  } else if length == 0 {
    1
  } else {
    ( length + 7 ) / 8
  }
}

/// Columns k to end - 1, from `state`. `claimed` holds the claimed result's
/// argument limbs from column k on.
fn columns_v1(
  hop: HopV1,
  k: Int,
  end: Int,
  state: ProductStateV1,
  dividend: List<Int>,
  divisor: List<Int>,
  claimed: List<Int>,
) -> ProductStateV1 {
  if k == end {
    state
  } else {
    let value = column_v1(hop, k) + state.carry
    let carry = value / hop.base
    let low = value - carry * hop.base
    let Pair(a, dividend_rest) = split_limb(dividend)
    let Pair(b, divisor_rest) = split_limb(divisor)
    let Pair(limb, Pair(borrow, order)) = if hop.tag == 2 {
      Pair(low, Pair(0, 0))
    } else {
      let difference = if hop.reversed {
        low - a - state.borrow
      } else {
        a - low - state.borrow
      }
      let borrow = if difference < 0 {
        1
      } else {
        0
      }
      let remainder = difference + borrow * hop.base
      Pair(
        remainder,
        Pair(
          borrow,
          if remainder > b {
            1
          } else if remainder < b {
            -1
          } else {
            state.order
          },
        ),
      )
    }
    let Pair(claimed_limb, claimed_rest) = split_limb(claimed)
    let increment = when hop.claimed is {
      None -> 0
      Some(result) -> {
        let spelled = claimed_limb + state.increment
        let increment = if spelled == hop.base {
          1
        } else {
          0
        }
        expect limb == spelled - increment * hop.base
        let top = ( result.length + limb_bytes - 1 ) / limb_bytes - 1
        expect or {
            k != top,
            result.words == words_v1(
              result.length,
              result.negative,
              spelled == builtin.bytearray_to_integer(
                True,
                builtin.cons_bytearray(
                  1,
                  builtin.replicate_byte(result.length - limb_bytes * top, 0),
                ),
              ),
            ),
          }
        increment
      }
    }
    columns_v1(
      hop,
      k + 1,
      end,
      ProductStateV1 { carry, borrow, order, increment },
      dividend_rest,
      divisor_rest,
      claimed_rest,
    )
  }
}

/// One hop of the loop: columns [cursor, end) after `from` (the cursor and
/// state the previous hop left, `None` on the first hop). Returns the state
/// to carry on with, or `None` when the hop ends the loop and every final
/// check holds.
pub fn advance_product_v1(
  arm: Int,
  inner: Data,
  witness: Data,
  reference_inputs: List<Input>,
  from: Option<Pair<Int, Data>>,
  end: Int,
) -> Option<Data> {
  let raw = chain.decode_builtin_success(arm, witness)
  expect facts: chain.SemanticRoots = inner
  expect material: StagedMaterialV1 = raw.material
  let tag = raw.tag
  let first = from == None
  // 2^4096, computed here rather than written as a constant: the compiler
  // writes a constant's 513 bytes into the script at every use.
  let base = squared_v1(256, 9)
  expect [a_value, b_value] = facts.arguments
  expect [a_source, b_source, ..more] = material.sources
  let inputs = ArmInputsV1 {
    tag,
    leading: [],
    arguments: [],
    result: facts.result,
    runs: collect_runs_v1(reference_inputs, material.runs.refs),
    operands: material.runs.operands,
  }
  let a = integer_v1(base, inputs, Some(a_value), a_source, first, True)
  let b = integer_v1(base, inputs, Some(b_value), b_source, first, True)
  let a_count = list.length(a.magnitude)
  let b_count = list.length(b.magnitude)
  let signs_differ = a.negative != b.negative
  // The sign a non-zero claimed result has.
  let Pair(x, Pair(claimed_source, Pair(reversed, sign))) = if tag == 2 {
    expect [result_source] = more
    Pair(a, Pair(Some(result_source), Pair(False, signs_differ)))
  } else {
    expect [q_source, ..rest] = more
    // quotientInteger and divideInteger return q, so q is bound to the
    // result's leaf; the others return the remainder.
    let quotient = tag == 3 || tag == 4
    let q = integer_v1(
      base,
      inputs,
      if quotient {
        Some(facts.result)
      } else {
        None
      },
      q_source,
      first,
      False,
    )
    expect q.magnitude == [] || q.negative == signs_differ
    if quotient {
      expect [] = rest
      Pair(q, Pair(None, Pair(tag == 3 && signs_differ, False)))
    } else {
      expect [result_source] = rest
      if tag == 5 {
        Pair(q, Pair(Some(result_source), Pair(False, a.negative)))
      } else {
        expect tag == 6
        Pair(q, Pair(Some(result_source), Pair(signs_differ, b.negative)))
      }
    }
  }
  let x_count = list.length(x.magnitude)
  let count = max_v1(
    1,
    if tag == 2 {
      a_count + b_count
    } else {
      max_v1(a_count, x_count + b_count)
    },
  )
  let claimed = when claimed_source is {
    None -> None
    Some(source) -> {
      let claimed = claimed_v1(inputs, source, first)
      expect claimed.length <= limb_bytes * count
      expect or {
          claimed.length > 0,
          claimed.words == 1,
        }
      // Zero is the one result spelled with an empty non-negative argument.
      expect or {
          claimed.negative == sign,
          and {
            !claimed.negative,
            claimed.length == 0,
          },
        }
      Some(claimed)
    }
  }
  let Pair(cursor, state) = when from is {
    None ->
      Pair(
        0,
        ProductStateV1 {
          carry: 0,
          borrow: 0,
          order: 0,
          increment: when claimed is {
            Some(ClaimedV1 { negative: True, .. }) -> 1
            _ -> 0
          },
        },
      )
    Some(Pair(cursor, state_data)) -> {
      expect state: ProductStateV1 = state_data
      Pair(cursor, state)
    }
  }
  expect cursor < end
  expect end <= count
  let after = columns_v1(
    HopV1 {
      base,
      tag,
      x: x.magnitude,
      x_count,
      y_reversed: list.reverse(b.magnitude),
      y_count: b_count,
      claimed,
      reversed,
    },
    cursor,
    end,
    state,
    list.drop(a.magnitude, cursor),
    list.drop(b.magnitude, cursor),
    when claimed is {
      None -> []
      Some(result) -> claimed_limbs_v1(result, cursor, end)
    },
  )
  if end == count {
    // x_count + y_count <= count bounds x * y below base^count, so the last
    // column carries nothing out.
    expect after.increment == 0
    expect or {
        tag == 2,
        and {
          after.borrow == 0,
          after.order == -1,
        },
      }
    None
  } else {
    let data: Data = after
    Some(data)
  }
}
