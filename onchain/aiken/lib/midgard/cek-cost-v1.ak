use aiken/builtin
use aiken/collection/list

/// Exact Plutus V3 builtin budget under the canonical V1 pinned cost model.
pub type BuiltinBudgetV1 {
  cpu: Int,
  memory: Int,
}

fn max(left: Int, right: Int) -> Int {
  if left > right {
    left
  } else {
    right
  }
}

fn min(left: Int, right: Int) -> Int {
  if left < right {
    left
  } else {
    right
  }
}

fn one(sizes: List<Int>) -> Int {
  expect [x] = sizes
  x
}

fn two(sizes: List<Int>) -> Pair<Int, Int> {
  expect [x, y] = sizes
  Pair(x, y)
}

fn three(sizes: List<Int>) -> Pair<Int, Pair<Int, Int>> {
  expect [x, y, z] = sizes
  Pair(x, Pair(y, z))
}

fn linear1(quote: Int, slope: Int, x: Int) -> Int {
  quote + x * slope
}

fn linear2_x(quote: Int, slope: Int, sizes: List<Int>) -> Int {
  let Pair(x, _) = two(sizes)
  quote + x * slope
}

fn linear2_y(quote: Int, slope: Int, sizes: List<Int>) -> Int {
  let Pair(_, y) = two(sizes)
  quote + y * slope
}

fn linear2_add(quote: Int, slope: Int, sizes: List<Int>) -> Int {
  let Pair(x, y) = two(sizes)
  quote + ( x + y ) * slope
}

fn linear2_mul(quote: Int, slope: Int, sizes: List<Int>) -> Int {
  let Pair(x, y) = two(sizes)
  quote + x * y * slope
}

fn linear2_sub(quote: Int, slope: Int, minimum: Int, sizes: List<Int>) -> Int {
  let Pair(x, y) = two(sizes)
  quote + max(minimum, x - y) * slope
}

fn linear2_min(quote: Int, slope: Int, sizes: List<Int>) -> Int {
  let Pair(x, y) = two(sizes)
  quote + min(x, y) * slope
}

fn linear2_max(quote: Int, slope: Int, sizes: List<Int>) -> Int {
  let Pair(x, y) = two(sizes)
  quote + max(x, y) * slope
}

fn linear_on_equal(
  quote: Int,
  slope: Int,
  constant: Int,
  sizes: List<Int>,
) -> Int {
  let Pair(x, y) = two(sizes)
  if x == y {
    quote + x * slope
  } else {
    constant
  }
}

fn linear3_x(quote: Int, slope: Int, sizes: List<Int>) -> Int {
  let Pair(x, _) = three(sizes)
  quote + x * slope
}

fn linear3_y(quote: Int, slope: Int, sizes: List<Int>) -> Int {
  let Pair(_, Pair(y, _)) = three(sizes)
  quote + y * slope
}

fn linear3_z(quote: Int, slope: Int, sizes: List<Int>) -> Int {
  let Pair(_, Pair(_, z)) = three(sizes)
  quote + z * slope
}

fn linear3_y_z(quote: Int, y_slope: Int, z_slope: Int, sizes: List<Int>) -> Int {
  let Pair(_, Pair(y, z)) = three(sizes)
  quote + y * y_slope + z * z_slope
}

fn linear3_max_y_z(quote: Int, slope: Int, sizes: List<Int>) -> Int {
  let Pair(_, Pair(y, z)) = three(sizes)
  quote + max(y, z) * slope
}

fn quadratic2_y(x0: Int, x1: Int, x2: Int, sizes: List<Int>) -> Int {
  let Pair(_, y) = two(sizes)
  x0 + x1 * y + x2 * y * y
}

fn quadratic3_z(x0: Int, x1: Int, x2: Int, sizes: List<Int>) -> Int {
  let Pair(_, Pair(_, z)) = three(sizes)
  x0 + x1 * z + x2 * z * z
}

fn const_y_or_linear_z(quote: Int, slope: Int, sizes: List<Int>) -> Int {
  let Pair(_, Pair(y, z)) = three(sizes)
  if y == 0 {
    quote + slope * z
  } else {
    y
  }
}

/// Plutus V3 variant E: divideInteger and modInteger cost the quadratic over
/// (larger, smaller) sizes on both sides of the diagonal; quotientInteger and
/// remainderInteger stay constant below it.
fn division_cpu(tag: Int, sizes: List<Int>) -> Int {
  let Pair(x, y) = two(sizes)
  if x < y && ( tag == 4 || tag == 5 ) {
    85848
  } else {
    let Pair(a, b) = if x < y {
      Pair(y, x)
    } else {
      Pair(x, y)
    }
    max(
      85848,
      123203 + 1716 * a + 7305 * b + 57 * a * a + 960 * a * b - 900 * b * b,
    )
  }
}

/// The budget of division builtin `tag` (3 to 6) over its two argument
/// sizes: `builtin_budget_v1`'s row for it, without the other builtins.
pub fn division_budget_v1(tag: Int, sizes: List<Int>) -> BuiltinBudgetV1 {
  expect tag >= 3 && tag <= 6
  expect list.all(sizes, fn(size) { size >= 0 })
  BuiltinBudgetV1 {
    cpu: division_cpu(tag, sizes),
    memory: if tag <= 4 {
      linear2_sub(0, 1, 1, sizes)
    } else {
      linear2_y(0, 1, sizes)
    },
  }
}

/// The budget of verifyEd25519Signature (21),
/// verifyEcdsaSecp256k1Signature (52) or verifySchnorrSecp256k1Signature (53)
/// over its three argument sizes: `builtin_budget_v1`'s row for it, without
/// the other builtins.
pub fn signature_budget_v1(tag: Int, sizes: List<Int>) -> BuiltinBudgetV1 {
  expect list.all(sizes, fn(size) { size >= 0 })
  expect [_, _, _] = sizes
  BuiltinBudgetV1 {
    cpu: if tag == 21 {
      linear3_y(53384111, 14333, sizes)
    } else if tag == 52 {
      43053543
    } else {
      expect tag == 53
      linear3_y(43574283, 26308, sizes)
    },
    memory: 10,
  }
}

/// The budget of integerToByteString (73) over its three argument sizes
/// (the endianness, the width's words and the input's memory):
/// `builtin_budget_v1`'s row for it, without the other builtins.
pub fn integer_to_bytes_budget_v1(sizes: List<Int>) -> BuiltinBudgetV1 {
  expect [_, _, _] = sizes
  BuiltinBudgetV1 {
    cpu: quadratic3_z(1293828, 28716, 63, sizes),
    memory: const_y_or_linear_z(0, 1, sizes),
  }
}

/// The budget of writeBits (80) over its three argument sizes (the input's
/// memory, the index count and the value's memory): `builtin_budget_v1`'s row
/// for it, without the other builtins.
pub fn write_bits_budget_v1(sizes: List<Int>) -> BuiltinBudgetV1 {
  BuiltinBudgetV1 {
    cpu: linear3_y(281145, 18848, sizes),
    memory: linear3_x(0, 1, sizes),
  }
}

/// The argument count of protocol version 11 builtin `tag` (87 to 100).
fn pv11_argument_count_v1(tag: Int) -> Int {
  expect tag <= 100
  if tag == 94 {
    4
  } else if tag == 87 || tag == 95 {
    3
  } else if or {
    tag == 89,
    tag == 90,
    tag == 98,
    tag == 99,
  } {
    1
  } else {
    2
  }
}

pub fn builtin_argument_count_v1(tag: Int) -> Int {
  expect tag >= 0
  if tag > 86 {
    pv11_argument_count_v1(tag)
  } else if tag == 36 {
    6
  } else if or {
    tag == 12,
    tag == 21,
    tag == 26,
    tag == 31,
    tag == 52,
    tag == 53,
    tag == 73,
    tag == 75,
    tag == 76,
    tag == 77,
    tag == 80,
  } {
    3
  } else if or {
    tag <= 11,
    tag == 14,
    tag == 15,
    tag == 16,
    tag == 17,
    tag == 22,
    tag == 23,
    tag == 27,
    tag == 28,
    tag == 32,
    tag == 37,
    tag == 47,
    tag == 48,
    tag == 54,
    tag == 56,
    tag == 57,
    tag == 60,
    tag == 61,
    tag == 63,
    tag == 64,
    tag == 67,
    tag == 68,
    tag == 69,
    tag == 70,
    tag == 74,
    tag == 79,
    tag == 81,
    tag == 82,
    tag == 83,
  } {
    2
  } else {
    1
  }
}

fn cpu_cost_v1(tag: Int, sizes: List<Int>) -> Int {
  if or {
    tag == 0,
    tag == 1,
  } {
    linear2_max(100788, 420, sizes)
  } else if tag == 2 {
    linear2_mul(90434, 519, sizes)
  } else if and {
    tag >= 3,
    tag <= 6,
  } {
    division_cpu(tag, sizes)
  } else if tag == 7 {
    linear2_min(51775, 558, sizes)
  } else if tag == 8 {
    linear2_min(44749, 541, sizes)
  } else if tag == 9 {
    linear2_min(43285, 552, sizes)
  } else if tag == 10 {
    linear2_add(1000, 173, sizes)
  } else if tag == 11 {
    linear2_y(72010, 178, sizes)
  } else if tag == 12 {
    linear3_z(20467, 1, sizes)
  } else if tag == 13 {
    expect [_] = sizes
    22100
  } else if tag == 14 {
    expect [_, _] = sizes
    13169
  } else if tag == 15 {
    linear_on_equal(28755, 75, 30623, sizes)
  } else if or {
    tag == 16,
    tag == 17,
  } {
    linear2_min(28999, 74, sizes)
  } else if tag == 18 {
    linear1(270652, 22588, one(sizes))
  } else if tag == 19 {
    linear1(1457325, 64566, one(sizes))
  } else if tag == 20 {
    linear1(201305, 8356, one(sizes))
  } else if tag == 21 {
    linear3_y(53384111, 14333, sizes)
  } else if tag == 22 {
    linear2_add(1000, 59957, sizes)
  } else if tag == 23 {
    linear_on_equal(1000, 60594, 39184, sizes)
  } else if tag == 24 {
    linear1(1000, 42921, one(sizes))
  } else if tag == 25 {
    linear1(91189, 769, one(sizes))
  } else if tag == 26 {
    expect [_, _, _] = sizes
    76049
  } else if tag == 27 {
    expect [_, _] = sizes
    61462
  } else if tag == 28 {
    expect [_, _] = sizes
    59498
  } else if tag == 29 {
    expect [_] = sizes
    141895
  } else if tag == 30 {
    expect [_] = sizes
    141992
  } else if tag == 31 {
    expect [_, _, _] = sizes
    132994
  } else if tag == 32 {
    expect [_, _] = sizes
    72362
  } else if tag == 33 {
    expect [_] = sizes
    83150
  } else if tag == 34 {
    expect [_] = sizes
    81663
  } else if tag == 35 {
    expect [_] = sizes
    74433
  } else if tag == 36 {
    expect [_, _, _, _, _, _] = sizes
    94375
  } else if tag == 37 {
    expect [_, _] = sizes
    22151
  } else if tag == 38 {
    expect [_] = sizes
    68246
  } else if tag == 39 {
    expect [_] = sizes
    33852
  } else if tag == 40 {
    expect [_] = sizes
    15299
  } else if tag == 41 {
    expect [_] = sizes
    11183
  } else if tag == 42 {
    expect [_] = sizes
    24588
  } else if tag == 43 {
    expect [_] = sizes
    24623
  } else if tag == 44 {
    expect [_] = sizes
    25933
  } else if tag == 45 {
    expect [_] = sizes
    20744
  } else if tag == 46 {
    expect [_] = sizes
    20142
  } else if tag == 47 {
    linear2_min(898148, 27279, sizes)
  } else if tag == 48 {
    expect [_, _] = sizes
    11546
  } else if tag == 49 {
    expect [_] = sizes
    7243
  } else if tag == 50 {
    expect [_] = sizes
    7391
  } else if tag == 51 {
    linear1(955506, 213312, one(sizes))
  } else if tag == 52 {
    expect [_, _, _] = sizes
    43053543
  } else if tag == 53 {
    linear3_y(43574283, 26308, sizes)
  } else if tag == 54 {
    expect [_, _] = sizes
    962335
  } else if tag == 55 {
    expect [_] = sizes
    267929
  } else if tag == 56 {
    linear2_x(76433006, 8868, sizes)
  } else if tag == 57 {
    expect [_, _] = sizes
    442008
  } else if tag == 60 {
    linear2_x(52538055, 3756, sizes)
  } else if tag == 58 {
    expect [_] = sizes
    2780678
  } else if tag == 59 {
    expect [_] = sizes
    52948122
  } else if tag == 61 {
    expect [_, _] = sizes
    1995836
  } else if tag == 62 {
    expect [_] = sizes
    284546
  } else if tag == 63 {
    linear2_x(158221314, 26549, sizes)
  } else if tag == 64 {
    expect [_, _] = sizes
    901022
  } else if tag == 67 {
    linear2_x(166917843, 4307, sizes)
  } else if tag == 65 {
    expect [_] = sizes
    3227919
  } else if tag == 66 {
    expect [_] = sizes
    74698472
  } else if tag == 68 {
    expect [_, _] = sizes
    254006273
  } else if tag == 69 {
    expect [_, _] = sizes
    2174038
  } else if tag == 70 {
    expect [_, _] = sizes
    333849714
  } else if tag == 71 {
    linear1(2261318, 64571, one(sizes))
  } else if tag == 72 {
    linear1(207616, 8310, one(sizes))
  } else if tag == 73 {
    quadratic3_z(1293828, 28716, 63, sizes)
  } else if tag == 74 {
    quadratic2_y(1006041, 43623, 251, sizes)
  } else if and {
    tag >= 75,
    tag <= 77,
  } {
    linear3_y_z(100181, 726, 719, sizes)
  } else if tag == 78 {
    linear1(107878, 680, one(sizes))
  } else if tag == 79 {
    expect [_, _] = sizes
    95336
  } else if tag == 80 {
    linear3_y(281145, 18848, sizes)
  } else if tag == 81 {
    linear2_x(180194, 159, sizes)
  } else if tag == 82 {
    linear2_x(158519, 8942, sizes)
  } else if tag == 83 {
    linear2_x(159378, 8813, sizes)
  } else if tag == 84 {
    linear1(107490, 3298, one(sizes))
  } else if tag == 85 {
    linear1(106057, 655, one(sizes))
  } else {
    expect tag == 86
    linear1(1964219, 24520, one(sizes))
  }
}

fn memory_cost_v1(tag: Int, sizes: List<Int>) -> Int {
  if or {
    tag == 0,
    tag == 1,
  } {
    linear2_max(1, 1, sizes)
  } else if tag == 2 {
    linear2_add(0, 1, sizes)
  } else if or {
    tag == 3,
    tag == 4,
  } {
    linear2_sub(0, 1, 1, sizes)
  } else if or {
    tag == 5,
    tag == 6,
  } {
    linear2_y(0, 1, sizes)
  } else if and {
    tag >= 7,
    tag <= 9,
  } {
    expect [_, _] = sizes
    1
  } else if or {
    tag == 10,
    tag == 11,
  } {
    linear2_add(0, 1, sizes)
  } else if tag == 12 {
    linear3_z(4, 0, sizes)
  } else if tag == 13 {
    expect [_] = sizes
    10
  } else if tag == 14 {
    expect [_, _] = sizes
    4
  } else if and {
    tag >= 15,
    tag <= 17,
  } {
    expect [_, _] = sizes
    1
  } else if and {
    tag >= 18,
    tag <= 20,
  } {
    expect [_] = sizes
    4
  } else if tag == 21 {
    expect [_, _, _] = sizes
    10
  } else if tag == 22 {
    linear2_add(4, 1, sizes)
  } else if tag == 23 {
    expect [_, _] = sizes
    1
  } else if or {
    tag == 24,
    tag == 25,
  } {
    linear1(4, 2, one(sizes))
  } else if tag == 26 {
    expect [_, _, _] = sizes
    1
  } else if tag == 27 {
    expect [_, _] = sizes
    4
  } else if and {
    tag >= 28,
    tag <= 46,
  } {
    expect list.length(sizes) == builtin_argument_count_v1(tag)
    32
  } else if tag == 47 {
    expect [_, _] = sizes
    1
  } else if and {
    tag >= 48,
    tag <= 50,
  } {
    expect list.length(sizes) == builtin_argument_count_v1(tag)
    32
  } else if tag == 51 {
    linear1(0, 2, one(sizes))
  } else if or {
    tag == 52,
    tag == 53,
  } {
    expect [_, _, _] = sizes
    10
  } else if and {
    tag >= 54,
    tag <= 56,
  } {
    expect list.length(sizes) == builtin_argument_count_v1(tag)
    18
  } else if tag == 57 {
    expect [_, _] = sizes
    1
  } else if tag == 60 {
    expect [_, _] = sizes
    18
  } else if tag == 58 {
    expect [_] = sizes
    6
  } else if tag == 59 {
    expect [_] = sizes
    18
  } else if and {
    tag >= 61,
    tag <= 63,
  } {
    expect list.length(sizes) == builtin_argument_count_v1(tag)
    36
  } else if tag == 64 {
    expect [_, _] = sizes
    1
  } else if tag == 67 {
    expect [_, _] = sizes
    36
  } else if tag == 65 {
    expect [_] = sizes
    12
  } else if tag == 66 {
    expect [_] = sizes
    36
  } else if or {
    tag == 68,
    tag == 69,
  } {
    expect [_, _] = sizes
    72
  } else if tag == 70 {
    expect [_, _] = sizes
    1
  } else if or {
    tag == 71,
    tag == 72,
  } {
    expect [_] = sizes
    4
  } else if tag == 73 {
    const_y_or_linear_z(0, 1, sizes)
  } else if tag == 74 {
    linear2_y(0, 1, sizes)
  } else if and {
    tag >= 75,
    tag <= 77,
  } {
    linear3_max_y_z(0, 1, sizes)
  } else if tag == 78 {
    linear1(0, 1, one(sizes))
  } else if tag == 79 {
    expect [_, _] = sizes
    1
  } else if tag == 80 {
    linear3_x(0, 1, sizes)
  } else if tag == 81 {
    linear2_x(1, 1, sizes)
  } else if or {
    tag == 82,
    tag == 83,
  } {
    linear2_x(0, 1, sizes)
  } else if or {
    tag == 84,
    tag == 85,
  } {
    expect [_] = sizes
    1
  } else {
    expect tag == 86
    expect [_] = sizes
    3
  }
}

/// Cardano's `CostingInteger` saturates at the largest signed 64-bit integer.
fn saturated(cost: Int) -> Int {
  min(cost, 9223372036854775807)
}

/// The cost rows of the protocol version 11 builtins 88 to 100 other than
/// unionValue (96) and valueContains (97), twelve big-endian bytes per tag
/// from 88: the index of the one size Cardano charges (1 byte), cpu quote (4),
/// cpu slope (4), cpu square (1), memory quote (1) and memory slope (1), so
/// cpu = quote + slope * s + square * s * s and memory = quote + slope * s.
///
///  88 dropList: 0, 116711, 1957, 0, 4, 0
///  89 lengthOfArray: 0, 231883, 0, 0, 10, 0
///  90 listToArray: 0, 1000, 24838, 0, 7, 1
///  91 indexArray: 0, 232010, 0, 0, 32, 0
///  92 bls12_381_G1_multiScalarMul: 0, 321837444, 25087669, 0, 18, 0
///  93 bls12_381_G2_multiScalarMul: 0, 617887431, 67302824, 0, 36, 0
///  94 insertCoin: 3, 356924, 18413, 0, 45, 21
///  95 lookupCoin: 2, 219951, 9444, 0, 1, 0
///  96 (unused): zeros
///  97 (unused): zeros
///  98 valueData: 0, 1000, 38159, 0, 2, 22
///  99 unValueData: 0, 1000, 95933, 1, 1, 11
/// 100 scaleValue: 1, 1000, 277577, 0, 12, 21
const pv11_cost_rows_v1: ByteArray =
  #"000001c7e7000007a500040000000389cb00000000000a0000000003e8000061060007010000038a4a0000000000200000132ed984017eceb50012000024d436c70402f5a8002400030005723c000047ed002d150200035b2f000024e400010000000000000000000000000000000000000000000000000000000003e80000950f00021600000003e8000176bd01010b01000003e800043c49000c15"

/// The budget of protocol version 11 builtin `tag` (87 to 100). Each size is
/// as Cardano measures it for that builtin (`builtin_cost_sizes_v1`); a size
/// no cost function of the builtin reads is 0.
fn pv11_budget_v1(tag: Int, sizes: List<Int>) -> BuiltinBudgetV1 {
  let Pair(cpu, memory) = if tag == 87 {
    // expModInteger over (base, exponent, modulus) words: half as much
    // again when the base is larger than the modulus.
    let Pair(x, Pair(y, z)) = three(sizes)
    let cost = 607153 + 231697 * y * z + 53144 * y * z * z
    Pair(
      if x > z {
        cost + cost / 2
      } else {
        cost
      },
      z,
    )
  } else if tag == 96 {
    let Pair(x, y) = two(sizes)
    Pair(1000 + 172116 * x + 183150 * y + 6 * x * y, linear2_add(24, 21, sizes))
  } else if tag == 97 {
    let Pair(x, y) = two(sizes)
    Pair(
      if x < y {
        213283
      } else {
        618401 + 1998 * x + 28258 * y
      },
      1,
    )
  } else {
    let row = builtin.slice_bytearray(12 * ( tag - 88 ), 12, pv11_cost_rows_v1)
    let field = fn(start, width) {
      builtin.bytearray_to_integer(
        True,
        builtin.slice_bytearray(start, width, row),
      )
    }
    let size = builtin.head_list(list.drop(sizes, field(0, 1)))
    Pair(
      field(1, 4) + size * ( field(5, 4) + size * field(9, 1) ),
      field(10, 1) + size * field(11, 1),
    )
  }
  BuiltinBudgetV1 { cpu: saturated(cpu), memory: saturated(memory) }
}

pub fn builtin_budget_v1(
  tag: Int,
  cost_argument_sizes: List<Int>,
) -> BuiltinBudgetV1 {
  // builtin_argument_count_v1 rejects a negative tag.
  expect and {
      list.length(cost_argument_sizes) == builtin_argument_count_v1(tag),
      list.all(cost_argument_sizes, fn(size) { size >= 0 }),
    }
  if tag > 86 {
    pv11_budget_v1(tag, cost_argument_sizes)
  } else {
    BuiltinBudgetV1 {
      cpu: cpu_cost_v1(tag, cost_argument_sizes),
      memory: memory_cost_v1(tag, cost_argument_sizes),
    }
  }
}
