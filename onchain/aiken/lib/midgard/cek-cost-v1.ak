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

fn division_cpu(sizes: List<Int>) -> Int {
  let Pair(x, y) = two(sizes)
  if x < y {
    85848
  } else {
    let real = 123203 + 1716 * x + 7305 * y + 57 * x * x + 549 * x * y - 900 * y
    max(85848, real)
  }
}

pub fn builtin_argument_count_v1(tag: Int) -> Int {
  expect and {
      tag >= 0,
      tag <= 86,
    }
  if tag == 36 {
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
    tag == 58,
    tag == 61,
    tag == 63,
    tag == 64,
    tag == 65,
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
    linear2_add(90434, 519, sizes)
  } else if and {
    tag >= 3,
    tag <= 6,
  } {
    division_cpu(sizes)
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
    linear_on_equal(29498, 38, 24548, sizes)
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
    linear3_z(53384111, 14333, sizes)
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
  } else if tag == 58 {
    linear2_x(52538055, 3756, sizes)
  } else if tag == 59 {
    expect [_] = sizes
    2780678
  } else if tag == 60 {
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
  } else if tag == 65 {
    linear2_x(166917843, 4307, sizes)
  } else if tag == 66 {
    expect [_] = sizes
    3227919
  } else if tag == 67 {
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
  } else if tag == 58 {
    expect [_, _] = sizes
    18
  } else if tag == 59 {
    expect [_] = sizes
    6
  } else if tag == 60 {
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
  } else if tag == 65 {
    expect [_, _] = sizes
    36
  } else if tag == 66 {
    expect [_] = sizes
    12
  } else if tag == 67 {
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

pub fn builtin_budget_v1(
  tag: Int,
  cost_argument_sizes: List<Int>,
) -> BuiltinBudgetV1 {
  expect and {
      tag >= 0,
      tag <= 86,
      list.length(cost_argument_sizes) == builtin_argument_count_v1(tag),
      list.all(cost_argument_sizes, fn(size) { size >= 0 }),
    }
  BuiltinBudgetV1 {
    cpu: cpu_cost_v1(tag, cost_argument_sizes),
    memory: memory_cost_v1(tag, cost_argument_sizes),
  }
}
