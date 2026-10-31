/**
 * The multiplyInteger and division rows of `cek-staged-arm-v1` (semantics
 * group 9): staged steps whose column loop completes, wrong products and
 * quotients, and arguments and results at the ends of the integer range.
 * The operands are 5,000 bytes of 0xff (2^40000 - 1) and integers built from
 * it, whose products and quotients are spelled by construction.
 */

import {
  BOTTOM,
  C,
  H,
  N,
  PAST_TOP,
  R,
  TOP,
  looped,
  small,
} from "./cek-staged-arm-v1.kit.mjs";

/** 2^40000 - 1, its square and the integers around them. */
const ONES = N(false, R(0xff, 5_000));
const MINUS_ONES = N(true, C(R(0xff, 4_999), H("fe")));
const ONES_LESS_ONE = N(false, C(R(0xff, 4_999), H("fe")));
const ONES_UP = N(false, C(H("01"), R(0, 5_000)));
const MINUS_ONES_UP = N(true, R(0xff, 5_000));
/** (2^40000 - 1)^2 + `low` - 1, for `low` up to 0xff. */
const square = (low) =>
  N(false, C(R(0xff, 4_999), H("fe"), R(0, 4_999), H(low)));
const SQUARE = square("01");
const MINUS_SQUARE = N(true, C(R(0xff, 4_999), H("fe"), R(0, 5_000)));
const SQUARE_AND_FIVE = square("06");
const MINUS_SQUARE_AND_FIVE = N(
  true,
  C(R(0xff, 4_999), H("fe"), R(0, 4_999), H("05")),
);
/** 2^40000 - 6 and -(2^40000 - 6). */
const ONES_LESS_FIVE = N(false, C(R(0xff, 4_999), H("fa")));
const MINUS_ONES_LESS_FIVE = N(true, C(R(0xff, 4_999), H("f9")));

export const PRODUCT_ROWS = [
  looped("multiplies_5000_byte_operands", true, 2, ONES, ONES, [SQUARE]),
  looped("multiplies_to_a_negative_product", true, 2, ONES, MINUS_ONES, [
    MINUS_SQUARE,
  ]),
  looped("multiplies_by_zero", true, 2, MINUS_ONES, small(0n), [small(0n)]),
  looped(
    "multiplies_the_largest_argument_past_the_range",
    true,
    2,
    TOP,
    small(2n),
    [N(false, C(R(0xff, 32_767), H("fe")))],
    true,
  ),
  looped("refuses_a_product_off_by_one", false, 2, ONES, ONES, [square("02")]),
  looped("refuses_a_product_of_the_wrong_sign", false, 2, ONES, MINUS_ONES, [
    SQUARE,
  ]),
  looped(
    "refuses_a_product_of_an_argument_one_past_the_largest",
    false,
    2,
    PAST_TOP,
    small(1n),
    [PAST_TOP],
    true,
  ),
  // Each division in every sign combination, with a remainder of 5.
  ...[
    [
      "positive_by_positive",
      SQUARE_AND_FIVE,
      ONES,
      ONES,
      ONES,
      small(5n),
      small(5n),
    ],
    [
      "negative_by_positive",
      MINUS_SQUARE_AND_FIVE,
      ONES,
      MINUS_ONES_UP,
      MINUS_ONES,
      small(-5n),
      ONES_LESS_FIVE,
    ],
    [
      "positive_by_negative",
      SQUARE_AND_FIVE,
      MINUS_ONES,
      MINUS_ONES_UP,
      MINUS_ONES,
      small(5n),
      MINUS_ONES_LESS_FIVE,
    ],
    [
      "negative_by_negative",
      MINUS_SQUARE_AND_FIVE,
      MINUS_ONES,
      ONES,
      ONES,
      small(-5n),
      small(-5n),
    ],
  ].flatMap(([signs, a, b, floored, truncated, remainder, modulus]) => [
    looped(`divides_${signs}`, true, 3, a, b, [floored]),
    looped(`quotients_${signs}`, true, 4, a, b, [truncated]),
    looped(`remainders_${signs}`, true, 5, a, b, [truncated, remainder]),
    looped(`mods_${signs}`, true, 6, a, b, [floored, modulus]),
  ]),
  looped(
    "divides_the_smallest_argument_by_minus_one_past_the_range",
    true,
    3,
    BOTTOM,
    small(-1n),
    [PAST_TOP],
    true,
  ),
  // A quotient one off, and a quotient rounded the other way.
  looped("refuses_a_quotient_one_above", false, 4, SQUARE_AND_FIVE, ONES, [
    ONES_UP,
  ]),
  looped(
    "refuses_a_divided_quotient_one_below",
    false,
    3,
    SQUARE_AND_FIVE,
    ONES,
    [ONES_LESS_ONE],
  ),
  looped(
    "refuses_a_remainder_from_a_quotient_one_above",
    false,
    5,
    SQUARE_AND_FIVE,
    ONES,
    [ONES_UP, small(5n)],
  ),
  looped(
    "refuses_a_modulus_from_a_quotient_one_below",
    false,
    6,
    SQUARE_AND_FIVE,
    ONES,
    [ONES_LESS_ONE, small(5n)],
  ),
  looped(
    "refuses_a_truncated_quotient_for_divide_integer",
    false,
    3,
    MINUS_SQUARE_AND_FIVE,
    ONES,
    [MINUS_ONES],
  ),
  looped(
    "refuses_a_floored_quotient_for_quotient_integer",
    false,
    4,
    MINUS_SQUARE_AND_FIVE,
    ONES,
    [MINUS_ONES_UP],
  ),
  looped(
    "refuses_a_remainder_for_mod_integer",
    false,
    6,
    MINUS_SQUARE_AND_FIVE,
    ONES,
    [MINUS_ONES, small(-5n)],
  ),
];
