/**
 * The protocol version 11 cases (tags 87 to 100) of the known builtin failure
 * conformance corpus (`known-builtin-failure-conformance.cases.mjs`): each
 * Plutus-defined failure on both sides of its limit, the builtins that never
 * fail, and ill-typed and wrong-arity applications.
 */

import {
  encodeMidgardCekConstantTypeCbor,
  encodeMidgardCekPlutusData,
} from "@al-ft/midgard-validation/cek-constant";

import {
  BLS12_381_G1_GENERATOR,
  BLS12_381_G2_GENERATOR,
  bytes,
  bytesList,
  data,
  DataB,
  DataI,
  DataList,
  DataMap,
  filled,
  int,
  intList,
} from "./known-builtin-failure-conformance.arguments.mjs";

const witness = (type, payload) => ({
  kind: "constant",
  witness: {
    typeCbor: encodeMidgardCekConstantTypeCbor(type),
    payloadCbor: encodeMidgardCekPlutusData(payload),
  },
});

const INTEGER = { kind: "integer" };
const EXP_MOD_BOUND = 2n ** 8191n;
const MSM_BOUND = 2n ** 4095n;
const QUANTITY_MAX = 2n ** 127n - 1n;
const QUANTITY_MIN = -(2n ** 127n);

/** An array of integers. */
const intArray = (values) =>
  witness(
    { kind: "array", element: INTEGER },
    new DataList(values.map((value) => new DataI(BigInt(value)))),
  );
/** A list of `count` copies of a BLS generator: G1 for tag 92, G2 for 93. */
const points = (tag, count) =>
  witness(
    { kind: "list", element: { kind: tag === 92 ? "blsG1" : "blsG2" } },
    new DataList(
      Array.from(
        { length: count },
        () =>
          new DataB(
            tag === 92 ? BLS12_381_G1_GENERATOR : BLS12_381_G2_GENERATOR,
          ),
      ),
    ),
  );

const POLICY = new Uint8Array(28).fill(0xaa);
const TOKEN = new Uint8Array(4).fill(0xbb);
const OTHER_TOKEN = new Uint8Array(4).fill(0xcc);

/** A Value's data: `entries` as [[policy, [[token, quantity]]]], as given. */
const valueData = (entries) =>
  new DataMap(
    entries.map(([policy, tokens]) => ({
      fst: new DataB(policy),
      snd: new DataMap(
        tokens.map(([token, quantity]) => ({
          fst: new DataB(token),
          snd: new DataI(quantity),
        })),
      ),
    })),
  );
const value = (entries) => witness({ kind: "value" }, valueData(entries));
/** A one-coin Value. */
const coin = (quantity, token = TOKEN) =>
  value([[POLICY, [[token, quantity]]]]);
const EMPTY = value([]);

const expModCases = (add) => {
  const cases = [
    ["small", 3n, 5n, 7n],
    ["modulus 0", 3n, 5n, 0n],
    ["modulus -7", 3n, 5n, -7n],
    ["modulus 1, base 2^8191", EXP_MOD_BOUND, -1n, 1n],
    ["modulus 2^8191 - 1", 3n, 5n, EXP_MOD_BOUND - 1n],
    ["modulus 2^8191", 3n, 5n, EXP_MOD_BOUND],
    ["base 2^8191 - 1", EXP_MOD_BOUND - 1n, 3n, 1000003n],
    ["base 2^8191", EXP_MOD_BOUND, 3n, 1000003n],
    ["base -2^8191", -EXP_MOD_BOUND, 3n, 1000003n],
    ["base -2^8191 - 1", -EXP_MOD_BOUND - 1n, 3n, 1000003n],
    ["exponent 2^8191 - 1", 2n, EXP_MOD_BOUND - 1n, 1000003n],
    ["exponent 2^8191", 2n, EXP_MOD_BOUND, 1000003n],
    ["exponent -2^8191", 3n, -EXP_MOD_BOUND, 7n],
    ["exponent -2^8191 - 1", 3n, -EXP_MOD_BOUND - 1n, 7n],
    ["zero base, zero exponent", 0n, 0n, 13n],
    ["zero base, exponent -1", 0n, -1n, 13n],
    ["inverse of 3 mod 7", 3n, -1n, 7n],
    ["inverse of -3 mod 7", -3n, -2n, 7n],
    ["no inverse of 2 mod 4", 2n, -1n, 4n],
    ["no inverse of 6 mod 9", 6n, -3n, 9n],
    ["no inverse of 15 mod 2^64 + 5", 2n ** 64n + 20n, -1n, 2n ** 64n + 5n],
  ];
  for (const [name, base, exponent, modulus] of cases) {
    add(87, name, [int(base), int(exponent), int(modulus)]);
  }
  add(87, "byte-string base", [bytes([3]), int(5), int(7)]);
  add(87, "two arguments", [int(3), int(5)]);
};

const indexArrayCases = (add) => {
  const array = intArray([5, 6, 7]);
  for (const index of [
    -(2n ** 64n),
    -(2n ** 63n) - 1n,
    -(2n ** 63n),
    -1n,
    0n,
    2n,
    3n,
    2n ** 63n - 1n,
    2n ** 63n,
    2n ** 64n,
  ]) {
    add(91, `index ${index}`, [array, int(index)]);
  }
  add(91, "index 0 of the empty array", [intArray([]), int(0)]);
  add(91, "index 0 of a G1 array", [
    witness(
      { kind: "array", element: { kind: "blsG1" } },
      new DataList([new DataB(BLS12_381_G1_GENERATOR)]),
    ),
    int(0),
  ]);
  add(91, "index into a list", [intList([5n, 6n, 7n]), int(0)]);
  add(91, "byte-string index", [array, bytes([0])]);
};

const multiScalarMulCases = (add) => {
  for (const tag of [92, 93]) {
    const scalars = [
      ["no scalars", [], 0],
      ["scalars without points", [1n, 2n], 0],
      ["scalar 2^4095 - 1", [MSM_BOUND - 1n], 1],
      ["scalar 2^4095", [MSM_BOUND], 1],
      ["scalar -2^4095", [-MSM_BOUND], 1],
      ["scalar -2^4095 - 1", [-MSM_BOUND - 1n], 1],
      ["out-of-bound scalar past the points", [1n, MSM_BOUND], 1],
      ["out-of-bound scalar without points", [MSM_BOUND], 0],
    ];
    for (const [name, values, count] of scalars) {
      add(tag, name, [intList(values), points(tag, count)]);
    }
    add(tag, "points of the other group", [
      intList([MSM_BOUND]),
      points(tag === 92 ? 93 : 92, 1),
    ]);
    add(tag, "byte-string scalars", [
      witness(
        { kind: "list", element: { kind: "bytes" } },
        new DataList([new DataB(new Uint8Array([1]))]),
      ),
      points(tag, 1),
    ]);
    add(tag, "an integer for the scalars", [int(1), points(tag, 1)]);
  }
};

const insertCoinCases = (add) => {
  const cases = [
    ["32-byte keys", filled(32), filled(32), 1n, EMPTY],
    ["33-byte currency", filled(33), filled(32), 1n, EMPTY],
    ["33-byte token", filled(32), filled(33), 1n, EMPTY],
    ["33-byte currency, quantity 0", filled(33), filled(1), 0n, coin(1n)],
    ["quantity 2^127 - 1", bytes(POLICY), bytes(TOKEN), QUANTITY_MAX, EMPTY],
    ["quantity 2^127", bytes(POLICY), bytes(TOKEN), QUANTITY_MAX + 1n, EMPTY],
    ["quantity -2^127", bytes(POLICY), bytes(TOKEN), QUANTITY_MIN, EMPTY],
    [
      "quantity -2^127 - 1",
      bytes(POLICY),
      bytes(TOKEN),
      QUANTITY_MIN - 1n,
      coin(1n),
    ],
    ["delete the only coin", bytes(POLICY), bytes(TOKEN), 0n, coin(1n)],
  ];
  for (const [name, currency, token, quantity, into] of cases) {
    add(94, name, [currency, token, int(quantity), into]);
  }
  add(94, "an integer for the value", [filled(1), filled(1), int(1), int(1)]);
};

const valueCases = (add) => {
  const pairs = [
    [96, "sum 2^127 - 1", coin(QUANTITY_MAX), EMPTY],
    [96, "sum 2^127", coin(QUANTITY_MAX), coin(1n)],
    [96, "sum -2^127", coin(QUANTITY_MIN + 1n), coin(-1n)],
    [96, "sum -2^127 - 1", coin(QUANTITY_MIN), coin(-1n)],
    [96, "sum cancels", coin(1n), coin(-1n)],
    [
      96,
      "disjoint coins at the bound",
      coin(QUANTITY_MAX),
      coin(QUANTITY_MAX, OTHER_TOKEN),
    ],
    [97, "contains itself", coin(1n), coin(1n)],
    [97, "empty contains empty", EMPTY, EMPTY],
    [97, "does not contain more", coin(1n), coin(2n)],
    [97, "negative left", coin(-1n), EMPTY],
    [97, "negative right", coin(1n), coin(-1n)],
  ];
  for (const [tag, name, left, right] of pairs) add(tag, name, [left, right]);
  add(96, "data for the right value", [coin(1n), data(valueData([]))]);
  add(97, "an integer for the left value", [int(1), EMPTY]);
  const scales = [
    ["2^127 - 1 times one", QUANTITY_MAX, coin(1n)],
    ["2 times 2^127 - 1", 2n, coin(QUANTITY_MAX)],
    ["-1 times -2^127", -1n, coin(QUANTITY_MIN)],
    ["-1 times 2^127 - 1", -1n, coin(QUANTITY_MAX)],
    ["0 times 2^127 - 1", 0n, coin(QUANTITY_MAX)],
    ["2^64 times the empty value", 2n ** 64n, EMPTY],
  ];
  for (const [name, scalar, of] of scales) add(100, name, [int(scalar), of]);
  add(100, "a byte-string scalar", [bytes([2]), coin(1n)]);
};

const unValueDataCases = (add) => {
  const cases = [
    ["canonical", valueData([[POLICY, [[TOKEN, 1n]]]])],
    ["empty", valueData([])],
    [
      "32-byte keys",
      valueData([[new Uint8Array(32), [[new Uint8Array(32), 1n]]]]),
    ],
    ["33-byte policy", valueData([[new Uint8Array(33), [[TOKEN, 1n]]]])],
    ["33-byte token", valueData([[POLICY, [[new Uint8Array(33), 1n]]]])],
    ["quantity 2^127 - 1", valueData([[POLICY, [[TOKEN, QUANTITY_MAX]]]])],
    ["quantity 2^127", valueData([[POLICY, [[TOKEN, QUANTITY_MAX + 1n]]]])],
    [
      "quantity -2^127 - 1",
      valueData([[POLICY, [[TOKEN, QUANTITY_MIN - 1n]]]]),
    ],
    ["zero quantity", valueData([[POLICY, [[TOKEN, 0n]]]])],
    ["empty token map", valueData([[POLICY, []]])],
    [
      "unsorted currencies",
      valueData([
        [new Uint8Array([2]), [[TOKEN, 1n]]],
        [new Uint8Array([1]), [[TOKEN, 1n]]],
      ]),
    ],
    [
      "repeated token",
      valueData([
        [
          POLICY,
          [
            [TOKEN, 1n],
            [TOKEN, 2n],
          ],
        ],
      ]),
    ],
    [
      "integer currency",
      new DataMap([{ fst: new DataI(1n), snd: new DataMap([]) }]),
    ],
    ["a list", new DataList([])],
  ];
  for (const [name, payload] of cases) add(99, name, [data(payload)]);
  add(99, "a Value for the data", [coin(1n)]);
};

/** The builtins that never fail, well-typed: never a known failure. */
const neverFailingCases = (add) => {
  add(88, "drop 5 of one", [int(5), intList([1n])]);
  add(89, "length of three", [intArray([1, 2, 3])]);
  add(90, "list of three to an array", [intList([1n, 2n, 3n])]);
  add(95, "look up an absent coin", [filled(33), filled(1), coin(1n)]);
  add(98, "data of one coin", [coin(1n)]);
};

/** A base per PV 11 tag of the known set for the argument kind sweep. */
export const PV11_KIND_SWEEP_BASES = [
  [87, [int(3), int(5), int(0)]],
  [91, [intArray([5, 6, 7]), int(3)]],
  [91, [intArray([5, 6, 7]), int(-1)]],
  [92, [intList([MSM_BOUND]), points(92, 1)]],
  [92, [bytesList([[1]]), points(92, 1)]],
  [93, [intList([MSM_BOUND]), points(93, 1)]],
  [93, [bytesList([[1]]), points(93, 1)]],
  [94, [filled(33), filled(1), int(1), EMPTY]],
  [94, [filled(1), filled(1), int(0), EMPTY]],
  [96, [coin(QUANTITY_MAX), coin(1n)]],
  [96, [EMPTY, coin(1n)]],
  [97, [coin(-1n), EMPTY]],
  [99, [data(valueData([[POLICY, [[TOKEN, 0n]]]]))]],
  [100, [int(2), coin(QUANTITY_MAX)]],
  [100, [int(2), EMPTY]],
];

/** The PV 11 tags `known_builtin_failure_v1` can answer true for. */
export const PV11_KNOWN_BUILTIN_FAILURE_TAGS = [
  87, 91, 92, 93, 94, 96, 97, 99, 100,
];

/** Every PV 11 case, through the corpus's `add`. */
export const pv11Cases = (add) => {
  expModCases(add);
  indexArrayCases(add);
  multiScalarMulCases(add);
  insertCoinCases(add);
  valueCases(add);
  unValueDataCases(add);
  neverFailingCases(add);
};
