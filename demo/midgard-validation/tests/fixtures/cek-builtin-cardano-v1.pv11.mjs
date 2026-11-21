/**
 * The protocol version 11 builtins (tags 87 to 100) of the
 * `cek-builtin-cardano-v1` golden channel: seeded random arguments and fixed
 * edge vectors at the boundary of each Plutus-defined failure.
 *
 * Argument encoding (JSON), beside the channel's others:
 *   ty: "value" | { array: ty }
 *   value: [[policy hex, [[token hex, quantity decimal]]]], canonical (keys
 *          ascending, no empty token map, no zero quantity);
 *   array: array, as a list.
 */
import {
  G1_POOL,
  G2_POOL,
  any,
  bs,
  hex,
  int,
  randomBytes,
  randomData,
  randomInteger,
  SIMPLE,
} from "./cek-builtin-cardano-v1.arguments.mjs";

const MAX_I128 = (1n << 127n) - 1n;
const MIN_I128 = -(1n << 127n);
const EXP_MOD_MAX = (1n << 8191n) - 1n;
const MSM_MAX = (1n << 4095n) - 1n;
const MSM_MIN = -(1n << 4095n);
const BIG = 1n << 64n;

const KEYS = ["", "01", "aa".repeat(28), "bb".repeat(28), "cc".repeat(32)];

const value = (entries) => ({ ty: "value", v: entries });
const list = (ty, items) => ({ ty: { list: ty }, v: items });
const array = (ty, items) => ({ ty: { array: ty }, v: items });
const ints = (...items) =>
  list(
    "int",
    items.map((item) => String(item)),
  );

/** A random canonical Value of up to three currencies of up to three tokens. */
const randomValue = (rng, { nonNegative = false } = {}) => {
  const pickKeys = (count) =>
    [...new Set(Array.from({ length: count }, () => rng.pick(KEYS)))].sort();
  return pickKeys(rng.int(4)).map((policy) => [
    policy,
    pickKeys(1 + rng.int(3)).map((token) => {
      let quantity = rng.chance(0.15)
        ? rng.pick([MAX_I128, MIN_I128])
        : BigInt(rng.int(2001) - 1000);
      if (nonNegative && quantity < 0n) quantity = -quantity - 1n;
      if (quantity === 0n) quantity = 1n;
      return [token, String(quantity)];
    }),
  ]);
};

/** Value data: mostly a Value's own, sometimes any Data. */
const randomValueData = (rng) => {
  if (rng.chance(0.3)) return randomData(rng);
  return {
    m: randomValue(rng).map(([policy, tokens]) => [
      { b: policy },
      { m: tokens.map(([token, quantity]) => [{ b: token }, { i: quantity }]) },
    ]),
  };
};

const sequence = (rng) => {
  const ty = rng.pick(SIMPLE);
  return {
    ty,
    items: Array.from({ length: rng.int(5) }, () => any(rng, ty).v),
  };
};

const scalars = (rng, length) =>
  Array.from({ length }, () => String(randomInteger(rng)));

/** One seeded random argument list for PV 11 builtin `tag`. */
export const pv11RandomArguments = (tag, rng) => {
  switch (tag) {
    case 87:
      return [
        int(randomInteger(rng)),
        int(BigInt(rng.int(41) - 8)),
        int(BigInt(rng.int(200) - 20)),
      ];
    case 88: {
      const { ty, items } = sequence(rng);
      return [int(BigInt(rng.int(8) - 2)), list(ty, items)];
    }
    case 89: {
      const { ty, items } = sequence(rng);
      return [array(ty, items)];
    }
    case 90: {
      const { ty, items } = sequence(rng);
      return [list(ty, items)];
    }
    case 91: {
      const { ty, items } = sequence(rng);
      return [array(ty, items), int(BigInt(rng.int(items.length + 3) - 1))];
    }
    case 92:
    case 93: {
      const pool = tag === 92 ? G1_POOL : G2_POOL;
      const points = Array.from({ length: rng.int(4) }, () => rng.pick(pool));
      return [
        list("int", scalars(rng, rng.int(4))),
        list(tag === 92 ? "g1" : "g2", points),
      ];
    }
    case 94:
      return [
        bs(rng.chance(0.8) ? rng.pick(KEYS) : hex(randomBytes(rng))),
        bs(rng.chance(0.8) ? rng.pick(KEYS) : hex(randomBytes(rng))),
        int(rng.chance(0.2) ? 0n : BigInt(rng.int(2001) - 1000)),
        value(randomValue(rng)),
      ];
    case 95:
      return [bs(rng.pick(KEYS)), bs(rng.pick(KEYS)), value(randomValue(rng))];
    case 96:
      return [value(randomValue(rng)), value(randomValue(rng))];
    case 97: {
      const nonNegative = rng.chance(0.7);
      const left = randomValue(rng, { nonNegative });
      return [
        value(left),
        value(
          rng.chance(0.3)
            ? left.slice(0, 1)
            : randomValue(rng, { nonNegative }),
        ),
      ];
    }
    case 98:
      return [value(randomValue(rng))];
    case 99:
      return [{ ty: "data", v: randomValueData(rng) }];
    case 100:
      return [
        int(rng.chance(0.2) ? randomInteger(rng) : BigInt(rng.int(11) - 5)),
        value(randomValue(rng)),
      ];
    default:
      throw new Error(`no argument generator for builtin ${String(tag)}`);
  }
};

const POLICY = KEYS[2];
const TOKEN = KEYS[3];
const ONE = [[POLICY, [[TOKEN, "1"]]]];
const AT_MAX = [[POLICY, [[TOKEN, String(MAX_I128)]]]];
const AT_MIN = [[POLICY, [[TOKEN, String(MIN_I128)]]]];

const valueData = (entries) => ({
  m: entries.map(([policy, tokens]) => [
    { b: policy },
    { m: tokens.map(([token, quantity]) => [{ b: token }, { i: quantity }]) },
  ]),
});

/** The fixed edge vectors of PV 11 builtin `tag`. */
export const PV11_EDGES = {
  87: () =>
    [
      ["modulus one", 123n, 456n, 1n],
      ["modulus zero", 3n, 5n, 0n],
      ["negative modulus", 3n, 5n, -7n],
      ["modulus at the bound", 3n, 5n, EXP_MOD_MAX],
      ["modulus over the bound", 3n, 5n, EXP_MOD_MAX + 1n],
      ["base over the bound", EXP_MOD_MAX + 1n, 3n, 1000003n],
      ["exponent over the bound", 2n, EXP_MOD_MAX + 1n, 1000003n],
      ["zero base, negative exponent", 0n, -1n, 13n],
      ["inverse", 3n, -1n, 7n],
      ["no inverse", 2n, -1n, 4n],
      ["wide operands", BIG * BIG + 7n, BIG + 3n, BIG * BIG * BIG + 11n],
    ].map(([note, base, exponent, modulus]) => ({
      note,
      args: [int(base), int(exponent), int(modulus)],
    })),
  // dropList is charged on the count's magnitude, so a count far outside
  // the list costs more than the evaluator's budget; the PV 11 differential
  // covers those against the exact charge.
  88: () =>
    [-1000n, -1n, 0n, 3n, 4n, 1000n].map((count) => ({
      note: `drop ${count}`,
      args: [int(count), ints(1, 2, 3)],
    })),
  91: () =>
    [-BIG, -1n, 0n, 2n, 3n, BIG].map((index) => ({
      note: `index ${index}`,
      args: [array("int", ["5", "-6", String(BIG)]), int(index)],
    })),
  92: () => [
    { note: "empty", args: [ints(), list("g1", [])] },
    {
      note: "zip to shorter",
      args: [ints(2, 3, 4), list("g1", G1_POOL.slice(0, 2))],
    },
    {
      note: "scalar at the bound",
      args: [ints(MSM_MAX), list("g1", G1_POOL.slice(0, 1))],
    },
    {
      note: "scalar over the bound",
      args: [ints(MSM_MAX + 1n), list("g1", G1_POOL.slice(0, 1))],
    },
    {
      note: "scalar under the bound",
      args: [ints(MSM_MIN - 1n), list("g1", [])],
    },
  ],
  93: () => [
    { note: "empty", args: [ints(), list("g2", [])] },
    {
      note: "scalar at the lower bound",
      args: [ints(MSM_MIN), list("g2", G2_POOL.slice(0, 1))],
    },
    {
      note: "scalar over the bound",
      args: [ints(MSM_MAX + 1n), list("g2", [])],
    },
  ],
  94: () =>
    [
      ["32-byte keys", KEYS[4], KEYS[4], 1n, []],
      ["33-byte policy", `${KEYS[4]}cc`, TOKEN, 1n, []],
      ["33-byte token", POLICY, `${KEYS[4]}cc`, 1n, []],
      ["33-byte key, zero quantity", `${KEYS[4]}cc`, TOKEN, 0n, ONE],
      ["quantity at the upper bound", POLICY, TOKEN, MAX_I128, []],
      ["quantity over the upper bound", POLICY, TOKEN, MAX_I128 + 1n, []],
      ["quantity at the lower bound", POLICY, TOKEN, MIN_I128, []],
      ["quantity under the lower bound", POLICY, TOKEN, MIN_I128 - 1n, []],
      ["delete the only token", POLICY, TOKEN, 0n, ONE],
    ].map(([note, policy, token, quantity, entries]) => ({
      note,
      args: [bs(policy), bs(token), int(quantity), value(entries)],
    })),
  96: () => [
    { note: "sum at the upper bound", args: [value(AT_MAX), value([])] },
    { note: "sum over the upper bound", args: [value(AT_MAX), value(ONE)] },
    {
      note: "sum under the lower bound",
      args: [value(AT_MIN), value([[POLICY, [[TOKEN, "-1"]]]])],
    },
    {
      note: "sum cancels",
      args: [value(ONE), value([[POLICY, [[TOKEN, "-1"]]]])],
    },
  ],
  97: () => [
    { note: "contains itself", args: [value(ONE), value(ONE)] },
    { note: "empty contains empty", args: [value([]), value([])] },
    {
      note: "negative left",
      args: [value([[POLICY, [[TOKEN, "-1"]]]]), value([])],
    },
    {
      note: "negative right",
      args: [value(ONE), value([[POLICY, [[TOKEN, "-1"]]]])],
    },
  ],
  99: () =>
    [
      ["canonical", valueData(ONE)],
      ["empty", { m: [] }],
      ["zero quantity", valueData([[POLICY, [[TOKEN, "0"]]]])],
      ["empty token map", { m: [[{ b: POLICY }, { m: [] }]] }],
      [
        "unsorted currencies",
        valueData([
          [KEYS[3], [[TOKEN, "1"]]],
          [KEYS[2], [[TOKEN, "1"]]],
        ]),
      ],
      [
        "repeated token",
        valueData([
          [
            POLICY,
            [
              [TOKEN, "1"],
              [TOKEN, "2"],
            ],
          ],
        ]),
      ],
      ["33-byte policy", valueData([[`${KEYS[4]}cc`, [[TOKEN, "1"]]]])],
      [
        "quantity over the bound",
        valueData([[POLICY, [[TOKEN, String(MAX_I128 + 1n)]]]]),
      ],
      ["not a map", { l: [] }],
    ].map(([note, data]) => ({ note, args: [{ ty: "data", v: data }] })),
  100: () => [
    { note: "scale to the upper bound", args: [int(MAX_I128), value(ONE)] },
    { note: "scale over the upper bound", args: [int(2n), value(AT_MAX)] },
    { note: "scale by zero", args: [int(0n), value(ONE)] },
    { note: "negate the lower bound", args: [int(-1n), value(AT_MIN)] },
  ],
};
