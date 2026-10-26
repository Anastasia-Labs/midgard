import { MIDGARD_CEK_CARDANO_BLS_TAGS } from "../../src/cek-builtin.cardano-tags.js";
import { evaluatePinnedBuiltin } from "../../src/cek-builtin.pinned-evaluator.js";
import { constT, ConstTyTag, UPLCConst } from "../../src/uplc-constant.js";
import { DataB, DataI, DataList } from "../../src/uplc-data.js";
import { pv11ValueCases } from "./cek-pv11-corpus.values.js";

/**
 * A generated corpus of PV 11 builtin applications (tags 87 to 100), each
 * argument a constant value, with the boundary cases of every Plutus-defined
 * failure. Every case's arguments fit one direct payload.
 */
export type Pv11Case = {
  readonly name: string;
  readonly tag: number;
  readonly arguments: readonly UPLCConst[];
};

const int = (value: bigint | number): UPLCConst => UPLCConst.int(value);
const ints = (...values: bigint[]): UPLCConst =>
  UPLCConst.listOf(constT.int)(values);

/** expModInteger's operand bound, 2^8191 - 1, and its negation less one. */
const EXP_MOD_MAX = (1n << 8191n) - 1n;
const EXP_MOD_MIN = -(1n << 8191n);
/** The multi-scalar multiplication scalar bound, 2^4095 - 1. */
const MSM_MAX = (1n << 4095n) - 1n;
const MSM_MIN = -(1n << 4095n);
const BIG = 1n << 64n;

/** Points of each group, hashed to the curve by the pinned evaluator. */
const points = (tag: number, count: number): readonly Uint8Array[] =>
  Array.from({ length: count }, (_, index) => {
    const result = evaluatePinnedBuiltin(tag, [
      UPLCConst.byteString(Buffer.from([index])),
      UPLCConst.byteString(Buffer.from("midgard-pv11", "utf8")),
    ]);
    if (result.kind !== "value") throw new Error("hash to group failed");
    return result.value.value as Uint8Array;
  });

const pointList = (group: 1 | 2, items: readonly Uint8Array[]): UPLCConst =>
  new UPLCConst(
    [
      ConstTyTag.list,
      group === 1
        ? ConstTyTag.bls12_381_G1_element
        : ConstTyTag.bls12_381_G2_element,
    ],
    items,
  );
const pointArray = (group: 1 | 2, items: readonly Uint8Array[]): UPLCConst =>
  new UPLCConst(
    [
      ConstTyTag.array,
      group === 1
        ? ConstTyTag.bls12_381_G1_element
        : ConstTyTag.bls12_381_G2_element,
    ],
    items,
  );

const arrayOf = (
  element: readonly ConstTyTag[],
  values: readonly unknown[],
): UPLCConst => new UPLCConst([ConstTyTag.array, ...element], values as never);
const listOf = (
  element: readonly ConstTyTag[],
  values: readonly unknown[],
): UPLCConst => new UPLCConst([ConstTyTag.list, ...element], values as never);

/** Whether a case's arguments hold a BLS point, which no program constant can. */
export const holdsPoint = (pv11Case: Pv11Case): boolean =>
  pv11Case.arguments.some(
    (argument) =>
      (argument.type.includes(ConstTyTag.bls12_381_G1_element) ||
        argument.type.includes(ConstTyTag.bls12_381_G2_element)) &&
      (!Array.isArray(argument.value) || argument.value.length > 0),
  );

const expMod = (): Pv11Case[] => {
  const triples: [string, bigint, bigint, bigint][] = [
    ["small", 3n, 5n, 7n],
    ["modulus one", 123n, 456n, 1n],
    ["modulus one, out-of-bound base", EXP_MOD_MAX + 1n, -1n, 1n],
    ["modulus zero", 3n, 5n, 0n],
    ["negative modulus", 3n, 5n, -7n],
    ["modulus at the bound", 3n, 5n, EXP_MOD_MAX],
    ["modulus over the bound", 3n, 5n, EXP_MOD_MAX + 1n],
    ["base at the upper bound", EXP_MOD_MAX, 3n, 1000003n],
    ["base at the lower bound", EXP_MOD_MIN, 3n, 1000003n],
    ["base over the bound", EXP_MOD_MAX + 1n, 3n, 1000003n],
    ["base under the bound", EXP_MOD_MIN - 1n, 3n, 1000003n],
    ["exponent at the bound", 2n, EXP_MOD_MAX, 1000003n],
    ["exponent over the bound", 2n, EXP_MOD_MAX + 1n, 1000003n],
    ["exponent under the bound", 2n, EXP_MOD_MIN - 1n, 1000003n],
    ["zero exponent", 0n, 0n, 13n],
    ["zero base, negative exponent", 0n, -1n, 13n],
    ["inverse", 3n, -1n, 7n],
    ["inverse power", 3n, -5n, 1000003n],
    ["no inverse", 2n, -1n, 4n],
    ["no inverse, large", 6n, -3n, 9n * BIG],
    ["negative base", -3n, 3n, 11n],
    ["base larger than modulus", BIG * BIG, 3n, 1000003n],
    ["wide operands", BIG * BIG + 7n, BIG + 3n, BIG * BIG * BIG + 11n],
  ];
  return triples.map(([name, base, exponent, modulus]) => ({
    name: `expModInteger ${name}`,
    tag: 87,
    arguments: [int(base), int(exponent), int(modulus)],
  }));
};

const dropList = (g1: readonly Uint8Array[]): Pv11Case[] => {
  const lists: [string, UPLCConst][] = [
    ["integers", ints(1n, 2n, 3n, 4n)],
    ["empty", ints()],
    [
      "data",
      listOf(
        [ConstTyTag.data],
        [new DataI(1n), new DataB(Buffer.from("ab", "hex"))],
      ),
    ],
    ["G1 points", pointList(1, g1)],
  ];
  const counts = [-BIG, -1n, 0n, 1n, 3n, 4n, 5n, BIG, BIG * BIG];
  return lists.flatMap(([name, list]) =>
    counts.map((count) => ({
      name: `dropList ${count} of ${name}`,
      tag: 88,
      arguments: [int(count), list],
    })),
  );
};

const arrays = (
  g1: readonly Uint8Array[],
  g2: readonly Uint8Array[],
): [string, UPLCConst][] => [
  ["empty", arrayOf([ConstTyTag.int], [])],
  ["integers", arrayOf([ConstTyTag.int], [5n, -6n, BIG])],
  ["byte strings", arrayOf([ConstTyTag.byteStr], [Buffer.from("00", "hex")])],
  ["strings", arrayOf([ConstTyTag.str], ["a", "bc"])],
  [
    "pairs",
    arrayOf(
      [ConstTyTag.pair, ConstTyTag.int, ConstTyTag.bool],
      [{ fst: 1n, snd: true }],
    ),
  ],
  ["lists", arrayOf([ConstTyTag.list, ConstTyTag.int], [[1n], []])],
  ["G1 points", pointArray(1, g1)],
  ["G2 points", pointArray(2, g2)],
];

const arrayCases = (
  g1: readonly Uint8Array[],
  g2: readonly Uint8Array[],
): Pv11Case[] =>
  arrays(g1, g2).flatMap(([name, array]) => {
    const length = BigInt((array.value as readonly unknown[]).length);
    return [
      { name: `lengthOfArray of ${name}`, tag: 89, arguments: [array] },
      ...[-BIG, -1n, 0n, length - 1n, length, BIG].map((index) => ({
        name: `indexArray ${index} of ${name}`,
        tag: 91,
        arguments: [array, int(index)],
      })),
    ];
  });

const listToArray = (
  g1: readonly Uint8Array[],
  g2: readonly Uint8Array[],
): Pv11Case[] =>
  [
    ["empty", ints()],
    ["integers", ints(1n, 2n, BIG)],
    ["data", listOf([ConstTyTag.data], [new DataList([])])],
    ["units", listOf([ConstTyTag.unit], [undefined, undefined])],
    ["G1 points", pointList(1, g1)],
    ["G2 points", pointList(2, g2)],
  ].map(([name, list]) => ({
    name: `listToArray of ${name as string}`,
    tag: 90,
    arguments: [list as UPLCConst],
  }));

const multiScalarMul = (
  g1: readonly Uint8Array[],
  g2: readonly Uint8Array[],
): Pv11Case[] =>
  (
    [
      [92, 1, g1],
      [93, 2, g2],
    ] as const
  ).flatMap(([tag, group, available]) => {
    const scalars: [string, bigint[], number][] = [
      ["no points", [], 0],
      ["scalars without points", [1n, 2n, 3n], 0],
      ["one point", [5n], 1],
      ["more scalars than points", [2n, 3n, 4n], 2],
      ["more points than scalars", [7n], 3],
      ["zero scalars", [0n, 0n], 2],
      ["negative scalars", [-1n, -BIG], 2],
      ["scalars at the bounds", [MSM_MAX, MSM_MIN], 2],
      ["scalar over the bound", [MSM_MAX + 1n], 1],
      ["scalar under the bound", [MSM_MIN - 1n], 1],
      ["out-of-bound scalar past the points", [1n, MSM_MAX + 1n], 1],
    ];
    return scalars.map(([name, values, count]) => ({
      name: `${group === 1 ? "G1" : "G2"} multiScalarMul ${name}`,
      tag,
      arguments: [ints(...values), pointList(group, available.slice(0, count))],
    }));
  });

/** The whole corpus, tags 87 to 100. */
export const pv11Corpus = (): readonly Pv11Case[] => {
  const g1 = points(MIDGARD_CEK_CARDANO_BLS_TAGS.g1HashToGroup, 3);
  const g2 = points(MIDGARD_CEK_CARDANO_BLS_TAGS.g2HashToGroup, 3);
  return [
    ...expMod(),
    ...dropList(g1),
    ...arrayCases(g1, g2),
    ...listToArray(g1, g2),
    ...multiScalarMul(g1, g2),
    ...pv11ValueCases(),
  ];
};
