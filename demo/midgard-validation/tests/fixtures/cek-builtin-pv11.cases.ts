import {
  DataB,
  DataConstr,
  DataI,
  DataList,
  DataMap,
  DataPair,
} from "@harmoniclabs/plutus-data";

import { type MidgardCekPv11Constant } from "../../src/cek-builtin.pv11.js";

export const integer = (n: bigint): MidgardCekPv11Constant => ({
  type: { kind: "integer" },
  payload: new DataI(n),
});
export const bytes = (value: string): MidgardCekPv11Constant => ({
  type: { kind: "bytes" },
  payload: new DataB(Buffer.from(value, "hex")),
});
export const integers = (
  items: readonly bigint[],
  kind: "list" | "array" = "list",
): MidgardCekPv11Constant => ({
  type: { kind, element: { kind: "integer" } },
  payload: new DataList(items.map((item) => new DataI(item))),
});
export const value = (
  ...items: readonly (readonly [
    string,
    readonly (readonly [string, bigint])[],
  ])[]
): MidgardCekPv11Constant => ({
  type: { kind: "value" },
  payload: new DataMap(
    items.map(
      ([policy, tokens]) =>
        new DataPair(
          new DataB(Buffer.from(policy, "hex")),
          new DataMap(
            tokens.map(
              ([token, quantity]) =>
                new DataPair(
                  new DataB(Buffer.from(token, "hex")),
                  new DataI(quantity),
                ),
            ),
          ),
        ),
    ),
  ),
});
const data = (constant: MidgardCekPv11Constant): MidgardCekPv11Constant => ({
  type: { kind: "data" },
  payload: constant.payload,
});
const empty = value();
const v1 = value(["01", [["02", 3n]]]);
const v2 = value([
  "01",
  [
    ["02", 2n],
    ["03", 1n],
  ],
]);
const exp = 1n << 8191n;
const scalar = 1n << 4095n;
const quantity = 1n << 127n;
const longKey = bytes("ab".repeat(33));

export type Pv11Case = {
  readonly name: string;
  readonly tag: bigint;
  readonly arguments: readonly MidgardCekPv11Constant[];
  readonly expected: MidgardCekPv11Constant | boolean | "failure";
};

export const PV11_CASES: Pv11Case[] = [
  {
    name: "expMod positive power",
    tag: 87n,
    arguments: [integer(2n), integer(10n), integer(17n)],
    expected: integer(4n),
  },
  {
    name: "expMod inverse",
    tag: 87n,
    arguments: [integer(3n), integer(-1n), integer(11n)],
    expected: integer(4n),
  },
  {
    name: "expMod noninvertible",
    tag: 87n,
    arguments: [integer(2n), integer(-1n), integer(4n)],
    expected: "failure",
  },
  {
    name: "expMod zero modulus",
    tag: 87n,
    arguments: [integer(1n), integer(1n), integer(0n)],
    expected: "failure",
  },
  {
    name: "expMod negative modulus",
    tag: 87n,
    arguments: [integer(1n), integer(1n), integer(-1n)],
    expected: "failure",
  },
  {
    name: "expMod modulus upper bound",
    tag: 87n,
    arguments: [integer(2n), integer(1n), integer(exp - 1n)],
    expected: integer(2n),
  },
  {
    name: "expMod modulus outside",
    tag: 87n,
    arguments: [integer(2n), integer(1n), integer(exp)],
    expected: "failure",
  },
  {
    name: "expMod base lower bound",
    tag: 87n,
    arguments: [integer(-exp), integer(1n), integer(3n)],
    expected: integer(1n),
  },
  {
    name: "expMod base lower outside",
    tag: 87n,
    arguments: [integer(-exp - 1n), integer(1n), integer(3n)],
    expected: "failure",
  },
  {
    name: "expMod base upper bound",
    tag: 87n,
    arguments: [integer(exp - 1n), integer(1n), integer(3n)],
    expected: integer(1n),
  },
  {
    name: "expMod base upper outside",
    tag: 87n,
    arguments: [integer(exp), integer(1n), integer(3n)],
    expected: "failure",
  },
  {
    name: "expMod exponent lower bound",
    tag: 87n,
    arguments: [integer(1n), integer(-exp), integer(3n)],
    expected: integer(1n),
  },
  {
    name: "expMod exponent lower outside",
    tag: 87n,
    arguments: [integer(1n), integer(-exp - 1n), integer(3n)],
    expected: "failure",
  },
  {
    name: "expMod exponent upper bound",
    tag: 87n,
    arguments: [integer(1n), integer(exp - 1n), integer(3n)],
    expected: integer(1n),
  },
  {
    name: "expMod exponent upper outside",
    tag: 87n,
    arguments: [integer(1n), integer(exp), integer(3n)],
    expected: "failure",
  },
  {
    name: "expMod modulus one precedes bounds",
    tag: 87n,
    arguments: [integer(exp), integer(-exp - 1n), integer(1n)],
    expected: integer(0n),
  },
  {
    name: "expMod zero inverse modulo one",
    tag: 87n,
    arguments: [integer(0n), integer(-1n), integer(1n)],
    expected: integer(0n),
  },
  {
    name: "expMod zero inverse fails",
    tag: 87n,
    arguments: [integer(0n), integer(-1n), integer(2n)],
    expected: "failure",
  },
  {
    name: "dropList count",
    tag: 88n,
    arguments: [integer(2n), integers([1n, 2n, 3n])],
    expected: integers([3n]),
  },
  {
    name: "dropList negative",
    tag: 88n,
    arguments: [integer(-2n), integers([1n, 2n])],
    expected: integers([1n, 2n]),
  },
  {
    name: "dropList past end",
    tag: 88n,
    arguments: [integer(3n), integers([1n])],
    expected: integers([]),
  },
  {
    name: "array length",
    tag: 89n,
    arguments: [integers([1n, 2n], "array")],
    expected: integer(2n),
  },
  {
    name: "empty array length",
    tag: 89n,
    arguments: [integers([], "array")],
    expected: integer(0n),
  },
  {
    name: "listToArray",
    tag: 90n,
    arguments: [integers([1n, 1n << 128n, 3n])],
    expected: integers([1n, 1n << 128n, 3n], "array"),
  },
  {
    name: "listToArray empty",
    tag: 90n,
    arguments: [integers([])],
    expected: integers([], "array"),
  },
  {
    name: "indexArray",
    tag: 91n,
    arguments: [integers([1n, 2n], "array"), integer(1n)],
    expected: integer(2n),
  },
  {
    name: "indexArray negative",
    tag: 91n,
    arguments: [integers([1n], "array"), integer(-1n)],
    expected: "failure",
  },
  {
    name: "indexArray at length",
    tag: 91n,
    arguments: [integers([1n], "array"), integer(1n)],
    expected: "failure",
  },
  {
    name: "indexArray Int64 minimum",
    tag: 91n,
    arguments: [integers([], "array"), integer(-(1n << 63n))],
    expected: "failure",
  },
  {
    name: "indexArray Int64 maximum",
    tag: 91n,
    arguments: [integers([], "array"), integer((1n << 63n) - 1n)],
    expected: "failure",
  },
  {
    name: "indexArray Int64 lower outside",
    tag: 91n,
    arguments: [integers([], "array"), integer(-(1n << 63n) - 1n)],
    expected: "failure",
  },
  {
    name: "indexArray Int64 upper outside",
    tag: 91n,
    arguments: [integers([], "array"), integer(1n << 63n)],
    expected: "failure",
  },
  {
    name: "indexArray boolean element",
    tag: 91n,
    arguments: [
      {
        type: { kind: "array", element: { kind: "boolean" } },
        payload: new DataList([new DataConstr(0, []), new DataConstr(1, [])]),
      },
      integer(1n),
    ],
    expected: true,
  },
  {
    name: "insertCoin maximum key lengths",
    tag: 94n,
    arguments: [
      bytes("ab".repeat(32)),
      bytes("cd".repeat(32)),
      integer(1n),
      empty,
    ],
    expected: value(["ab".repeat(32), [["cd".repeat(32), 1n]]]),
  },
  {
    name: "insertCoin replaces",
    tag: 94n,
    arguments: [bytes("01"), bytes("02"), integer(4n), v1],
    expected: value(["01", [["02", 4n]]]),
  },
  {
    name: "insertCoin deletes empty policy",
    tag: 94n,
    arguments: [bytes("01"), bytes("02"), integer(0n), v1],
    expected: empty,
  },
  {
    name: "insertCoin zero permits long keys",
    tag: 94n,
    arguments: [longKey, longKey, integer(0n), v1],
    expected: v1,
  },
  {
    name: "insertCoin long policy",
    tag: 94n,
    arguments: [longKey, bytes(""), integer(1n), empty],
    expected: "failure",
  },
  {
    name: "insertCoin long token",
    tag: 94n,
    arguments: [bytes(""), longKey, integer(1n), empty],
    expected: "failure",
  },
  {
    name: "insertCoin quantity lower bound",
    tag: 94n,
    arguments: [bytes(""), bytes(""), integer(-quantity), empty],
    expected: value(["", [["", -quantity]]]),
  },
  {
    name: "insertCoin quantity upper bound",
    tag: 94n,
    arguments: [bytes(""), bytes(""), integer(quantity - 1n), empty],
    expected: value(["", [["", quantity - 1n]]]),
  },
  {
    name: "insertCoin quantity lower outside",
    tag: 94n,
    arguments: [bytes(""), bytes(""), integer(-quantity - 1n), empty],
    expected: "failure",
  },
  {
    name: "insertCoin quantity upper outside",
    tag: 94n,
    arguments: [bytes(""), bytes(""), integer(quantity), empty],
    expected: "failure",
  },
  {
    name: "lookupCoin found",
    tag: 95n,
    arguments: [bytes("01"), bytes("02"), v1],
    expected: integer(3n),
  },
  {
    name: "lookupCoin missing",
    tag: 95n,
    arguments: [bytes("01"), bytes("03"), v1],
    expected: integer(0n),
  },
  {
    name: "lookupCoin long keys",
    tag: 95n,
    arguments: [longKey, longKey, v1],
    expected: integer(0n),
  },
  {
    name: "unionValue combines",
    tag: 96n,
    arguments: [v1, v2],
    expected: value([
      "01",
      [
        ["02", 5n],
        ["03", 1n],
      ],
    ]),
  },
  {
    name: "unionValue orders separate policies",
    tag: 96n,
    arguments: [value(["02", [["", 1n]]]), value(["01", [["", 2n]]])],
    expected: value(["01", [["", 2n]]], ["02", [["", 1n]]]),
  },
  {
    name: "unionValue cancels",
    tag: 96n,
    arguments: [v1, value(["01", [["02", -3n]]])],
    expected: empty,
  },
  {
    name: "unionValue upper overflow",
    tag: 96n,
    arguments: [value(["", [["", quantity - 1n]]]), value(["", [["", 1n]]])],
    expected: "failure",
  },
  {
    name: "unionValue lower overflow",
    tag: 96n,
    arguments: [value(["", [["", -quantity]]]), value(["", [["", -1n]]])],
    expected: "failure",
  },
  {
    name: "valueContains true",
    tag: 97n,
    arguments: [v1, value(["01", [["02", 2n]]])],
    expected: true,
  },
  {
    name: "valueContains false",
    tag: 97n,
    arguments: [v1, v2],
    expected: false,
  },
  {
    name: "valueContains negative left",
    tag: 97n,
    arguments: [value(["", [["", -1n]]]), empty],
    expected: "failure",
  },
  {
    name: "valueContains negative right",
    tag: 97n,
    arguments: [empty, value(["", [["", -1n]]])],
    expected: "failure",
  },
  { name: "valueData", tag: 98n, arguments: [v1], expected: data(v1) },
  {
    name: "valueData empty",
    tag: 98n,
    arguments: [empty],
    expected: data(empty),
  },
  { name: "unValueData", tag: 99n, arguments: [data(v1)], expected: v1 },
  {
    name: "unValueData empty",
    tag: 99n,
    arguments: [data(empty)],
    expected: empty,
  },
  {
    name: "unValueData nonmap",
    tag: 99n,
    arguments: [data(integer(1n))],
    expected: "failure",
  },
  {
    name: "unValueData zero quantity",
    tag: 99n,
    arguments: [data(value(["", [["", 0n]]]))],
    expected: "failure",
  },
  {
    name: "unValueData empty token map",
    tag: 99n,
    arguments: [data(value(["", []]))],
    expected: "failure",
  },
  {
    name: "unValueData duplicate token",
    tag: 99n,
    arguments: [
      data(
        value([
          "",
          [
            ["", 1n],
            ["", 2n],
          ],
        ]),
      ),
    ],
    expected: "failure",
  },
  {
    name: "unValueData descending policy",
    tag: 99n,
    arguments: [data(value(["01", [["", 1n]]], ["", [["", 1n]]]))],
    expected: "failure",
  },
  {
    name: "unValueData long key",
    tag: 99n,
    arguments: [data(value(["ab".repeat(33), [["", 1n]]]))],
    expected: "failure",
  },
  {
    name: "unValueData quantity outside",
    tag: 99n,
    arguments: [data(value(["", [["", quantity]]]))],
    expected: "failure",
  },
  {
    name: "unValueData quantity lower bound",
    tag: 99n,
    arguments: [data(value(["", [["", -quantity]]]))],
    expected: value(["", [["", -quantity]]]),
  },
  {
    name: "unValueData quantity upper bound",
    tag: 99n,
    arguments: [data(value(["", [["", quantity - 1n]]]))],
    expected: value(["", [["", quantity - 1n]]]),
  },
  {
    name: "unValueData quantity lower outside",
    tag: 99n,
    arguments: [data(value(["", [["", -quantity - 1n]]]))],
    expected: "failure",
  },
  {
    name: "scaleValue negative",
    tag: 100n,
    arguments: [integer(-1n), v1],
    expected: value(["01", [["02", -3n]]]),
  },
  {
    name: "scaleValue positive",
    tag: 100n,
    arguments: [integer(2n), v1],
    expected: value(["01", [["02", 6n]]]),
  },
  {
    name: "scaleValue zero",
    tag: 100n,
    arguments: [integer(0n), v1],
    expected: empty,
  },
  {
    name: "scaleValue empty permits large scalar",
    tag: 100n,
    arguments: [integer(quantity), empty],
    expected: empty,
  },
  {
    name: "scaleValue overflow",
    tag: 100n,
    arguments: [integer(-1n), value(["", [["", -quantity]]])],
    expected: "failure",
  },
];

for (const [group, tag, generator, zero] of [
  [
    "blsG1",
    92n,
    "97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb",
    `c0${"00".repeat(47)}`,
  ],
  [
    "blsG2",
    93n,
    "93e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8",
    `c0${"00".repeat(95)}`,
  ],
] as const) {
  const points = (items: string[]): MidgardCekPv11Constant => ({
    type: { kind: "list", element: { kind: group } },
    payload: new DataList(
      items.map((item) => new DataB(Buffer.from(item, "hex"))),
    ),
  });
  const point = (encoded: string): MidgardCekPv11Constant => ({
    type: { kind: group },
    payload: new DataB(Buffer.from(encoded, "hex")),
  });
  PV11_CASES.push(
    {
      name: `${group} array preserves point`,
      tag: 91n,
      arguments: [
        {
          type: { kind: "array", element: { kind: group } },
          payload: points([generator]).payload,
        },
        integer(0n),
      ],
      expected: point(generator),
    },
    {
      name: `${group} MSM identity`,
      tag,
      arguments: [integers([1n]), points([generator])],
      expected: point(generator),
    },
    {
      name: `${group} MSM cancellation`,
      tag,
      arguments: [integers([1n, -1n]), points([generator, generator])],
      expected: point(zero),
    },
    {
      name: `${group} MSM empty`,
      tag,
      arguments: [integers([]), points([])],
      expected: point(zero),
    },
    {
      name: `${group} MSM truncates points`,
      tag,
      arguments: [integers([1n]), points([generator, generator])],
      expected: point(generator),
    },
    {
      name: `${group} MSM scalar lower bound`,
      tag,
      arguments: [integers([-scalar]), points([])],
      expected: point(zero),
    },
    {
      name: `${group} MSM scalar upper bound`,
      tag,
      arguments: [integers([scalar - 1n]), points([])],
      expected: point(zero),
    },
    {
      name: `${group} MSM unused scalar lower outside`,
      tag,
      arguments: [integers([-scalar - 1n]), points([])],
      expected: "failure",
    },
    {
      name: `${group} MSM unused scalar upper outside`,
      tag,
      arguments: [integers([1n, scalar]), points([generator])],
      expected: "failure",
    },
  );
}
