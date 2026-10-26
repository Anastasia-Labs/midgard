import { ConstTyTag, UPLCConst } from "../../src/uplc-constant.js";
import {
  type Data,
  DataB,
  DataConstr,
  DataI,
  DataList,
  DataMap,
  DataPair,
} from "../../src/uplc-data.js";
import type { Pv11Case } from "./cek-pv11-corpus.js";

/** The Value builtins' cases (tags 94 to 100) of the PV 11 corpus. */

const int = (value: bigint | number): UPLCConst => UPLCConst.int(value);
const bytes = (hex: string): UPLCConst =>
  UPLCConst.byteString(Buffer.from(hex, "hex"));
const hexBytes = (hex: string): DataB => new DataB(Buffer.from(hex, "hex"));

const MAX_I128 = (1n << 127n) - 1n;
const MIN_I128 = -(1n << 127n);
const BIG = 1n << 64n;

const POLICY = "aa".repeat(28);
const POLICY_B = "bb".repeat(28);
const KEY_32 = "cc".repeat(32);
const KEY_33 = "cc".repeat(33);

/** A Value from `[policy, [[token, quantity], …]]` entries, as given. */
export const valueData = (
  entries: readonly (readonly [
    string,
    readonly (readonly [string, bigint])[],
  ])[],
): DataMap<Data, Data> =>
  new DataMap(
    entries.map(
      ([policy, tokens]) =>
        new DataPair(
          hexBytes(policy),
          new DataMap(
            tokens.map(
              ([token, quantity]) =>
                new DataPair(hexBytes(token), new DataI(quantity)),
            ),
          ),
        ),
    ),
  );

export const valueConstant = (
  entries: readonly (readonly [
    string,
    readonly (readonly [string, bigint])[],
  ])[],
): UPLCConst => new UPLCConst([ConstTyTag.value], valueData(entries));

const BASE_VALUE = [
  ["", [["", 1_000_000n]]],
  [
    POLICY,
    [
      ["01", 5n],
      ["0102", -3n],
    ],
  ],
] as const;

const insertCoin = (): Pv11Case[] => {
  const rows: [
    string,
    string,
    string,
    bigint,
    typeof BASE_VALUE | readonly [],
  ][] = [
    ["new entry", POLICY_B, "00", 7n, BASE_VALUE],
    ["replace", POLICY, "01", 9n, BASE_VALUE],
    ["delete", POLICY, "01", 0n, BASE_VALUE],
    ["delete last token", "", "", 0n, BASE_VALUE],
    ["delete absent", POLICY_B, "", 0n, BASE_VALUE],
    ["delete absent long keys", KEY_33, KEY_33, 0n, BASE_VALUE],
    ["into empty", POLICY, "", 1n, []],
    ["32-byte keys", KEY_32, KEY_32, 1n, BASE_VALUE],
    ["33-byte currency", KEY_33, "00", 1n, BASE_VALUE],
    ["33-byte token", POLICY, KEY_33, 1n, BASE_VALUE],
    ["quantity at the maximum", POLICY, "02", MAX_I128, BASE_VALUE],
    ["quantity at the minimum", POLICY, "02", MIN_I128, BASE_VALUE],
    ["quantity over the maximum", POLICY, "02", MAX_I128 + 1n, BASE_VALUE],
    ["quantity under the minimum", POLICY, "02", MIN_I128 - 1n, BASE_VALUE],
  ];
  return rows.map(([name, currency, token, quantity, value]) => ({
    name: `insertCoin ${name}`,
    tag: 94,
    arguments: [
      bytes(currency),
      bytes(token),
      int(quantity),
      valueConstant(value),
    ],
  }));
};

const lookupCoin = (): Pv11Case[] =>
  (
    [
      ["present", POLICY, "0102"],
      ["ada", "", ""],
      ["absent token", POLICY, "02"],
      ["absent currency", POLICY_B, "01"],
      ["long keys", KEY_33, KEY_33],
    ] as const
  ).map(([name, currency, token]) => ({
    name: `lookupCoin ${name}`,
    tag: 95,
    arguments: [bytes(currency), bytes(token), valueConstant(BASE_VALUE)],
  }));

type Entries = readonly (readonly [
  string,
  readonly (readonly [string, bigint])[],
])[];

const pairs: [string, Entries, Entries][] = [
  ["empty and empty", [], []],
  ["value and empty", BASE_VALUE, []],
  ["empty and value", [], BASE_VALUE],
  ["value and itself", BASE_VALUE, BASE_VALUE],
  [
    "cancelling",
    BASE_VALUE,
    [
      [
        POLICY,
        [
          ["01", -5n],
          ["0102", 3n],
        ],
      ],
    ],
  ],
  [
    "disjoint",
    [[POLICY, [["01", 1n]]]],
    [
      [POLICY_B, [["01", 1n]]],
      [KEY_32, [[KEY_32, 2n]]],
    ],
  ],
  [
    "sum at the maximum",
    [[POLICY, [["01", MAX_I128 - 1n]]]],
    [[POLICY, [["01", 1n]]]],
  ],
  [
    "sum over the maximum",
    [[POLICY, [["01", MAX_I128]]]],
    [[POLICY, [["01", 1n]]]],
  ],
  [
    "sum under the minimum",
    [[POLICY, [["01", MIN_I128]]]],
    [[POLICY, [["01", -1n]]]],
  ],
  [
    "subset",
    [
      [
        POLICY,
        [
          ["01", 5n],
          ["02", 1n],
        ],
      ],
    ],
    [[POLICY, [["01", 4n]]]],
  ],
  [
    "superset",
    [[POLICY, [["01", 4n]]]],
    [
      [
        POLICY,
        [
          ["01", 5n],
          ["02", 1n],
        ],
      ],
    ],
  ],
  ["larger quantity", [[POLICY, [["01", 3n]]]], [[POLICY, [["01", 4n]]]]],
  ["negative left", [[POLICY, [["01", -1n]]]], [[POLICY, [["01", 1n]]]]],
  ["negative right", [[POLICY, [["01", 9n]]]], [[POLICY, [["01", -1n]]]]],
];

const binaryValue = (): Pv11Case[] =>
  pairs.flatMap(([name, left, right]) => [
    {
      name: `unionValue ${name}`,
      tag: 96,
      arguments: [valueConstant(left), valueConstant(right)],
    },
    {
      name: `valueContains ${name}`,
      tag: 97,
      arguments: [valueConstant(left), valueConstant(right)],
    },
  ]);

const valueDataCases = (): Pv11Case[] =>
  pairs.slice(0, 7).map(([name, left]) => ({
    name: `valueData ${name}`,
    tag: 98,
    arguments: [valueConstant(left)],
  }));

const map = (...entries: [Data, Data][]): DataMap<Data, Data> =>
  new DataMap(entries.map(([key, item]) => new DataPair(key, item)));
const b = hexBytes;
const i = (value: bigint): DataI => new DataI(value);

const unValueData = (): Pv11Case[] => {
  const rows: [string, Data][] = [
    ["canonical", valueData(BASE_VALUE)],
    ["empty", map()],
    ["not a map", new DataList([])],
    ["integer", i(1n)],
    ["constructor", new DataConstr(0n, [])],
    [
      "unsorted currencies",
      map(
        [b(POLICY_B), map([b("01"), i(1n)])],
        [b(POLICY), map([b("01"), i(1n)])],
      ),
    ],
    [
      "duplicate currency",
      map(
        [b(POLICY), map([b("01"), i(1n)])],
        [b(POLICY), map([b("02"), i(1n)])],
      ),
    ],
    [
      "unsorted tokens",
      map([b(POLICY), map([b("02"), i(1n)], [b("01"), i(1n)])]),
    ],
    ["empty token map", map([b(POLICY), map()])],
    ["zero quantity", map([b(POLICY), map([b("01"), i(0n)])])],
    ["33-byte currency", map([b(KEY_33), map([b("01"), i(1n)])])],
    ["33-byte token", map([b(POLICY), map([b(KEY_33), i(1n)])])],
    ["quantity at the maximum", map([b(POLICY), map([b("01"), i(MAX_I128)])])],
    [
      "quantity over the maximum",
      map([b(POLICY), map([b("01"), i(MAX_I128 + 1n)])]),
    ],
    [
      "quantity under the minimum",
      map([b(POLICY), map([b("01"), i(MIN_I128 - 1n)])]),
    ],
    ["integer currency", map([i(1n), map([b("01"), i(1n)])])],
    ["integer token", map([b(POLICY), map([i(1n), i(1n)])])],
    ["byte-string quantity", map([b(POLICY), map([b("01"), b("01")])])],
    ["list token map", map([b(POLICY), new DataList([])])],
  ];
  return rows.map(([name, data]) => ({
    name: `unValueData ${name}`,
    tag: 99,
    arguments: [UPLCConst.data(data)],
  }));
};

const scaleValue = (): Pv11Case[] => {
  const rows: [string, bigint, Entries][] = [
    ["by zero", 0n, BASE_VALUE],
    ["by one", 1n, BASE_VALUE],
    ["by minus one", -1n, BASE_VALUE],
    ["by two", 2n, BASE_VALUE],
    ["empty by a large factor", BIG * BIG * BIG, []],
    ["to the maximum", MAX_I128, [[POLICY, [["01", 1n]]]]],
    ["past the maximum", MAX_I128, [[POLICY, [["01", 2n]]]]],
    ["to the minimum", MIN_I128, [[POLICY, [["01", 1n]]]]],
    ["past the minimum", MIN_I128, [[POLICY, [["01", -1n]]]]],
    ["large factor by zero", BIG * BIG * BIG, []],
  ];
  return rows.map(([name, factor, value]) => ({
    name: `scaleValue ${name}`,
    tag: 100,
    arguments: [int(factor), valueConstant(value)],
  }));
};

export const pv11ValueCases = (): Pv11Case[] => [
  ...insertCoin(),
  ...lookupCoin(),
  ...binaryValue(),
  ...valueDataCases(),
  ...unValueData(),
  ...scaleValue(),
];
