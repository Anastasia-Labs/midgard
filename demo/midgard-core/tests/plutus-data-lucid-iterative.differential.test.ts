import {
  type DeepDataShape,
  deepPlutusDataCbor,
  type FuzzRng,
  makeFuzzRng,
  PLUTUS_DATA_EDGE_INTEGERS,
  randomDataTree,
  randomPlutusDataCbor,
} from "@al-ft/midgard-test-support/plutus-data-fuzz";
import { Constr, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  lucidDataFromCborIterative,
  lucidDataToCborIterative,
} from "../src/plutus-data-lucid-iterative.js";

/**
 * The iterative Lucid-shaped codec against Lucid itself (`Data.from` /
 * `Data.to`, which go through CML): the same accept set and structurally
 * identical values or byte-identical encodings. Depths stay at or below 1,500
 * so Lucid's own recursion does not overflow.
 */

const SEEDED_CASES = 20_000;

const describeData = (value: unknown): string => {
  if (typeof value === "bigint") return `${value}n`;
  if (typeof value === "string") return `h:${value}`;
  if (value instanceof Constr) {
    const index: unknown = value.index;
    return `C(${typeof index}:${String(index)})[${(value.fields as unknown[])
      .map(describeData)
      .join(",")}]`;
  }
  if (Array.isArray(value)) return `[${value.map(describeData).join(",")}]`;
  if (value instanceof Map) {
    return `M{${[...(value as Map<unknown, unknown>)]
      .map(([k, v]) => `${describeData(k)}=>${describeData(v)}`)
      .join(",")}}`;
  }
  return `?${String(value)}`;
};

const outcome = (run: () => unknown): string => {
  try {
    const result = run();
    return `ok ${Buffer.isBuffer(result) ? result.toString("hex") : describeData(result)}`;
  } catch {
    return "rejected";
  }
};

const expectSameFrom = (bytes: Uint8Array, label: string): boolean => {
  const hex = Buffer.from(bytes).toString("hex");
  const expected = outcome(() => Data.from(hex));
  expect(
    outcome(() => lucidDataFromCborIterative(bytes)),
    `${label} ${hex}`,
  ).toBe(expected);
  expect(
    outcome(() => lucidDataFromCborIterative(hex)),
    `${label} hex`,
  ).toBe(expected);
  return expected !== "rejected";
};

const expectSameTo = (value: unknown, label: string): boolean => {
  const expected = outcome(() => Buffer.from(Data.to(value as Data), "hex"));
  expect(
    outcome(() => lucidDataToCborIterative(value as Data)),
    label,
  ).toBe(expected);
  return expected !== "rejected";
};

const hexOf = (length: number, byte: string): string => byte.repeat(length);

const FROM_EDGES: readonly string[] = [
  "",
  "00",
  "0000",
  "01ff",
  "1801",
  "1b0000000000000001",
  "3801",
  "20",
  "3bffffffffffffffff",
  "1c",
  "1f",
  "3f",
  "c240",
  "c24100",
  "c2410001",
  "c25f4101ff",
  "c25fff",
  "c249010000000000000000",
  "c349010000000000000000",
  "c34100",
  "c24200ff",
  `c25840${hexOf(64, "aa")}`,
  `c25841${hexOf(65, "aa")}`,
  `c25f5828${hexOf(40, "01")}5828${hexOf(40, "01")}ff`,
  "c201",
  "c280",
  "c2c24101",
  "c25f4101",
  "d866820180",
  "d86683018000",
  "d8669f0180ff",
  "d8669f018000ff",
  "d866821bffffffffffffffff80",
  "d866821b001fffffffffffff80",
  "d86682c2410180",
  "d866823880",
  "d8668201a0",
  "d866820101",
  "d866810180",
  "d86680",
  "d866a0",
  "d8669fff",
  "d8669f01",
  "d866",
  "d8668480800000",
  "82d8668301800005",
  "82d8669f0180ff05",
  "83d86684018000000506",
  "d8798000",
  "d87980",
  "d8799fff",
  "d87a80",
  "d87f80",
  "d9050080",
  "d9057880",
  "d9057980",
  "d8788080",
  "d904ff80",
  "d8790101",
  "d879a0",
  "d87901",
  "d9007980",
  "da0000007980",
  "db000000000000007980",
  "d8799f01",
  "5f4101ff",
  "5fff",
  "5f",
  `5840${hexOf(64, "aa")}`,
  `5841${hexOf(65, "aa")}`,
  `5f5841${hexOf(65, "aa")}ff`,
  "5f4000ff",
  "5f5f4100ffff",
  "5f6100ff",
  "4aab",
  "41ab",
  "80",
  "9fff",
  "9f01ff",
  "8101",
  "9f",
  "818181",
  "8201",
  "a0",
  "bfff",
  "bf0102ff",
  "bf01ff",
  "bf0102",
  "a201020103",
  "a21801020103",
  "a2c24101020103",
  "a2d8798001d8798002",
  "a3d879800181010ad8798002",
  "a2d8798001d86682008002",
  "a2d879810102d8799f01ff03",
  "a2a1010205a1010206",
  "a2a20102030405a2030401020607",
  "a28141aa01815f41aaff02",
  "a2818001819fff02",
  "a2a001bfff02",
  "a2c2410002000c",
  "a28101",
  "a101a0",
  "a1a00101",
  "f4",
  "f5",
  "f6",
  "f7",
  "f93c00",
  "fb3ff0000000000000",
  "6161",
  "c0",
  "d81e8201",
  "d90102820101",
  "ff",
  "c6",
  "d87bff",
  // A break where an item or key is due closes a definite container early.
  "97ff",
  "8301ff",
  "d87f97ff",
  "828301ff05",
  "a20102ff",
  "a201ff",
  "a2ff",
  "82f6",
  "8301f7",
  "83d866830180ff0506",
  "d8669f0183ffff",
  "d8669f0183ff",
];

describe("lucidDataFromCborIterative vs Lucid Data.from", () => {
  it("agrees on the edge list", () => {
    for (const hex of FROM_EDGES) {
      expectSameFrom(Buffer.from(hex, "hex"), "edge");
    }
  });

  it("agrees on malformed hex strings", () => {
    for (const hex of ["0", "zz", "AB", "4aAB", "41AB", "0x00", " 00"]) {
      expect(
        outcome(() => lucidDataFromCborIterative(hex)),
        hex,
      ).toBe(outcome(() => Data.from(hex)));
    }
  });

  it(`agrees on ${SEEDED_CASES} seeded inputs`, () => {
    let accepted = 0;
    for (let i = 0; i < SEEDED_CASES; i += 1) {
      const rng = makeFuzzRng(0x1dcd0000 + i);
      const bytes = randomPlutusDataCbor(rng, {
        maxDepth: 1 + rng.int(6),
        maxWidth: 1 + rng.int(5),
        malformedRate: rng.chance(0.5) ? 0 : 0.03,
      });
      if (expectSameFrom(bytes, `seed ${i}`)) accepted += 1;
    }
    expect(accepted).toBeGreaterThan(SEEDED_CASES / 4);
    expect(accepted).toBeLessThan(SEEDED_CASES);
  });

  it("agrees on deep chains up to 1,500", () => {
    const shapes: DeepDataShape[] = [
      "definite-list",
      "indefinite-list",
      "map",
      "constr",
    ];
    for (const shape of shapes) {
      for (const depth of [1, 255, 256, 257, 1_500]) {
        expect(
          expectSameFrom(deepPlutusDataCbor(shape, depth), `${shape} ${depth}`),
        ).toBe(true);
      }
    }
  });
});

const lucidBuilders = (rng: FuzzRng) => ({
  integer: (value: bigint): Data => value,
  bytes: (hex: string): Data => (rng.chance(0.1) ? hex.toUpperCase() : hex),
  list: (items: Data[]): Data => items,
  map: (entries: [Data, Data][]): Data => new Map(entries),
  constr: (index: bigint, fields: Data[]): Data =>
    new Constr(
      rng.chance(0.2) ? (index as unknown as number) : Number(index),
      fields,
    ),
});

const circularList = (): unknown => {
  const list: unknown[] = [1n];
  list.push(list);
  return list;
};

const TO_EDGES: readonly [string, () => unknown][] = [
  ...PLUTUS_DATA_EDGE_INTEGERS.map((value): [string, () => unknown] => [
    `int ${value}`,
    () => value,
  ]),
  ["huge int", () => (1n << 4000n) + 7n],
  ["huge negative", () => -(1n << 4000n)],
  ...[0, 1, 63, 64, 65, 128, 129, 1000].map(
    (length): [string, () => unknown] => [
      `bytes ${length}`,
      () => "ab".repeat(length),
    ],
  ),
  ["upper hex", () => "AbCd"],
  ["odd hex", () => "abc"],
  ["non-hex", () => "zz"],
  ["0x hex", () => "0x12"],
  ["empty list", () => []],
  ["holes", () => [1n, , 2n]], // eslint-disable-line no-sparse-arrays
  ["undefined item", () => [1n, undefined]],
  ["empty map", () => new Map()],
  ...[0, 6, 7, 127, 128, 2 ** 53, -0].map((index): [string, () => unknown] => [
    `constr ${index}`,
    () => new Constr(index, [1n]),
  ]),
  ...[
    "5",
    "+5",
    "1_0",
    "_1",
    "5_",
    " 5",
    "05",
    "-0",
    "-1",
    "",
    "+",
    "-",
    "-+5",
    "+-5",
    "++5",
    "0x10",
    "1e3",
    "18446744073709551615",
    "18446744073709551616",
  ].map((index): [string, () => unknown] => [
    `constr index ${JSON.stringify(index)}`,
    () => new Constr(index as unknown as number, []),
  ]),
  ["constr bigint index", () => new Constr(3n as unknown as number, [])],
  [
    "constr u64 max",
    () => new Constr(18446744073709551615n as unknown as number, []),
  ],
  [
    "constr past u64",
    () => new Constr(18446744073709551616n as unknown as number, []),
  ],
  ["constr NaN", () => new Constr(NaN, [])],
  ["constr negative", () => new Constr(-5, [1n])],
  ["constr fraction", () => new Constr(1.5, [])],
  [
    "constr undefined index",
    () => new Constr(undefined as unknown as number, []),
  ],
  [
    "constr object index",
    () => new Constr({ toString: () => "3" } as unknown as number, []),
  ],
  [
    "constr set fields",
    () => new Constr(0, new Set([1n]) as unknown as Data[]),
  ],
  ["constr string fields", () => new Constr(0, "ab" as unknown as Data[])],
  [
    "constr forEach fields",
    () =>
      new Constr(0, {
        forEach: (f: (x: unknown) => void) => {
          f(1n);
          f(2n);
        },
      } as unknown as Data[]),
  ],
  ["constr holes", () => new Constr(0, [1n, , 2n])], // eslint-disable-line no-sparse-arrays
  [
    "dup object keys",
    () =>
      new Map<Data, Data>([
        [[1n], 1n],
        [2n, 5n],
        [[1n], 2n],
      ]),
  ],
  [
    "dup hex keys",
    () =>
      new Map<Data, Data>([
        ["aa", 1n],
        ["AA", 2n],
      ]),
  ],
  [
    "dup constr keys",
    () =>
      new Map<Data, Data>([
        [new Constr(0, []), 1n],
        [new Constr(0, []), 2n],
        [new Constr(1, []), 3n],
        [new Constr(0, []), 4n],
      ]),
  ],
  [
    "dup map keys",
    () =>
      new Map<Data, Data>([
        [new Map([[1n, 2n]]), 1n],
        [new Map([[1n, 2n]]), 2n],
      ]),
  ],
  [
    "ordered map keys differ",
    () =>
      new Map<Data, Data>([
        [
          new Map([
            [1n, 2n],
            [3n, 4n],
          ]),
          1n,
        ],
        [
          new Map([
            [3n, 4n],
            [1n, 2n],
          ]),
          2n,
        ],
      ]),
  ],
  [
    "int vs bytes keys",
    () =>
      new Map<Data, Data>([
        [1n, 1n],
        ["01", 2n],
      ]),
  ],
  ["unsupported", () => [5]],
  ["boolean", () => true],
  ["null", () => null],
  ["object", () => ({ a: 1n })],
  ["bytes array", () => new Uint8Array([1])],
  ["array subclass", () => new (class Items extends Array {})()],
  ["circular", circularList],
];

describe("lucidDataToCborIterative vs Lucid Data.to", () => {
  it("agrees on the edge list", () => {
    for (const [label, make] of TO_EDGES) {
      expectSameTo(make(), label);
    }
  });

  it(`agrees on ${SEEDED_CASES} seeded trees`, () => {
    let accepted = 0;
    for (let i = 0; i < SEEDED_CASES; i += 1) {
      const rng = makeFuzzRng(0x70cb0000 + i);
      const value = randomDataTree(rng, lucidBuilders(rng), {
        maxDepth: 1 + rng.int(6),
        maxWidth: 1 + rng.int(5),
      });
      if (expectSameTo(value, `seed ${i}`)) accepted += 1;
    }
    expect(accepted).toBeGreaterThan(SEEDED_CASES / 2);
  });

  it("round-trips every accepted decode like Lucid", () => {
    let accepted = 0;
    for (let i = 0; i < 5_000; i += 1) {
      const rng = makeFuzzRng(0x7e7e0000 + i);
      const bytes = randomPlutusDataCbor(rng, { maxDepth: 1 + rng.int(8) });
      if (!expectSameFrom(bytes, `decode ${i}`)) continue;
      accepted += 1;
      const decoded = lucidDataFromCborIterative(bytes);
      if (expectSameTo(decoded, `round trip ${i}`)) {
        expectSameFrom(lucidDataToCborIterative(decoded), `re-decode ${i}`);
      }
    }
    expect(accepted).toBeGreaterThan(1_000);
  });
});
