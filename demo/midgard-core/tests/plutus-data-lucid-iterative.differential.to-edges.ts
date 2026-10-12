import { PLUTUS_DATA_EDGE_INTEGERS } from "@al-ft/midgard-test-support/plutus-data-fuzz";
import { Constr, type Data } from "@lucid-evolution/lucid";

const circularList = (): unknown => {
  const list: unknown[] = [1n];
  list.push(list);
  return list;
};

/** A map whose first value, unencodable, an equal later key replaces. */
const replacedBadValue = (): Data =>
  new Map<Data, Data>([
    [[], new Constr(2 ** 64, [])],
    [[], 1n],
  ]);

/**
 * JS values for `lucidDataToCborIterative` to encode as Lucid's `Data.to`
 * does, accepted and refused alike, by label.
 */
export const TO_EDGES: readonly [string, () => unknown][] = [
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
  // Lucid's own encoder never reads a value that a later equal key replaces;
  // CML converts it first, so a decline anywhere else refuses the whole value.
  ["dup key drops a bad value", () => replacedBadValue()],
  [
    "dup key drops a bad value, string index elsewhere",
    () => [replacedBadValue(), new Constr("3" as unknown as number, [])],
  ],
  [
    "dup key drops a bad value, set fields elsewhere",
    () => [replacedBadValue(), new Constr(0, new Set([1n]) as unknown as [])],
  ],
  [
    "dup key drops a bad value, object index elsewhere",
    () => [
      new Constr({ toString: () => "3" } as unknown as number, []),
      replacedBadValue(),
    ],
  ],
  [
    "dup key inside a key drops a bad value",
    () => new Map([[replacedBadValue(), 2n]]),
  ],
  [
    "dup key inside a key drops a bad value, string index elsewhere",
    () =>
      new Map<Data, Data>([
        [replacedBadValue(), 2n],
        [1n, new Constr("3" as unknown as number, [])],
      ]),
  ],
  [
    "dup hex keys drop a bad value",
    () =>
      new Map<Data, Data>([
        ["AA", 5 as unknown as Data],
        ["aa", 1n],
      ]),
  ],
  [
    "dup key drops a circular value",
    () =>
      new Map<Data, Data>([
        [[], circularList() as Data],
        [[], 1n],
      ]),
  ],
  [
    "dup key drops a circular value, string index elsewhere",
    () =>
      new Map<Data, Data>([
        [[], circularList() as Data],
        [[], new Constr("3" as unknown as number, [])],
      ]),
  ],
  [
    "dup key keeps the bad value",
    () =>
      new Map<Data, Data>([
        [[], 1n],
        [[], 5 as unknown as Data],
      ]),
  ],
  ["constr number index past u64", () => new Constr(2 ** 64, [])],
  ["constr number index 2^63", () => new Constr(2 ** 63, [1n])],
  ["unsupported", () => [5]],
  ["boolean", () => true],
  ["null", () => null],
  ["object", () => ({ a: 1n })],
  ["bytes array", () => new Uint8Array([1])],
  ["array subclass", () => new (class Items extends Array {})()],
  ["circular", circularList],
];
