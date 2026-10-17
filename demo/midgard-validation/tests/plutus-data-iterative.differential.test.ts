/**
 * Differential tests for the iterative Plutus Data reader, writer and memory
 * size against the code they replace: the pinned harmonic `dataFromCbor` for the
 * reader (same accept/reject decision and the same value, byte-string array
 * class included) and the old recursive encoder and memory function, vendored
 * in `plutus-data-iterative.recursive-oracles.ts`, for the other two
 * (byte-identical output, equal sizes).
 */
import {
  makeFuzzRng,
  PLUTUS_DATA_EDGE_INTEGERS,
  randomDataTree,
  randomPlutusDataCbor,
} from "@al-ft/midgard-test-support/plutus-data-fuzz";
import {
  type Data,
  DataB,
  DataConstr,
  dataFromCbor,
  DataI,
  DataList,
  DataMap,
} from "@harmoniclabs/plutus-data";
import { describe, expect, it } from "vitest";

import { plutusDataFromCborIterative } from "../src/plutus-data-iterative.decode.js";
import { encodeMidgardCekPlutusData } from "../src/plutus-data-iterative.encode.js";
import { midgardCekDataMemorySize } from "../src/plutus-data-iterative.memory.js";
import {
  harmonicDataDifference,
  recursiveEncodeMidgardCekPlutusData,
  recursiveMidgardCekDataMemorySize,
} from "./plutus-data-iterative.recursive-oracles.js";

type Outcome =
  | { readonly accepted: true; readonly value: Data }
  | { readonly accepted: false };

const attempt = (read: () => Data): Outcome => {
  try {
    return { accepted: true, value: read() };
  } catch {
    return { accepted: false };
  }
};

/** The seeded loops take seconds; the default 5 s is too tight on CI. */
const SEEDED_TIMEOUT_MS = 120_000;

const hex = (bytes: Uint8Array): string => Buffer.from(bytes).toString("hex");

/** Checks one input as a Uint8Array and as a Buffer; returns the verdict. */
const compareDecoders = (bytes: Uint8Array): boolean => {
  let accepted = false;
  for (const input of [Uint8Array.from(bytes), Buffer.from(bytes)]) {
    const expected = attempt(() => dataFromCbor(input));
    const actual = attempt(() => plutusDataFromCborIterative(input));
    const label = `${hex(bytes)} as ${input.constructor.name}`;
    expect(actual.accepted, label).toBe(expected.accepted);
    if (expected.accepted && actual.accepted) {
      expect(harmonicDataDifference(expected.value, actual.value), label).toBe(
        undefined,
      );
      accepted = true;
    }
  }
  return accepted;
};

/** The encoder, memory and value checks for one harmonic value. */
const compareWriters = (value: Data, label: string): void => {
  expect(hex(encodeMidgardCekPlutusData(value)), label).toBe(
    hex(recursiveEncodeMidgardCekPlutusData(value)),
  );
  expect(midgardCekDataMemorySize(value), label).toBe(
    recursiveMidgardCekDataMemorySize(value),
  );
};

const EDGE_VECTORS: readonly string[] = [
  // Scalars, non-minimal heads, reserved and indefinite additional info.
  "00",
  "17",
  "1818",
  "1800",
  "190000",
  "1a00000001",
  "1b0000000000000001",
  "1bffffffffffffffff",
  "3bffffffffffffffff",
  "20",
  "1c",
  "1d",
  "1e",
  "1f",
  "3f",
  "5c",
  "9c",
  "bc",
  "dc",
  // Bignums: tag 2/3 over bytes, empty, chunked, below -2^64, over non-bytes.
  "c249010000000000000000",
  "c349010000000000000000",
  "c349ffffffffffffffffff",
  "c240",
  "c340",
  "c25f4101ff",
  "c25f41014102ff",
  "c200",
  "c280",
  "c2c249010000000000000000",
  // Byte strings: definite, empty, indefinite with 0, 1, 2 chunks, nested.
  "40",
  "4401020304",
  "5f",
  "5fff",
  "5f41aaff",
  "5f41aa42bbccff",
  "5f5f41aaffff",
  "5f5fff41aaff",
  "5f00ff",
  "5f80ff",
  "5f60ff",
  // Text, simple and float items anywhere.
  "60",
  "6161",
  "8160",
  "a16000",
  "f4",
  "f6",
  "f7",
  "f93c00",
  "fa3f800000",
  "fb3ff0000000000000",
  "81f5",
  "d87981f6",
  // Lists and maps: definite, indefinite, empty, duplicate and unsorted keys.
  "80",
  "9fff",
  "9f00ff",
  "8200",
  "83000102",
  "a0",
  "bfff",
  "bf0000ff",
  "bf00ff",
  "a2000000000",
  "a200000001",
  "a201000000",
  "a20100000001",
  "a1400041aa01",
  "bf8000a000ff",
  // Constructors: compact, general, tag 102, tag 1375, over non-arrays.
  "d87980",
  "d8799fff",
  "d87f80",
  "d9050080",
  "d9050080",
  "d9057880",
  "d9057980",
  "d8668218668080",
  "d866820080",
  "d866821a0000000180",
  "d8668201",
  "d86683000080",
  "d8669f0080ff",
  "d866820180ff",
  "d9055f80",
  "d95f80",
  "d8790",
  "d87900",
  "d87940",
  "d879a0",
  "d87a9f00ff",
  // Unknown and nested tags are transparent.
  "c000",
  "d81800",
  "d9ffff00",
  "da0001000000",
  "dbffffffffffffffff00",
  "c0c000",
  "d879d87980",
  // Truncations and trailing bytes.
  "",
  "18",
  "19ff",
  "41",
  "81",
  "9f",
  "9f00",
  "a1",
  "a100",
  "d879",
  "0000",
  "00ff",
  "80ff",
  "d87980ff00",
  "ff",
  "1f00",
];

describe("plutusDataFromCborIterative vs harmonic dataFromCbor", () => {
  it("agrees on the edge vectors", () => {
    for (const vector of EDGE_VECTORS) {
      compareDecoders(Buffer.from(vector, "hex"));
    }
    for (const value of PLUTUS_DATA_EDGE_INTEGERS) {
      compareDecoders(recursiveEncodeMidgardCekPlutusData(new DataI(value)));
    }
  });

  it(
    "agrees on 20,000 seeded inputs, malformed ones included",
    () => {
      let accepted = 0;
      for (let seed = 1; seed <= 20_000; seed += 1) {
        const rng = makeFuzzRng(seed);
        const bytes = randomPlutusDataCbor(rng, {
          maxDepth: 1 + rng.int(7),
          maxWidth: 1 + rng.int(5),
          malformedRate: seed % 2 === 0 ? 0 : 0.05,
        });
        if (compareDecoders(bytes)) accepted += 1;
      }
      // Both decisions are exercised in volume.
      expect(accepted).toBeGreaterThan(5_000);
      expect(accepted).toBeLessThan(20_000);
    },
    SEEDED_TIMEOUT_MS,
  );
});

const harmonicBuilders = {
  integer: (value: bigint): Data => new DataI(value),
  bytes: (bytesHex: string): Data =>
    new DataB(
      bytesHex.length % 4 === 0
        ? Buffer.from(bytesHex, "hex")
        : Uint8Array.from(Buffer.from(bytesHex, "hex")),
    ),
  list: (items: Data[]): Data => new DataList(items),
  map: (entries: [Data, Data][]): Data =>
    new DataMap(entries.map(([key, value]) => ({ fst: key, snd: value }))),
  constr: (index: bigint, fields: Data[]): Data =>
    new DataConstr(index, fields),
};

describe("encodeMidgardCekPlutusData and midgardCekDataMemorySize vs the recursive versions", () => {
  it("agree on the edge values", () => {
    const values: Data[] = [
      ...PLUTUS_DATA_EDGE_INTEGERS.map((value) => new DataI(value)),
      ...[0, 1, 63, 64, 65, 127, 128, 129, 1_000].map(
        (length) => new DataB(Buffer.alloc(length, 0xa5)),
      ),
      new DataList([]),
      new DataMap([]),
      ...[0n, 6n, 7n, 127n, 128n, 1_000n, (1n << 64n) - 1n, 1n << 70n].map(
        (index) => new DataConstr(index, [new DataI(index)]),
      ),
      new DataConstr(3n, []),
    ];
    values.forEach((value, index) => {
      compareWriters(value, `edge value ${index.toString()}`);
    });
  });

  it(
    "agree on 20,000 seeded harmonic trees",
    () => {
      for (let seed = 1; seed <= 20_000; seed += 1) {
        const rng = makeFuzzRng(seed ^ 0x5eed);
        const value = randomDataTree(rng, harmonicBuilders, {
          maxDepth: 1 + rng.int(7),
          maxWidth: 1 + rng.int(5),
        });
        compareWriters(value, `tree seed ${seed.toString()}`);
      }
    },
    SEEDED_TIMEOUT_MS,
  );

  it(
    "agree on 20,000 seeded decoded values",
    () => {
      let compared = 0;
      for (let seed = 1; seed <= 20_000; seed += 1) {
        const rng = makeFuzzRng(seed ^ 0xdeca);
        const bytes = randomPlutusDataCbor(rng, {
          maxDepth: 1 + rng.int(7),
          maxWidth: 1 + rng.int(5),
        });
        const decoded = attempt(() =>
          plutusDataFromCborIterative(Buffer.from(bytes)),
        );
        if (!decoded.accepted) continue;
        compareWriters(decoded.value, `decoded seed ${seed.toString()}`);
        compared += 1;
      }
      expect(compared).toBeGreaterThan(10_000);
    },
    SEEDED_TIMEOUT_MS,
  );
});
