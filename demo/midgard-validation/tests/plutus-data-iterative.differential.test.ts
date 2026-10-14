/**
 * The iterative Plutus Data reader against the Cardano ledger's decoder
 * (`decodeData` in plutus-core 1.66 `PlutusCore/Data.hs`, run over the whole
 * input), and the iterative writer and memory size against the recursive
 * code they replace, vendored in `plutus-data-iterative.recursive-oracles.ts`
 * (byte-identical output, equal sizes).
 *
 * The expected decoder outcomes were produced by that plutus-core decoder and
 * are pinned here, so this test needs no Haskell toolchain: each edge vector's
 * value (rendered by `render`) or refusal, and for the seeded inputs the
 * accepted count and a SHA-256 over every rendered outcome.
 */
import { createHash } from "node:crypto";

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
  DataI,
  DataList,
  DataMap,
} from "@harmoniclabs/plutus-data";
import { describe, expect, it } from "vitest";

import { plutusDataFromCborIterative } from "../src/plutus-data-iterative.decode.js";
import { encodeMidgardCekPlutusData } from "../src/plutus-data-iterative.encode.js";
import { midgardCekDataMemorySize } from "../src/plutus-data-iterative.memory.js";
import {
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

/** The rendering the pinned plutus-core outcomes use. */
const render = (value: Data): string => {
  if (value instanceof DataI) return `I ${value.int.toString()}`;
  if (value instanceof DataB) return `B ${hex(value.bytes)}`;
  if (value instanceof DataList)
    return `L[${value.list.map(render).join(",")}]`;
  if (value instanceof DataMap) {
    return `M[${value.map
      .map(
        (entry) => `${render(entry.fst as Data)}=${render(entry.snd as Data)}`,
      )
      .join(",")}]`;
  }
  if (value instanceof DataConstr) {
    return `C ${value.constr.toString()}[${value.fields.map(render).join(",")}]`;
  }
  throw new Error("unknown Data value");
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

const REFUSED = "refused";

/** The outcome of one input, as a Uint8Array and as a Buffer alike. */
const outcomeOf = (bytes: Uint8Array): string => {
  const outcomes = [Uint8Array.from(bytes), Buffer.from(bytes)].map((input) => {
    const outcome = attempt(() => plutusDataFromCborIterative(input));
    return outcome.accepted ? render(outcome.value) : REFUSED;
  });
  expect(outcomes[1], hex(bytes)).toBe(outcomes[0]);
  return outcomes[0]!;
};

const AB64 = "ab".repeat(64);
const AB65 = "ab".repeat(65);

/** Input hex and the plutus-core outcome. */
const EDGE_VECTORS: readonly (readonly [string, string])[] = [
  ["00", "I 0"],
  ["17", "I 23"],
  ["1818", "I 24"],
  ["1800", "I 0"],
  ["190000", "I 0"],
  ["1a00000001", "I 1"],
  ["1b0000000000000001", "I 1"],
  ["1bffffffffffffffff", "I 18446744073709551615"],
  ["3bffffffffffffffff", "I -18446744073709551616"],
  ["20", "I -1"],
  ["1c", REFUSED],
  ["1d", REFUSED],
  ["1e", REFUSED],
  ["1f", REFUSED],
  ["3f", REFUSED],
  ["5c", REFUSED],
  ["9c", REFUSED],
  ["bc", REFUSED],
  ["dc", REFUSED],
  ["c249010000000000000000", "I 18446744073709551616"],
  ["c349010000000000000000", "I -18446744073709551617"],
  ["c349ffffffffffffffffff", "I -4722366482869645213696"],
  ["c240", "I 0"],
  ["c340", "I -1"],
  ["c25f4101ff", "I 1"],
  ["c25f41014102ff", "I 258"],
  ["c200", REFUSED],
  ["c280", REFUSED],
  ["c2c249010000000000000000", REFUSED],
  ["40", "B "],
  ["4401020304", "B 01020304"],
  ["5f", REFUSED],
  ["5fff", "B "],
  ["5f41aaff", "B aa"],
  ["5f41aa42bbccff", "B aabbcc"],
  ["5f5f41aaffff", REFUSED],
  ["5f5fff41aaff", REFUSED],
  ["5f00ff", REFUSED],
  ["5f80ff", REFUSED],
  ["5f60ff", REFUSED],
  ["60", REFUSED],
  ["6161", REFUSED],
  ["8160", REFUSED],
  ["a16000", REFUSED],
  ["f4", REFUSED],
  ["f6", REFUSED],
  ["f7", REFUSED],
  ["f93c00", REFUSED],
  ["fa3f800000", REFUSED],
  ["fb3ff0000000000000", REFUSED],
  ["81f5", REFUSED],
  ["d87981f6", REFUSED],
  ["80", "L[]"],
  ["9fff", "L[]"],
  ["9f00ff", "L[I 0]"],
  ["8200", REFUSED],
  ["83000102", "L[I 0,I 1,I 2]"],
  ["a0", "M[]"],
  ["bfff", "M[]"],
  ["bf0000ff", "M[I 0=I 0]"],
  ["bf00ff", REFUSED],
  ["a200000001", "M[I 0=I 0,I 0=I 1]"],
  ["a201000000", "M[I 1=I 0,I 0=I 0]"],
  ["a20100000001", REFUSED],
  ["a1400041aa01", REFUSED],
  ["bf8000a000ff", "M[L[]=I 0,M[]=I 0]"],
  ["d87980", "C 0[]"],
  ["d8799fff", "C 0[]"],
  ["d87f80", "C 6[]"],
  ["d9050080", "C 7[]"],
  ["d9050080", "C 7[]"],
  ["d9057880", "C 127[]"],
  ["d9057980", REFUSED],
  ["d8668218668080", REFUSED],
  ["d866820080", "C 0[]"],
  ["d866821a0000000180", "C 1[]"],
  ["d8668201", REFUSED],
  ["d86683000080", REFUSED],
  ["d8669f0080ff", "C 0[]"],
  ["d866820180ff", REFUSED],
  ["d9055f80", "C 102[]"],
  ["d95f80", REFUSED],
  ["d87900", REFUSED],
  ["d87940", REFUSED],
  ["d879a0", REFUSED],
  ["d87a9f00ff", "C 1[I 0]"],
  ["c000", REFUSED],
  ["d81800", REFUSED],
  ["d9ffff00", REFUSED],
  ["da0001000000", REFUSED],
  ["dbffffffffffffffff00", REFUSED],
  ["c0c000", REFUSED],
  ["d879d87980", REFUSED],
  ["", REFUSED],
  ["18", REFUSED],
  ["19ff", REFUSED],
  ["41", REFUSED],
  ["81", REFUSED],
  ["9f", REFUSED],
  ["9f00", REFUSED],
  ["a1", REFUSED],
  ["a100", REFUSED],
  ["d879", REFUSED],
  ["0000", REFUSED],
  ["00ff", REFUSED],
  ["80ff", REFUSED],
  ["d87980ff00", REFUSED],
  ["ff", REFUSED],
  ["1f00", REFUSED],
  ["", REFUSED],
  [`5840${AB64}`, `B ${AB64}`],
  [`5841${AB65}`, REFUSED],
  [`5f5840${AB64}41abff`, `B ${AB65}`],
  [`5f5841${AB65}ff`, REFUSED],
  [`5f5840${AB64}5840${AB64}ff`, `B ${"ab".repeat(128)}`],
  [`c25840${AB64}`, `I ${BigInt(`0x${AB64}`).toString()}`],
  [`c25841${AB65}`, REFUSED],
  [`c25f5841${AB65}ff`, REFUSED],
  ["c25f4101410000ff", REFUSED],
  ["c3484000000000000000", "I -4611686018427387905"],
  ["d86682188080", "C 128[]"],
  ["d866821903e880", "C 1000[]"],
  ["d866821bffffffffffffffff80", "C 18446744073709551615[]"],
  ["d86682c24901000000000000000080", REFUSED],
  ["d86682c2410180", REFUSED],
  ["d866823880", REFUSED],
  ["d8669f187f80ff", "C 127[]"],
  ["d8669f18809f00ffff", "C 128[I 0]"],
  ["d8669f008000ff", REFUSED],
  ["d866820083", REFUSED],
  ["8140", "L[B ]"],
  ["a10040", "M[I 0=B ]"],
  ["a14000", "M[B =I 0]"],
  ["d87a8140", "C 1[B ]"],
  ["8240d87980", "L[B ,C 0[]]"],
  ["81d86682188080", "L[C 128[]]"],
  ["a1d8668218808000", "M[C 128[]=I 0]"],
  ["a100d866821903e880", "M[I 0=C 1000[]]"],
  ["d8024101", REFUSED],
  ["d8034101", REFUSED],
  ["d9000241ff", REFUSED],
  ["d87981d866821b000000010000000080", "C 0[C 4294967296[]]"],
];

describe("plutusDataFromCborIterative vs the plutus-core decoder", () => {
  it("agrees on the edge vectors", () => {
    for (const [vector, expected] of EDGE_VECTORS) {
      expect(outcomeOf(Buffer.from(vector, "hex")), vector).toBe(expected);
    }
  });

  it("round-trips the edge integers through the canonical writer", () => {
    for (const value of PLUTUS_DATA_EDGE_INTEGERS) {
      const cbor = recursiveEncodeMidgardCekPlutusData(new DataI(value));
      expect(outcomeOf(cbor), value.toString()).toBe(`I ${value.toString()}`);
    }
  });

  it(
    "agrees on 20,000 seeded inputs, malformed ones included",
    () => {
      const outcomes: string[] = [];
      for (let seed = 1; seed <= 20_000; seed += 1) {
        const rng = makeFuzzRng(seed);
        const bytes = randomPlutusDataCbor(rng, {
          maxDepth: 1 + rng.int(7),
          maxWidth: 1 + rng.int(5),
          malformedRate: seed % 2 === 0 ? 0 : 0.05,
        });
        outcomes.push(outcomeOf(bytes));
      }
      expect(outcomes.filter((outcome) => outcome !== REFUSED).length).toBe(
        12_911,
      );
      expect(
        createHash("sha256")
          .update(
            outcomes
              .map((outcome) => (outcome === REFUSED ? "ERR" : `OK ${outcome}`))
              .join("\n"),
          )
          .digest("hex"),
      ).toBe(
        "5a8bb54ad69a16ada4048bbbf821c4ce6753ef9132b63b98c5c65f0bb77a0af1",
      );
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
