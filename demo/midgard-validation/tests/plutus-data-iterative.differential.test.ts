/**
 * The iterative Plutus Data reader against the Cardano ledger's decoder
 * (`decodeData` in plutus-core 1.66 `PlutusCore/Data.hs`, run over the whole
 * input), and the iterative writer and memory size against the recursive
 * code they replace, vendored in `plutus-data-iterative.recursive-oracles.ts`
 * (byte-identical output, equal sizes).
 *
 * The expected decoder outcomes were produced by that plutus-core decoder and
 * are pinned, so this test needs no Haskell toolchain: each edge vector's value
 * (rendered by `render`) or refusal in
 * `fixtures/plutus-data-cbor-decoding-v1.vectors.mjs`, and here, for the seeded
 * inputs, the accepted count and a SHA-256 over every rendered outcome.
 */
import { createHash } from "node:crypto";

import {
  makeFuzzRng,
  PLUTUS_DATA_EDGE_INTEGERS,
  randomDataTree,
  randomPlutusDataCbor,
} from "@al-ft/midgard-test-support/plutus-data-fuzz";
import { describe, expect, it } from "vitest";

import { plutusDataFromCborIterative } from "../src/plutus-data-iterative.decode.js";
import { encodeMidgardCekPlutusData } from "../src/plutus-data-iterative.encode.js";
import { midgardCekDataMemorySize } from "../src/plutus-data-iterative.memory.js";
import {
  type Data,
  DataB,
  DataConstr,
  DataI,
  DataList,
  DataMap,
} from "../src/uplc-data.js";
import {
  PLUTUS_DATA_CARDANO_EDGE_VECTORS,
  REFUSED,
} from "./fixtures/plutus-data-cbor-decoding-v1.vectors.mjs";
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

/** The outcome of one input, as a Uint8Array and as a Buffer alike. */
const outcomeOf = (bytes: Uint8Array): string => {
  const outcomes = [Uint8Array.from(bytes), Buffer.from(bytes)].map((input) => {
    const outcome = attempt(() => plutusDataFromCborIterative(input));
    return outcome.accepted ? render(outcome.value) : REFUSED;
  });
  expect(outcomes[1], hex(bytes)).toBe(outcomes[0]);
  return outcomes[0]!;
};

describe("plutusDataFromCborIterative vs the plutus-core decoder", () => {
  it("agrees on the edge vectors", () => {
    for (const [vector, expected] of PLUTUS_DATA_CARDANO_EDGE_VECTORS) {
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
