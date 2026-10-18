/**
 * The inputs behind the Plutus Data CBOR decoding golden vectors, and the
 * outcomes the Cardano ledger's decoder gives the hand-written ones.
 *
 * Driven from two sides. The generator
 * (`scripts/generate-plutus-data-cbor-decoding-v1-goldens.mjs`) runs the rows
 * through the built `dist/` decoders to emit the JSON fixture and the Aiken
 * module `onchain/aiken/lib/midgard/cbor-deserialise-v1-golden.test.ak`, which
 * checks both entries of `midgard/cbor_deserialise` against them. The vitest
 * suite (`tests/plutus-data-cbor-decoding-v1.golden.test.ts`) builds the same
 * rows through `src/` and checks them against the checked-in fixture.
 *
 * `PLUTUS_DATA_CARDANO_EDGE_VECTORS` pins, for each input, what `decodeData` in
 * plutus-core 1.66 (`PlutusCore/Data.hs`, run over the whole input) returns,
 * rendered as `tests/plutus-data-iterative.differential.test.ts` renders it,
 * or `REFUSED`. That suite holds the Data entry to these outcomes.
 *
 * This module imports nothing from the package, so either side can load it.
 */

import {
  DataB,
  DataConstr,
  DataI,
  DataList,
  DataMap,
} from "@harmoniclabs/plutus-data";

export const REFUSED = "refused";

const AB64 = "ab".repeat(64);
const AB65 = "ab".repeat(65);
const AB300 = "ab".repeat(300);

/** Input hex and the plutus-core outcome. */
export const PLUTUS_DATA_CARDANO_EDGE_VECTORS = [
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

/**
 * Inputs only the envelope entry accepts, with its outcome: each holds a
 * definite byte string or chunk longer than 64 bytes.
 */
export const ENVELOPE_ONLY_VECTORS = [
  [`5841${AB65}`, `B ${AB65}`],
  [`59012c${AB300}`, `B ${AB300}`],
  [`5f5841${AB65}ff`, `B ${AB65}`],
  [`5f5841${AB65}5840${AB64}ff`, `B ${AB65}${AB64}`],
  [`c25841${AB65}`, `I ${BigInt(`0x${AB65}`).toString()}`],
  [`c35841${AB65}`, `I ${(-1n - BigInt(`0x${AB65}`)).toString()}`],
  [`d8798159012c${AB300}`, `C 0[B ${AB300}]`],
  [`a15841${AB65}00`, `M[B ${AB65}=I 0]`],
];

/** Inputs the envelope entry refuses, as the Data entry does. */
export const ENVELOPE_REFUSED_VECTORS = [
  "",
  "0000",
  `5841${AB65}00`,
  "5f5f4101ffff",
  `5f5f5841${AB65}ffff`,
  "5f6101ff",
  "61ff",
  "f6",
  "f97e00",
  "1c",
  "5c",
  "ff",
  "d87b4101",
  "d8024101",
  "d9000241ff",
  "c26101",
  "c280",
  "d86682c2410180",
  "d866823880",
  "d866820083",
  "d8669f008000ff",
  "d8664101",
  "d8798000",
  "9f00",
  "a1",
  "5a00000001",
  "58",
];

const filled = (length, byte) => new DataB(Buffer.alloc(length, byte));

/**
 * Values whose canonical serialisation both entries read back: every
 * constructor-index width, integers around the 64-bit boundary and beyond,
 * byte strings either side of 64 bytes, and wide values nested in lists and
 * maps. Constructor indices stay at or below 2^64-1, the ledger's bound.
 */
export const plutusDataCborDecodingValues = () => [
  ...[0n, 6n, 7n, 127n, 128n, 1_000n, 1n << 32n, (1n << 64n) - 1n].map(
    (index) => new DataConstr(index, [new DataI(index)]),
  ),
  ...[
    0n,
    23n,
    24n,
    (1n << 64n) - 1n,
    1n << 64n,
    (1n << 512n) - 1n,
    1n << 600n,
  ].flatMap((value) => [new DataI(value), new DataI(-1n - value)]),
  ...[0, 1, 23, 24, 63, 64, 65, 128, 129, 300].map((length) =>
    filled(length, 0xa5),
  ),
  new DataList([]),
  new DataMap([]),
  new DataList([
    new DataConstr(128n, []),
    new DataI(-(1n << 70n)),
    filled(100, 1),
  ]),
  new DataMap([
    { fst: new DataConstr(300n, [new DataI(1n)]), snd: new DataI(1n << 65n) },
    { fst: new DataI(0n), snd: new DataI(0n) },
    { fst: new DataI(0n), snd: new DataI(1n) },
  ]),
  new DataConstr(1n, [
    filled(200, 0x5a),
    new DataList([new DataI(-(1n << 80n)), filled(64, 1)]),
    new DataMap([{ fst: filled(65, 2), snd: new DataConstr(300n, []) }]),
  ]),
];

/** Payload lengths for the envelope writer `encodeCborBytes`. */
export const ENVELOPE_WRITER_LENGTHS = [0, 1, 23, 24, 64, 65, 255, 256, 4_095];

/** A value carried in compact proof carriage (`compactPlutusDataCarriageCbor`). */
export const plutusDataCborDecodingCarried = () =>
  new DataConstr(1n, [
    filled(200, 0x5a),
    new DataList([new DataI(-(1n << 80n)), filled(64, 1)]),
    new DataMap([{ fst: filled(65, 2), snd: new DataConstr(300n, []) }]),
  ]);

/** Inputs at most this long also contribute every truncation as a row. */
const MAX_TRUNCATED_INPUT_BYTES = 24;

const hexOf = (bytes) => Buffer.from(bytes).toString("hex");

/**
 * The rows: each input, where it comes from (`edge`: written by hand above;
 * `written`: a writer's output; `mutated`: a written input truncated or with a
 * byte appended), and what the Data entry and the envelope entry return for
 * it, as the canonical serialisation of the decoded value, or `null` where
 * the entry refuses. The first occurrence of a repeated input is kept. The decoders and writers are passed in, so the same rows
 * come out of `src/` and `dist/`.
 */
export const buildPlutusDataCborDecodingRows = ({
  decodeData,
  decodeEnvelope,
  encodeData,
  encodeCborBytes,
  compactCarriageCbor,
}) => {
  const written = plutusDataCborDecodingValues().map((value) =>
    hexOf(encodeData(value)),
  );
  const sourced = (source, inputs) =>
    inputs.map((input) => ({ input, source }));
  const inputs = [
    ...sourced("edge", [
      ...PLUTUS_DATA_CARDANO_EDGE_VECTORS.map(([input]) => input),
      ...ENVELOPE_ONLY_VECTORS.map(([input]) => input),
      ...ENVELOPE_REFUSED_VECTORS,
    ]),
    ...sourced("written", [
      ...written,
      ...ENVELOPE_WRITER_LENGTHS.map((length) =>
        hexOf(
          encodeCborBytes(
            Uint8Array.from(
              { length },
              (_, index) => (index * 7 + length) & 0xff,
            ),
          ),
        ),
      ),
      compactCarriageCbor(hexOf(encodeData(plutusDataCborDecodingCarried()))),
    ]),
    ...sourced("mutated", [
      ...written.map((input) => `${input}00`),
      ...written
        .filter((input) => input.length <= 2 * MAX_TRUNCATED_INPUT_BYTES)
        .flatMap((input) =>
          Array.from({ length: input.length / 2 }, (_, length) =>
            input.slice(0, 2 * length),
          ),
        ),
    ]),
  ];
  const outcome = (decode, input) => {
    let value;
    try {
      value = decode(Buffer.from(input, "hex"));
    } catch {
      return null;
    }
    return hexOf(encodeData(value));
  };
  const seen = new Set();
  return inputs
    .filter(({ input }) => !seen.has(input) && seen.add(input))
    .map(({ input, source }) => ({
      input,
      source,
      data: outcome(decodeData, input),
      envelope: outcome(decodeEnvelope, input),
    }));
};
