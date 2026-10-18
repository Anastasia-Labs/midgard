#!/usr/bin/env node

/**
 * The `cek-staged-v1` cross-language golden channel: the staged-operand and
 * staged-integer libraries (`cek_staged_blob_v1`, `cek_staged_integer_v1`,
 * and the bulk CBOR reader and writers of `cek_staged_integer_cbor_v1`)
 * against their TypeScript twins (built `dist/`).
 *
 * Every root here comes from TypeScript: blob roots from
 * `commitMidgardCekBlob`, staged-operand roots from
 * `bindMidgardCekStagedOperand`, Integer CBOR from
 * `encodeMidgardCekCanonicalIntegerCbor` (checked against Lucid's own Data
 * encoding), memory words and Integer leaves from their twins, and limbwise
 * sums and differences from the limb twin (checked against BigInt). The
 * generated Aiken module rebuilds the same inputs and requires those values:
 *
 *   * the flat root at 0 B to 131,072 B (one to nine chunks with odd tails,
 *     9,215, 9,216, 16,384, 32,768 and 65,536 B), equal to
 *     `cek_proof_v1.bounded_blob_root_v1` up to 9,215 B;
 *   * staged operands bound from staging reference inputs;
 *   * the Integer CBOR root, length, memory words and leaf of each value, read
 *     and re-encoded by both the per-segment and the bulk reader and writer; and
 *   * the CBOR root of limbwise sums and differences, written by both writers.
 *
 * usage: node scripts/generate-cek-staged-v1-goldens.mjs [--check]
 */

import { basename, dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { Data } from "@lucid-evolution/lucid";

import {
  addMidgardCekSigned,
  encodeMidgardCekCanonicalIntegerCbor,
  midgardCekCanonicalIntegerMagnitudeWords,
  midgardCekCanonicalIntegerOf,
  midgardCekIntegerCborFromMagnitude,
  midgardCekIntegerLeafFromCbor,
  midgardCekSignedLimbs,
  parseMidgardCekCanonicalIntegerCbor,
  subtractMidgardCekSigned,
} from "../dist/cek-staged-integer.js";
import {
  bindMidgardCekStagedOperand,
  groupMidgardCekStagedRuns,
  splitMidgardCekStagedChunks,
} from "../dist/cek-staged-runs.js";
import { commitMidgardCekBlob } from "../dist/index.js";
import {
  aikenBytes,
  formatAikenSource,
  goldenChannelEmitter,
  hex,
  parseGoldenChannelArguments,
} from "./golden-channel.mjs";
import { cekStagedV1AikenPreamble } from "./generate-cek-staged-v1-goldens.render-aiken.mjs";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const packageRoot = resolve(scriptDirectory, "..");
const repositoryRoot = resolve(packageRoot, "../..");
const generatedAikenPath = join(
  repositoryRoot,
  "onchain/aiken/lib/midgard/cek-staged-v1-golden.test.ak",
);
const aikenFileName = basename(generatedAikenPath);

const { checkOnly } = parseGoldenChannelArguments(
  "usage: node scripts/generate-cek-staged-v1-goldens.mjs [--check]",
);
const writeOrCheck = goldenChannelEmitter({ repositoryRoot, checkOnly });

const fail = (message) => {
  throw new Error(`cek-staged-v1: ${message}`);
};

const CHUNK_BYTES = 4_095;
const BOUNDED_BYTES = 9_215;
const PIECE_BYTES = 65_536;

// ## Blob roots

/** Byte i is (i + shift) mod 256, as the Aiken `pattern` builds it. */
const pattern = (length, shift) =>
  Buffer.from(Array.from({ length }, (_, index) => (index + shift) % 256));

const blobLengths = [
  0,
  1,
  CHUNK_BYTES - 1,
  CHUNK_BYTES,
  CHUNK_BYTES + 1,
  ...[2, 3, 4, 5, 6, 7, 8, 9].flatMap((count) => [
    count * CHUNK_BYTES - 1,
    count * CHUNK_BYTES,
    count * CHUNK_BYTES + 1,
  ]),
  BOUNDED_BYTES,
  BOUNDED_BYTES + 1,
  16_384,
  32_768,
  65_536,
  102_400,
  131_072,
];
const sortedLengths = [...new Set(blobLengths)].sort((a, b) => a - b);

const blobRows = sortedLengths.map((length) => {
  const shift = length % 251;
  const root = commitMidgardCekBlob(pattern(length, shift)).root;
  return { length, shift, root: hex(root) };
});

const blobTest = ({ length, shift, root }) => {
  const checks = [
    `flat_root_of_pieces_v1(pattern_pieces(${length}, ${shift})) == expected`,
  ];
  if (length <= PIECE_BYTES) {
    checks.push(`flat_root_v1(pattern(${length}, ${shift})) == expected`);
  }
  if (length <= BOUNDED_BYTES) {
    checks.push(
      `cek_proof_v1.bounded_blob_root_v1(pattern(${length}, ${shift})) == expected`,
    );
  }
  // A one-operand `and` chain does not compile.
  const body =
    checks.length === 1
      ? checks[0]
      : `and {\n    ${checks.join(",\n    ")},\n  }`;
  return `test flat_root_of_${length}_bytes() {
  let expected = ${aikenBytes(root)}
  ${body}
}`;
};

// ## Staged operands

const stagedLengths = [0, 9_216, 65_536, 131_072];
const stagedRows = stagedLengths.map((length) => {
  const shift = (length + 3) % 251;
  const runs = groupMidgardCekStagedRuns(
    splitMidgardCekStagedChunks(pattern(length, shift)),
  );
  const bound = bindMidgardCekStagedOperand(
    runs,
    runs.map((_, index) => index),
  );
  if (
    !Buffer.from(bound.root).equals(
      Buffer.from(commitMidgardCekBlob(pattern(length, shift)).root),
    )
  ) {
    fail(`staged ${length}-byte operand does not bind to its flat root`);
  }
  return { length, shift, runs: runs.length, root: hex(bound.root) };
});

const stagedTest = ({
  length,
  shift,
  root,
}) => `test staged_operand_of_${length}_bytes() {
  let runs = runs_of(pattern_chunks(${length}, ${shift}))
  let Pair(inputs, refs) = staged(runs)
  let bound =
    bind_operand_v1(collect_runs_v1(inputs, refs), list.range(0, list.length(runs) - 1))
  and {
    bound.root == ${aikenBytes(root)},
    bound.length == ${length},
  }
}`;

// ## Integers

const power = (bits) => ({
  aiken: `power(${bits})`,
  value: 1n << BigInt(bits),
});
const repeated = (size, byte) => ({
  aiken: `repeated(${size}, 0x${byte.toString(16).padStart(2, "0")})`,
  value: BigInt(`0x${Buffer.alloc(size, byte).toString("hex")}`),
});
const counting = (size) => ({
  aiken: `counting(${size})`,
  value: BigInt(
    `0x${Buffer.from(Array.from({ length: size }, (_, i) => ((i + 1) % 255) + 1)).toString("hex")}`,
  ),
});
const literal = (value) => ({ aiken: `${value}`, value });
const largest = { aiken: "largest()", value: (1n << 262_143n) - 1n };
const smallest = { aiken: "smallest()", value: -(1n << 262_143n) };
const negate = (x) => ({ aiken: `-${x.aiken}`, value: -x.value });
const plus = (x, offset) => ({
  aiken: offset < 0n ? `${x.aiken} - ${-offset}` : `${x.aiken} + ${offset}`,
  value: x.value + offset,
});

const integerCases = [
  ["zero", literal(0n)],
  ["one", literal(1n)],
  ["minus_one", literal(-1n)],
  ["23", literal(23n)],
  ["24", literal(24n)],
  ["minus_25", literal(-25n)],
  ["255", literal(255n)],
  ["256", literal(256n)],
  ["65535", literal(65_535n)],
  ["65536", literal(65_536n)],
  ["two_to_32_minus_one", plus(power(32), -1n)],
  ["two_to_32", power(32)],
  ["two_to_64_minus_one", plus(power(64), -1n)],
  ["minus_two_to_64", negate(power(64))],
  ["two_to_64", power(64)],
  ["minus_two_to_64_minus_one", plus(negate(power(64)), -1n)],
  ["two_to_512_minus_one", plus(power(512), -1n)],
  ["two_to_512", power(512)],
  ["minus_two_to_512", negate(power(512))],
  ["counting_64", counting(64)],
  ["minus_counting_65", negate(counting(65))],
  ["ff_128", repeated(128, 0xff)],
  ["minus_ff_129_minus_one", plus(negate(repeated(129, 0xff)), -1n)],
  ["minus_two_to_4096", negate(power(4_096))],
  ["largest", largest],
  ["smallest", smallest],
  ["minus_5a_32768", negate(repeated(32_768, 0x5a))],
];

const integerRows = integerCases.map(([name, { aiken, value }]) => {
  const integer = midgardCekCanonicalIntegerOf(value);
  const cbor = encodeMidgardCekCanonicalIntegerCbor(integer);
  if (cbor.toString("hex") !== Data.to(value)) {
    fail(`integer ${name} is not spelled as Lucid spells it`);
  }
  const parsed = parseMidgardCekCanonicalIntegerCbor(cbor);
  const words = midgardCekCanonicalIntegerMagnitudeWords(parsed);
  const cborRoot = commitMidgardCekBlob(cbor).root;
  const leaf = midgardCekIntegerLeafFromCbor({
    cborRoot,
    cborLength: BigInt(cbor.length),
    words,
  });
  return {
    name,
    aiken,
    cborRoot: hex(cborRoot),
    cborLength: cbor.length,
    words: Number(words),
    leafRoot: hex(leaf.root),
  };
});

const integerTest = ({
  name,
  aiken,
  cborRoot,
  cborLength,
  words,
  leafRoot,
}) => `test integer_${name}() {
  let encoded = cbor.serialise(${aiken})
  let parsed = parse_canonical_integer_cbor_v1(encoded)
  let root = ${aikenBytes(cborRoot)}
  and {
    flat_root_v1(encoded) == root,
    builtin.length_of_bytearray(encoded) == ${cborLength},
    magnitude_words_v1(parsed) == ${words},
    integer_leaf_from_cbor_v1(root, ${cborLength}, ${words}).root == ${aikenBytes(leafRoot)},
    encode_canonical_integer_cbor_v1(parsed) == encoded,
    parse_integer_cbor_bulk_v1(encoded) == parsed,
    encode_integer_cbor_bulk_v1(parsed) == encoded,
  }
}`;

// ## Limb sums and differences

const limbCases = [
  ["five_and_minus_five", literal(5n), literal(-5n)],
  ["ff_1536_and_one", repeated(1_536, 0xff), literal(1n)],
  ["growth_and_minus_one", plus(negate(power(4_096)), 1n), literal(-1n)],
  [
    "counting_1025_and_minus_counting_1024",
    counting(1_025),
    negate(counting(1_024)),
  ],
  ["largest_minus_one_and_one", plus(largest, -1n), literal(1n)],
  [
    "counting_32768_and_minus_7f_32768",
    counting(32_768),
    negate(repeated(32_768, 0x7f)),
  ],
];

const limbsOf = (value) =>
  midgardCekSignedLimbs(
    parseMidgardCekCanonicalIntegerCbor(
      encodeMidgardCekCanonicalIntegerCbor(midgardCekCanonicalIntegerOf(value)),
    ),
  );

const limbRows = limbCases.map(([name, left, right]) => {
  const a = limbsOf(left.value);
  const b = limbsOf(right.value);
  const sum = midgardCekIntegerCborFromMagnitude(addMidgardCekSigned(a, b));
  const difference = midgardCekIntegerCborFromMagnitude(
    subtractMidgardCekSigned(a, b),
  );
  const spell = (value) =>
    encodeMidgardCekCanonicalIntegerCbor(midgardCekCanonicalIntegerOf(value));
  if (!sum.equals(spell(left.value + right.value))) fail(`${name} sum`);
  if (!difference.equals(spell(left.value - right.value))) {
    fail(`${name} difference`);
  }
  return {
    name,
    left: left.aiken,
    right: right.aiken,
    sumRoot: hex(commitMidgardCekBlob(sum).root),
    differenceRoot: hex(commitMidgardCekBlob(difference).root),
  };
});

const limbTest = ({
  name,
  left,
  right,
  sumRoot,
  differenceRoot,
}) => `test limbs_${name}() {
  let a = limbs_of(${left})
  let b = limbs_of(${right})
  and {
    flat_root_v1(integer_cbor_from_magnitude_v1(add_signed_v1(a, b))) == ${aikenBytes(sumRoot)},
    flat_root_v1(integer_cbor_from_magnitude_v1(subtract_signed_v1(a, b))) == ${aikenBytes(differenceRoot)},
    flat_root_v1(integer_cbor_from_magnitude_bulk_v1(add_signed_v1(a, b))) == ${aikenBytes(sumRoot)},
    flat_root_of_pieces_v1(integer_cbor_pieces_from_magnitude_v1(subtract_signed_v1(a, b))) == ${aikenBytes(differenceRoot)},
  }
}`;

// ## The module

const source = [
  cekStagedV1AikenPreamble,
  "// ## Flat roots",
  ...blobRows.map(blobTest),
  "// ## Staged operands",
  ...stagedRows.map(stagedTest),
  "// ## Integers",
  ...integerRows.map(integerTest),
  "// ## Limb sums and differences",
  ...limbRows.map(limbTest),
].join("\n\n");

writeOrCheck(
  generatedAikenPath,
  formatAikenSource({
    source: `${source}\n`,
    fileName: aikenFileName,
    repositoryRoot,
    tmpPrefix: "cek-staged-v1-",
  }),
);
