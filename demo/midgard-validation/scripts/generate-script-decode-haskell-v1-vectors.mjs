#!/usr/bin/env node

/**
 * Emits the script-decode Haskell vectors: Cardano's own verdict on every
 * PlutusV3 script of the step golden vectors and on seeded one-bit flips of
 * the small ones, written as the record the step machine must reach.
 *
 * The verdicts come from `scripts/haskell/ScriptDecodeProbe.hs`
 * (`deserialiseScript` and `mkTermToEvaluate` at protocol version 11), whose
 * built binary is named by `MIDGARD_SCRIPT_DECODE_PROBE`. A refusal becomes
 * a class code and the bit offset, from the start of the script bytes, of the
 * position Cardano's Flat decoder stopped at; a decoded script becomes its
 * program version and phase-2 class. A wrapper that is not one CBOR byte
 * string is class 1 at offset 0, and bytes after it are class 2 at the end of
 * the byte string. `tests/script-decode.haskell.test.ts`
 * compares each with the record of `scriptDecodeRecordOfScript`.
 *
 * usage: MIDGARD_SCRIPT_DECODE_PROBE=<binary> \
 *   node scripts/generate-script-decode-haskell-v1-vectors.mjs [--check]
 */

import { execFileSync } from "node:child_process";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  goldenChannelEmitter,
  parseGoldenChannelArguments,
} from "@al-ft/midgard-core/scripts/golden-channel.mjs";

import { scriptDecodeStepScripts } from "../tests/fixtures/script-decode-step-v1.vectors.mjs";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const packageRoot = resolve(scriptDirectory, "..");
const repositoryRoot = resolve(packageRoot, "../..");
const vectorsPath = join(
  packageRoot,
  "tests/fixtures/script-decode-haskell-v1.vectors.json",
);
const probeSource = join(packageRoot, "scripts/haskell/ScriptDecodeProbe.hs");

const usage =
  "usage: MIDGARD_SCRIPT_DECODE_PROBE=<binary> node scripts/generate-script-decode-haskell-v1-vectors.mjs [--check]";
const { checkOnly } = parseGoldenChannelArguments(usage);
const writeOrCheck = goldenChannelEmitter({ repositoryRoot, checkOnly });

const probe = process.env.MIDGARD_SCRIPT_DECODE_PROBE;
if (probe === undefined || probe.length === 0) {
  throw new Error(
    `MIDGARD_SCRIPT_DECODE_PROBE is not set; build it from ${probeSource}\n${usage}`,
  );
}

const PLUTUS_V3 = 3;

/** Scripts up to this many bytes also get seeded one-bit flips. */
const FLIP_SCRIPT_BYTES = 64;

/** One-bit flips per small script. */
const FLIPS_PER_SCRIPT = 12;

/** A deterministic 32-bit xorshift stream seeded by `seed`. */
const xorshift = (seed) => {
  let state = seed >>> 0 || 1;
  return () => {
    state ^= state << 13;
    state >>>= 0;
    state ^= state >>> 17;
    state ^= state << 5;
    state >>>= 0;
    return state;
  };
};

const seedOf = (label) =>
  [...label].reduce(
    (hash, character) => Math.imul(hash ^ character.charCodeAt(0), 16777619),
    2166136261,
  );

/** `script` with bit `bit` (most significant first) flipped. */
const flipScriptBit = (script, bit) => {
  const flipped = Buffer.from(script);
  flipped[bit >> 3] ^= 0x80 >> (bit & 7);
  return flipped;
};

/** The rows: every PlutusV3 golden script, and flips of the small ones. */
const corpus = () => {
  const rows = [];
  for (const [label, language, script] of scriptDecodeStepScripts()) {
    if (language !== PLUTUS_V3) continue;
    rows.push({ label, script });
    if (script.length > FLIP_SCRIPT_BYTES) continue;
    const next = xorshift(seedOf(label));
    const flips = new Set();
    while (flips.size < Math.min(FLIPS_PER_SCRIPT, 8 * script.length)) {
      flips.add(next() % (8 * script.length));
    }
    for (const flipBit of [...flips].sort((a, b) => a - b)) {
      rows.push({ label, flipBit, script: flipScriptBit(script, flipBit) });
    }
  }
  return rows;
};

/** The length of the CBOR byte-string head of `script` and its Flat bytes. */
const cborHead = (script) => {
  const first = script[0];
  if (first >= 0x40 && first < 0x58) return [1, first - 0x40];
  if (first === 0x58) return [2, script[1]];
  if (first === 0x59) return [3, script.readUInt16BE(1)];
  return undefined;
};

/**
 * The bit offset, from the start of the script, of the Flat decoder's state
 * in a `DecodeException`: its environment `(end, S {currPtr, usedBits})`.
 */
const flatOffset = (script, reason) => {
  const match =
    /\((0x[0-9a-f]+),S \{currPtr = (0x[0-9a-f]+), usedBits = (\d+)\}\)/u.exec(
      reason,
    );
  if (match === null) return undefined;
  const [headLength, flatLength] = cborHead(script);
  const left = Number(BigInt(match[1]) - BigInt(match[2]));
  return 8 * headLength + 8 * (flatLength - left) + Number(match[3]);
};

/** The refusal class of a Flat decoding failure, by its message. */
const FLAT_CLASSES = [
  [/^NotEnoughSpace/u, 3],
  [/^TooMuchSpace/u, 4],
  [/usedBits \/= 0/u, 4],
  [/Unknown term constructor tag/u, 6],
  [/is not allowed before version/u, 7],
  [/Failed to decode builtin tag/u, 8],
  [/is not available in language/u, 8],
  [/constr with \d+ fields exceeds the maximum/u, 9],
  [/Unexpected extra byte in unsigned integer/u, 10],
  [/Failed to decode a universe/u, 12],
  [/A non-star type can't have a value to decode/u, 12],
  [/exceeds the maximum type header size/u, 13],
  [/Flat decoding is not supported for objects of type bls/u, 14],
  [/invalid UTF-8/u, 15],
  [/DeserialiseFailure/u, 16],
  [/Value Flat decoder/u, 17],
  [/Invalid Value key/u, 17],
  [/Quantity out of signed \d+-bit integer bounds/u, 17],
];

/** The three Flat naturals a program starts with. */
const versionOf = (flat) => {
  const out = [];
  let position = 0;
  for (let index = 0; index < 3; index += 1) {
    let value = 0n;
    let shift = 0n;
    for (;;) {
      const group = flat[position];
      position += 1;
      value += BigInt(group & 0x7f) << shift;
      shift += 7n;
      if (group < 0x80) break;
    }
    out.push(value);
  }
  return out;
};

const PHASE2_CLASSES = { phase2ok: 0, CodecError: 5, DeBruijnError: 11 };

/** The record Cardano's verdict on `script` stands for. */
const recordOf = (script, verdict) => {
  const [kind, ...rest] = verdict;
  if (kind === "ok") {
    const [flatHex, phase2] = rest;
    const [major, minor, patch] = versionOf(Buffer.from(flatHex, "hex"));
    const available = major === 1n && minor <= 1n && patch === 0n;
    const phase2Name = phase2 === "phase2ok" ? phase2 : phase2.split(" ")[1];
    const phase2Class = PHASE2_CLASSES[phase2Name];
    if (phase2Class === undefined) throw new Error(`unknown ${phase2}`);
    return {
      k: "decoded",
      programVersion: available ? Number(minor) : -1,
      phase2Class,
    };
  }
  const [error] = rest;
  if (error.startsWith("RemainderError ")) {
    const [headLength, flatLength] = cborHead(script);
    return { k: "refused", class: 2, bitOffset: 8 * (headLength + flatLength) };
  }
  const cbor = /dfOffset = (\d+), dfReason = (.*)\}\)$/u.exec(error);
  if (cbor === null) throw new Error(`unparsed error ${error}`);
  const [, , reason] = cbor;
  // A wrapper that is not one CBOR byte string has no Flat position.
  if (reason === "ExpectedBytes" || reason === "EndOfInput") {
    return { k: "refused", class: 1, bitOffset: 0 };
  }
  const other = /^OtherReason "(.*)"$/u.exec(reason);
  if (other === null) throw new Error(`unparsed reason ${reason}`);
  const message = other[1];
  const entry = FLAT_CLASSES.find(([pattern]) => pattern.test(message));
  const bitOffset = flatOffset(script, message);
  if (entry === undefined || bitOffset === undefined) {
    throw new Error(`unclassified reason ${message}`);
  }
  return { k: "refused", class: entry[1], bitOffset };
};

const rows = corpus();
const input = rows
  .map((row, index) => `${index.toString()}\t${row.script.toString("hex")}\n`)
  .join("");
const verdicts = new Map(
  execFileSync(probe, [], { input, maxBuffer: 1 << 28 })
    .toString()
    .trim()
    .split("\n")
    .map((line) => {
      const [index, ...verdict] = line.split("\t");
      return [Number(index), verdict];
    }),
);

const vectors = rows.map(({ label, flipBit, script }, index) => {
  const verdict = verdicts.get(index);
  if (verdict === undefined) throw new Error(`no verdict for ${label}`);
  return {
    label,
    ...(flipBit === undefined ? {} : { flipBit }),
    scriptBytes: script.length,
    record: recordOf(script, verdict),
  };
});

writeOrCheck(vectorsPath, `${JSON.stringify({ rows: vectors }, null, 2)}\n`);
