#!/usr/bin/env node

/**
 * The `cek-data-canonical-corpus-v1` cross-language golden channel: the
 * canonical Data corpus (`tests/fixtures/cek-data-canonical-corpus-v1.vectors.mjs`)
 * rendered as an Aiken module.
 *
 * Every accepted span is bound to the Data summary the TypeScript traverse
 * twin computes for it (built `dist/`); the generated Aiken tests run the
 * honest driver (`midgard/validation_machine_tests/data_traverse_driver`) over
 * `cek_data_traverse_v1` and require that same summary, both for the span on
 * its own and for the span embedded at a nonzero offset of a larger blob.
 * Every other span must stop with `unreadable_v1` at the named stage. The
 * one-shot `canonical_plutus_data_v1` predicate and its resumable reader
 * `canonical_plutus_data_stream_v1` (whole, and resumed one node per call over
 * 68-byte chunks with one revealed frame) must give the same verdict on every
 * span. The linear summariser `data_summary_walk_v1` must give the accepted
 * span's summary, on its own and embedded, and `Unreadable` for every other
 * span. The serialise loop's reader `data_serialise_walk_v1` (whole, and in
 * 68-byte windows of one step each) must give the accepted span's summary and
 * `None` for every other span. And the corpus is held to its definition: a
 * span is accepted exactly when the ledger's permissive decoder
 * `cbor_deserialise.deserialise` decodes it and `serialiseData` gives the same
 * bytes back.
 *
 * The channel also renders the canonical CBOR head vectors
 * (`tests/fixtures/canonical-cbor-head-v1.vectors.mjs`) as
 * `onchain/aiken/lib/midgard/canonical-cbor-head-v1-golden.test.ak`: every
 * canonical head reader (`canonical_cbor_scan_v1.head_at_v1` and the copy in
 * `native_script_scan_v1`) must give each vector's head under its own major
 * type and refuse it under every other, `data_summary_walk_v1.head` must
 * give each vector's head too, except that it opens an indefinite head as
 * argument -1, and `cbor_deserialise.deserialise` must give each vector's
 * permissive result where it has one. The walk's head is not run at a
 * negative offset: there its byte read fails the script rather than refuse.
 *
 * usage: node scripts/generate-cek-data-canonical-corpus-v1-goldens.mjs [--check]
 */

import { basename, dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { buildMidgardCekDataTraverseTrace } from "../dist/index.js";
import {
  reasons,
  vectors as headVectors,
} from "../tests/fixtures/canonical-cbor-head-v1.vectors.mjs";
import { vectors } from "../tests/fixtures/cek-data-canonical-corpus-v1.vectors.mjs";
import {
  aikenBytes,
  formatAikenSource,
  goldenChannelEmitter,
  hex,
  parseGoldenChannelArguments,
} from "./golden-channel.mjs";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const packageRoot = resolve(scriptDirectory, "..");
const repositoryRoot = resolve(packageRoot, "../..");
const generatedAikenPath = join(
  repositoryRoot,
  "onchain/aiken/lib/midgard/cek-data-canonical-corpus-v1-golden.test.ak",
);
const aikenFileName = basename(generatedAikenPath);
const generatedHeadAikenPath = join(
  repositoryRoot,
  "onchain/aiken/lib/midgard/canonical-cbor-head-v1-golden.test.ak",
);

const { checkOnly } = parseGoldenChannelArguments(
  "usage: node scripts/generate-cek-data-canonical-corpus-v1-goldens.mjs [--check]",
);
const writeOrCheck = goldenChannelEmitter({ repositoryRoot, checkOnly });

const fail = (message) => {
  throw new Error(`cek-data-canonical-corpus-v1: ${message}`);
};

const names = new Set();
for (const vector of vectors) {
  if (!/^[a-z0-9_]+$/u.test(vector.name) || names.has(vector.name)) {
    fail(`vector name ${vector.name} is not a fresh snake_case name`);
  }
  names.add(vector.name);
  if (!/^(?:[0-9a-f]{2})+$/u.test(vector.hex)) {
    fail(`vector ${vector.name} is not lowercase hex`);
  }
}

const expectedOf = (vector) => {
  if (vector.verdict === "unreadable") {
    if (!Number.isInteger(vector.stage)) {
      fail(`unreadable vector ${vector.name} names no stage`);
    }
    return `data_traverse_driver.DriveUnreadable { stage: ${vector.stage} }`;
  }
  if (vector.verdict !== "accepted") {
    fail(`vector ${vector.name} has verdict ${vector.verdict}`);
  }
  const trace = buildMidgardCekDataTraverseTrace({
    sourceStart: 0,
    source: Buffer.from(vector.hex, "hex"),
  });
  const summary = trace.terminal.result;
  if (summary === null || summary === undefined) {
    fail(`the TypeScript twin gives accepted vector ${vector.name} no summary`);
  }
  return [
    "data_traverse_driver.DriveAccepted {",
    "summary: cek_data_v1.DataSummaryV1 {",
    `root: ${aikenBytes(hex(summary.root))},`,
    `cbor_length: ${summary.cborLength},`,
    `memory: ${summary.memory},`,
    "},",
    "}",
  ].join(" ");
};

const walkOutcomeOf = (vector) => {
  if (vector.verdict !== "accepted") {
    return "data_summary_walk_v1.Unreadable";
  }
  const trace = buildMidgardCekDataTraverseTrace({
    sourceStart: 0,
    source: Buffer.from(vector.hex, "hex"),
  });
  const summary = trace.terminal.result;
  return [
    "data_summary_walk_v1.Summarised {",
    "summary: cek_data_v1.DataSummaryV1 {",
    `root: ${aikenBytes(hex(summary.root))},`,
    `cbor_length: ${summary.cborLength},`,
    `memory: ${summary.memory},`,
    "},",
    "}",
  ].join(" ");
};

const serialiseOutcomeOf = (vector) => {
  if (vector.verdict !== "accepted") {
    return "None";
  }
  const trace = buildMidgardCekDataTraverseTrace({
    sourceStart: 0,
    source: Buffer.from(vector.hex, "hex"),
  });
  const summary = trace.terminal.result;
  return [
    "Some(cek_data_v1.DataSummaryV1 {",
    `root: ${aikenBytes(hex(summary.root))},`,
    `cbor_length: ${summary.cborLength},`,
    `memory: ${summary.memory},`,
    "})",
  ].join(" ");
};

const renderTest = (vector) => `
test corpus_${vector.name}() {
  let source = ${aikenBytes(vector.hex)}
  let expected = ${expectedOf(vector)}
  let walked = ${walkOutcomeOf(vector)}
  let serialised = ${serialiseOutcomeOf(vector)}
  and {
    data_summary_walk_v1.resume(
      data_summary_walk_v1.start(0),
      source,
      bytearray.length(source),
      [],
      data_summary_walk_v1.empty_stack_root,
      1_000_000,
    ) == walked,
    data_summary_walk_v1.resume(
      data_summary_walk_v1.start(1),
      bytearray.concat(#"5a", bytearray.concat(source, #"a5")),
      1 + bytearray.length(source),
      [],
      data_summary_walk_v1.empty_stack_root,
      1_000_000,
    ) == walked,
    data_traverse_driver.drive_source_v1(source) == expected,
    data_traverse_driver.drive_v1(
      bytearray.concat(#"5a", bytearray.concat(source, #"a5")),
      1,
      bytearray.length(source),
    ) == expected,
    canonical_plutus_data_v1.is_canonical_plutus_data_v1(source) == ${vector.verdict === "accepted" ? "True" : "False"},
    data_stream_driver.stream_verdict_v1(source, 0, bytearray.length(source), 4095, 1_000_000) == ${vector.verdict === "accepted" ? "True" : "False"},
    data_stream_driver.stream_verdict_v1(
      bytearray.concat(#"5a", bytearray.concat(source, #"a5")),
      1,
      bytearray.length(source),
      68,
      1,
    ) == ${vector.verdict === "accepted" ? "True" : "False"},
    data_serialise_driver.whole_v1(source) == serialised,
    data_serialise_driver.windows_v1(source, 68, 1) == serialised,
    round_trips(source) == ${vector.verdict === "accepted" ? "True" : "False"},
  }
}
`;

const source = `//// GENERATED by demo/midgard-core/scripts/generate-cek-data-canonical-corpus-v1-goldens.mjs
//// from demo/midgard-core/tests/fixtures/cek-data-canonical-corpus-v1.vectors.mjs.
//// Do not edit; run \`pnpm --dir demo/midgard-core run fixtures:cek-data-canonical-corpus-v1:sync\`.

use aiken/cbor
use aiken/primitive/bytearray
use midgard/canonical_plutus_data_v1
use midgard/cbor_deserialise
use midgard/cek_data_v1
use midgard/data_summary_walk_v1
use midgard/validation_machine_tests/data_serialise_driver
use midgard/validation_machine_tests/data_stream_driver
use midgard/validation_machine_tests/data_traverse_driver

/// Whether the ledger's permissive decoder reads \`source\` and \`serialiseData\`
/// writes the same bytes back: the corpus's definition of canonical Data.
fn round_trips(source: ByteArray) -> Bool {
  when cbor_deserialise.deserialise(source) is {
    Some(data) -> cbor.serialise(data) == source
    None -> False
  }
}
${vectors.map(renderTest).join("")}`;

writeOrCheck(
  generatedAikenPath,
  formatAikenSource({
    source,
    fileName: aikenFileName,
    repositoryRoot,
    tmpPrefix: "cek-data-canonical-corpus-v1-",
  }),
);

// ## The canonical CBOR head vectors

const reasonClasses = new Set(Object.values(reasons));
const headNames = new Set();
for (const vector of headVectors) {
  if (!/^[a-z0-9_]+$/u.test(vector.name) || headNames.has(vector.name)) {
    fail(`head vector ${vector.name} is not a fresh snake_case name`);
  }
  headNames.add(vector.name);
  if (!/^(?:[0-9a-f]{2})*$/u.test(vector.hex)) {
    fail(`head vector ${vector.name} is not lowercase hex`);
  }
  if (!Number.isInteger(vector.offset)) {
    fail(`head vector ${vector.name} has no integer offset`);
  }
  const { canonical, permissive } = vector;
  if (
    canonical.verdict === "accept"
      ? !(
          Number.isInteger(canonical.major) &&
          canonical.major >= 0 &&
          canonical.major <= 7 &&
          typeof canonical.value === "bigint" &&
          Number.isInteger(canonical.next)
        )
      : canonical.verdict !== "refuse" || !reasonClasses.has(canonical.reason)
  ) {
    fail(`head vector ${vector.name} has no well-formed canonical result`);
  }
  if (
    permissive !== undefined &&
    permissive.verdict !== "refuse" &&
    !(
      permissive.verdict === "accept" &&
      /^(?:[0-9a-f]{2})+$/u.test(permissive.serialised)
    )
  ) {
    fail(`head vector ${vector.name} has no well-formed permissive result`);
  }
}

const expectedHeadOf = ({ canonical }) =>
  canonical.verdict === "accept"
    ? `Some((${canonical.major}, ${canonical.value}, ${canonical.next}))`
    : "None";

/** What the data summary walk's head gives: the canonical head, or an
 * indefinite head as argument -1. */
const expectedWalkHeadOf = (vector) => {
  const { canonical, hex, offset } = vector;
  if (
    canonical.verdict === "refuse" &&
    canonical.reason === reasons.indefinite
  ) {
    const first = Number.parseInt(hex.slice(2 * offset, 2 * offset + 2), 16);
    return `Some((${first >> 5}, -1, ${offset + 1}))`;
  }
  return expectedHeadOf(vector);
};

const renderHeadTest = (vector) => {
  const { canonical, permissive } = vector;
  const literal = aikenBytes(vector.hex);
  const checks = [
    `reads(${literal}, ${vector.offset}, ${expectedHeadOf(vector)})`,
    ...(vector.offset < 0
      ? []
      : [`walks(${literal}, ${vector.offset}, ${expectedWalkHeadOf(vector)})`]),
    ...(permissive === undefined
      ? []
      : [
          `deserialises(${literal}, ${
            permissive.verdict === "accept"
              ? `Some(${aikenBytes(permissive.serialised)})`
              : "None"
          })`,
        ]),
  ];
  const body =
    checks.length === 1
      ? checks[0]
      : `and {\n${checks.map((check) => `${check},`).join("\n")}\n}`;
  const comment =
    canonical.verdict === "refuse"
      ? `// Canonical readers refuse it: ${canonical.reason}.\n`
      : "";
  return `\n${comment}test head_${vector.name}() {\n${body}\n}\n`;
};

const headSource = `//// GENERATED by demo/midgard-core/scripts/generate-cek-data-canonical-corpus-v1-goldens.mjs
//// from demo/midgard-core/tests/fixtures/canonical-cbor-head-v1.vectors.mjs.
//// Do not edit; run \`pnpm --dir demo/midgard-core run fixtures:cek-data-canonical-corpus-v1:sync\`.
////
//// Every canonical CBOR head reader gives each vector's head under the
//// vector's major type and refuses it under every other major type. The data
//// summary walk's head gives the same head, or argument -1 for an indefinite
//// one, at every offset that is not negative. Where a vector has a permissive
//// result, \`cbor_deserialise.deserialise\` gives it.

use aiken/cbor
use aiken/collection/list
use aiken/option
use aiken/primitive/bytearray
use midgard/canonical_cbor_scan_v1
use midgard/cbor_deserialise
use midgard/data_summary_walk_v1
use midgard/native_script_scan_v1

/// Whether every canonical head reader, asked for each major type in turn,
/// gives \`expected\` (major type, argument, next offset) for its own major type
/// and \`None\` for the others.
fn reads(
  source: ByteArray,
  offset: Int,
  expected: Option<(Int, Int, Int)>,
) -> Bool {
  list.all(
    [0, 1, 2, 3, 4, 5, 6, 7],
    fn(major) {
      let wanted =
        when expected is {
          Some((expected_major, value, next)) ->
            if expected_major == major {
              Some((value, next))
            } else {
              None
            }
          None -> None
        }
      and {
        option.map(
          canonical_cbor_scan_v1.head_at_v1(source, offset, major),
          fn(head) { (head.value, head.next_offset) },
        ) == wanted,
        option.map(
          native_script_scan_v1.canonical_head(source, offset, major),
          fn(head) { (head.value, head.next_offset) },
        ) == wanted,
      }
    },
  )
}

/// Whether the data summary walk's head, reading to the end of \`source\`,
/// gives \`expected\` (major type, argument or -1 when indefinite, next offset).
fn walks(
  source: ByteArray,
  offset: Int,
  expected: Option<(Int, Int, Int)>,
) -> Bool {
  data_summary_walk_v1.head(source, offset, bytearray.length(source)) == expected
}

/// Whether the permissive decoder gives \`expected\`: what \`serialiseData\`
/// writes for the decoded value, or \`None\` where it refuses.
fn deserialises(source: ByteArray, expected: Option<ByteArray>) -> Bool {
  option.map(cbor_deserialise.deserialise(source), cbor.serialise) == expected
}
${headVectors.map(renderHeadTest).join("")}`;

writeOrCheck(
  generatedHeadAikenPath,
  formatAikenSource({
    source: headSource,
    fileName: basename(generatedHeadAikenPath),
    repositoryRoot,
    tmpPrefix: "canonical-cbor-head-v1-",
  }),
);
