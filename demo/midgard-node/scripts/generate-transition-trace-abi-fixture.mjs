#!/usr/bin/env node

// Emits the transition-trace ABI golden and the Aiken golden that consumes the
// same bytes, so `tests/fixtures/transition-trace-abi.json` is a contract
// between two independent implementations rather than a copy of the current
// TypeScript output.
//
// Channels:
//   * JSON golden <-> SDK encoder: asserted by
//     tests/sdk-abi-fixtures.test.ts ("matches transition trace golden ABI
//     fixture files"), which never writes the golden it asserts against.
//   * JSON golden <-> Aiken golden: asserted by `--check` here (pure file
//     transform, no build required), wired into CI.
//   * Aiken golden <-> Aiken types: asserted by `aiken check`, which decodes
//     every fixture into its on-chain type and re-serialises it byte for byte.
//
// Usage:
//   node scripts/generate-transition-trace-abi-fixture.mjs          # regenerate both
//   node scripts/generate-transition-trace-abi-fixture.mjs --check  # verify the Aiken golden is in sync

import { spawnSync } from "node:child_process";
import {
  existsSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const packageRoot = resolve(scriptDirectory, "..");
const repositoryRoot = resolve(packageRoot, "../..");
const goldenJsonPath = join(
  packageRoot,
  "tests/fixtures/transition-trace-abi.json",
);
const generatedAikenPath = join(
  repositoryRoot,
  "onchain/aiken/validators/fraud-proofs/transition-trace/abi-v1-golden.test.ak",
);
const testFile = "tests/sdk-abi-fixtures.test.ts";
const testName = "matches transition trace golden ABI fixture files";

const checkOnly = process.argv.includes("--check");
if (
  process.argv.slice(2).some((argument) => argument !== "--check") ||
  process.argv.slice(2).filter((argument) => argument === "--check").length > 1
) {
  throw new Error(
    "usage: node scripts/generate-transition-trace-abi-fixture.mjs [--check]",
  );
}

const fail = (message) => {
  throw new Error(`transition-trace ABI fixture generation failed: ${message}`);
};

// Every schema the golden carries must be listed here: an unmapped schema is a
// hard failure, so a new fixture family cannot silently escape the Aiken side
// of the contract.
const AIKEN_TYPE_BY_SCHEMA = {
  HeaderV1: "ledger_state.HeaderV1",
  ForcedInclusionTxV1: "ledger_state.ForcedInclusionTxV1",
  EventKey: "ledger_state.EventKey",
  EventToStepValue: "ledger_state.EventToStepValue",
  TransitionStep: "ledger_state.TransitionStep",
  TransitionFault: "proof.TransitionFault",
  TransitionFaultProof: "proof.TransitionFaultProof",
  TransitionTraceRouteSpendRedeemer: "route_v1.SpendRedeemer",
  TransitionTraceFinalSpendRedeemer: "final_v1.SpendRedeemer",
  // The DA payload body is an off-chain envelope with no on-chain counterpart;
  // no Aiken type can consume it.
  DaPayloadBody: null,
};

// Minimal CBOR walk used only to answer one question: does this fixture carry a
// map? lucid emits Plutus Data maps with indefinite-length headers while Aiken
// re-serialises them with definite headers, so byte equality after an Aiken
// round trip holds for every map-free fixture and for no map-carrying one. The
// walk is structural (never scans raw bytes) so a 0xbf byte inside a byte
// string cannot be mistaken for a map header.
const containsMap = (bytes) => {
  let offset = 0;
  let sawMap = false;
  const readUnsigned = (info) => {
    if (info < 24) return info;
    const width = { 24: 1, 25: 2, 26: 4, 27: 8 }[info];
    if (width === undefined) fail("unsupported CBOR additional information");
    let value = 0n;
    for (let index = 0; index < width; index += 1) {
      value = (value << 8n) | BigInt(bytes[offset + index]);
    }
    offset += width;
    return Number(value);
  };
  const walk = () => {
    if (offset >= bytes.length) fail("truncated CBOR fixture");
    const initial = bytes[offset];
    offset += 1;
    const major = initial >> 5;
    const info = initial & 0x1f;
    if (major === 7) {
      if (info === 31) return "break";
      readUnsigned(info === 31 ? 0 : info);
      return "value";
    }
    if (info === 31) {
      // Indefinite length: byte/text strings concatenate chunks, arrays and
      // maps run until a break.
      if (major === 5) sawMap = true;
      for (;;) {
        if (walk() === "break") break;
      }
      return "value";
    }
    const length = readUnsigned(info);
    if (major === 0 || major === 1) return "value";
    if (major === 2 || major === 3) {
      offset += length;
      return "value";
    }
    if (major === 4) {
      for (let index = 0; index < length; index += 1) walk();
      return "value";
    }
    if (major === 5) {
      sawMap = true;
      for (let index = 0; index < length; index += 1) {
        walk();
        walk();
      }
      return "value";
    }
    if (major === 6) {
      walk();
      return "value";
    }
    fail("unsupported CBOR major type");
    return "value";
  };
  walk();
  if (offset !== bytes.length) fail("trailing bytes in CBOR fixture");
  return sawMap;
};

const AIKEN_MAX_LINE_WIDTH = 80;

// Reproduces the one `aiken fmt` decision this file can trigger: keep
// `<head><tail>` on one line when it fits in 80 columns, otherwise put the tail
// on its own line indented one level deeper.
const wrapBinding = (head, tail, indent) =>
  head.length + tail.length <= AIKEN_MAX_LINE_WIDTH
    ? [`${head}${tail}`]
    : [head.trimEnd(), `${indent}  ${tail}`];

// `expect Some(data) = cbor.deserialise(<name>_cbor)` under the same 80-column
// rule, one break at a time: keep it on one line, else move the call down, else
// break the call argument out too.
const deserialiseLines = (id) => {
  const single = `  expect Some(data) = cbor.deserialise(${id}_cbor)`;
  if (single.length <= AIKEN_MAX_LINE_WIDTH) return [single];
  return ["  expect Some(data) = cbor.deserialise(", `    ${id}_cbor,`, "  )"];
};

const identifier = (name) => {
  const snake = name
    .replaceAll(/[.-]/gu, "_")
    .replaceAll(/([a-z0-9])([A-Z])/gu, "$1_$2")
    .toLowerCase();
  if (!/^[a-z][a-z0-9_]*$/u.test(snake)) {
    fail(`fixture name ${name} does not map to an Aiken identifier`);
  }
  return snake;
};

const goldenBytes = readFileSync(goldenJsonPath, "utf8");
let golden;
try {
  golden = JSON.parse(goldenBytes);
} catch (error) {
  fail(`golden is invalid JSON: ${String(error)}`);
}
if (golden.version !== 1) fail("golden version must be 1");
if (golden.encoding !== "lucid-plutus-data-cbor-hex") {
  fail("golden encoding must be lucid-plutus-data-cbor-hex");
}
const entries = Object.entries(golden.fixtures ?? {});
if (entries.length === 0) fail("golden carries no fixtures");

const makeAiken = () => {
  const lines = [
    "// Generated by demo/midgard-node/scripts/generate-transition-trace-abi-fixture.mjs.",
    "// Do not edit; regenerate from demo/midgard-node/tests/fixtures/transition-trace-abi.json.",
    "//",
    "// Each fixture is the Plutus Data CBOR the TypeScript SDK emits for one",
    "// transition-trace ABI value. Decoding it into the on-chain type and",
    "// re-serialising it byte for byte makes the golden a two-implementation",
    "// contract: an encoder change on either side breaks these tests.",
    "",
    "use aiken/cbor",
    "use fraud_proofs/transition_trace/route_v1",
    "use midgard/fraud_proofs/transition_trace/final_v1",
    "use midgard/fraud_proofs/transition_trace/proof",
    "use midgard/ledger_state",
    "",
  ];
  const seen = new Set();
  let covered = 0;
  for (const [name, fixture] of entries) {
    if (typeof fixture?.schema !== "string") {
      fail(`fixture ${name} has no schema`);
    }
    if (!Object.hasOwn(AIKEN_TYPE_BY_SCHEMA, fixture.schema)) {
      fail(
        `fixture ${name} uses schema ${fixture.schema}, which has no entry in AIKEN_TYPE_BY_SCHEMA`,
      );
    }
    const aikenType = AIKEN_TYPE_BY_SCHEMA[fixture.schema];
    if (aikenType === null) continue;
    if (!/^[0-9a-f]+$/u.test(fixture.cborHex ?? "")) {
      fail(`fixture ${name} has no lowercase hexadecimal cborHex`);
    }
    if (
      Buffer.byteLength(fixture.cborHex, "hex") !== fixture.byteLength ||
      fixture.cborHex.length % 2 !== 0
    ) {
      fail(`fixture ${name} byteLength disagrees with cborHex`);
    }
    const id = identifier(name);
    if (seen.has(id)) fail(`fixture identifier ${id} is not unique`);
    seen.add(id);
    covered += 1;
    lines.push(
      // `aiken fmt` breaks a binding whose right-hand side would push the line
      // past 80 columns, and leaves it alone otherwise. Emitting that shape
      // directly keeps the generator hermetic: `--check` needs no Aiken binary
      // and stays byte-exact against `aiken fmt --check`.
      ...wrapBinding(`const ${id}_cbor = `, `#"${fixture.cborHex}"`, ""),
      "",
      `test transition_trace_abi_${id}() {`,
      ...(containsMap(Buffer.from(fixture.cborHex, "hex"))
        ? [
            // Map-carrying fixture: lucid writes indefinite-length map headers,
            // Aiken writes definite ones, so the two encoders agree on the
            // value but not on the bytes. Decoding the SDK bytes into the
            // on-chain type is still the full structural ABI check; the extra
            // round trip pins that Aiken's own re-encoding decodes back to the
            // same value.
            ...deserialiseLines(id),
            `  expect value: ${aikenType} = data`,
            "  expect Some(round_tripped) = cbor.deserialise(cbor.serialise(value))",
            `  expect recovered: ${aikenType} = round_tripped`,
            "  recovered == value",
          ]
        : [
            ...deserialiseLines(id),
            `  expect value: ${aikenType} = data`,
            `  cbor.serialise(value) == ${id}_cbor`,
          ]),
      "}",
      "",
    );
  }
  if (covered === 0) fail("no fixture maps to an Aiken type");
  return { source: `${lines.join("\n").trimEnd()}\n`, covered };
};

const writeOrCheck = (target, expected) => {
  const label = relative(repositoryRoot, target);
  if (checkOnly) {
    if (!existsSync(target)) fail(`missing generated artifact ${label}`);
    if (readFileSync(target, "utf8") !== expected) {
      fail(
        `generated artifact is stale: ${label}; run pnpm --dir demo/midgard-node run fixtures:transition-trace-abi`,
      );
    }
    return;
  }
  writeFileSync(target, expected, "utf8");
};

// Regeneration re-encodes the fixtures with the production SDK schemas by
// running the ABI test in emit mode against a scratch path, so no test run can
// ever rewrite the golden it asserts against.
if (!checkOnly) {
  const directory = mkdtempSync(join(tmpdir(), "midgard-tt-abi-emit-"));
  const emitPath = join(directory, "transition-trace-abi.json");
  try {
    const result = spawnSync(
      "npx",
      ["vitest", "run", testFile, "-t", testName],
      {
        cwd: packageRoot,
        encoding: "utf8",
        env: {
          ...process.env,
          MIDGARD_TRANSITION_TRACE_ABI_EMIT_PATH: emitPath,
        },
        stdio: ["ignore", "inherit", "inherit"],
      },
    );
    if (result.status !== 0) {
      fail(`emitting fixtures via ${testFile} failed`);
    }
    if (!existsSync(emitPath)) fail(`${testFile} emitted no fixture file`);
    const emitted = readFileSync(emitPath, "utf8");
    writeFileSync(goldenJsonPath, emitted, "utf8");
    golden = JSON.parse(emitted);
  } finally {
    rmSync(directory, { force: true, recursive: true });
  }
}

const { source, covered } = makeAiken();
writeOrCheck(generatedAikenPath, source);

if (!checkOnly) {
  process.stdout.write(
    `generated transition-trace ABI golden: ${Object.keys(golden.fixtures).length.toString()} fixtures, ${covered.toString()} consumed by Aiken\n`,
  );
}
