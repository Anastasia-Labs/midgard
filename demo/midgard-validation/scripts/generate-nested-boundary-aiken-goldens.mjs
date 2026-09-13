#!/usr/bin/env node

/**
 * Rebinds the Aiken constants produced by the three genuine signed-Cardano
 * *nested* boundary suites:
 *
 *   * the maximum balanced constructor/list/map inline datum
 *     (`tests/nested-data-boundary.test.ts`);
 *   * the maximum balanced nested redeemer `Data`
 *     (`tests/nested-redeemer-data-boundary.test.ts`);
 *   * the exact 5,000-byte nested `Value`
 *     (`tests/nested-value-boundary.test.ts`).
 *
 * All three were hand-mirrored families: each suite printed its terminal vector
 * under `MIDGARD_PRINT_AIKEN_VECTOR=1` and a human retyped the controls, frame,
 * root, lengths and frontier hashes into
 * `onchain/aiken/lib/midgard/cek-data-traverse.max-cardano.test.ak` and
 * `onchain/aiken/lib/midgard/ledger-output-value-v1.test.ak`. Worse, the print
 * mode *also* switched the suites' own terminal comparison off, so the one
 * environment variable that regenerated the Aiken side simultaneously removed
 * the oracle that made the regenerated values trustworthy.
 *
 * This generator closes both halves. The suites now assert unconditionally and
 * publish their vector on the `MIDGARD_WRITE_AIKEN_VECTOR` machine channel
 * (`tests/helpers/aiken-vector-channel.ts`) *after* those assertions, and this
 * script owns the mapping from vector to Aiken constant plus the `--check`
 * contract. A vector this generator can see is a vector its suite has already
 * accepted.
 *
 * **Why it runs the suites instead of recomputing the boundary.** The boundary
 * is the result of an emulator-driven search that only exists inside those
 * suites; there is no shorter producer. The three take about forty seconds.
 *
 * usage: node scripts/generate-nested-boundary-aiken-goldens.mjs [--check]
 */

import { spawnSync } from "node:child_process";
import { mkdtempSync, readFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  bytes,
  goldenChannelEmitter,
  parseGoldenChannelArguments,
  rebindAikenConstants,
} from "@al-ft/midgard-core/scripts/golden-channel.mjs";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const packageRoot = resolve(scriptDirectory, "..");
const repositoryRoot = resolve(packageRoot, "../..");

const { checkOnly } = parseGoldenChannelArguments(
  "usage: node scripts/generate-nested-boundary-aiken-goldens.mjs [--check]",
);
const writeOrCheck = goldenChannelEmitter({ repositoryRoot, checkOnly });

const PRODUCING_SUITES = [
  "tests/nested-data-boundary.test.ts",
  "tests/nested-redeemer-data-boundary.test.ts",
  "tests/nested-value-boundary.test.ts",
];

const VECTOR_NAMES = [
  "nested-data-boundary-v1",
  "nested-redeemer-data-boundary-v1",
  "nested-value-boundary-v1",
];

const runProducingSuites = (vectorDirectory) => {
  const result = spawnSync(
    "node",
    [
      resolve(packageRoot, "node_modules/vitest/vitest.mjs"),
      "run",
      ...PRODUCING_SUITES,
    ],
    {
      cwd: packageRoot,
      encoding: "utf8",
      env: { ...process.env, MIDGARD_WRITE_AIKEN_VECTOR: vectorDirectory },
      maxBuffer: 64 * 1024 * 1024,
      stdio: ["ignore", "pipe", "inherit"],
    },
  );
  if (result.error !== undefined) {
    throw result.error;
  }
  if (result.status !== 0) {
    process.stdout.write(result.stdout ?? "");
    throw new Error(
      "the nested boundary suites did not pass, so their vectors are not usable",
    );
  }
};

const vectorDirectory = mkdtempSync(
  join(tmpdir(), "midgard-nested-boundary-aiken-vectors-"),
);
let vectors;
try {
  runProducingSuites(vectorDirectory);
  vectors = Object.fromEntries(
    VECTOR_NAMES.map((name) => [
      name,
      JSON.parse(readFileSync(join(vectorDirectory, `${name}.json`), "utf8")),
    ]),
  );
} finally {
  rmSync(vectorDirectory, { force: true, recursive: true });
}

const nestedData = vectors["nested-data-boundary-v1"];
const nestedRedeemer = vectors["nested-redeemer-data-boundary-v1"];
const nestedValue = vectors["nested-value-boundary-v1"];

/**
 * A decimal count published as a string (the suites carry `cborLength` and
 * `memory` as strings because they are `bigint`s off-chain) turned into the
 * integer literal Aiken spells.
 */
const count = (value, name) => {
  const parsed = typeof value === "number" ? value : Number(value);
  if (!Number.isSafeInteger(parsed)) {
    throw new Error(
      `published ${name} is not a safe integer: ${String(value)}`,
    );
  }
  return parsed;
};

/** The constants of one nested-`Data` traversal terminal, keyed by its prefix. */
const traverseTerminalConstants = (prefix, vector) => ({
  [`${prefix}_pre_control`]: bytes(vector.terminalPreControlCborHex),
  [`${prefix}_post_control`]: bytes(vector.terminalPostControlCborHex),
  [`${prefix}_terminal_frame`]: bytes(vector.terminalFrameCborHex),
  [`${prefix}_terminal_frame_child`]: bytes(
    vector.terminalFrameSequenceRootHex,
  ),
  [`${prefix}_root`]: bytes(vector.terminalSummary.rootHex),
  [`${prefix}_cbor_length`]: count(
    vector.terminalSummary.cborLength,
    `${prefix} cbor length`,
  ),
  [`${prefix}_memory`]: count(
    vector.terminalSummary.memory,
    `${prefix} memory`,
  ),
});

/**
 * The nested-`Value` twin builds its asset frontier from five peaks whose
 * *heights* are the set bits of the asset count and therefore structural, while
 * the hashes are measured. Only the hashes are generated, so a boundary that
 * moved to a different asset count would leave stale heights behind — which the
 * check below turns into a generator failure naming the file to repair, rather
 * than an unexplained red Aiken row.
 */
const VALUE_FRONTIER_PEAK_HEIGHTS = [3, 4, 5, 9, 10];

const valueFrontierConstants = (vector) => {
  const heights = vector.valueFrontier.map((peak) => peak.height);
  if (heights.join(",") !== VALUE_FRONTIER_PEAK_HEIGHTS.join(",")) {
    throw new Error(
      `the maximum nested Value frontier now has peak heights [${heights.join(", ")}], not [${VALUE_FRONTIER_PEAK_HEIGHTS.join(", ")}]: update the heights in onchain/aiken/lib/midgard/ledger-output-value-v1.test.ak and VALUE_FRONTIER_PEAK_HEIGHTS here`,
    );
  }
  return Object.fromEntries(
    vector.valueFrontier.map((peak, index) => [
      `typescript_maximum_value_asset_peak_hash_${String(index)}`,
      bytes(peak.hashHex),
    ]),
  );
};

const AIKEN_FAMILIES = [
  {
    aiken: "onchain/aiken/lib/midgard/cek-data-traverse.max-cardano.test.ak",
    constants: {
      ...traverseTerminalConstants("maximum_cardano_nested_data", nestedData),
      ...traverseTerminalConstants(
        "maximum_cardano_nested_redeemer",
        nestedRedeemer,
      ),
    },
  },
  {
    aiken: "onchain/aiken/lib/midgard/ledger-output-value-v1.test.ak",
    constants: {
      typescript_maximum_value_pre_terminal_control: bytes(
        nestedValue.preTerminalControlCborHex,
      ),
      typescript_maximum_value_terminal_control: bytes(
        nestedValue.terminalControlCborHex,
      ),
      typescript_maximum_value_root: bytes(nestedValue.terminalResult.rootHex),
      typescript_maximum_value_cbor_length: count(
        nestedValue.terminalResult.cborLength,
        "maximum value cbor length",
      ),
      typescript_maximum_value_memory: count(
        nestedValue.terminalResult.memory,
        "maximum value memory",
      ),
      typescript_maximum_value_asset_count: count(
        nestedValue.valueAssetCount,
        "maximum value asset count",
      ),
      typescript_maximum_value_lovelace: count(
        nestedValue.lovelace,
        "maximum value lovelace",
      ),
      ...valueFrontierConstants(nestedValue),
    },
  },
];

for (const family of AIKEN_FAMILIES) {
  const aikenPath = join(repositoryRoot, family.aiken);
  writeOrCheck(
    aikenPath,
    rebindAikenConstants({
      source: readFileSync(aikenPath, "utf8"),
      constants: family.constants,
    }),
  );
}
