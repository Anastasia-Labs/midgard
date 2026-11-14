#!/usr/bin/env node

/**
 * Writes `tests/fixtures/validation-trace-transcript-v1.json`, the validation
 * trace transcript: for every emission-order row the trace builder can trace,
 * the trace's verdict, its phase runs, its trace root, a digest of every work
 * and auxiliary witness, its context digest and its ledger operations
 * (`tests/emission-order.transcript.ts`). No other golden pins the whole trace,
 * so a change to the builder that moves any byte of any row's trace turns
 * `--check` red, and the row and phase that moved are in the diff.
 *
 * The rows, their transactions and their pre-states exist only as vitest
 * modules, so the producer is the suite that already builds every row's
 * trace: `tests/emission-order.aiken-rows.test.ts`. It publishes the
 * transcript on the `MIDGARD_WRITE_AIKEN_VECTOR` channel
 * (`tests/helpers/aiken-vector-channel.ts`) only after every row's verdict and
 * the on-chain vectors check, so a transcript this script sees is one that
 * suite accepted.
 *
 * usage: node scripts/generate-validation-trace-transcript-v1-fixture.mjs [--check]
 */

import { spawnSync } from "node:child_process";
import { mkdtempSync, readFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  goldenChannelEmitter,
  parseGoldenChannelArguments,
} from "@al-ft/midgard-core/scripts/golden-channel.mjs";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const packageRoot = resolve(scriptDirectory, "..");
const repositoryRoot = resolve(packageRoot, "../..");

const { checkOnly } = parseGoldenChannelArguments(
  "usage: node scripts/generate-validation-trace-transcript-v1-fixture.mjs [--check]",
);
const writeOrCheck = goldenChannelEmitter({ repositoryRoot, checkOnly });

const PRODUCING_SUITE = "tests/emission-order.aiken-rows.test.ts";
const FIXTURE = "tests/fixtures/validation-trace-transcript-v1.json";

const vectorDirectory = mkdtempSync(
  join(tmpdir(), "midgard-validation-trace-transcript-"),
);
let transcript;
try {
  const result = spawnSync(
    "node",
    [
      resolve(packageRoot, "node_modules/vitest/vitest.mjs"),
      "run",
      PRODUCING_SUITE,
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
      `${PRODUCING_SUITE} did not pass, so its transcript is not usable`,
    );
  }
  transcript = readFileSync(
    join(vectorDirectory, "validation-trace-transcript-v1.json"),
    "utf8",
  );
} finally {
  rmSync(vectorDirectory, { force: true, recursive: true });
}

writeOrCheck(join(packageRoot, FIXTURE), transcript);
