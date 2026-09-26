#!/usr/bin/env node

// Proves the Aiken project still builds a blueprint under the pinned fork,
// without touching the tracked tree's onchain/aiken/plutus.json: the blueprint
// is written to a temporary file and deleted afterwards. The tracked blueprint
// belongs to whichever deployment profile last built it
// (`pnpm --dir demo deployment:build <profile>`) and is never committed.
//
// usage: node scripts/preflight/aiken-build-check.mjs [--env <name>]
// Exit 0 when the build succeeds and writes a blueprint with validators, 1
// otherwise, 2 on usage errors.

import { spawnSync } from "node:child_process";
import { existsSync, mkdtempSync, readFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  assertPinnedAiken,
  defaultAikenBinary,
} from "../../onchain/aiken/scripts/pinned-compiler.mjs";

const project = resolve(
  dirname(fileURLToPath(import.meta.url)),
  "../../onchain/aiken",
);
const args = process.argv.slice(2);
const environment = args[0] === "--env" ? args[1] : undefined;
if (
  (args.length !== 0 && args.length !== 2) ||
  (args.length === 2 &&
    (environment === undefined || !/^[a-z0-9_-]+$/u.test(environment)))
) {
  console.error(
    "usage: node scripts/preflight/aiken-build-check.mjs [--env <name>]",
  );
  process.exit(2);
}

const binary = defaultAikenBinary();
try {
  assertPinnedAiken(binary);
} catch (error) {
  console.error(error instanceof Error ? error.message : String(error));
  process.exit(1);
}

const directory = mkdtempSync(join(tmpdir(), "midgard-preflight-blueprint-"));
const out = join(directory, "plutus.json");
try {
  const run = spawnSync(
    binary,
    [
      "build",
      "--out",
      out,
      ...(environment === undefined ? [] : ["--env", environment]),
    ],
    { cwd: project, stdio: "inherit" },
  );
  if (run.error !== undefined || run.status !== 0) {
    console.error(
      `aiken build failed (${run.error?.message ?? `exit ${String(run.status)}`})`,
    );
    process.exit(1);
  }
  // A build that exits 0 and writes nothing, or writes an empty blueprint,
  // proves nothing.
  if (!existsSync(out)) {
    console.error(`aiken build exited 0 but wrote no blueprint at ${out}`);
    process.exit(1);
  }
  const validators = JSON.parse(readFileSync(out, "utf8")).validators ?? [];
  if (validators.length === 0) {
    console.error("aiken build wrote a blueprint with no validators");
    process.exit(1);
  }
  console.log(
    `blueprint builds under ${binary}: ${String(validators.length)} validator entries (temporary file, discarded)`,
  );
} finally {
  rmSync(directory, { recursive: true, force: true });
}
