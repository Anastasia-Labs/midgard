#!/usr/bin/env node

// Proves the Aiken project still builds a blueprint, without touching the tracked tree's onchain/aiken/plutus.json: the blueprint
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
import { aikenBinary } from "../lib/aiken.mjs";

// Builds `project` into a temporary blueprint and returns the exit status
// with its report lines.
export const buildCheck = ({ project, environment }) => {
  const directory = mkdtempSync(join(tmpdir(), "midgard-preflight-blueprint-"));
  const out = join(directory, "plutus.json");
  try {
    const run = spawnSync(
      aikenBinary(),
      [
        "build",
        "--out",
        out,
        ...(environment === undefined ? [] : ["--env", environment]),
      ],
      { cwd: project, stdio: ["ignore", "inherit", "inherit"] },
    );
    if (run.error !== undefined || run.status !== 0) {
      return {
        status: 1,
        error: `aiken build failed (${run.error?.message ?? `exit ${String(run.status)}`})`,
      };
    }
    // A build that exits 0 and writes nothing, or writes an empty blueprint,
    // proves nothing.
    if (!existsSync(out)) {
      return {
        status: 1,
        error: `aiken build exited 0 but wrote no blueprint at ${out}`,
      };
    }
    const validators = JSON.parse(readFileSync(out, "utf8")).validators ?? [];
    if (validators.length === 0) {
      return {
        status: 1,
        error: "aiken build wrote a blueprint with no validators",
      };
    }
    return {
      status: 0,
      report: `blueprint builds: ${String(validators.length)} validator entries (temporary file, discarded)`,
    };
  } finally {
    rmSync(directory, { recursive: true, force: true });
  }
};

const isMain =
  process.argv[1] !== undefined &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url);

if (isMain) {
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
  const outcome = buildCheck({
    project: resolve(
      dirname(fileURLToPath(import.meta.url)),
      "../../onchain/aiken",
    ),
    environment,
  });
  if (outcome.status === 0) console.log(outcome.report);
  else console.error(outcome.error);
  process.exitCode = outcome.status;
}
