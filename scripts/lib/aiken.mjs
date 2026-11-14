#!/usr/bin/env node

// The one place any script, hook or test gets its aiken from.
//
// aiken is a build-system dependency: `demo/flake.nix` names it as a flake
// input and `demo/flake.lock` pins the exact source. This module builds (or
// fetches from the store) that locked input with nix and returns the binary
// inside it. Nothing reads aiken from PATH, so an aiken installed some other
// way can never be the one that runs.
//
// The locked input is built directly (`github:<owner>/<repo>/<rev>?narHash=…`)
// rather than through `./demo#aiken`, which would copy the whole checkout into
// the store on every call.
//
// CLI: `node scripts/lib/aiken.mjs` prints the binary's path.

import { spawnSync } from "node:child_process";
import { readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const repoRoot = resolve(dirname(fileURLToPath(import.meta.url)), "../..");

let resolved;

export const aikenBinary = () => {
  if (resolved !== undefined) return resolved;
  const lock = JSON.parse(
    readFileSync(join(repoRoot, "demo/flake.lock"), "utf8"),
  );
  const locked = lock.nodes.aiken.locked;
  const ref = `github:${locked.owner}/${locked.repo}/${locked.rev}?narHash=${encodeURIComponent(locked.narHash)}#aiken`;
  const build = spawnSync(
    "nix",
    ["build", ref, "--no-link", "--print-out-paths"],
    {
      encoding: "utf8",
      stdio: ["ignore", "pipe", "pipe"],
    },
  );
  if (build.error !== undefined || build.status !== 0) {
    throw new Error(
      `could not get aiken from demo/flake.lock (nix build ${ref}): ${(build.error?.message ?? build.stderr).trim()}`,
    );
  }
  resolved = join(build.stdout.trim(), "bin/aiken");
  return resolved;
};

if (
  process.argv[1] !== undefined &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url)
) {
  try {
    process.stdout.write(`${aikenBinary()}\n`);
  } catch (error) {
    process.stderr.write(`${error.message}\n`);
    process.exit(1);
  }
}
