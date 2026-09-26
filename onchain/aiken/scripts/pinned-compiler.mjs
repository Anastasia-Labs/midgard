#!/usr/bin/env node

// The compiler identity every local Aiken run must match. CI builds exactly one
// compiler — the patched fork pinned by `AIKEN_FORK_VERSION` in the workflows —
// because stock v1.1.22 ships an unsound expect-decoder (distinct same-named
// types share one decoder). Locally, `aiken` is an aikup-managed symlink that a
// stray `aikup` silently repoints at stock, so a script that runs "whatever
// `aiken` is on PATH" can compile, format, or test under the unsound compiler
// and still report success. Every local entry point asserts the identity here
// before it spawns the compiler.
//
// CLI: `node onchain/aiken/scripts/pinned-compiler.mjs [binary]` exits 0 when
// the binary (default: MIDGARD_AIKEN_BIN, else `aiken`) reports the pin, 1 when
// it reports anything else or cannot run.

import { spawnSync } from "node:child_process";
import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const repositoryRoot = resolve(
  dirname(fileURLToPath(import.meta.url)),
  "../../..",
);
// Both workflows that build the fork declare the pin; they must agree, or one
// of them is building a compiler the other does not test against.
const pinnedWorkflows = [
  ".github/workflows/aiken-ci.yml",
  ".github/workflows/midgard-node-ci.yml",
];

export const pinnedAikenVersion = (root = repositoryRoot) => {
  const pins = pinnedWorkflows.map((workflow) => {
    const matches = [
      ...readFileSync(resolve(root, workflow), "utf8").matchAll(
        /^\s*AIKEN_FORK_VERSION:\s*(.+?)\s*$/gmu,
      ),
    ];
    if (matches.length !== 1) {
      throw new Error(
        `${workflow} must declare AIKEN_FORK_VERSION exactly once; found ${String(matches.length)}`,
      );
    }
    return { workflow, version: matches[0][1] };
  });
  const versions = new Set(pins.map(({ version }) => version));
  if (versions.size !== 1) {
    throw new Error(
      `the workflows pin different Aiken compilers: ${pins
        .map(({ workflow, version }) => `${workflow}=${version}`)
        .join(", ")}`,
    );
  }
  return pins[0].version;
};

export const defaultAikenBinary = () =>
  process.env.MIDGARD_AIKEN_BIN ?? "aiken";

export const assertPinnedAiken = (binary = defaultAikenBinary()) => {
  const expected = pinnedAikenVersion();
  const run = spawnSync(binary, ["--version"], { encoding: "utf8" });
  const reported = (run.stdout ?? "").trim();
  if (run.error !== undefined || run.status !== 0 || reported !== expected) {
    const found =
      run.error !== undefined
        ? `could not run (${run.error.message})`
        : run.status !== 0
          ? `exited with status ${String(run.status)}`
          : `reports '${reported}'`;
    throw new Error(
      [
        `Aiken compiler '${binary}' ${found}; this repository requires '${expected}' (AIKEN_FORK_VERSION in ${pinnedWorkflows[0]}).`,
        "Stock v1.1.22 compiles unsound expect-decoders, so nothing may compile, format, or test under it.",
        "Point MIDGARD_AIKEN_BIN at the pinned fork, or repoint the `aiken` on PATH at it (a stray `aikup` resets that symlink to stock).",
      ].join("\n"),
    );
  }
  return expected;
};

const isMain =
  process.argv[1] !== undefined &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url);

if (isMain) {
  try {
    assertPinnedAiken(process.argv[2] ?? defaultAikenBinary());
  } catch (error) {
    console.error(error instanceof Error ? error.message : String(error));
    process.exitCode = 1;
  }
}
