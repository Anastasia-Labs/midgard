#!/usr/bin/env node

// The CI formatter check (aiken-ci.yml "Run normalized Aiken auto-formatter
// check") for a chosen set of files, without rewriting anything.
//
// `aiken fmt --check` cannot be used: the formatter emits trailing spaces after
// monadic `expect`/`let`, and the tree stores them stripped, so `--check`
// reports every such file as unformatted. CI formats and then strips trailing
// whitespace before diffing; this does the same through `aiken fmt --stdin`.
//
// usage: node scripts/preflight/aiken-fmt-check.mjs [--write] (--all | <file.ak> ...)
// Exit 0 when every file is formatted (or was rewritten with --write), 1 when
// one is not or the compiler is not the pinned fork, 2 on usage errors.

import { spawnSync } from "node:child_process";
import { readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  assertPinnedAiken,
  defaultAikenBinary,
} from "../../onchain/aiken/scripts/pinned-compiler.mjs";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const usage =
  "usage: node scripts/preflight/aiken-fmt-check.mjs [--write] (--all | <file.ak> ...)";

// What the CI step's `sed -i 's/[[:space:]]\+$//'` does to formatter output.
export const normalize = (source) => source.replace(/[ \t\r\f\v]+$/gmu, "");

export const formatSource = (binary, source) => {
  const run = spawnSync(binary, ["fmt", "--stdin"], {
    input: source,
    encoding: "utf8",
    maxBuffer: 64 * 1024 * 1024,
  });
  if (run.error !== undefined || run.status !== 0) {
    throw new Error(
      run.error?.message ??
        (run.stderr || `aiken fmt exited ${String(run.status)}`).trim(),
    );
  }
  return normalize(run.stdout);
};

const isMain =
  process.argv[1] !== undefined &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url);

if (isMain) {
  const args = process.argv.slice(2);
  const write = args.includes("--write");
  const rest = args.filter((arg) => arg !== "--write");
  let files;
  if (rest.length === 1 && rest[0] === "--all") {
    files = spawnSync("git", ["ls-files", "-z", "*.ak"], {
      cwd: root,
      encoding: "utf8",
    })
      .stdout.split("\0")
      .filter(Boolean);
  } else if (rest.length > 0 && rest.every((file) => file.endsWith(".ak"))) {
    files = rest;
  } else {
    console.error(usage);
    process.exit(2);
  }

  const binary = defaultAikenBinary();
  try {
    assertPinnedAiken(binary);
  } catch (error) {
    console.error(error instanceof Error ? error.message : String(error));
    process.exit(1);
  }

  const unformatted = [];
  for (const file of files) {
    const path = resolve(root, file);
    const source = readFileSync(path, "utf8");
    let formatted;
    try {
      formatted = formatSource(binary, source);
    } catch (error) {
      console.error(`${file}: ${error.message}`);
      unformatted.push(file);
      continue;
    }
    if (formatted !== source) {
      if (write) {
        writeFileSync(path, formatted);
        console.log(`formatted ${file}`);
      } else {
        unformatted.push(file);
      }
    }
  }
  if (unformatted.length > 0) {
    console.error(
      [
        `${String(unformatted.length)} of ${String(files.length)} Aiken file(s) are not formatted as CI requires:`,
        ...unformatted.map((file) => `  ${file}`),
        "Fix:",
        `  node scripts/preflight/aiken-fmt-check.mjs --write ${unformatted.join(" ")}`,
      ].join("\n"),
    );
    process.exit(1);
  }
  console.log(`${String(files.length)} Aiken file(s) formatted as CI requires`);
}
