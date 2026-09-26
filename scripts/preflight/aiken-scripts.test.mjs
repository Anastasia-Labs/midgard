// The Aiken wrappers against a stub compiler: hermetic, no real aiken needed.

import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import {
  chmodSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import { pinnedAikenVersion } from "../../onchain/aiken/scripts/pinned-compiler.mjs";
import { normalize } from "./aiken-fmt-check.mjs";

const here = dirname(fileURLToPath(import.meta.url));
const pin = pinnedAikenVersion(resolve(here, "../.."));

// A stub `aiken`: `--version` reports $STUB_VERSION; `fmt --stdin` collapses
// runs of spaces and, like the real formatter after `expect`, leaves trailing
// spaces; `build --out <file>` writes $STUB_BLUEPRINT (if set) and exits
// $STUB_BUILD_STATUS.
const STUB = `#!/usr/bin/env bash
case "$1" in
  --version) echo "$STUB_VERSION" ;;
  fmt) sed -e 's/  */ /g' -e 's/$/  /' ;;
  build)
    if [[ -n "\${STUB_BLUEPRINT:-}" ]]; then printf '%s' "$STUB_BLUEPRINT" > "$3"; fi
    exit "\${STUB_BUILD_STATUS:-0}" ;;
  *) exit 64 ;;
esac
`;

const withStub = (body) => {
  const scratch = mkdtempSync(join(tmpdir(), "preflight-aiken-test-"));
  try {
    const binary = join(scratch, "aiken");
    writeFileSync(binary, STUB);
    chmodSync(binary, 0o755);
    return body(scratch, (script, args, env = {}) =>
      spawnSync(process.execPath, [join(here, script), ...args], {
        encoding: "utf8",
        env: {
          ...process.env,
          MIDGARD_AIKEN_BIN: binary,
          STUB_VERSION: pin,
          ...env,
        },
      }),
    );
  } finally {
    rmSync(scratch, { recursive: true, force: true });
  }
};

test("normalize strips what CI's sed strips and nothing else", () => {
  assert.equal(normalize("a  \n\tb\t\n  c\n"), "a\n\tb\n  c\n");
  assert.equal(normalize("x\r\ny"), "x\ny");
});

test("aiken-fmt-check passes formatted files and fails unformatted ones with the fix", () =>
  withStub((scratch, run) => {
    const clean = join(scratch, "clean.ak");
    const dirty = join(scratch, "dirty.ak");
    writeFileSync(clean, "fn a() { 1 }\n");
    writeFileSync(dirty, "fn  a() {  1 }\n");
    assert.equal(run("aiken-fmt-check.mjs", [clean]).status, 0);
    const failed = run("aiken-fmt-check.mjs", [clean, dirty]);
    assert.equal(failed.status, 1);
    assert.match(failed.stderr, /1 of 2 Aiken file\(s\) are not formatted/u);
    assert.match(failed.stderr, /aiken-fmt-check\.mjs --write .*dirty\.ak/u);
    assert.equal(run("aiken-fmt-check.mjs", ["--write", dirty]).status, 0);
    assert.equal(readFileSync(dirty, "utf8"), "fn a() { 1 }\n");
  }));

test("aiken-fmt-check refuses a compiler that is not the pin, and bad usage", () =>
  withStub((scratch, run) => {
    const file = join(scratch, "clean.ak");
    writeFileSync(file, "fn a() { 1 }\n");
    const stock = run("aiken-fmt-check.mjs", [file], {
      STUB_VERSION: "aiken v1.1.22+stock",
    });
    assert.equal(stock.status, 1);
    assert.match(stock.stderr, /requires/u);
    assert.equal(run("aiken-fmt-check.mjs", []).status, 2);
    assert.equal(run("aiken-fmt-check.mjs", ["notes.txt"]).status, 2);
  }));

test("aiken-build-check needs a blueprint with validators, not just exit 0", () =>
  withStub((_scratch, run) => {
    const good = run("aiken-build-check.mjs", [], {
      STUB_BLUEPRINT: JSON.stringify({ validators: [{ title: "v" }] }),
    });
    assert.equal(good.status, 0, good.stderr);
    assert.match(good.stdout, /1 validator entries/u);
    const empty = run("aiken-build-check.mjs", [], {
      STUB_BLUEPRINT: JSON.stringify({ validators: [] }),
    });
    assert.equal(empty.status, 1);
    const nothing = run("aiken-build-check.mjs", []);
    assert.equal(nothing.status, 1);
    assert.match(nothing.stderr, /wrote no blueprint/u);
    const broken = run("aiken-build-check.mjs", [], { STUB_BUILD_STATUS: "1" });
    assert.equal(broken.status, 1);
    const stock = run("aiken-build-check.mjs", [], {
      STUB_VERSION: "aiken v1.1.22+stock",
    });
    assert.equal(stock.status, 1);
    assert.equal(run("aiken-build-check.mjs", ["--env"]).status, 2);
  }));
