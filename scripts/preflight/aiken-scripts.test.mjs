// The Aiken wrappers against the real aiken from demo/flake.lock, on throwaway
// files and projects.

import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import {
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import { buildCheck } from "./aiken-build-check.mjs";
import { normalize } from "./aiken-fmt-check.mjs";

const here = dirname(fileURLToPath(import.meta.url));

const withScratch = (body) => {
  const scratch = mkdtempSync(join(tmpdir(), "preflight-aiken-test-"));
  try {
    return body(scratch);
  } finally {
    rmSync(scratch, { recursive: true, force: true });
  }
};
const run = (script, args) =>
  spawnSync(process.execPath, [join(here, script), ...args], {
    encoding: "utf8",
  });

test("normalize strips what CI's sed strips and nothing else", () => {
  assert.equal(normalize("a  \n\tb\t\n  c\n"), "a\n\tb\n  c\n");
  assert.equal(normalize("x\r\ny"), "x\ny");
});

test("aiken-fmt-check passes formatted files and fails unformatted ones with the fix", () =>
  withScratch((scratch) => {
    const clean = join(scratch, "clean.ak");
    const dirty = join(scratch, "dirty.ak");
    writeFileSync(clean, "fn a() {\n  1\n}\n");
    writeFileSync(dirty, "fn  a() {  1 }\n");
    assert.equal(run("aiken-fmt-check.mjs", [clean]).status, 0);
    const failed = run("aiken-fmt-check.mjs", [clean, dirty]);
    assert.equal(failed.status, 1);
    assert.match(failed.stderr, /1 of 2 Aiken file\(s\) are not formatted/u);
    assert.match(failed.stderr, /aiken-fmt-check\.mjs --write .*dirty\.ak/u);
    assert.equal(run("aiken-fmt-check.mjs", ["--write", dirty]).status, 0);
    assert.equal(readFileSync(dirty, "utf8"), "fn a() {\n  1\n}\n");
  }));

test("aiken-fmt-check refuses bad usage", () => {
  assert.equal(run("aiken-fmt-check.mjs", []).status, 2);
  assert.equal(run("aiken-fmt-check.mjs", ["notes.txt"]).status, 2);
});

const project = (scratch, modules) => {
  mkdirSync(scratch, { recursive: true });
  writeFileSync(
    join(scratch, "aiken.toml"),
    'name = "midgard/selftest"\nversion = "0.0.0"\nplutus = "v3"\n',
  );
  for (const [path, source] of Object.entries(modules)) {
    mkdirSync(dirname(join(scratch, path)), { recursive: true });
    writeFileSync(join(scratch, path), source);
  }
  return scratch;
};

test("aiken-build-check needs a blueprint with validators, not just a build that succeeds", () =>
  withScratch((scratch) => {
    const good = buildCheck({
      project: project(join(scratch, "good"), {
        "validators/always.ak":
          "validator always {\n  spend(_d: Option<Data>, _r: Data, _o: Data, _t: Data) {\n    True\n  }\n}\n",
      }),
    });
    assert.equal(good.status, 0, good.error);
    assert.match(good.report, /[1-9]\d* validator entries/u);
    const none = buildCheck({
      project: project(join(scratch, "none"), {
        "lib/midgard/x.ak": "pub const x = 1\n",
      }),
    });
    assert.equal(none.status, 1);
    assert.match(none.error, /no validators/u);
    const broken = buildCheck({
      project: project(join(scratch, "broken"), {
        "validators/always.ak": "validator always {\n",
      }),
    });
    assert.equal(broken.status, 1);
    assert.match(broken.error, /aiken build failed/u);
  }));

test("aiken-build-check refuses bad usage", () => {
  assert.equal(run("aiken-build-check.mjs", ["--env"]).status, 2);
  assert.equal(run("aiken-build-check.mjs", ["--env", "Bad Name"]).status, 2);
});
