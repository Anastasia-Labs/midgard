import assert from "node:assert/strict";
import { mkdirSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { after, test } from "node:test";

import { focusedCheck } from "./run-focused-check.mjs";

// One throwaway project, checked by the real aiken from demo/flake.lock. Its
// module names carry the shapes the real tree uses: hyphens (which aiken folds
// to underscores) and dots (which `-m` cannot spell in full), plus a module
// whose name is a prefix of another's.
const projectDirectory = mkdtempSync(join(tmpdir(), "midgard-focused-check-"));
after(() => rmSync(projectDirectory, { recursive: true, force: true }));
const write = (path, text) => {
  mkdirSync(dirname(join(projectDirectory, path)), { recursive: true });
  writeFileSync(join(projectDirectory, path), text);
};
write(
  "aiken.toml",
  'name = "midgard/selftest"\nversion = "0.0.0"\nplutus = "v3"\n',
);
write(
  "lib/midgard/bounded-blob-v1.test.ak",
  "test keeps_bound() { 1 + 1 == 2 }\n\ntest rejects_gap() { False }\n",
);
write("lib/midgard/shared.ak", "test same_name() { True }\n");
write("lib/midgard/shared.test.ak", "test same_name() { True }\n");

const check = (moduleName, ...testNames) =>
  focusedCheck({ projectDirectory, moduleName, testNames });

test("a hyphenated, dotted module runs under either spelling and reports its result", () => {
  for (const moduleName of [
    "midgard/bounded-blob-v1.test",
    "midgard/bounded_blob_v1.test",
  ]) {
    const outcome = check(moduleName, "keeps_bound");
    assert.equal(outcome.status, 0, outcome.stderr);
    const report = JSON.parse(outcome.stdout);
    assert.equal(report.modules[0].name, "midgard/bounded_blob_v1.test");
    assert.equal(report.summary.passed, 1);
  }
});

test("a passing run summarises its largest execution units on stderr, leaving stdout the report", () => {
  const outcome = check("midgard/bounded-blob-v1.test", "keeps_bound");
  assert.equal(outcome.status, 0, outcome.stderr);
  const summary = JSON.parse(outcome.stderr.trimEnd().split("\n").at(-1));
  assert.equal(summary.exUnits.unitTests, 1);
  assert.equal(
    summary.exUnits.maxMem.test,
    "midgard/bounded_blob_v1.test.keeps_bound",
  );
  assert.equal(JSON.parse(outcome.stdout).summary.total, 1);
});

test("a failing test fails the check", () => {
  const outcome = check("midgard/bounded-blob-v1.test", "rejects_gap");
  assert.equal(outcome.status, 1);
  assert.match(
    outcome.stderr,
    /expected exactly 1 passing test\(s\); collected=1, passed=0, failed=1/u,
  );
});

test("results from another module fail closed", () => {
  // `-m midgard/shared.{same_name}` matches both modules by substring.
  const outcome = check("midgard/shared.test", "same_name");
  assert.equal(outcome.status, 1);
  assert.match(
    outcome.stderr,
    /expected results from module 'midgard\/shared\.test' only/u,
  );
});

test("a missing module fails closed with a diagnostic", () => {
  const outcome = check("midgard/no_such_module", "keeps_bound");
  assert.equal(outcome.status, 2);
  assert.match(outcome.stderr, /no module 'midgard\/no_such_module'/u);
});

test("a module or test outside the allowlist is rejected", () => {
  for (const [moduleName, testName] of [
    ["../escape", "keeps_bound"],
    ["Midgard/Upper", "keeps_bound"],
    ["midgard/bounded-blob-v1.test", "Bad-Name"],
  ]) {
    const outcome = check(moduleName, testName);
    assert.equal(outcome.status, 2);
    assert.match(outcome.stderr, /usage/u);
  }
  assert.equal(check("midgard/shared").status, 2);
  assert.equal(check("midgard/shared", "same_name", "same_name").status, 2);
});
