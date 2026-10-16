import assert from "node:assert/strict";
import { mkdirSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { test } from "node:test";

import {
  ALL_MODULES,
  evaluateSelectorReport,
  exUnitsSummary,
  parseInvocation,
  parseSelectors,
  runSelector,
} from "./guard-focused-selector.mjs";

// End-to-end runs use the real aiken from demo/flake.lock on a throwaway
// project: `passing` holds two passing tests, `failing` one passing and one
// failing test.
const withProject = (modules, body) => {
  const directory = mkdtempSync(join(tmpdir(), "midgard-guard-selftest-"));
  try {
    writeFileSync(
      join(directory, "aiken.toml"),
      'name = "midgard/selftest"\nversion = "0.0.0"\nplutus = "v3"\n',
    );
    mkdirSync(join(directory, "lib/midgard"), { recursive: true });
    for (const [name, source] of Object.entries(modules))
      writeFileSync(join(directory, `lib/midgard/${name}.ak`), source);
    return body(directory);
  } finally {
    rmSync(directory, { recursive: true, force: true });
  }
};
const PASSING =
  "test keeps_order() { True }\n\ntest wide() { [1, 2, 3] == [1, 2, 3] }\n";
const FAILING = "test keeps_order() { True }\n\ntest rejects_gap() { False }\n";
const run = (selector, modules) =>
  withProject(modules, (projectDirectory) =>
    runSelector(selector, { projectDirectory }),
  );

test("rejects unusable selector arguments", () => {
  assert.throws(() => parseSelectors([]), /usage/u);
  assert.throws(() => parseSelectors(["../escape"]), /usage/u);
  assert.throws(() => parseSelectors(["Not_Lowercase"]), /usage/u);
  assert.throws(
    () =>
      parseSelectors(
        Array.from({ length: 65 }, (_, index) => `midgard/module_${index}`),
      ),
    /usage/u,
  );
  assert.throws(
    () => parseSelectors(["state_queue", "state_queue"]),
    /unique/u,
  );
  assert.deepEqual(parseSelectors(["midgard/state_queue"]), [
    "midgard/state_queue",
  ]);
});

test("accepts --all alone and refuses it mixed with selectors", () => {
  assert.deepEqual(parseInvocation(["--all"]), ["--all"]);
  assert.deepEqual(parseInvocation(["state_queue"]), ["state_queue"]);
  assert.throws(() => parseInvocation(["--all", "state_queue"]), /usage/u);
  assert.throws(() => parseInvocation([]), /usage/u);
});

test("treats zero collection as failure and names the selector", () => {
  const outcome = evaluateSelectorReport("fraud_claim_lock", {
    stdout: JSON.stringify({
      summary: { total: 0, passed: 0, failed: 0 },
      modules: [],
    }),
    status: 0,
  });
  assert.equal(outcome.ok, false);
  assert.match(outcome.diagnostic, /fraud_claim_lock/u);
  assert.match(outcome.diagnostic, /collected 0 tests/u);
});

test("treats failing, unparseable, and unlaunchable runs as failure", () => {
  assert.equal(
    evaluateSelectorReport("state_queue", {
      stdout: JSON.stringify({ summary: { total: 5, passed: 4, failed: 1 } }),
      status: 1,
    }).ok,
    false,
  );
  assert.match(
    evaluateSelectorReport("state_queue", { stdout: "not json", status: 0 })
      .diagnostic,
    /structured test report/u,
  );
  assert.match(
    evaluateSelectorReport("state_queue", {
      stdout: "",
      status: null,
      error: new Error("spawn ENOENT"),
    }).diagnostic,
    /could not be executed/u,
  );
});

test("accepts a selector that collects and passes every test", () => {
  assert.deepEqual(
    evaluateSelectorReport("fraud_proofs/da_hash_preimage/step_01", {
      stdout: JSON.stringify({ summary: { total: 10, passed: 10, failed: 0 } }),
      status: 0,
    }),
    {
      selector: "fraud_proofs/da_hash_preimage/step_01",
      total: 10,
      passed: 10,
      failed: 0,
      exUnits: { unitTests: 0 },
      ok: true,
    },
  );
});

test("summarises the largest unit-test execution units against the §3.3 basis", () => {
  const report = {
    modules: [
      {
        name: "midgard/a",
        tests: [
          { title: "small", execution_units: { mem: 1_000, cpu: 9_000_000 } },
          { title: "property", iterations: 100 },
        ],
      },
      {
        name: "midgard/b",
        tests: [
          { title: "wide", execution_units: { mem: 3_300_000, cpu: 1_000 } },
        ],
      },
    ],
  };
  assert.deepEqual(exUnitsSummary(report), {
    unitTests: 2,
    maxMem: { test: "midgard/b.wide", mem: 3_300_000, ofBasis: "25.0%" },
    maxCpu: { test: "midgard/a.small", cpu: 9_000_000, ofBasis: "0.1%" },
  });
});

test("refuses a signalled compiler even after it emitted a green report", () => {
  const outcome = evaluateSelectorReport("midgard/state_queue", {
    stdout: JSON.stringify({ summary: { total: 1, passed: 1, failed: 0 } }),
    status: null,
    signal: "SIGTERM",
  });
  assert.equal(outcome.ok, false, "a signal cannot establish compiler success");
});

// The negative self-test the guard exists for: a zero-collecting selector that
// aiken itself reports with exit status 0 must fail the guard.
test("fails a zero-collecting selector and names it", () => {
  const outcome = run("midgard/selector_that_matches_nothing", {
    passing: PASSING,
  });
  assert.equal(outcome.ok, false);
  assert.match(outcome.diagnostic, /midgard\/selector_that_matches_nothing/u);
  assert.match(outcome.diagnostic, /collected 0 tests/u);
});

test("passes a selector that collects passing tests, with their ex-units", () => {
  const outcome = run("midgard/passing", {
    passing: PASSING,
    failing: FAILING,
  });
  assert.equal(outcome.ok, true, outcome.diagnostic);
  assert.equal(outcome.total, 2);
  assert.equal(outcome.exUnits.unitTests, 2);
  assert.match(outcome.exUnits.maxMem.test, /^midgard\/passing\./u);
});

// CI's full-suite step runs `--all`. A whole-suite run that collected nothing
// (a source root moved, a filter left in the environment) must fail it.
test("fails a whole-suite run that collects nothing", () => {
  const outcome = run(ALL_MODULES, { empty: "pub const x = 1\n" });
  assert.equal(outcome.ok, false);
  assert.match(outcome.diagnostic, /\(all modules\) collected 0 tests/u);
});

test("runs the whole suite with no module filter", () => {
  const outcome = run(ALL_MODULES, {
    passing: PASSING,
    other: "test other() { True }\n",
  });
  assert.equal(outcome.ok, true, outcome.diagnostic);
  assert.equal(outcome.total, 3);
});

test("names the failing tests of a red whole-suite run", () => {
  const outcome = run(ALL_MODULES, { passing: PASSING, failing: FAILING });
  assert.equal(outcome.ok, false);
  assert.match(outcome.diagnostic, /collected=4, passed=3, failed=1/u);
  assert.match(outcome.diagnostic, /"module":"midgard\/failing"/u);
  assert.match(outcome.diagnostic, /"title":"rejects_gap"/u);
  assert.doesNotMatch(outcome.diagnostic, /keeps_order/u);
});
