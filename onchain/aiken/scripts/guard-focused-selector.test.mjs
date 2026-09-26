import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { chmodSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import {
  evaluateSelectorReport,
  parseInvocation,
  parseSelectors,
} from "./guard-focused-selector.mjs";
import { pinnedAikenVersion } from "./pinned-compiler.mjs";

const guardPath = resolve(
  dirname(fileURLToPath(import.meta.url)),
  "guard-focused-selector.mjs",
);

// Runs the guard end to end with a stub standing in for `aiken`, so the whole
// CLI path — spawn, parse, classify, exit — is exercised without paying for a
// real compile. The stub reproduces the exact behaviour that made zero
// collection invisible: a well-formed report and exit status 0.
// With `expectedArgs`, the stub exits 64 and reports nothing unless the guard
// invoked it with exactly those arguments.
const runGuardAgainstStub = (
  stubReport,
  selectors,
  stubVersion = pinnedAikenVersion(),
  expectedArgs = undefined,
) => {
  const directory = mkdtempSync(join(tmpdir(), "midgard-guard-selftest-"));
  try {
    const stubPath = join(directory, "aiken-stub.mjs");
    writeFileSync(
      stubPath,
      `#!/usr/bin/env node\nif (process.argv[2] === "--version") {\n  process.stdout.write(${JSON.stringify(
        `${stubVersion}\n`,
      )});\n  process.exit(0);\n}\n${
        expectedArgs === undefined
          ? ""
          : `if (JSON.stringify(process.argv.slice(2)) !== ${JSON.stringify(
              JSON.stringify(expectedArgs),
            )}) {\n  process.exit(64);\n}\n`
      }process.stdout.write(${JSON.stringify(
        JSON.stringify(stubReport),
      )});\nprocess.exit(0);\n`,
    );
    chmodSync(stubPath, 0o755);
    return spawnSync(process.execPath, [guardPath, ...selectors], {
      encoding: "utf8",
      env: {
        ...process.env,
        MIDGARD_AIKEN_BIN: stubPath,
      },
    });
  } finally {
    rmSync(directory, { recursive: true, force: true });
  }
};

test("rejects unusable selector arguments", () => {
  assert.throws(() => parseSelectors([]), /usage/u);
  assert.throws(() => parseSelectors(["../escape"]), /usage/u);
  assert.throws(() => parseSelectors(["Not_Lowercase"]), /usage/u);
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
      ok: true,
    },
  );
});

// The negative self-test the guard exists for: a zero-collecting selector that
// aiken itself reports with exit status 0 must leave the guard exiting nonzero.
test("exits nonzero end to end on a zero-collecting selector", () => {
  const result = runGuardAgainstStub(
    { summary: { total: 0, passed: 0, failed: 0 }, modules: [] },
    ["midgard/selector_that_matches_nothing"],
  );
  assert.notEqual(result.status, 0);
  assert.match(result.stderr, /midgard\/selector_that_matches_nothing/u);
  assert.match(result.stderr, /collected 0 tests/u);
});

test("exits zero end to end on a selector that collects passing tests", () => {
  const result = runGuardAgainstStub(
    { summary: { total: 3, passed: 3, failed: 0 } },
    ["state_queue"],
  );
  assert.equal(result.status, 0);
  assert.match(result.stdout, /"collected":3/u);
});

test("refuses a compiler other than the pinned fork even when its report is green", () => {
  const result = runGuardAgainstStub(
    { summary: { total: 3, passed: 3, failed: 0 } },
    ["state_queue"],
    "aiken v1.1.22+39d6b04",
  );
  assert.equal(result.status, 1);
  assert.equal(result.stdout, "");
  assert.match(result.stderr, /reports 'aiken v1\.1\.22\+39d6b04'/u);
});

// CI's full-suite step runs `--all`. A whole-suite report that collected
// nothing (a source root moved, a filter left in the environment) must fail it.
test("exits nonzero end to end when the whole suite collects nothing", () => {
  const result = runGuardAgainstStub(
    { summary: { total: 0, passed: 0, failed: 0 }, modules: [] },
    ["--all"],
    pinnedAikenVersion(),
    ["check", "--plain-numbers"],
  );
  assert.equal(result.status, 1);
  assert.match(result.stderr, /\(all modules\) collected 0 tests/u);
});

test("runs the whole suite with no module filter and reports its count", () => {
  const result = runGuardAgainstStub(
    { summary: { total: 4467, passed: 4467, failed: 0 }, modules: [] },
    ["--all"],
    pinnedAikenVersion(),
    ["check", "--plain-numbers"],
  );
  assert.equal(result.status, 0, result.stderr);
  assert.match(result.stdout, /"selector":"\(all modules\)"/u);
  assert.match(result.stdout, /"collected":4467/u);
});

test("names the failing tests of a red whole-suite run", () => {
  const result = runGuardAgainstStub(
    {
      summary: { total: 2, passed: 1, failed: 1 },
      modules: [
        {
          name: "midgard/state_queue",
          tests: [
            { title: "keeps_order", status: "pass" },
            { title: "rejects_gap", status: "fail" },
          ],
        },
      ],
    },
    ["--all"],
    pinnedAikenVersion(),
    ["check", "--plain-numbers"],
  );
  assert.equal(result.status, 1);
  assert.match(result.stderr, /collected=2, passed=1, failed=1/u);
  assert.match(result.stderr, /"module":"midgard\/state_queue"/u);
  assert.match(result.stderr, /"title":"rejects_gap"/u);
  assert.doesNotMatch(result.stderr, /keeps_order/u);
});
