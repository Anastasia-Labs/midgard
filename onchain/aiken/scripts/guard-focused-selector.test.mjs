import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { chmodSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import {
  evaluateSelectorReport,
  parseSelectors,
} from "./guard-focused-selector.mjs";

const guardPath = resolve(
  dirname(fileURLToPath(import.meta.url)),
  "guard-focused-selector.mjs",
);

// Runs the guard end to end with a stub standing in for `aiken`, so the whole
// CLI path — spawn, parse, classify, exit — is exercised without paying for a
// real compile. The stub reproduces the exact behaviour that made zero
// collection invisible: a well-formed report and exit status 0.
const runGuardAgainstStub = (stubReport, selectors) => {
  const directory = mkdtempSync(join(tmpdir(), "midgard-guard-selftest-"));
  try {
    const stubPath = join(directory, "aiken-stub.mjs");
    writeFileSync(
      stubPath,
      `#!/usr/bin/env node\nprocess.stdout.write(${JSON.stringify(
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
