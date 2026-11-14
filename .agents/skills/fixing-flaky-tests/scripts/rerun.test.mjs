// Self-tests for rerun.mjs. Every case drives a small fake command in a temp
// directory; none of them runs a real test suite.

import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { getPriority } from "node:os";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { after, describe, it } from "node:test";
import { fileURLToPath } from "node:url";

import {
  missProbability,
  parseArgs,
  requiredRuns,
  ruledOutOneIn,
  sizingReport,
  UsageError,
} from "./rerun.mjs";

const SCRIPT = join(dirname(fileURLToPath(import.meta.url)), "rerun.mjs");
const work = mkdtempSync(join(tmpdir(), "rerun-test-"));
after(() => rmSync(work, { recursive: true, force: true }));

// Counts its own invocations in a file and fails on the listed invocations.
const FAKE = join(work, "fake.mjs");
writeFileSync(
  FAKE,
  `import { existsSync, readFileSync, writeFileSync } from "node:fs";
import { getPriority } from "node:os";
const counter = process.env.FAKE_COUNTER;
const n = (existsSync(counter) ? Number(readFileSync(counter, "utf8")) : 0) + 1;
writeFileSync(counter, String(n));
const failOn = (process.env.FAKE_FAIL_ON ?? "").split(",").filter(Boolean).map(Number);
console.log("fake invocation " + n + " iteration " + process.env.RERUN_ITERATION + " nice " + getPriority());
if (process.env.FAKE_SLEEP_S) await new Promise((r) => setTimeout(r, Number(process.env.FAKE_SLEEP_S) * 1000));
if (process.env.FAKE_ALWAYS_FAIL === "1" || failOn.includes(n)) {
  console.error("boom at invocation " + n);
  process.exit(3);
}
`,
);

let caseNumber = 0;
const rerun = (args, env = {}) => {
  caseNumber += 1;
  const counter = join(work, `counter-${caseNumber}`);
  const result = spawnSync(process.execPath, [SCRIPT, ...args], {
    encoding: "utf8",
    env: { ...process.env, FAKE_COUNTER: counter, ...env },
    timeout: 60_000,
  });
  let invocations = 0;
  try {
    invocations = Number(readFileSync(counter, "utf8"));
  } catch {
    // never ran
  }
  return { ...result, invocations };
};

const fake = ["--", process.execPath, FAKE];

describe("rerun.mjs end to end", () => {
  it("exits 0 when every run passes", () => {
    const r = rerun(["--times", "4", ...fake]);
    assert.equal(r.status, 0, r.stderr);
    assert.equal(r.invocations, 4);
    assert.match(r.stdout, /result: 4\/4 passed, 0 failed/u);
    assert.doesNotMatch(r.stdout, /first failure/u);
  });

  it("exits 1 and names the failing runs when chosen iterations fail", () => {
    const r = rerun(["--times", "6", ...fake], { FAKE_FAIL_ON: "3,5" });
    assert.equal(r.status, 1, r.stderr);
    assert.equal(r.invocations, 6, "keeps running after a failure");
    assert.match(r.stdout, /run 3\/6: FAIL \(exit 3\)/u);
    assert.match(r.stdout, /result: 4\/6 passed, 2 failed \(runs 3, 5\)/u);
    assert.match(r.stdout, /first failure, run 3/u);
    assert.match(r.stdout, /boom at invocation 3/u);
    assert.match(
      r.stdout,
      /Observed 2 failure\(s\) in 6 runs, about 1 in 3\.0\. Not fixed\./u,
    );
    assert.doesNotMatch(r.stdout, /clean runs rule out/u);
    assert.doesNotMatch(
      r.stdout,
      /boom at invocation 5/u,
      "only the first failure's tail",
    );
  });

  it("negative self-test: a command that always fails cannot report green", () => {
    const r = rerun(["--times", "3", ...fake], { FAKE_ALWAYS_FAIL: "1" });
    assert.equal(r.status, 1);
    assert.match(r.stdout, /result: 0\/3 passed, 3 failed/u);
  });

  it("counts a timeout as a failure", () => {
    const r = rerun(["--times", "1", "--timeout-s", "1", ...fake], {
      FAKE_SLEEP_S: "10",
    });
    assert.equal(r.status, 1);
    assert.match(r.stdout, /FAIL \(timed out\)/u);
  });

  it("runs the command under nice -n 19 by default and at caller priority with --no-nice", () => {
    // A failing run prints its tail, which carries the fake's own priority.
    const niced = rerun(["--times", "1", ...fake], { FAKE_ALWAYS_FAIL: "1" });
    assert.match(niced.stdout, /nice 19\b/u);
    const plain = rerun(["--times", "1", "--no-nice", ...fake], {
      FAKE_ALWAYS_FAIL: "1",
    });
    assert.match(plain.stdout, new RegExp(`nice ${getPriority()}\\b`, "u"));
  });

  it("passes RERUN_ITERATION to each run", () => {
    const r = rerun(["--times", "3", ...fake], { FAKE_FAIL_ON: "2" });
    assert.match(r.stdout, /fake invocation 2 iteration 2 /u);
  });

  it("warns when N is below max(3k, 20) and stays quiet when it is not", () => {
    const low = rerun(["--times", "2", "--k", "10", ...fake]);
    assert.equal(low.status, 0);
    assert.match(low.stdout, /WARNING: N=2 is below max\(3k, 20\) = 30/u);
    const enough = rerun(["--times", "20", "--k", "5", ...fake]);
    assert.doesNotMatch(enough.stdout, /WARNING/u);
  });

  for (const [label, args] of [
    ["no separator", ["--times", "2", process.execPath, FAKE]],
    ["no command", ["--times", "2", "--"]],
    ["no --times", ["--", process.execPath, FAKE]],
    ["zero runs", ["--times", "0", ...fake]],
    ["unknown flag", ["--times", "2", "--bogus", ...fake]],
  ]) {
    it(`exits 2 on a usage error: ${label}`, () => {
      const r = rerun(args);
      assert.equal(r.status, 2);
      assert.equal(r.invocations, 0);
      assert.match(r.stderr, /usage:/u);
    });
  }

  it("exits 2, not 1, when the command cannot be run at all", () => {
    const r = rerun(["--times", "3", "--", join(work, "does-not-exist")]);
    assert.equal(r.status, 2);
    assert.match(r.stderr, /Nothing was measured/u);
  });
});

describe("sizing arithmetic", () => {
  it("N = max(3k, 20)", () => {
    assert.equal(requiredRuns(2), 20);
    assert.equal(requiredRuns(7), 21);
    assert.equal(requiredRuns(50), 150);
  });

  it("3k runs leave about a 5% chance of missing a 1-in-k flake", () => {
    for (const k of [10, 50, 200]) {
      const p = missProbability(k, requiredRuns(k));
      assert.ok(p > 0.04 && p < 0.05, `k=${k}: ${p}`);
    }
  });

  it("N clean runs rule out roughly a 3/N failure rate", () => {
    assert.ok(Math.abs(ruledOutOneIn(100) - 100 / 3) < 1);
    assert.match(sizingReport({ times: 30 }), /No --k given/u);
  });

  it("parseArgs rejects a non-integer --k", () => {
    assert.throws(
      () => parseArgs(["--times", "3", "--k", "2.5", "--", "x"]),
      UsageError,
    );
  });
});
