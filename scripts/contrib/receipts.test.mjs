import assert from "node:assert/strict";
import { mkdirSync, writeFileSync } from "node:fs";
import { resolve } from "node:path";
import test from "node:test";

import { atomicJson, inputIdentity } from "./files.mjs";
import { fixture } from "./fixture.test-support.mjs";
import { countsFromVitest, verifyReceipt, writeReceipt } from "./receipts.mjs";

const report = (assertions) => ({
  success: true,
  numPassedTests: assertions.filter((item) => item.status === "passed").length,
  numFailedTests: assertions.filter((item) => item.status === "failed").length,
  testResults: [{ assertionResults: assertions }],
});
test("counts actual assertions, separates filtered/skipped and rejects inconsistent totals", () => {
  const data = report([
    { status: "passed", fullName: "selected works" },
    { status: "pending", fullName: "other case" },
    { status: "pending", fullName: "selected skip" },
  ]);
  assert.deepEqual(countsFromVitest(data, "selected"), {
    passed: 1,
    failed: 0,
    skipped: 1,
    filtered: 1,
    todo: 0,
    setupErrors: 0,
    executed: 1,
  });
  assert.throws(
    () => countsFromVitest({ ...data, numPassedTests: 5 }),
    /disagree/u,
  );
  assert.equal(
    countsFromVitest({ ...report([]), success: false }).setupErrors,
    1,
  );
  assert.throws(
    () =>
      countsFromVitest(
        {
          ...report([{ status: "passed" }]),
          testResults: [
            {
              name: "/wrong.test.ts",
              assertionResults: [{ status: "passed" }],
            },
          ],
        },
        undefined,
        ["/wanted.test.ts"],
      ),
    /different files/u,
  );
});

test("receipt rejects zero tests, missing report, source edits and substituted logs", (t) => {
  const root = fixture(t);
  const directory = resolve(root, "run");
  mkdirSync(directory);
  const logPath = resolve(directory, "test.log");
  writeFileSync(logPath, "actual execution");
  const reportPath = resolve(directory, "report.json");
  const identity = inputIdentity(root, "example");
  const step = {
    argv: ["vitest", "run"],
    cwd: root,
    exitCode: 0,
    startedAt: "2026-10-03T00:00:00Z",
    endedAt: "2026-10-03T00:00:01Z",
    logPath,
  };
  const write = () =>
    writeReceipt({
      root,
      pkg: { name: "example" },
      directory,
      kind: "test",
      before: identity,
      after: identity,
      steps: [step],
      reportPath,
    });
  assert.equal(write().status, "failed");
  atomicJson(reportPath, report([]));
  assert.equal(write().status, "failed");
  atomicJson(reportPath, report([{ status: "passed" }]));
  const receipt = write();
  assert.equal(verifyReceipt(root, receipt.path).status, "passed");
  atomicJson(reportPath, report([]));
  assert.throws(() => verifyReceipt(root, receipt.path), /changed/u);
  atomicJson(reportPath, report([{ status: "passed" }]));
  writeFileSync(logPath, "invented pass");
  assert.throws(() => verifyReceipt(root, receipt.path), /log.*changed/u);
  writeFileSync(logPath, "actual execution");
  writeFileSync(resolve(root, "demo/example/src/index.ts"), "changed");
  assert.throws(() => verifyReceipt(root, receipt.path), /stale/u);
});

test("a compiler version probe cannot be reclassified as a build", (t) => {
  const root = fixture(t);
  const logPath = resolve(root, "probe.log");
  writeFileSync(logPath, "v1.0");
  const identity = inputIdentity(root, "example");
  const receipt = writeReceipt({
    root,
    pkg: { name: "example" },
    directory: root,
    kind: "build",
    before: identity,
    after: identity,
    steps: [
      {
        argv: ["aiken", "--version"],
        exitCode: 0,
        cwd: root,
        startedAt: "2026-10-03T00:00:00Z",
        endedAt: "2026-10-03T00:00:01Z",
        logPath,
      },
    ],
  });
  assert.throws(() => verifyReceipt(root, receipt.path), /version\/help/u);
});
