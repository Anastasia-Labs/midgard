import { spawnSync } from "node:child_process";
import { mkdtempSync, readFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { basename, dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const repositoryRoot = resolve(scriptDirectory, "../..");
const dependencyMapPath = resolve(
  repositoryRoot,
  "docs/exec-plans/evidence/canonical-v1-watcher-dependency-map-v1.json",
);
const dependencyMap = JSON.parse(readFileSync(dependencyMapPath, "utf8"));
const watcherEvidence = dependencyMap.requiredWatcherPackage;

const expectedByFile = new Map([
  [
    "config.test.ts",
    watcherEvidence.strictConfiguration?.expectedFocusedTestCount,
  ],
  [
    "deployment-identity.test.ts",
    watcherEvidence.deploymentIdentity?.expectedFocusedTestCount,
  ],
  [
    "durable-store.test.ts",
    watcherEvidence.durableStore?.expectedFocusedTestCount,
  ],
  ["l1-adapter.test.ts", watcherEvidence.l1Adapter?.expectedFocusedTestCount],
  [
    "multi-provider-consistency.test.ts",
    watcherEvidence.multiProviderConsistency?.expectedFocusedTestCount,
  ],
  [
    "finality-engine.test.ts",
    watcherEvidence.finalityEngine?.expectedFocusedTestCount,
  ],
  [
    "rollback-engine.test.ts",
    watcherEvidence.rollbackEngine?.expectedFocusedTestCount,
  ],
  [
    "state-queue-indexer.test.ts",
    watcherEvidence.stateQueueIndexer?.expectedFocusedTestCount,
  ],
  [
    "user-event-indexer.test.ts",
    watcherEvidence.userEventIndexer?.expectedFocusedTestCount,
  ],
  [
    "settlement-indexer.test.ts",
    watcherEvidence.settlementIndexer?.expectedFocusedTestCount,
  ],
  [
    "proof-thread-indexer.test.ts",
    watcherEvidence.proofThreadIndexer?.expectedFocusedTestCount,
  ],
  [
    "rule-bundle-v1.test.ts",
    watcherEvidence.ruleBundle?.expectedFocusedTestCount,
  ],
  [
    "public-da-client.test.ts",
    watcherEvidence.publicDaClient?.expectedFocusedTestCount,
  ],
  [
    "canonical-block-store.test.ts",
    watcherEvidence.canonicalBlockStore?.expectedFocusedTestCount,
  ],
  [
    "header-root-reconstruction.test.ts",
    watcherEvidence.headerRootReconstruction?.expectedFocusedTestCount,
  ],
  [
    "phase-a-verifier.test.ts",
    watcherEvidence.phaseAVerifier?.expectedFocusedTestCount,
  ],
  [
    "block-replay.test.ts",
    watcherEvidence.blockReplay?.expectedFocusedTestCount,
  ],
  [
    "event-classification-verifier.test.ts",
    watcherEvidence.eventClassificationVerifier?.expectedFocusedTestCount,
  ],
  ["scaffold.test.ts", watcherEvidence.scaffold?.expectedFocusedTestCount],
  [
    "crash-rollback-matrix.test.ts",
    watcherEvidence.crashRollbackMatrix?.expectedFocusedTestCount,
  ],
]);

const fail = (message) => {
  throw new Error(`Watcher focused-test verification failed: ${message}`);
};

if (
  expectedByFile.size !== 20 ||
  [...expectedByFile.values()].some(
    (count) => !Number.isSafeInteger(count) || count <= 0,
  )
) {
  fail(
    "dependency map must declare a positive expected count for all 20 files",
  );
}

// #519 finding V-4 (#527): the published 20-file/616-test total used to be a
// `reduce` over the dependency map's own numbers compared against the runner —
// but the per-file expectations came from the same map, so editing the artifact
// under audit moved both sides of every comparison at once. Four suites
// (publicDaClient, canonicalBlockStore, headerRootReconstruction,
// phaseAVerifier — 301 of the 616) additionally had no literal pin in
// verify-canonical-v1-watcher-dependency-map.mjs, so their counts were
// self-declared end to end.
//
// The literals below are pins measured from a package-local Vitest 3.0.7 JSON
// report (`node node_modules/vitest/vitest.mjs run --pool=forks
// --no-file-parallelism --maxWorkers=1 --reporter=json` in
// demo/midgard-watcher). Every published count is now checked three ways: the
// runner report against these pins, the dependency map against these pins, and
// the aggregate against the runner's own numTotalTests. Changing a watcher test
// count requires editing this pin table, and the change only holds if a real
// run agrees.
const pinnedByFile = new Map([
  ["config.test.ts", 42],
  ["deployment-identity.test.ts", 18],
  ["durable-store.test.ts", 12],
  ["l1-adapter.test.ts", 23],
  ["multi-provider-consistency.test.ts", 18],
  ["finality-engine.test.ts", 25],
  ["rollback-engine.test.ts", 26],
  ["state-queue-indexer.test.ts", 19],
  ["user-event-indexer.test.ts", 23],
  ["settlement-indexer.test.ts", 25],
  ["proof-thread-indexer.test.ts", 17],
  ["rule-bundle-v1.test.ts", 9],
  ["public-da-client.test.ts", 102],
  ["canonical-block-store.test.ts", 46],
  ["header-root-reconstruction.test.ts", 59],
  ["phase-a-verifier.test.ts", 94],
  ["block-replay.test.ts", 21],
  ["event-classification-verifier.test.ts", 15],
  ["scaffold.test.ts", 5],
  ["crash-rollback-matrix.test.ts", 17],
]);
const pinnedTotal = 616;
const sumCounts = (counts) => counts.reduce((sum, count) => sum + count, 0);
if (
  pinnedByFile.size !== expectedByFile.size ||
  [...pinnedByFile.keys()].some((name) => !expectedByFile.has(name)) ||
  sumCounts([...pinnedByFile.values()]) !== pinnedTotal
) {
  fail(
    `pinned focused-test counts must cover the same ${String(expectedByFile.size)} files and sum to the pinned total ${String(pinnedTotal)}`,
  );
}
const declarationDrift = [...pinnedByFile]
  .filter(([name, pinned]) => expectedByFile.get(name) !== pinned)
  .map(
    ([name, pinned]) =>
      `${name} declares ${String(expectedByFile.get(name))} against pin ${String(pinned)}`,
  );
if (declarationDrift.length !== 0) {
  fail(
    `dependency map focused-test counts drift from the runner-measured pins: ${declarationDrift.join("; ")}`,
  );
}

const temporaryDirectory = mkdtempSync(
  join(tmpdir(), "midgard-watcher-focused-tests-"),
);
const reportPath = join(temporaryDirectory, "vitest.json");
try {
  const run = spawnSync(
    "pnpm",
    [
      "--dir",
      "demo/midgard-watcher",
      "exec",
      "vitest",
      "run",
      "--pool=forks",
      "--no-file-parallelism",
      "--maxWorkers=1",
      "--reporter=json",
      `--outputFile=${reportPath}`,
    ],
    {
      cwd: repositoryRoot,
      encoding: "utf8",
      maxBuffer: 128 * 1024 * 1024,
    },
  );
  if (run.error !== undefined) {
    throw run.error;
  }
  if (run.status !== 0) {
    process.stderr.write(run.stdout);
    process.stderr.write(run.stderr);
    try {
      const failedReport = JSON.parse(readFileSync(reportPath, "utf8"));
      for (const failedFile of failedReport.testResults ?? []) {
        if (failedFile.status === "passed") {
          continue;
        }
        const name = basename(failedFile.name);
        // Vitest sets file-level `message` to the EMPTY STRING when the
        // failure is at assertion level and the detail lives in
        // assertionResults. `??` does not catch "", so this used to print a
        // bare "settlement-indexer.test.ts:" and discard the only evidence of
        // why CI failed. Report the per-test detail explicitly.
        const fileMessage =
          typeof failedFile.message === "string" && failedFile.message !== ""
            ? failedFile.message
            : null;
        if (fileMessage !== null) {
          process.stderr.write(`${name}: ${fileMessage}\n`);
        }
        const failedAssertions = (failedFile.assertionResults ?? []).filter(
          ({ status }) => status === "failed",
        );
        for (const assertion of failedAssertions) {
          const detail = (assertion.failureMessages ?? []).join("\n  ");
          process.stderr.write(
            `${name} > ${assertion.fullName ?? assertion.title}: ${
              detail === "" ? "failed without a diagnostic" : detail
            }\n`,
          );
        }
        if (fileMessage === null && failedAssertions.length === 0) {
          // Neither a file message nor a failed assertion: the worker died
          // before reporting (crash, OOM, or the runner reclaiming it).
          process.stderr.write(
            `${name}: status="${failedFile.status}" with no failed assertion and no message — ` +
              `the worker exited before reporting. Treat as an infrastructure ` +
              `failure, not a test failure, and re-run before diagnosing.\n`,
          );
        }
      }
    } catch (error) {
      process.stderr.write(
        `Unable to read failed Vitest JSON: ${
          error instanceof Error ? error.message : String(error)
        }\n`,
      );
    }
    fail(`Vitest exited with status ${String(run.status)}`);
  }

  const report = JSON.parse(readFileSync(reportPath, "utf8"));
  // A skipped or todo test still counts toward numTotalTests, so a `.skip`
  // would otherwise keep a pinned count green while running nothing. Checked
  // before the all-passing assertion so the diagnostic names the real cause.
  if (report.numPendingTests !== 0 || report.numTodoTests !== 0) {
    fail(
      `Vitest reported ${String(report.numPendingTests)} skipped and ${String(report.numTodoTests)} todo tests; every pinned test must actually execute`,
    );
  }
  if (
    !Array.isArray(report.testResults) ||
    report.numFailedTests !== 0 ||
    report.numPassedTests !== report.numTotalTests
  ) {
    fail("Vitest JSON must report an exact all-passing run");
  }
  const actualByFile = new Map();
  for (const file of report.testResults) {
    const name = basename(file.name);
    if (
      actualByFile.has(name) ||
      !Array.isArray(file.assertionResults) ||
      file.status !== "passed"
    ) {
      fail(`malformed or duplicate Vitest file result for ${name}`);
    }
    const collected = file.assertionResults.length;
    const passed = file.assertionResults.filter(
      ({ status }) => status === "passed",
    ).length;
    const failed = file.assertionResults.filter(
      ({ status }) => status === "failed",
    ).length;
    if (collected <= 0 || passed !== collected || failed !== 0) {
      fail(`${name} did not collect and pass every declared test`);
    }
    actualByFile.set(name, collected);
  }
  const measuredDrift = [...pinnedByFile]
    .filter(([name, pinned]) => actualByFile.get(name) !== pinned)
    .map(
      ([name, pinned]) =>
        `${name} collected ${String(actualByFile.get(name) ?? 0)} against pin ${String(pinned)}`,
    );
  const unpinnedFiles = [...actualByFile.keys()].filter(
    (name) => !pinnedByFile.has(name),
  );
  if (measuredDrift.length !== 0 || unpinnedFiles.length !== 0) {
    fail(
      `Vitest collected counts do not match the runner-measured pins: ${[
        ...measuredDrift,
        ...unpinnedFiles.map((name) => `${name} has no pinned count`),
      ].join("; ")}`,
    );
  }
  // The published aggregate is the runner's own total, cross-checked against
  // the sum of the per-file results it reported and against the pin. It is
  // never a reduce over the dependency map's numbers.
  const measuredTotal = sumCounts([...actualByFile.values()]);
  if (
    report.numTotalTests !== measuredTotal ||
    report.numPassedTests !== measuredTotal ||
    measuredTotal !== pinnedTotal
  ) {
    fail(
      `runner aggregate ${String(report.numPassedTests)}/${String(report.numTotalTests)} (per-file sum ${String(measuredTotal)}) does not equal the pinned total ${String(pinnedTotal)}`,
    );
  }
  process.stdout.write(
    `Canonical V1 watcher focused tests verified: ${String(actualByFile.size)} files, ${String(report.numPassedTests)}/${String(report.numTotalTests)} passed (runner-measured aggregate equals the pinned total ${String(pinnedTotal)}).\n`,
  );
} finally {
  rmSync(temporaryDirectory, { recursive: true, force: true });
}
