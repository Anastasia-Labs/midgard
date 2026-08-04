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
  ["scaffold.test.ts", watcherEvidence.scaffold?.expectedFocusedTestCount],
]);

const fail = (message) => {
  throw new Error(`Watcher focused-test verification failed: ${message}`);
};

if (
  expectedByFile.size !== 18 ||
  [...expectedByFile.values()].some(
    (count) => !Number.isSafeInteger(count) || count <= 0,
  )
) {
  fail(
    "dependency map must declare a positive expected count for all 18 files",
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
  if (
    actualByFile.size !== expectedByFile.size ||
    [...expectedByFile].some(
      ([name, expected]) => actualByFile.get(name) !== expected,
    ) ||
    report.numTotalTests !==
      [...expectedByFile.values()].reduce((sum, count) => sum + count, 0)
  ) {
    fail("Vitest collected counts do not match the dependency map");
  }
  process.stdout.write(
    `Canonical V1 watcher focused tests verified: ${String(actualByFile.size)} files, ${String(report.numPassedTests)}/${String(report.numTotalTests)} passed.\n`,
  );
} finally {
  rmSync(temporaryDirectory, { recursive: true, force: true });
}
