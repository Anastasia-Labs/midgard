import { spawn } from "node:child_process";
import { mkdtempSync, readFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

// C23/C24/C25 closure: three inline-datum breadth maxima, one genuine
// redeemer projection, and one complete-Data carriage-fit case per kind that
// measures direct and reference carriage before any bounded fallback.
const EXPECTED_TEST_COUNT = 7;
const REQUIRED_SUITE = "tests/data-breadth-boundary.test.ts";
const packageDirectory = fileURLToPath(new URL("../", import.meta.url));
const reportDirectory = mkdtempSync(
  join(tmpdir(), "midgard-data-breadth-report-"),
);
const reportPath = join(reportDirectory, "vitest.json");

const child = spawn(
  "vitest",
  [
    "run",
    REQUIRED_SUITE,
    "--pool=forks",
    "--poolOptions.forks.singleFork=true",
    "--testTimeout=360000",
    "--hookTimeout=60000",
    "--reporter=json",
    `--outputFile=${reportPath}`,
  ],
  {
    cwd: packageDirectory,
    env: process.env,
    stdio: ["ignore", "inherit", "inherit"],
  },
);

const status = await new Promise((resolve, reject) => {
  child.once("error", reject);
  child.once("close", (code) => resolve(code));
});

let report;
try {
  report = JSON.parse(readFileSync(reportPath, "utf8"));
} catch (error) {
  throw new Error(
    `Data-breadth Vitest report was not valid JSON: ${
      error instanceof Error ? error.message : String(error)
    }`,
  );
} finally {
  rmSync(reportDirectory, { recursive: true, force: true });
}

if (status !== 0) {
  throw new Error(
    `Data-breadth Vitest run failed with status ${String(status)}: ${JSON.stringify(
      {
        numTotalTests: report.numTotalTests,
        numPassedTests: report.numPassedTests,
        numFailedTests: report.numFailedTests,
        numPendingTests: report.numPendingTests,
        failures: report.testResults?.flatMap((suite) =>
          suite.assertionResults
            ?.filter((assertion) => assertion.status === "failed")
            .map((assertion) => ({
              title: assertion.fullName ?? assertion.title,
              failureMessages: assertion.failureMessages,
            })),
        ),
      },
    )}`,
  );
}

const suite = report.testResults?.[0];
const assertionResults = suite?.assertionResults;
const exactPassingResult =
  report.success === true &&
  report.numTotalTestSuites > 0 &&
  report.numPassedTestSuites === report.numTotalTestSuites &&
  report.numFailedTestSuites === 0 &&
  report.numPendingTestSuites === 0 &&
  (report.numRuntimeErrorTestSuites ?? 0) === 0 &&
  report.numTotalTests === EXPECTED_TEST_COUNT &&
  report.numPassedTests === EXPECTED_TEST_COUNT &&
  report.numFailedTests === 0 &&
  report.numPendingTests === 0 &&
  report.numTodoTests === 0 &&
  report.testResults?.length === 1 &&
  typeof suite?.name === "string" &&
  suite.name.endsWith(REQUIRED_SUITE) &&
  assertionResults?.length === EXPECTED_TEST_COUNT &&
  assertionResults.every((assertion) => assertion.status === "passed");

if (!exactPassingResult) {
  throw new Error(
    `Data-breadth gate requires exactly ${EXPECTED_TEST_COUNT.toString()} passing tests in ${REQUIRED_SUITE}; received ${JSON.stringify(
      {
        success: report.success,
        numTotalTestSuites: report.numTotalTestSuites,
        numPassedTestSuites: report.numPassedTestSuites,
        numFailedTestSuites: report.numFailedTestSuites,
        numPendingTestSuites: report.numPendingTestSuites,
        numRuntimeErrorTestSuites: report.numRuntimeErrorTestSuites,
        numTotalTests: report.numTotalTests,
        numPassedTests: report.numPassedTests,
        numFailedTests: report.numFailedTests,
        numPendingTests: report.numPendingTests,
        numTodoTests: report.numTodoTests,
        suiteCount: report.testResults?.length,
        suiteName: suite?.name,
        assertionCount: assertionResults?.length,
        assertionStatuses: assertionResults?.map(
          (assertion) => assertion.status,
        ),
      },
    )}`,
  );
}

process.stdout.write(
  `Data-breadth gate passed: ${EXPECTED_TEST_COUNT.toString()}/${EXPECTED_TEST_COUNT.toString()} tests in ${REQUIRED_SUITE}\n`,
);
