import { spawnSync } from "node:child_process";
import {
  copyFileSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const fixtureDirectory = dirname(fileURLToPath(import.meta.url));
const packageRoot = resolve(fixtureDirectory, "../..");
const demoRoot = resolve(packageRoot, "..");
const validationPackageRoot = resolve(demoRoot, "midgard-validation");
const daCommitteePackageRoot = resolve(demoRoot, "da-committee-node");
const temporaryDirectory = mkdtempSync(
  join(tmpdir(), "midgard-cardano-p2-retained-da-"),
);
const jsonlPath = resolve(temporaryDirectory, "corpus.jsonl");
const regeneratedPath = resolve(temporaryDirectory, "regenerated.json");
const checkedFixturePath = resolve(
  fixtureDirectory,
  "cardano-capability-p2-boundary-corpus-v1.json",
);
const converterPath = resolve(
  fixtureDirectory,
  "build-cardano-capability-p2-boundary-corpus-v1.mjs",
);
const [mode, ...unexpectedArguments] = process.argv.slice(2);
if (
  unexpectedArguments.length !== 0 ||
  (mode !== undefined && mode !== "--update")
) {
  throw new Error(
    "usage: node verify-cardano-capability-p2-retained-da-v1.mjs [--update]",
  );
}

const run = (
  command,
  arguments_,
  environment = process.env,
  workingDirectory = demoRoot,
  allowNonzero = false,
  timeout,
) => {
  const result = spawnSync(command, arguments_, {
    cwd: workingDirectory,
    env: environment,
    stdio: "inherit",
    timeout,
  });
  if (result.error !== undefined) {
    throw result.error;
  }
  if (result.status !== 0 && !allowNonzero) {
    throw new Error(
      `${command} ${arguments_.join(" ")} failed with status ${String(
        result.status,
      )}`,
    );
  }
  return result;
};

const runFilteredVitest = ({
  label,
  packageDirectory,
  testCounts,
  extraArguments = [],
  environment = process.env,
  timeout,
}) => {
  const reportPath = resolve(temporaryDirectory, `${label}.vitest.json`);
  const testFiles = Object.keys(testCounts);
  const expectedTestCount = Object.values(testCounts).reduce(
    (total, count) => total + count,
    0,
  );
  if (
    testFiles.length === 0 ||
    expectedTestCount <= 0 ||
    Object.values(testCounts).some(
      (count) => !Number.isSafeInteger(count) || count <= 0,
    )
  ) {
    throw new Error(
      `${label}: expected test counts must be exact and positive`,
    );
  }

  const execution = run(
    process.execPath,
    [
      resolve(packageDirectory, "node_modules/vitest/vitest.mjs"),
      "run",
      ...testFiles,
      "--pool=forks",
      "--poolOptions.forks.singleFork=true",
      ...extraArguments,
      "--reporter=json",
      `--outputFile=${reportPath}`,
    ],
    environment,
    packageDirectory,
    true,
    timeout,
  );

  const report = JSON.parse(readFileSync(reportPath, "utf8"));
  const exactOverallCounts =
    report.success === true &&
    report.numTotalTests === expectedTestCount &&
    report.numPassedTests === expectedTestCount &&
    report.numFailedTests === 0 &&
    report.numPendingTests === 0 &&
    Number.isSafeInteger(report.numTotalTestSuites) &&
    report.numTotalTestSuites > 0 &&
    report.numPassedTestSuites === report.numTotalTestSuites &&
    report.numFailedTestSuites === 0 &&
    report.numPendingTestSuites === 0 &&
    Array.isArray(report.testResults) &&
    report.testResults.length === testFiles.length;
  if (!exactOverallCounts) {
    const failedTests = Array.isArray(report.testResults)
      ? report.testResults.flatMap((result) =>
          Array.isArray(result.assertionResults)
            ? result.assertionResults
                .filter((assertion) => assertion.status !== "passed")
                .map(
                  (assertion) =>
                    `${String(result.name)} :: ${String(assertion.fullName ?? assertion.title)}`,
                )
            : [],
        )
      : [];
    throw new Error(
      `${label}: expected exactly ${String(expectedTestCount)} collected and passed tests across ${String(testFiles.length)} filtered files; actual total=${String(report.numTotalTests)} passed=${String(report.numPassedTests)} failed=${String(report.numFailedTests)} pending=${String(report.numPendingTests)}; non-passing=${JSON.stringify(failedTests)}`,
    );
  }
  if (execution.status !== 0) {
    throw new Error(
      `${label}: Vitest exited with status ${String(execution.status)} despite exact passing report counts`,
    );
  }

  const resultsByPath = new Map(
    report.testResults.map((result) => [resolve(result.name), result]),
  );
  for (const [testFile, expectedCount] of Object.entries(testCounts)) {
    const result = resultsByPath.get(resolve(packageDirectory, testFile));
    const assertionResults = result?.assertionResults;
    const exactFileCounts =
      result?.status === "passed" &&
      Array.isArray(assertionResults) &&
      assertionResults.length === expectedCount &&
      assertionResults.every((assertion) => assertion.status === "passed");
    if (!exactFileCounts) {
      throw new Error(
        `${label}: ${testFile} must collect and pass exactly ${String(expectedCount)} tests`,
      );
    }
  }
};

try {
  // Producers append because separate test files contribute rows. Each
  // invocation owns a private temporary directory so concurrent verification
  // cannot cross-contaminate either artifact.
  writeFileSync(jsonlPath, "", "utf8");
  writeFileSync(regeneratedPath, "", "utf8");

  runFilteredVitest({
    label: "producer",
    packageDirectory: validationPackageRoot,
    testCounts: {
      "tests/ordered-collection-boundary-v1.test.ts": 1,
      "tests/ordered-collection-signer-witness-boundary-v1.test.ts": 1,
      "tests/ordered-collection-spend-inputs-boundary-v1.test.ts": 1,
      "tests/ordered-collection-reference-inputs-boundary-v1.test.ts": 1,
      "tests/ordered-collection-observer-native-script-boundary-v1.test.ts": 1,
      "tests/ordered-collection-mint-boundary-v1.test.ts": 1,
      "tests/ordered-collection-redeemer-boundary-v1.test.ts": 1,
      "tests/blob-chunk-boundary-v1.test.ts": 1,
      "tests/nested-value-boundary-v1.test.ts": 1,
      "tests/nested-data-boundary-v1.test.ts": 1,
      "tests/nested-redeemer-data-boundary-v1.test.ts": 1,
      "tests/retained-da-boundary-v1.test.ts": 1,
      "tests/data-breadth-boundary-v1.test.ts": 2,
    },
    extraArguments: ["--testTimeout=360000", "--hookTimeout=60000"],
    timeout: 1_800_000,
    environment: {
      ...process.env,
      NODE_OPTIONS: "--max-old-space-size=4096",
      MIDGARD_BOUNDARY_CORPUS_JSONL: jsonlPath,
    },
  });
  run(process.execPath, [converterPath, jsonlPath, regeneratedPath]);
  if (mode === undefined) {
    run("cmp", [regeneratedPath, checkedFixturePath]);
  }
  const generatedCorpusEnvironment = {
    ...process.env,
    MIDGARD_BOUNDARY_CORPUS_JSON: regeneratedPath,
  };
  runFilteredVitest({
    label: "da-committee-consumer",
    packageDirectory: daCommitteePackageRoot,
    testCounts: {
      "tests/cardano-capability-retained-da-corpus-v1.test.ts": 20,
    },
    extraArguments: ["--testTimeout=180000"],
    timeout: 600_000,
    environment: generatedCorpusEnvironment,
  });
  runFilteredVitest({
    label: "fault-proof-consumer",
    packageDirectory: packageRoot,
    testCounts: {
      "tests/cardano-capability-retained-da-v1.test.ts": 3,
    },
    extraArguments: ["--testTimeout=180000"],
    timeout: 600_000,
    environment: generatedCorpusEnvironment,
  });
  if (mode === "--update") {
    // Promote the regenerated fixture only after every strict production
    // consumer has accepted that exact temporary artifact.
    copyFileSync(regeneratedPath, checkedFixturePath);
    run("cmp", [regeneratedPath, checkedFixturePath]);
  }
} finally {
  rmSync(temporaryDirectory, { recursive: true, force: true });
}
