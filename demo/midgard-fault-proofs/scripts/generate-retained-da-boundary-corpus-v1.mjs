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

const packageRoot = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const demoRoot = resolve(packageRoot, "..");
const validationPackageRoot = resolve(demoRoot, "midgard-validation");
const daCommitteePackageRoot = resolve(demoRoot, "da-committee-node");
const temporaryDirectory = mkdtempSync(
  join(tmpdir(), "midgard-retained-da-boundary-corpus-"),
);
const jsonlPath = resolve(temporaryDirectory, "corpus.jsonl");
const regeneratedPath = resolve(temporaryDirectory, "regenerated.json");
const checkedFixturePath = resolve(
  packageRoot,
  "tests/fixtures/retained-da-boundary-corpus-v1.json",
);
const [mode, ...unexpectedArguments] = process.argv.slice(2);
if (
  unexpectedArguments.length !== 0 ||
  (mode !== undefined && mode !== "--check")
) {
  throw new Error(
    "usage: node scripts/generate-retained-da-boundary-corpus-v1.mjs [--check]",
  );
}

const expectedLabels = [
  "balanced-nested-datum",
  "balanced-nested-redeemer",
  "maximum-constructor-datum-breadth",
  "maximum-constructor-redeemer-breadth",
  "maximum-inline-datum-blob",
  "maximum-list-datum-breadth",
  "maximum-list-redeemer-breadth",
  "maximum-map-datum-breadth",
  "maximum-map-redeemer-breadth",
  "maximum-mint-and-native-policies",
  "maximum-nested-value",
  "maximum-observers-and-native-scripts",
  "maximum-outputs",
  "maximum-redeemers",
  "maximum-reference-inputs",
  "maximum-signers-and-witnesses",
  "maximum-spend-inputs",
  "mixed-size-balanced",
];
const plutusScriptLabels = new Set([
  "balanced-nested-redeemer",
  "maximum-constructor-redeemer-breadth",
  "maximum-list-redeemer-breadth",
  "maximum-map-redeemer-breadth",
  "maximum-redeemers",
]);
const resolvedReferenceUtxoLabels = new Set(["maximum-reference-inputs"]);
const baseKeys = [
  "canonicalCborHex",
  "label",
  "productionAdmission",
  "transactionCommitmentHex",
  "transactionIdHex",
];
const canonicalHex = (value) =>
  typeof value === "string" && /^(?:[0-9a-f]{2})+$/.test(value);
const validResolvedReferenceUtxos = (value) => {
  if (!Array.isArray(value) || value.length === 0) {
    return false;
  }
  let previousKey;
  for (const entry of value) {
    if (
      !Array.isArray(entry) ||
      entry.length !== 2 ||
      !canonicalHex(entry[0]) ||
      !canonicalHex(entry[1]) ||
      (previousKey !== undefined && entry[0] <= previousKey)
    ) {
      return false;
    }
    previousKey = entry[0];
  }
  return true;
};

// Reads the producers' JSONL rows and returns the corpus file text.
const buildCorpus = (jsonlPath) => {
  const entries = readFileSync(jsonlPath, "utf8")
    .trim()
    .split("\n")
    .map((line) => JSON.parse(line))
    .sort((left, right) => left.label.localeCompare(right.label));
  const labels = entries.map(({ label }) => label);
  if (JSON.stringify(labels) !== JSON.stringify(expectedLabels)) {
    throw new Error(`boundary corpus labels differ: ${JSON.stringify(labels)}`);
  }
  for (const entry of entries) {
    const expectedKeys = [
      ...baseKeys,
      ...(plutusScriptLabels.has(entry.label) ? ["sourceScriptHash"] : []),
      ...(resolvedReferenceUtxoLabels.has(entry.label)
        ? ["resolvedReferenceUtxos"]
        : []),
    ].sort();
    const actualKeys = Object.keys(entry).sort();
    const expectedProductionAdmission =
      entry.label === "mixed-size-balanced"
        ? "diagnostic-synthetic-script-witnesses"
        : "required";
    if (
      JSON.stringify(actualKeys) !== JSON.stringify(expectedKeys) ||
      entry.productionAdmission !== expectedProductionAdmission ||
      !canonicalHex(entry.canonicalCborHex) ||
      !/^[0-9a-f]{64}$/.test(entry.transactionIdHex) ||
      !/^[0-9a-f]{64}$/.test(entry.transactionCommitmentHex) ||
      (plutusScriptLabels.has(entry.label) &&
        !/^[0-9a-f]{56}$/.test(entry.sourceScriptHash)) ||
      (resolvedReferenceUtxoLabels.has(entry.label) &&
        !validResolvedReferenceUtxos(entry.resolvedReferenceUtxos))
    ) {
      throw new Error(`invalid corpus row ${entry.label}`);
    }
  }
  return `${JSON.stringify(
    { schema: "midgard-retained-da-boundary-corpus-v1", entries },
    null,
    2,
  )}\n`;
};

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
      "--maxWorkers=1",
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
  // invocation owns a private temporary directory, so concurrent runs cannot
  // mix their rows.
  writeFileSync(jsonlPath, "", "utf8");

  runFilteredVitest({
    label: "producer",
    packageDirectory: validationPackageRoot,
    testCounts: {
      "tests/ordered-collection-boundary.test.ts": 1,
      "tests/ordered-collection-signer-witness-boundary.test.ts": 1,
      "tests/ordered-collection-spend-inputs-boundary.test.ts": 1,
      "tests/ordered-collection-reference-inputs-boundary.test.ts": 1,
      "tests/ordered-collection-observer-native-script-boundary.test.ts": 1,
      "tests/ordered-collection-mint-boundary.test.ts": 2,
      "tests/ordered-collection-redeemer-boundary.test.ts": 1,
      "tests/blob-chunk-boundary.test.ts": 1,
      // The boundary case plus the complete-Value carriage-fit
      // case that must be measured before any incremental Value fallback.
      "tests/nested-value-boundary.test.ts": 2,
      "tests/nested-data-boundary.test.ts": 1,
      "tests/nested-redeemer-data-boundary.test.ts": 1,
      "tests/retained-da-boundary.test.ts": 1,
      // Three inline-datum breadth maxima, the genuine
      // redeemer projection, and one complete-Data carriage-fit case per kind.
      "tests/data-breadth-boundary.test.ts": 7,
    },
    extraArguments: ["--testTimeout=360000", "--hookTimeout=60000"],
    timeout: 1_800_000,
    environment: {
      ...process.env,
      NODE_OPTIONS: "--max-old-space-size=4096",
      MIDGARD_BOUNDARY_CORPUS_JSONL: jsonlPath,
    },
  });
  writeFileSync(regeneratedPath, buildCorpus(jsonlPath), "utf8");
  if (mode === "--check") {
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
      "tests/cardano-capability-retained-da-corpus.test.ts": 18,
    },
    extraArguments: ["--testTimeout=180000"],
    timeout: 600_000,
    environment: generatedCorpusEnvironment,
  });
  runFilteredVitest({
    label: "fault-proof-consumer",
    packageDirectory: packageRoot,
    testCounts: {
      "tests/cardano-capability-retained-da.test.ts": 3,
    },
    extraArguments: ["--testTimeout=180000"],
    timeout: 600_000,
    environment: generatedCorpusEnvironment,
  });
  if (mode === undefined) {
    // Promote the regenerated fixture only after every strict production
    // consumer has accepted that exact temporary artifact.
    copyFileSync(regeneratedPath, checkedFixturePath);
    run("cmp", [regeneratedPath, checkedFixturePath]);
  }
} finally {
  rmSync(temporaryDirectory, { recursive: true, force: true });
}
