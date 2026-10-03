import { existsSync, readFileSync } from "node:fs";
import { resolve } from "node:path";
import { artifactChannels, channelIdentity } from "./channels.mjs";

import {
  atomicJson,
  hashFiles,
  inputIdentity,
  json,
  outputIdentity,
  packageByName,
  sha256,
} from "./files.mjs";

export const countsFromVitest = (report, testName, selectedFiles) => {
  if (!Array.isArray(report.testResults))
    throw new Error("missing vitest testResults array");
  if (
    selectedFiles &&
    JSON.stringify(
      [...new Set(report.testResults.map((suite) => suite.name))].sort(),
    ) !== JSON.stringify([...new Set(selectedFiles)].sort())
  )
    throw new Error(
      "vitest collected different files than the explicit selection",
    );
  const assertions = report.testResults.flatMap(
    (suite) => suite.assertionResults ?? [],
  );
  const counts = {
    passed: 0,
    failed: 0,
    skipped: 0,
    filtered: 0,
    todo: 0,
    setupErrors: 0,
    executed: 0,
  };
  const selector = testName ? new RegExp(testName) : undefined;
  for (const assertion of assertions) {
    if (assertion.status === "passed") counts.passed += 1;
    else if (assertion.status === "failed") counts.failed += 1;
    else if (["pending", "skipped", "disabled"].includes(assertion.status)) {
      if (
        selector &&
        !selector.test(
          assertion.fullName ??
            [...(assertion.ancestorTitles ?? []), assertion.title].join(" "),
        )
      )
        counts.filtered += 1;
      else counts.skipped += 1;
    } else if (assertion.status === "todo") counts.todo += 1;
    else throw new Error(`unrecognized test result status ${assertion.status}`);
  }
  counts.executed = counts.passed + counts.failed;
  counts.setupErrors = Number(report.numRuntimeErrorTestSuites ?? 0);
  if (report.success !== true && counts.failed === 0)
    counts.setupErrors = Math.max(1, counts.setupErrors);
  if (
    Number(report.numPassedTests) !== counts.passed ||
    Number(report.numFailedTests) !== counts.failed
  )
    throw new Error("vitest totals disagree with executed assertions");
  return counts;
};

export const writeReceipt = ({
  root,
  pkg,
  directory,
  kind,
  before,
  after,
  steps,
  reportPath,
  testName,
  selectedFiles,
  proofKind = "candidate-pass",
}) => {
  let counts;
  let reportHash;
  let reportError;
  if (reportPath) {
    try {
      const data = readFileSync(reportPath);
      counts = countsFromVitest(JSON.parse(data), testName, selectedFiles);
      reportHash = sha256(data);
    } catch (error) {
      reportError = error.message;
    }
  }
  const failedStep =
    steps.length === 0 ||
    steps.some((step) => step.exitCode !== 0 || step.signal || step.reason);
  const status =
    failedStep ||
    reportError ||
    before.sha256 !== after.sha256 ||
    (counts &&
      (counts.executed === 0 || counts.failed > 0 || counts.setupErrors > 0))
      ? "failed"
      : counts && (counts.skipped > 0 || counts.todo > 0)
        ? "incomplete"
        : "passed";
  const receipt = {
    schema: "midgard-contrib-receipt/v1",
    kind,
    proofKind,
    package: pkg.name,
    root,
    status,
    inputs: before,
    finalInputSha256: after.sha256,
    counts,
    testName,
    selectedFiles,
    reportError,
    steps: steps.map((step) => ({
      ...step,
      logSha256: sha256(readFileSync(step.logPath)),
    })),
    report: reportHash ? { path: reportPath, sha256: reportHash } : undefined,
    createdAt: new Date().toISOString(),
  };
  const path = resolve(directory, "receipt.json");
  atomicJson(path, receipt);
  return {
    ...receipt,
    path,
    exitCode: status === "passed" ? 0 : status === "incomplete" ? 3 : 1,
  };
};

export const verifyReceipt = (root, path) => {
  const receipt = json(path);
  if (
    receipt.schema !== "midgard-contrib-receipt/v1" ||
    ![
      "build",
      "native-build",
      "test",
      "gate",
      "artifact",
      "boundary",
      "reproduce",
      "devnet-generation",
      "acceptance-execution",
      "preflight-step",
    ].includes(receipt.kind)
  )
    throw new Error("unknown receipt schema/kind");
  if (receipt.status !== "passed" || receipt.steps.length === 0)
    throw new Error(`receipt is ${receipt.status}, not a completed pass`);
  if (
    receipt.inputs.sha256 !== inputIdentity(root, receipt.package).sha256 ||
    receipt.finalInputSha256 !== receipt.inputs.sha256
  )
    throw new Error(
      "receipt inputs are stale or changed during execution; rerun its exact command",
    );
  if (receipt.channel) {
    const channel = artifactChannels(root).channels.find(
      (entry) => entry.id === receipt.channel.id,
    );
    if (
      !channel ||
      channelIdentity(root, channel).sha256 !== receipt.channel.inputs.sha256
    )
      throw new Error(
        "artifact channel inputs changed; rerun the declared check",
      );
  }
  for (const artifact of receipt.artifacts ?? []) {
    const pkg = packageByName(root, artifact.name);
    if (
      !artifact.outputs ||
      artifact.outputs.sha256 !==
        outputIdentity(root, `${pkg.directory}/dist`).sha256
    )
      throw new Error(`receipt artifact changed: ${artifact.name}`);
  }
  for (const artifact of receipt.retainedArtifacts ?? [])
    if (
      artifact.outputs.sha256 !==
      outputIdentity(artifact.root, artifact.directory).sha256
    )
      throw new Error("retained generator artifact changed");
  if (
    receipt.outputs &&
    receipt.kind === "native-build" &&
    receipt.outputs.sha256 !==
      hashFiles(root, Object.keys(receipt.outputs.files)).sha256
  )
    throw new Error("native receipt binary changed");
  for (const artifact of receipt.nativeArtifacts ?? [])
    if (
      artifact.outputs.sha256 !==
      hashFiles(root, Object.keys(artifact.outputs.files)).sha256
    )
      throw new Error(`native receipt artifact changed: ${artifact.name}`);
  for (const execution of receipt.testExecutions ?? []) {
    if (
      !execution.report ||
      sha256(readFileSync(execution.report.path)) !== execution.report.sha256
    )
      throw new Error("artifact validation report changed");
    const counts = countsFromVitest(
      json(execution.report.path),
      undefined,
      execution.selectedFiles,
    );
    if (
      !counts.executed ||
      counts.failed ||
      counts.setupErrors ||
      counts.skipped ||
      counts.todo
    )
      throw new Error("artifact validation did not complete nonzero tests");
  }
  for (const entry of receipt.evidenceFiles ?? [])
    if (!entry.sha256 || sha256(readFileSync(entry.path)) !== entry.sha256)
      throw new Error("measurement evidence missing/changed");
  if (
    receipt.generatedOutputs &&
    receipt.generatedOutputs.sha256 !==
      hashFiles(root, Object.keys(receipt.generatedOutputs.files)).sha256
  )
    throw new Error("generated output evidence changed");
  for (const step of receipt.steps) {
    if (
      step.exitCode !== 0 ||
      step.signal ||
      step.reason ||
      !step.startedAt ||
      !step.endedAt ||
      Date.parse(step.endedAt) < Date.parse(step.startedAt)
    )
      throw new Error("incomplete/failed step");
    if (
      !existsSync(step.logPath) ||
      sha256(readFileSync(step.logPath)) !== step.logSha256
    )
      throw new Error("step log is missing or changed");
  }
  if (
    receipt.kind === "build" &&
    receipt.steps.every((step) =>
      step.argv.some((arg) => ["--version", "-v", "--help"].includes(arg)),
    )
  )
    throw new Error("a version/help probe cannot prove a build");
  if (receipt.kind === "test") {
    if (
      !receipt.report ||
      sha256(readFileSync(receipt.report.path)) !== receipt.report.sha256
    )
      throw new Error("test report missing/changed");
    if (receipt.proofKind === "live-acceptance")
      throw new Error("synthetic tests are not live acceptance");
    const counts = countsFromVitest(
      json(receipt.report.path),
      receipt.testName,
      receipt.selectedFiles,
    );
    if (
      counts.executed === 0 ||
      counts.failed ||
      counts.setupErrors ||
      counts.skipped ||
      counts.todo ||
      JSON.stringify(counts) !== JSON.stringify(receipt.counts)
    )
      throw new Error(
        "test report does not prove a complete nonzero execution",
      );
  }
  return receipt;
};
