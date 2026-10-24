import {
  evaluateClosureIdentity,
  evaluateExactClosureIdentityShape,
  evaluateExactSourceIdentityShape,
  isCanonicalAbsolutePath,
} from "./phase3-architecture-g-closure-lib.mjs";
import {
  exactShape,
  finite,
  validateCorpusPreflightSummaryShape,
  validateIsolationSummaryShape,
  validateNodeRevalidationSummaryShape,
  validateSampleShape,
} from "./verify-phase3-architecture-g-soak-report.validate-sample-shape.mjs";
import { validateWorkloadSummaryShape } from "./verify-phase3-architecture-g-soak-report.validate-workload-summary-shape.mjs";

export const validateSoakReportShape = (report, reasons) => {
  exactShape(
    report,
    [
      "schemaVersion",
      "scenario",
      "testOnly",
      "configuredDurationSec",
      "sampleIntervalMs",
      "startedAtMs",
      "completedAtMs",
      "preflight",
      "identity",
      "sourceAtCompletion",
      "observation",
      "workload",
      "termination",
      "samples",
    ],
    "Phase 3 soak report",
    reasons,
  );
  if (typeof report?.testOnly !== "boolean") {
    reasons.push("testOnly must be an explicit V1 boolean");
  }
  for (const [label, value] of [
    ["configuredDurationSec", report?.configuredDurationSec],
    ["sampleIntervalMs", report?.sampleIntervalMs],
    ["completedAtMs", report?.completedAtMs],
  ]) {
    if (!Number.isSafeInteger(value)) {
      reasons.push(`${label} must be a canonical safe integer`);
    }
  }
  if (
    !(report?.startedAtMs === null || Number.isSafeInteger(report?.startedAtMs))
  ) {
    reasons.push("startedAtMs must be canonical epoch milliseconds or null");
  }
  if (report?.preflight !== null) {
    exactShape(
      report?.preflight,
      [
        "startedAtMs",
        "completedAtMs",
        "durationMs",
        "lifecycleStartedAtMs",
        "initialReadiness",
        "nodePreLifecycleRevalidation",
      ],
      "soak preflight timing",
      reasons,
    );
    for (const [label, value] of [
      ["startedAtMs", report?.preflight?.startedAtMs],
      ["completedAtMs", report?.preflight?.completedAtMs],
      ["durationMs", report?.preflight?.durationMs],
      ["lifecycleStartedAtMs", report?.preflight?.lifecycleStartedAtMs],
    ]) {
      if (!Number.isSafeInteger(value)) {
        reasons.push(`preflight ${label} must be a canonical safe integer`);
      }
    }
    validateSampleShape(
      report?.preflight?.initialReadiness,
      "preflight initial readiness",
      reasons,
    );
    validateNodeRevalidationSummaryShape(
      report?.preflight?.nodePreLifecycleRevalidation,
      reasons,
    );
  }
  if (report?.identity !== null) {
    reasons.push(
      ...evaluateExactClosureIdentityShape(report?.identity, [
        "corpusPreflight",
        "loadGeneratorIsolation",
        "nodePreLifecycleRevalidation",
      ]),
    );
    validateCorpusPreflightSummaryShape(
      report?.identity?.corpusPreflight,
      reasons,
    );
    validateIsolationSummaryShape(
      report?.identity?.loadGeneratorIsolation,
      reasons,
    );
    validateNodeRevalidationSummaryShape(
      report?.identity?.nodePreLifecycleRevalidation,
      reasons,
    );
  }
  if (report?.sourceAtCompletion !== null) {
    reasons.push(
      ...evaluateExactSourceIdentityShape(
        report?.sourceAtCompletion,
        "completion source identity",
      ),
    );
  }
  exactShape(
    report?.observation,
    [
      "workloadSpawnedAtMs",
      "workloadExitedAtMs",
      "firstSampleAtMs",
      "lastSampleAtMs",
    ],
    "soak observation",
    reasons,
  );
  for (const [label, value] of Object.entries(report?.observation ?? {})) {
    if (!(value === null || Number.isSafeInteger(value))) {
      reasons.push(`observation ${label} must be canonical epoch milliseconds`);
    }
  }
  if (report?.workload !== null) {
    exactShape(
      report?.workload,
      [
        "scriptPath",
        "scriptSha256",
        "reportPath",
        "reportSha256",
        "reportBytes",
        "reportSummary",
        "submitRecords",
      ],
      "soak workload",
      reasons,
    );
    if (
      !isCanonicalAbsolutePath(report?.workload?.scriptPath) ||
      !isCanonicalAbsolutePath(report?.workload?.reportPath)
    ) {
      reasons.push("soak workload paths must be canonical absolute paths");
    }
    validateWorkloadSummaryShape(report?.workload?.reportSummary, reasons);
    exactShape(
      report?.workload?.submitRecords,
      [
        "path",
        "sha256",
        "bytes",
        "recordCount",
        "successCount",
        "errorCount",
        "timeoutCount",
        "attemptSequenceSha256",
      ],
      "submit-record summary",
      reasons,
    );
  }
  const terminationKeys =
    report?.termination?.reason === "setup_failure"
      ? [
          "completed",
          "reason",
          "phase",
          "workloadExitCode",
          "workloadSignal",
          "earlyExit",
          "error",
        ]
      : [
          "completed",
          "reason",
          "workloadExitCode",
          "workloadSignal",
          "earlyExit",
          "error",
        ];
  exactShape(report?.termination, terminationKeys, "soak termination", reasons);
  for (const [index, sample] of (Array.isArray(report?.samples)
    ? report.samples
    : []
  ).entries()) {
    validateSampleShape(sample, `sample ${index.toString()}`, reasons);
  }
};

export const slopePerSecond = (samples, select) => {
  if (samples.length < 2) return null;
  let points;
  try {
    points = samples.map((sample) => ({
      x: sample?.elapsedMs / 1_000,
      y: select(sample),
    }));
  } catch {
    return null;
  }
  if (points.some(({ x, y }) => finite(x) === null || finite(y) === null)) {
    return null;
  }
  const xMean = points.reduce((sum, point) => sum + point.x, 0) / points.length;
  const yMean = points.reduce((sum, point) => sum + point.y, 0) / points.length;
  let numerator = 0;
  let denominator = 0;
  for (const point of points) {
    numerator += (point.x - xMean) * (point.y - yMean);
    denominator += (point.x - xMean) ** 2;
  }
  return denominator > 0 ? numerator / denominator : null;
};

export const maximumFinite = (samples, select) => {
  let values;
  try {
    values = samples.map(select);
  } catch {
    return null;
  }
  return values.length > 0 && values.every((value) => finite(value) !== null)
    ? Math.max(...values)
    : null;
};

export const identityReasons = (identity) => {
  return evaluateClosureIdentity(identity);
};

export const counterDelta = (samples, field, reasons) => {
  const values = samples.map((sample) => finite(sample?.metrics?.[field]));
  if (values.some((value) => value === null)) {
    reasons.push(`${field} is missing or non-finite`);
    return null;
  }
  for (let index = 1; index < values.length; index += 1) {
    if (values[index] < values[index - 1]) {
      reasons.push(`${field} reset during the soak`);
      return null;
    }
  }
  return values.at(-1) - values[0];
};
