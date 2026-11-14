import { exactShape } from "./verify-phase3-architecture-g-soak-report.validate-sample-shape.mjs";

export const validateWorkloadSummaryShape = (summary, reasons) => {
  exactShape(
    summary,
    [
      "scenario",
      "scenarioClass",
      "benchmarkMode",
      "formalBenchmark",
      "targetAcceptedTps",
      "openLoopRateTps",
      "measuredDurationSec",
      "warmupTxs",
      "warmupSec",
      "cooldownSec",
      "drainTimeoutSec",
      "offeredRateMinRatio",
      "acceptedRateMinRatio",
      "nodeSaturationMinRatio",
      "loadGenerator",
      "calibration",
      "corpus",
      "measuredElapsedSec",
      "offeredRatePerSec",
      "acceptedRatePerSec",
      "submitted",
      "logicalSubmitAttempts",
      "physicalSubmitAttempts",
      "submitErrors",
      "rejectedDelta",
      "missingRequiredMetrics",
      "allPrimaryStagesPassed",
      "allPrimaryDrainsCompleted",
      "primaryStageMeasurements",
    ],
    "workload summary",
    reasons,
  );
  exactShape(
    summary?.loadGenerator,
    ["placement", "cohosted", "clockOffsetMs", "isolation"],
    "workload load-generator summary",
    reasons,
  );
  exactShape(
    summary?.corpus,
    [
      "path",
      "indexPath",
      "manifestPath",
      "sliceId",
      "shape",
      "validation",
      "artifactIdentity",
      "preflight",
      "consumption",
    ],
    "workload corpus summary",
    reasons,
  );
  exactShape(
    summary?.corpus?.validation,
    ["rowCount", "uniqueTxHashes", "uniqueSelectedInputs"],
    "workload corpus validation",
    reasons,
  );
  exactShape(
    summary?.corpus?.artifactIdentity,
    [
      "corpusSha256",
      "indexSha256",
      "manifestSha256",
      "manifestExpectedCorpusSha256",
      "manifestExpectedIndexSha256",
      "manifestMatchesArtifacts",
    ],
    "workload corpus artifact identity",
    reasons,
  );
  exactShape(
    summary?.corpus?.preflight,
    [
      "path",
      "sha256",
      "bytes",
      "schemaVersion",
      "sourceTreeSha256",
      "sourceIdentitySha256",
      "phase1BindingSha256",
    ],
    "workload corpus-preflight identity",
    reasons,
  );
  exactShape(
    summary?.corpus?.consumption,
    ["schemaVersion", "rowCount", "chains"],
    "workload corpus consumption",
    reasons,
  );
  for (const [index, chain] of (Array.isArray(
    summary?.corpus?.consumption?.chains,
  )
    ? summary.corpus.consumption.chains
    : []
  ).entries()) {
    exactShape(
      chain,
      ["chainIndex", "chainId", "rowCount", "prefixSha256"],
      `workload corpus consumption chain ${index.toString()}`,
      reasons,
    );
  }
  for (const [index, stage] of (Array.isArray(summary?.primaryStageMeasurements)
    ? summary.primaryStageMeasurements
    : []
  ).entries()) {
    exactShape(
      stage,
      [
        "name",
        "targetRateTps",
        "startedAtMs",
        "endedAtMs",
        "measuredElapsedSec",
        "logicalSubmitAttempts",
        "physicalSubmitAttempts",
        "submitted",
        "submitErrors",
        "offeredRatePerSec",
        "acceptedRatePerSec",
        "nodeSaturationRatio",
        "nodeSaturationMinRatio",
        "nodeSaturationPassed",
        "drainCompleted",
        "drainElapsedMs",
      ],
      `primary workload stage ${index.toString()}`,
      reasons,
    );
    if (
      !Number.isSafeInteger(stage?.startedAtMs) ||
      !Number.isSafeInteger(stage?.endedAtMs)
    ) {
      reasons.push(
        `primary workload stage ${index.toString()} timestamps are not canonical`,
      );
    }
  }
};
