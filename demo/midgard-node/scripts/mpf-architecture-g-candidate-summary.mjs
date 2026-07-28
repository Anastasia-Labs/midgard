import { isAbsolute, resolve } from "node:path";

import {
  validateArchitectureGCrossGateEvidenceIdentity,
  validateArchitectureGCrossGateSourceIdentity,
  validateArchitectureGPhase1FormalBindingIdentity,
  validateArchitectureGRuntimeIdentity,
  validateCommitCandidateProbeResult,
} from "./mpf-architecture-g-gate-config.mjs";

const exactObject = (value, keys, label) => {
  if (
    value === null ||
    typeof value !== "object" ||
    Array.isArray(value) ||
    JSON.stringify(Object.keys(value).sort()) !==
      JSON.stringify([...keys].sort())
  ) {
    throw new Error(`${label} must contain exactly: ${keys.join(", ")}`);
  }
  return value;
};

const jsonEqual = (left, right) =>
  JSON.stringify(left) === JSON.stringify(right);

const isHash = (value) =>
  typeof value === "string" && /^[0-9a-f]{64}$/u.test(value);

const isCanonicalAbsolutePath = (value) =>
  typeof value === "string" &&
  value.length > 0 &&
  isAbsolute(value) &&
  resolve(value) === value;

const isPositiveSafeInteger = (value) =>
  Number.isSafeInteger(value) && value > 0;

const percentile = (values, quantile) => {
  const sorted = [...values].sort((left, right) => left - right);
  return sorted[Math.max(0, Math.ceil(sorted.length * quantile) - 1)];
};

const validateRootGateIdentity = ({
  value,
  phase1FormalBinding,
  runtimeIdentity,
}) => {
  exactObject(
    value,
    [
      "path",
      "sha256",
      "sourceSha256",
      "diffSha256",
      "gitStatusSha256",
      "phase1FormalBinding",
      "runtimeIdentity",
      "expectedSourceIdentity",
      "currentSourceIdentity",
    ],
    "Architecture G candidate root-gate identity",
  );
  if (
    !isCanonicalAbsolutePath(value.path) ||
    ![
      value.sha256,
      value.sourceSha256,
      value.diffSha256,
      value.gitStatusSha256,
    ].every(isHash)
  ) {
    throw new Error("Architecture G candidate root-gate identity is invalid");
  }
  validateArchitectureGPhase1FormalBindingIdentity(value.phase1FormalBinding);
  validateArchitectureGRuntimeIdentity({
    identity: value.runtimeIdentity,
    expectedVersion: value.runtimeIdentity?.version,
    expectedExecutableSha256: value.runtimeIdentity?.executableSha256,
  });
  validateArchitectureGCrossGateEvidenceIdentity({
    expected: phase1FormalBinding,
    current: value.phase1FormalBinding,
    label: "candidate summary Phase 1 formal binding",
  });
  validateArchitectureGCrossGateEvidenceIdentity({
    expected: runtimeIdentity,
    current: value.runtimeIdentity,
    label: "candidate summary runtime",
  });
  validateArchitectureGCrossGateSourceIdentity({
    expected: value.expectedSourceIdentity,
    current: value.currentSourceIdentity,
  });
  if (
    value.sourceSha256 !== value.expectedSourceIdentity.sourceSha256 ||
    value.diffSha256 !== value.expectedSourceIdentity.diffSha256 ||
    value.gitStatusSha256 !== value.expectedSourceIdentity.gitStatusSha256
  ) {
    throw new Error(
      "Architecture G candidate root-gate source identity is inconsistent",
    );
  }
  return value;
};

const validateFixtureIdentity = ({ value, fixtureSize, label }) => {
  exactObject(value, ["path", "marker", "records", "logicalSha256"], label);
  if (
    !isCanonicalAbsolutePath(value.path) ||
    !isHash(value.marker) ||
    !isHash(value.logicalSha256) ||
    value.records !== fixtureSize + 1
  ) {
    throw new Error(`${label} is invalid`);
  }
  return value;
};

const validateGroup = ({
  group,
  fixtureSize,
  runs,
  transactions,
  cpuSet,
  probePath,
  probeSha256,
}) => {
  exactObject(
    group,
    [
      "fixtureSize",
      "inputPath",
      "inputSha256",
      "corpusSha256",
      "corpusSliceSha256",
      "fundingMapSha256",
      "fixtureCreationSha256",
      "baseUtxoPayloadAggregate",
      "binarySha256",
      "fixtureBefore",
      "fixtureAfter",
      "roots",
      "durations",
      "results",
    ],
    "Architecture G candidate fixture group",
  );
  exactObject(
    group.baseUtxoPayloadAggregate,
    ["entryCount", "encodedTupleBytes"],
    "Architecture G candidate base UTxO aggregate",
  );
  if (
    group.fixtureSize !== fixtureSize ||
    !isCanonicalAbsolutePath(group.inputPath) ||
    ![
      group.inputSha256,
      group.corpusSha256,
      group.corpusSliceSha256,
      group.fundingMapSha256,
      group.fixtureCreationSha256,
      group.binarySha256,
    ].every(isHash) ||
    group.baseUtxoPayloadAggregate.entryCount !== fixtureSize ||
    !isPositiveSafeInteger(group.baseUtxoPayloadAggregate.encodedTupleBytes) ||
    !Array.isArray(group.results) ||
    group.results.length !== runs
  ) {
    throw new Error(
      `Architecture G candidate group is invalid at ${fixtureSize.toString()} UTxOs`,
    );
  }
  const before = validateFixtureIdentity({
    value: group.fixtureBefore,
    fixtureSize,
    label: "Architecture G candidate fixture-before identity",
  });
  const after = validateFixtureIdentity({
    value: group.fixtureAfter,
    fixtureSize,
    label: "Architecture G candidate fixture-after identity",
  });
  if (!jsonEqual(after, before)) {
    throw new Error(
      `Architecture G candidate fixture drifted at ${fixtureSize.toString()} UTxOs`,
    );
  }
  exactObject(
    group.roots,
    [
      "utxos",
      "rawTransactions",
      "transactions",
      "deposits",
      "forcedTransactions",
      "withdrawals",
      "transitionTrace",
      "eventToStep",
    ],
    "Architecture G candidate group roots",
  );
  if (!Object.values(group.roots).every(isHash)) {
    throw new Error("Architecture G candidate group roots are invalid");
  }
  for (const result of group.results) {
    const roots = validateCommitCandidateProbeResult({
      result,
      transactions,
      cpuSet,
      fixtureSize,
      inputPath: group.inputPath,
      inputSha256: group.inputSha256,
      probePath,
      probeSha256,
      binarySha256: group.binarySha256,
    });
    if (
      result.corpusSha256 !== group.corpusSha256 ||
      result.corpusSliceSha256 !== group.corpusSliceSha256 ||
      result.fundingMapSha256 !== group.fundingMapSha256 ||
      result.fixtureCreationSha256 !== group.fixtureCreationSha256 ||
      !jsonEqual(
        result.baseUtxoPayloadAggregate,
        group.baseUtxoPayloadAggregate,
      ) ||
      result.ownerBefore.durableRoot !== before.marker ||
      !jsonEqual(roots, group.roots)
    ) {
      throw new Error(
        `Architecture G candidate result drifted at ${fixtureSize.toString()} UTxOs`,
      );
    }
  }
  const durations = group.results.map((result) => result.durationMs);
  const expectedDurations = {
    min: Math.min(...durations),
    median: percentile(durations, 0.5),
    p95: percentile(durations, 0.95),
    max: Math.max(...durations),
  };
  exactObject(
    group.durations,
    ["min", "median", "p95", "max"],
    "Architecture G candidate duration aggregate",
  );
  if (!jsonEqual(group.durations, expectedDurations)) {
    throw new Error(
      `Architecture G candidate duration aggregate drifted at ${fixtureSize.toString()} UTxOs`,
    );
  }
  return group;
};

export const validateArchitectureGCommitCandidateGateSummaryV1 = ({
  summary,
  config,
  cpuSet,
}) => {
  exactObject(
    summary,
    [
      "schemaVersion",
      "formal",
      "profile",
      "mode",
      "runs",
      "transactions",
      "requiredCardinality",
      "phase1FormalBinding",
      "runtimeIdentity",
      "cpuSet",
      "probePath",
      "probeSha256",
      "rootGateSummary",
      "percentileMethod",
      "groups",
      "verdict",
    ],
    "Architecture G commit-candidate gate summary",
  );
  exactObject(
    summary.requiredCardinality,
    ["runs", "transactions"],
    "Architecture G candidate required cardinality",
  );
  validateArchitectureGPhase1FormalBindingIdentity(summary.phase1FormalBinding);
  validateArchitectureGRuntimeIdentity({
    identity: summary.runtimeIdentity,
    expectedVersion: summary.runtimeIdentity?.version,
    expectedExecutableSha256: summary.runtimeIdentity?.executableSha256,
  });
  const expectedSchema = config.formal
    ? "midgard-architecture-g-commit-candidate-gate-v1"
    : "midgard-architecture-g-commit-candidate-smoke-v1";
  if (
    summary.schemaVersion !== expectedSchema ||
    summary.formal !== config.formal ||
    summary.profile !== config.profile ||
    summary.mode !== config.mode ||
    summary.runs !== config.runs ||
    summary.transactions !== config.transactions ||
    !jsonEqual(summary.requiredCardinality, config.required) ||
    summary.cpuSet !== cpuSet ||
    typeof cpuSet !== "string" ||
    cpuSet.trim().length === 0 ||
    cpuSet.length > 4096 ||
    !isCanonicalAbsolutePath(summary.probePath) ||
    !isHash(summary.probeSha256) ||
    summary.percentileMethod !==
      "nearest-rank: sorted[max(0, ceil(N*q)-1)]; q=0.5 median, q=0.95 p95"
  ) {
    throw new Error("Architecture G commit-candidate gate identity is invalid");
  }
  if (config.formal) {
    validateRootGateIdentity({
      value: summary.rootGateSummary,
      phase1FormalBinding: summary.phase1FormalBinding,
      runtimeIdentity: summary.runtimeIdentity,
    });
  } else if (summary.rootGateSummary !== null) {
    validateRootGateIdentity({
      value: summary.rootGateSummary,
      phase1FormalBinding: summary.phase1FormalBinding,
      runtimeIdentity: summary.runtimeIdentity,
    });
  }
  const expectedSizes =
    config.mode === "50k" ? [1_000_000] : [100_000, 300_000, 1_000_000];
  if (
    !Array.isArray(summary.groups) ||
    summary.groups.length !== expectedSizes.length
  ) {
    throw new Error(
      "Architecture G commit-candidate fixture groups are incomplete",
    );
  }
  for (const [index, fixtureSize] of expectedSizes.entries()) {
    validateGroup({
      group: summary.groups[index],
      fixtureSize,
      runs: config.runs,
      transactions: config.transactions,
      cpuSet,
      probePath: summary.probePath,
      probeSha256: summary.probeSha256,
    });
  }
  const sharedIdentityFields = [
    "corpusSha256",
    "corpusSliceSha256",
    "fundingMapSha256",
    "binarySha256",
  ];
  for (const field of sharedIdentityFields) {
    if (new Set(summary.groups.map((group) => group[field])).size !== 1) {
      throw new Error(`Architecture G candidate groups disagree on ${field}`);
    }
  }
  const expectedVerdict =
    config.mode === "50k"
      ? {
          pass: summary.groups[0].durations.p95 < 10_000,
          gate: "50k_full_commit_candidate_p95_under_10s",
          p95Ms: summary.groups[0].durations.p95,
          limitMs: 10_000,
        }
      : (() => {
          const medians = summary.groups.map((group) => group.durations.median);
          const minimumMedianMs = Math.min(...medians);
          const maximumMedianMs = Math.max(...medians);
          const maxMinSlopePercent =
            ((maximumMedianMs - minimumMedianMs) / minimumMedianMs) * 100;
          return {
            pass: maxMinSlopePercent <= 10,
            gate: "100k_300k_1m_full_commit_candidate_slope_within_10_percent",
            maxMinSlopePercent,
            minimumMedianMs,
            maximumMedianMs,
            limitAbsolutePercent: 10,
          };
        })();
  exactObject(
    summary.verdict,
    config.mode === "50k"
      ? ["pass", "gate", "p95Ms", "limitMs"]
      : [
          "pass",
          "gate",
          "maxMinSlopePercent",
          "minimumMedianMs",
          "maximumMedianMs",
          "limitAbsolutePercent",
        ],
    "Architecture G commit-candidate verdict",
  );
  if (!expectedVerdict.pass || !jsonEqual(summary.verdict, expectedVerdict)) {
    throw new Error(
      "Architecture G commit-candidate verdict is invalid or failed",
    );
  }
  return summary;
};
