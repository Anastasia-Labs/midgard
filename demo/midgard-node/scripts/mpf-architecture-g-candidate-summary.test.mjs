import assert from "node:assert/strict";
import test from "node:test";

import { validateArchitectureGCommitCandidateGateSummaryV1 } from "./mpf-architecture-g-candidate-summary.mjs";

const hash = (byte) => byte.toString(16).padStart(2, "0").repeat(32);

const phase1FormalBinding = {
  schemaVersion: "midgard-architecture-g-phase1-formal-binding-identity-v1",
  path: "/evidence/phase1-formal-binding.json",
  sha256: hash(1),
  deploymentManifestId: "deployment-manifest-id",
  nodeImageId: "sha256:node-image",
  nodeContainerId: "node-container-id",
  walletSetSha256: hash(2),
  fundingSetSha256: hash(3),
  corpus: {
    path: "/evidence/corpus.ndjson",
    indexPath: "/evidence/corpus.ndjson.index.ndjson",
    manifestPath: "/evidence/corpus.ndjson.manifest.json",
    sliceId: "phase1-live",
    corpusSha256: hash(4),
    indexSha256: hash(5),
    manifestSha256: hash(6),
  },
  generationResult: {
    path: "/evidence/generation-result.json",
    sha256: hash(7),
    schemaVersion: "midgard-stress-corpus-generation-v1",
  },
  harness: { scenarioId: hash(8), engineId: hash(9) },
};

const runtimeIdentity = {
  schemaVersion: "midgard-architecture-g-runtime-identity-v1",
  version: "v22.22.2",
  execPath: "/opt/node-v22.22.2/bin/node",
  executableSha256: hash(10),
};

const ownerDiagnostics = (durableRoot) => ({
  ownerEpoch: { type: "Buffer", data: Array(16).fill(7) },
  durableRoot,
  residentNodes: 10,
  residentEdges: 9,
  residentBytes: 1024,
  activeGenerations: 0,
  generatedNodes: 20,
  generatedBytes: 2048,
  rssBytes: 4096,
  peakRssBytes: 8192,
  childRestarts: 0,
});

const candidateRoots = (seed) => ({
  utxos: hash(seed),
  rawTransactions: hash(seed + 1),
  transactions: hash(seed + 2),
  deposits: hash(seed + 3),
  forcedTransactions: hash(seed + 4),
  withdrawals: hash(seed + 5),
  transitionTrace: hash(seed + 6),
  eventToStep: hash(seed + 7),
});

const candidateResult = ({
  durationMs,
  transactions,
  fixtureSize,
  fixtureRoot,
  rootSeed,
}) => {
  const roots = candidateRoots(rootSeed);
  const baseHeaderHash = hash(30).slice(0, 56);
  const minimumWatermarkMs = 1_699_999_999_000;
  return {
    schemaVersion: "midgard-architecture-g-commit-candidate-probe-v1",
    probePath: "/probes/mpf-commit-candidate-probe.js",
    probeSha256: hash(11),
    inputPath: `/inputs/utxos-${fixtureSize.toString()}.json`,
    inputSha256: hash(12 + rootSeed),
    expectedTransactionCount: transactions,
    corpusSha256: phase1FormalBinding.corpus.corpusSha256,
    corpusSliceSha256: hash(13),
    fundingMapSha256: hash(14),
    fixtureCreationSha256: hash(15 + rootSeed),
    fixtureInitialUtxoCount: fixtureSize,
    baseUtxoPayloadAggregate: {
      entryCount: fixtureSize,
      encodedTupleBytes: fixtureSize * 80,
    },
    binarySha256: hash(16),
    cpuAffinity: "2-9",
    durationMs,
    confirmedLedgerFullScans: 0,
    journalRowsBefore: 0,
    journalRowsAfter: 0,
    candidateConfig: {
      mpfEngine: "architecture_g",
      scratchBuild: "fromlist",
      payloadRootCheck: "off",
      parallelRoots: true,
      costModel: "ewma",
      mempoolRetrievePageSize: transactions,
      maxL2TxCount: transactions,
      maxLedgerOpCount: transactions * 3,
      maxTransitionStepCount: transactions,
    },
    providerBoundaryAttempts: 0,
    submissionAttempts: 0,
    candidate: {
      candidateId: "123e4567-e89b-42d3-a456-426614174000",
      baseHeaderHash,
      endTimeMs: 1_700_000_000_000,
      builtAtMs: 1_700_000_000_100,
      buildDurationMs: Math.max(1, durationMs - 1),
      invalidationKey: `${baseHeaderHash}:1700000000000:${minimumWatermarkMs.toString()}`,
      watermarks: {
        depositMs: minimumWatermarkMs,
        withdrawalMs: minimumWatermarkMs + 100,
        txOrderMs: minimumWatermarkMs + 200,
        refreshedAtMs: 1_700_000_000_050,
      },
      expectedUserEventCounts: {
        deposits: 0,
        forcedTransactions: 0,
        withdrawals: 0,
      },
      expectedL2TransactionCount: transactions,
      roots,
    },
    ownerBefore: ownerDiagnostics(fixtureRoot),
    ownerAfter: ownerDiagnostics(fixtureRoot),
  };
};

const percentile = (values, quantile) => {
  const sorted = [...values].sort((left, right) => left - right);
  return sorted[Math.max(0, Math.ceil(sorted.length * quantile) - 1)];
};

const candidateGroup = ({ fixtureSize, durations, transactions, rootSeed }) => {
  const fixtureRoot = hash(100 + rootSeed);
  const results = durations.map((durationMs) =>
    candidateResult({
      durationMs,
      transactions,
      fixtureSize,
      fixtureRoot,
      rootSeed,
    }),
  );
  const fixture = {
    path: `/fixtures/utxos-${fixtureSize.toString()}-level`,
    marker: fixtureRoot,
    records: fixtureSize + 1,
    logicalSha256: hash(110 + rootSeed),
  };
  return {
    fixtureSize,
    inputPath: results[0].inputPath,
    inputSha256: results[0].inputSha256,
    corpusSha256: results[0].corpusSha256,
    corpusSliceSha256: results[0].corpusSliceSha256,
    fundingMapSha256: results[0].fundingMapSha256,
    fixtureCreationSha256: results[0].fixtureCreationSha256,
    baseUtxoPayloadAggregate: results[0].baseUtxoPayloadAggregate,
    binarySha256: results[0].binarySha256,
    fixtureBefore: structuredClone(fixture),
    fixtureAfter: structuredClone(fixture),
    roots: structuredClone(results[0].candidate.roots),
    durations: {
      min: Math.min(...durations),
      median: percentile(durations, 0.5),
      p95: percentile(durations, 0.95),
      max: Math.max(...durations),
    },
    results,
  };
};

const sourceIdentity = {
  gitHead: "ab".repeat(20),
  sourceSha256: hash(20),
  diffSha256: hash(21),
  gitStatusSha256: hash(22),
};

const rootGateIdentity = {
  path: "/evidence/root-gate-summary.json",
  sha256: hash(23),
  sourceSha256: sourceIdentity.sourceSha256,
  diffSha256: sourceIdentity.diffSha256,
  gitStatusSha256: sourceIdentity.gitStatusSha256,
  phase1FormalBinding: structuredClone(phase1FormalBinding),
  runtimeIdentity: structuredClone(runtimeIdentity),
  expectedSourceIdentity: structuredClone(sourceIdentity),
  currentSourceIdentity: structuredClone(sourceIdentity),
};

const summaryFor = ({ mode, profile, runs, transactions, durations }) => {
  const formal = profile === "formal";
  const required =
    mode === "50k"
      ? { runs: 20, transactions: 50_000 }
      : { runs: 3, transactions: 10_000 };
  const fixtureSizes =
    mode === "50k" ? [1_000_000] : [100_000, 300_000, 1_000_000];
  const groups = fixtureSizes.map((fixtureSize, index) =>
    candidateGroup({
      fixtureSize,
      durations: durations[index],
      transactions,
      rootSeed: index + 1,
    }),
  );
  const verdict =
    mode === "50k"
      ? {
          pass: groups[0].durations.p95 < 10_000,
          gate: "50k_full_commit_candidate_p95_under_10s",
          p95Ms: groups[0].durations.p95,
          limitMs: 10_000,
        }
      : (() => {
          const medians = groups.map((group) => group.durations.median);
          const minimumMedianMs = Math.min(...medians);
          const maximumMedianMs = Math.max(...medians);
          return {
            pass:
              ((maximumMedianMs - minimumMedianMs) / minimumMedianMs) * 100 <=
              10,
            gate: "100k_300k_1m_full_commit_candidate_slope_within_10_percent",
            maxMinSlopePercent:
              ((maximumMedianMs - minimumMedianMs) / minimumMedianMs) * 100,
            minimumMedianMs,
            maximumMedianMs,
            limitAbsolutePercent: 10,
          };
        })();
  return {
    config: {
      mode,
      profile,
      formal,
      runs,
      transactions,
      required,
    },
    summary: {
      schemaVersion: formal
        ? "midgard-architecture-g-commit-candidate-gate-v1"
        : "midgard-architecture-g-commit-candidate-smoke-v1",
      formal,
      profile,
      mode,
      runs,
      transactions,
      requiredCardinality: structuredClone(required),
      phase1FormalBinding: structuredClone(phase1FormalBinding),
      runtimeIdentity: structuredClone(runtimeIdentity),
      cpuSet: "2-9",
      probePath: "/probes/mpf-commit-candidate-probe.js",
      probeSha256: hash(11),
      rootGateSummary: formal ? structuredClone(rootGateIdentity) : null,
      percentileMethod:
        "nearest-rank: sorted[max(0, ceil(N*q)-1)]; q=0.5 median, q=0.95 p95",
      groups,
      verdict,
    },
  };
};

const validate = ({ summary, config }) =>
  validateArchitectureGCommitCandidateGateSummaryV1({
    summary,
    config,
    cpuSet: "2-9",
  });

test("accepts only the complete passing formal 50k candidate summary", () => {
  const valid = summaryFor({
    mode: "50k",
    profile: "formal",
    runs: 20,
    transactions: 50_000,
    durations: [Array.from({ length: 20 }, (_, index) => 8_000 + index)],
  });
  assert.equal(validate(valid), valid.summary);
  for (const mutate of [
    (value) => void (value.summary.unknown = true),
    (value) => void (value.summary.formal = false),
    (value) => void (value.summary.requiredCardinality.runs = 19),
    (value) => void (value.summary.phase1FormalBinding.unknown = true),
    (value) => void (value.summary.runtimeIdentity.unknown = true),
    (value) => void (value.summary.rootGateSummary.unknown = true),
    (value) =>
      void (value.summary.rootGateSummary.currentSourceIdentity.diffSha256 =
        hash(90)),
    (value) => void value.summary.groups.pop(),
    (value) => void (value.summary.groups[0].unknown = true),
    (value) => void value.summary.groups[0].results.pop(),
    (value) => void (value.summary.groups[0].fixtureAfter.marker = hash(91)),
    (value) => void (value.summary.groups[0].roots.utxos = hash(92)),
    (value) =>
      void (value.summary.groups[0].results[0].corpusSliceSha256 = hash(93)),
    (value) =>
      void (value.summary.groups[0].results[0].ownerAfter.unknown = true),
    (value) => void (value.summary.groups[0].durations.p95 = 9_999),
    (value) => void (value.summary.verdict.p95Ms = 9_999),
  ]) {
    const invalid = structuredClone(valid);
    mutate(invalid);
    assert.throws(() => validate(invalid));
  }
});

test("recomputes the complete smoke growth candidate verdict", () => {
  const valid = summaryFor({
    mode: "growth",
    profile: "smoke",
    runs: 2,
    transactions: 2,
    durations: [
      [100, 110],
      [101, 109],
      [102, 108],
    ],
  });
  assert.equal(validate(valid), valid.summary);
  for (const mutate of [
    (value) => void (value.summary.groups[2].durations.median = 103),
    (value) => void (value.summary.verdict.maxMinSlopePercent = 3),
    (value) => void (value.summary.groups[2].binarySha256 = hash(94)),
    (value) => void (value.summary.rootGateSummary = {}),
  ]) {
    const invalid = structuredClone(valid);
    mutate(invalid);
    assert.throws(() => validate(invalid));
  }
});

test("rejects an internally consistent failed candidate verdict", () => {
  const failed = summaryFor({
    mode: "50k",
    profile: "smoke",
    runs: 2,
    transactions: 2,
    durations: [[10_000, 10_001]],
  });
  assert.equal(failed.summary.verdict.pass, false);
  assert.throws(() => validate(failed));
});
