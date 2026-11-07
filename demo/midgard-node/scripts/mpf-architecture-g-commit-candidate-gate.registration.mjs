import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname } from "node:path";

import { validateArchitectureGCommitCandidateGateSummaryV1 } from "./mpf-architecture-g-candidate-summary.mjs";
import {
  candidateRootsAsRootGateTuple,
  captureSourceIdentity,
  config,
  cpuSet,
  currentSourceIdentity,
  execute,
  expectedRuntimeExecutableSha256,
  expectedRuntimeVersion,
  expectedSourceIdentity,
  fixtureIdentity,
  groups,
  inputs,
  outPath,
  phase1FormalBinding,
  phase1FormalBindingPath,
  phase1FormalBindingSha256,
  probePath,
  probeSha256,
  resolvedRootGateSummaryPath,
  rootGateSummary,
  rootGateSummaryPath,
  rootGateSummarySha256,
  rootTuple,
  runtimeIdentity,
} from "./mpf-architecture-g-commit-candidate-gate.capture-source-identity.mjs";
import {
  captureArchitectureGPhase1FormalBindingIdentity,
  captureArchitectureGRuntimeIdentity,
  discoverArchitectureGSourceFiles,
  percentile,
  validateArchitectureGCommitCandidateInputV1,
  validateArchitectureGCrossGateEvidenceIdentity,
  validateArchitectureGCrossGateFixtureIdentity,
  validateArchitectureGCrossGateSourceIdentity,
  validateArchitectureGSourceFileList,
} from "./mpf-architecture-g-gate-config.mjs";

for (const [fixtureSize, inputPath] of inputs) {
  const inputBytes = readFileSync(inputPath);
  const inputSha256 = createHash("sha256").update(inputBytes).digest("hex");
  const input = JSON.parse(inputBytes.toString("utf8"));
  validateArchitectureGCommitCandidateInputV1(input);
  validateArchitectureGCrossGateEvidenceIdentity({
    expected: phase1FormalBinding,
    current: input.phase1FormalBinding,
    label: "Phase 1 formal binding input",
  });
  validateArchitectureGCrossGateEvidenceIdentity({
    expected: runtimeIdentity,
    current: input.runtimeIdentity,
    label: "runtime input",
  });
  assert.equal(input.expectedTransactionCount, config.transactions);
  assert.equal(
    createHash("sha256").update(readFileSync(input.binaryPath)).digest("hex"),
    input.binarySha256,
    `Commit-candidate binary identity mismatch: ${String(input.binaryPath)}`,
  );
  const fixtureBefore = await fixtureIdentity(input.levelPath);
  const rootGateGroup = rootGateSummary?.groups?.find(
    (group) => group.initialUtxos === fixtureSize,
  );
  if (rootGateSummary !== null) {
    validateArchitectureGCrossGateFixtureIdentity({
      rootGateGroup,
      fixtureBefore,
      fixtureSize,
    });
  }
  const results = Array.from({ length: config.runs }, (_, index) =>
    execute({
      fixtureSize,
      inputPath,
      inputSha256,
      binarySha256: input.binarySha256,
      runIndex: index + 1,
    }),
  );
  assert.equal(
    createHash("sha256").update(readFileSync(inputPath)).digest("hex"),
    inputSha256,
    `Commit-candidate input mutated during gate execution: ${inputPath}`,
  );
  assert.equal(
    createHash("sha256").update(readFileSync(input.binaryPath)).digest("hex"),
    input.binarySha256,
    `Commit-candidate binary mutated during gate execution: ${String(input.binaryPath)}`,
  );
  assert.equal(
    createHash("sha256")
      .update(readFileSync(input.fixtureCreationPath))
      .digest("hex"),
    input.fixtureCreationSha256,
    `Fixture creation evidence mutated during candidate gate execution: ${String(input.fixtureCreationPath)}`,
  );
  const fixtureAfter = await fixtureIdentity(input.levelPath);
  assert.deepEqual(
    fixtureAfter,
    fixtureBefore,
    `Commit-candidate gate mutated fixture ${input.levelPath}`,
  );
  for (const result of results.slice(1)) {
    assert.deepEqual(
      rootTuple(result),
      rootTuple(results[0]),
      `Commit-candidate roots diverged at ${fixtureSize.toString()} UTxOs`,
    );
    assert.equal(result.corpusSha256, results[0].corpusSha256);
    assert.equal(result.corpusSliceSha256, results[0].corpusSliceSha256);
    assert.equal(result.fundingMapSha256, results[0].fundingMapSha256);
    assert.equal(
      result.fixtureCreationSha256,
      results[0].fixtureCreationSha256,
    );
    assert.deepEqual(
      result.baseUtxoPayloadAggregate,
      results[0].baseUtxoPayloadAggregate,
    );
    assert.equal(result.binarySha256, results[0].binarySha256);
  }
  if (rootGateSummary !== null) {
    assert.ok(
      rootGateGroup !== undefined,
      "Root gate fixture group is missing",
    );
    assert.deepEqual(
      candidateRootsAsRootGateTuple(rootTuple(results[0])),
      {
        utxoRoot: rootGateGroup.roots.utxoRoot,
        rawTxRoot: rootGateGroup.roots.rawTxRoot,
        txRoot: rootGateGroup.roots.txRoot,
        transitionTraceRoot: rootGateGroup.roots.transitionTraceRoot,
        eventToStepRoot: rootGateGroup.roots.eventToStepRoot,
        depositsRoot: rootGateGroup.roots.depositsRoot,
        withdrawalsRoot: rootGateGroup.roots.withdrawalsRoot,
        forcedTransactionsRoot: rootGateGroup.roots.forcedTransactionsRoot,
      },
      `Full candidate roots differ from the complete root gate at ${fixtureSize.toString()} UTxOs`,
    );
    assert.equal(results[0].binarySha256, rootGateSummary.binarySha256);
    assert.equal(
      results[0].corpusSha256,
      rootGateSummary.canonicalCorpus.corpusSha256,
    );
    assert.equal(
      results[0].corpusSliceSha256,
      rootGateSummary.canonicalCorpus.sliceSha256,
    );
    assert.equal(
      results[0].fundingMapSha256,
      rootGateSummary.canonicalCorpus.fundingMapSha256,
    );
    if (config.formal) {
      assert.equal(
        results[0].fixtureCreationSha256,
        rootGateGroup.fixtureCreation.sha256,
        "Candidate fixture creation evidence differs from the root gate",
      );
      assert.deepEqual(
        results[0].baseUtxoPayloadAggregate,
        rootGateGroup.fixtureCreation.utxoPayloadAggregate,
        "Candidate fixture aggregate differs from the root gate",
      );
    }
  }
  const durations = results.map((result) => result.durationMs);
  groups.push({
    fixtureSize,
    inputPath,
    inputSha256,
    corpusSha256: results[0].corpusSha256,
    corpusSliceSha256: results[0].corpusSliceSha256,
    fundingMapSha256: results[0].fundingMapSha256,
    fixtureCreationSha256: results[0].fixtureCreationSha256,
    baseUtxoPayloadAggregate: results[0].baseUtxoPayloadAggregate,
    binarySha256: results[0].binarySha256,
    fixtureBefore,
    fixtureAfter,
    roots: rootTuple(results[0]),
    durations: {
      min: Math.min(...durations),
      median: percentile(durations, 0.5),
      p95: percentile(durations, 0.95),
      max: Math.max(...durations),
    },
    results,
  });
}

if (rootGateSummary !== null) {
  const finalSourceFiles = validateArchitectureGSourceFileList({
    expected: rootGateSummary.sourceFiles,
    current: discoverArchitectureGSourceFiles(),
  });
  assert.deepEqual(
    validateArchitectureGCrossGateSourceIdentity({
      expected: expectedSourceIdentity,
      current: captureSourceIdentity(finalSourceFiles),
    }),
    currentSourceIdentity,
    "Architecture G source identity mutated during candidate gate execution",
  );
}

assert.equal(
  createHash("sha256").update(readFileSync(probePath)).digest("hex"),
  probeSha256,
  "Commit-candidate probe mutated during gate execution",
);

if (resolvedRootGateSummaryPath !== null && rootGateSummarySha256 !== null) {
  assert.equal(
    createHash("sha256")
      .update(readFileSync(resolvedRootGateSummaryPath))
      .digest("hex"),
    rootGateSummarySha256,
    "Root gate summary mutated during candidate gate execution",
  );
}

assert.deepEqual(
  captureArchitectureGPhase1FormalBindingIdentity({
    bindingPath: phase1FormalBindingPath,
    bindingSha256: phase1FormalBindingSha256,
  }),
  phase1FormalBinding,
  "Phase 1 formal binding identity mutated during candidate gate execution",
);

assert.deepEqual(
  captureArchitectureGRuntimeIdentity({
    expectedVersion: expectedRuntimeVersion,
    expectedExecutableSha256: expectedRuntimeExecutableSha256,
  }),
  runtimeIdentity,
  "Runtime identity mutated during candidate gate execution",
);

let verdict;

if (config.mode === "50k") {
  verdict = {
    pass: groups[0].durations.p95 < 10_000,
    gate: "50k_full_commit_candidate_p95_under_10s",
    p95Ms: groups[0].durations.p95,
    limitMs: 10_000,
  };
} else {
  const corpusIdentities = new Set(
    groups.flatMap((group) =>
      group.results.map(
        (result) => `${result.corpusSha256}:${result.corpusSliceSha256}`,
      ),
    ),
  );
  assert.equal(
    corpusIdentities.size,
    1,
    "Growth candidate fixtures did not use one identical corpus workload",
  );
  const medians = groups.map((group) => group.durations.median);
  const minimumMedianMs = Math.min(...medians);
  const maximumMedianMs = Math.max(...medians);
  const maxMinSlopePercent =
    ((maximumMedianMs - minimumMedianMs) / minimumMedianMs) * 100;
  verdict = {
    pass: maxMinSlopePercent <= 10,
    gate: "100k_300k_1m_full_commit_candidate_slope_within_10_percent",
    maxMinSlopePercent,
    minimumMedianMs,
    maximumMedianMs,
    limitAbsolutePercent: 10,
  };
}

const summary = validateArchitectureGCommitCandidateGateSummaryV1({
  summary: {
    schemaVersion: config.formal
      ? "midgard-architecture-g-commit-candidate-gate-v1"
      : "midgard-architecture-g-commit-candidate-smoke-v1",
    formal: config.formal,
    profile: config.profile,
    mode: config.mode,
    runs: config.runs,
    transactions: config.transactions,
    requiredCardinality: config.required,
    phase1FormalBinding,
    runtimeIdentity,
    cpuSet,
    probePath,
    probeSha256,
    rootGateSummary:
      rootGateSummaryPath.length === 0
        ? null
        : {
            path: resolvedRootGateSummaryPath,
            sha256: rootGateSummarySha256,
            sourceSha256: rootGateSummary.sourceSha256,
            diffSha256: rootGateSummary.diffSha256,
            gitStatusSha256: rootGateSummary.gitStatusSha256,
            phase1FormalBinding: rootGateSummary.phase1FormalBinding,
            runtimeIdentity: rootGateSummary.runtimeIdentity,
            expectedSourceIdentity,
            currentSourceIdentity,
          },
    percentileMethod:
      "nearest-rank: sorted[max(0, ceil(N*q)-1)]; q=0.5 median, q=0.95 p95",
    groups,
    verdict,
  },
  config,
  cpuSet,
});

mkdirSync(dirname(outPath), { recursive: true });

writeFileSync(outPath, `${JSON.stringify(summary, null, 2)}\n`);

process.stdout.write(`${JSON.stringify({ outPath, verdict })}\n`);

if (!verdict.pass) process.exitCode = 1;
