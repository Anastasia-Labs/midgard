import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname } from "node:path";

import {
  cgroupMembership,
  cgroupMemoryMax,
  diffSha256,
  gitHead,
  gitStatusEntries,
  gitStatusSha256,
  groups,
  memoryMaxPath,
  probeSha256,
  sourceFiles,
  sourceSha256,
  verdict,
} from "./mpf-architecture-g-gate.execute.mjs";
import {
  binaryPath,
  cpuSet,
  expectedRuntimeExecutableSha256,
  expectedRuntimeVersion,
  gateConfig,
  mode,
  outPath,
  phase1FormalBinding,
  phase1FormalBindingPath,
  phase1FormalBindingSha256,
  probePath,
  profile,
  runs,
  runtimeIdentity,
  transactionCount,
} from "./mpf-architecture-g-gate.fixtures.mjs";
import {
  binarySha256,
  canonicalCorpus,
} from "./mpf-architecture-g-gate.prepare-canonical-corpus-slice.mjs";
import {
  captureArchitectureGPhase1FormalBindingIdentity,
  captureArchitectureGRuntimeIdentity,
  validateArchitectureGRootGateSummary,
} from "./mpf-architecture-g-gate-config.mjs";

assert.equal(
  createHash("sha256").update(readFileSync(probePath)).digest("hex"),
  probeSha256,
  "Architecture G probe mutated during root gate execution",
);

assert.equal(
  createHash("sha256").update(readFileSync(binaryPath)).digest("hex"),
  binarySha256,
  "Architecture G native binary mutated during root gate execution",
);

if (canonicalCorpus !== null) {
  assert.equal(
    createHash("sha256")
      .update(readFileSync(canonicalCorpus.slicePath))
      .digest("hex"),
    canonicalCorpus.sliceSha256,
    "Canonical corpus slice mutated during root gate execution",
  );
  assert.equal(
    createHash("sha256")
      .update(readFileSync(canonicalCorpus.fundingMapPath))
      .digest("hex"),
    canonicalCorpus.fundingMapSha256,
    "Canonical funding map mutated during root gate execution",
  );
}

for (const group of groups) {
  if (group.fixtureCreation !== null) {
    assert.equal(
      createHash("sha256")
        .update(readFileSync(group.fixtureCreation.path))
        .digest("hex"),
      group.fixtureCreation.sha256,
      `Fixture creation evidence mutated during root gate execution at ${group.initialUtxos.toString()} UTxOs`,
    );
  }
}

assert.deepEqual(
  captureArchitectureGPhase1FormalBindingIdentity({
    bindingPath: phase1FormalBindingPath,
    bindingSha256: phase1FormalBindingSha256,
  }),
  phase1FormalBinding,
  "Phase 1 formal binding identity mutated during root gate execution",
);

assert.deepEqual(
  captureArchitectureGRuntimeIdentity({
    expectedVersion: expectedRuntimeVersion,
    expectedExecutableSha256: expectedRuntimeExecutableSha256,
  }),
  runtimeIdentity,
  "Runtime identity mutated during root gate execution",
);

const summary = validateArchitectureGRootGateSummary({
  summary: {
    schemaVersion: gateConfig.formal
      ? "midgard-architecture-g-production-root-gate-v1"
      : "midgard-architecture-g-root-diagnostic-smoke-v1",
    formal: gateConfig.formal,
    profile,
    requiredCardinality: gateConfig.required,
    generatedAt: new Date().toISOString(),
    mode,
    freshProcessRunsPerFixture: runs,
    transactionCount,
    phase1FormalBinding,
    runtimeIdentity,
    canonicalCorpus,
    binaryPath,
    binarySha256,
    probePath,
    probeSha256,
    gitHead,
    sourceSha256,
    diffSha256,
    gitStatusSha256,
    gitStatusEntries,
    sourceFiles,
    cpuSet,
    nodeOptions: "--max-old-space-size=4096",
    cgroup: {
      membership: cgroupMembership,
      memoryMaxPath: memoryMaxPath ?? "unavailable",
      memoryMax: cgroupMemoryMax,
    },
    percentileMethod:
      "nearest-rank: sorted[max(0, ceil(N*q)-1)]; q=0.5 median, q=0.95 p95",
    groups,
    verdict,
  },
  mode,
  runs,
  transactions: transactionCount,
  cpuSet,
});

mkdirSync(dirname(outPath), { recursive: true });

writeFileSync(outPath, `${JSON.stringify(summary, null, 2)}\n`);

const markdownPath = outPath.replace(/\.json$/, ".md");

writeFileSync(
  markdownPath,
  [
    "# Architecture G production gate",
    "",
    `- Mode: ${mode}`,
    `- Profile: ${profile}`,
    `- Formal closure evidence: ${String(gateConfig.formal)}`,
    `- Phase 1 formal binding: \`${phase1FormalBinding.sha256}\` (${phase1FormalBinding.path})`,
    `- Runtime: \`${runtimeIdentity.version}\`, executable \`${runtimeIdentity.executableSha256}\` (${runtimeIdentity.execPath})`,
    `- Binary SHA-256: \`${binarySha256}\``,
    `- Probe SHA-256: \`${probeSha256}\``,
    `- Source SHA-256: \`${sourceSha256}\``,
    `- Diff SHA-256: \`${diffSha256}\``,
    `- Git-status SHA-256: \`${gitStatusSha256}\``,
    `- CPU affinity: \`${cpuSet}\``,
    `- Transactions per build: ${transactionCount.toLocaleString("en-US")}`,
    ...(canonicalCorpus === null
      ? ["- Workload: fixed synthetic growth operation stream"]
      : [
          `- Canonical corpus SHA-256: \`${canonicalCorpus.corpusSha256}\``,
          `- Canonical parent slice: \`${canonicalCorpus.parentSliceId}\``,
          `- Canonical selection: ${canonicalCorpus.selectionAlgorithm}, slice rows ${canonicalCorpus.sourceSliceOrdinalRange.start.toString()}-${canonicalCorpus.sourceSliceOrdinalRange.end.toString()} (${canonicalCorpus.sliceRowCount.toLocaleString("en-US")} rows)`,
          `- Canonical chain closure: ${canonicalCorpus.completeChainCount.toLocaleString("en-US")} complete chain(s), final prefix ${canonicalCorpus.finalChainPrefixLength.toLocaleString("en-US")} row(s)`,
          `- Parent slice boundary proof: ${canonicalCorpus.parentSliceChainCount.toLocaleString("en-US")} contiguous chain(s), cross-slice chains=${String(canonicalCorpus.chainsCrossSliceBoundaries)}`,
          `- Funding roots SHA-256: \`${canonicalCorpus.fundingRootsSha256}\``,
          `- Funding map SHA-256: \`${canonicalCorpus.fundingMapSha256}\` (${canonicalCorpus.fundingEntryCount.toLocaleString("en-US")} roots)`,
          `- Canonical slice SHA-256: \`${canonicalCorpus.sliceSha256}\``,
        ]),
    `- Fresh processes per fixture: ${runs.toString()}`,
    `- Percentiles: ${summary.percentileMethod}`,
    `- Verdict: **${verdict.pass ? "PASS" : "FAIL"}** (${verdict.gate})`,
    "",
    "| Initial UTxOs | Fixture SHA-256 | Median ms | p95 ms | Max ms |",
    "| ---: | --- | ---: | ---: | ---: |",
    ...groups.map(
      (group) =>
        `| ${group.initialUtxos.toLocaleString("en-US")} | \`${group.fixtureBefore.logicalSha256}\` | ${group.durationMs.median.toFixed(3)} | ${group.durationMs.p95.toFixed(3)} | ${group.durationMs.max.toFixed(3)} |`,
    ),
    "",
  ].join("\n"),
);

process.stdout.write(`${JSON.stringify({ outPath, markdownPath, verdict })}\n`);

if (!verdict.pass) process.exitCode = 1;
