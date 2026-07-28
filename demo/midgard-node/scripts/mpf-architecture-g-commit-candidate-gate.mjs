import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { createHash } from "node:crypto";
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";

import { Level } from "level";

import { validateArchitectureGCommitCandidateGateSummaryV1 } from "./mpf-architecture-g-candidate-summary.mjs";
import {
  captureArchitectureGPhase1FormalBindingIdentity,
  captureArchitectureGRuntimeIdentity,
  discoverArchitectureGSourceFiles,
  resolveArchitectureGGateConfig,
  validateArchitectureGCommitCandidateInputV1,
  validateArchitectureGCrossGateEvidenceIdentity,
  validateArchitectureGCrossGateFixtureIdentity,
  validateArchitectureGCrossGateSourceIdentity,
  validateCommitCandidateProbeResult,
  validateArchitectureGRootGateSummary,
  validateArchitectureGSourceFileList,
} from "./mpf-architecture-g-gate-config.mjs";

const option = (name, fallback) =>
  process.argv
    .find((value) => value.startsWith(`--${name}=`))
    ?.slice(name.length + 3) ?? fallback;
const config = resolveArchitectureGGateConfig({
  mode: option("mode", "50k"),
  profile: option("profile", "formal"),
  runs: option("runs", undefined),
  transactions: option("transactions", undefined),
});
const phase1FormalBindingPath = option(
  "phase1-formal-binding",
  process.env.MPF_ARCH_G_PHASE1_FORMAL_BINDING_PATH ?? "",
).trim();
const phase1FormalBindingSha256 = option(
  "phase1-formal-binding-sha256",
  process.env.MPF_ARCH_G_PHASE1_FORMAL_BINDING_SHA256 ?? "",
).trim();
const phase1FormalBinding = captureArchitectureGPhase1FormalBindingIdentity({
  bindingPath: phase1FormalBindingPath,
  bindingSha256: phase1FormalBindingSha256,
});
const expectedRuntimeVersion = option(
  "runtime-version",
  process.env.MPF_ARCH_G_RUNTIME_VERSION ?? "",
).trim();
const expectedRuntimeExecutableSha256 = option(
  "runtime-executable-sha256",
  process.env.MPF_ARCH_G_RUNTIME_EXECUTABLE_SHA256 ?? "",
).trim();
const runtimeIdentity = captureArchitectureGRuntimeIdentity({
  expectedVersion: expectedRuntimeVersion,
  expectedExecutableSha256: expectedRuntimeExecutableSha256,
});
const cpuSet = option("cpuset", process.env.MPF_ARCH_G_CPUSET ?? "").trim();
if (cpuSet.length === 0) {
  throw new Error("Set --cpuset or MPF_ARCH_G_CPUSET");
}
const rootGateSummaryPath = option(
  "root-gate-summary",
  process.env.MPF_ARCH_G_ROOT_GATE_SUMMARY ?? "",
).trim();
if (config.formal && rootGateSummaryPath.length === 0) {
  throw new Error("A formal candidate gate requires --root-gate-summary");
}
const resolvedRootGateSummaryPath =
  rootGateSummaryPath.length === 0 ? null : resolve(rootGateSummaryPath);
const rootGateSummaryBytes =
  resolvedRootGateSummaryPath === null
    ? null
    : readFileSync(resolvedRootGateSummaryPath);
const rootGateSummarySha256 =
  rootGateSummaryBytes === null
    ? null
    : createHash("sha256").update(rootGateSummaryBytes).digest("hex");
const rootGateSummary =
  rootGateSummaryBytes === null
    ? null
    : JSON.parse(rootGateSummaryBytes.toString("utf8"));
if (rootGateSummary !== null) {
  if (config.formal) {
    validateArchitectureGRootGateSummary({
      summary: rootGateSummary,
      mode: config.mode,
      runs: config.runs,
      transactions: config.transactions,
      cpuSet,
    });
  } else {
    assert.equal(rootGateSummary.mode, config.mode);
    assert.equal(rootGateSummary.transactionCount, config.transactions);
  }
  validateArchitectureGCrossGateEvidenceIdentity({
    expected: rootGateSummary.phase1FormalBinding,
    current: phase1FormalBinding,
    label: "Phase 1 formal binding",
  });
  validateArchitectureGCrossGateEvidenceIdentity({
    expected: rootGateSummary.runtimeIdentity,
    current: runtimeIdentity,
    label: "runtime",
  });
}
const updateFramedHash = (hash, path, bytes) => {
  const pathBytes = Buffer.from(path);
  const lengths = Buffer.allocUnsafe(12);
  lengths.writeUInt32LE(pathBytes.length, 0);
  lengths.writeBigUInt64LE(BigInt(bytes.length), 4);
  hash.update(lengths).update(pathBytes).update(bytes);
};
const captureSourceIdentity = (sourceFiles) => {
  const sourceHash = createHash("sha256");
  for (const path of sourceFiles) {
    updateFramedHash(sourceHash, path, readFileSync(resolve(path)));
  }
  const gitHeadResult = spawnSync("git", ["rev-parse", "HEAD"], {
    encoding: "utf8",
  });
  assert.equal(gitHeadResult.status, 0, gitHeadResult.stderr);
  const diff = spawnSync(
    "git",
    ["diff", "--binary", "HEAD", "--", ...sourceFiles],
    { encoding: "buffer", maxBuffer: 128 * 1024 * 1024 },
  );
  assert.equal(diff.status, 0, diff.stderr?.toString() ?? "git diff failed");
  const gitStatus = spawnSync(
    "git",
    [
      "status",
      "--porcelain=v1",
      "-z",
      "--untracked-files=all",
      "--",
      ...sourceFiles,
    ],
    { encoding: "buffer", maxBuffer: 128 * 1024 * 1024 },
  );
  assert.equal(
    gitStatus.status,
    0,
    gitStatus.stderr?.toString() ?? "git status failed",
  );
  return {
    gitHead: gitHeadResult.stdout.trim(),
    sourceSha256: sourceHash.digest("hex"),
    diffSha256: createHash("sha256").update(diff.stdout).digest("hex"),
    gitStatusSha256: createHash("sha256")
      .update(gitStatus.stdout)
      .digest("hex"),
  };
};
const expectedSourceIdentity =
  rootGateSummary === null
    ? null
    : {
        gitHead: rootGateSummary.gitHead,
        sourceSha256: rootGateSummary.sourceSha256,
        diffSha256: rootGateSummary.diffSha256,
        gitStatusSha256: rootGateSummary.gitStatusSha256,
      };
let currentSourceIdentity = null;
if (rootGateSummary !== null) {
  const sourceFiles = validateArchitectureGSourceFileList({
    expected: rootGateSummary.sourceFiles,
    current: discoverArchitectureGSourceFiles(),
  });
  currentSourceIdentity = validateArchitectureGCrossGateSourceIdentity({
    expected: expectedSourceIdentity,
    current: captureSourceIdentity(sourceFiles),
  });
}
const probePath = resolve(
  option("probe", "dist/mpf-commit-candidate-probe.js"),
);
if (!existsSync(probePath)) {
  throw new Error(`Missing commit-candidate probe ${probePath}`);
}
const probeSha256 = createHash("sha256")
  .update(readFileSync(probePath))
  .digest("hex");
const fixtureSizes =
  config.mode === "50k" ? [1_000_000] : [100_000, 300_000, 1_000_000];
const inputs = new Map(
  fixtureSizes.map((size) => {
    const path = resolve(
      option(
        `candidate-input-${size.toString()}`,
        resolve(
          option(
            "input-root",
            process.env.MPF_ARCH_G_CANDIDATE_INPUT_ROOT ?? "",
          ),
          `utxos-${size.toString()}.json`,
        ),
      ),
    );
    if (!existsSync(path)) {
      throw new Error(`Missing commit-candidate input ${path}`);
    }
    return [size, path];
  }),
);
const timestamp = new Date().toISOString().replaceAll(/[-:.]/gu, "");
const outPath = resolve(
  option(
    "out",
    `logs/phase-3-architecture-g-commit-candidate-${config.mode}-${timestamp}/summary.json`,
  ),
);

const percentile = (values, quantile) => {
  const sorted = [...values].sort((left, right) => left - right);
  return sorted[Math.max(0, Math.ceil(sorted.length * quantile) - 1)];
};
const fixtureIdentity = async (path) => {
  const db = new Level(path, { valueEncoding: "json" });
  await db.open();
  try {
    const hash = createHash("sha256");
    let records = 0;
    let marker;
    for await (const [key, value] of db.iterator()) {
      const keyBytes = Buffer.from(key);
      const valueBytes = Buffer.from(JSON.stringify(value));
      const lengths = Buffer.allocUnsafe(8);
      lengths.writeUInt32LE(keyBytes.length, 0);
      lengths.writeUInt32LE(valueBytes.length, 4);
      hash.update(lengths).update(keyBytes).update(valueBytes);
      records += 1;
      if (key === "__root__") marker = value;
    }
    if (typeof marker !== "string" || !/^[0-9a-f]{64}$/u.test(marker)) {
      throw new Error(`Fixture ${path} has no canonical marker`);
    }
    return { path, marker, records, logicalSha256: hash.digest("hex") };
  } finally {
    await db.close();
  }
};
const rootTuple = (result) => result.candidate.roots;
const candidateRootsAsRootGateTuple = (roots) => ({
  utxoRoot: roots.utxos,
  rawTxRoot: roots.rawTransactions,
  txRoot: roots.transactions,
  transitionTraceRoot: roots.transitionTrace,
  eventToStepRoot: roots.eventToStep,
  depositsRoot: roots.deposits,
  withdrawalsRoot: roots.withdrawals,
  forcedTransactionsRoot: roots.forcedTransactions,
});
const execute = ({
  fixtureSize,
  inputPath,
  inputSha256,
  binarySha256,
  runIndex,
}) => {
  const child = spawnSync(
    "taskset",
    ["-c", cpuSet, process.execPath, "--expose-gc", probePath, inputPath],
    {
      encoding: "utf8",
      maxBuffer: 64 * 1024 * 1024,
      env: { ...process.env, NODE_OPTIONS: "--max-old-space-size=4096" },
    },
  );
  assert.equal(
    child.status,
    0,
    `Commit-candidate probe failed fixture=${fixtureSize.toString()} run=${runIndex.toString()}\n${child.stderr}`,
  );
  const result = JSON.parse(child.stdout.trim().split("\n").at(-1));
  validateCommitCandidateProbeResult({
    result,
    transactions: config.transactions,
    cpuSet,
    fixtureSize,
    inputPath,
    inputSha256,
    probePath,
    probeSha256,
    binarySha256,
  });
  return result;
};

const groups = [];
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
