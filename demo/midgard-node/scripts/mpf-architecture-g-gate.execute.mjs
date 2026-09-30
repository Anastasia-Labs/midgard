import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { createHash } from "node:crypto";
import { existsSync, readFileSync } from "node:fs";
import { resolve } from "node:path";

import {
  binaryPath,
  cpuSet,
  fixtures,
  gateConfig,
  mode,
  probePath,
  runs,
  transactionCount,
} from "./mpf-architecture-g-gate.fixtures.mjs";
import {
  binarySha256,
  canonicalCorpus,
  fixtureCreationIdentity,
  fixtureIdentity,
} from "./mpf-architecture-g-gate.prepare-canonical-corpus-slice.mjs";
import {
  discoverArchitectureGSourceFiles,
  percentile,
  validateArchitectureGSourceFileList,
} from "./mpf-architecture-g-gate-config.mjs";

const updateFramedHash = (hash, path, bytes) => {
  const pathBytes = Buffer.from(path);
  const lengths = Buffer.allocUnsafe(12);
  lengths.writeUInt32LE(pathBytes.length, 0);
  lengths.writeBigUInt64LE(BigInt(bytes.length), 4);
  hash.update(lengths).update(pathBytes).update(bytes);
};

const captureSourceIdentity = (files) => {
  const sourceHash = createHash("sha256");
  for (const path of files) {
    updateFramedHash(sourceHash, path, readFileSync(resolve(path)));
  }
  const gitHeadResult = spawnSync("git", ["rev-parse", "HEAD"], {
    encoding: "utf8",
  });
  assert.equal(gitHeadResult.status, 0, gitHeadResult.stderr);
  const diff = spawnSync("git", ["diff", "--binary", "HEAD", "--", ...files], {
    encoding: "buffer",
    maxBuffer: 128 * 1024 * 1024,
  });
  assert.equal(diff.status, 0, diff.stderr?.toString() ?? "git diff failed");
  const gitStatus = spawnSync(
    "git",
    ["status", "--porcelain=v1", "-z", "--untracked-files=all", "--", ...files],
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
    gitStatusEntries: gitStatus.stdout
      .toString("utf8")
      .split("\0")
      .filter((entry) => entry.length > 0),
  };
};

export const sourceFiles = discoverArchitectureGSourceFiles();

export const {
  gitHead,
  sourceSha256,
  diffSha256,
  gitStatusSha256,
  gitStatusEntries,
} = captureSourceIdentity(sourceFiles);

export const probeSha256 = createHash("sha256")
  .update(readFileSync(probePath))
  .digest("hex");

export const cgroupMembership = existsSync("/proc/self/cgroup")
  ? readFileSync("/proc/self/cgroup", "utf8").trim()
  : "unavailable";

const cgroupPath =
  cgroupMembership
    .split("\n")
    .find((line) => line.startsWith("0::"))
    ?.slice(3) ?? "/";

const memoryMaxCandidates = [
  `/sys/fs/cgroup${cgroupPath === "/" ? "" : cgroupPath}/memory.max`,
  "/sys/fs/cgroup/memory.max",
  "/sys/fs/cgroup/memory/memory.limit_in_bytes",
];

export const memoryMaxPath = memoryMaxCandidates.find(existsSync);

export const cgroupMemoryMax =
  memoryMaxPath === undefined
    ? "unavailable"
    : readFileSync(memoryMaxPath, "utf8").trim();

const median = (values) => percentile(values, 0.5);

const execute = (initialUtxos, fixturePath, index) => {
  const run = spawnSync(
    "taskset",
    ["-c", cpuSet, process.execPath, "--expose-gc", probePath],
    {
      cwd: process.cwd(),
      encoding: "utf8",
      maxBuffer: 64 * 1024 * 1024,
      env: {
        ...process.env,
        NODE_OPTIONS: "--max-old-space-size=4096",
        MPF_ENGINE_PROBE_TXS: transactionCount.toString(),
        MPF_ENGINE_PROBE_INITIAL_UTXOS: initialUtxos.toString(),
        MPF_ENGINE_PROBE_LEVEL_DB: fixturePath,
        MPF_ENGINE_PROBE_REUSE_LEVEL_DB: "true",
        MPF_ENGINE_PROBE_PARALLEL_ROOTS: "true",
        MPF_NATIVE_OWNER_BINARY_PATH: binaryPath,
        MPF_NATIVE_OWNER_BINARY_SHA256: binarySha256,
        MPF_NATIVE_OWNER_SIDECAR_PATH: `${fixturePath}.architecture-g-gate.sidecar`,
        ...(canonicalCorpus === null
          ? {}
          : {
              MPF_ENGINE_PROBE_CORPUS_SLICE_PATH: canonicalCorpus.slicePath,
              MPF_ENGINE_PROBE_CORPUS_SLICE_SHA256: canonicalCorpus.sliceSha256,
              MPF_ENGINE_PROBE_CORPUS_SHA256: canonicalCorpus.corpusSha256,
              MPF_ENGINE_PROBE_CORPUS_FUNDING_PATH:
                canonicalCorpus.fundingMapPath,
              MPF_ENGINE_PROBE_CORPUS_FUNDING_SHA256:
                canonicalCorpus.fundingMapSha256,
            }),
      },
    },
  );
  assert.equal(
    run.error,
    undefined,
    `Architecture G gate could not spawn child utxos=${initialUtxos.toString()} run=${index.toString()}: ${run.error?.message ?? "unknown spawn error"}`,
  );
  assert.equal(
    run.status,
    0,
    `Architecture G gate child failed utxos=${initialUtxos.toString()} run=${index.toString()}\n${run.stderr}`,
  );
  const resultLine = run.stdout.trim().split("\n").at(-1);
  assert.ok(
    resultLine !== undefined && resultLine.length > 0,
    `Architecture G gate child returned no result utxos=${initialUtxos.toString()} run=${index.toString()}`,
  );
  const result = JSON.parse(resultLine);
  assert.equal(result.probePath, probePath, "child probe path drifted");
  assert.equal(result.probeSha256, probeSha256, "child probe SHA-256 drifted");
  assert.equal(
    result.binarySha256,
    binarySha256,
    "child native binary SHA-256 drifted",
  );
  assert.equal(result.cpuAffinity, cpuSet, "child CPU affinity drifted");
  assert.equal(
    result.confirmedLedgerFullScans,
    0,
    "Architecture G build performed a confirmed-ledger full scan",
  );
  for (const [name, root] of Object.entries({
    utxoRoot: result.utxoRoot,
    rawTxRoot: result.rawTxRoot,
    txRoot: result.txRoot,
    transitionTraceRoot: result.transitionTraceRoot,
    eventToStepRoot: result.eventToStepRoot,
    depositsRoot: result.depositsRoot,
    withdrawalsRoot: result.withdrawalsRoot,
    forcedTransactionsRoot: result.forcedTransactionsRoot,
  })) {
    assert.match(root, /^[0-9a-f]{64}$/, `Invalid ${name}`);
  }
  assert.equal(
    result.transitionRoots.length,
    transactionCount,
    "Architecture G child returned the wrong transition-root count",
  );
  assert.equal(
    result.transitionRoots[0].pre,
    result.ownerBefore.durableRoot,
    "First transition root does not start at the durable fixture marker",
  );
  for (
    let transition = 1;
    transition < result.transitionRoots.length;
    transition += 1
  ) {
    assert.equal(
      result.transitionRoots[transition].pre,
      result.transitionRoots[transition - 1].post,
      `Transition-root chain broke at index ${transition.toString()}`,
    );
  }
  assert.equal(
    result.transitionRoots.at(-1).post,
    result.utxoRoot,
    "Last transition root does not end at the candidate UTxO root",
  );
  assert.ok(
    Number.isFinite(result.durationMs) && result.durationMs > 0,
    "Architecture G child returned an invalid measured duration",
  );
  if (canonicalCorpus !== null) {
    assert.deepEqual(
      result.canonicalCorpusSlice,
      {
        path: canonicalCorpus.slicePath,
        sha256: canonicalCorpus.sliceSha256,
        rowCount: canonicalCorpus.sliceRowCount,
      },
      "Architecture G child did not use the verified canonical corpus slice",
    );
    assert.deepEqual(
      result.canonicalFunding,
      {
        path: canonicalCorpus.fundingMapPath,
        sha256: canonicalCorpus.fundingMapSha256,
        entryCount: canonicalCorpus.fundingEntryCount,
      },
      "Architecture G child did not use the verified canonical funding map",
    );
  }
  return result;
};

export const groups = [];

for (const [initialUtxos, fixturePath] of fixtures) {
  const fixtureBefore = await fixtureIdentity(fixturePath);
  const fixtureCreation = gateConfig.formal
    ? fixtureCreationIdentity(initialUtxos, fixturePath, fixtureBefore)
    : null;
  const results = Array.from({ length: runs }, (_, index) =>
    execute(initialUtxos, fixturePath, index + 1),
  );
  const fixtureAfter = await fixtureIdentity(fixturePath);
  assert.deepEqual(
    {
      marker: fixtureAfter.marker,
      logicalSha256: fixtureAfter.logicalSha256,
      records: fixtureAfter.records,
    },
    {
      marker: fixtureBefore.marker,
      logicalSha256: fixtureBefore.logicalSha256,
      records: fixtureBefore.records,
    },
    `Architecture G gate mutated fixture ${fixturePath}`,
  );
  const rootTuple = (result) => ({
    utxoRoot: result.utxoRoot,
    rawTxRoot: result.rawTxRoot,
    txRoot: result.txRoot,
    transitionTraceRoot: result.transitionTraceRoot,
    eventToStepRoot: result.eventToStepRoot,
    depositsRoot: result.depositsRoot,
    withdrawalsRoot: result.withdrawalsRoot,
    forcedTransactionsRoot: result.forcedTransactionsRoot,
    transitionRoots: result.transitionRoots,
  });
  for (const result of results.slice(1)) {
    assert.deepEqual(
      rootTuple(result),
      rootTuple(results[0]),
      `Architecture G roots diverged across fresh runs at ${initialUtxos.toString()} UTxOs`,
    );
  }
  const durations = results.map((result) => result.durationMs);
  groups.push({
    initialUtxos,
    fixtureCreation,
    fixtureBefore,
    fixtureAfter,
    roots: rootTuple(results[0]),
    durationMs: {
      min: Math.min(...durations),
      median: median(durations),
      p95: percentile(durations, 0.95),
      max: Math.max(...durations),
    },
    results,
  });
}

export let verdict;

if (mode === "50k") {
  const p95Ms = groups[0].durationMs.p95;
  verdict = {
    pass: p95Ms < 10_000,
    gate: "50k_complete_root_build_p95_under_10s",
    p95Ms,
    limitMs: 10_000,
  };
} else {
  assert.equal(
    new Set(
      groups.flatMap((group) =>
        group.results.map((result) => result.workloadSha256),
      ),
    ).size,
    1,
    "Growth fixtures did not execute an identical operation stream",
  );
  const medians = groups.map((group) => group.durationMs.median);
  const minimumMedianMs = Math.min(...medians);
  const maximumMedianMs = Math.max(...medians);
  const maxMinSlopePercent =
    ((maximumMedianMs - minimumMedianMs) / minimumMedianMs) * 100;
  verdict = {
    pass: maxMinSlopePercent <= 10,
    gate: "100k_300k_1m_max_min_build_slope_within_10_percent",
    maxMinSlopePercent,
    minimumMedianMs,
    maximumMedianMs,
    limitAbsolutePercent: 10,
  };
}

const finalSourceFiles = validateArchitectureGSourceFileList({
  expected: sourceFiles,
  current: discoverArchitectureGSourceFiles(),
});

assert.deepEqual(
  captureSourceIdentity(finalSourceFiles),
  { gitHead, sourceSha256, diffSha256, gitStatusSha256, gitStatusEntries },
  "Architecture G source identity mutated during root gate execution",
);
