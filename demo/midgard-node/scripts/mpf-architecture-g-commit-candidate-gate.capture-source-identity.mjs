import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { createHash } from "node:crypto";
import { existsSync, readFileSync } from "node:fs";
import { resolve } from "node:path";

import { EMPTY_MERKLE_TREE_ROOT } from "@al-ft/midgard-sdk";
import { Level } from "level";

import {
  captureArchitectureGPhase1FormalBindingIdentity,
  captureArchitectureGRuntimeIdentity,
  discoverArchitectureGSourceFiles,
  resolveArchitectureGGateConfig,
  validateArchitectureGCrossGateEvidenceIdentity,
  validateArchitectureGCrossGateSourceIdentity,
  validateArchitectureGRootGateSummary,
  validateArchitectureGSourceFileList,
  validateCommitCandidateProbeResult,
} from "./mpf-architecture-g-gate-config.mjs";

const option = (name, fallback) =>
  process.argv
    .find((value) => value.startsWith(`--${name}=`))
    ?.slice(name.length + 3) ?? fallback;

export const config = resolveArchitectureGGateConfig({
  mode: option("mode", "50k"),
  profile: option("profile", "formal"),
  runs: option("runs", undefined),
  transactions: option("transactions", undefined),
});

export const phase1FormalBindingPath = option(
  "phase1-formal-binding",
  process.env.MPF_ARCH_G_PHASE1_FORMAL_BINDING_PATH ?? "",
).trim();

export const phase1FormalBindingSha256 = option(
  "phase1-formal-binding-sha256",
  process.env.MPF_ARCH_G_PHASE1_FORMAL_BINDING_SHA256 ?? "",
).trim();

export const phase1FormalBinding =
  captureArchitectureGPhase1FormalBindingIdentity({
    bindingPath: phase1FormalBindingPath,
    bindingSha256: phase1FormalBindingSha256,
  });

export const expectedRuntimeVersion = option(
  "runtime-version",
  process.env.MPF_ARCH_G_RUNTIME_VERSION ?? "",
).trim();

export const expectedRuntimeExecutableSha256 = option(
  "runtime-executable-sha256",
  process.env.MPF_ARCH_G_RUNTIME_EXECUTABLE_SHA256 ?? "",
).trim();

export const runtimeIdentity = captureArchitectureGRuntimeIdentity({
  expectedVersion: expectedRuntimeVersion,
  expectedExecutableSha256: expectedRuntimeExecutableSha256,
});

export const cpuSet = option(
  "cpuset",
  process.env.MPF_ARCH_G_CPUSET ?? "",
).trim();

if (cpuSet.length === 0) {
  throw new Error("Set --cpuset or MPF_ARCH_G_CPUSET");
}

export const rootGateSummaryPath = option(
  "root-gate-summary",
  process.env.MPF_ARCH_G_ROOT_GATE_SUMMARY ?? "",
).trim();

if (config.formal && rootGateSummaryPath.length === 0) {
  throw new Error("A formal candidate gate requires --root-gate-summary");
}

export const resolvedRootGateSummaryPath =
  rootGateSummaryPath.length === 0 ? null : resolve(rootGateSummaryPath);

const rootGateSummaryBytes =
  resolvedRootGateSummaryPath === null
    ? null
    : readFileSync(resolvedRootGateSummaryPath);

export const rootGateSummarySha256 =
  rootGateSummaryBytes === null
    ? null
    : createHash("sha256").update(rootGateSummaryBytes).digest("hex");

export const rootGateSummary =
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

export const captureSourceIdentity = (sourceFiles) => {
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

export const expectedSourceIdentity =
  rootGateSummary === null
    ? null
    : {
        gitHead: rootGateSummary.gitHead,
        sourceSha256: rootGateSummary.sourceSha256,
        diffSha256: rootGateSummary.diffSha256,
        gitStatusSha256: rootGateSummary.gitStatusSha256,
      };

export let currentSourceIdentity = null;

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

export const probePath = resolve(
  option("probe", "dist/mpf-commit-candidate-probe.js"),
);

if (!existsSync(probePath)) {
  throw new Error(`Missing commit-candidate probe ${probePath}`);
}

export const probeSha256 = createHash("sha256")
  .update(readFileSync(probePath))
  .digest("hex");

const fixtureSizes =
  config.mode === "50k" ? [1_000_000] : [100_000, 300_000, 1_000_000];

export const inputs = new Map(
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

export const outPath = resolve(
  option(
    "out",
    `logs/phase-3-architecture-g-commit-candidate-${config.mode}-${timestamp}/summary.json`,
  ),
);

export const fixtureIdentity = async (path) => {
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

export const rootTuple = (result) => result.candidate.roots;

// The candidate carries no user-event roots; its probe proves the fixture holds
// no user events, so those roots are the empty tree.
export const candidateRootsAsRootGateTuple = (roots) => ({
  utxoRoot: roots.utxos,
  rawTxRoot: roots.rawTransactions,
  txRoot: roots.transactions,
  transitionTraceRoot: roots.transitionTrace,
  eventToStepRoot: roots.eventToStep,
  depositsRoot: EMPTY_MERKLE_TREE_ROOT,
  withdrawalsRoot: EMPTY_MERKLE_TREE_ROOT,
  forcedTransactionsRoot: EMPTY_MERKLE_TREE_ROOT,
});

export const execute = ({
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

export const groups = [];
