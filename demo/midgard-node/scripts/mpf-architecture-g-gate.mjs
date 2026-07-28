import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { createHash } from "node:crypto";
import {
  createReadStream,
  existsSync,
  mkdirSync,
  readFileSync,
  readdirSync,
  statSync,
  writeFileSync,
} from "node:fs";
import { dirname, resolve } from "node:path";
import { createInterface } from "node:readline";

import { Level } from "level";

import {
  createCanonicalCorpusPrefixSelector,
  validateCanonicalCorpusVerificationEvidence,
} from "./mpf-architecture-g-corpus.mjs";
import {
  captureArchitectureGPhase1FormalBindingIdentity,
  captureArchitectureGRuntimeIdentity,
  discoverArchitectureGSourceFiles,
  resolveArchitectureGGateConfig,
  validateArchitectureGCorpusPreparationV1,
  validateArchitectureGFixtureCreationEvidence,
  validateArchitectureGSourceFileList,
} from "./mpf-architecture-g-gate-config.mjs";

const option = (name, fallback) =>
  process.argv
    .find((value) => value.startsWith(`--${name}=`))
    ?.slice(name.length + 3) ?? fallback;
const gateConfig = resolveArchitectureGGateConfig({
  mode: option("mode", "50k"),
  profile: option("profile", "formal"),
  runs: option("runs", undefined),
  transactions: option("transactions", undefined),
});
const { mode, profile, runs, transactions: transactionCount } = gateConfig;
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
const prepareCorpusOnly = option("prepare-corpus-only", "false") === "true";
const fixtureRoot = option(
  "fixture-root",
  process.env.MPF_ARCH_G_FIXTURE_ROOT ?? "",
).trim();
if (fixtureRoot.length === 0) {
  throw new Error(
    "Set --fixture-root or MPF_ARCH_G_FIXTURE_ROOT to a fresh durable fixture directory",
  );
}
const cpuSet = option("cpuset", process.env.MPF_ARCH_G_CPUSET ?? "").trim();
if (cpuSet.length === 0) {
  throw new Error(
    "Set --cpuset or MPF_ARCH_G_CPUSET for reproducible CPU affinity",
  );
}
const corpusPath = option(
  "corpus",
  process.env.MPF_ARCH_G_CORPUS_PATH ?? "",
).trim();
const corpusManifestPath = option(
  "corpus-manifest",
  process.env.MPF_ARCH_G_CORPUS_MANIFEST_PATH ??
    (corpusPath.length === 0 ? "" : `${corpusPath}.manifest.json`),
).trim();
const corpusSliceId = option(
  "corpus-slice-id",
  process.env.MPF_ARCH_G_CORPUS_SLICE_ID ?? "",
).trim();
const corpusIndexPath = option(
  "corpus-index",
  process.env.MPF_ARCH_G_CORPUS_INDEX_PATH ??
    (corpusPath.length === 0 ? "" : `${corpusPath}.index.ndjson`),
).trim();
const corpusVerificationPath = option(
  "corpus-verification",
  process.env.MPF_ARCH_G_CORPUS_VERIFICATION_PATH ?? "",
).trim();
const walletsDirectory = option(
  "wallets-dir",
  process.env.MPF_ARCH_G_WALLETS_DIR ?? "",
).trim();
const corpusInputs = [
  corpusPath,
  corpusManifestPath,
  corpusIndexPath,
  corpusVerificationPath,
  corpusSliceId,
  walletsDirectory,
];
if (gateConfig.formal && corpusInputs.some((value) => value.length === 0)) {
  throw new Error(
    "A formal gate requires --corpus, --corpus-manifest, --corpus-index, --corpus-verification, --corpus-slice-id, and --wallets-dir",
  );
}
if (
  !gateConfig.formal &&
  corpusInputs.some((value) => value.length > 0) &&
  corpusInputs.some((value) => value.length === 0)
) {
  throw new Error(
    "A smoke gate must supply either every canonical corpus input or none of them",
  );
}
const usesCanonicalCorpus = corpusPath.length > 0;
const fixtures = new Map(
  (mode === "50k" ? [1_000_000] : [100_000, 300_000, 1_000_000]).map(
    (utxos) => [
      utxos,
      resolve(
        option(
          `fixture-${utxos.toString()}`,
          resolve(fixtureRoot, `utxos-${utxos.toString()}-level`),
        ),
      ),
    ],
  ),
);
const fixtureCreations = new Map(
  [...fixtures.keys()].map((utxos) => [
    utxos,
    resolve(
      option(
        `fixture-creation-${utxos.toString()}`,
        resolve(fixtureRoot, "..", `fixture-create-${utxos.toString()}.json`),
      ),
    ),
  ]),
);
const binaryPath = resolve(
  option(
    "binary",
    "native/mpf-event-flat-wasm/target/release/architecture-g-owner",
  ),
);
const probePath = resolve(option("probe", "dist/mpf-engine-probe.js"));
for (const path of usesCanonicalCorpus
  ? [
      corpusPath,
      corpusManifestPath,
      corpusIndexPath,
      corpusVerificationPath,
      walletsDirectory,
    ]
  : []) {
  if (!existsSync(path)) {
    throw new Error(`Missing Architecture G gate input: ${path}`);
  }
}
const timestamp = new Date().toISOString().replaceAll(/[-:.]/g, "");
const outPath = resolve(
  option(
    "out",
    `logs/phase-3-architecture-g-${mode}-${timestamp}/summary.json`,
  ),
);

const sha256File = async (path) => {
  const hash = createHash("sha256");
  for await (const chunk of createReadStream(path)) hash.update(chunk);
  return hash.digest("hex");
};
const prepareCanonicalCorpusSlice = async () => {
  if (!usesCanonicalCorpus) return null;
  const manifest = JSON.parse(readFileSync(corpusManifestPath, "utf8"));
  if (manifest.schemaVersion !== "midgard-stress-corpus-manifest-v1") {
    throw new Error(
      `Unsupported canonical corpus manifest schema: ${String(manifest.schemaVersion)}`,
    );
  }
  const expectedCorpusSha256 = manifest.files?.corpus?.sha256;
  if (!/^[0-9a-f]{64}$/.test(expectedCorpusSha256 ?? "")) {
    throw new Error(
      "Canonical corpus manifest has no valid files.corpus.sha256",
    );
  }
  const corpusSha256 = await sha256File(corpusPath);
  assert.equal(
    corpusSha256,
    expectedCorpusSha256,
    "Canonical corpus does not match its manifest SHA-256",
  );
  if (
    !Array.isArray(manifest.corpusSliceIds) ||
    !manifest.corpusSliceIds.includes(corpusSliceId)
  ) {
    throw new Error(`Manifest does not declare corpus slice ${corpusSliceId}`);
  }
  const expectedIndexSha256 = manifest.files?.index?.sha256;
  if (!/^[0-9a-f]{64}$/.test(expectedIndexSha256 ?? "")) {
    throw new Error(
      "Canonical corpus manifest has no valid files.index.sha256",
    );
  }
  const indexSha256 = await sha256File(corpusIndexPath);
  assert.equal(
    indexSha256,
    expectedIndexSha256,
    "Canonical corpus index does not match its manifest SHA-256",
  );
  const verificationBytes = readFileSync(corpusVerificationPath);
  const verificationArtifact = JSON.parse(verificationBytes.toString("utf8"));
  validateCanonicalCorpusVerificationEvidence({
    artifact: verificationArtifact,
    corpusSha256,
    indexSha256,
    rowCount: manifest.files.corpus.rowCount,
    chainCount: manifest.chainCount,
  });
  const selector = createCanonicalCorpusPrefixSelector({
    corpusSliceId,
    transactionCount,
  });
  const input = createInterface({
    input: createReadStream(corpusPath, { encoding: "utf8" }),
    crlfDelay: Infinity,
  });
  let corpusRows = 0;
  for await (const line of input) {
    if (line.trim().length === 0) continue;
    corpusRows += 1;
    const row = JSON.parse(line);
    selector.consider({ line, row, corpusRowNumber: corpusRows });
  }
  assert.equal(
    corpusRows,
    manifest.files.corpus.rowCount,
    "Canonical corpus row count does not match its manifest",
  );
  const selection = selector.finish();
  mkdirSync(dirname(outPath), { recursive: true });
  const slicePath = resolve(dirname(outPath), "canonical-corpus-slice.ndjson");
  const sliceBytes = Buffer.from(`${selection.selectedLines.join("\n")}\n`);
  writeFileSync(slicePath, sliceBytes);
  const walletRecords = new Map();
  for (const entry of readdirSync(walletsDirectory, { withFileTypes: true })) {
    if (!entry.isFile() || !entry.name.endsWith(".json")) continue;
    const record = JSON.parse(
      readFileSync(resolve(walletsDirectory, entry.name), "utf8"),
    );
    if (
      record?.schemaVersion === "midgard-stress-wallet-v1" &&
      typeof record.walletId === "string"
    ) {
      if (walletRecords.has(record.walletId)) {
        throw new Error(`Duplicate stress wallet record ${record.walletId}`);
      }
      walletRecords.set(record.walletId, record);
    }
  }
  const fundingEntries = selection.fundingRoots.map(({ walletId, outref }) => {
    const record = walletRecords.get(walletId);
    if (record === undefined) {
      throw new Error(
        `Missing stress wallet record for corpus chain ${walletId}`,
      );
    }
    const funding = record.latestFunding?.fundingUtxos?.find(
      (candidate) => candidate?.outref === outref,
    );
    const outputCbor = funding?.outputCbor;
    if (
      typeof outputCbor !== "string" ||
      outputCbor.length === 0 ||
      outputCbor.length % 2 !== 0 ||
      Buffer.from(outputCbor, "hex").toString("hex") !==
        outputCbor.toLowerCase()
    ) {
      throw new Error(
        `Missing canonical funding output ${outref} for corpus chain ${walletId}`,
      );
    }
    return { walletId, outref, outputCbor: outputCbor.toLowerCase() };
  });
  const fundingMapPath = resolve(
    dirname(outPath),
    "canonical-corpus-funding.json",
  );
  const fundingMapBytes = Buffer.from(
    `${JSON.stringify(
      {
        schemaVersion: "midgard-architecture-g-corpus-funding-v1",
        corpusSha256,
        sliceSha256: createHash("sha256").update(sliceBytes).digest("hex"),
        entries: fundingEntries,
      },
      null,
      2,
    )}\n`,
  );
  writeFileSync(fundingMapPath, fundingMapBytes);
  return {
    corpusPath: resolve(corpusPath),
    manifestPath: resolve(corpusManifestPath),
    manifestSha256: await sha256File(corpusManifestPath),
    corpusSha256,
    indexPath: resolve(corpusIndexPath),
    indexSha256,
    verificationPath: resolve(corpusVerificationPath),
    verificationSha256: createHash("sha256")
      .update(verificationBytes)
      .digest("hex"),
    corpusManifestRowCount: manifest.files.corpus.rowCount,
    parentSliceId: selection.parentSliceId,
    parentSliceRowsSeen: selection.parentSliceRowsSeen,
    parentSliceChainCount: selection.parentSliceChainCount,
    verifiedCorpusChainCount: selection.verifiedCorpusChainCount,
    sliceChainsContiguous: selection.sliceChainsContiguous,
    chainsCrossSliceBoundaries: selection.chainsCrossSliceBoundaries,
    selectionAlgorithm: selection.selectionAlgorithm,
    sourceCorpusRowRange: selection.sourceCorpusRowRange,
    sourceSliceOrdinalRange: selection.sourceSliceOrdinalRange,
    completeChainCount: selection.completeChainCount,
    finalChainPrefixLength: selection.finalChainPrefixLength,
    fundingRootOutrefs: selection.fundingRootOutrefs,
    fundingRoots: selection.fundingRoots,
    fundingRootsSha256: selection.fundingRootsSha256,
    fundingMapPath,
    fundingMapSha256: createHash("sha256")
      .update(fundingMapBytes)
      .digest("hex"),
    fundingEntryCount: fundingEntries.length,
    slicePath,
    sliceSha256: createHash("sha256").update(sliceBytes).digest("hex"),
    sliceRowCount: selection.selectedRowCount,
  };
};
const canonicalCorpus = await prepareCanonicalCorpusSlice();
if (canonicalCorpus !== null) {
  assert.deepEqual(
    {
      corpusPath: canonicalCorpus.corpusPath,
      corpusSha256: canonicalCorpus.corpusSha256,
      indexPath: canonicalCorpus.indexPath,
      indexSha256: canonicalCorpus.indexSha256,
      manifestPath: canonicalCorpus.manifestPath,
      manifestSha256: canonicalCorpus.manifestSha256,
      sliceId: canonicalCorpus.parentSliceId,
      generationResultPath: canonicalCorpus.verificationPath,
      generationResultSha256: canonicalCorpus.verificationSha256,
    },
    {
      corpusPath: phase1FormalBinding.corpus.path,
      corpusSha256: phase1FormalBinding.corpus.corpusSha256,
      indexPath: phase1FormalBinding.corpus.indexPath,
      indexSha256: phase1FormalBinding.corpus.indexSha256,
      manifestPath: phase1FormalBinding.corpus.manifestPath,
      manifestSha256: phase1FormalBinding.corpus.manifestSha256,
      sliceId: phase1FormalBinding.corpus.sliceId,
      generationResultPath: phase1FormalBinding.generationResult.path,
      generationResultSha256: phase1FormalBinding.generationResult.sha256,
    },
    "Architecture G corpus inputs do not match the verified Phase 1 formal binding",
  );
}
if (prepareCorpusOnly) {
  if (canonicalCorpus === null) {
    throw new Error(
      "--prepare-corpus-only=true requires canonical corpus inputs",
    );
  }
  const corpusPreparation = validateArchitectureGCorpusPreparationV1({
    artifact: {
      schemaVersion: "midgard-architecture-g-corpus-preparation-v1",
      formalGateEvidence: false,
      phase1FormalBinding,
      runtimeIdentity,
      canonicalCorpus,
    },
    transactions: transactionCount,
  });
  process.stdout.write(`${JSON.stringify(corpusPreparation)}\n`);
  process.exit(0);
}
for (const path of [
  binaryPath,
  probePath,
  ...fixtures.values(),
  ...(gateConfig.formal ? fixtureCreations.values() : []),
]) {
  if (!existsSync(path)) {
    throw new Error(`Missing Architecture G gate input: ${path}`);
  }
}
const binarySha256 = createHash("sha256")
  .update(readFileSync(binaryPath))
  .digest("hex");

const directoryBytes = (path) =>
  readdirSync(path, { withFileTypes: true }).reduce((total, entry) => {
    const entryPath = resolve(path, entry.name);
    return (
      total +
      (entry.isDirectory()
        ? directoryBytes(entryPath)
        : statSync(entryPath).size)
    );
  }, 0);
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
    if (typeof marker !== "string" || !/^[0-9a-f]{64}$/.test(marker)) {
      throw new Error(`Fixture ${path} has no canonical __root__ marker`);
    }
    return {
      path,
      directoryBytes: directoryBytes(path),
      logicalSha256: hash.digest("hex"),
      records,
      marker,
    };
  } finally {
    await db.close();
  }
};
const fixtureCreationIdentity = (initialUtxos, fixturePath, fixture) => {
  const path = fixtureCreations.get(initialUtxos);
  if (path === undefined) {
    throw new Error(
      `Missing fixture creation evidence path for ${initialUtxos.toString()}`,
    );
  }
  const bytes = readFileSync(path);
  const artifact = JSON.parse(bytes.toString("utf8"));
  const utxoPayloadAggregate = validateArchitectureGFixtureCreationEvidence({
    artifact: {
      ...artifact,
      fixturePath: resolve(String(artifact.fixturePath ?? "")),
    },
    expectedFixturePath: fixturePath,
    expectedMarker: fixture.marker,
    expectedUtxos: initialUtxos,
  });
  return {
    path,
    sha256: createHash("sha256").update(bytes).digest("hex"),
    initialUtxoCount: artifact.initialUtxoCount,
    marker: artifact.marker,
    utxoPayloadAggregate,
  };
};
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
const sourceFiles = discoverArchitectureGSourceFiles();
const { gitHead, sourceSha256, diffSha256, gitStatusSha256, gitStatusEntries } =
  captureSourceIdentity(sourceFiles);
const probeSha256 = createHash("sha256")
  .update(readFileSync(probePath))
  .digest("hex");
const cgroupMembership = existsSync("/proc/self/cgroup")
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
const memoryMaxPath = memoryMaxCandidates.find(existsSync);
const cgroupMemoryMax =
  memoryMaxPath === undefined
    ? "unavailable"
    : readFileSync(memoryMaxPath, "utf8").trim();

const percentile = (values, quantile) => {
  const sorted = [...values].sort((left, right) => left - right);
  return sorted[Math.max(0, Math.ceil(sorted.length * quantile) - 1)];
};
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
        MPF_ENGINE: "architecture_g",
        MPF_ENGINE_PROBE_TXS: transactionCount.toString(),
        MPF_ENGINE_PROBE_INITIAL_UTXOS: initialUtxos.toString(),
        MPF_ENGINE_PROBE_LEVEL_DB: fixturePath,
        MPF_ENGINE_PROBE_REUSE_LEVEL_DB: "true",
        MPF_ENGINE_PROBE_KEEP_LEVEL_DB: "true",
        MPF_ENGINE_PROBE_PARALLEL_ROOTS: "true",
        MPF_NATIVE_OWNER_BINARY_PATH: binaryPath,
        MPF_NATIVE_OWNER_BINARY_SHA256: binarySha256,
        MPF_NATIVE_OWNER_SIDECAR_PATH: `${fixturePath}.architecture-g-gate.sidecar`,
        ...(canonicalCorpus === null
          ? {}
          : {
              MPF_ENGINE_PROBE_CORPUS_SLICE_PATH: canonicalCorpus.slicePath,
              MPF_ENGINE_PROBE_CORPUS_SLICE_SHA256: canonicalCorpus.sliceSha256,
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

const groups = [];
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

let verdict;
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
const summary = {
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
};
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
