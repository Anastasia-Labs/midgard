import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import {
  createReadStream,
  existsSync,
  mkdirSync,
  readdirSync,
  readFileSync,
  statSync,
  writeFileSync,
} from "node:fs";
import { dirname, resolve } from "node:path";
import { createInterface } from "node:readline";

import { Level } from "level";

import {
  createCanonicalCorpusPrefixSelector,
  projectStressWalletFundingRecord,
  stressWalletFileNameFromId,
  validateCanonicalCorpusVerificationEvidence,
} from "./mpf-architecture-g-corpus.mjs";
import {
  binaryPath,
  corpusIndexPath,
  corpusManifestPath,
  corpusPath,
  corpusSliceId,
  corpusVerificationPath,
  fixtureCreations,
  fixtures,
  gateConfig,
  outPath,
  phase1FormalBinding,
  prepareCorpusOnly,
  probePath,
  runtimeIdentity,
  sha256File,
  transactionCount,
  usesCanonicalCorpus,
  walletsDirectory,
} from "./mpf-architecture-g-gate.fixtures.mjs";
import {
  validateArchitectureGCorpusFundingV1,
  validateArchitectureGCorpusPreparationV1,
  validateArchitectureGFixtureCreationEvidence,
} from "./mpf-architecture-g-gate-config.mjs";
import {
  parseCorpusManifest,
  parseCorpusRowLine,
} from "./throughput-valid-stress-corpus.mjs";

const prepareCanonicalCorpusSlice = async () => {
  if (!usesCanonicalCorpus) return null;
  const manifest = parseCorpusManifest(
    JSON.parse(readFileSync(corpusManifestPath, "utf8")),
  );
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
    const row = parseCorpusRowLine(
      line,
      `Architecture G corpus row ${corpusRows.toString()}`,
    );
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
  for (const walletId of new Set(
    selection.fundingRoots.map((root) => root.walletId),
  )) {
    const record = projectStressWalletFundingRecord(
      JSON.parse(
        readFileSync(
          resolve(walletsDirectory, stressWalletFileNameFromId(walletId)),
          "utf8",
        ),
      ),
    );
    if (record.walletId !== walletId) {
      throw new Error(
        `Stress wallet file for ${walletId} contains ${record.walletId}`,
      );
    }
    walletRecords.set(record.walletId, record);
  }
  const fundingEntries = selection.fundingRoots.map(({ walletId, outref }) => {
    const record = walletRecords.get(walletId);
    if (record === undefined) {
      throw new Error(
        `Missing stress wallet record for corpus chain ${walletId}`,
      );
    }
    const funding = record.fundingUtxos.find(
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
  const sliceSha256 = createHash("sha256").update(sliceBytes).digest("hex");
  const fundingMap = validateArchitectureGCorpusFundingV1({
    artifact: {
      schemaVersion: "midgard-architecture-g-corpus-funding-v1",
      corpusSha256,
      sliceSha256,
      entries: fundingEntries,
    },
    expectedCorpusSha256: corpusSha256,
    expectedSliceSha256: sliceSha256,
    expectedFundingRoots: selection.fundingRoots,
  });
  const fundingMapBytes = Buffer.from(
    `${JSON.stringify(fundingMap, null, 2)}\n`,
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
    sliceSha256,
    sliceRowCount: selection.selectedRowCount,
  };
};

export const canonicalCorpus = await prepareCanonicalCorpusSlice();

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

export const binarySha256 = createHash("sha256")
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

export const fixtureCreationIdentity = (initialUtxos, fixturePath, fixture) => {
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
