import {
  artifactIsoTimestamp,
  exactObject,
  manifestDigest,
  manifestInteger,
  manifestNumber,
  manifestString,
  parseStressCorpusWalletSetIdentity,
  STRESS_CORPUS_REBUILD_SAMPLE_ALGORITHM,
  STRESS_CORPUS_VERIFICATION_SCHEMA_VERSION,
  type StressCorpusVerificationArtifact,
  type VerifyStressCorpusRebuildSampleResult,
} from "./verify.parse-stress-corpus-wallet-set-identity.js";

export const parseStressCorpusRebuildSampleResult = (
  value: unknown,
  label = "stress corpus rebuildSample",
): VerifyStressCorpusRebuildSampleResult => {
  const root = exactObject(value, label, [
    "algorithm",
    "sampleRate",
    "checkedChainCount",
    "checkedRowCount",
    "sampledChainIds",
    "livePreflightEntries",
  ]);
  if (root.algorithm !== STRESS_CORPUS_REBUILD_SAMPLE_ALGORITHM) {
    throw new Error(`${label}.algorithm is unsupported.`);
  }
  const sampleRate = manifestNumber(root.sampleRate, `${label}.sampleRate`);
  if (sampleRate > 1) {
    throw new Error(`${label}.sampleRate must be <= 1.`);
  }
  if (!Array.isArray(root.sampledChainIds)) {
    throw new Error(`${label}.sampledChainIds must be an array.`);
  }
  const sampledChainIds = root.sampledChainIds.map((entry, index) =>
    manifestString(entry, `${label}.sampledChainIds[${index.toString()}]`),
  );
  if (new Set(sampledChainIds).size !== sampledChainIds.length) {
    throw new Error(`${label}.sampledChainIds must be unique.`);
  }
  if (!Array.isArray(root.livePreflightEntries)) {
    throw new Error(`${label}.livePreflightEntries must be an array.`);
  }
  const livePreflightEntries = root.livePreflightEntries.map((value, index) => {
    const entryLabel = `${label}.livePreflightEntries[${index.toString()}]`;
    const entry = exactObject(value, entryLabel, [
      "walletId",
      "l2Address",
      "firstInputOutref",
      "outputCborSha256",
    ]);
    const firstInputOutref = manifestString(
      entry.firstInputOutref,
      `${entryLabel}.firstInputOutref`,
    );
    if (!/^[0-9a-f]{64}#(0|[1-9][0-9]*)$/u.test(firstInputOutref)) {
      throw new Error(
        `${entryLabel}.firstInputOutref must be a canonical transaction outref.`,
      );
    }
    return {
      walletId: manifestString(entry.walletId, `${entryLabel}.walletId`),
      l2Address: manifestString(entry.l2Address, `${entryLabel}.l2Address`),
      firstInputOutref,
      outputCborSha256: manifestDigest(
        entry.outputCborSha256,
        `${entryLabel}.outputCborSha256`,
      ),
    };
  });
  const checkedChainCount = manifestInteger(
    root.checkedChainCount,
    `${label}.checkedChainCount`,
    1,
  );
  const checkedRowCount = manifestInteger(
    root.checkedRowCount,
    `${label}.checkedRowCount`,
    1,
  );
  if (
    sampledChainIds.length !== checkedChainCount ||
    livePreflightEntries.length !== checkedChainCount ||
    checkedRowCount < checkedChainCount ||
    livePreflightEntries.some(
      (entry, index) => entry.walletId !== sampledChainIds[index],
    )
  ) {
    throw new Error(`${label} cardinality binding is inconsistent.`);
  }
  return {
    algorithm: STRESS_CORPUS_REBUILD_SAMPLE_ALGORITHM,
    sampleRate,
    checkedChainCount,
    checkedRowCount,
    sampledChainIds,
    livePreflightEntries,
  };
};

export const parseStressCorpusVerificationArtifact = (
  value: unknown,
): StressCorpusVerificationArtifact => {
  const root = exactObject(
    value,
    "stress corpus verification artifact",
    ["schemaVersion", "verifiedAtIso", "corpus", "rowCount", "chainCount"],
    ["walletSetIdentity", "rebuildSample"],
  );
  if (root.schemaVersion !== STRESS_CORPUS_VERIFICATION_SCHEMA_VERSION) {
    throw new Error(
      `Unsupported stress corpus verification artifact schemaVersion ${String(root.schemaVersion)}.`,
    );
  }
  const corpus = exactObject(
    root.corpus,
    "stress corpus verification artifact corpus",
    [
      "path",
      "indexPath",
      "manifestPath",
      "corpusSha256",
      "indexSha256",
      "manifestSha256",
    ],
  );
  const rowCount = manifestInteger(
    root.rowCount,
    "stress corpus verification artifact rowCount",
    1,
  );
  const chainCount = manifestInteger(
    root.chainCount,
    "stress corpus verification artifact chainCount",
    1,
  );
  const walletSetIdentity =
    root.walletSetIdentity === undefined
      ? undefined
      : parseStressCorpusWalletSetIdentity(
          root.walletSetIdentity,
          "stress corpus verification artifact walletSetIdentity",
        );
  const rebuildSample =
    root.rebuildSample === undefined
      ? undefined
      : parseStressCorpusRebuildSampleResult(
          root.rebuildSample,
          "stress corpus verification artifact rebuildSample",
        );
  if ((walletSetIdentity === undefined) !== (rebuildSample === undefined)) {
    throw new Error(
      "stress corpus verification artifact walletSetIdentity and rebuildSample must be present together.",
    );
  }
  if (
    walletSetIdentity !== undefined &&
    (walletSetIdentity.walletCount !== chainCount ||
      rebuildSample!.checkedChainCount > chainCount ||
      rebuildSample!.checkedRowCount > rowCount)
  ) {
    throw new Error(
      "stress corpus verification artifact cardinality binding is inconsistent.",
    );
  }
  return {
    schemaVersion: STRESS_CORPUS_VERIFICATION_SCHEMA_VERSION,
    verifiedAtIso: artifactIsoTimestamp(
      root.verifiedAtIso,
      "stress corpus verification artifact verifiedAtIso",
    ),
    corpus: {
      path: manifestString(
        corpus.path,
        "stress corpus verification artifact corpus.path",
      ),
      indexPath: manifestString(
        corpus.indexPath,
        "stress corpus verification artifact corpus.indexPath",
      ),
      manifestPath: manifestString(
        corpus.manifestPath,
        "stress corpus verification artifact corpus.manifestPath",
      ),
      corpusSha256: manifestDigest(
        corpus.corpusSha256,
        "stress corpus verification artifact corpus.corpusSha256",
      ),
      indexSha256: manifestDigest(
        corpus.indexSha256,
        "stress corpus verification artifact corpus.indexSha256",
      ),
      manifestSha256: manifestDigest(
        corpus.manifestSha256,
        "stress corpus verification artifact corpus.manifestSha256",
      ),
    },
    rowCount,
    chainCount,
    ...(walletSetIdentity === undefined ? {} : { walletSetIdentity }),
    ...(rebuildSample === undefined ? {} : { rebuildSample }),
  };
};
