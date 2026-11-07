import {
  exactObject,
  manifestDecimal,
  manifestDigest,
  manifestInteger,
  manifestIsoTimestamp,
  manifestNumber,
  manifestString,
  STRESS_CORPUS_MANIFEST_SCHEMA_VERSION,
  STRESS_CORPUS_REBUILD_SAMPLE_ALGORITHM,
  type StressCorpusManifest,
} from "./verify.parse-stress-corpus-wallet-set-identity.js";
import {
  STRESS_CORPUS_FUNDING_SET_HASH_ALGORITHM,
  STRESS_CORPUS_WALLET_SET_HASH_ALGORITHM,
} from "./wallet-set-identity.js";

export const parseStressCorpusManifest = (
  value: unknown,
): StressCorpusManifest => {
  const root = exactObject(value, "stress corpus manifest", [
    "schemaVersion",
    "targetRateTps",
    "durationMs",
    "warmupCount",
    "cooldownCount",
    "safetyFactor",
    "assumedAcceptanceLatencyMs",
    "chainCount",
    "chainDepth",
    "corpusShape",
    "corpusSliceIds",
    "generatedAtIso",
    "generatorGitSha",
    "lucidMidgardVersion",
    "feeParams",
    "network",
    "networkId",
    "maxSubmitTxCborBytes",
    "amountTemplate",
    "verification",
    "fundingSummary",
    "walletSetIdentity",
    "sliceSummary",
    "files",
  ]);
  if (root.schemaVersion !== STRESS_CORPUS_MANIFEST_SCHEMA_VERSION) {
    throw new Error(
      `Unsupported stress corpus manifest schemaVersion ${String(root.schemaVersion)}.`,
    );
  }
  const corpusShape = root.corpusShape;
  if (
    corpusShape !== "fanout" &&
    corpusShape !== "chain" &&
    corpusShape !== "mixed"
  ) {
    throw new Error("stress corpus manifest corpusShape is unsupported.");
  }
  const network = root.network;
  if (network !== "Mainnet" && network !== "Preprod") {
    throw new Error("stress corpus manifest network is unsupported.");
  }
  if (!Array.isArray(root.corpusSliceIds) || root.corpusSliceIds.length === 0) {
    throw new Error(
      "stress corpus manifest corpusSliceIds must be a non-empty array.",
    );
  }
  const corpusSliceIds = root.corpusSliceIds.map((entry, index) =>
    manifestString(entry, `corpusSliceIds[${index.toString()}]`),
  );
  if (new Set(corpusSliceIds).size !== corpusSliceIds.length) {
    throw new Error("stress corpus manifest corpusSliceIds must be unique.");
  }
  const feeParams = exactObject(root.feeParams, "manifest feeParams", [
    "minFeeA",
    "minFeeB",
  ]);
  const amountTemplate = exactObject(
    root.amountTemplate,
    "manifest amountTemplate",
    ["lovelace", "shape"],
  );
  if (amountTemplate.shape !== "self-transfer-change-chain") {
    throw new Error("manifest amountTemplate.shape is unsupported.");
  }
  const verification = exactObject(root.verification, "manifest verification", [
    "rebuildSampleRate",
    "rebuildSampleAlgorithm",
  ]);
  if (
    verification.rebuildSampleAlgorithm !==
    STRESS_CORPUS_REBUILD_SAMPLE_ALGORITHM
  ) {
    throw new Error("manifest rebuild sample algorithm is unsupported.");
  }
  const rebuildSampleRate = manifestNumber(
    verification.rebuildSampleRate,
    "manifest verification.rebuildSampleRate",
  );
  if (rebuildSampleRate > 1) {
    throw new Error("manifest verification.rebuildSampleRate must be <= 1.");
  }
  const fundingSummary = exactObject(
    root.fundingSummary,
    "manifest fundingSummary",
    ["walletCount", "perWalletFundingLovelace", "totalFundingLovelace"],
  );
  const walletSetIdentity = exactObject(
    root.walletSetIdentity,
    "manifest walletSetIdentity",
    [
      "walletCount",
      "fundingRowCount",
      "uniqueFirstFundingOutrefCount",
      "walletSetHashAlgorithm",
      "walletSetSha256",
      "fundingSetHashAlgorithm",
      "fundingSetSha256",
    ],
  );
  if (
    walletSetIdentity.walletSetHashAlgorithm !==
      STRESS_CORPUS_WALLET_SET_HASH_ALGORITHM ||
    walletSetIdentity.fundingSetHashAlgorithm !==
      STRESS_CORPUS_FUNDING_SET_HASH_ALGORITHM
  ) {
    throw new Error("manifest wallet-set hash algorithm is unsupported.");
  }
  if (!Array.isArray(root.sliceSummary) || root.sliceSummary.length === 0) {
    throw new Error(
      "stress corpus manifest sliceSummary must be a non-empty array.",
    );
  }
  const sliceSummary = root.sliceSummary.map((value, index) => {
    const entry = exactObject(
      value,
      `manifest sliceSummary[${index.toString()}]`,
      ["corpusSliceId", "walletCount", "rowCount"],
    );
    return {
      corpusSliceId: manifestString(
        entry.corpusSliceId,
        `manifest sliceSummary[${index.toString()}].corpusSliceId`,
      ),
      walletCount: manifestInteger(
        entry.walletCount,
        `manifest sliceSummary[${index.toString()}].walletCount`,
        1,
      ),
      rowCount: manifestInteger(
        entry.rowCount,
        `manifest sliceSummary[${index.toString()}].rowCount`,
        1,
      ),
    };
  });
  const files = exactObject(root.files, "manifest files", [
    "corpus",
    "index",
    "shards",
  ]);
  const parseBoundFile = (
    value: unknown,
    label: string,
  ): {
    readonly path: string;
    readonly sha256: string;
    readonly rowCount: number;
  } => {
    const file = exactObject(value, label, ["path", "sha256", "rowCount"]);
    return {
      path: manifestString(file.path, `${label}.path`),
      sha256: manifestDigest(file.sha256, `${label}.sha256`),
      rowCount: manifestInteger(file.rowCount, `${label}.rowCount`, 1),
    };
  };
  if (
    !Array.isArray(files.shards) ||
    files.shards.length === 0 ||
    files.shards.some((path) => typeof path !== "string" || path.length === 0)
  ) {
    throw new Error("manifest files.shards must be a non-empty string array.");
  }
  const parsed: StressCorpusManifest = {
    schemaVersion: STRESS_CORPUS_MANIFEST_SCHEMA_VERSION,
    targetRateTps: manifestNumber(root.targetRateTps, "manifest targetRateTps"),
    durationMs: manifestInteger(root.durationMs, "manifest durationMs", 1),
    warmupCount: manifestInteger(root.warmupCount, "manifest warmupCount", 0),
    cooldownCount: manifestInteger(
      root.cooldownCount,
      "manifest cooldownCount",
      0,
    ),
    safetyFactor: manifestNumber(root.safetyFactor, "manifest safetyFactor"),
    assumedAcceptanceLatencyMs: manifestInteger(
      root.assumedAcceptanceLatencyMs,
      "manifest assumedAcceptanceLatencyMs",
      1,
    ),
    chainCount: manifestInteger(root.chainCount, "manifest chainCount", 1),
    chainDepth: manifestInteger(root.chainDepth, "manifest chainDepth", 1),
    corpusShape,
    corpusSliceIds,
    generatedAtIso: manifestIsoTimestamp(
      root.generatedAtIso,
      "manifest generatedAtIso",
    ),
    generatorGitSha: manifestString(
      root.generatorGitSha,
      "manifest generatorGitSha",
    ),
    lucidMidgardVersion: manifestString(
      root.lucidMidgardVersion,
      "manifest lucidMidgardVersion",
    ),
    feeParams: {
      minFeeA: manifestDecimal(feeParams.minFeeA, "manifest feeParams.minFeeA"),
      minFeeB: manifestDecimal(feeParams.minFeeB, "manifest feeParams.minFeeB"),
    },
    network,
    networkId: manifestDecimal(root.networkId, "manifest networkId"),
    maxSubmitTxCborBytes: manifestInteger(
      root.maxSubmitTxCborBytes,
      "manifest maxSubmitTxCborBytes",
      1,
    ),
    amountTemplate: {
      lovelace: manifestDecimal(
        amountTemplate.lovelace,
        "manifest amountTemplate.lovelace",
      ),
      shape: "self-transfer-change-chain",
    },
    verification: {
      rebuildSampleRate,
      rebuildSampleAlgorithm: STRESS_CORPUS_REBUILD_SAMPLE_ALGORITHM,
    },
    fundingSummary: {
      walletCount: manifestInteger(
        fundingSummary.walletCount,
        "manifest fundingSummary.walletCount",
        1,
      ),
      perWalletFundingLovelace: manifestDecimal(
        fundingSummary.perWalletFundingLovelace,
        "manifest fundingSummary.perWalletFundingLovelace",
      ),
      totalFundingLovelace: manifestDecimal(
        fundingSummary.totalFundingLovelace,
        "manifest fundingSummary.totalFundingLovelace",
      ),
    },
    walletSetIdentity: {
      walletCount: manifestInteger(
        walletSetIdentity.walletCount,
        "manifest walletSetIdentity.walletCount",
        1,
      ),
      fundingRowCount: manifestInteger(
        walletSetIdentity.fundingRowCount,
        "manifest walletSetIdentity.fundingRowCount",
        1,
      ),
      uniqueFirstFundingOutrefCount: manifestInteger(
        walletSetIdentity.uniqueFirstFundingOutrefCount,
        "manifest walletSetIdentity.uniqueFirstFundingOutrefCount",
        1,
      ),
      walletSetHashAlgorithm: STRESS_CORPUS_WALLET_SET_HASH_ALGORITHM,
      walletSetSha256: manifestDigest(
        walletSetIdentity.walletSetSha256,
        "manifest walletSetIdentity.walletSetSha256",
      ),
      fundingSetHashAlgorithm: STRESS_CORPUS_FUNDING_SET_HASH_ALGORITHM,
      fundingSetSha256: manifestDigest(
        walletSetIdentity.fundingSetSha256,
        "manifest walletSetIdentity.fundingSetSha256",
      ),
    },
    sliceSummary,
    files: {
      corpus: parseBoundFile(files.corpus, "manifest files.corpus"),
      index: parseBoundFile(files.index, "manifest files.index"),
      shards: files.shards.map((path, index) =>
        manifestString(path, `manifest files.shards[${index.toString()}]`),
      ),
    },
  };
  const expectedNetworkId = parsed.network === "Mainnet" ? "1" : "0";
  const sliceWalletCount = parsed.sliceSummary.reduce(
    (sum, entry) => sum + entry.walletCount,
    0,
  );
  const sliceIds = parsed.sliceSummary.map((entry) => entry.corpusSliceId);
  const expectedRowCount = parsed.chainCount * parsed.chainDepth;
  const expectedFunding =
    BigInt(parsed.fundingSummary.walletCount) *
    BigInt(parsed.fundingSummary.perWalletFundingLovelace);
  if (
    parsed.networkId !== expectedNetworkId ||
    JSON.stringify(sliceIds) !== JSON.stringify(parsed.corpusSliceIds) ||
    new Set(sliceIds).size !== sliceIds.length ||
    new Set(parsed.files.shards).size !== parsed.files.shards.length ||
    parsed.sliceSummary.some(
      (entry) => entry.rowCount !== entry.walletCount * parsed.chainDepth,
    ) ||
    parsed.files.corpus.rowCount !== expectedRowCount ||
    parsed.files.corpus.rowCount !==
      parsed.sliceSummary.reduce((sum, entry) => sum + entry.rowCount, 0) ||
    parsed.files.index.rowCount !== parsed.chainCount ||
    parsed.chainCount !== parsed.fundingSummary.walletCount ||
    sliceWalletCount !== parsed.fundingSummary.walletCount ||
    parsed.fundingSummary.walletCount !==
      parsed.walletSetIdentity.walletCount ||
    parsed.walletSetIdentity.uniqueFirstFundingOutrefCount !==
      parsed.walletSetIdentity.walletCount ||
    BigInt(parsed.fundingSummary.totalFundingLovelace) !== expectedFunding
  ) {
    throw new Error(
      "stress corpus manifest cardinality binding is inconsistent.",
    );
  }
  return parsed;
};
