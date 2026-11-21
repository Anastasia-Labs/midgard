import { readFile } from "node:fs/promises";

import {
  canonicalDecimal,
  CORPUS_MANIFEST_KEYS,
  CORPUS_MANIFEST_SCHEMA,
  exactObject,
  exactSha256,
  exactString,
  finiteNumber,
  safeInteger,
  SHAPES,
} from "./throughput-valid-stress-corpus.corpus-manifest-keys.mjs";

export const parseCorpusManifest = (value) => {
  const manifest = exactObject(value, "corpus manifest", CORPUS_MANIFEST_KEYS);
  if (manifest.schemaVersion !== CORPUS_MANIFEST_SCHEMA) {
    throw new Error(
      `unsupported corpus manifest schemaVersion ${String(manifest.schemaVersion)}`,
    );
  }
  finiteNumber(manifest.targetRateTps, "corpus manifest targetRateTps");
  safeInteger(manifest.durationMs, "corpus manifest durationMs", 1);
  safeInteger(manifest.warmupCount, "corpus manifest warmupCount", 0);
  safeInteger(manifest.cooldownCount, "corpus manifest cooldownCount", 0);
  finiteNumber(manifest.safetyFactor, "corpus manifest safetyFactor");
  safeInteger(
    manifest.assumedAcceptanceLatencyMs,
    "corpus manifest assumedAcceptanceLatencyMs",
    1,
  );
  const chainCount = safeInteger(
    manifest.chainCount,
    "corpus manifest chainCount",
    1,
  );
  const chainDepth = safeInteger(
    manifest.chainDepth,
    "corpus manifest chainDepth",
    1,
  );
  if (!SHAPES.has(manifest.corpusShape)) {
    throw new Error("corpus manifest corpusShape is unsupported");
  }
  if (
    !Array.isArray(manifest.corpusSliceIds) ||
    manifest.corpusSliceIds.length === 0
  ) {
    throw new Error("corpus manifest corpusSliceIds must be non-empty");
  }
  const corpusSliceIds = manifest.corpusSliceIds.map((entry, index) =>
    exactString(entry, `corpus manifest corpusSliceIds[${index}]`),
  );
  if (new Set(corpusSliceIds).size !== corpusSliceIds.length) {
    throw new Error("corpus manifest corpusSliceIds must be unique");
  }
  const generatedAtIso = exactString(
    manifest.generatedAtIso,
    "corpus manifest generatedAtIso",
  );
  if (
    Number.isNaN(Date.parse(generatedAtIso)) ||
    new Date(generatedAtIso).toISOString() !== generatedAtIso
  ) {
    throw new Error(
      "corpus manifest generatedAtIso must be canonical ISO-8601",
    );
  }
  exactString(manifest.generatorGitSha, "corpus manifest generatorGitSha");
  exactString(
    manifest.lucidMidgardVersion,
    "corpus manifest lucidMidgardVersion",
  );
  const feeParams = exactObject(
    manifest.feeParams,
    "corpus manifest feeParams",
    ["minFeeA", "minFeeB"],
  );
  canonicalDecimal(feeParams.minFeeA, "corpus manifest feeParams.minFeeA");
  canonicalDecimal(feeParams.minFeeB, "corpus manifest feeParams.minFeeB");
  if (manifest.network !== "Mainnet" && manifest.network !== "Preprod") {
    throw new Error("corpus manifest network is unsupported");
  }
  const networkId = canonicalDecimal(
    manifest.networkId,
    "corpus manifest networkId",
  );
  if (
    (manifest.network === "Mainnet" && networkId !== "1") ||
    (manifest.network === "Preprod" && networkId !== "0")
  ) {
    throw new Error("corpus manifest networkId does not match network");
  }
  safeInteger(
    manifest.maxSubmitTxCborBytes,
    "corpus manifest maxSubmitTxCborBytes",
    1,
  );
  const amountTemplate = exactObject(
    manifest.amountTemplate,
    "corpus manifest amountTemplate",
    ["lovelace", "shape"],
  );
  canonicalDecimal(
    amountTemplate.lovelace,
    "corpus manifest amountTemplate.lovelace",
  );
  if (amountTemplate.shape !== "self-transfer-change-chain") {
    throw new Error("corpus manifest amountTemplate.shape is unsupported");
  }
  const verification = exactObject(
    manifest.verification,
    "corpus manifest verification",
    ["rebuildSampleRate", "rebuildSampleAlgorithm"],
  );
  const rebuildSampleRate = finiteNumber(
    verification.rebuildSampleRate,
    "corpus manifest verification.rebuildSampleRate",
  );
  if (rebuildSampleRate > 1) {
    throw new Error(
      "corpus manifest verification.rebuildSampleRate must be <= 1",
    );
  }
  if (
    verification.rebuildSampleAlgorithm !== "sha256-corpus-chain-id-order-v1"
  ) {
    throw new Error(
      "corpus manifest verification.rebuildSampleAlgorithm is unsupported",
    );
  }
  const fundingSummary = exactObject(
    manifest.fundingSummary,
    "corpus manifest fundingSummary",
    ["walletCount", "perWalletFundingLovelace", "totalFundingLovelace"],
  );
  const fundingWalletCount = safeInteger(
    fundingSummary.walletCount,
    "corpus manifest fundingSummary.walletCount",
    1,
  );
  const perWalletFundingLovelace = canonicalDecimal(
    fundingSummary.perWalletFundingLovelace,
    "corpus manifest fundingSummary.perWalletFundingLovelace",
  );
  const totalFundingLovelace = canonicalDecimal(
    fundingSummary.totalFundingLovelace,
    "corpus manifest fundingSummary.totalFundingLovelace",
  );
  const walletSetIdentity = exactObject(
    manifest.walletSetIdentity,
    "corpus manifest walletSetIdentity",
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
  const walletSetCount = safeInteger(
    walletSetIdentity.walletCount,
    "corpus manifest walletSetIdentity.walletCount",
    1,
  );
  safeInteger(
    walletSetIdentity.fundingRowCount,
    "corpus manifest walletSetIdentity.fundingRowCount",
    1,
  );
  const uniqueFirstFundingOutrefCount = safeInteger(
    walletSetIdentity.uniqueFirstFundingOutrefCount,
    "corpus manifest walletSetIdentity.uniqueFirstFundingOutrefCount",
    1,
  );
  if (
    walletSetIdentity.walletSetHashAlgorithm !==
      "sha256-wallet-id-l2-address-lines-v1" ||
    walletSetIdentity.fundingSetHashAlgorithm !==
      "sha256-wallet-id-outref-output-cbor-sha256-lines-v1"
  ) {
    throw new Error(
      "corpus manifest walletSetIdentity hash algorithm is unsupported",
    );
  }
  exactSha256(
    walletSetIdentity.walletSetSha256,
    "corpus manifest walletSetIdentity.walletSetSha256",
  );
  exactSha256(
    walletSetIdentity.fundingSetSha256,
    "corpus manifest walletSetIdentity.fundingSetSha256",
  );
  if (
    !Array.isArray(manifest.sliceSummary) ||
    manifest.sliceSummary.length === 0
  ) {
    throw new Error("corpus manifest sliceSummary must be non-empty");
  }
  let sliceRowCount = 0;
  let sliceWalletCount = 0;
  const observedSliceIds = [];
  for (const [index, entry] of manifest.sliceSummary.entries()) {
    const exactEntry = exactObject(
      entry,
      `corpus manifest sliceSummary[${index}]`,
      ["corpusSliceId", "walletCount", "rowCount"],
    );
    observedSliceIds.push(
      exactString(
        exactEntry.corpusSliceId,
        `corpus manifest sliceSummary[${index}].corpusSliceId`,
      ),
    );
    const walletCount = safeInteger(
      exactEntry.walletCount,
      `corpus manifest sliceSummary[${index}].walletCount`,
      1,
    );
    sliceWalletCount += walletCount;
    const rowCount = safeInteger(
      exactEntry.rowCount,
      `corpus manifest sliceSummary[${index}].rowCount`,
      1,
    );
    if (rowCount !== walletCount * chainDepth) {
      throw new Error(
        `corpus manifest sliceSummary[${index}].rowCount must equal walletCount*chainDepth`,
      );
    }
    sliceRowCount += rowCount;
  }
  if (
    new Set(observedSliceIds).size !== observedSliceIds.length ||
    JSON.stringify(observedSliceIds) !== JSON.stringify(corpusSliceIds)
  ) {
    throw new Error(
      "corpus manifest sliceSummary identities must exactly match corpusSliceIds",
    );
  }
  const files = exactObject(manifest.files, "corpus manifest files", [
    "corpus",
    "index",
    "shards",
  ]);
  for (const artifact of ["corpus", "index"]) {
    const entry = exactObject(
      files[artifact],
      `corpus manifest files.${artifact}`,
      ["path", "sha256", "rowCount"],
    );
    if (
      exactString(entry.path, `corpus manifest files.${artifact}.path`) ===
        "" ||
      exactSha256(entry.sha256, `corpus manifest files.${artifact}.sha256`) ===
        "" ||
      safeInteger(
        entry.rowCount,
        `corpus manifest files.${artifact}.rowCount`,
        1,
      ) < 1
    ) {
      throw new Error(`corpus manifest files.${artifact} is malformed`);
    }
  }
  if (
    !Array.isArray(files.shards) ||
    files.shards.length === 0 ||
    files.shards.some(
      (entry, index) =>
        exactString(entry, `corpus manifest files.shards[${index}]`) === "",
    ) ||
    new Set(files.shards).size !== files.shards.length
  ) {
    throw new Error(
      "corpus manifest files.shards must be non-empty and unique",
    );
  }
  if (
    files.corpus.rowCount !== sliceRowCount ||
    files.corpus.rowCount !== chainCount * chainDepth ||
    files.index.rowCount !== chainCount ||
    chainCount !== fundingWalletCount ||
    sliceWalletCount !== fundingWalletCount ||
    fundingWalletCount !== walletSetCount ||
    uniqueFirstFundingOutrefCount !== walletSetCount ||
    BigInt(totalFundingLovelace) !==
      BigInt(fundingWalletCount) * BigInt(perWalletFundingLovelace)
  ) {
    throw new Error("corpus manifest cardinality binding is inconsistent");
  }
  return manifest;
};

export const defaultCorpusIndexPath = (corpusPath) =>
  `${corpusPath}.index.ndjson`;

export const defaultCorpusManifestPath = (corpusPath) =>
  `${corpusPath}.manifest.json`;

export const loadCorpusManifest = async (manifestPath) => {
  return parseCorpusManifest(JSON.parse(await readFile(manifestPath, "utf8")));
};
