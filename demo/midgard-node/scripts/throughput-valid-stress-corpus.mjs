import { createHash } from "node:crypto";
import { createReadStream, lstatSync } from "node:fs";
import { mkdtemp, open, readFile, rm, writeFile } from "node:fs/promises";
import os from "node:os";
import path from "node:path";
import readline from "node:readline";

import {
  computeMidgardNativeTxIdV1,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
} from "@al-ft/midgard-core/codec";

const TX_HASH_PATTERN = /^[0-9a-f]{64}$/u;
const SHA256_PATTERN = /^[0-9a-f]{64}$/u;
const OUTREF_PATTERN = /^[0-9a-f]{64}#(0|[1-9][0-9]*)$/u;
const SHAPES = new Set(["fanout", "chain", "mixed"]);
const CORPUS_MANIFEST_SCHEMA = "midgard-stress-corpus-manifest-v1";
const CORPUS_MANIFEST_KEYS = [
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
];
const CORPUS_ROW_KEYS = [
  "txHash",
  "canonicalCborHex",
  "canonicalCborSha256",
  "canonicalCborByteLength",
  "senderWalletId",
  "selectedInputOutref",
  "outputOutrefs",
  "planShape",
  "parentTxHash",
  "corpusSliceId",
];
const CORPUS_INDEX_KEYS = [
  "corpusSliceId",
  "planShape",
  "chainId",
  "startByteOffset",
  "endByteOffset",
  "rowCount",
];
const DEFAULT_UNIQUENESS_CHUNK_ENTRIES = 50_000;
const MAX_STREAMING_CORPUS_BUFFERED_ROWS = 8_192;
const POSITIONAL_READ_BYTES = 64 * 1024;

const sha256Hex = (bytes) => createHash("sha256").update(bytes).digest("hex");

export const CORPUS_PREFIX_EVIDENCE_SCHEMA =
  "midgard-stress-corpus-prefix-evidence-v1";

const corpusRowEvidenceBytes = ({
  chainIndex,
  chainId,
  rowIndex,
  txHash,
  canonicalCborSha256,
  rowSha256,
}) =>
  Buffer.from(
    `${chainIndex.toString()}\0${chainId}\0${rowIndex.toString()}\0${txHash}\0${canonicalCborSha256}\0${rowSha256}\n`,
    "utf8",
  );

const corpusRowEvidence = ({
  chainIndex,
  chainId,
  rowIndex,
  row,
  rowSha256,
}) => ({
  chainIndex,
  chainId,
  rowIndex,
  txHash: row.txHash,
  canonicalCborSha256: row.canonicalCborSha256,
  rowSha256,
});

const parseJsonLine = (line, label) => {
  try {
    return JSON.parse(line);
  } catch (error) {
    throw new Error(
      `${label} is not valid JSON: ${error instanceof Error ? error.message : String(error)}`,
    );
  }
};

const exactObject = (value, label, keys) => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${label} must be a JSON object`);
  }
  const missing = keys.filter((key) => !Object.hasOwn(value, key));
  const extra = Object.keys(value).filter((key) => !keys.includes(key));
  if (missing.length > 0 || extra.length > 0) {
    throw new Error(
      `${label} keys must be exact; missing=[${missing.join(",")}], extra=[${extra.join(",")}]`,
    );
  }
  return value;
};

const exactString = (value, label) => {
  if (
    typeof value !== "string" ||
    value.length === 0 ||
    value !== value.trim()
  ) {
    throw new Error(`${label} must be a non-empty exact string`);
  }
  return value;
};

const safeInteger = (value, label, minimum) => {
  if (!Number.isSafeInteger(value) || value < minimum) {
    throw new Error(`${label} must be a safe integer >= ${minimum.toString()}`);
  }
  return value;
};

const finiteNumber = (value, label, allowZero = false) => {
  if (
    typeof value !== "number" ||
    !Number.isFinite(value) ||
    (allowZero ? value < 0 : value <= 0)
  ) {
    throw new Error(
      `${label} must be a finite ${allowZero ? "non-negative" : "positive"} number`,
    );
  }
  return value;
};

const canonicalDecimal = (value, label) => {
  const text = exactString(value, label);
  if (!/^(0|[1-9][0-9]*)$/u.test(text)) {
    throw new Error(`${label} must be a canonical non-negative decimal`);
  }
  return text;
};

const exactSha256 = (value, label) => {
  const digest = exactString(value, label);
  if (!SHA256_PATTERN.test(digest)) {
    throw new Error(`${label} must be an exact lowercase SHA-256`);
  }
  return digest;
};

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

const sha256File = async (filePath) =>
  new Promise((resolve, reject) => {
    const hash = createHash("sha256");
    const input = createReadStream(filePath);
    input.on("data", (chunk) => hash.update(chunk));
    input.on("error", reject);
    input.on("end", () => resolve(hash.digest("hex")));
  });

const requiredManifestArtifactSha256 = (manifest, artifact) => {
  const sha256 = manifest?.files?.[artifact]?.sha256;
  if (typeof sha256 !== "string" || !SHA256_PATTERN.test(sha256)) {
    throw new Error(
      `corpus manifest files.${artifact}.sha256 must be 32-byte hex`,
    );
  }
  return sha256.toLowerCase();
};

export const verifyCorpusArtifactIdentity = async ({
  corpusPath,
  indexPath,
  manifestPath,
  manifest,
}) => {
  const exactManifest = parseCorpusManifest(manifest);
  const persistedManifest = await loadCorpusManifest(manifestPath);
  if (JSON.stringify(exactManifest) !== JSON.stringify(persistedManifest)) {
    throw new Error(
      "supplied corpus manifest does not match the persisted manifest bytes",
    );
  }
  if (
    path.resolve(exactManifest.files.corpus.path) !==
      path.resolve(corpusPath) ||
    path.resolve(exactManifest.files.index.path) !== path.resolve(indexPath)
  ) {
    throw new Error(
      "corpus manifest paths do not bind the requested corpus and index",
    );
  }
  const [corpusSha256, indexSha256, manifestSha256] = await Promise.all([
    sha256File(corpusPath),
    sha256File(indexPath),
    sha256File(manifestPath),
  ]);
  const expectedCorpusSha256 = requiredManifestArtifactSha256(
    exactManifest,
    "corpus",
  );
  const expectedIndexSha256 = requiredManifestArtifactSha256(
    exactManifest,
    "index",
  );
  if (corpusSha256 !== expectedCorpusSha256) {
    throw new Error(
      `corpus sha256 ${corpusSha256} does not match manifest ${expectedCorpusSha256}`,
    );
  }
  if (indexSha256 !== expectedIndexSha256) {
    throw new Error(
      `corpus index sha256 ${indexSha256} does not match manifest ${expectedIndexSha256}`,
    );
  }
  return {
    corpusSha256,
    indexSha256,
    manifestSha256,
    manifestExpectedCorpusSha256: expectedCorpusSha256,
    manifestExpectedIndexSha256: expectedIndexSha256,
    manifestMatchesArtifacts: true,
  };
};

export const loadCorpusIndex = async (indexPath) =>
  (await readFile(indexPath, "utf8"))
    .split(/\r?\n/u)
    .map((line) => line.trim())
    .filter((line) => line.length > 0)
    .map((line, index) => {
      const parsed = exactObject(
        parseJsonLine(line, `corpus index row ${index + 1}`),
        `corpus index row ${index + 1}`,
        CORPUS_INDEX_KEYS,
      );
      if (
        typeof parsed.corpusSliceId !== "string" ||
        typeof parsed.chainId !== "string" ||
        !SHAPES.has(parsed.planShape) ||
        !Number.isSafeInteger(parsed.startByteOffset) ||
        !Number.isSafeInteger(parsed.endByteOffset) ||
        !Number.isSafeInteger(parsed.rowCount) ||
        parsed.startByteOffset < 0 ||
        parsed.endByteOffset < parsed.startByteOffset ||
        parsed.rowCount <= 0
      ) {
        throw new Error(`corpus index row ${index + 1} is invalid`);
      }
      return {
        corpusSliceId: parsed.corpusSliceId,
        planShape: parsed.planShape,
        chainId: parsed.chainId,
        startByteOffset: parsed.startByteOffset,
        endByteOffset: parsed.endByteOffset,
        rowCount: parsed.rowCount,
      };
    });

export const selectCorpusIndexEntries = ({
  index,
  corpusSliceId,
  corpusShape,
  maxChains,
}) => {
  const matching = index.filter(
    (entry) =>
      entry.corpusSliceId === corpusSliceId && entry.planShape === corpusShape,
  );
  if (matching.length === 0) {
    throw new Error(
      `corpus slice ${corpusSliceId} has no ${corpusShape} chain ranges`,
    );
  }
  return Number.isSafeInteger(maxChains) && maxChains > 0
    ? matching.slice(0, maxChains)
    : matching;
};

export const parseCorpusRowLine = (line, label) => {
  const row = exactObject(parseJsonLine(line, label), label, CORPUS_ROW_KEYS);
  for (const field of [
    "txHash",
    "canonicalCborHex",
    "canonicalCborSha256",
    "senderWalletId",
    "selectedInputOutref",
    "corpusSliceId",
  ]) {
    if (
      typeof row[field] !== "string" ||
      row[field].length === 0 ||
      row[field] !== row[field].trim()
    ) {
      throw new Error(`${label}.${field} must be an exact non-empty string`);
    }
  }
  for (const field of ["txHash", "canonicalCborHex", "canonicalCborSha256"]) {
    if (row[field] !== row[field].trim().toLowerCase()) {
      throw new Error(`${label}.${field} must use exact lowercase encoding`);
    }
  }
  if (!TX_HASH_PATTERN.test(row.txHash)) {
    throw new Error(`${label}.txHash must be 32-byte hex`);
  }
  if (!SHA256_PATTERN.test(row.canonicalCborSha256)) {
    throw new Error(`${label}.canonicalCborSha256 must be 32-byte hex`);
  }
  const cborBytes = Buffer.from(row.canonicalCborHex, "hex");
  if (
    row.canonicalCborHex.length === 0 ||
    row.canonicalCborHex.length % 2 !== 0 ||
    cborBytes.toString("hex") !== row.canonicalCborHex
  ) {
    throw new Error(`${label}.canonicalCborHex must be valid hex`);
  }
  if (sha256Hex(cborBytes) !== row.canonicalCborSha256) {
    throw new Error(`${label}.canonicalCborSha256 does not match CBOR bytes`);
  }
  if (
    !Number.isSafeInteger(row.canonicalCborByteLength) ||
    row.canonicalCborByteLength !== cborBytes.length
  ) {
    throw new Error(`${label}.canonicalCborByteLength does not match CBOR`);
  }
  let outputCount;
  let computedTxHash;
  try {
    const nativeTx = decodeMidgardNativeTxFullV1FromCanonicalCbor(cborBytes);
    computedTxHash = computeMidgardNativeTxIdV1(nativeTx).toString("hex");
    outputCount = decodeMidgardNativeByteListPreimage(
      nativeTx.body.outputsPreimageCbor,
      `${label}.outputs`,
    ).length;
  } catch (cause) {
    throw new Error(
      `${label}.canonicalCborHex must be canonical Midgard native V1 transaction CBOR: ${cause instanceof Error ? cause.message : String(cause)}`,
    );
  }
  if (computedTxHash !== row.txHash) {
    throw new Error(`${label}.txHash does not bind canonicalCborHex`);
  }
  if (!OUTREF_PATTERN.test(row.selectedInputOutref)) {
    throw new Error(
      `${label}.selectedInputOutref must be canonical <64hex>#<index>`,
    );
  }
  if (
    !Array.isArray(row.outputOutrefs) ||
    row.outputOutrefs.some(
      (entry) =>
        typeof entry !== "string" ||
        entry.length === 0 ||
        entry !== entry.trim(),
    )
  ) {
    throw new Error(`${label}.outputOutrefs must be an exact string array`);
  }
  if (
    row.outputOutrefs.length !== outputCount ||
    row.outputOutrefs.some(
      (outref, outputIndex) =>
        outref !== `${row.txHash}#${outputIndex.toString()}`,
    )
  ) {
    throw new Error(
      `${label}.outputOutrefs must exactly enumerate canonicalCborHex outputs`,
    );
  }
  if (!SHAPES.has(row.planShape)) {
    throw new Error(`${label}.planShape is unsupported`);
  }
  if (
    row.parentTxHash !== null &&
    (typeof row.parentTxHash !== "string" ||
      !TX_HASH_PATTERN.test(row.parentTxHash) ||
      row.parentTxHash !== row.parentTxHash.toLowerCase())
  ) {
    throw new Error(`${label}.parentTxHash must be null or 32-byte hex`);
  }
  return {
    txHash: row.txHash,
    canonicalCborHex: row.canonicalCborHex,
    canonicalCborSha256: row.canonicalCborSha256,
    canonicalCborByteLength: row.canonicalCborByteLength,
    senderWalletId: row.senderWalletId,
    selectedInputOutref: row.selectedInputOutref,
    outputOutrefs: row.outputOutrefs,
    planShape: row.planShape,
    parentTxHash: row.parentTxHash === null ? null : row.parentTxHash,
    corpusSliceId: row.corpusSliceId,
  };
};

async function* readIndexedRangeLines(corpusPath, entry) {
  if (entry.endByteOffset <= entry.startByteOffset) {
    return;
  }
  const input = createReadStream(corpusPath, {
    encoding: "utf8",
    start: entry.startByteOffset,
    end: entry.endByteOffset - 1,
  });
  const reader = readline.createInterface({
    input,
    crlfDelay: Infinity,
  });
  for await (const line of reader) {
    const trimmed = line.trim();
    if (trimmed.length > 0) {
      yield trimmed;
    }
  }
}

const heapPush = (heap, value) => {
  heap.push(value);
  let index = heap.length - 1;
  while (index > 0) {
    const parent = Math.floor((index - 1) / 2);
    if (heap[parent].value <= heap[index].value) break;
    [heap[parent], heap[index]] = [heap[index], heap[parent]];
    index = parent;
  }
};

const heapPop = (heap) => {
  const first = heap[0];
  const last = heap.pop();
  if (heap.length > 0) {
    heap[0] = last;
    let index = 0;
    while (true) {
      const left = index * 2 + 1;
      const right = left + 1;
      let smallest = index;
      if (left < heap.length && heap[left].value < heap[smallest].value) {
        smallest = left;
      }
      if (right < heap.length && heap[right].value < heap[smallest].value) {
        smallest = right;
      }
      if (smallest === index) break;
      [heap[index], heap[smallest]] = [heap[smallest], heap[index]];
      index = smallest;
    }
  }
  return first;
};

const makeExactUniquenessSpool = ({
  directory,
  name,
  duplicateLabel,
  chunkEntries,
}) => {
  let buffered = [];
  const chunkPaths = [];
  let valueCount = 0;

  const flush = async () => {
    if (buffered.length === 0) return;
    buffered.sort();
    for (let index = 1; index < buffered.length; index += 1) {
      if (buffered[index] === buffered[index - 1]) {
        throw new Error(
          `duplicate ${duplicateLabel} ${buffered[index]} in selected corpus`,
        );
      }
    }
    const chunkPath = path.join(
      directory,
      `${name}-${String(chunkPaths.length).padStart(6, "0")}.ndjson`,
    );
    await writeFile(
      chunkPath,
      `${buffered.map((value) => JSON.stringify(value)).join("\n")}\n`,
      { encoding: "utf8", flag: "wx" },
    );
    chunkPaths.push(chunkPath);
    buffered = [];
  };

  return {
    async add(value) {
      buffered.push(value);
      valueCount += 1;
      if (buffered.length >= chunkEntries) await flush();
    },
    async verify() {
      await flush();
      const streams = [];
      const readers = [];
      const iterators = [];
      const heap = [];
      try {
        for (let index = 0; index < chunkPaths.length; index += 1) {
          const stream = createReadStream(chunkPaths[index], {
            encoding: "utf8",
          });
          const reader = readline.createInterface({
            input: stream,
            crlfDelay: Infinity,
          });
          const iterator = reader[Symbol.asyncIterator]();
          streams.push(stream);
          readers.push(reader);
          iterators.push(iterator);
          const next = await iterator.next();
          if (!next.done) {
            heapPush(heap, { value: JSON.parse(next.value), index });
          }
        }

        let previous = null;
        let mergedCount = 0;
        while (heap.length > 0) {
          const current = heapPop(heap);
          if (previous !== null && current.value === previous) {
            throw new Error(
              `duplicate ${duplicateLabel} ${current.value} in selected corpus`,
            );
          }
          previous = current.value;
          mergedCount += 1;
          const next = await iterators[current.index].next();
          if (!next.done) {
            heapPush(heap, {
              value: JSON.parse(next.value),
              index: current.index,
            });
          }
        }
        if (mergedCount !== valueCount) {
          throw new Error(
            `${duplicateLabel} uniqueness spool expected ${valueCount} values, merged ${mergedCount}`,
          );
        }
        return valueCount;
      } finally {
        for (const reader of readers) reader.close();
        for (const stream of streams) stream.destroy();
      }
    },
  };
};

export const validateCorpusSlice = async ({
  corpusPath,
  indexEntries,
  uniquenessChunkEntries = DEFAULT_UNIQUENESS_CHUNK_ENTRIES,
  temporaryDirectory = os.tmpdir(),
}) => {
  if (
    !Number.isSafeInteger(uniquenessChunkEntries) ||
    uniquenessChunkEntries <= 0
  ) {
    throw new Error("uniquenessChunkEntries must be a positive integer");
  }
  if (
    typeof temporaryDirectory !== "string" ||
    temporaryDirectory.length === 0
  ) {
    throw new Error("temporaryDirectory must be a non-empty path");
  }
  const uniquenessDirectory = await mkdtemp(
    path.join(temporaryDirectory, "midgard-corpus-uniqueness-"),
  );
  const txHashes = makeExactUniquenessSpool({
    directory: uniquenessDirectory,
    name: "tx-hashes",
    duplicateLabel: "txHash",
    chunkEntries: uniquenessChunkEntries,
  });
  const inputs = makeExactUniquenessSpool({
    directory: uniquenessDirectory,
    name: "selected-inputs",
    duplicateLabel: "selected input",
    chunkEntries: uniquenessChunkEntries,
  });
  let rowCount = 0;
  try {
    for (const entry of indexEntries) {
      let rowsInRange = 0;
      for await (const line of readIndexedRangeLines(corpusPath, entry)) {
        const row = parseCorpusRowLine(
          line,
          `corpus ${entry.chainId} row ${rowsInRange + 1}`,
        );
        if (row.corpusSliceId !== entry.corpusSliceId) {
          throw new Error(
            `corpus range ${entry.chainId} contains slice ${row.corpusSliceId}, expected ${entry.corpusSliceId}`,
          );
        }
        if (row.planShape !== entry.planShape) {
          throw new Error(
            `corpus range ${entry.chainId} contains shape ${row.planShape}, expected ${entry.planShape}`,
          );
        }
        await txHashes.add(row.txHash);
        await inputs.add(row.selectedInputOutref);
        rowsInRange += 1;
        rowCount += 1;
      }
      if (rowsInRange !== entry.rowCount) {
        throw new Error(
          `corpus range ${entry.chainId} expected ${entry.rowCount} rows, read ${rowsInRange}`,
        );
      }
    }
    const uniqueTxHashes = await txHashes.verify();
    const uniqueSelectedInputs = await inputs.verify();
    return {
      rowCount,
      uniqueTxHashes,
      uniqueSelectedInputs,
    };
  } finally {
    await rm(uniquenessDirectory, { recursive: true, force: true });
  }
};

const readIndexedLineAt = async ({ fileHandle, offset, endOffset }) => {
  if (offset >= endOffset) return null;
  const chunks = [];
  let position = offset;
  while (position < endOffset) {
    const buffer = Buffer.allocUnsafe(
      Math.min(POSITIONAL_READ_BYTES, endOffset - position),
    );
    const { bytesRead } = await fileHandle.read(
      buffer,
      0,
      buffer.length,
      position,
    );
    if (bytesRead === 0) break;
    const bytes = buffer.subarray(0, bytesRead);
    const newline = bytes.indexOf(0x0a);
    if (newline >= 0) {
      chunks.push(bytes.subarray(0, newline));
      const lineBytes = Buffer.concat(chunks);
      return {
        line: lineBytes.toString("utf8").trim(),
        lineSha256: sha256Hex(lineBytes),
        nextOffset: position + newline + 1,
      };
    }
    chunks.push(bytes);
    position += bytesRead;
  }
  const lineBytes = Buffer.concat(chunks);
  return {
    line: lineBytes.toString("utf8").trim(),
    lineSha256: sha256Hex(lineBytes),
    nextOffset: endOffset,
  };
};

const makeCursor = ({
  fileHandlePromise,
  entry,
  chainIndex,
  readAheadRows,
}) => {
  let byteOffset = entry.startByteOffset;
  let rowsRead = 0;
  let consumedRows = 0;
  const consumedPrefix = createHash("sha256");
  const cursor = {
    chain: {
      outRefHex: entry.chainId,
      txs: { length: entry.rowCount },
      source: "corpus",
    },
    chainIndex,
    nextIndex: 0,
    stopped: false,
    entry,
    queue: [],
    done: false,
    readAheadRows,
    async fill() {
      if (this.done || this.queue.length >= this.readAheadRows) {
        return;
      }
      while (!this.done && this.queue.length < this.readAheadRows) {
        if (rowsRead >= entry.rowCount) {
          this.done = true;
          break;
        }
        const next = await readIndexedLineAt({
          fileHandle: await fileHandlePromise,
          offset: byteOffset,
          endOffset: entry.endByteOffset,
        });
        if (next === null) {
          throw new Error(
            `corpus range ${entry.chainId} ended after ${rowsRead} of ${entry.rowCount} rows`,
          );
        }
        byteOffset = next.nextOffset;
        if (next.line.length === 0) continue;
        const rowIndex = rowsRead;
        const row = parseCorpusRowLine(
          next.line,
          `corpus ${entry.chainId} row ${rowsRead + 1}`,
        );
        rowsRead += 1;
        this.queue.push({
          txHex: row.canonicalCborHex,
          txIdHex: row.txHash,
          corpusRow: row,
          corpusEvidence: corpusRowEvidence({
            chainIndex,
            chainId: entry.chainId,
            rowIndex,
            row,
            rowSha256: next.lineSha256,
          }),
        });
      }
    },
    async takeNextTx() {
      if (this.stopped || this.nextIndex >= entry.rowCount) {
        return null;
      }
      await this.fill();
      const tx = this.queue.shift();
      if (tx === undefined) {
        this.stopped = true;
        return null;
      }
      const txIndex = this.nextIndex;
      this.nextIndex += 1;
      if (tx.corpusEvidence.rowIndex !== txIndex) {
        throw new Error(
          `corpus ${entry.chainId} dequeue index ${txIndex.toString()} diverged from parsed row ${tx.corpusEvidence.rowIndex.toString()}`,
        );
      }
      consumedPrefix.update(corpusRowEvidenceBytes(tx.corpusEvidence));
      consumedRows += 1;
      if (this.queue.length < Math.max(1, Math.floor(this.readAheadRows / 4))) {
        await this.fill();
      }
      return {
        ...tx,
        chainIndex: this.chainIndex,
        txIndex,
      };
    },
    consumptionSnapshot() {
      return {
        chainIndex,
        chainId: entry.chainId,
        rowCount: consumedRows,
        prefixSha256: consumedPrefix.copy().digest("hex"),
      };
    },
  };
  return cursor;
};

export const openStreamingCorpusReader = ({
  corpusPath,
  indexEntries,
  readAheadRows = 50,
}) => {
  const fileHandlePromise = open(corpusPath, "r");
  const boundedReadAheadRows = Math.min(
    Math.max(1, readAheadRows),
    Math.max(
      1,
      Math.floor(MAX_STREAMING_CORPUS_BUFFERED_ROWS / indexEntries.length),
    ),
  );
  const cursors = indexEntries.map((entry, chainIndex) =>
    makeCursor({
      fileHandlePromise,
      entry,
      chainIndex,
      readAheadRows: boundedReadAheadRows,
    }),
  );
  Object.defineProperties(cursors, {
    close: {
      value: async () => (await fileHandlePromise).close(),
    },
    effectiveReadAheadRows: {
      value: boundedReadAheadRows,
    },
    consumptionSnapshot: {
      value: () => ({
        schemaVersion: CORPUS_PREFIX_EVIDENCE_SCHEMA,
        rowCount: cursors.reduce(
          (sum, cursor) => sum + cursor.consumptionSnapshot().rowCount,
          0,
        ),
        chains: cursors.map((cursor) => cursor.consumptionSnapshot()),
      }),
    },
  });
  return cursors;
};

const sameStat = (left, right) =>
  right.isFile() &&
  !right.isSymbolicLink() &&
  left.dev === right.dev &&
  left.ino === right.ino &&
  left.size === right.size &&
  left.mtimeMs === right.mtimeMs;

/**
 * Re-hash the complete corpus while recomputing only the consumed prefix for
 * each selected chain. Memory remains O(selected chains), and this replaces
 * rather than supplements the offline full-corpus hash pass.
 */
export const scanCorpusPrefixEvidence = async ({
  corpusPath,
  fullIndex,
  selectedEntries,
  consumption,
  expectedCorpusSha256,
}) => {
  const exactConsumption = exactObject(consumption, "corpus prefix evidence", [
    "schemaVersion",
    "rowCount",
    "chains",
  ]);
  if (
    exactConsumption.schemaVersion !== CORPUS_PREFIX_EVIDENCE_SCHEMA ||
    !Array.isArray(exactConsumption.chains) ||
    exactConsumption.chains.length !== selectedEntries.length
  ) {
    throw new Error("corpus prefix evidence is missing or malformed");
  }
  const expectedByRange = new Map();
  let expectedRows = 0;
  for (const [chainIndex, entry] of selectedEntries.entries()) {
    const expected = exactObject(
      exactConsumption.chains[chainIndex],
      `corpus prefix evidence chains[${chainIndex.toString()}]`,
      ["chainIndex", "chainId", "rowCount", "prefixSha256"],
    );
    if (
      expected?.chainIndex !== chainIndex ||
      expected?.chainId !== entry.chainId ||
      !Number.isSafeInteger(expected?.rowCount) ||
      expected.rowCount < 0 ||
      expected.rowCount > entry.rowCount ||
      !SHA256_PATTERN.test(expected?.prefixSha256 ?? "")
    ) {
      throw new Error(
        `corpus prefix evidence is invalid for selected chain ${chainIndex.toString()}`,
      );
    }
    expectedRows += expected.rowCount;
    expectedByRange.set(
      `${entry.startByteOffset.toString()}:${entry.endByteOffset.toString()}`,
      {
        expected,
        digest: createHash("sha256"),
        rowsSeen: 0,
      },
    );
  }
  if (exactConsumption.rowCount !== expectedRows) {
    throw new Error("corpus prefix evidence row count is inconsistent");
  }

  const orderedIndex = [...fullIndex].sort(
    (left, right) => left.startByteOffset - right.startByteOffset,
  );
  const before = lstatSync(corpusPath);
  if (!before.isFile() || before.isSymbolicLink()) {
    throw new Error("corpus must be a regular, non-symlink file");
  }
  const fileHash = createHash("sha256");
  let pending = Buffer.alloc(0);
  let pendingOffset = 0;
  let bytes = 0;
  let indexOrdinal = 0;
  let currentEntryRows = 0;
  const processLine = (lineBytes, lineOffset) => {
    while (
      indexOrdinal < orderedIndex.length &&
      lineOffset >= orderedIndex[indexOrdinal].endByteOffset
    ) {
      indexOrdinal += 1;
      currentEntryRows = 0;
    }
    const entry = orderedIndex[indexOrdinal];
    if (
      entry === undefined ||
      lineOffset < entry.startByteOffset ||
      lineOffset >= entry.endByteOffset
    ) {
      throw new Error(
        `corpus line at byte ${lineOffset.toString()} is outside the bound index`,
      );
    }
    const range = expectedByRange.get(
      `${entry.startByteOffset.toString()}:${entry.endByteOffset.toString()}`,
    );
    if (range !== undefined && currentEntryRows < range.expected.rowCount) {
      const row = parseCorpusRowLine(
        lineBytes.toString("utf8").trim(),
        `offline corpus ${entry.chainId} row ${currentEntryRows + 1}`,
      );
      range.digest.update(
        corpusRowEvidenceBytes(
          corpusRowEvidence({
            chainIndex: range.expected.chainIndex,
            chainId: entry.chainId,
            rowIndex: currentEntryRows,
            row,
            rowSha256: sha256Hex(lineBytes),
          }),
        ),
      );
      range.rowsSeen += 1;
    }
    currentEntryRows += 1;
  };

  for await (const chunk of createReadStream(corpusPath)) {
    fileHash.update(chunk);
    bytes += chunk.byteLength;
    const combined =
      pending.byteLength === 0
        ? chunk
        : Buffer.concat(
            [pending, chunk],
            pending.byteLength + chunk.byteLength,
          );
    let start = 0;
    let newline = combined.indexOf(0x0a, start);
    while (newline >= 0) {
      processLine(combined.subarray(start, newline), pendingOffset + start);
      start = newline + 1;
      newline = combined.indexOf(0x0a, start);
    }
    pending = combined.subarray(start);
    pendingOffset = bytes - pending.byteLength;
  }
  if (pending.byteLength > 0) processLine(pending, pendingOffset);
  const after = lstatSync(corpusPath);
  const corpusSha256 = fileHash.digest("hex");
  if (!sameStat(before, after) || bytes !== after.size) {
    throw new Error("corpus changed during offline prefix verification");
  }
  if (corpusSha256 !== expectedCorpusSha256) {
    throw new Error("corpus does not match its preflight SHA-256");
  }
  for (const range of expectedByRange.values()) {
    if (
      range.rowsSeen !== range.expected.rowCount ||
      range.digest.digest("hex") !== range.expected.prefixSha256
    ) {
      throw new Error(
        `consumed corpus prefix changed for chain ${range.expected.chainId}`,
      );
    }
  }
  return { corpusSha256, bytes, consumedRowCount: expectedRows };
};

export const corpusRowsForEntries = (indexEntries) =>
  indexEntries.reduce((sum, entry) => sum + entry.rowCount, 0);
