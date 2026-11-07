import { createHash } from "node:crypto";

export const TX_HASH_PATTERN = /^[0-9a-f]{64}$/u;

export const SHA256_PATTERN = /^[0-9a-f]{64}$/u;

export const OUTREF_PATTERN = /^[0-9a-f]{64}#(0|[1-9][0-9]*)$/u;

export const SHAPES = new Set(["fanout", "chain", "mixed"]);

export const CORPUS_MANIFEST_SCHEMA = "midgard-stress-corpus-manifest-v1";

export const CORPUS_MANIFEST_KEYS = [
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

export const CORPUS_ROW_KEYS = [
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

export const CORPUS_INDEX_KEYS = [
  "corpusSliceId",
  "planShape",
  "chainId",
  "startByteOffset",
  "endByteOffset",
  "rowCount",
];

export const DEFAULT_UNIQUENESS_CHUNK_ENTRIES = 50_000;

export const MAX_STREAMING_CORPUS_BUFFERED_ROWS = 8_192;

export const POSITIONAL_READ_BYTES = 64 * 1024;

export const sha256Hex = (bytes) =>
  createHash("sha256").update(bytes).digest("hex");

export const CORPUS_PREFIX_EVIDENCE_SCHEMA =
  "midgard-stress-corpus-prefix-evidence-v1";

export const corpusRowEvidenceBytes = ({
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

export const corpusRowEvidence = ({
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

export const parseJsonLine = (line, label) => {
  try {
    return JSON.parse(line);
  } catch (error) {
    throw new Error(
      `${label} is not valid JSON: ${error instanceof Error ? error.message : String(error)}`,
    );
  }
};

export const exactObject = (value, label, keys) => {
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

export const exactString = (value, label) => {
  if (
    typeof value !== "string" ||
    value.length === 0 ||
    value !== value.trim()
  ) {
    throw new Error(`${label} must be a non-empty exact string`);
  }
  return value;
};

export const safeInteger = (value, label, minimum) => {
  if (!Number.isSafeInteger(value) || value < minimum) {
    throw new Error(`${label} must be a safe integer >= ${minimum.toString()}`);
  }
  return value;
};

export const finiteNumber = (value, label, allowZero = false) => {
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

export const canonicalDecimal = (value, label) => {
  const text = exactString(value, label);
  if (!/^(0|[1-9][0-9]*)$/u.test(text)) {
    throw new Error(`${label} must be a canonical non-negative decimal`);
  }
  return text;
};

export const exactSha256 = (value, label) => {
  const digest = exactString(value, label);
  if (!SHA256_PATTERN.test(digest)) {
    throw new Error(`${label} must be an exact lowercase SHA-256`);
  }
  return digest;
};
