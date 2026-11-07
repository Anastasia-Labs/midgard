import { createHash } from "node:crypto";
import { createReadStream } from "node:fs";
import { readFile } from "node:fs/promises";
import path from "node:path";
import readline from "node:readline";

import {
  computeMidgardNativeTxId,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
} from "@al-ft/midgard-core/codec";

import {
  CORPUS_INDEX_KEYS,
  CORPUS_ROW_KEYS,
  exactObject,
  OUTREF_PATTERN,
  parseJsonLine,
  SHA256_PATTERN,
  sha256Hex,
  SHAPES,
  TX_HASH_PATTERN,
} from "./throughput-valid-stress-corpus.corpus-manifest-keys.mjs";
import {
  loadCorpusManifest,
  parseCorpusManifest,
} from "./throughput-valid-stress-corpus.parse-corpus-manifest.mjs";

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
    const nativeTx = decodeMidgardNativeTxFullFromCanonicalCbor(cborBytes);
    computedTxHash = computeMidgardNativeTxId(nativeTx).toString("hex");
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

export async function* readIndexedRangeLines(corpusPath, entry) {
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

export const heapPush = (heap, value) => {
  heap.push(value);
  let index = heap.length - 1;
  while (index > 0) {
    const parent = Math.floor((index - 1) / 2);
    if (heap[parent].value <= heap[index].value) break;
    [heap[parent], heap[index]] = [heap[index], heap[parent]];
    index = parent;
  }
};

export const heapPop = (heap) => {
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
