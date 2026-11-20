import { createHash } from "node:crypto";
import { createReadStream, lstatSync } from "node:fs";
import { mkdtemp, open, readFile, rm, writeFile } from "node:fs/promises";
import os from "node:os";
import path from "node:path";
import readline from "node:readline";

const TX_HASH_PATTERN = /^[0-9a-f]{64}$/iu;
const SHA256_PATTERN = /^[0-9a-f]{64}$/iu;
const SHAPES = new Set(["fanout", "chain", "mixed"]);
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

export const defaultCorpusIndexPath = (corpusPath) =>
  `${corpusPath}.index.ndjson`;

export const defaultCorpusManifestPath = (corpusPath) =>
  `${corpusPath}.manifest.json`;

export const loadCorpusManifest = async (manifestPath) => {
  const parsed = JSON.parse(await readFile(manifestPath, "utf8"));
  if (typeof parsed !== "object" || parsed === null) {
    throw new Error(`corpus manifest ${manifestPath} must be a JSON object`);
  }
  return parsed;
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
  const [corpusSha256, indexSha256, manifestSha256] = await Promise.all([
    sha256File(corpusPath),
    sha256File(indexPath),
    sha256File(manifestPath),
  ]);
  const expectedCorpusSha256 = requiredManifestArtifactSha256(
    manifest,
    "corpus",
  );
  const expectedIndexSha256 = requiredManifestArtifactSha256(manifest, "index");
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
      const parsed = parseJsonLine(line, `corpus index row ${index + 1}`);
      if (
        typeof parsed !== "object" ||
        parsed === null ||
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
  const parsed = parseJsonLine(line, label);
  if (typeof parsed !== "object" || parsed === null) {
    throw new Error(`${label} must be a JSON object`);
  }
  const row = parsed;
  for (const field of [
    "txHash",
    "canonicalCborHex",
    "canonicalCborSha256",
    "senderWalletId",
    "selectedInputOutref",
    "corpusSliceId",
  ]) {
    if (typeof row[field] !== "string" || row[field].trim().length === 0) {
      throw new Error(`${label}.${field} must be a non-empty string`);
    }
  }
  row.txHash = row.txHash.trim().toLowerCase();
  row.canonicalCborHex = row.canonicalCborHex.trim().toLowerCase();
  row.canonicalCborSha256 = row.canonicalCborSha256.trim().toLowerCase();
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
  if (!Array.isArray(row.outputOutrefs)) {
    throw new Error(`${label}.outputOutrefs must be an array`);
  }
  if (!SHAPES.has(row.planShape)) {
    throw new Error(`${label}.planShape is unsupported`);
  }
  if (
    row.parentTxHash !== null &&
    row.parentTxHash !== undefined &&
    (typeof row.parentTxHash !== "string" ||
      !TX_HASH_PATTERN.test(row.parentTxHash))
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
    outputOutrefs: row.outputOutrefs.map((entry) => String(entry)),
    planShape: row.planShape,
    parentTxHash:
      row.parentTxHash === null || row.parentTxHash === undefined
        ? null
        : row.parentTxHash.toLowerCase(),
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
  if (
    consumption?.schemaVersion !== CORPUS_PREFIX_EVIDENCE_SCHEMA ||
    !Array.isArray(consumption?.chains) ||
    consumption.chains.length !== selectedEntries.length
  ) {
    throw new Error("corpus prefix evidence is missing or malformed");
  }
  const expectedByRange = new Map();
  let expectedRows = 0;
  for (const [chainIndex, entry] of selectedEntries.entries()) {
    const expected = consumption.chains[chainIndex];
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
  if (consumption.rowCount !== expectedRows) {
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
