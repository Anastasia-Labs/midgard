import { createReadStream } from "node:fs";
import { mkdtemp, rm, writeFile } from "node:fs/promises";
import os from "node:os";
import path from "node:path";
import readline from "node:readline";

import {
  DEFAULT_UNIQUENESS_CHUNK_ENTRIES,
  POSITIONAL_READ_BYTES,
  sha256Hex,
} from "./throughput-valid-stress-corpus.corpus-manifest-keys.mjs";
import {
  heapPop,
  heapPush,
  parseCorpusRowLine,
  readIndexedRangeLines,
} from "./throughput-valid-stress-corpus.parse-corpus-row-line.mjs";

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

export const readIndexedLineAt = async ({ fileHandle, offset, endOffset }) => {
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
