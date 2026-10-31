import { createHash } from "node:crypto";
import { open } from "node:fs/promises";

import {
  CORPUS_PREFIX_EVIDENCE_SCHEMA,
  corpusRowEvidence,
  corpusRowEvidenceBytes,
  MAX_STREAMING_CORPUS_BUFFERED_ROWS,
} from "./throughput-valid-stress-corpus.corpus-manifest-keys.mjs";
import { readIndexedLineAt } from "./throughput-valid-stress-corpus.make-exact-uniqueness-spool.mjs";
import { parseCorpusRowLine } from "./throughput-valid-stress-corpus.parse-corpus-row-line.mjs";

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

export const sameStat = (left, right) =>
  right.isFile() &&
  !right.isSymbolicLink() &&
  left.dev === right.dev &&
  left.ino === right.ino &&
  left.size === right.size &&
  left.mtimeMs === right.mtimeMs;
