import { createHash } from "node:crypto";
import { createReadStream, lstatSync } from "node:fs";

import {
  CORPUS_PREFIX_EVIDENCE_SCHEMA,
  corpusRowEvidence,
  corpusRowEvidenceBytes,
  exactObject,
  SHA256_PATTERN,
  sha256Hex,
} from "./throughput-valid-stress-corpus.corpus-manifest-keys.mjs";
import { sameStat } from "./throughput-valid-stress-corpus.make-cursor.mjs";
import { parseCorpusRowLine } from "./throughput-valid-stress-corpus.parse-corpus-row-line.mjs";

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
