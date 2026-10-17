import { createHash } from "node:crypto";
import { createReadStream } from "node:fs";
import { open, readdir, readFile } from "node:fs/promises";
import { join } from "node:path";

import { type OpenLoopCorpusRow } from "../stress-open-loop.js";
import {
  parseStressWalletRecord,
  type StressWalletRecord,
} from "../stress-wallets/index.js";
import type { CorpusIndexEntry } from "./assemble.js";
import { type CorpusFundingUtxo } from "./build-chain.js";
import {
  DEFAULT_STRESS_CORPUS_REBUILD_SAMPLE_RATE,
  exactObject,
  type ObservedRun,
} from "./verify.parse-stress-corpus-wallet-set-identity.js";

export const sha256File = async (path: string): Promise<string> =>
  new Promise((resolve, reject) => {
    const hash = createHash("sha256");
    const input = createReadStream(path);
    input.on("data", (chunk: string | Buffer) => {
      hash.update(chunk);
    });
    input.on("error", reject);
    input.on("end", () => resolve(hash.digest("hex")));
  });

export const parseStressCorpusIndexLine = (
  line: string,
  index: number,
): CorpusIndexEntry => {
  const parsed = exactObject(
    JSON.parse(line) as unknown,
    `index row ${index.toString()}`,
    [
      "corpusSliceId",
      "planShape",
      "chainId",
      "startByteOffset",
      "endByteOffset",
      "rowCount",
    ],
  );
  if (
    typeof parsed.corpusSliceId !== "string" ||
    (parsed.planShape !== "fanout" &&
      parsed.planShape !== "chain" &&
      parsed.planShape !== "mixed") ||
    typeof parsed.chainId !== "string" ||
    !Number.isSafeInteger(parsed.startByteOffset) ||
    !Number.isSafeInteger(parsed.endByteOffset) ||
    !Number.isSafeInteger(parsed.rowCount) ||
    (parsed.startByteOffset as number) < 0 ||
    (parsed.endByteOffset as number) <= (parsed.startByteOffset as number) ||
    (parsed.rowCount as number) <= 0
  ) {
    throw new Error(
      `index row ${index.toString()} is not a valid corpus index entry.`,
    );
  }
  return {
    corpusSliceId: parsed.corpusSliceId,
    planShape: parsed.planShape,
    chainId: parsed.chainId,
    startByteOffset: parsed.startByteOffset as number,
    endByteOffset: parsed.endByteOffset as number,
    rowCount: parsed.rowCount as number,
  };
};

export const parseIndex = async (
  path: string,
): Promise<readonly CorpusIndexEntry[]> =>
  (await readFile(path, "utf8"))
    .split(/\r?\n/u)
    .map((line) => line.trim())
    .filter((line) => line.length > 0)
    .map((line, index) => parseStressCorpusIndexLine(line, index + 1));

export const closeObservedRun = (
  runs: ObservedRun[],
  currentRun:
    | {
        readonly corpusSliceId: string;
        readonly planShape: OpenLoopCorpusRow["planShape"];
        readonly chainId: string;
        readonly startByteOffset: number;
        rowCount: number;
      }
    | undefined,
  endByteOffset: number,
): void => {
  if (currentRun === undefined) {
    return;
  }
  runs.push({
    corpusSliceId: currentRun.corpusSliceId,
    planShape: currentRun.planShape,
    chainId: currentRun.chainId,
    startByteOffset: currentRun.startByteOffset,
    endByteOffset,
    rowCount: currentRun.rowCount,
  });
};

export const compareIndexEntries = (
  expected: readonly CorpusIndexEntry[],
  observed: readonly CorpusIndexEntry[],
): void => {
  if (expected.length !== observed.length) {
    throw new Error(
      `index entry count ${expected.length.toString()} does not match observed chain runs ${observed.length.toString()}.`,
    );
  }
  for (let i = 0; i < expected.length; i += 1) {
    const lhs = expected[i]!;
    const rhs = observed[i]!;
    if (JSON.stringify(lhs) !== JSON.stringify(rhs)) {
      throw new Error(
        `index entry ${(i + 1).toString()} does not match observed corpus run: expected ${JSON.stringify(lhs)}, observed ${JSON.stringify(rhs)}.`,
      );
    }
  }
};

const walletFilePattern = /^wallet-\d{4}\.json$/u;

export const readWalletRecordsById = async (
  walletsDir: string,
): Promise<{
  readonly records: readonly StressWalletRecord[];
  readonly recordsById: ReadonlyMap<string, StressWalletRecord>;
}> => {
  const files = (await readdir(walletsDir))
    .filter((file) => walletFilePattern.test(file))
    .sort();
  const records = await Promise.all(
    files.map(async (file) => {
      return parseStressWalletRecord(
        JSON.parse(await readFile(join(walletsDir, file), "utf8")) as unknown,
      );
    }),
  );
  return {
    records,
    recordsById: new Map(records.map((record) => [record.walletId, record])),
  };
};

export const fundingUtxoForRecord = (
  record: StressWalletRecord,
): CorpusFundingUtxo => {
  const funding = record.latestFunding?.fundingUtxos?.[0];
  if (funding === undefined) {
    throw new Error(
      `Stress wallet ${record.walletId} has no latestFunding.fundingUtxos[0]; cannot run corpus rebuild sample.`,
    );
  }
  const [txHash, indexRaw, extra] = funding.outref.split("#");
  if (
    txHash === undefined ||
    indexRaw === undefined ||
    extra !== undefined ||
    !/^[0-9a-f]{64}$/iu.test(txHash) ||
    !/^(0|[1-9][0-9]*)$/u.test(indexRaw)
  ) {
    throw new Error(
      `Stress wallet ${record.walletId} funding outref ${funding.outref} must use <64hex>#<index>.`,
    );
  }
  return {
    txHash: txHash.toLowerCase(),
    outputIndex: Number(indexRaw),
    outputCborHex: funding.outputCbor,
  };
};

export const normalizedSampleRate = (
  sampleRate: number | undefined,
): number => {
  const parsed = sampleRate ?? DEFAULT_STRESS_CORPUS_REBUILD_SAMPLE_RATE;
  if (!Number.isFinite(parsed) || parsed <= 0 || parsed > 1) {
    throw new Error("rebuild sample rate must be > 0 and <= 1.");
  }
  return parsed;
};

const sampleKey = (corpusSha256: string, entry: CorpusIndexEntry): string =>
  createHash("sha256")
    .update(corpusSha256)
    .update("\0")
    .update(entry.chainId)
    .update("\0")
    .update(String(entry.startByteOffset))
    .digest("hex");

export const selectRebuildSample = (
  index: readonly CorpusIndexEntry[],
  corpusSha256: string,
  sampleRate: number,
): readonly CorpusIndexEntry[] => {
  if (index.length === 0) {
    return [];
  }
  const sampleCount = Math.max(1, Math.ceil(index.length * sampleRate));
  return [...index]
    .sort((left, right) =>
      sampleKey(corpusSha256, left).localeCompare(
        sampleKey(corpusSha256, right),
      ),
    )
    .slice(0, sampleCount);
};

export const readCorpusRangeLines = async (
  corpusPath: string,
  entry: CorpusIndexEntry,
): Promise<readonly string[]> => {
  const byteLength = entry.endByteOffset - entry.startByteOffset;
  if (!Number.isSafeInteger(byteLength) || byteLength <= 0) {
    throw new Error(
      `index entry for ${entry.chainId} has invalid byte range ${entry.startByteOffset.toString()}..${entry.endByteOffset.toString()}.`,
    );
  }
  const file = await open(corpusPath, "r");
  try {
    const buffer = Buffer.alloc(byteLength);
    const { bytesRead } = await file.read(
      buffer,
      0,
      byteLength,
      entry.startByteOffset,
    );
    if (bytesRead !== byteLength) {
      throw new Error(
        `could only read ${bytesRead.toString()} of ${byteLength.toString()} bytes for sampled chain ${entry.chainId}.`,
      );
    }
    const lines = buffer
      .toString("utf8")
      .split("\n")
      .map((line) => line.replace(/\r$/u, ""))
      .filter((line) => line.length > 0);
    if (lines.length !== entry.rowCount) {
      throw new Error(
        `sampled chain ${entry.chainId} index rowCount ${entry.rowCount.toString()} does not match ${lines.length.toString()} corpus rows.`,
      );
    }
    return lines;
  } finally {
    await file.close();
  }
};
