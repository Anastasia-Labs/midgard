import { createHash } from "node:crypto";
import { createReadStream } from "node:fs";
import { open, readdir, readFile, writeFile } from "node:fs/promises";
import { join, resolve } from "node:path";

import type { Network } from "@lucid-evolution/lucid";

import type { CorpusIndexEntry } from "@/commands/stress-corpus/assemble.js";
import {
  buildCorpusChain,
  type CorpusFeeParams,
  type CorpusFundingUtxo,
} from "@/commands/stress-corpus/build-chain.js";
import {
  computeStressCorpusWalletSetIdentity,
  type StressCorpusWalletSetIdentity,
} from "@/commands/stress-corpus/wallet-set-identity.js";
import {
  type OpenLoopCorpusRow,
  parseOpenLoopCorpusLine,
} from "@/commands/stress-open-loop.js";
import {
  parseStressWalletRecord,
  type StressWalletRecord,
} from "@/commands/stress-wallets.js";

export const DEFAULT_STRESS_CORPUS_REBUILD_SAMPLE_RATE = 0.001;
export const STRESS_CORPUS_REBUILD_SAMPLE_ALGORITHM =
  "sha256-corpus-chain-id-order-v1";
export const STRESS_CORPUS_VERIFICATION_SCHEMA_VERSION =
  "midgard-stress-corpus-verification-v1";

export type VerifyStressCorpusRebuildSampleOptions = {
  readonly walletsDir: string;
  readonly amountLovelace: bigint;
  readonly feeParams: CorpusFeeParams;
  readonly network: Network;
  readonly networkId: bigint;
  readonly maxSubmitTxCborBytes: number;
  readonly sampleRate?: number;
  readonly terminalChangeFloorLovelace?: bigint;
};

export type VerifyStressCorpusRebuildSampleResult = {
  readonly algorithm: typeof STRESS_CORPUS_REBUILD_SAMPLE_ALGORITHM;
  readonly sampleRate: number;
  readonly checkedChainCount: number;
  readonly checkedRowCount: number;
  readonly sampledChainIds: readonly string[];
  readonly livePreflightEntries: readonly {
    readonly walletId: string;
    readonly l2Address: string;
    readonly firstInputOutref: string;
    readonly outputCborSha256: string;
  }[];
};

export type VerifyStressCorpusOptions = {
  readonly corpusPath: string;
  readonly indexPath: string;
  readonly manifestPath?: string;
  readonly rebuildSample?: VerifyStressCorpusRebuildSampleOptions;
  readonly resultOutPath?: string;
};

export type VerifyStressCorpusResult = {
  readonly corpusPath: string;
  readonly indexPath: string;
  readonly manifestPath?: string;
  readonly rowCount: number;
  readonly chainCount: number;
  readonly corpusSha256: string;
  readonly indexSha256: string;
  readonly manifestSha256?: string;
  readonly walletSetIdentity?: StressCorpusWalletSetIdentity;
  readonly rebuildSample?: VerifyStressCorpusRebuildSampleResult;
  readonly verificationArtifact?: {
    readonly path: string;
    readonly sha256: string;
  };
};

type ObservedRun = CorpusIndexEntry;

const sha256File = async (path: string): Promise<string> =>
  new Promise((resolve, reject) => {
    const hash = createHash("sha256");
    const input = createReadStream(path);
    input.on("data", (chunk: string | Buffer) => {
      hash.update(chunk);
    });
    input.on("error", reject);
    input.on("end", () => resolve(hash.digest("hex")));
  });

const parseIndex = async (path: string): Promise<readonly CorpusIndexEntry[]> =>
  (await readFile(path, "utf8"))
    .split(/\r?\n/u)
    .map((line) => line.trim())
    .filter((line) => line.length > 0)
    .map((line, index) => {
      const parsed = JSON.parse(line) as Partial<CorpusIndexEntry>;
      if (
        typeof parsed.corpusSliceId !== "string" ||
        (parsed.planShape !== "fanout" &&
          parsed.planShape !== "chain" &&
          parsed.planShape !== "mixed") ||
        typeof parsed.chainId !== "string" ||
        typeof parsed.startByteOffset !== "number" ||
        typeof parsed.endByteOffset !== "number" ||
        typeof parsed.rowCount !== "number"
      ) {
        throw new Error(
          `index row ${(index + 1).toString()} is not a valid corpus index entry.`,
        );
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

const expectedManifestHash = (
  manifest: unknown,
  file: "corpus" | "index",
): string | undefined => {
  if (typeof manifest !== "object" || manifest === null) {
    throw new Error("manifest must be an object.");
  }
  const files = (manifest as { readonly files?: unknown }).files;
  if (typeof files !== "object" || files === null) {
    return undefined;
  }
  const entry = (files as Record<string, unknown>)[file];
  if (typeof entry !== "object" || entry === null) {
    return undefined;
  }
  const sha256 = (entry as { readonly sha256?: unknown }).sha256;
  if (sha256 !== undefined && typeof sha256 !== "string") {
    throw new Error(`manifest files.${file}.sha256 must be a string.`);
  }
  return sha256;
};

const closeObservedRun = (
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

const compareIndexEntries = (
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

const readWalletRecordsById = async (
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

const fundingUtxoForRecord = (
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

const normalizedSampleRate = (sampleRate: number | undefined): number => {
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

const selectRebuildSample = (
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

const readCorpusRangeLines = async (
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

const verifyRebuildSample = async ({
  corpusPath,
  index,
  corpusSha256,
  options,
  recordsById,
}: {
  readonly corpusPath: string;
  readonly index: readonly CorpusIndexEntry[];
  readonly corpusSha256: string;
  readonly options: VerifyStressCorpusRebuildSampleOptions;
  readonly recordsById: ReadonlyMap<string, StressWalletRecord>;
}): Promise<VerifyStressCorpusRebuildSampleResult> => {
  const sampleRate = normalizedSampleRate(options.sampleRate);
  const sample = selectRebuildSample(index, corpusSha256, sampleRate);
  let checkedRowCount = 0;
  const livePreflightEntries: Array<
    VerifyStressCorpusRebuildSampleResult["livePreflightEntries"][number]
  > = [];

  for (const entry of sample) {
    const record = recordsById.get(entry.chainId);
    if (record === undefined) {
      throw new Error(
        `sampled chain ${entry.chainId} has no matching stress wallet record in ${options.walletsDir}.`,
      );
    }
    const corpusLines = await readCorpusRangeLines(corpusPath, entry);
    const firstRow = parseOpenLoopCorpusLine(corpusLines[0]!, 1);
    const firstFunding = record.latestFunding?.fundingUtxos?.[0];
    if (firstFunding === undefined) {
      throw new Error(
        `sampled chain ${entry.chainId} has no first wallet funding entry.`,
      );
    }
    if (firstRow.selectedInputOutref !== firstFunding.outref.toLowerCase()) {
      throw new Error(
        `sampled chain ${entry.chainId} first input ${firstRow.selectedInputOutref} does not match wallet funding ${firstFunding.outref}.`,
      );
    }
    livePreflightEntries.push({
      walletId: entry.chainId,
      l2Address: record.l2Address,
      firstInputOutref: firstRow.selectedInputOutref,
      outputCborSha256: createHash("sha256")
        .update(Buffer.from(firstFunding.outputCbor, "hex"))
        .digest("hex"),
    });
    const rebuilt = await buildCorpusChain({
      seedPhrase: record.seedPhrase,
      walletId: record.walletId,
      fundingUtxo: fundingUtxoForRecord(record),
      depth: entry.rowCount,
      amountLovelace: options.amountLovelace,
      feeParams: options.feeParams,
      network: options.network,
      networkId: options.networkId,
      maxSubmitTxCborBytes: options.maxSubmitTxCborBytes,
      corpusSliceId: entry.corpusSliceId,
      planShape: entry.planShape,
      terminalChangeFloorLovelace: options.terminalChangeFloorLovelace,
    });
    if (rebuilt.rows.length !== corpusLines.length) {
      throw new Error(
        `sampled chain ${entry.chainId} rebuilt ${rebuilt.rows.length.toString()} rows, expected ${corpusLines.length.toString()}.`,
      );
    }
    for (let rowOffset = 0; rowOffset < corpusLines.length; rowOffset += 1) {
      const corpusLine = corpusLines[rowOffset]!;
      parseOpenLoopCorpusLine(corpusLine, rowOffset + 1);
      const rebuiltLine = JSON.stringify(rebuilt.rows[rowOffset]!);
      if (corpusLine !== rebuiltLine) {
        throw new Error(
          `rebuild sample mismatch for ${entry.chainId} row ${(rowOffset + 1).toString()}: corpus row is not byte-identical to a fresh build.`,
        );
      }
    }
    checkedRowCount += corpusLines.length;
  }

  return {
    algorithm: STRESS_CORPUS_REBUILD_SAMPLE_ALGORITHM,
    sampleRate,
    checkedChainCount: sample.length,
    checkedRowCount,
    sampledChainIds: sample.map((entry) => entry.chainId),
    livePreflightEntries,
  };
};

const writeVerificationArtifact = async (
  path: string,
  result: Omit<VerifyStressCorpusResult, "verificationArtifact">,
): Promise<{ readonly path: string; readonly sha256: string }> => {
  const absolutePath = resolve(path);
  const document = {
    schemaVersion: STRESS_CORPUS_VERIFICATION_SCHEMA_VERSION,
    verifiedAtIso: new Date().toISOString(),
    corpus: {
      path: resolve(result.corpusPath),
      indexPath: resolve(result.indexPath),
      ...(result.manifestPath === undefined
        ? {}
        : { manifestPath: resolve(result.manifestPath) }),
      corpusSha256: result.corpusSha256,
      indexSha256: result.indexSha256,
      ...(result.manifestSha256 === undefined
        ? {}
        : { manifestSha256: result.manifestSha256 }),
    },
    rowCount: result.rowCount,
    chainCount: result.chainCount,
    walletSetIdentity: result.walletSetIdentity,
    rebuildSample: result.rebuildSample,
  };
  await writeFile(
    absolutePath,
    `${JSON.stringify(document, null, 2)}\n`,
    "utf8",
  );
  return { path: absolutePath, sha256: await sha256File(absolutePath) };
};

export const verifyStressCorpus = async (
  options: VerifyStressCorpusOptions,
): Promise<VerifyStressCorpusResult> => {
  const expectedIndex = await parseIndex(options.indexPath);
  const seenInputs = new Set<string>();
  const lastByChain = new Map<
    string,
    { readonly txHash: string; readonly changeOutref: string }
  >();
  const observedRuns: ObservedRun[] = [];
  const corpusHash = createHash("sha256");
  let carry = Buffer.alloc(0);
  let byteOffset = 0;
  let rowIndex = 0;
  let currentRun:
    | {
        readonly corpusSliceId: string;
        readonly planShape: OpenLoopCorpusRow["planShape"];
        readonly chainId: string;
        readonly startByteOffset: number;
        rowCount: number;
      }
    | undefined;

  const processLine = (lineBytes: Buffer, rawLength: number): void => {
    const startByteOffset = byteOffset;
    byteOffset += rawLength;
    const line = lineBytes.toString("utf8").replace(/\r$/u, "");
    if (line.trim().length === 0) {
      return;
    }
    rowIndex += 1;
    const row = parseOpenLoopCorpusLine(line, rowIndex);
    const existingInput = seenInputs.has(row.selectedInputOutref);
    if (existingInput) {
      throw new Error(
        `duplicate selected input ${row.selectedInputOutref} at row ${rowIndex.toString()}.`,
      );
    }
    seenInputs.add(row.selectedInputOutref);
    const previous = lastByChain.get(row.senderWalletId);
    if (previous === undefined) {
      if (row.parentTxHash !== null) {
        throw new Error(
          `row ${rowIndex.toString()} starts chain ${row.senderWalletId} with non-null parentTxHash.`,
        );
      }
    } else {
      if (row.parentTxHash !== previous.txHash) {
        throw new Error(
          `row ${rowIndex.toString()} parentTxHash ${String(row.parentTxHash)} does not match previous chain tx ${previous.txHash}.`,
        );
      }
      if (row.selectedInputOutref !== previous.changeOutref) {
        throw new Error(
          `row ${rowIndex.toString()} selected input ${row.selectedInputOutref} does not spend previous change ${previous.changeOutref}.`,
        );
      }
    }
    if (row.outputOutrefs[1] === undefined) {
      throw new Error(
        `row ${rowIndex.toString()} must include change output outref at index 1.`,
      );
    }
    lastByChain.set(row.senderWalletId, {
      txHash: row.txHash,
      changeOutref: row.outputOutrefs[1],
    });
    if (
      currentRun === undefined ||
      currentRun.chainId !== row.senderWalletId ||
      currentRun.corpusSliceId !== row.corpusSliceId ||
      currentRun.planShape !== row.planShape
    ) {
      closeObservedRun(observedRuns, currentRun, startByteOffset);
      currentRun = {
        corpusSliceId: row.corpusSliceId,
        planShape: row.planShape,
        chainId: row.senderWalletId,
        startByteOffset,
        rowCount: 0,
      };
    }
    currentRun.rowCount += 1;
  };

  for await (const chunk of createReadStream(options.corpusPath)) {
    const buffer = Buffer.isBuffer(chunk) ? chunk : Buffer.from(chunk);
    corpusHash.update(buffer);
    let pending = Buffer.concat([carry, buffer]);
    let newlineIndex = pending.indexOf(0x0a);
    while (newlineIndex >= 0) {
      processLine(pending.subarray(0, newlineIndex), newlineIndex + 1);
      pending = pending.subarray(newlineIndex + 1);
      newlineIndex = pending.indexOf(0x0a);
    }
    carry = pending;
  }
  if (carry.length > 0) {
    processLine(carry, carry.length);
  }
  closeObservedRun(observedRuns, currentRun, byteOffset);
  compareIndexEntries(expectedIndex, observedRuns);

  const corpusSha256 = corpusHash.digest("hex");
  const indexSha256 = await sha256File(options.indexPath);
  const manifestSha256 =
    options.manifestPath === undefined
      ? undefined
      : await sha256File(options.manifestPath);
  let manifest: unknown;
  if (options.manifestPath !== undefined) {
    manifest = JSON.parse(
      await readFile(options.manifestPath, "utf8"),
    ) as unknown;
    const expectedCorpusSha = expectedManifestHash(manifest, "corpus");
    const expectedIndexSha = expectedManifestHash(manifest, "index");
    if (expectedCorpusSha !== undefined && expectedCorpusSha !== corpusSha256) {
      throw new Error(
        `manifest corpus sha256 ${expectedCorpusSha} does not match ${corpusSha256}.`,
      );
    }
    if (expectedIndexSha !== undefined && expectedIndexSha !== indexSha256) {
      throw new Error(
        `manifest index sha256 ${expectedIndexSha} does not match ${indexSha256}.`,
      );
    }
  }
  const expectedWalletIds = new Set(
    expectedIndex.map((entry) => entry.chainId),
  );
  const walletRecords =
    options.rebuildSample === undefined
      ? undefined
      : await readWalletRecordsById(options.rebuildSample.walletsDir);
  const walletSetIdentity =
    walletRecords === undefined
      ? undefined
      : computeStressCorpusWalletSetIdentity({
          records: walletRecords.records,
          expectedWalletCount: expectedWalletIds.size,
          expectedWalletIds,
        });
  if (
    walletSetIdentity !== undefined &&
    typeof manifest === "object" &&
    manifest !== null &&
    "walletSetIdentity" in manifest &&
    JSON.stringify(manifest.walletSetIdentity) !==
      JSON.stringify(walletSetIdentity)
  ) {
    throw new Error(
      "manifest walletSetIdentity does not match the complete rebuild wallet set.",
    );
  }
  const rebuildSample =
    options.rebuildSample === undefined
      ? undefined
      : await verifyRebuildSample({
          corpusPath: options.corpusPath,
          index: expectedIndex,
          corpusSha256,
          options: options.rebuildSample,
          recordsById: walletRecords!.recordsById,
        });
  const result: Omit<VerifyStressCorpusResult, "verificationArtifact"> = {
    corpusPath: options.corpusPath,
    indexPath: options.indexPath,
    ...(options.manifestPath === undefined
      ? {}
      : { manifestPath: options.manifestPath }),
    rowCount: rowIndex,
    chainCount: observedRuns.length,
    corpusSha256,
    indexSha256,
    ...(manifestSha256 === undefined ? {} : { manifestSha256 }),
    ...(walletSetIdentity === undefined ? {} : { walletSetIdentity }),
    ...(rebuildSample === undefined ? {} : { rebuildSample }),
  };
  if (options.resultOutPath === undefined) {
    return result;
  }
  return {
    ...result,
    verificationArtifact: await writeVerificationArtifact(
      options.resultOutPath,
      result,
    ),
  };
};
