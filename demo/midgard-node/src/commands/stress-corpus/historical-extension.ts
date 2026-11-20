import { createHash } from "node:crypto";
import { createReadStream } from "node:fs";
import { open, readFile, stat } from "node:fs/promises";
import { createInterface } from "node:readline";

import {
  computeMidgardNativeTxId,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardNativeTxCanonical,
} from "@al-ft/midgard-core/codec";
import { CML } from "@lucid-evolution/lucid";

import type { CorpusIndexEntry } from "@/commands/stress-corpus/assemble.js";
import {
  type CorpusFundingUtxo,
  nodeUtxoFromCorpusFunding,
} from "@/commands/stress-corpus/build-chain.js";
import { verifyStressCorpus } from "@/commands/stress-corpus/verify.js";
import {
  type OpenLoopCorpusRow,
  parseOpenLoopCorpusLine,
} from "@/commands/stress-open-loop.js";

export const HISTORICAL_EXTENSION_SCHEDULE_ALGORITHM =
  "balanced-prefix-preserving-chain-depth-v1";

export type HistoricalExtensionScheduleEntry = {
  readonly chainId: string;
  readonly baseDepth: number;
  readonly targetDepth: number;
  readonly extensionRows: number;
};

export type HistoricalExtensionSchedule = {
  readonly algorithm: typeof HISTORICAL_EXTENSION_SCHEDULE_ALGORITHM;
  readonly baseChainCount: number;
  readonly baseDepth: number;
  readonly baseRowCount: number;
  readonly targetRowCount: number;
  readonly extensionRowCount: number;
  readonly minimumTargetDepth: number;
  readonly maximumTargetDepth: number;
  readonly depthHistogram: readonly {
    readonly targetDepth: number;
    readonly chainCount: number;
  }[];
  readonly entriesSha256: string;
  readonly entries: readonly HistoricalExtensionScheduleEntry[];
};

export type HistoricalExtensionCorpusVerification = {
  readonly rowCount: number;
  readonly uniqueChainCount: number;
  readonly indexEntryCount: number;
  readonly corpusSha256: string;
  readonly indexSha256: string;
  readonly manifestSha256?: string;
  readonly checkedPrefixBytes: number;
  readonly checkedPrefixRows: number;
  readonly checkedExtensionRows: number;
  readonly checkedContinuationCount: number;
  readonly checkedCanonicalBaseTerminalRows: number;
  readonly checkedCanonicalContinuationRows: number;
};

export type HistoricalRetainedTerminalExpectation = {
  readonly chainId: string;
  readonly address: string;
  readonly corpusSliceId: string;
  readonly baseDepth: number;
};

export type VerifiedHistoricalRetainedTerminal = {
  readonly chainId: string;
  readonly parentTxHash: string;
  readonly fundingUtxo: CorpusFundingUtxo;
  readonly lovelace: bigint;
  readonly outputCborSha256: string;
};

const requirePositiveSafeInteger = (value: number, label: string): void => {
  if (!Number.isSafeInteger(value) || value <= 0) {
    throw new Error(`${label} must be a positive safe integer.`);
  }
};

const scheduleLines = (
  entries: readonly HistoricalExtensionScheduleEntry[],
): readonly string[] =>
  entries.map(
    (entry) =>
      `${entry.chainId}|${entry.baseDepth.toString()}|${entry.targetDepth.toString()}|${entry.extensionRows.toString()}`,
  );

export const historicalExtensionScheduleSha256 = (
  entries: readonly HistoricalExtensionScheduleEntry[],
): string =>
  createHash("sha256").update(scheduleLines(entries).join("\n")).digest("hex");

export const createHistoricalExtensionSchedule = ({
  orderedChainIds,
  baseDepth,
  targetRowCount,
}: {
  readonly orderedChainIds: readonly string[];
  readonly baseDepth: number;
  readonly targetRowCount: number;
}): HistoricalExtensionSchedule => {
  requirePositiveSafeInteger(baseDepth, "baseDepth");
  requirePositiveSafeInteger(targetRowCount, "targetRowCount");
  if (orderedChainIds.length === 0) {
    throw new Error("orderedChainIds must not be empty.");
  }
  if (
    orderedChainIds.some((chainId) => chainId.trim().length === 0) ||
    new Set(orderedChainIds).size !== orderedChainIds.length
  ) {
    throw new Error("orderedChainIds must contain unique non-empty values.");
  }
  const baseRowCount = orderedChainIds.length * baseDepth;
  if (!Number.isSafeInteger(baseRowCount)) {
    throw new Error("base row count exceeds the safe integer range.");
  }
  if (targetRowCount <= baseRowCount) {
    throw new Error(
      `targetRowCount must exceed base row count ${baseRowCount.toString()}.`,
    );
  }
  const extensionRowCount = targetRowCount - baseRowCount;
  const uniformExtensionDepth = Math.floor(
    extensionRowCount / orderedChainIds.length,
  );
  const remainder = extensionRowCount % orderedChainIds.length;
  const entries = orderedChainIds.map(
    (chainId, index): HistoricalExtensionScheduleEntry => {
      const extensionRows = uniformExtensionDepth + (index < remainder ? 1 : 0);
      if (extensionRows <= 0) {
        throw new Error(
          "targetRowCount must add at least one continuation row per chain.",
        );
      }
      return {
        chainId,
        baseDepth,
        targetDepth: baseDepth + extensionRows,
        extensionRows,
      };
    },
  );
  const targetDepths = entries.map((entry) => entry.targetDepth);
  const depthCounts = new Map<number, number>();
  for (const targetDepth of targetDepths) {
    depthCounts.set(targetDepth, (depthCounts.get(targetDepth) ?? 0) + 1);
  }
  return {
    algorithm: HISTORICAL_EXTENSION_SCHEDULE_ALGORITHM,
    baseChainCount: orderedChainIds.length,
    baseDepth,
    baseRowCount,
    targetRowCount,
    extensionRowCount,
    minimumTargetDepth: Math.min(...targetDepths),
    maximumTargetDepth: Math.max(...targetDepths),
    depthHistogram: [...depthCounts.entries()]
      .sort(([left], [right]) => left - right)
      .map(([targetDepth, chainCount]) => ({ targetDepth, chainCount })),
    entriesSha256: historicalExtensionScheduleSha256(entries),
    entries,
  };
};

export const assertHistoricalExtensionSchedule = (
  schedule: HistoricalExtensionSchedule,
): void => {
  if (schedule.algorithm !== HISTORICAL_EXTENSION_SCHEDULE_ALGORITHM) {
    throw new Error(
      `historical extension schedule algorithm must be ${HISTORICAL_EXTENSION_SCHEDULE_ALGORITHM}.`,
    );
  }
  const expected = createHistoricalExtensionSchedule({
    orderedChainIds: schedule.entries.map((entry) => entry.chainId),
    baseDepth: schedule.baseDepth,
    targetRowCount: schedule.targetRowCount,
  });
  if (JSON.stringify(schedule) !== JSON.stringify(expected)) {
    throw new Error(
      "historical extension schedule does not match its deterministic balanced schedule.",
    );
  }
};

export const parseCorpusIndexEntries = async (
  path: string,
): Promise<readonly CorpusIndexEntry[]> =>
  (await readFile(path, "utf8"))
    .split(/\r?\n/u)
    .map((line) => line.trim())
    .filter((line) => line.length > 0)
    .map((line, index) => {
      const parsed = JSON.parse(line) as Partial<CorpusIndexEntry>;
      const startByteOffset = parsed.startByteOffset;
      const endByteOffset = parsed.endByteOffset;
      const rowCount = parsed.rowCount;
      if (
        typeof parsed.corpusSliceId !== "string" ||
        (parsed.planShape !== "fanout" &&
          parsed.planShape !== "chain" &&
          parsed.planShape !== "mixed") ||
        typeof parsed.chainId !== "string" ||
        typeof startByteOffset !== "number" ||
        !Number.isSafeInteger(startByteOffset) ||
        typeof endByteOffset !== "number" ||
        !Number.isSafeInteger(endByteOffset) ||
        typeof rowCount !== "number" ||
        !Number.isSafeInteger(rowCount) ||
        startByteOffset < 0 ||
        endByteOffset <= startByteOffset ||
        rowCount <= 0
      ) {
        throw new Error(
          `index row ${(index + 1).toString()} is not a valid corpus index entry.`,
        );
      }
      return {
        corpusSliceId: parsed.corpusSliceId,
        planShape: parsed.planShape,
        chainId: parsed.chainId,
        startByteOffset,
        endByteOffset,
        rowCount,
      };
    });

const assertByteIdenticalPrefix = async ({
  basePath,
  extendedPath,
}: {
  readonly basePath: string;
  readonly extendedPath: string;
}): Promise<number> => {
  const baseSize = (await stat(basePath)).size;
  const extendedSize = (await stat(extendedPath)).size;
  if (baseSize <= 0 || extendedSize <= baseSize) {
    throw new Error(
      "historical extension must be larger than its non-empty base corpus.",
    );
  }
  const [base, extended] = await Promise.all([
    open(basePath, "r"),
    open(extendedPath, "r"),
  ]);
  try {
    const chunkSize = 1024 * 1024;
    let offset = 0;
    while (offset < baseSize) {
      const length = Math.min(chunkSize, baseSize - offset);
      const baseBuffer = Buffer.allocUnsafe(length);
      const extendedBuffer = Buffer.allocUnsafe(length);
      const [baseRead, extendedRead] = await Promise.all([
        base.read(baseBuffer, 0, length, offset),
        extended.read(extendedBuffer, 0, length, offset),
      ]);
      if (
        baseRead.bytesRead !== length ||
        extendedRead.bytesRead !== length ||
        !baseBuffer.equals(extendedBuffer)
      ) {
        throw new Error(
          `extended corpus prefix differs from the retained base corpus at or before byte ${offset.toString()}.`,
        );
      }
      offset += length;
    }
    return baseSize;
  } finally {
    await Promise.all([base.close(), extended.close()]);
  }
};

const canonicalOutrefBytes = (outref: string): Buffer => {
  const match = /^([0-9a-f]{64})#(0|[1-9][0-9]*)$/u.exec(outref.toLowerCase());
  if (match === null) {
    throw new Error(`invalid declared transaction outref ${outref}.`);
  }
  return Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(match[1]!),
      BigInt(match[2]!),
    ).to_cbor_bytes(),
  );
};

const verifyCanonicalCorpusRow = (
  row: OpenLoopCorpusRow,
  rowNumber: number,
  label: "retained terminal" | "continuation",
): readonly Buffer[] => {
  const bytes = Buffer.from(row.canonicalCborHex, "hex");
  const native = decodeMidgardNativeTxFullFromCanonicalCbor(bytes);
  if (!encodeMidgardNativeTxCanonical(native).equals(bytes)) {
    throw new Error(
      `${label} row ${rowNumber.toString()} transaction CBOR is not canonical.`,
    );
  }
  const computedTxId = computeMidgardNativeTxId(native).toString("hex");
  if (computedTxId !== row.txHash) {
    throw new Error(
      `${label} row ${rowNumber.toString()} transaction ID does not match its canonical native body.`,
    );
  }
  const spendInputs = decodeMidgardNativeByteListPreimage(
    native.body.spendInputsPreimageCbor,
    "native.spend_inputs",
  );
  const selectedInput = canonicalOutrefBytes(row.selectedInputOutref);
  if (
    spendInputs.length !== 1 ||
    !Buffer.from(spendInputs[0]!).equals(selectedInput)
  ) {
    throw new Error(
      `${label} row ${rowNumber.toString()} canonical transaction does not spend its declared selected input.`,
    );
  }
  const outputs = decodeMidgardNativeByteListPreimage(
    native.body.outputsPreimageCbor,
    "native.outputs",
  );
  const expectedOutputOutrefs = outputs.map(
    (_output, outputIndex) => `${row.txHash}#${outputIndex.toString()}`,
  );
  if (
    row.outputOutrefs.length !== expectedOutputOutrefs.length ||
    row.outputOutrefs.some(
      (outref, index) => outref.toLowerCase() !== expectedOutputOutrefs[index],
    )
  ) {
    throw new Error(
      `${label} row ${rowNumber.toString()} declared output outrefs do not match its decoded canonical outputs.`,
    );
  }
  return outputs;
};

const readIndexedTerminalLine = async ({
  file,
  entry,
}: {
  readonly file: Awaited<ReturnType<typeof open>>;
  readonly entry: CorpusIndexEntry;
}): Promise<string> => {
  const chunkSize = 64 * 1024;
  let cursor = entry.endByteOffset;
  let suffix = Buffer.alloc(0);
  while (cursor > entry.startByteOffset) {
    const length = Math.min(chunkSize, cursor - entry.startByteOffset);
    const offset = cursor - length;
    const chunk = Buffer.allocUnsafe(length);
    const { bytesRead } = await file.read(chunk, 0, length, offset);
    if (bytesRead !== length) {
      throw new Error(
        `could only read ${bytesRead.toString()} of ${length.toString()} bytes while locating retained terminal ${entry.chainId}.`,
      );
    }
    suffix = Buffer.concat([chunk, suffix]);
    cursor = offset;
    if (suffix.at(-1) !== 0x0a) {
      throw new Error(
        `retained chain ${entry.chainId} indexed byte range is not newline terminated.`,
      );
    }
    const previousNewline = suffix.lastIndexOf(0x0a, suffix.length - 2);
    if (previousNewline >= 0 || cursor === entry.startByteOffset) {
      const terminalBytes = suffix.subarray(previousNewline + 1, -1);
      if (terminalBytes.length === 0 || terminalBytes.includes(0x0a)) {
        throw new Error(
          `retained chain ${entry.chainId} has an invalid terminal row boundary.`,
        );
      }
      return terminalBytes.toString("utf8").replace(/\r$/u, "");
    }
  }
  throw new Error(`retained chain ${entry.chainId} has no terminal row.`);
};

export const readVerifiedHistoricalRetainedTerminals = async ({
  corpusPath,
  index,
  expectations,
}: {
  readonly corpusPath: string;
  readonly index: readonly CorpusIndexEntry[];
  readonly expectations: readonly HistoricalRetainedTerminalExpectation[];
}): Promise<readonly VerifiedHistoricalRetainedTerminal[]> => {
  if (index.length !== expectations.length || index.length === 0) {
    throw new Error(
      "retained terminal extraction requires one indexed chain per expectation.",
    );
  }
  const corpusSize = (await stat(corpusPath)).size;
  for (const [position, entry] of index.entries()) {
    const previousEnd = position === 0 ? 0 : index[position - 1]!.endByteOffset;
    if (
      entry.startByteOffset !== previousEnd ||
      entry.endByteOffset > corpusSize ||
      entry.rowCount !== expectations[position]!.baseDepth
    ) {
      throw new Error(
        `retained index entry ${(position + 1).toString()} is not an exact contiguous base-chain range.`,
      );
    }
  }
  if (index.at(-1)!.endByteOffset !== corpusSize) {
    throw new Error(
      "retained index does not cover the exact base corpus bytes.",
    );
  }

  const file = await open(corpusPath, "r");
  try {
    const terminals: VerifiedHistoricalRetainedTerminal[] = [];
    for (const [position, entry] of index.entries()) {
      const expected = expectations[position]!;
      if (
        entry.chainId !== expected.chainId ||
        entry.corpusSliceId !== expected.corpusSliceId ||
        entry.planShape !== "chain"
      ) {
        throw new Error(
          `retained index entry ${(position + 1).toString()} does not match expected chain ${expected.chainId}.`,
        );
      }
      const row = parseOpenLoopCorpusLine(
        await readIndexedTerminalLine({ file, entry }),
        (position + 1) * expected.baseDepth,
      );
      if (
        row.senderWalletId !== expected.chainId ||
        row.corpusSliceId !== expected.corpusSliceId ||
        row.planShape !== "chain"
      ) {
        throw new Error(
          `retained terminal for ${expected.chainId} does not match its indexed chain metadata.`,
        );
      }
      const outputs = verifyCanonicalCorpusRow(
        row,
        (position + 1) * expected.baseDepth,
        "retained terminal",
      );
      const outputCbor = outputs[1];
      if (
        outputCbor === undefined ||
        row.outputOutrefs[1] !== `${row.txHash}#1`
      ) {
        throw new Error(
          `retained terminal for ${expected.chainId} has no canonical change output #1.`,
        );
      }
      const fundingUtxo: CorpusFundingUtxo = {
        txHash: row.txHash,
        outputIndex: 1,
        outputCborHex: outputCbor.toString("hex"),
      };
      const decoded = nodeUtxoFromCorpusFunding(fundingUtxo);
      if (decoded.address !== expected.address) {
        throw new Error(
          `retained terminal output #1 for ${expected.chainId} belongs to ${decoded.address}, not ${expected.address}.`,
        );
      }
      const lovelace = decoded.assets.lovelace ?? 0n;
      if (lovelace <= 0n) {
        throw new Error(
          `retained terminal output #1 for ${expected.chainId} has no positive lovelace value.`,
        );
      }
      terminals.push({
        chainId: expected.chainId,
        parentTxHash: row.txHash,
        fundingUtxo,
        lovelace,
        outputCborSha256: createHash("sha256").update(outputCbor).digest("hex"),
      });
    }
    return terminals;
  } finally {
    await file.close();
  }
};

const verifyCanonicalBoundaryRows = async ({
  corpusPath,
  baseRowCount,
  baseDepth,
  expectedBaseTerminalRows,
  expectedExtensionRows,
}: {
  readonly corpusPath: string;
  readonly baseRowCount: number;
  readonly baseDepth: number;
  readonly expectedBaseTerminalRows: number;
  readonly expectedExtensionRows: number;
}): Promise<{
  readonly checkedBaseTerminalRows: number;
  readonly checkedContinuationRows: number;
  readonly checkedContinuationBoundaries: number;
}> => {
  const input = createInterface({
    input: createReadStream(corpusPath),
    crlfDelay: Infinity,
  });
  let rowNumber = 0;
  let checkedBaseTerminalRows = 0;
  let checkedContinuationRows = 0;
  const retainedTerminalByChain = new Map<
    string,
    { readonly txHash: string; readonly changeOutref: string }
  >();
  const continuationBoundaryChains = new Set<string>();
  try {
    for await (const line of input) {
      if (line.trim().length === 0) {
        continue;
      }
      rowNumber += 1;
      if (rowNumber <= baseRowCount) {
        if (rowNumber % baseDepth === 0) {
          const row = parseOpenLoopCorpusLine(line, rowNumber);
          verifyCanonicalCorpusRow(row, rowNumber, "retained terminal");
          if (retainedTerminalByChain.has(row.senderWalletId)) {
            throw new Error(
              `canonical retained-terminal verification repeated chain ${row.senderWalletId}.`,
            );
          }
          retainedTerminalByChain.set(row.senderWalletId, {
            txHash: row.txHash,
            changeOutref: `${row.txHash}#1`,
          });
          checkedBaseTerminalRows += 1;
        }
        continue;
      }
      const row = parseOpenLoopCorpusLine(line, rowNumber);
      verifyCanonicalCorpusRow(row, rowNumber, "continuation");
      if (!continuationBoundaryChains.has(row.senderWalletId)) {
        const retainedTerminal = retainedTerminalByChain.get(
          row.senderWalletId,
        );
        if (
          retainedTerminal === undefined ||
          row.parentTxHash !== retainedTerminal.txHash ||
          row.selectedInputOutref !== retainedTerminal.changeOutref
        ) {
          throw new Error(
            `first continuation for ${row.senderWalletId} does not spend and name its exact retained terminal parent.`,
          );
        }
        continuationBoundaryChains.add(row.senderWalletId);
      }
      checkedContinuationRows += 1;
    }
  } finally {
    input.close();
  }
  if (checkedBaseTerminalRows !== expectedBaseTerminalRows) {
    throw new Error(
      `canonical retained-terminal verification checked ${checkedBaseTerminalRows.toString()} rows, expected ${expectedBaseTerminalRows.toString()}.`,
    );
  }
  if (checkedContinuationRows !== expectedExtensionRows) {
    throw new Error(
      `canonical continuation verification checked ${checkedContinuationRows.toString()} rows, expected ${expectedExtensionRows.toString()}.`,
    );
  }
  if (continuationBoundaryChains.size !== expectedBaseTerminalRows) {
    throw new Error(
      `canonical continuation verification checked ${continuationBoundaryChains.size.toString()} retained-terminal boundaries, expected ${expectedBaseTerminalRows.toString()}.`,
    );
  }
  return {
    checkedBaseTerminalRows,
    checkedContinuationRows,
    checkedContinuationBoundaries: continuationBoundaryChains.size,
  };
};

export const verifyHistoricalExtensionCorpus = async ({
  baseCorpusPath,
  baseIndexPath,
  extendedCorpusPath,
  extendedIndexPath,
  extendedManifestPath,
  schedule,
}: {
  readonly baseCorpusPath: string;
  readonly baseIndexPath: string;
  readonly extendedCorpusPath: string;
  readonly extendedIndexPath: string;
  readonly extendedManifestPath?: string;
  readonly schedule: HistoricalExtensionSchedule;
}): Promise<HistoricalExtensionCorpusVerification> => {
  assertHistoricalExtensionSchedule(schedule);
  const [baseIndex, extendedIndex, checkedPrefixBytes] = await Promise.all([
    parseCorpusIndexEntries(baseIndexPath),
    parseCorpusIndexEntries(extendedIndexPath),
    assertByteIdenticalPrefix({
      basePath: baseCorpusPath,
      extendedPath: extendedCorpusPath,
    }),
  ]);
  if (baseIndex.length !== schedule.baseChainCount) {
    throw new Error(
      `base index has ${baseIndex.length.toString()} entries, expected ${schedule.baseChainCount.toString()}.`,
    );
  }
  for (const [index, base] of baseIndex.entries()) {
    const scheduled = schedule.entries[index]!;
    if (
      base.chainId !== scheduled.chainId ||
      base.rowCount !== schedule.baseDepth
    ) {
      throw new Error(
        `base index entry ${(index + 1).toString()} does not match the historical extension schedule.`,
      );
    }
  }
  if (extendedIndex.length !== schedule.baseChainCount * 2) {
    throw new Error(
      `extended index must contain one retained run and one continuation run per chain; found ${extendedIndex.length.toString()} entries.`,
    );
  }
  for (let index = 0; index < schedule.baseChainCount; index += 1) {
    if (
      JSON.stringify(extendedIndex[index]) !== JSON.stringify(baseIndex[index])
    ) {
      throw new Error(
        `extended index retained entry ${(index + 1).toString()} is not byte-range identical to the base index.`,
      );
    }
    const base = baseIndex[index]!;
    const continuation = extendedIndex[index + schedule.baseChainCount]!;
    const scheduled = schedule.entries[index]!;
    if (
      continuation.chainId !== scheduled.chainId ||
      continuation.corpusSliceId !== base.corpusSliceId ||
      continuation.planShape !== base.planShape ||
      continuation.rowCount !== scheduled.extensionRows
    ) {
      throw new Error(
        `continuation index entry for ${scheduled.chainId} does not match its scheduled depth.`,
      );
    }
  }
  const structurallyVerified = await verifyStressCorpus({
    corpusPath: extendedCorpusPath,
    indexPath: extendedIndexPath,
    ...(extendedManifestPath === undefined
      ? {}
      : { manifestPath: extendedManifestPath }),
  });
  if (structurallyVerified.rowCount !== schedule.targetRowCount) {
    throw new Error(
      `extended corpus has ${structurallyVerified.rowCount.toString()} rows, expected exactly ${schedule.targetRowCount.toString()}.`,
    );
  }
  const canonicalBoundaryRows = await verifyCanonicalBoundaryRows({
    corpusPath: extendedCorpusPath,
    baseRowCount: schedule.baseRowCount,
    baseDepth: schedule.baseDepth,
    expectedBaseTerminalRows: schedule.baseChainCount,
    expectedExtensionRows: schedule.extensionRowCount,
  });
  return {
    rowCount: structurallyVerified.rowCount,
    uniqueChainCount: schedule.baseChainCount,
    indexEntryCount: structurallyVerified.chainCount,
    corpusSha256: structurallyVerified.corpusSha256,
    indexSha256: structurallyVerified.indexSha256,
    ...(structurallyVerified.manifestSha256 === undefined
      ? {}
      : { manifestSha256: structurallyVerified.manifestSha256 }),
    checkedPrefixBytes,
    checkedPrefixRows: schedule.baseRowCount,
    checkedExtensionRows: schedule.extensionRowCount,
    checkedContinuationCount:
      canonicalBoundaryRows.checkedContinuationBoundaries,
    checkedCanonicalBaseTerminalRows:
      canonicalBoundaryRows.checkedBaseTerminalRows,
    checkedCanonicalContinuationRows:
      canonicalBoundaryRows.checkedContinuationRows,
  };
};
