import { mkdir, writeFile } from "node:fs/promises";
import { join } from "node:path";

import {
  invalidRangeViolationReason,
  type NativeTxCompact as NativeTxCompactData,
  type NormalizedTimeRange,
  normalizeNativeTxValidityRange,
} from "@al-ft/midgard-sdk";

import { parseHex, parseSignedInteger, stringifyJson } from "./json-file.js";
import {
  buildTrieView,
  decodeTransactionMaterial,
  type FetchLike,
  fetchNodeBlockTransactions,
  type NodeTransactionPayload,
  type PreparedTxInclusionJson,
  readNodeTransactionPayloadsFile,
  requireProof,
  requireTransactionsRootMatchV1,
  transactionSourceTrieItemV1,
} from "./prepare-double-spend.js";

export type InvalidRangeViolationReason = NonNullable<
  ReturnType<typeof invalidRangeViolationReason>
>;

export type PrepareInvalidRangeCliConfig = {
  readonly midgardNodeUrl: string;
  readonly headerHash: string;
  readonly blockSlot: string | number | bigint;
  readonly expectedTransactionsRoot?: string;
  readonly txId?: string;
  readonly outputDir?: string;
  readonly fetchImpl?: FetchLike;
};

export type PrepareInvalidRangeFromFileConfig = {
  readonly transactionsPath: string;
  readonly headerHash: string;
  readonly blockSlot: string | number | bigint;
  readonly expectedTransactionsRoot?: string;
  readonly txId?: string;
  readonly outputDir?: string;
};

export type PreparedInvalidRangeTx = {
  readonly nodeTxId: string;
  readonly nativeTx: NativeTxCompactData;
  readonly nativeTxCompactCbor: string;
  readonly txInclusion: PreparedTxInclusionJson;
  readonly normalizedValidityRange: NormalizedTimeRange;
  readonly violationReason: InvalidRangeViolationReason;
};

export type PreparedInvalidRangeOutput = {
  readonly headerHash: string;
  readonly txCount: number;
  readonly blockSlot: bigint;
  readonly commitmentEncodings: {
    readonly nativeNode: {
      readonly transactionsRoot: string;
    };
    readonly expectedTransactionsRoot?: {
      readonly value: string;
    };
  };
  readonly tx: PreparedInvalidRangeTx;
  readonly files?: {
    readonly txInclusionPath: string;
    readonly planPath: string;
  };
};

const writePreparedFiles = async ({
  output,
  outputDir,
}: {
  readonly output: PreparedInvalidRangeOutput;
  readonly outputDir: string;
}): Promise<PreparedInvalidRangeOutput["files"]> => {
  await mkdir(outputDir, { recursive: true });
  const paths = {
    txInclusionPath: join(outputDir, "tx-inclusion.json"),
    planPath: join(outputDir, "plan.json"),
  };
  await Promise.all([
    writeFile(paths.txInclusionPath, stringifyJson(output.tx.txInclusion)),
    writeFile(
      paths.planPath,
      stringifyJson({
        headerHash: output.headerHash,
        blockSlot: output.blockSlot,
        txNodeTxId: output.tx.nodeTxId,
        normalizedValidityRange: output.tx.normalizedValidityRange,
        violationReason: output.tx.violationReason,
        commitmentEncodings: output.commitmentEncodings,
      }),
    ),
  ]);
  return paths;
};

export const prepareInvalidRangeFromTransactions = async ({
  headerHash,
  transactions,
  blockSlot,
  expectedTransactionsRoot,
  txId,
  outputDir,
}: {
  readonly headerHash: string;
  readonly transactions: readonly NodeTransactionPayload[];
  readonly blockSlot: string | number | bigint;
  readonly expectedTransactionsRoot?: string;
  readonly txId?: string;
  readonly outputDir?: string;
}): Promise<PreparedInvalidRangeOutput> => {
  const normalizedHeaderHash = parseHex(headerHash, "--header-hash", 28);
  const normalizedBlockSlot = parseSignedInteger(blockSlot, "--block-slot");
  const normalizedExpectedRoot =
    expectedTransactionsRoot === undefined
      ? undefined
      : parseHex(expectedTransactionsRoot, "--expected-transactions-root", 32);
  const normalizedTxId =
    txId === undefined ? undefined : parseHex(txId, "--tx-id", 32);
  const decoded = await Promise.all(
    transactions.map(decodeTransactionMaterial),
  );
  const candidates = decoded
    .map((tx) => {
      const normalizedValidityRange = normalizeNativeTxValidityRange(
        tx.nativeTxCompact.body,
      );
      const violationReason = invalidRangeViolationReason({
        blockSlot: normalizedBlockSlot,
        normalizedRange: normalizedValidityRange,
      });
      return {
        tx,
        normalizedValidityRange,
        violationReason,
      };
    })
    .filter(
      (
        entry,
      ): entry is {
        readonly tx: (typeof decoded)[number];
        readonly normalizedValidityRange: NormalizedTimeRange;
        readonly violationReason: InvalidRangeViolationReason;
      } => entry.violationReason !== null,
    );

  const selected =
    normalizedTxId === undefined
      ? candidates[0]
      : candidates.find((entry) => entry.tx.nodeTxId === normalizedTxId);
  if (selected === undefined) {
    if (normalizedTxId !== undefined) {
      const exists = decoded.some((tx) => tx.nodeTxId === normalizedTxId);
      throw new Error(
        exists
          ? `Requested --tx-id ${normalizedTxId} is valid at --block-slot ${normalizedBlockSlot.toString()}.`
          : `Requested --tx-id ${normalizedTxId} was not found in the block.`,
      );
    }
    throw new Error(
      `No transaction with a validity interval excluding --block-slot ${normalizedBlockSlot.toString()} was found in the selected block.`,
    );
  }

  const nativeTrie = await buildTrieView(
    decoded.map(transactionSourceTrieItemV1),
  );
  const proofCbor = requireProof(
    nativeTrie,
    transactionSourceTrieItemV1(selected.tx).key,
    "invalid-range tx",
  );
  await requireTransactionsRootMatchV1({
    sourceRoot: nativeTrie.root,
    expectedTransactionsRoot: normalizedExpectedRoot,
    count: BigInt(decoded.length),
  });
  const baseOutput: PreparedInvalidRangeOutput = {
    headerHash: normalizedHeaderHash,
    txCount: decoded.length,
    blockSlot: normalizedBlockSlot,
    commitmentEncodings: {
      nativeNode: {
        transactionsRoot: nativeTrie.root,
      },
      ...(normalizedExpectedRoot === undefined
        ? {}
        : {
            expectedTransactionsRoot: {
              value: normalizedExpectedRoot,
            },
          }),
    },
    tx: {
      nodeTxId: selected.tx.nodeTxId,
      nativeTx: selected.tx.nativeTxCompact,
      nativeTxCompactCbor: selected.tx.nativeCompactCbor,
      txInclusion: {
        nativeTxId: selected.tx.nodeTxId,
        nativeTx: selected.tx.nativeTxCompact,
        nativeTxCompactCbor: selected.tx.nativeCompactCbor,
        l2TransactionSourceCbor: selected.tx.l2TransactionSourceCbor,
        transactionsPhasRoot: nativeTrie.root,
        txMembershipProofCbor: proofCbor,
      },
      normalizedValidityRange: selected.normalizedValidityRange,
      violationReason: selected.violationReason,
    },
  };
  if (outputDir === undefined) {
    return baseOutput;
  }
  const files = await writePreparedFiles({
    output: baseOutput,
    outputDir,
  });
  return { ...baseOutput, files };
};

export const prepareInvalidRangeFromNode = async (
  config: PrepareInvalidRangeCliConfig,
): Promise<PreparedInvalidRangeOutput> => {
  const headerHash = parseHex(config.headerHash, "--header-hash", 28);
  const transactions = await fetchNodeBlockTransactions({
    midgardNodeUrl: config.midgardNodeUrl,
    headerHash,
    fetchImpl: config.fetchImpl,
  });
  return await prepareInvalidRangeFromTransactions({
    headerHash,
    transactions,
    blockSlot: config.blockSlot,
    expectedTransactionsRoot: config.expectedTransactionsRoot,
    txId: config.txId,
    outputDir: config.outputDir,
  });
};

export const prepareInvalidRangeFromFile = async (
  config: PrepareInvalidRangeFromFileConfig,
): Promise<PreparedInvalidRangeOutput> => {
  const transactions = await readNodeTransactionPayloadsFile(
    config.transactionsPath,
  );
  return await prepareInvalidRangeFromTransactions({
    headerHash: config.headerHash,
    transactions,
    blockSlot: config.blockSlot,
    expectedTransactionsRoot: config.expectedTransactionsRoot,
    txId: config.txId,
    outputDir: config.outputDir,
  });
};
