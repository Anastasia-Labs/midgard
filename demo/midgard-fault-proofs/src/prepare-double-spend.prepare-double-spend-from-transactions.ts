import { mkdir, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { commitCountedRootProgram, ROOT_DOMAINS } from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import { parseHex, stringifyJson } from "./json-file.js";
import {
  type DecodedTransactionMaterial,
  fetchNodeBlockTransactions,
  type NodeTransactionPayload,
  type PreparedDoubleSpendOutput,
  type PreparedDoubleSpendTx,
  type PrepareDoubleSpendCliConfig,
  type PrepareDoubleSpendFromFileConfig,
  type PrepareSampleDoubleSpendConfig,
  readNodeTransactionPayloadsFile,
} from "./prepare-double-spend.encode-l2-transaction-source-value.js";
import {
  buildTrieView,
  decodeTransactionMaterial,
  makeSampleDoubleSpendTransactions,
  requireProof,
  resolveDoubleSpendPair,
  transactionSourceTrieItem,
} from "./prepare-double-spend.resolve-double-spend-pair.js";

const prepareTx = ({
  tx,
  doubleSpentInputIndex,
  proofCbor,
  transactionsPhasRoot,
}: {
  readonly tx: DecodedTransactionMaterial;
  readonly doubleSpentInputIndex: number;
  readonly proofCbor: string;
  readonly transactionsPhasRoot: string;
}): PreparedDoubleSpendTx => ({
  nodeTxId: tx.nodeTxId,
  nativeTx: tx.nativeTxCompact,
  nativeTxCompactCbor: tx.nativeCompactCbor,
  txInclusion: {
    nativeTxId: tx.nodeTxId,
    nativeTx: tx.nativeTxCompact,
    nativeTxCompactCbor: tx.nativeCompactCbor,
    l2TransactionSourceCbor: tx.l2TransactionSourceCbor,
    transactionsPhasRoot,
    txMembershipProofCbor: proofCbor,
  },
  inputs: tx.inputs,
  spendInputCbors: tx.spendInputCbors,
  doubleSpentInputIndex,
});

export const requireTransactionsRootMatch = async ({
  sourceRoot,
  expectedTransactionsRoot,
  count,
}: {
  readonly sourceRoot: string;
  readonly expectedTransactionsRoot?: string;
  readonly count: bigint;
}): Promise<void> => {
  if (expectedTransactionsRoot === undefined) {
    return;
  }
  const committedRoot = await Effect.runPromise(
    commitCountedRootProgram({
      domain: ROOT_DOMAINS.transactionsV1,
      phasRoot: sourceRoot,
      count,
    }),
  );
  if (expectedTransactionsRoot !== committedRoot) {
    throw new Error(
      `Expected V1 transactions root ${expectedTransactionsRoot} does not match the counted L2TransactionSourceV1 root ${committedRoot} derived from raw PHAS root ${sourceRoot} at count ${count.toString()}.`,
    );
  }
};

const writePreparedFiles = async ({
  output,
  outputDir,
}: {
  readonly output: PreparedDoubleSpendOutput;
  readonly outputDir: string;
}): Promise<PreparedDoubleSpendOutput["files"]> => {
  await mkdir(outputDir, { recursive: true });
  const paths = {
    tx1InclusionPath: join(outputDir, "tx1-inclusion.json"),
    tx2InclusionPath: join(outputDir, "tx2-inclusion.json"),
    tx1InputsPath: join(outputDir, "tx1-inputs.json"),
    tx2InputsPath: join(outputDir, "tx2-inputs.json"),
    planPath: join(outputDir, "plan.json"),
  };
  await Promise.all([
    writeFile(paths.tx1InclusionPath, stringifyJson(output.tx1.txInclusion)),
    writeFile(paths.tx2InclusionPath, stringifyJson(output.tx2.txInclusion)),
    writeFile(paths.tx1InputsPath, stringifyJson(output.tx1.spendInputCbors)),
    writeFile(paths.tx2InputsPath, stringifyJson(output.tx2.spendInputCbors)),
    writeFile(
      paths.planPath,
      stringifyJson({
        headerHash: output.headerHash,
        doubleSpentInput: output.doubleSpentInput,
        tx1NodeTxId: output.tx1.nodeTxId,
        tx2NodeTxId: output.tx2.nodeTxId,
        tx1DoubleSpentInputIndex: output.tx1.doubleSpentInputIndex,
        tx2DoubleSpentInputIndex: output.tx2.doubleSpentInputIndex,
        commitmentEncodings: output.commitmentEncodings,
      }),
    ),
  ]);
  return paths;
};

const writeBlockTransactionsFile = async ({
  outputDir,
  transactions,
}: {
  readonly outputDir: string;
  readonly transactions: readonly NodeTransactionPayload[];
}): Promise<string> => {
  await mkdir(outputDir, { recursive: true });
  const path = join(outputDir, "block-transactions.json");
  await writeFile(path, stringifyJson(transactions));
  return path;
};

export const prepareDoubleSpendFromTransactions = async ({
  headerHash,
  transactions,
  expectedTransactionsRoot,
  tx1Id,
  tx2Id,
  outputDir,
}: {
  readonly headerHash: string;
  readonly transactions: readonly NodeTransactionPayload[];
  readonly expectedTransactionsRoot?: string;
  readonly tx1Id?: string;
  readonly tx2Id?: string;
  readonly outputDir?: string;
}): Promise<PreparedDoubleSpendOutput> => {
  const normalizedHeaderHash = parseHex(headerHash, "--header-hash", 28);
  const normalizedExpectedRoot =
    expectedTransactionsRoot === undefined
      ? undefined
      : parseHex(expectedTransactionsRoot, "--expected-transactions-root", 32);
  const decoded = await Promise.all(
    transactions.map(decodeTransactionMaterial),
  );
  const pair = resolveDoubleSpendPair({ transactions: decoded, tx1Id, tx2Id });
  const sourceTrie = await buildTrieView(
    decoded.map(transactionSourceTrieItem),
  );
  const tx1Proof = requireProof(
    sourceTrie,
    transactionSourceTrieItem(pair.tx1).key,
    "tx1",
  );
  const tx2Proof = requireProof(
    sourceTrie,
    transactionSourceTrieItem(pair.tx2).key,
    "tx2",
  );
  await requireTransactionsRootMatch({
    sourceRoot: sourceTrie.root,
    expectedTransactionsRoot: normalizedExpectedRoot,
    count: BigInt(decoded.length),
  });
  const baseOutput: PreparedDoubleSpendOutput = {
    headerHash: normalizedHeaderHash,
    txCount: decoded.length,
    doubleSpentInput: pair.doubleSpentInput,
    commitmentEncodings: {
      nativeNode: {
        transactionsRoot: sourceTrie.root,
      },
      ...(normalizedExpectedRoot === undefined
        ? {}
        : {
            expectedTransactionsRoot: {
              value: normalizedExpectedRoot,
            },
          }),
    },
    tx1: prepareTx({
      tx: pair.tx1,
      doubleSpentInputIndex: pair.tx1DoubleSpentInputIndex,
      proofCbor: tx1Proof,
      transactionsPhasRoot: sourceTrie.root,
    }),
    tx2: prepareTx({
      tx: pair.tx2,
      doubleSpentInputIndex: pair.tx2DoubleSpentInputIndex,
      proofCbor: tx2Proof,
      transactionsPhasRoot: sourceTrie.root,
    }),
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

export const prepareDoubleSpendFromNode = async (
  config: PrepareDoubleSpendCliConfig,
): Promise<PreparedDoubleSpendOutput> => {
  const headerHash = parseHex(config.headerHash, "--header-hash", 28);
  const transactions = await fetchNodeBlockTransactions({
    midgardNodeUrl: config.midgardNodeUrl,
    headerHash,
  });
  return await prepareDoubleSpendFromTransactions({
    headerHash,
    transactions,
    expectedTransactionsRoot: config.expectedTransactionsRoot,
    tx1Id: config.tx1Id,
    tx2Id: config.tx2Id,
    outputDir: config.outputDir,
  });
};

export const prepareDoubleSpendFromFile = async (
  config: PrepareDoubleSpendFromFileConfig,
): Promise<PreparedDoubleSpendOutput> => {
  const transactions = await readNodeTransactionPayloadsFile(
    config.transactionsPath,
  );
  return await prepareDoubleSpendFromTransactions({
    headerHash: config.headerHash,
    transactions,
    expectedTransactionsRoot: config.expectedTransactionsRoot,
    tx1Id: config.tx1Id,
    tx2Id: config.tx2Id,
    outputDir: config.outputDir,
  });
};

export const prepareSampleDoubleSpend = async (
  config: PrepareSampleDoubleSpendConfig,
): Promise<PreparedDoubleSpendOutput> => {
  const transactions = makeSampleDoubleSpendTransactions();
  const output = await prepareDoubleSpendFromTransactions({
    headerHash: config.headerHash,
    transactions,
    expectedTransactionsRoot: config.expectedTransactionsRoot,
    outputDir: config.outputDir,
  });
  if (config.outputDir === undefined) {
    return output;
  }
  const blockTransactionsPath = await writeBlockTransactionsFile({
    outputDir: config.outputDir,
    transactions,
  });
  return {
    ...output,
    files:
      output.files === undefined
        ? undefined
        : {
            blockTransactionsPath,
            ...output.files,
          },
  };
};
