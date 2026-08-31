/** Evidence builder for a code-1 normal `transactions_root` leaf. */
import { mkdir, writeFile } from "node:fs/promises";
import { join } from "node:path";

import {
  commitCountedRootProgram,
  type NativeTxCompact,
  ROOT_DOMAINS,
} from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import { parseHex, stringifyJson } from "../json-file.js";
import {
  buildTrieView,
  decodeTransactionMaterial,
  type FetchLike,
  fetchNodeBlockTransactions,
  type NodeTransactionPayload,
  type PreparedTxInclusionJson,
  readNodeTransactionPayloadsFile,
  requireProof,
  transactionSourceTrieItemV1,
} from "../prepare-double-spend.js";

export type PreparedL2TxMistagOutputV1 = {
  readonly headerHash: string;
  readonly txCount: number;
  readonly transactionsPhasRoot: string;
  readonly committedTransactionsRoot: string;
  readonly tx: {
    readonly nodeTxId: string;
    readonly nativeTx: NativeTxCompact;
    readonly nativeTxCompactCbor: string;
    readonly committedValidityCode: bigint;
    readonly txInclusion: PreparedTxInclusionJson;
  };
  readonly files?: {
    readonly txInclusionPath: string;
    readonly planPath: string;
  };
};

export type PrepareL2TxMistagConfigV1 = {
  readonly headerHash: string;
  readonly expectedTransactionsRoot: string;
  readonly txId?: string;
  readonly outputDir?: string;
};

const writePreparedFiles = async (
  output: PreparedL2TxMistagOutputV1,
  outputDir: string,
): Promise<NonNullable<PreparedL2TxMistagOutputV1["files"]>> => {
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
        txId: output.tx.nodeTxId,
        committedValidityCode: output.tx.committedValidityCode,
        transactionsPhasRoot: output.transactionsPhasRoot,
        committedTransactionsRoot: output.committedTransactionsRoot,
      }),
    ),
  ]);
  return paths;
};

export const prepareL2TxMistagFromTransactionsV1 = async ({
  headerHash,
  transactions,
  expectedTransactionsRoot,
  txId,
  outputDir,
}: PrepareL2TxMistagConfigV1 & {
  readonly transactions: readonly NodeTransactionPayload[];
}): Promise<PreparedL2TxMistagOutputV1> => {
  const normalizedHeaderHash = parseHex(headerHash, "--header-hash", 28);
  const normalizedExpectedRoot = parseHex(
    expectedTransactionsRoot,
    "--expected-transactions-root",
    32,
  );
  const normalizedTxId =
    txId === undefined ? undefined : parseHex(txId, "--tx-id", 32);
  const decoded = await Promise.all(
    transactions.map(decodeTransactionMaterial),
  );
  const candidates = decoded.filter(
    (tx) => tx.nativeTxCompact.validity_code !== 0n,
  );
  const selected =
    normalizedTxId === undefined
      ? candidates[0]
      : candidates.find((tx) => tx.nodeTxId === normalizedTxId);
  if (selected === undefined) {
    const found =
      normalizedTxId === undefined
        ? undefined
        : decoded.find((tx) => tx.nodeTxId === normalizedTxId);
    throw new Error(
      found === undefined
        ? "No code-1 normal transaction leaf was found in the selected block."
        : `Requested --tx-id ${normalizedTxId} carries validity code 0; an honest acceptance cannot be challenged.`,
    );
  }

  const trie = await buildTrieView(decoded.map(transactionSourceTrieItemV1));
  const committedTransactionsRoot = await Effect.runPromise(
    commitCountedRootProgram({
      domain: ROOT_DOMAINS.transactionsV1,
      phasRoot: trie.root,
      count: BigInt(decoded.length),
    }),
  );
  if (committedTransactionsRoot !== normalizedExpectedRoot) {
    throw new Error(
      `Reconstructed counted transactions root ${committedTransactionsRoot} does not match ${normalizedExpectedRoot}.`,
    );
  }
  const txMembershipProofCbor = requireProof(
    trie,
    transactionSourceTrieItemV1(selected).key,
    "l2-tx-mistag tx",
  );
  const txInclusion: PreparedTxInclusionJson = {
    nativeTxId: selected.nodeTxId,
    nativeTx: selected.nativeTxCompact,
    nativeTxCompactCbor: selected.nativeCompactCbor,
    l2TransactionSourceCbor: selected.l2TransactionSourceCbor,
    transactionsPhasRoot: trie.root,
    txMembershipProofCbor,
  };
  const output: PreparedL2TxMistagOutputV1 = {
    headerHash: normalizedHeaderHash,
    txCount: decoded.length,
    transactionsPhasRoot: trie.root,
    committedTransactionsRoot,
    tx: {
      nodeTxId: selected.nodeTxId,
      nativeTx: selected.nativeTxCompact,
      nativeTxCompactCbor: selected.nativeCompactCbor,
      committedValidityCode: selected.nativeTxCompact.validity_code,
      txInclusion,
    },
  };
  return outputDir === undefined
    ? output
    : { ...output, files: await writePreparedFiles(output, outputDir) };
};

export const prepareL2TxMistagFromNodeV1 = async (
  config: PrepareL2TxMistagConfigV1 & {
    readonly midgardNodeUrl: string;
    readonly fetchImpl?: FetchLike;
  },
): Promise<PreparedL2TxMistagOutputV1> => {
  const headerHash = parseHex(config.headerHash, "--header-hash", 28);
  const transactions = await fetchNodeBlockTransactions({
    midgardNodeUrl: config.midgardNodeUrl,
    headerHash,
    fetchImpl: config.fetchImpl,
  });
  return prepareL2TxMistagFromTransactionsV1({ ...config, transactions });
};

export const prepareL2TxMistagFromFileV1 = async (
  config: PrepareL2TxMistagConfigV1 & { readonly transactionsPath: string },
): Promise<PreparedL2TxMistagOutputV1> =>
  prepareL2TxMistagFromTransactionsV1({
    ...config,
    transactions: await readNodeTransactionPayloadsFile(
      config.transactionsPath,
    ),
  });
