import { mkdir, writeFile } from "node:fs/promises";
import { join } from "node:path";

import {
  EMPTY_SPEND_INPUTS_HASH,
  nativeTxBodyHasZeroInputViolation,
  type NativeTxCompact as NativeTxCompactData,
} from "@al-ft/midgard-sdk";

import { parseHex, stringifyJson } from "./json-file.js";
import {
  buildTrieView,
  compatibilityReasons,
  decodeTransactionMaterial,
  type FetchLike,
  fetchNodeBlockTransactions,
  nativeTrieItem,
  type NodeTransactionPayload,
  type PreparedTxInclusionJson,
  readNodeTransactionPayloadsFile,
  requireProof,
} from "./prepare-double-spend.js";

export type PrepareZeroInputCliConfig = {
  readonly midgardNodeUrl: string;
  readonly headerHash: string;
  readonly expectedTransactionsRoot?: string;
  readonly txId?: string;
  readonly outputDir?: string;
  readonly allowIncompatibleOutput?: boolean;
  readonly fetchImpl?: FetchLike;
};

export type PrepareZeroInputFromFileConfig = {
  readonly transactionsPath: string;
  readonly headerHash: string;
  readonly expectedTransactionsRoot?: string;
  readonly txId?: string;
  readonly outputDir?: string;
  readonly allowIncompatibleOutput?: boolean;
};

export type PreparedZeroInputTx = {
  readonly nodeTxId: string;
  readonly nativeTx: NativeTxCompactData;
  readonly nativeTxCompactCbor: string;
  readonly txInclusion: PreparedTxInclusionJson;
  readonly spendInputsHash: string;
};

export type PreparedZeroInputOutput = {
  readonly headerHash: string;
  readonly txCount: number;
  readonly compatibility: {
    readonly canUseSubmitStepCommands: boolean;
    readonly reasons: readonly string[];
  };
  readonly commitmentEncodings: {
    readonly nativeNode: {
      readonly transactionsRoot: string;
    };
    readonly expectedTransactionsRoot?: {
      readonly value: string;
      readonly matchesNativeNodeRoot: boolean;
    };
  };
  readonly tx: PreparedZeroInputTx;
  readonly files?: {
    readonly txInclusionPath: string;
    readonly planPath: string;
  };
};

const writePreparedFiles = async ({
  output,
  outputDir,
  allowIncompatibleOutput,
}: {
  readonly output: PreparedZeroInputOutput;
  readonly outputDir: string;
  readonly allowIncompatibleOutput: boolean;
}): Promise<PreparedZeroInputOutput["files"]> => {
  if (
    !output.compatibility.canUseSubmitStepCommands &&
    !allowIncompatibleOutput
  ) {
    throw new Error(
      "Refusing to write submit-step material because the selected block is not compatible with the current fault-proof ABI. Pass --allow-incompatible-output to write diagnostic files anyway.",
    );
  }
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
        txNodeTxId: output.tx.nodeTxId,
        spendInputsHash: output.tx.spendInputsHash,
        compatibility: output.compatibility,
        commitmentEncodings: output.commitmentEncodings,
      }),
    ),
  ]);
  return paths;
};

export const prepareZeroInputFromTransactions = async ({
  headerHash,
  transactions,
  expectedTransactionsRoot,
  txId,
  outputDir,
  allowIncompatibleOutput = false,
}: {
  readonly headerHash: string;
  readonly transactions: readonly NodeTransactionPayload[];
  readonly expectedTransactionsRoot?: string;
  readonly txId?: string;
  readonly outputDir?: string;
  readonly allowIncompatibleOutput?: boolean;
}): Promise<PreparedZeroInputOutput> => {
  const normalizedHeaderHash = parseHex(headerHash, "--header-hash", 28);
  const normalizedExpectedRoot =
    expectedTransactionsRoot === undefined
      ? undefined
      : parseHex(expectedTransactionsRoot, "--expected-transactions-root", 32);
  const normalizedTxId =
    txId === undefined ? undefined : parseHex(txId, "--tx-id", 32);
  const decoded = await Promise.all(
    transactions.map(decodeTransactionMaterial),
  );
  const candidates = decoded.filter((tx) =>
    nativeTxBodyHasZeroInputViolation({ txBody: tx.nativeTxCompact.body }),
  );

  const selected =
    normalizedTxId === undefined
      ? candidates[0]
      : candidates.find((tx) => tx.nodeTxId === normalizedTxId);
  if (selected === undefined) {
    if (normalizedTxId !== undefined) {
      const exists = decoded.some((tx) => tx.nodeTxId === normalizedTxId);
      throw new Error(
        exists
          ? `Requested --tx-id ${normalizedTxId} spends at least one input, so it does not violate the zero-input ledger rule.`
          : `Requested --tx-id ${normalizedTxId} was not found in the block.`,
      );
    }
    throw new Error("No zero-input transaction found in the selected block.");
  }

  const nativeTrie = await buildTrieView(decoded.map(nativeTrieItem));
  const proofCbor = requireProof(
    nativeTrie,
    nativeTrieItem(selected).key,
    "zero-input tx",
  );
  const reasons = compatibilityReasons({
    nativeRoot: nativeTrie.root,
    expectedTransactionsRoot: normalizedExpectedRoot,
  });
  const baseOutput: PreparedZeroInputOutput = {
    headerHash: normalizedHeaderHash,
    txCount: decoded.length,
    compatibility: {
      canUseSubmitStepCommands: reasons.length === 0,
      reasons,
    },
    commitmentEncodings: {
      nativeNode: {
        transactionsRoot: nativeTrie.root,
      },
      ...(normalizedExpectedRoot === undefined
        ? {}
        : {
            expectedTransactionsRoot: {
              value: normalizedExpectedRoot,
              matchesNativeNodeRoot: normalizedExpectedRoot === nativeTrie.root,
            },
          }),
    },
    tx: {
      nodeTxId: selected.nodeTxId,
      nativeTx: selected.nativeTxCompact,
      nativeTxCompactCbor: selected.nativeCompactCbor,
      txInclusion: {
        nativeTxId: selected.nodeTxId,
        nativeTx: selected.nativeTxCompact,
        nativeTxCompactCbor: selected.nativeCompactCbor,
        transactionsPhasRoot: nativeTrie.root,
        txMembershipProofCbor: proofCbor,
      },
      spendInputsHash: EMPTY_SPEND_INPUTS_HASH,
    },
  };
  if (outputDir === undefined) {
    return baseOutput;
  }
  const files = await writePreparedFiles({
    output: baseOutput,
    outputDir,
    allowIncompatibleOutput,
  });
  return { ...baseOutput, files };
};

export const prepareZeroInputFromNode = async (
  config: PrepareZeroInputCliConfig,
): Promise<PreparedZeroInputOutput> => {
  const headerHash = parseHex(config.headerHash, "--header-hash", 28);
  const transactions = await fetchNodeBlockTransactions({
    midgardNodeUrl: config.midgardNodeUrl,
    headerHash,
    fetchImpl: config.fetchImpl,
  });
  return await prepareZeroInputFromTransactions({
    headerHash,
    transactions,
    expectedTransactionsRoot: config.expectedTransactionsRoot,
    txId: config.txId,
    outputDir: config.outputDir,
    allowIncompatibleOutput: config.allowIncompatibleOutput,
  });
};

export const prepareZeroInputFromFile = async (
  config: PrepareZeroInputFromFileConfig,
): Promise<PreparedZeroInputOutput> => {
  const transactions = await readNodeTransactionPayloadsFile(
    config.transactionsPath,
  );
  return await prepareZeroInputFromTransactions({
    headerHash: config.headerHash,
    transactions,
    expectedTransactionsRoot: config.expectedTransactionsRoot,
    txId: config.txId,
    outputDir: config.outputDir,
    allowIncompatibleOutput: config.allowIncompatibleOutput,
  });
};
