import { mkdir, writeFile } from "node:fs/promises";
import { join } from "node:path";

import {
  invalidRangeViolationReason,
  normalizeNativeTxValidityRange,
  type NativeTxCompact as NativeTxCompactData,
  type NormalizedTimeRange,
} from "@al-ft/midgard-sdk";

import {
  buildTrieView,
  compatibilityReasons,
  decodeTransactionMaterial,
  fetchNodeBlockTransactions,
  type FetchLike,
  nativeTrieItem,
  type NodeTransactionPayload,
  type PreparedTxInclusionJson,
  readNodeTransactionPayloadsFile,
  requireProof,
} from "./prepare-double-spend.js";
import {
  parseHex,
  parseSignedInteger,
  stringifyJson,
} from "./json-file.js";

export type InvalidRangeViolationReason = NonNullable<
  ReturnType<typeof invalidRangeViolationReason>
>;

export type PrepareInvalidRangeCliConfig = {
  readonly midgardNodeUrl: string;
  readonly headerHash: string;
  readonly blockValidFrom: string | number | bigint;
  readonly blockValidTo: string | number | bigint;
  readonly expectedTransactionsRoot?: string;
  readonly txId?: string;
  readonly outputDir?: string;
  readonly allowIncompatibleOutput?: boolean;
  readonly fetchImpl?: FetchLike;
};

export type PrepareInvalidRangeFromFileConfig = {
  readonly transactionsPath: string;
  readonly headerHash: string;
  readonly blockValidFrom: string | number | bigint;
  readonly blockValidTo: string | number | bigint;
  readonly expectedTransactionsRoot?: string;
  readonly txId?: string;
  readonly outputDir?: string;
  readonly allowIncompatibleOutput?: boolean;
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
  readonly blockValidity: {
    readonly validFrom: bigint;
    readonly validTo: bigint;
  };
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
  readonly tx: PreparedInvalidRangeTx;
  readonly files?: {
    readonly txInclusionPath: string;
    readonly planPath: string;
  };
};

const parseBlockValidity = ({
  blockValidFrom,
  blockValidTo,
}: {
  readonly blockValidFrom: string | number | bigint;
  readonly blockValidTo: string | number | bigint;
}): { readonly validFrom: bigint; readonly validTo: bigint } => {
  const validFrom = parseSignedInteger(blockValidFrom, "--block-valid-from");
  const validTo = parseSignedInteger(blockValidTo, "--block-valid-to");
  if (validFrom > validTo) {
    throw new Error("--block-valid-from must be less than or equal to --block-valid-to.");
  }
  return { validFrom, validTo };
};

const writePreparedFiles = async ({
  output,
  outputDir,
  allowIncompatibleOutput,
}: {
  readonly output: PreparedInvalidRangeOutput;
  readonly outputDir: string;
  readonly allowIncompatibleOutput: boolean;
}): Promise<PreparedInvalidRangeOutput["files"]> => {
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
        blockValidity: output.blockValidity,
        txNodeTxId: output.tx.nodeTxId,
        normalizedValidityRange: output.tx.normalizedValidityRange,
        violationReason: output.tx.violationReason,
        compatibility: output.compatibility,
        commitmentEncodings: output.commitmentEncodings,
      }),
    ),
  ]);
  return paths;
};

export const prepareInvalidRangeFromTransactions = async ({
  headerHash,
  transactions,
  blockValidFrom,
  blockValidTo,
  expectedTransactionsRoot,
  txId,
  outputDir,
  allowIncompatibleOutput = false,
}: {
  readonly headerHash: string;
  readonly transactions: readonly NodeTransactionPayload[];
  readonly blockValidFrom: string | number | bigint;
  readonly blockValidTo: string | number | bigint;
  readonly expectedTransactionsRoot?: string;
  readonly txId?: string;
  readonly outputDir?: string;
  readonly allowIncompatibleOutput?: boolean;
}): Promise<PreparedInvalidRangeOutput> => {
  const normalizedHeaderHash = parseHex(headerHash, "--header-hash", 28);
  const blockValidity = parseBlockValidity({ blockValidFrom, blockValidTo });
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
        blockValidFrom: blockValidity.validFrom,
        blockValidTo: blockValidity.validTo,
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
          ? `Requested --tx-id ${normalizedTxId} does not violate the block validity range.`
          : `Requested --tx-id ${normalizedTxId} was not found in the block.`,
      );
    }
    throw new Error("No invalid-range transaction found in the selected block.");
  }

  const nativeTrie = await buildTrieView(decoded.map(nativeTrieItem));
  const proofCbor = requireProof(
    nativeTrie,
    nativeTrieItem(selected.tx).key,
    "invalid-range tx",
  );
  const reasons = compatibilityReasons({
    nativeRoot: nativeTrie.root,
    expectedTransactionsRoot: normalizedExpectedRoot,
  });
  const baseOutput: PreparedInvalidRangeOutput = {
    headerHash: normalizedHeaderHash,
    txCount: decoded.length,
    blockValidity,
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
      nodeTxId: selected.tx.nodeTxId,
      nativeTx: selected.tx.nativeTxCompact,
      nativeTxCompactCbor: selected.tx.nativeCompactCbor,
      txInclusion: {
        nativeTxId: selected.tx.nodeTxId,
        nativeTx: selected.tx.nativeTxCompact,
        nativeTxCompactCbor: selected.tx.nativeCompactCbor,
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
    allowIncompatibleOutput,
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
    blockValidFrom: config.blockValidFrom,
    blockValidTo: config.blockValidTo,
    expectedTransactionsRoot: config.expectedTransactionsRoot,
    txId: config.txId,
    outputDir: config.outputDir,
    allowIncompatibleOutput: config.allowIncompatibleOutput,
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
    blockValidFrom: config.blockValidFrom,
    blockValidTo: config.blockValidTo,
    expectedTransactionsRoot: config.expectedTransactionsRoot,
    txId: config.txId,
    outputDir: config.outputDir,
    allowIncompatibleOutput: config.allowIncompatibleOutput,
  });
};
