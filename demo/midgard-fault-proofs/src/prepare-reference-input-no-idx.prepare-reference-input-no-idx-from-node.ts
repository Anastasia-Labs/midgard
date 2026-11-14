import {
  admitCanonicalEvidenceForProofBuild,
  type CanonicalBlockEvidence,
} from "./evidence/index.js";
import { parseHex } from "./json-file.js";
import {
  fetchNodeBlockTransactions,
  readNodeTransactionPayloadsFile,
} from "./prepare-double-spend.js";
import {
  type PreparedReferenceInputNoIdxOutput,
  type PrepareReferenceInputNoIdxCliConfig,
  type PrepareReferenceInputNoIdxFromFileConfig,
} from "./prepare-reference-input-no-idx.decode-tx.js";
import { prepareReferenceInputNoIdxFromTransactions } from "./prepare-reference-input-no-idx.prepare-reference-input-no-idx-from-transactions.js";

/** Security-grade builder over one authenticated header/public-DA block. */
export const prepareReferenceInputNoIdxFromCanonicalEvidence = async ({
  evidence,
  badTxId,
  badReferenceInputIndex,
  outputDir,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly badTxId?: string;
  readonly badReferenceInputIndex?: string | number;
  readonly outputDir?: string;
}): Promise<PreparedReferenceInputNoIdxOutput> => {
  const admitted = admitCanonicalEvidenceForProofBuild(evidence);
  return await prepareReferenceInputNoIdxFromTransactions({
    headerHash: admitted.headerHash,
    transactions: admitted.transactions,
    expectedTransactionsRoot: admitted.expectedTransactionsRoot,
    ...(badTxId === undefined ? {} : { badTxId }),
    ...(badReferenceInputIndex === undefined ? {} : { badReferenceInputIndex }),
    ...(outputDir === undefined ? {} : { outputDir }),
  });
};

/**
 * Operator-diagnostic rehearsal route: block material is read from the node's
 * REST surface, an `operator_only_diagnostic_endpoint`. Never security grade.
 */
export const prepareReferenceInputNoIdxFromNode = async (
  config: PrepareReferenceInputNoIdxCliConfig,
): Promise<PreparedReferenceInputNoIdxOutput> => {
  const headerHash = parseHex(config.headerHash, "--header-hash", 28);
  const transactions = await fetchNodeBlockTransactions({
    midgardNodeUrl: config.midgardNodeUrl,
    headerHash,
    ...(config.fetchImpl === undefined ? {} : { fetchImpl: config.fetchImpl }),
  });
  return await prepareReferenceInputNoIdxFromTransactions({
    headerHash,
    transactions,
    ...(config.badTxId === undefined ? {} : { badTxId: config.badTxId }),
    ...(config.badReferenceInputIndex === undefined
      ? {}
      : { badReferenceInputIndex: config.badReferenceInputIndex }),
    ...(config.expectedTransactionsRoot === undefined
      ? {}
      : { expectedTransactionsRoot: config.expectedTransactionsRoot }),
    ...(config.outputDir === undefined ? {} : { outputDir: config.outputDir }),
  });
};

/** Operator-diagnostic rehearsal route over an `operator_private_file`. */
export const prepareReferenceInputNoIdxFromFile = async (
  config: PrepareReferenceInputNoIdxFromFileConfig,
): Promise<PreparedReferenceInputNoIdxOutput> => {
  const transactions = await readNodeTransactionPayloadsFile(
    config.transactionsPath,
  );
  return await prepareReferenceInputNoIdxFromTransactions({
    headerHash: config.headerHash,
    transactions,
    ...(config.badTxId === undefined ? {} : { badTxId: config.badTxId }),
    ...(config.badReferenceInputIndex === undefined
      ? {}
      : { badReferenceInputIndex: config.badReferenceInputIndex }),
    ...(config.expectedTransactionsRoot === undefined
      ? {}
      : { expectedTransactionsRoot: config.expectedTransactionsRoot }),
    ...(config.outputDir === undefined ? {} : { outputDir: config.outputDir }),
  });
};
