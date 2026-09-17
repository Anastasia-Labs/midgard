/**
 * Canonical-evidence proof builders (`Q03`).
 *
 * These are the builder entry points `GOAL_SPEC.md` §9.2 asks for: they take
 * verified `DaPayloadV1` evidence bound to an authenticated L1 observation and
 * nothing else. Each one is fail-closed twice:
 *
 * 1. `assertSecurityGradeEvidenceV1` — no diagnostic or operator-private record
 *    can reach a submittable proof; and
 * 2. `assertTransactionSourceInclusionRootAuthenticated` — the exact
 *    `L2TransactionSourceV1` MPF root the inclusion argument carries must re-commit to the
 *    L1-committed `transactions_root` under `TransactionsV1RootDomain`.
 *
 * The source leaf binds the compact transaction, witness compact, and field
 * length material. Compact-only membership is not admitted as an alternate
 * convention.
 */
import { assertTransactionSourceInclusionRootAuthenticated } from "@al-ft/midgard-sdk";

import {
  type PreparedDoubleSpendOutput,
  prepareDoubleSpendFromTransactions,
} from "../prepare-double-spend.js";
import { prepareInputNoIdxFromCanonicalEvidence } from "../prepare-input-no-idx.js";
import {
  type PreparedInvalidRangeOutput,
  prepareInvalidRangeFromTransactions,
} from "../prepare-invalid-range.js";
import {
  type PreparedInvalidSignatureOutput,
  prepareInvalidSignatureFromTransactions,
} from "../prepare-invalid-signature.js";
import {
  type PreparedMinFeeOutput,
  prepareMinFeeFromTransactions,
} from "../prepare-min-fee.js";
import {
  type PreparedNoReferenceInputOutput,
  prepareNoReferenceInputFromTransactions,
} from "../prepare-no-reference-input.js";
import {
  type PreparedNonExistentInputOutput,
  prepareNonExistentInputFromTransactions,
} from "../prepare-non-existent-input.js";
import {
  type PreparedZeroInputOutput,
  prepareZeroInputFromTransactions,
} from "../prepare-zero-input.js";
import {
  blockTransactionsFromCanonicalEvidence,
  type CanonicalBlockEvidence,
} from "./canonical-block-evidence.js";

export type CanonicalEvidenceBuilderInput = {
  readonly evidence: CanonicalBlockEvidence;
  readonly outputDir?: string;
};

/**
 * Shared admission for every canonical-evidence builder. Returns the
 * authenticated transaction material and the L1-committed transactions root the
 * prepared proof must match.
 */
export const admitCanonicalEvidenceForProofBuild = (
  evidence: CanonicalBlockEvidence,
) => {
  const transactions = blockTransactionsFromCanonicalEvidence(evidence);
  assertTransactionSourceInclusionRootAuthenticated(
    evidence.inclusionRootAuthentication,
  );
  return {
    transactions,
    headerHash: evidence.headerHash,
    expectedTransactionsRoot: evidence.header.transactionsRoot,
  };
};

export const prepareDoubleSpendFromCanonicalEvidence = async ({
  evidence,
  tx1Id,
  tx2Id,
  outputDir,
}: CanonicalEvidenceBuilderInput & {
  readonly tx1Id?: string;
  readonly tx2Id?: string;
}): Promise<PreparedDoubleSpendOutput> => {
  const admitted = admitCanonicalEvidenceForProofBuild(evidence);
  return await prepareDoubleSpendFromTransactions({
    headerHash: admitted.headerHash,
    transactions: admitted.transactions,
    expectedTransactionsRoot: admitted.expectedTransactionsRoot,
    ...(tx1Id === undefined ? {} : { tx1Id }),
    ...(tx2Id === undefined ? {} : { tx2Id }),
    ...(outputDir === undefined ? {} : { outputDir }),
  });
};

export const prepareZeroInputFromCanonicalEvidence = async ({
  evidence,
  txId,
  outputDir,
}: CanonicalEvidenceBuilderInput & {
  readonly txId?: string;
}): Promise<PreparedZeroInputOutput> => {
  const admitted = admitCanonicalEvidenceForProofBuild(evidence);
  return await prepareZeroInputFromTransactions({
    headerHash: admitted.headerHash,
    transactions: admitted.transactions,
    expectedTransactionsRoot: admitted.expectedTransactionsRoot,
    ...(txId === undefined ? {} : { txId }),
    ...(outputDir === undefined ? {} : { outputDir }),
  });
};

export const prepareInvalidRangeFromCanonicalEvidence = async ({
  evidence,
  txId,
  outputDir,
}: CanonicalEvidenceBuilderInput & {
  readonly txId?: string;
}): Promise<PreparedInvalidRangeOutput> => {
  const admitted = admitCanonicalEvidenceForProofBuild(evidence);
  // Phase B's evaluation slot is header-bound evidence, never an operator
  // parameter: it comes from the same authenticated header the roots do.
  return await prepareInvalidRangeFromTransactions({
    headerHash: admitted.headerHash,
    transactions: admitted.transactions,
    blockSlot: evidence.header.blockSlot,
    expectedTransactionsRoot: admitted.expectedTransactionsRoot,
    ...(txId === undefined ? {} : { txId }),
    ...(outputDir === undefined ? {} : { outputDir }),
  });
};

export const prepareInvalidSignatureFromCanonicalEvidence = async ({
  evidence,
  txId,
  outputDir,
}: CanonicalEvidenceBuilderInput & {
  readonly txId?: string;
}): Promise<PreparedInvalidSignatureOutput> => {
  const admitted = admitCanonicalEvidenceForProofBuild(evidence);
  return await prepareInvalidSignatureFromTransactions({
    headerHash: admitted.headerHash,
    transactions: admitted.transactions,
    expectedTransactionsRoot: admitted.expectedTransactionsRoot,
    ...(txId === undefined ? {} : { txId }),
    ...(outputDir === undefined ? {} : { outputDir }),
  });
};

export const prepareMinFeeFromCanonicalEvidence = async ({
  evidence,
  txId,
  categoryId,
  outputDir,
}: CanonicalEvidenceBuilderInput & {
  readonly txId?: string;
  readonly categoryId?: string;
}): Promise<PreparedMinFeeOutput> => {
  const admitted = admitCanonicalEvidenceForProofBuild(evidence);
  return await prepareMinFeeFromTransactions({
    headerHash: admitted.headerHash,
    transactions: admitted.transactions,
    expectedTransactionsRoot: admitted.expectedTransactionsRoot,
    minFeeA: evidence.header.minFeeA,
    minFeeB: evidence.header.minFeeB,
    ...(txId === undefined ? {} : { txId }),
    ...(categoryId === undefined ? {} : { categoryId }),
    ...(outputDir === undefined ? {} : { outputDir }),
  });
};

/**
 * Builds a non-existent-input proof solely from admitted block evidence. A
 * non-genesis predecessor ledger must be supplied as its own verified
 * DA/L1-bound block evidence; an operator file is never accepted here.
 */
export const prepareNonExistentInputFromCanonicalEvidence = async ({
  evidence,
  previousBlockEvidence,
  badTxId,
  badInputIndex,
  outputDir,
}: CanonicalEvidenceBuilderInput & {
  readonly previousBlockEvidence?: CanonicalBlockEvidence;
  readonly badTxId?: string;
  readonly badInputIndex?: string | number;
}): Promise<PreparedNonExistentInputOutput> => {
  const admitted = admitCanonicalEvidenceForProofBuild(evidence);
  let previousBlockPayloadEnvelopeCbor: Uint8Array | undefined;
  if (previousBlockEvidence !== undefined) {
    // The predecessor contributes an authenticated ledger snapshot, not a
    // native transaction-inclusion argument. Admit both provenances without
    // incorrectly requiring its transaction leaf convention to be native.
    blockTransactionsFromCanonicalEvidence(previousBlockEvidence);
    if (evidence.header.prevHeaderHash !== previousBlockEvidence.headerHash) {
      throw new Error(
        "Previous canonical block evidence is not the predecessor committed by this header.",
      );
    }
    if (
      evidence.header.prevUtxosRoot !== previousBlockEvidence.header.utxosRoot
    ) {
      throw new Error(
        "Previous canonical block evidence does not authenticate this block's prev_utxos_root.",
      );
    }
    previousBlockPayloadEnvelopeCbor =
      previousBlockEvidence.reconstruction.payloadEnvelopeCbor;
  }
  return await prepareNonExistentInputFromTransactions({
    headerHash: admitted.headerHash,
    transactions: admitted.transactions,
    prevUtxosRoot: evidence.header.prevUtxosRoot,
    expectedTransactionsRoot: admitted.expectedTransactionsRoot,
    ...(previousBlockPayloadEnvelopeCbor === undefined
      ? {}
      : { prevBlockPayloadEnvelopeCbor: previousBlockPayloadEnvelopeCbor }),
    ...(badTxId === undefined ? {} : { badTxId }),
    ...(badInputIndex === undefined ? {} : { badInputIndex }),
    ...(outputDir === undefined ? {} : { outputDir }),
  });
};

/**
 * Builds a no-reference-input proof from the same exact authenticated current
 * and predecessor evidence boundary as non-existent-input.
 */
export const prepareNoReferenceInputFromCanonicalEvidence = async ({
  evidence,
  previousBlockEvidence,
  badTxId,
  badReferenceInputIndex,
  outputDir,
}: CanonicalEvidenceBuilderInput & {
  readonly previousBlockEvidence?: CanonicalBlockEvidence;
  readonly badTxId?: string;
  readonly badReferenceInputIndex?: string | number;
}): Promise<PreparedNoReferenceInputOutput> => {
  const admitted = admitCanonicalEvidenceForProofBuild(evidence);
  let previousBlockPayloadEnvelopeCbor: Uint8Array | undefined;
  if (previousBlockEvidence !== undefined) {
    blockTransactionsFromCanonicalEvidence(previousBlockEvidence);
    if (evidence.header.prevHeaderHash !== previousBlockEvidence.headerHash) {
      throw new Error(
        "Previous canonical block evidence is not the predecessor committed by this header.",
      );
    }
    if (
      evidence.header.prevUtxosRoot !== previousBlockEvidence.header.utxosRoot
    ) {
      throw new Error(
        "Previous canonical block evidence does not authenticate this block's prev_utxos_root.",
      );
    }
    previousBlockPayloadEnvelopeCbor =
      previousBlockEvidence.reconstruction.payloadEnvelopeCbor;
  }
  return await prepareNoReferenceInputFromTransactions({
    headerHash: admitted.headerHash,
    transactions: admitted.transactions,
    prevUtxosRoot: evidence.header.prevUtxosRoot,
    expectedTransactionsRoot: admitted.expectedTransactionsRoot,
    ...(previousBlockPayloadEnvelopeCbor === undefined
      ? {}
      : { prevBlockPayloadEnvelopeCbor: previousBlockPayloadEnvelopeCbor }),
    ...(badTxId === undefined ? {} : { badTxId }),
    ...(badReferenceInputIndex === undefined ? {} : { badReferenceInputIndex }),
    ...(outputDir === undefined ? {} : { outputDir }),
  });
};

export { prepareInputNoIdxFromCanonicalEvidence };

export type CanonicalPrepareCommand =
  | {
      readonly command: "prepare-double-spend";
      readonly tx1Id?: string;
      readonly tx2Id?: string;
      readonly outputDir?: string;
    }
  | {
      readonly command: "prepare-invalid-range";
      readonly txId?: string;
      readonly outputDir?: string;
    }
  | {
      readonly command: "prepare-min-fee";
      readonly txId?: string;
      readonly categoryId?: string;
      readonly outputDir?: string;
    }
  | {
      readonly command: "prepare-invalid-signature";
      readonly txId?: string;
      readonly outputDir?: string;
    }
  | {
      readonly command: "prepare-non-existent-input";
      readonly badTxId?: string;
      readonly badInputIndex?: string | number;
      readonly outputDir?: string;
    }
  | {
      readonly command: "prepare-input-no-idx";
      readonly badTxId?: string;
      readonly badInputsIndex?: string | number;
      readonly outputDir?: string;
    }
  | {
      readonly command: "prepare-zero-input";
      readonly txId?: string;
      readonly outputDir?: string;
    };

/** Package-root-reachable canonical router for every prepare CLI verb. */
export const executeCanonicalPrepareCommand = async ({
  request,
  evidence,
  previousBlockEvidence,
}: {
  readonly request: CanonicalPrepareCommand;
  readonly evidence: CanonicalBlockEvidence;
  readonly previousBlockEvidence?: CanonicalBlockEvidence;
}) => {
  switch (request.command) {
    case "prepare-double-spend":
      return await prepareDoubleSpendFromCanonicalEvidence({
        evidence,
        ...(request.tx1Id === undefined ? {} : { tx1Id: request.tx1Id }),
        ...(request.tx2Id === undefined ? {} : { tx2Id: request.tx2Id }),
        ...(request.outputDir === undefined
          ? {}
          : { outputDir: request.outputDir }),
      });
    case "prepare-invalid-range":
      return await prepareInvalidRangeFromCanonicalEvidence({
        evidence,
        ...(request.txId === undefined ? {} : { txId: request.txId }),
        ...(request.outputDir === undefined
          ? {}
          : { outputDir: request.outputDir }),
      });
    case "prepare-min-fee":
      return await prepareMinFeeFromCanonicalEvidence({
        evidence,
        ...(request.txId === undefined ? {} : { txId: request.txId }),
        ...(request.categoryId === undefined
          ? {}
          : { categoryId: request.categoryId }),
        ...(request.outputDir === undefined
          ? {}
          : { outputDir: request.outputDir }),
      });
    case "prepare-invalid-signature":
      return await prepareInvalidSignatureFromCanonicalEvidence({
        evidence,
        ...(request.txId === undefined ? {} : { txId: request.txId }),
        ...(request.outputDir === undefined
          ? {}
          : { outputDir: request.outputDir }),
      });
    case "prepare-non-existent-input":
      return await prepareNonExistentInputFromCanonicalEvidence({
        evidence,
        ...(previousBlockEvidence === undefined
          ? {}
          : { previousBlockEvidence }),
        ...(request.badTxId === undefined ? {} : { badTxId: request.badTxId }),
        ...(request.badInputIndex === undefined
          ? {}
          : { badInputIndex: request.badInputIndex }),
        ...(request.outputDir === undefined
          ? {}
          : { outputDir: request.outputDir }),
      });
    case "prepare-input-no-idx":
      return await prepareInputNoIdxFromCanonicalEvidence({
        evidence,
        ...(request.badTxId === undefined ? {} : { badTxId: request.badTxId }),
        ...(request.badInputsIndex === undefined
          ? {}
          : { badInputsIndex: request.badInputsIndex }),
        ...(request.outputDir === undefined
          ? {}
          : { outputDir: request.outputDir }),
      });
    case "prepare-zero-input":
      return await prepareZeroInputFromCanonicalEvidence({
        evidence,
        ...(request.txId === undefined ? {} : { txId: request.txId }),
        ...(request.outputDir === undefined
          ? {}
          : { outputDir: request.outputDir }),
      });
  }
};
