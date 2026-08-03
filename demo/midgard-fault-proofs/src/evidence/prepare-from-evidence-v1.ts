/**
 * Canonical-evidence proof builders (`Q03`).
 *
 * These are the builder entry points `GOAL_SPEC.md` §9.2 asks for: they take
 * verified `DaPayloadV1` evidence bound to an authenticated L1 observation and
 * nothing else. Each one is fail-closed twice:
 *
 * 1. `assertSecurityGradeEvidenceV1` — no diagnostic or operator-private record
 *    can reach a submittable proof; and
 * 2. `assertNativeInclusionRootAuthenticatedV1` — the raw transactions MPF root
 *    the family's `NativeTxInclusionArgs` will carry must re-commit to the
 *    L1-committed `transactions_root` under `TransactionsV1RootDomain`.
 *
 * Gate (2) is not cosmetic. The deployed Aiken step
 * (`verify_native_tx_in_state_queue_node`) opens the membership proof with
 * `value_bytes = native_tx_compact_cbor`, while the node commits the header's
 * `transactions_root` over `Data(L2TransactionSourceV1)` leaves
 * (`encodeTransactionRootValue`). Where those disagree, any inclusion argument
 * built from real block data is unprovable, and this gate refuses to emit it
 * instead of handing a prover a proof that must fail on-chain.
 */
import { assertNativeInclusionRootAuthenticatedV1 } from "@al-ft/midgard-sdk";

import {
  type PreparedDoubleSpendOutput,
  prepareDoubleSpendFromTransactions,
} from "../prepare-double-spend.js";
import {
  type PreparedInvalidRangeOutput,
  prepareInvalidRangeFromTransactions,
} from "../prepare-invalid-range.js";
import {
  type PreparedNonExistentInputOutput,
  prepareNonExistentInputFromTransactions,
} from "../prepare-non-existent-input.js";
import {
  type PreparedZeroInputOutput,
  prepareZeroInputFromTransactions,
} from "../prepare-zero-input.js";
import {
  blockTransactionsFromCanonicalEvidenceV1,
  type CanonicalBlockEvidenceV1,
} from "./canonical-block-evidence-v1.js";

export type CanonicalEvidenceBuilderInputV1 = {
  readonly evidence: CanonicalBlockEvidenceV1;
  readonly outputDir?: string;
};

/**
 * Shared admission for every canonical-evidence builder. Returns the
 * authenticated transaction material and the L1-committed transactions root the
 * prepared proof must match.
 */
export const admitCanonicalEvidenceForProofBuildV1 = (
  evidence: CanonicalBlockEvidenceV1,
) => {
  const transactions = blockTransactionsFromCanonicalEvidenceV1(evidence);
  assertNativeInclusionRootAuthenticatedV1(
    evidence.inclusionRootAuthentication,
  );
  return {
    transactions,
    headerHash: evidence.headerHash,
    expectedTransactionsRoot: evidence.header.transactionsRoot,
  };
};

export const prepareDoubleSpendFromCanonicalEvidenceV1 = async ({
  evidence,
  tx1Id,
  tx2Id,
  outputDir,
}: CanonicalEvidenceBuilderInputV1 & {
  readonly tx1Id?: string;
  readonly tx2Id?: string;
}): Promise<PreparedDoubleSpendOutput> => {
  const admitted = admitCanonicalEvidenceForProofBuildV1(evidence);
  return await prepareDoubleSpendFromTransactions({
    headerHash: admitted.headerHash,
    transactions: admitted.transactions,
    expectedTransactionsRoot: admitted.expectedTransactionsRoot,
    ...(tx1Id === undefined ? {} : { tx1Id }),
    ...(tx2Id === undefined ? {} : { tx2Id }),
    ...(outputDir === undefined ? {} : { outputDir }),
  });
};

export const prepareZeroInputFromCanonicalEvidenceV1 = async ({
  evidence,
  txId,
  outputDir,
}: CanonicalEvidenceBuilderInputV1 & {
  readonly txId?: string;
}): Promise<PreparedZeroInputOutput> => {
  const admitted = admitCanonicalEvidenceForProofBuildV1(evidence);
  return await prepareZeroInputFromTransactions({
    headerHash: admitted.headerHash,
    transactions: admitted.transactions,
    expectedTransactionsRoot: admitted.expectedTransactionsRoot,
    ...(txId === undefined ? {} : { txId }),
    ...(outputDir === undefined ? {} : { outputDir }),
  });
};

export const prepareInvalidRangeFromCanonicalEvidenceV1 = async ({
  evidence,
  txId,
  outputDir,
}: CanonicalEvidenceBuilderInputV1 & {
  readonly txId?: string;
}): Promise<PreparedInvalidRangeOutput> => {
  const admitted = admitCanonicalEvidenceForProofBuildV1(evidence);
  // The block validity window is header-bound evidence, never an operator
  // parameter: it comes from the same authenticated header the roots do.
  return await prepareInvalidRangeFromTransactions({
    headerHash: admitted.headerHash,
    transactions: admitted.transactions,
    blockValidFrom: evidence.header.startTime,
    blockValidTo: evidence.header.endTime,
    expectedTransactionsRoot: admitted.expectedTransactionsRoot,
    ...(txId === undefined ? {} : { txId }),
    ...(outputDir === undefined ? {} : { outputDir }),
  });
};

/**
 * Builds a non-existent-input proof solely from admitted block evidence. A
 * non-genesis predecessor ledger must be supplied as its own verified
 * DA/L1-bound block evidence; an operator file is never accepted here.
 */
export const prepareNonExistentInputFromCanonicalEvidenceV1 = async ({
  evidence,
  previousBlockEvidence,
  badTxId,
  badInputIndex,
  outputDir,
}: CanonicalEvidenceBuilderInputV1 & {
  readonly previousBlockEvidence?: CanonicalBlockEvidenceV1;
  readonly badTxId?: string;
  readonly badInputIndex?: string | number;
}): Promise<PreparedNonExistentInputOutput> => {
  const admitted = admitCanonicalEvidenceForProofBuildV1(evidence);
  let previousBlockPayloadEnvelopeCbor: Uint8Array | undefined;
  if (previousBlockEvidence !== undefined) {
    // The predecessor contributes an authenticated ledger snapshot, not a
    // native transaction-inclusion argument. Admit both provenances without
    // incorrectly requiring its transaction leaf convention to be native.
    blockTransactionsFromCanonicalEvidenceV1(previousBlockEvidence);
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

export type CanonicalPrepareCommandV1 =
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
      readonly command: "prepare-non-existent-input";
      readonly badTxId?: string;
      readonly badInputIndex?: string | number;
      readonly outputDir?: string;
    }
  | {
      readonly command: "prepare-zero-input";
      readonly txId?: string;
      readonly outputDir?: string;
    };

/** Package-root-reachable canonical router for all four prepare CLI verbs. */
export const executeCanonicalPrepareCommandV1 = async ({
  request,
  evidence,
  previousBlockEvidence,
}: {
  readonly request: CanonicalPrepareCommandV1;
  readonly evidence: CanonicalBlockEvidenceV1;
  readonly previousBlockEvidence?: CanonicalBlockEvidenceV1;
}) => {
  switch (request.command) {
    case "prepare-double-spend":
      return await prepareDoubleSpendFromCanonicalEvidenceV1({
        evidence,
        ...(request.tx1Id === undefined ? {} : { tx1Id: request.tx1Id }),
        ...(request.tx2Id === undefined ? {} : { tx2Id: request.tx2Id }),
        ...(request.outputDir === undefined
          ? {}
          : { outputDir: request.outputDir }),
      });
    case "prepare-invalid-range":
      return await prepareInvalidRangeFromCanonicalEvidenceV1({
        evidence,
        ...(request.txId === undefined ? {} : { txId: request.txId }),
        ...(request.outputDir === undefined
          ? {}
          : { outputDir: request.outputDir }),
      });
    case "prepare-non-existent-input":
      return await prepareNonExistentInputFromCanonicalEvidenceV1({
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
    case "prepare-zero-input":
      return await prepareZeroInputFromCanonicalEvidenceV1({
        evidence,
        ...(request.txId === undefined ? {} : { txId: request.txId }),
        ...(request.outputDir === undefined
          ? {}
          : { outputDir: request.outputDir }),
      });
  }
};
