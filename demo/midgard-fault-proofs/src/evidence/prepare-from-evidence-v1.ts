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
