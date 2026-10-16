import * as SDK from "@al-ft/midgard-sdk";

type EventCommitmentRoots = Pick<
  SDK.MakeHeaderTransitionCommitmentsInput,
  | "withdrawalsRoot"
  | "forcedTransactionsRoot"
  | "transactionsRoot"
  | "depositsRoot"
  | "transitionTraceRoot"
  | "eventToStepRoot"
>;

type EventCommitmentCounts = Pick<
  SDK.MakeHeaderTransitionCommitmentsInput,
  | "withdrawalCount"
  | "forcedTransactionCount"
  | "l2TransactionCount"
  | "depositCount"
  | "transitionStepCount"
>;

/**
 * A block commits each transaction's leaf verdict and no validation-trace
 * descriptor, so the header's validation-trace pair is the empty tree with
 * count zero.
 */
export const makeEventCommitments = (
  roots: EventCommitmentRoots,
  counts: EventCommitmentCounts,
) =>
  SDK.makeHeaderTransitionCommitmentsProgram({
    ...roots,
    ...counts,
    validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    validationTraceCount: 0n,
  });
