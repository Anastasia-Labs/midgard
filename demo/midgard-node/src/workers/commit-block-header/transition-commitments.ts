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

export const makeEventCommitments = (
  roots: EventCommitmentRoots,
  counts: EventCommitmentCounts,
) => SDK.makeHeaderTransitionCommitmentsProgram({ ...roots, ...counts });
