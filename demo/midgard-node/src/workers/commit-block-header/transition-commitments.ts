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

export type ValidationCommitments = Pick<
  SDK.MakeHeaderTransitionCommitmentsInput,
  "validationTracesRoot" | "validationTraceCount"
>;

export const makeEventCommitments = (
  roots: EventCommitmentRoots,
  counts: EventCommitmentCounts,
  validation: ValidationCommitments,
) =>
  SDK.makeHeaderTransitionCommitmentsProgram({
    ...roots,
    ...counts,
    ...validation,
  });
