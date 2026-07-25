import * as SDK from "@al-ft/midgard-sdk";

type EventCommitmentRoots = Pick<
  SDK.MakeHeaderTransitionCommitmentsV1Input,
  | "withdrawalsRoot"
  | "forcedTransactionsRoot"
  | "transactionsRoot"
  | "depositsRoot"
  | "transitionTraceRoot"
  | "eventToStepRoot"
>;

type EventCommitmentCounts = Pick<
  SDK.MakeHeaderTransitionCommitmentsV1Input,
  | "withdrawalCount"
  | "forcedTransactionCount"
  | "l2TransactionCount"
  | "depositCount"
  | "transitionStepCount"
>;

export type ValidationCommitments = Pick<
  SDK.MakeHeaderTransitionCommitmentsV1Input,
  "validationTracesRoot" | "validationTraceCount"
>;

export const makeEventCommitments = (
  roots: EventCommitmentRoots,
  counts: EventCommitmentCounts,
  validation: ValidationCommitments,
) =>
  SDK.makeHeaderTransitionCommitmentsV1Program({
    ...roots,
    ...counts,
    ...validation,
  });
