import { Effect } from "effect";

import { PendingBlockFinalizationsDB } from "../database/index.js";
import { HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS } from "../services/history-commit-window.js";
import {
  type ContractDeploymentIdentityValue,
  Lucid,
  MidgardContracts,
} from "../services/index.js";
import { pendingCommitEventInstants } from "./commit-block-header.pending-event-instants.js";
import { resolveCommitAppendFenceEndTimeCapLocal } from "./commit-block-header/state-queue.js";
import {
  type CommitEventLimits,
  planCommitEventWindow,
} from "./utils/commit-block-planner.event-window.js";
import { resolveHistoryCommitBlockTime } from "./utils/commit-end-time.js";

/** The four protocol event caps used by the grouped window planner. */
export const commitEventLimitsFromConsensusProfile = (
  profile: ContractDeploymentIdentityValue["consensusProfile"],
): CommitEventLimits => ({
  deposits: profile.limits.maxDepositCount,
  forcedTransactions: profile.limits.maxForcedTransactionCount,
  withdrawals: profile.limits.maxWithdrawalCount,
  totalEvents: profile.limits.maxTotalEventCount,
});

/** One source-owned clock, stable horizon and complete event grouping for every retry. */
export const planHistoryCommitEventWindow = (input: {
  readonly latestEndTime: number;
  readonly nowMs: number;
  readonly horizonMs: number;
  readonly schedulerWindowStartTimeMs?: number;
  readonly schedulerWindowEndTimeMs?: number;
  readonly limits: CommitEventLimits;
}) =>
  Effect.gen(function* () {
    const lucid = yield* Lucid;
    const contracts = yield* MidgardContracts;
    const submitSlot = yield* lucid.submitSlotSnapshot();
    yield* PendingBlockFinalizationsDB.assertNoUnreconciledSignedSubmission;
    const appendFenceEndTimeMs = yield* resolveCommitAppendFenceEndTimeCapLocal(
      lucid.api,
      {
        stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
        stateQueuePolicyId: contracts.stateQueue.policyId,
      },
    );
    const clockFit = resolveHistoryCommitBlockTime({
      lucid: lucid.api,
      currentSlot: submitSlot.currentSlot,
      latestEndTime: input.latestEndTime,
      nowMs: input.nowMs,
      minimumFutureBufferMs: HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS,
      schedulerWindowStartTimeMs: input.schedulerWindowStartTimeMs,
      schedulerWindowEndTimeMs: input.schedulerWindowEndTimeMs,
      appendFenceEndTimeMs,
      coverageHorizonMs: input.horizonMs,
    });
    if (clockFit.status !== "fits")
      return { kind: "clock_hold" as const, clockFit };
    const blockTimeMs = clockFit.resolvedEndTime - 1;
    const groups = yield* pendingCommitEventInstants(
      new Date(Math.min(input.horizonMs, blockTimeMs)),
      input.limits,
    );
    const window = planCommitEventWindow({
      groups,
      startTimeMs: input.latestEndTime,
      horizonMs: input.horizonMs,
      blockTimeMs,
      limits: input.limits,
    });
    return { kind: "selected" as const, clockFit, blockTimeMs, groups, window };
  });
