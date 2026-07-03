import { describe, expect, it } from "vitest";

import {
  classifyOldestQueuedBlockReadiness,
  planMergeLocalLedgerReadiness,
  planMergePreflight,
} from "@/transactions/state-queue/merge-readiness.js";

const baseInput = {
  force: false,
  queueLength: 1,
  minQueueLength: 8,
  unresolvedSubmittedBlockTxHash: "",
  localFinalizationPending: false,
  resetInProgress: false,
  durableAdmissionBacklog: 0n,
  mempoolTxCount: 0n,
  unfinishedMutationJobs: 0n,
};

describe("merge readiness planner", () => {
  it("bypasses only the queue-length guard for an idle final tail", () => {
    expect(planMergePreflight(baseInput)).toMatchObject({
      status: "tail_eligible_final_merge",
      bypassQueueLengthGuard: true,
      queueLength: 1,
      minQueueLength: 8,
    });
  });

  it("keeps normal batching behavior when local work is pending", () => {
    expect(
      planMergePreflight({
        ...baseInput,
        durableAdmissionBacklog: 1n,
      }),
    ).toMatchObject({
      status: "skipped_pending_local_work",
      bypassQueueLengthGuard: false,
    });
    expect(
      planMergePreflight({
        ...baseInput,
        mempoolTxCount: 1n,
      }),
    ).toMatchObject({
      status: "skipped_pending_local_work",
      bypassQueueLengthGuard: false,
    });
  });

  it("does not bypass unresolved commitment or local finalization guards", () => {
    expect(
      planMergePreflight({
        ...baseInput,
        unresolvedSubmittedBlockTxHash: "aa".repeat(32),
      }),
    ).toMatchObject({
      status: "skipped_unresolved_commitment",
      bypassQueueLengthGuard: false,
    });
    expect(
      planMergePreflight({
        ...baseInput,
        localFinalizationPending: true,
      }),
    ).toMatchObject({
      status: "skipped_local_finalization_pending",
      bypassQueueLengthGuard: false,
    });
  });

  it("uses the normal threshold path for batched queues", () => {
    expect(
      planMergePreflight({
        ...baseInput,
        queueLength: 8,
      }),
    ).toMatchObject({
      status: "ready",
      bypassQueueLengthGuard: false,
    });
  });

  it("classifies oldest-block DA and maturity as hard checks", () => {
    expect(
      classifyOldestQueuedBlockReadiness({
        headerHash: "11".repeat(28),
        currentDaAttestation: "00".repeat(28),
        requiredDaAttestation: "22".repeat(28),
        readyAfterUnixTime: 200,
        nowUnixTime: 250,
      }),
    ).toMatchObject({
      status: "skipped_oldest_block_unattested",
    });
    expect(
      classifyOldestQueuedBlockReadiness({
        headerHash: "11".repeat(28),
        currentDaAttestation: "22".repeat(28),
        requiredDaAttestation: "22".repeat(28),
        readyAfterUnixTime: 300,
        nowUnixTime: 250,
      }),
    ).toMatchObject({
      status: "skipped_oldest_block_not_mature",
    });
    expect(
      classifyOldestQueuedBlockReadiness({
        headerHash: "11".repeat(28),
        currentDaAttestation: "22".repeat(28),
        requiredDaAttestation: "22".repeat(28),
        readyAfterUnixTime: 300,
        nowUnixTime: 300,
      }),
    ).toMatchObject({
      status: "ready",
    });
  });

  it("retry-laters before volatile merge inputs when local ledger lags validFrom plus buffer", () => {
    expect(
      planMergeLocalLedgerReadiness({
        validFromSlot: 126544954,
        localLedgerSlot: 126544938,
        maxWaitMs: 10_000,
      }),
    ).toMatchObject({
      status: "retry_later",
      targetSlot: 126544956,
      deltaSlots: 18,
      waitMs: 18_000,
    });
  });

  it("carries raw not-due submit timing evidence when dependency keys are provided", () => {
    const decision = planMergeLocalLedgerReadiness({
      validFromSlot: 126544954,
      localLedgerSlot: 126544938,
      maxWaitMs: 10_000,
      dependencyKey: "merge:header:126544954",
      invalidationKey: "merge:header:126544954",
    });

    expect(decision).toMatchObject({
      status: "retry_later",
      submitTimingNotDuePlan: {
        status: "not_due",
        callerLabel: "merge",
        currentSlot: 126544938,
        targetSlot: 126544956,
        waitMs: 18_000,
        slotSource: "test",
        dependencyKey: "merge:header:126544954",
        invalidationKey: "merge:header:126544954",
      },
    });
  });

  it("allows merge pre-build once the local submit ledger reaches the margin", () => {
    expect(
      planMergeLocalLedgerReadiness({
        validFromSlot: 126544954,
        localLedgerSlot: 126544956,
        maxWaitMs: 10_000,
      }),
    ).toMatchObject({
      status: "ready",
      targetSlot: 126544956,
      deltaSlots: 0,
      waitMs: 0,
    });
  });
});
