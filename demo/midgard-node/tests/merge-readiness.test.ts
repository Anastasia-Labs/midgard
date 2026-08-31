import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

const PUBLISHED = {
  Published: { terminal_commitment: "22".repeat(32) },
} satisfies SDK.DaAvailabilityStateQueueStatusV1;
const ATTESTED = {
  Attested: { da_bond_asset_name: "33".repeat(32) },
} satisfies SDK.DaAvailabilityStateQueueStatusV1;
const CHALLENGED = {
  Challenged: {
    da_bond_asset_name: "33".repeat(32),
    challenge_asset_name: "44".repeat(32),
  },
} satisfies SDK.DaAvailabilityStateQueueStatusV1;

import {
  classifyOldestQueuedBlockCandidateReadiness,
  classifyOldestQueuedBlockReadiness,
  mergeCandidateIdentity,
  mergeSubmitValidityEvidence,
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
        currentDaAvailability: SDK.NO_DA_ATTESTATION,
        readyAfterUnixTime: 200,
        nowUnixTime: 250,
      }),
    ).toMatchObject({
      status: "skipped_oldest_block_unattested",
    });
    expect(
      classifyOldestQueuedBlockReadiness({
        headerHash: "11".repeat(28),
        currentDaAvailability: CHALLENGED,
        readyAfterUnixTime: 200,
        nowUnixTime: 250,
      }),
    ).toMatchObject({
      status: "skipped_oldest_block_unattested",
    });
    expect(
      classifyOldestQueuedBlockReadiness({
        headerHash: "11".repeat(28),
        currentDaAvailability: ATTESTED,
        readyAfterUnixTime: 300,
        nowUnixTime: 300,
      }),
    ).toMatchObject({
      status: "ready",
    });
    expect(
      classifyOldestQueuedBlockReadiness({
        headerHash: "11".repeat(28),
        currentDaAvailability: PUBLISHED,
        readyAfterUnixTime: 300,
        nowUnixTime: 250,
      }),
    ).toMatchObject({
      status: "skipped_oldest_block_not_mature",
    });
    expect(
      classifyOldestQueuedBlockReadiness({
        headerHash: "11".repeat(28),
        currentDaAvailability: PUBLISHED,
        readyAfterUnixTime: 300,
        nowUnixTime: 300,
      }),
    ).toMatchObject({
      status: "ready",
    });
  });

  it("derives a stable merge candidate identity from semantic readiness evidence", () => {
    const baseIdentityInput = {
      firstBlockOutRef: `${"aa".repeat(32)}#0`,
      headerHash: "11".repeat(28),
      currentDaAvailability: PUBLISHED,
      readyAfterUnixTime: 300,
    };

    expect(mergeCandidateIdentity(baseIdentityInput)).toBe(
      [
        baseIdentityInput.firstBlockOutRef,
        baseIdentityInput.headerHash,
        SDK.daAvailabilityStateQueueStatusIdentityV1(
          baseIdentityInput.currentDaAvailability,
        ),
        baseIdentityInput.readyAfterUnixTime.toString(),
      ].join("|"),
    );

    const variants = [
      { firstBlockOutRef: `${"bb".repeat(32)}#0` },
      { headerHash: "44".repeat(28) },
      { currentDaAvailability: SDK.NO_DA_ATTESTATION },
      {
        currentDaAvailability: {
          Published: { terminal_commitment: "66".repeat(32) },
        },
      },
      { readyAfterUnixTime: 301 },
    ];
    for (const variant of variants) {
      expect(
        mergeCandidateIdentity({
          ...baseIdentityInput,
          ...variant,
        }),
      ).not.toBe(mergeCandidateIdentity(baseIdentityInput));
    }
  });

  it("carries candidate identity and validity evidence with semantic readiness", () => {
    expect(
      classifyOldestQueuedBlockCandidateReadiness({
        firstBlockOutRef: `${"aa".repeat(32)}#0`,
        headerHash: "11".repeat(28),
        currentDaAvailability: PUBLISHED,
        validFromUnixTime: 280,
        readyAfterUnixTime: 300,
        nowUnixTime: 250,
      }),
    ).toMatchObject({
      status: "skipped_oldest_block_not_mature",
      firstBlockOutRef: `${"aa".repeat(32)}#0`,
      candidateIdentity: [
        `${"aa".repeat(32)}#0`,
        "11".repeat(28),
        SDK.daAvailabilityStateQueueStatusIdentityV1(PUBLISHED),
        "300",
      ].join("|"),
      validFromUnixTime: 280,
      readyAfterUnixTime: 300,
      nowUnixTime: 250,
    });
  });

  it("keeps semantic oldest-block readiness independent from force batching", () => {
    expect(
      planMergePreflight({
        ...baseInput,
        force: true,
      }),
    ).toMatchObject({
      status: "ready",
      bypassQueueLengthGuard: true,
    });
    expect(
      classifyOldestQueuedBlockCandidateReadiness({
        firstBlockOutRef: `${"aa".repeat(32)}#0`,
        headerHash: "11".repeat(28),
        currentDaAvailability: PUBLISHED,
        validFromUnixTime: 280,
        readyAfterUnixTime: 300,
        nowUnixTime: 250,
      }),
    ).toMatchObject({
      status: "skipped_oldest_block_not_mature",
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

  it("retry-laters every positive local-ledger wait in no-inline mode", () => {
    const evidence = mergeSubmitValidityEvidence({
      headerHash: "11".repeat(28),
      candidateIdentity: "candidate-a",
      validFromSlot: 126544954,
    });

    const decision = planMergeLocalLedgerReadiness({
      validFromSlot: evidence.validFromSlot,
      localLedgerSlot: 126544955,
      maxWaitMs: 120_000,
      inlineWaitPolicy: "defer_positive_wait",
      dependencyKey: evidence.dependencyKey,
      invalidationKey: evidence.invalidationKey,
    });

    expect(decision).toMatchObject({
      status: "retry_later",
      targetSlot: 126544956,
      deltaSlots: 1,
      waitMs: 1_000,
      submitTimingNotDuePlan: {
        status: "not_due",
        reason: "inline_wait_policy=defer_positive_wait,wait_ms=1000",
        dependencyKey: evidence.dependencyKey,
        invalidationKey: evidence.invalidationKey,
      },
    });
  });

  it("derives merge submit-validity evidence from candidate identity and slots", () => {
    expect(
      mergeSubmitValidityEvidence({
        headerHash: "11".repeat(28),
        candidateIdentity: "candidate-a",
        validFromSlot: 126544954,
      }),
    ).toEqual({
      key: "merge:candidate-a:126544954:126544956",
      dependencyKey: "merge:candidate-a:126544954:126544956",
      invalidationKey: "merge:candidate-a:126544954:126544956",
      headerHash: "11".repeat(28),
      validFromSlot: 126544954,
      targetSlot: 126544956,
      candidateIdentity: "candidate-a",
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
