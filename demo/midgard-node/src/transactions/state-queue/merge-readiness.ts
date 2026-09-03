import { MIDGARD_CONSENSUS_PROFILE_V1 } from "@al-ft/midgard-core/consensus-profile-v1";
import * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";

import {
  SUBMIT_SLOT_LENGTH_MS,
  SUBMIT_SLOT_VALIDITY_BUFFER,
  type SubmitSlotSnapshot,
} from "../../local-ledger-slot.js";
import { alignedUnixTimeStrictlyAfter } from "../../workers/utils/commit-end-time.js";
import { type InlineWaitPolicy, planSubmitTiming } from "../submit-timing.js";
import type { SubmitTimingNotDuePlanWithDueWorkEvidence } from "../submit-timing-due-work.js";

export const DEFAULT_MIN_QUEUE_LENGTH_FOR_MERGING = 8;

// Keep these values in one place so diagnostics and transaction construction
// agree on the exact maturity boundary.
export const STATE_QUEUE_MATURITY_DURATION_MS =
  MIDGARD_CONSENSUS_PROFILE_V1.limits.blockMaturityMs;
export const MERGE_MATURITY_DELAY_BUFFER_MS = 20_000;

export type MergeReadinessStatus =
  | "ready"
  | "tail_eligible_final_merge"
  | "no_queued_block"
  | "skipped_unresolved_commitment"
  | "skipped_local_finalization_pending"
  | "skipped_reset_in_progress"
  | "skipped_below_min_queue_length"
  | "skipped_pending_local_work"
  | "skipped_oldest_block_unattested"
  | "skipped_oldest_block_not_mature"
  | "skipped_oldest_block_local_ledger_not_ready"
  | "skipped_merge_candidate_changed";

export type MergePreflightInput = {
  readonly force: boolean;
  readonly queueLength: number;
  readonly minQueueLength: number;
  readonly unresolvedSubmittedBlockTxHash: string;
  readonly localFinalizationPending: boolean;
  readonly resetInProgress: boolean;
  readonly durableAdmissionBacklog: bigint;
  readonly mempoolTxCount: bigint;
  readonly unfinishedMutationJobs: bigint;
};

export type MergePreflightDecision = {
  readonly status: Exclude<
    MergeReadinessStatus,
    | "skipped_oldest_block_unattested"
    | "skipped_oldest_block_not_mature"
    | "skipped_merge_candidate_changed"
  >;
  readonly reason: string;
  readonly queueLength: number;
  readonly minQueueLength: number;
  readonly bypassQueueLengthGuard: boolean;
};

const formatIdleReason = (input: MergePreflightInput): string =>
  `queue_length=${input.queueLength.toString()},min_queue_length=${input.minQueueLength.toString()},durable_admission_backlog=${input.durableAdmissionBacklog.toString()},mempool_tx_count=${input.mempoolTxCount.toString()},unfinished_mutation_jobs=${input.unfinishedMutationJobs.toString()}`;

export const planMergePreflight = (
  input: MergePreflightInput,
): MergePreflightDecision => {
  if (input.localFinalizationPending) {
    return {
      status: "skipped_local_finalization_pending",
      reason: "local_finalization_pending=true",
      queueLength: input.queueLength,
      minQueueLength: input.minQueueLength,
      bypassQueueLengthGuard: false,
    };
  }
  if (input.unresolvedSubmittedBlockTxHash !== "") {
    return {
      status: "skipped_unresolved_commitment",
      reason: `submitted_tx=${input.unresolvedSubmittedBlockTxHash}`,
      queueLength: input.queueLength,
      minQueueLength: input.minQueueLength,
      bypassQueueLengthGuard: false,
    };
  }
  if (input.resetInProgress) {
    return {
      status: "skipped_reset_in_progress",
      reason: "reset_in_progress=true",
      queueLength: input.queueLength,
      minQueueLength: input.minQueueLength,
      bypassQueueLengthGuard: false,
    };
  }
  if (input.queueLength <= 0) {
    return {
      status: "no_queued_block",
      reason: `queue_length=${input.queueLength.toString()}`,
      queueLength: input.queueLength,
      minQueueLength: input.minQueueLength,
      bypassQueueLengthGuard: false,
    };
  }
  if (input.force) {
    return {
      status: "ready",
      reason: "force=true",
      queueLength: input.queueLength,
      minQueueLength: input.minQueueLength,
      bypassQueueLengthGuard: true,
    };
  }
  if (input.queueLength >= input.minQueueLength) {
    return {
      status: "ready",
      reason: formatIdleReason(input),
      queueLength: input.queueLength,
      minQueueLength: input.minQueueLength,
      bypassQueueLengthGuard: false,
    };
  }

  const localWorkCount =
    input.durableAdmissionBacklog +
    input.mempoolTxCount +
    input.unfinishedMutationJobs;
  if (input.queueLength === 1 && localWorkCount === 0n) {
    return {
      status: "tail_eligible_final_merge",
      reason: `final_tail_below_min_queue_length,${formatIdleReason(input)}`,
      queueLength: input.queueLength,
      minQueueLength: input.minQueueLength,
      bypassQueueLengthGuard: true,
    };
  }
  if (input.queueLength === 1) {
    return {
      status: "skipped_pending_local_work",
      reason: `final_tail_waiting_for_local_work,${formatIdleReason(input)}`,
      queueLength: input.queueLength,
      minQueueLength: input.minQueueLength,
      bypassQueueLengthGuard: false,
    };
  }
  return {
    status: "skipped_below_min_queue_length",
    reason: formatIdleReason(input),
    queueLength: input.queueLength,
    minQueueLength: input.minQueueLength,
    bypassQueueLengthGuard: false,
  };
};

export const mergeMaturityWindow = (
  lucid: LucidEvolution,
  blockEndTimeMs: number,
): {
  readonly validFromUnixTime: number;
  readonly readyAfterUnixTime: number;
} => {
  const maturityThresholdUnixTime =
    blockEndTimeMs + STATE_QUEUE_MATURITY_DURATION_MS;
  const validFromUnixTime = alignedUnixTimeStrictlyAfter(
    lucid,
    maturityThresholdUnixTime - 1,
  );
  return {
    validFromUnixTime,
    readyAfterUnixTime: validFromUnixTime + MERGE_MATURITY_DELAY_BUFFER_MS,
  };
};

export const MERGE_LOCAL_LEDGER_SLOT_BUFFER = SUBMIT_SLOT_VALIDITY_BUFFER;
export const DEFAULT_MERGE_LOCAL_LEDGER_MAX_WAIT_MS = 120_000;

export type MergeLocalLedgerGateInput = {
  readonly validFromSlot: number;
  readonly localLedgerSlot: number;
  readonly slotLengthMs?: number;
  readonly maxWaitMs?: number;
  readonly slotSource?: SubmitSlotSnapshot["source"];
  readonly observedAtMs?: number;
  readonly inlineWaitPolicy?: InlineWaitPolicy;
  readonly dependencyKey?: string;
  readonly invalidationKey?: string;
};

export type MergeLocalLedgerGateDecision =
  | {
      readonly status: "ready";
      readonly targetSlot: number;
      readonly localLedgerSlot: number;
      readonly deltaSlots: number;
      readonly waitMs: 0;
    }
  | {
      readonly status: "wait";
      readonly targetSlot: number;
      readonly localLedgerSlot: number;
      readonly deltaSlots: number;
      readonly waitMs: number;
    }
  | {
      readonly status: "retry_later";
      readonly targetSlot: number;
      readonly localLedgerSlot: number;
      readonly deltaSlots: number;
      readonly waitMs: number;
      readonly reason: string;
      readonly submitTimingNotDuePlan?: SubmitTimingNotDuePlanWithDueWorkEvidence;
    };

export const planMergeLocalLedgerGate = (
  input: MergeLocalLedgerGateInput,
): MergeLocalLedgerGateDecision => {
  const slotLengthMs = Math.max(
    1,
    Math.floor(input.slotLengthMs ?? SUBMIT_SLOT_LENGTH_MS),
  );
  const maxWaitMs = Math.max(
    0,
    Math.floor(input.maxWaitMs ?? DEFAULT_MERGE_LOCAL_LEDGER_MAX_WAIT_MS),
  );
  const snapshot: SubmitSlotSnapshot = {
    source: input.slotSource ?? "test",
    currentSlot: input.localLedgerSlot,
    observedAtMs: input.observedAtMs ?? Date.now(),
    slotLengthMs,
  };
  const plan = planSubmitTiming({
    callerLabel: "merge",
    invalidBeforeSlot: input.validFromSlot,
    slotSnapshot: snapshot,
    submitSlotBuffer: MERGE_LOCAL_LEDGER_SLOT_BUFFER,
    maxInlineWaitMs: maxWaitMs,
    inlineWaitPolicy: input.inlineWaitPolicy,
    dependencyKey: input.dependencyKey,
    invalidationKey: input.invalidationKey,
  });
  if (plan.status === "ready") {
    const targetSlot =
      plan.targetSlot ?? input.validFromSlot + MERGE_LOCAL_LEDGER_SLOT_BUFFER;
    const deltaSlots = Math.max(0, targetSlot - input.localLedgerSlot);
    return {
      status: "ready",
      targetSlot,
      localLedgerSlot: input.localLedgerSlot,
      deltaSlots,
      waitMs: 0,
    };
  }
  if (plan.status === "not_due") {
    const submitTimingNotDuePlan =
      plan.dependencyKey === undefined || plan.invalidationKey === undefined
        ? undefined
        : {
            ...plan,
            dependencyKey: plan.dependencyKey,
            invalidationKey: plan.invalidationKey,
          };
    return {
      status: "retry_later",
      targetSlot: plan.targetSlot,
      localLedgerSlot: input.localLedgerSlot,
      deltaSlots: plan.deltaSlots,
      waitMs: plan.waitMs,
      reason: `local_ledger_slot=${input.localLedgerSlot.toString()},target_slot=${plan.targetSlot.toString()},wait_ms=${plan.waitMs.toString()},max_wait_ms=${maxWaitMs.toString()}`,
      ...(submitTimingNotDuePlan === undefined
        ? {}
        : { submitTimingNotDuePlan }),
    };
  }
  if (plan.status !== "wait") {
    const targetSlot = input.validFromSlot + MERGE_LOCAL_LEDGER_SLOT_BUFFER;
    const deltaSlots = Math.max(0, targetSlot - input.localLedgerSlot);
    return {
      status: "retry_later",
      targetSlot,
      localLedgerSlot: input.localLedgerSlot,
      deltaSlots,
      waitMs: deltaSlots * slotLengthMs,
      reason: `submit_timing_status=${plan.status},local_ledger_slot=${input.localLedgerSlot.toString()},target_slot=${targetSlot.toString()}`,
    };
  }
  return {
    status: "wait",
    targetSlot: plan.targetSlot,
    localLedgerSlot: input.localLedgerSlot,
    deltaSlots: plan.deltaSlots,
    waitMs: plan.waitMs,
  };
};

export const planMergeLocalLedgerReadiness = planMergeLocalLedgerGate;

export type MergeSubmitValidityEvidenceInput = {
  readonly headerHash: string;
  readonly validFromSlot: number;
  readonly candidateIdentity?: string;
};

export type MergeSubmitValidityEvidence = {
  readonly key: string;
  readonly dependencyKey: string;
  readonly invalidationKey: string;
  readonly headerHash: string;
  readonly validFromSlot: number;
  readonly targetSlot: number;
  readonly candidateIdentity?: string;
};

export const mergeSubmitValidityEvidence = ({
  headerHash,
  validFromSlot,
  candidateIdentity,
}: MergeSubmitValidityEvidenceInput): MergeSubmitValidityEvidence => {
  const targetSlot = validFromSlot + MERGE_LOCAL_LEDGER_SLOT_BUFFER;
  const candidateKey = candidateIdentity ?? headerHash;
  const key = [
    "merge",
    candidateKey,
    validFromSlot.toString(),
    targetSlot.toString(),
  ].join(":");
  return {
    key,
    dependencyKey: key,
    invalidationKey: key,
    headerHash,
    validFromSlot,
    targetSlot,
    ...(candidateIdentity === undefined ? {} : { candidateIdentity }),
  };
};

export type OldestQueuedBlockReadinessInput = {
  readonly headerHash: string;
  readonly currentDaAvailability: SDK.DaAvailabilityStateQueueStatusV1;
  readonly readyAfterUnixTime: number;
  readonly nowUnixTime: number;
};

export type OldestQueuedBlockReadiness =
  | {
      readonly status: "ready";
      readonly headerHash: string;
      readonly reason: string;
      readonly readyAfterUnixTime: number;
      readonly nowUnixTime: number;
    }
  | {
      readonly status:
        | "skipped_oldest_block_unattested"
        | "skipped_oldest_block_not_mature";
      readonly headerHash: string;
      readonly reason: string;
      readonly readyAfterUnixTime: number;
      readonly nowUnixTime: number;
    };

export type MergeCandidateIdentityInput = {
  readonly firstBlockOutRef: string;
  readonly headerHash: string;
  readonly currentDaAvailability: SDK.DaAvailabilityStateQueueStatusV1;
  readonly readyAfterUnixTime: number;
};

export const mergeCandidateIdentity = (
  input: MergeCandidateIdentityInput,
): string =>
  [
    input.firstBlockOutRef,
    input.headerHash,
    SDK.daAvailabilityStateQueueStatusIdentityV1(input.currentDaAvailability),
    input.readyAfterUnixTime.toString(),
  ].join("|");

export type OldestQueuedBlockCandidateReadinessInput =
  OldestQueuedBlockReadinessInput & {
    readonly firstBlockOutRef: string;
    readonly validFromUnixTime: number;
  };

export type OldestQueuedBlockCandidateReadiness = OldestQueuedBlockReadiness & {
  readonly firstBlockOutRef: string;
  readonly candidateIdentity: string;
  readonly currentDaAvailability: SDK.DaAvailabilityStateQueueStatusV1;
  readonly validFromUnixTime: number;
};

export const classifyOldestQueuedBlockReadiness = (
  input: OldestQueuedBlockReadinessInput,
): OldestQueuedBlockReadiness => {
  if (
    !SDK.daAvailabilityStateQueueStatusPermitsMergeV1(
      input.currentDaAvailability,
    )
  ) {
    return {
      status: "skipped_oldest_block_unattested",
      headerHash: input.headerHash,
      reason: `header=${input.headerHash},current_da_availability=${SDK.daAvailabilityStateQueueStatusIdentityV1(input.currentDaAvailability)},required_da_availability=Attested|Published`,
      readyAfterUnixTime: input.readyAfterUnixTime,
      nowUnixTime: input.nowUnixTime,
    };
  }
  if (input.nowUnixTime < input.readyAfterUnixTime) {
    return {
      status: "skipped_oldest_block_not_mature",
      headerHash: input.headerHash,
      reason: `header=${input.headerHash},ready_after=${input.readyAfterUnixTime.toString()},now=${input.nowUnixTime.toString()}`,
      readyAfterUnixTime: input.readyAfterUnixTime,
      nowUnixTime: input.nowUnixTime,
    };
  }
  return {
    status: "ready",
    headerHash: input.headerHash,
    reason: `header=${input.headerHash}`,
    readyAfterUnixTime: input.readyAfterUnixTime,
    nowUnixTime: input.nowUnixTime,
  };
};

export const classifyOldestQueuedBlockCandidateReadiness = (
  input: OldestQueuedBlockCandidateReadinessInput,
): OldestQueuedBlockCandidateReadiness => {
  const readiness = classifyOldestQueuedBlockReadiness(input);
  return {
    ...readiness,
    firstBlockOutRef: input.firstBlockOutRef,
    candidateIdentity: mergeCandidateIdentity(input),
    currentDaAvailability: input.currentDaAvailability,
    validFromUnixTime: input.validFromUnixTime,
  };
};
