import * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";

import {
  Globals,
  Lucid,
  withL1ControlPlaneWaitTimeout,
} from "../services/index.js";
import { type StateQueueSnapshot } from "../services/landed-state-queue.js";
import { type MergeReadinessStatus } from "../transactions/state-queue/merge-readiness.js";
import {
  type CanonicalMergeCandidateReadiness,
  type ConfirmedMergeFinalization,
  mergeSemanticSkipResult,
} from "../transactions/state-queue/merge-to-confirmed-state.js";
import {
  checkSlotAwareDueWork,
  clearSlotAwareDueWork,
  listSlotAwareDueWork,
} from "./slot-aware-due-work.js";

/**
 * Background merge flow for confirmed state-queue blocks.
 *
 * The merge fiber switches to the dedicated merge wallet and submits the
 * on-chain merge transaction that folds confirmed queue state into the next
 * durable checkpoint.
 */

export type MergeActionResult =
  | {
      readonly status: "merged";
      readonly postMergeSnapshot: StateQueueSnapshot;
      readonly headerHash: string;
      readonly txHash: string;
      readonly trigger: "threshold" | "manual" | "final_tail_auto_merge";
    }
  | {
      readonly status:
        | Exclude<MergeReadinessStatus, "ready">
        | "skipped_state_queue_lease_busy"
        | "skipped_l1_control_plane_busy";
      readonly reason: string;
      readonly headerHash?: string;
      readonly queueLength?: number;
      readonly minQueueLength?: number;
      readonly readyAfterUnixTime?: number;
      readonly nowUnixTime?: number;
    };

type CurrentMergeDueWorkEvidence = {
  readonly key: string;
  readonly dependencyKey: string;
  readonly invalidationKey: string;
  readonly headerHash: string;
  readonly validFromSlot: number;
  readonly targetSlot: number;
};

export const SCHEDULED_MERGE_CONTROL_PLANE_WAIT_MS = 30_000;

/** How long one merge attempt may hold the L1 control plane. */
export const MERGE_L1_CONTROL_PLANE_MAX_HOLD_MS = 180_000;

/**
 * The part of the hold kept for the local finalization after an L1
 * confirmation: the confirmation wait gives up this long before the hold
 * ends, so a confirmed merge is finalized within its attempt.
 */
export const MERGE_LOCAL_FINALIZATION_RESERVE_MS = 30_000;

export const withScheduledMergeControlPlaneWait = <A, E, R>({
  globals,
  effect,
  waitTimeoutMs = SCHEDULED_MERGE_CONTROL_PLANE_WAIT_MS,
}: {
  readonly globals: Globals;
  readonly effect: Effect.Effect<A, E, R>;
  readonly waitTimeoutMs?: number;
}): Effect.Effect<Option.Option<A>, E | Error, R> =>
  withL1ControlPlaneWaitTimeout(
    globals,
    {
      scope: "state_queue_merge",
      waitTimeoutMs,
      maxHoldMs: MERGE_L1_CONTROL_PLANE_MAX_HOLD_MS,
    },
    effect,
  );

export const registeredMergeDueWorkSkip = (
  currentEvidence: CurrentMergeDueWorkEvidence,
): Effect.Effect<MergeActionResult | undefined, never, Lucid> =>
  Effect.gen(function* () {
    const entries = listSlotAwareDueWork().filter(
      (entry) => entry.kind === "merge_submit_validity",
    );
    if (entries.length === 0) {
      return undefined;
    }
    const lucid = yield* Lucid;
    const slotSnapshot = yield* Effect.either(lucid.submitSlotSnapshot());
    for (const entry of entries) {
      if (entry.key !== currentEvidence.key) {
        clearSlotAwareDueWork(entry.kind, entry.key);
        yield* Effect.logInfo(
          `🔸 Clearing stale merge due work before re-plan (key=${entry.key},current_key=${currentEvidence.key},header=${currentEvidence.headerHash},valid_from_slot=${currentEvidence.validFromSlot.toString()},target_slot=${currentEvidence.targetSlot.toString()}).`,
        );
        continue;
      }
      const decision = checkSlotAwareDueWork({
        kind: entry.kind,
        key: entry.key,
        currentSlot:
          slotSnapshot._tag === "Right"
            ? slotSnapshot.right.currentSlot
            : undefined,
        dependencyKey: currentEvidence.dependencyKey,
        invalidationKey: currentEvidence.invalidationKey,
      });
      switch (decision.status) {
        case "skip": {
          const reason = `merge_due_work_not_due,key=${entry.key},current_slot=${decision.currentSlot.toString()},due_slot=${entry.dueSlot.toString()},wait_ms=${entry.waitMs.toString()}`;
          yield* Effect.logInfo(`🔸 Skipping merge (${reason}).`);
          return {
            status: "skipped_oldest_block_local_ledger_not_ready",
            reason,
          } satisfies MergeActionResult;
        }
        case "due":
          yield* Effect.logInfo(
            `🔸 Waking merge due work (key=${entry.key},current_slot=${decision.currentSlot.toString()},due_slot=${entry.dueSlot.toString()}).`,
          );
          break;
        case "invalidated":
          yield* Effect.logInfo(
            `🔸 Clearing merge due work before re-plan (key=${entry.key},reason=${decision.reason}).`,
          );
          break;
        case "missing":
          break;
      }
    }
    return undefined;
  });

type SemanticCandidate = Extract<
  CanonicalMergeCandidateReadiness,
  { readonly status: "candidate" }
>;

type SemanticCandidateSkip = Exclude<
  SemanticCandidate["readiness"],
  { readonly status: "ready" }
>;

type MergeCandidateChangedResult = {
  readonly status: "skipped_merge_candidate_changed";
  readonly reason: string;
  readonly headerHash?: string;
  readonly readyAfterUnixTime?: number;
  readonly nowUnixTime?: number;
};

export const logSemanticSkip = (
  phase: "before lease" | "after leased recheck",
  readiness: SemanticCandidateSkip,
): Effect.Effect<void> => {
  const message =
    readiness.status === "skipped_oldest_block_unattested"
      ? "oldest block is not DA-attested yet"
      : readiness.status === "skipped_oldest_block_proven_fraud"
        ? "oldest block has completed fraud and requires state correction"
        : "oldest block is not mature yet";
  return Effect.logInfo(
    `🔸 Skipping merge ${phase} because ${message} (${readiness.reason}).`,
  );
};

export const mergeActionSemanticSkipResult = (
  readiness: SemanticCandidateSkip,
): MergeActionResult => mergeSemanticSkipResult(readiness) as MergeActionResult;

export const mergeValidFromSlot = (
  lucid: LucidEvolution,
  validFromUnixTime: number,
): Effect.Effect<number, SDK.StateQueueError> =>
  Effect.try({
    try: () => {
      const slot = Number(lucid.unixTimeToSlot(validFromUnixTime));
      if (!Number.isSafeInteger(slot) || slot < 0) {
        throw new Error(`invalid slot=${slot.toString()}`);
      }
      return slot;
    },
    catch: (cause) =>
      new SDK.StateQueueError({
        message: "Failed to convert merge pre-lease valid-from time to a slot",
        cause,
      }),
  });

export const changedCandidateResult = ({
  preLeaseCandidate,
  leasedCandidate,
}: {
  readonly preLeaseCandidate: SemanticCandidate;
  readonly leasedCandidate: CanonicalMergeCandidateReadiness;
}): MergeCandidateChangedResult => {
  const preLeaseIdentity = preLeaseCandidate.readiness.candidateIdentity;
  const leasedIdentity =
    leasedCandidate.status === "candidate"
      ? leasedCandidate.readiness.candidateIdentity
      : leasedCandidate.reason;
  const readiness =
    leasedCandidate.status === "candidate"
      ? leasedCandidate.readiness
      : preLeaseCandidate.readiness;
  return {
    status: "skipped_merge_candidate_changed",
    reason: `preflight_candidate=${preLeaseIdentity},leased_candidate=${leasedIdentity}`,
    headerHash: readiness.headerHash,
    readyAfterUnixTime: readiness.readyAfterUnixTime,
    nowUnixTime: readiness.nowUnixTime,
  };
};

/**
 * Runs one merge attempt, optionally bypassing the queue-length guard for
 * explicit recovery/administrative flows.
 */
export type MergeTrigger = Extract<
  MergeActionResult,
  { readonly status: "merged" }
>["trigger"];

/** A merge confirmed on L1 during this attempt, with its local finalization's
 * exit, recorded even if the attempt was interrupted afterwards. */
export type ConfirmedMerge = ConfirmedMergeFinalization & {
  readonly trigger: MergeTrigger;
};
