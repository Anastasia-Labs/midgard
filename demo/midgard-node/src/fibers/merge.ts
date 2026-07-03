import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Ref, Schedule } from "effect";

import {
  MempoolDB,
  MutationJobsDB,
  StateQueueMutationLeasesDB,
  TxAdmissionsDB,
} from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import {
  Database,
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "@/services/index.js";
import {
  fetchStateQueueSnapshotProgram,
  refreshStateQueueGlobalsFromSnapshot,
  type StateQueueSnapshot,
} from "@/services/state-queue-topology.js";
import {
  DEFAULT_MIN_QUEUE_LENGTH_FOR_MERGING,
  type MergeReadinessStatus,
  planMergePreflight,
} from "@/transactions/state-queue/merge-readiness.js";
import { buildAndSubmitMergeTx } from "@/transactions/state-queue/merge-to-confirmed-state.js";
import {
  TxConfirmError,
  TxSignError,
  TxSubmitError,
} from "@/transactions/utils.js";
import {
  checkSlotAwareDueWork,
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
        | "skipped_state_queue_lease_busy";
      readonly reason: string;
      readonly headerHash?: string;
      readonly queueLength?: number;
      readonly minQueueLength?: number;
      readonly readyAfterUnixTime?: number;
      readonly nowUnixTime?: number;
    };

const registeredMergeDueWorkSkip: Effect.Effect<
  MergeActionResult | undefined,
  never,
  Lucid
> = Effect.gen(function* () {
  const entries = listSlotAwareDueWork().filter(
    (entry) => entry.kind === "merge_submit_validity",
  );
  if (entries.length === 0) {
    return undefined;
  }
  const lucid = yield* Lucid;
  const slotSnapshot = yield* Effect.either(lucid.submitSlotSnapshot());
  for (const entry of entries) {
    const decision = checkSlotAwareDueWork({
      kind: entry.kind,
      key: entry.key,
      currentSlot:
        slotSnapshot._tag === "Right"
          ? slotSnapshot.right.currentSlot
          : undefined,
      dependencyKey: entry.dependencyKey,
      invalidationKey: entry.invalidationKey,
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

/**
 * Runs one merge attempt, optionally bypassing the queue-length guard for
 * explicit recovery/administrative flows.
 */
export const mergeAction = (
  force: boolean = false,
): Effect.Effect<
  MergeActionResult,
  | SDK.CmlDeserializationError
  | SDK.DataCoercionError
  | SDK.HashingError
  | SDK.LinkedListError
  | SDK.LucidError
  | SDK.StateQueueError
  | SDK.CmlUnexpectedError
  | SDK.CborSerializationError
  | DatabaseError
  | TxConfirmError
  | TxSubmitError
  | TxSignError,
  Lucid | MidgardContracts | Database | Globals | NodeConfig
> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const nodeConfig = yield* NodeConfig;
    yield* Ref.set(globals.HEARTBEAT_MERGE, Date.now());
    const [
      initialUnconfirmedSubmittedBlockTxHash,
      initialLocalFinalizationPending,
      initialResetInProgress,
    ] = yield* Effect.all(
      [
        Ref.get(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH),
        Ref.get(globals.LOCAL_FINALIZATION_PENDING),
        Ref.get(globals.RESET_IN_PROGRESS),
      ],
      { concurrency: "unbounded" },
    );
    if (initialLocalFinalizationPending) {
      const reason = "local_finalization_pending=true";
      yield* Effect.logInfo(`🔸 Skipping merge (${reason}).`);
      return {
        status: "skipped_local_finalization_pending",
        reason,
      } satisfies MergeActionResult;
    }
    if (initialUnconfirmedSubmittedBlockTxHash !== "") {
      const reason = `submitted_tx=${initialUnconfirmedSubmittedBlockTxHash}`;
      yield* Effect.logInfo(`🔸 Skipping merge (${reason}).`);
      return {
        status: "skipped_unresolved_commitment",
        reason,
      } satisfies MergeActionResult;
    }
    if (initialResetInProgress) {
      const reason = "reset_in_progress=true";
      yield* Effect.logInfo(`🔸 Skipping merge (${reason}).`);
      return {
        status: "skipped_reset_in_progress",
        reason,
      } satisfies MergeActionResult;
    }
    const lucid = yield* Lucid;
    const contracts = yield* MidgardContracts;
    const { stateQueue: stateQueueAuthValidator } = contracts;

    const fetchConfig: SDK.StateQueueFetchConfig = {
      stateQueueAddress: stateQueueAuthValidator.spendingScriptAddress,
      stateQueuePolicyId: stateQueueAuthValidator.policyId,
    };
    const mergeDueWorkSkip = yield* registeredMergeDueWorkSkip;
    if (mergeDueWorkSkip !== undefined) {
      return mergeDueWorkSkip;
    }
    const leaseResult = yield* StateQueueMutationLeasesDB.tryWithLease(
      "state_queue_merge",
      (leaseToken) =>
        Effect.gen(function* () {
          const preMergeSnapshot = yield* fetchStateQueueSnapshotProgram(
            lucid.api,
            stateQueueAuthValidator,
            "manual_status",
          );
          yield* refreshStateQueueGlobalsFromSnapshot(
            globals,
            preMergeSnapshot,
          );
          const [
            unconfirmedSubmittedBlockTxHash,
            localFinalizationPending,
            resetInProgress,
            durableAdmissionBacklog,
            mempoolTxCount,
            unfinishedMutationJobs,
          ] = yield* Effect.all(
            [
              Ref.get(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH),
              Ref.get(globals.LOCAL_FINALIZATION_PENDING),
              Ref.get(globals.RESET_IN_PROGRESS),
              TxAdmissionsDB.countBacklog,
              MempoolDB.retrieveTxCount,
              MutationJobsDB.countUnfinished,
            ],
            { concurrency: "unbounded" },
          );
          const queueLength = Math.max(
            0,
            preMergeSnapshot.topology.parsedNodeCount - 1,
          );
          const minQueueLength =
            nodeConfig.MIN_QUEUE_LENGTH_FOR_MERGING ??
            DEFAULT_MIN_QUEUE_LENGTH_FOR_MERGING;
          const preflight = planMergePreflight({
            force,
            queueLength,
            minQueueLength,
            unresolvedSubmittedBlockTxHash: unconfirmedSubmittedBlockTxHash,
            localFinalizationPending,
            resetInProgress,
            durableAdmissionBacklog,
            mempoolTxCount,
            unfinishedMutationJobs,
          });
          if (preflight.status !== "ready") {
            if (preflight.status === "tail_eligible_final_merge") {
              yield* Effect.logInfo(
                `🔸 Auto-merging mature final tail below batch threshold if hard merge checks pass (${preflight.reason}).`,
              );
            } else {
              yield* Effect.logInfo(
                `🔸 Skipping merge (${preflight.status}; ${preflight.reason}).`,
              );
              return {
                status: preflight.status,
                reason: preflight.reason,
                queueLength: preflight.queueLength,
                minQueueLength: preflight.minQueueLength,
              } satisfies MergeActionResult;
            }
          }
          yield* lucid.switchToOperatorsMergingWallet;
          yield* StateQueueMutationLeasesDB.revalidate(leaseToken);
          const mergeTxResult = yield* buildAndSubmitMergeTx(
            lucid.api,
            fetchConfig,
            contracts,
            {
              bypassQueueLengthGuard: preflight.bypassQueueLengthGuard,
              leaseToken,
              revalidateMutationLease: () =>
                StateQueueMutationLeasesDB.revalidate(
                  leaseToken,
                ) as Effect.Effect<void, unknown, never>,
              referenceScriptsAddress: lucid.referenceScriptsAddress,
            },
          );
          if (mergeTxResult.status !== "merged") {
            return {
              status: mergeTxResult.status,
              reason: mergeTxResult.reason,
              ...(mergeTxResult.headerHash === undefined
                ? {}
                : { headerHash: mergeTxResult.headerHash }),
              ...(mergeTxResult.queueLength === undefined
                ? {}
                : { queueLength: mergeTxResult.queueLength }),
              ...(mergeTxResult.minQueueLength === undefined
                ? {}
                : { minQueueLength: mergeTxResult.minQueueLength }),
              ...(mergeTxResult.readyAfterUnixTime === undefined
                ? {}
                : { readyAfterUnixTime: mergeTxResult.readyAfterUnixTime }),
              ...(mergeTxResult.nowUnixTime === undefined
                ? {}
                : { nowUnixTime: mergeTxResult.nowUnixTime }),
            } satisfies MergeActionResult;
          }
          const snapshot = yield* fetchStateQueueSnapshotProgram(
            lucid.api,
            stateQueueAuthValidator,
            "post_merge",
          );
          yield* refreshStateQueueGlobalsFromSnapshot(globals, snapshot);
          yield* Effect.logInfo(
            `🔸 Refreshed live state-queue tail after merge: tail=${snapshot.tailCommitBase.outRef},snapshot=${snapshot.snapshotId}`,
          );
          return {
            status: "merged",
            postMergeSnapshot: snapshot,
            headerHash: mergeTxResult.headerHash,
            txHash: mergeTxResult.txHash,
            trigger: force
              ? "manual"
              : preflight.status === "tail_eligible_final_merge"
                ? "final_tail_auto_merge"
                : "threshold",
          } satisfies MergeActionResult;
        }),
      {
        ttlMs: nodeConfig.STATE_QUEUE_MUTATION_LEASE_TTL_MS,
        renewIntervalMs:
          nodeConfig.STATE_QUEUE_MUTATION_LEASE_RENEW_INTERVAL_MS,
      },
    );
    if (leaseResult._tag === "Busy") {
      const reason = StateQueueMutationLeasesDB.describeActiveLease(
        leaseResult.activeLease,
      );
      yield* Effect.logInfo(
        `🔸 Skipping merge because the state-queue mutation lease is busy (${reason}).`,
      );
      return {
        status: "skipped_state_queue_lease_busy",
        reason,
      } satisfies MergeActionResult;
    }
    return leaseResult.value;
  });

/**
 * Fiber wrapper that repeats merge attempts on the provided schedule.
 */
export const mergeFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<
  void,
  never,
  Lucid | MidgardContracts | Database | Globals | NodeConfig
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🟠 Merge fiber started.");
    const action = mergeAction().pipe(
      Effect.withSpan("merge-confirmed-state-fiber"),
      Effect.catchAllCause(Effect.logWarning),
    );
    yield* Effect.repeat(action, schedule);
  });
