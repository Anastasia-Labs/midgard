import { DatabaseError } from "@/database/utils/common.js";
import {
  Lucid,
  MidgardContracts,
  Globals,
  NodeConfig,
} from "@/services/index.js";
import { buildAndSubmitMergeTx } from "@/transactions/state-queue/merge-to-confirmed-state.js";
import { StateQueueMutationLeasesDB } from "@/database/index.js";
import {
  TxConfirmError,
  TxSignError,
  TxSubmitError,
} from "@/transactions/utils.js";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, pipe, Ref, Schedule } from "effect";
import { Database } from "@/services/index.js";
import {
  fetchStateQueueSnapshotProgram,
  type StateQueueSnapshot,
  refreshStateQueueGlobalsFromSnapshot,
} from "@/services/state-queue-topology.js";

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
    }
  | {
      readonly status:
        | "skipped_unresolved_commitment"
        | "skipped_state_queue_lease_busy"
        | "no_queued_block";
      readonly reason: string;
    };

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
    yield* Ref.set(globals.HEARTBEAT_MERGE, Date.now());
    if (!force) {
      const [unconfirmedSubmittedBlockTxHash, localFinalizationPending] =
        yield* Effect.all(
          [
            Ref.get(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH),
            Ref.get(globals.LOCAL_FINALIZATION_PENDING),
          ],
          { concurrency: "unbounded" },
        );
      if (unconfirmedSubmittedBlockTxHash !== "" || localFinalizationPending) {
        const reason = `submitted_tx=${
          unconfirmedSubmittedBlockTxHash || "none"
        },local_finalization_pending=${localFinalizationPending.toString()}`;
        yield* Effect.logInfo(
          `🔸 Skipping merge while block commitment is unresolved (${reason}).`,
        );
        return {
          status: "skipped_unresolved_commitment",
          reason,
        };
      }
    }
    const lucid = yield* Lucid;
    const contracts = yield* MidgardContracts;
    const { stateQueue: stateQueueAuthValidator } = contracts;

    const fetchConfig: SDK.StateQueueFetchConfig = {
      stateQueueAddress: stateQueueAuthValidator.spendingScriptAddress,
      stateQueuePolicyId: stateQueueAuthValidator.policyId,
    };
    const leaseResult = yield* StateQueueMutationLeasesDB.tryWithLease(
      "state_queue_merge",
      (leaseToken) =>
        Effect.gen(function* () {
          const preMergeSnapshot = yield* fetchStateQueueSnapshotProgram(
            lucid.api,
            stateQueueAuthValidator,
            "manual_status",
          );
          if (preMergeSnapshot.topology.parsedNodeCount <= 1) {
            return {
              status: "no_queued_block",
              reason: `queue_length=${preMergeSnapshot.topology.parsedNodeCount.toString()}`,
            } satisfies MergeActionResult;
          }
          yield* lucid.switchToOperatorsMergingWallet;
          yield* StateQueueMutationLeasesDB.revalidate(leaseToken);
          yield* buildAndSubmitMergeTx(
            lucid.api,
            fetchConfig,
            contracts,
            {
              bypassQueueLengthGuard: force,
              referenceScriptsAddress: lucid.referenceScriptsAddress,
            },
          );
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
          } satisfies MergeActionResult;
        }),
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
  pipe(
    Effect.gen(function* () {
      yield* Effect.logInfo("🟠 Merge fiber started.");
      const action = mergeAction().pipe(
        Effect.withSpan("merge-confirmed-state-fiber"),
        Effect.catchAllCause(Effect.logWarning),
      );
      yield* Effect.repeat(action, schedule);
    }),
  );
