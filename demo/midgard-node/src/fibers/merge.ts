import { DatabaseError } from "@/database/utils/common.js";
import {
  Lucid,
  MidgardContracts,
  Globals,
  NodeConfig,
} from "@/services/index.js";
import { StateQueueTx } from "@/transactions/index.js";
import { TxSignError, TxSubmitError } from "@/transactions/utils.js";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, pipe, Ref, Schedule } from "effect";
import { Database } from "@/services/index.js";

/**
 * Background merge flow for confirmed state-queue blocks.
 *
 * The merge fiber switches to the dedicated merge wallet and submits the
 * on-chain merge transaction that folds confirmed queue state into the next
 * durable checkpoint.
 */

/**
 * Runs one merge attempt, optionally bypassing the queue-length guard for
 * explicit recovery/administrative flows.
 */
export const mergeAction = (
  force: boolean = false,
): Effect.Effect<
  void,
  | SDK.CmlDeserializationError
  | SDK.DataCoercionError
  | SDK.HashingError
  | SDK.LinkedListError
  | SDK.LucidError
  | SDK.StateQueueError
  | DatabaseError
  | TxSubmitError
  | TxSignError,
  Lucid | MidgardContracts | Database | Globals | NodeConfig
> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    yield* Ref.set(globals.HEARTBEAT_MERGE, Date.now());
    const lucid = yield* Lucid;
    const contracts = yield* MidgardContracts;
    const { stateQueue: stateQueueAuthValidator } = contracts;

    const fetchConfig: SDK.StateQueueFetchConfig = {
      stateQueueAddress: stateQueueAuthValidator.spendingScriptAddress,
      stateQueuePolicyId: stateQueueAuthValidator.policyId,
    };
    yield* lucid.switchToOperatorsMergingWallet;
    yield* StateQueueTx.buildAndSubmitMergeTx(
      lucid.api,
      fetchConfig,
      contracts,
      {
        bypassQueueLengthGuard: force,
        referenceScriptsAddress: lucid.referenceScriptsAddress,
      },
    );
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
