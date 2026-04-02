import { DatabaseError } from "@/database/utils/common.js";
import { Lucid, AlwaysSucceedsContract, Globals } from "@/services/index.js";
import { StateQueueTx } from "@/transactions/index.js";
import { TxSignError, TxSubmitError } from "@/transactions/utils.js";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, pipe, Schedule } from "effect";
import { Database } from "@/services/index.js";
export const mergeAction: Effect.Effect<
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
  Lucid | AlwaysSucceedsContract | Database | Globals
> = Effect.gen(function* () {
  const lucid = yield* Lucid;
  const { stateQueue: stateQueueAuthValidator } = yield* AlwaysSucceedsContract;

  const fetchConfig: SDK.StateQueueFetchConfig = {
    stateQueueAddress: stateQueueAuthValidator.spendingScriptAddress,
    stateQueuePolicyId: stateQueueAuthValidator.policyId,
  };
  yield* lucid.switchToOperatorsMergingWallet;
  yield* StateQueueTx.buildAndSubmitMergeTx(
    lucid.api,
    fetchConfig,
    stateQueueAuthValidator.spendingScript,
    stateQueueAuthValidator.mintingScript,
  );
});

// possible issues:
// 1. tx-generator: large batch size & high concurrency
// 2. after initing node, can't commit the block
export const mergeFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<
  void,
  never,
  Lucid | AlwaysSucceedsContract | Database | Globals
> =>
  pipe(
    Effect.gen(function* () {
      yield* Effect.logInfo("🟠 Merge fiber started.");
      const action = mergeAction.pipe(
        Effect.withSpan("merge-confirmed-state-fiber"),
        Effect.catchAllCause(Effect.logWarning),
      );
      yield* Effect.repeat(action, schedule);
    }),
  );
