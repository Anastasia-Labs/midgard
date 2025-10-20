import { DatabaseError } from "@/database/utils/common.js";
import { Lucid, AlwaysSucceedsContract, Globals } from "@/services/index.js";
import { StateQueueTx } from "@/transactions/index.js";
import { TxSignError, TxSubmitError } from "@/transactions/utils.js";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql/SqlClient";
import { Effect, pipe, Schedule } from "effect";

export const mergeAction: Effect.Effect<
  void,
  | SDK.Utils.CmlDeserializationError
  | SDK.Utils.DataCoercionError
  | SDK.Utils.HashingError
  | SDK.Utils.LinkedListError
  | SDK.Utils.LucidError
  | SDK.Utils.StateQueueError
  | DatabaseError
  | TxSubmitError
  | TxSignError,
  Lucid | AlwaysSucceedsContract | SqlClient | Globals
> = Effect.gen(function* () {
  const lucid = yield* Lucid;
  const { stateQueueAuthValidator } = yield* AlwaysSucceedsContract;

  const fetchConfig: SDK.TxBuilder.StateQueue.FetchConfig = {
    stateQueueAddress: stateQueueAuthValidator.spendScriptAddress,
    stateQueuePolicyId: stateQueueAuthValidator.policyId,
  };
  yield* lucid.switchToOperatorsMergingWallet;
  yield* StateQueueTx.buildAndSubmitMergeTx(
    lucid.api,
    fetchConfig,
    stateQueueAuthValidator.spendScript,
    stateQueueAuthValidator.mintScript,
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
  Lucid | AlwaysSucceedsContract | SqlClient | Globals
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
