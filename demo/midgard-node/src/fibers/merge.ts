import { DatabaseError } from "@/database/utils/common.js";
import {
  Database,
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "@/services/index.js";
import { StateQueueTx } from "@/transactions/index.js";
import {
  TxConfirmError,
  TxSignError,
  TxSubmitError,
} from "@/transactions/utils.js";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, pipe, Ref, Schedule } from "effect";

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
        yield* Effect.logInfo(
          `🔸 Skipping merge while block commitment is unresolved (submitted_tx=${
            unconfirmedSubmittedBlockTxHash || "none"
          },local_finalization_pending=${localFinalizationPending.toString()}).`,
        );
        return;
      }
    }
    const lucid = yield* Lucid;
    const contracts = yield* MidgardContracts;
    const fetchConfig: SDK.StateQueueFetchConfig = {
      stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
      stateQueuePolicyId: contracts.stateQueue.policyId,
    };
    yield* lucid.switchToOperatorsMergingWallet;
    yield* StateQueueTx.buildAndSubmitMergeTx(
      lucid.api,
      fetchConfig,
      contracts,
      { bypassQueueLengthGuard: force },
    );
  });

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
