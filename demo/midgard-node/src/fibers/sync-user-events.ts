import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Ref } from "effect";
import {
  AlwaysSucceedsContract,
  Globals,
  Lucid,
  Database,
} from "@/services/index.js";
import { LucidEvolution, utxoToCore } from "@lucid-evolution/lucid";
import {
  DepositsDB,
  TxOrdersDB,
  UserEventsUtils,
  WithdrawalsDB,
} from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import { Schedule } from "effect";

const fetchUserEventUTxOs = (
  lucid: LucidEvolution,
  inclusionStartTime: number,
  inclusionEndTime: number,
): Effect.Effect<
  {
    deposits: SDK.DepositUTxO[];
    txOrders: SDK.TxOrderUTxO[];
    withdrawals: SDK.WithdrawalUTxO[];
  },
  SDK.LucidError,
  AlwaysSucceedsContract
> =>
  Effect.gen(function* () {
    const { deposit, txOrder, withdrawal } = yield* AlwaysSucceedsContract;
    const inclusionTimeLowerBound = BigInt(inclusionStartTime);
    const inclusionTimeUpperBound = BigInt(inclusionEndTime);
    const depositFetchConfig: SDK.DepositFetchConfig = {
      eventAddress: deposit.spendingScriptAddress,
      eventPolicyId: deposit.policyId,
      inclusionTimeLowerBound,
      inclusionTimeUpperBound,
    };
    const txOrderFetchConfig: SDK.TxOrderFetchConfig = {
      eventAddress: txOrder.spendingScriptAddress,
      eventPolicyId: txOrder.policyId,
      inclusionTimeLowerBound,
      inclusionTimeUpperBound,
    };
    const withdrawalFetchConfig: SDK.WithdrawalFetchConfig = {
      eventAddress: withdrawal.spendingScriptAddress,
      eventPolicyId: withdrawal.policyId,
      inclusionTimeLowerBound,
      inclusionTimeUpperBound,
    };
    return {
      deposits: yield* SDK.fetchDepositUTxOsProgram(lucid, depositFetchConfig),
      txOrders: yield* SDK.fetchTxOrderUTxOsProgram(lucid, txOrderFetchConfig),
      withdrawals: yield* SDK.fetchWithdrawalUTxOsProgram(
        lucid,
        withdrawalFetchConfig,
      ),
    };
  });

const userEventUTxOsToEntry = (
  eventUTxOs: (SDK.DepositUTxO | SDK.TxOrderUTxO | SDK.WithdrawalUTxO)[],
): UserEventsUtils.Entry[] => {
  return eventUTxOs.map((utxo) => ({
    [UserEventsUtils.Columns.ID]: utxo.idCbor,
    [UserEventsUtils.Columns.INFO]: utxo.infoCbor,
    [UserEventsUtils.Columns.ASSET_NAME]: utxo.assetName,
    [UserEventsUtils.Columns.L1_UTXO_CBOR]: Buffer.from(
      utxoToCore(utxo.utxo).to_cbor_bytes(),
    ),
    [UserEventsUtils.Columns.INCLUSION_TIME]: utxo.inclusionTime,
  }));
};

export const syncUserEvents: Effect.Effect<
  void,
  SDK.LucidError | DatabaseError,
  AlwaysSucceedsContract | Lucid | Database | Globals
> = Effect.gen(function* () {
  const { api: lucid } = yield* Lucid;
  const globals = yield* Globals;
  const startTime: number = yield* Ref.get(
    globals.LATEST_USER_EVENTS_FETCH_TIME,
  );
  const endTime: number = Date.now();

  yield* Effect.logDebug("🏦 Fetching user event UTxOs...");

  const { deposits, txOrders, withdrawals } = yield* fetchUserEventUTxOs(
    lucid,
    startTime,
    endTime,
  );

  if (deposits.length <= 0 && txOrders.length <= 0 && withdrawals.length <= 0) {
    yield* Effect.logDebug(
      `🏦 No user events found within [${startTime}, ${endTime})`,
    );
    return;
  }

  yield* Effect.logInfo(`🏦 ${deposits.length} deposit event(s) found.`);
  yield* Effect.logInfo(`🏦 ${txOrders.length} tx order(s) found.`);
  yield* Effect.logInfo(`🏦 ${withdrawals.length} withdrawal order(s) found.`);

  const depositEntries: UserEventsUtils.Entry[] =
    userEventUTxOsToEntry(deposits);
  const txOrderEntries: UserEventsUtils.Entry[] =
    userEventUTxOsToEntry(txOrders);
  const withdrawalEntries: UserEventsUtils.Entry[] =
    userEventUTxOsToEntry(withdrawals);

  yield* DepositsDB.insertEntries(depositEntries);
  yield* TxOrdersDB.insertEntries(txOrderEntries);
  yield* WithdrawalsDB.insertEntries(withdrawalEntries);

  yield* Ref.set(globals.LATEST_USER_EVENTS_FETCH_TIME, endTime);
});

export const syncUserEventsFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<
  void,
  SDK.LucidError | DatabaseError,
  AlwaysSucceedsContract | Lucid | Database | Globals
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🏦 Sync user events to db");
    const action = syncUserEvents.pipe(
      Effect.withSpan("sync-user-events-fiber"),
      Effect.catchAllCause(Effect.logWarning),
    );
    yield* Effect.repeat(action, schedule);
  });
