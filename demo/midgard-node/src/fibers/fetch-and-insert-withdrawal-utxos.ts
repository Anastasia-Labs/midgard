import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Ref } from "effect";
import {
  AlwaysSucceedsContract,
  Database,
  Globals,
  Lucid,
} from "@/services/index.js";
import { LucidEvolution } from "@lucid-evolution/lucid";
import { WithdrawalsDB, UserEventsUtils } from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import { Schedule } from "effect";

const fetchWithdrawalUTxOs = (
  lucid: LucidEvolution,
  inclusionStartTime: number,
  inclusionEndTime: number,
): Effect.Effect<
  SDK.WithdrawalUTxO[],
  SDK.LucidError,
  AlwaysSucceedsContract
> =>
  Effect.gen(function* () {
    const { withdrawalAuthValidator } = yield* AlwaysSucceedsContract;
    const fetchConfig: SDK.WithdrawalFetchConfig = {
      withdrawalAddress: withdrawalAuthValidator.spendScriptAddress,
      withdrawalPolicyId: withdrawalAuthValidator.policyId,
      inclusionTimeLowerBound: BigInt(inclusionStartTime),
      inclusionTimeUpperBound: BigInt(inclusionEndTime),
    };
    return yield* SDK.fetchWithdrawalUTxOsProgram(lucid, fetchConfig);
  });

export const fetchAndInsertWithdrawalUTxOs: Effect.Effect<
  void,
  SDK.LucidError | DatabaseError,
  AlwaysSucceedsContract | Lucid | Database | Globals
> = Effect.gen(function* () {
  const { api: lucid } = yield* Lucid;
  const globals = yield* Globals;
  const startTime: number = yield* Ref.get(
    globals.LATEST_WITHDRAWAL_FETCH_TIME,
  );
  const endTime: number = Date.now();

  yield* Effect.logInfo(
    `💸 Fetching WithdrawalUTxOs (time range: ${startTime} - ${endTime})...`,
  );

  const withdrawalUTxOs = yield* fetchWithdrawalUTxOs(
    lucid,
    startTime,
    endTime,
  );

  if (withdrawalUTxOs.length <= 0) {
    yield* Effect.logDebug("No withdrawal UTxOs found.");
    return;
  }

  yield* Effect.logInfo(`${withdrawalUTxOs.length} withdrawal UTxOs found.`);

  const entries: UserEventsUtils.Entry[] = withdrawalUTxOs.map((utxo) => ({
    [UserEventsUtils.Columns.ID]: utxo.idCbor,
    [UserEventsUtils.Columns.INFO]: utxo.infoCbor,
    [UserEventsUtils.Columns.INCLUSION_TIME]: utxo.inclusionTime,
  }));

  yield* WithdrawalsDB.insertEntries(entries);

  yield* Ref.set(globals.LATEST_WITHDRAWAL_FETCH_TIME, endTime);
});

export const fetchAndInsertWithdrawalUTxOsFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<
  void,
  SDK.LucidError | DatabaseError,
  AlwaysSucceedsContract | Lucid | Database | Globals
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("💸 Fetch and insert Withdrawal UTxOs to the DB.");
    const action = fetchAndInsertWithdrawalUTxOs;
    yield* Effect.repeat(action, schedule);
  });
