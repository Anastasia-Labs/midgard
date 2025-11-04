import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Ref } from "effect";
import {
  AlwaysSucceedsContract,
  Database,
  Globals,
  Lucid,
} from "@/services/index.js";
import { LucidEvolution } from "@lucid-evolution/lucid";
import { TxOrdersDB, UserEventsUtils } from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import { Schedule } from "effect";

const fetchTxOrderUTxOs = (
  lucid: LucidEvolution,
  inclusionStartTime: number,
  inclusionEndTime: number,
): Effect.Effect<
  SDK.TxOrderUTxO[],
  SDK.LucidError,
  AlwaysSucceedsContract
> =>
  Effect.gen(function* () {
    const { txOrderAuthValidator } = yield* AlwaysSucceedsContract;
    const fetchConfig: SDK.TxOrderFetchConfig = {
      txOrderAddress: txOrderAuthValidator.spendScriptAddress,
      txOrderPolicyId: txOrderAuthValidator.policyId,
      inclusionStartTime: BigInt(inclusionStartTime),
      inclusionEndTime: BigInt(inclusionEndTime),
    };
    return yield* SDK.fetchTxOrderUTxOsProgram(
      lucid,
      fetchConfig,
    );
  });

export const fetchAndInsertTxOrderUTxOs = (Effect.Effect<
  void,
  SDK.LucidError | DatabaseError,
  AlwaysSucceedsContract | Lucid | Database | Globals
> = Effect.gen(function* () {
  const { api: lucid } = yield* Lucid;
  const globals = yield* Globals;
  const startTime: number = yield* Ref.get(globals.LATEST_TX_ORDER_FETCH_TIME);
  const endTime: number = Date.now();

  yield* Effect.logInfo("  fetching TxOrderUTxOs...");

  const txOrderUTxOs = yield* fetchTxOrderUTxOs(lucid, startTime, endTime);

  if (txOrderUTxOs.length <= 0) {
    yield* Effect.logDebug("No tx order UTxOs found.");
    return;
  }

  yield* Effect.logInfo(`${txOrderUTxOs.length} deposit UTxOs found.`);

  const entries: UserEventsUtils.Entry[] = txOrderUTxOs.map((utxo) => ({
    [UserEventsUtils.Columns.ID]: utxo.idCbor,
    [UserEventsUtils.Columns.INFO]: utxo.infoCbor,
    [UserEventsUtils.Columns.INCLUSION_TIME]: utxo.inclusionTime,
  }));

  yield* TxOrdersDB.insertEntries(entries);

  yield* Ref.set(globals.LATEST_TX_ORDER_FETCH_TIME, endTime);
}));

export const fetchAndInsertTxOrderUTxOsFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<
  void,
  SDK.LucidError | DatabaseError,
  AlwaysSucceedsContract | Lucid | Database | Globals
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🟪 Fetch and insert TxOrder UTxOs to the DB.");
    const action = fetchAndInsertTxOrderUTxOs;
    yield* Effect.repeat(action, schedule);
  });
