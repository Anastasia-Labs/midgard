import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Ref } from "effect";
import {
  AlwaysSucceedsContract,
  Globals,
  Lucid,
  Database,
} from "@/services/index.js";
import { LucidEvolution, utxoToCore } from "@lucid-evolution/lucid";
import { DepositsDB, UserEventsUtils } from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import { Schedule } from "effect";

const fetchDepositUTxOs = (
  lucid: LucidEvolution,
  inclusionStartTime: number,
  inclusionEndTime: number,
): Effect.Effect<SDK.DepositUTxO[], SDK.LucidError, AlwaysSucceedsContract> =>
  Effect.gen(function* () {
    const { deposit: depositAuthValidator } = yield* AlwaysSucceedsContract;
    const fetchConfig: SDK.DepositFetchConfig = {
      depositAddress: depositAuthValidator.spendingScriptAddress,
      depositPolicyId: depositAuthValidator.policyId,
      inclusionTimeLowerBound: BigInt(inclusionStartTime),
      inclusionTimeUpperBound: BigInt(inclusionEndTime),
    };
    return yield* SDK.fetchDepositUTxOsProgram(lucid, fetchConfig);
  });

export const fetchAndInsertDepositUTxOs: Effect.Effect<
  void,
  SDK.LucidError | DatabaseError,
  AlwaysSucceedsContract | Lucid | Database | Globals
> = Effect.gen(function* () {
  const { api: lucid } = yield* Lucid;
  const globals = yield* Globals;
  const startTime: number = yield* Ref.get(globals.LATEST_DEPOSIT_FETCH_TIME);
  const endTime: number = Date.now();

  yield* Effect.logDebug("🏦 Fetching DepositUTxOs...");

  const depositUTxOs: SDK.DepositUTxO[] = yield* fetchDepositUTxOs(
    lucid,
    startTime,
    endTime,
  );

  if (depositUTxOs.length <= 0) {
    yield* Effect.logDebug("🏦 No deposit UTxOs found.");
    return;
  }

  yield* Effect.logInfo(`🏦 ${depositUTxOs.length} deposit UTxOs found.`);

  const entries: UserEventsUtils.Entry[] = depositUTxOs.map((utxo) => ({
    [UserEventsUtils.Columns.ID]: utxo.idCbor,
    [UserEventsUtils.Columns.INFO]: utxo.infoCbor,
    [UserEventsUtils.Columns.ASSET_NAME]: utxo.assetName,
    [UserEventsUtils.Columns.L1_UTXO_CBOR]: Buffer.from(
      utxoToCore(utxo.utxo).to_cbor_bytes(),
    ),
    [UserEventsUtils.Columns.INCLUSION_TIME]: utxo.inclusionTime,
  }));

  yield* DepositsDB.insertEntries(entries);

  yield* Ref.set(globals.LATEST_DEPOSIT_FETCH_TIME, endTime);
});

export const fetchAndInsertDepositUTxOsFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<
  void,
  SDK.LucidError | DatabaseError,
  AlwaysSucceedsContract | Lucid | Database | Globals
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🏦 Fetch and insert DepositUTxOs to DepositsDB.");
    const action = fetchAndInsertDepositUTxOs.pipe(
      Effect.withSpan("fetch-and-inser-deposi-utxos-fiber"),
      Effect.catchAllCause(Effect.logWarning),
    );
    yield* Effect.repeat(action, schedule);
  });
