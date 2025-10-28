import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Ref } from "effect";
import {
  AlwaysSucceedsContract,
  Globals,
  Lucid,
  NodeConfig,
  Database,
} from "@/services/index.js";
import { LucidEvolution, Data, fromHex } from "@lucid-evolution/lucid";
import { DepositsDB, UserEventsUtils } from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import { Schedule } from "effect";

const fetchDepositUTxOs = (
  lucid: LucidEvolution,
  inclusionStartTime: number,
  inclusionEndTime: number,
): Effect.Effect<
  SDK.TxBuilder.Deposit.DepositUTxO[],
  SDK.Utils.LucidError,
  AlwaysSucceedsContract | NodeConfig
> =>
  Effect.gen(function* () {
    const { depositAuthValidator } = yield* AlwaysSucceedsContract;
    const fetchConfig: SDK.TxBuilder.Deposit.FetchConfig = {
      depositAddress: depositAuthValidator.spendScriptAddress,
      depositPolicyId: depositAuthValidator.policyId,
      inclusionStartTime: BigInt(inclusionStartTime),
      inclusionEndTime: BigInt(inclusionEndTime),
    };
    return yield* SDK.Endpoints.fetchDepositUTxOsProgram(lucid, fetchConfig);
  });

export const fetchAndInsertDepositUTxOs: Effect.Effect<
  void,
  SDK.Utils.LucidError | DatabaseError,
  AlwaysSucceedsContract | NodeConfig | Lucid | Database | Globals
> = Effect.gen(function* () {
  const { api: lucid } = yield* Lucid;
  const globals = yield* Globals;
  const startTime: number = yield* Ref.get(globals.LATEST_DEPOSIT_FETCH_TIME);
  const endTime: number = Date.now();
  yield* Ref.set(globals.LATEST_DEPOSIT_FETCH_TIME, endTime);

  yield* Effect.logInfo("  fetching DepositUTxOs...");
  const depositUTxOs = yield* fetchDepositUTxOs(lucid, startTime, endTime);
  if (depositUTxOs.length > 0) {
    yield* Effect.log(`FOUND DEPOSIT UTXOS!!! ${depositUTxOs}`)
  }

  const getOutRef = (utxo: SDK.TxBuilder.Deposit.DepositUTxO): string =>
    Data.to(utxo.datum.event.id, SDK.TxBuilder.Common.OutputReference);

  const getDepositInfo = (utxo: SDK.TxBuilder.Deposit.DepositUTxO): string =>
    Data.to(utxo.datum.event.info, SDK.TxBuilder.Deposit.DepositInfo);

  const toBuffer = (str: string): Buffer => Buffer.from(fromHex(str));

  const getInclusionTime = (utxo: SDK.TxBuilder.Deposit.DepositUTxO): Date =>
    new Date(Number(utxo.datum.inclusionTime)); // TODO: Check if that the correct conversion for the db entry

  const entries: UserEventsUtils.Entry[] = depositUTxOs.map((utxo) => ({
    [UserEventsUtils.Columns.ID]: toBuffer(getOutRef(utxo)),
    [UserEventsUtils.Columns.INFO]: toBuffer(getDepositInfo(utxo)),
    [UserEventsUtils.Columns.INCLUSION_TIME]: getInclusionTime(utxo),
  }));

  yield* DepositsDB.insertEntries(entries);
});

export const fetchAndInsertDepositUTxOsFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<
  void,
  | SDK.Utils.LucidError
  | SDK.Utils.DataCoercionError
  | SDK.Utils.AssetError
  | SDK.Utils.UnauthenticUtxoError
  | DatabaseError,
  AlwaysSucceedsContract | NodeConfig | Lucid | Database | Globals
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🟪 Fetch and insert DepositUTxOs to the DB.");
    const action = fetchAndInsertDepositUTxOs;
    yield* Effect.repeat(action, schedule);
  });
