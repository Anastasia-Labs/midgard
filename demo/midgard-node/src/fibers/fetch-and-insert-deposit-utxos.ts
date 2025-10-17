import * as SDK from "@al-ft/midgard-sdk";
import { Console, Effect, Ref } from "effect";
import {
  AlwaysSucceedsContract,
  Globals,
  Lucid,
  NodeConfig,
} from "@/services/index.js";
import { LucidEvolution, CML, Data, fromHex } from "@lucid-evolution/lucid";
import { DepositsDB } from "@/database/index.js";
import { SqlClient } from "@effect/sql";
import { DatabaseError } from "@/database/utils/common.js";
import { Schedule } from "effect";

const fetchDepositUTxOs = (
  lucid: LucidEvolution,
  inclusionStartTime: number,
  inclusionEndTime: number,
): Effect.Effect<
  SDK.TxBuilder.Deposit.DepositUTxO[],
  | SDK.Utils.LucidError
  | SDK.Utils.DataCoercionError
  | SDK.Utils.AssetError
  | SDK.Utils.UnauthenticUtxoError,
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

export const fetchAndInsertDepositUTxOs = (): Effect.Effect<
  void,
  | SDK.Utils.LucidError
  | SDK.Utils.DataCoercionError
  | SDK.Utils.AssetError
  | SDK.Utils.UnauthenticUtxoError
  | DatabaseError,
  AlwaysSucceedsContract | NodeConfig | Lucid | SqlClient.SqlClient | Globals
> =>
  Effect.gen(function* () {
    const { api: lucid } = yield* Lucid;
    const globals = yield* Globals;
    const startTime: number = yield* Ref.get(globals.LATEST_DEPOSIT_FETCH_TIME);
    const endTime: number = Date.now();
    yield* Ref.set(globals.LATEST_DEPOSIT_FETCH_TIME, endTime);

    yield* Effect.logInfo("  fetching DepositUTxOs...");
    const depositUTxOs = yield* fetchDepositUTxOs(lucid, startTime, endTime);

    const getOutRef = (utxo: SDK.TxBuilder.Deposit.DepositUTxO): string =>
      Data.to(utxo.datum.event.id, SDK.TxBuilder.Common.OutputReference);

    const getDepositInfo = (utxo: SDK.TxBuilder.Deposit.DepositUTxO): string =>
      Data.to(utxo.datum.event.info, SDK.TxBuilder.Deposit.DepositInfo);

    const toBuffer = (str: string): Buffer => Buffer.from(fromHex(str));

    const getInclusionTime = (utxo: SDK.TxBuilder.Deposit.DepositUTxO): Date =>
      new Date(Number(utxo.datum.inclusionTime)); // TODO: Check if that the correct conversion for the db entry

    const entries: DepositsDB.Entry[] = depositUTxOs.map((utxo) => ({
      [DepositsDB.Columns.ID]: toBuffer(getOutRef(utxo)),
      [DepositsDB.Columns.INFO]: toBuffer(getDepositInfo(utxo)),
      [DepositsDB.Columns.INCLUSION_TIME]: getInclusionTime(utxo),
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
  AlwaysSucceedsContract | NodeConfig | Lucid | SqlClient.SqlClient | Globals
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🟪 Fetch and insert DepositUTxOs to the DB.");
    const action = fetchAndInsertDepositUTxOs();
    yield* Effect.repeat(action, schedule);
  });
