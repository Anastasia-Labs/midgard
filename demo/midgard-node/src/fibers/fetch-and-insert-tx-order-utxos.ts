import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Ref } from "effect";
import {
  AlwaysSucceedsContract,
  Database,
  Globals,
  Lucid,
  NodeConfig,
} from "@/services/index.js";
import { LucidEvolution, CML, Data, fromHex } from "@lucid-evolution/lucid";
import { TxOrdersDB } from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import { Schedule } from "effect";

const fetchTxOrderUTxOs = (
  lucid: LucidEvolution,
  inclusionStartTime: number,
  inclusionEndTime: number,
): Effect.Effect<
  SDK.TxBuilder.TxOrder.TxOrderUTxO[],
  | SDK.Utils.LucidError
  | SDK.Utils.DataCoercionError
  | SDK.Utils.AssetError
  | SDK.Utils.UnauthenticUtxoError,
  AlwaysSucceedsContract | NodeConfig
> =>
  Effect.gen(function* () {
    const { txOrderAuthValidator } = yield* AlwaysSucceedsContract;
    const fetchConfig: SDK.TxBuilder.TxOrder.FetchConfig = {
      txOrderAddress: txOrderAuthValidator.spendScriptAddress,
      txOrderPolicyId: txOrderAuthValidator.policyId,
      inclusionStartTime: BigInt(inclusionStartTime),
      inclusionEndTime: BigInt(inclusionEndTime),
    };
    return yield* SDK.Endpoints.fetchTxOrderUTxOsProgram(lucid, fetchConfig);
  });

export const fetchAndInsertTxOrderUTxOs = (): Effect.Effect<
  void,
  | SDK.Utils.LucidError
  | SDK.Utils.DataCoercionError
  | SDK.Utils.AssetError
  | SDK.Utils.UnauthenticUtxoError
  | DatabaseError,
  AlwaysSucceedsContract | NodeConfig | Lucid | Database | Globals
> =>
  Effect.gen(function* () {
    const { api: lucid } = yield* Lucid;
    const globals = yield* Globals;
    const startTime: number = yield* Ref.get(
      globals.LATEST_TX_ORDER_FETCH_TIME,
    );
    const endTime: number = Date.now();
    yield* Ref.set(globals.LATEST_TX_ORDER_FETCH_TIME, endTime);

    yield* Effect.logInfo("  fetching TxOrder UTxOs...");
    const txOrderUTxOs = yield* fetchTxOrderUTxOs(lucid, startTime, endTime);

    const getOutRef = (utxo: SDK.TxBuilder.TxOrder.TxOrderUTxO): string =>
      Data.to(utxo.datum.event.id, SDK.TxBuilder.Common.OutputReference);

    const getTxOrderInfo = (utxo: SDK.TxBuilder.TxOrder.TxOrderUTxO): string =>
      Data.to(utxo.datum.event.info, SDK.TxBuilder.TxOrder.TxOrderUTxO);

    const toBuffer = (str: string): Buffer => Buffer.from(fromHex(str));

    const getInclusionTime = (utxo: SDK.TxBuilder.TxOrder.TxOrderUTxO): Date =>
      new Date(Number(utxo.datum.inclusionTime)); // TODO: Check if that the correct conversion for the db entry

    const entries: TxOrdersDB.Entry[] = txOrderUTxOs.map((utxo) => ({
      [TxOrdersDB.Columns.ID]: toBuffer(getOutRef(utxo)),
      [TxOrdersDB.Columns.INFO]: toBuffer(getTxOrderInfo(utxo)),
      [TxOrdersDB.Columns.INCLUSION_TIME]: getInclusionTime(utxo),
    }));

    yield* TxOrdersDB.insertEntries(entries);
  });

export const fetchAndInsertTxOrderUTxOsFiber = (
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
    yield* Effect.logInfo("🟪 Fetch and insert TxOrder UTxOs to the DB.");
    const action = fetchAndInsertTxOrderUTxOs();
    yield* Effect.repeat(action, schedule);
  });
