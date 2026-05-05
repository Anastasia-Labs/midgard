import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Ref, Schedule } from "effect";
import { Data, LucidEvolution } from "@lucid-evolution/lucid";
import { Database, Globals, Lucid, MidgardContracts } from "@/services/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import { WithdrawalsDB } from "@/database/index.js";

/**
 * Fetches the currently visible withdrawal UTxO set.
 *
 * This intentionally mirrors deposit ingestion: repeated full-set
 * reconciliation is safer than cursor-only scanning when provider visibility
 * lags or an earlier scan races the chain indexer.
 */
const fetchWithdrawalUTxOs = (
  lucid: LucidEvolution,
  config?: Pick<
    SDK.WithdrawalFetchConfig,
    "inclusionTimeLowerBound" | "inclusionTimeUpperBound"
  >,
): Effect.Effect<SDK.WithdrawalUTxO[], SDK.LucidError, MidgardContracts> =>
  Effect.gen(function* () {
    const { withdrawal } = yield* MidgardContracts;
    const fetchConfig: SDK.WithdrawalFetchConfig = {
      eventAddress: withdrawal.spendingScriptAddress,
      eventPolicyId: withdrawal.policyId,
      ...config,
    };
    return yield* SDK.fetchWithdrawalUTxOsProgram(lucid, fetchConfig);
  });

const cborBuffer = (value: unknown, schema: unknown): Buffer =>
  Buffer.from(Data.to(value as never, schema as never), "hex");

const withdrawalUTxOToEntry = (
  withdrawalUTxO: SDK.WithdrawalUTxO,
): Effect.Effect<WithdrawalsDB.Entry, SDK.LucidError> =>
  Effect.try({
    try: () => {
      const { body } = withdrawalUTxO.datum.event.info;
      return {
        [WithdrawalsDB.Columns.ID]: Buffer.from(withdrawalUTxO.idCbor),
        [WithdrawalsDB.Columns.RAW_EVENT_INFO]: Buffer.from(
          withdrawalUTxO.infoCbor,
        ),
        [WithdrawalsDB.Columns.SETTLEMENT_EVENT_INFO]: null,
        [WithdrawalsDB.Columns.INCLUSION_TIME]: withdrawalUTxO.inclusionTime,
        [WithdrawalsDB.Columns.WITHDRAWAL_L1_TX_HASH]: Buffer.from(
          withdrawalUTxO.utxo.txHash,
          "hex",
        ),
        [WithdrawalsDB.Columns.WITHDRAWAL_L1_OUTPUT_INDEX]:
          withdrawalUTxO.utxo.outputIndex,
        [WithdrawalsDB.Columns.ASSET_NAME]: Buffer.from(
          withdrawalUTxO.assetName,
          "hex",
        ),
        [WithdrawalsDB.Columns.L2_OUTREF]: cborBuffer(
          body.l2_outref,
          SDK.OutputReference,
        ),
        [WithdrawalsDB.Columns.L2_OWNER]: Buffer.from(
          body.l2_owner,
          "hex",
        ),
        [WithdrawalsDB.Columns.L2_VALUE]: cborBuffer(body.l2_value, SDK.Value),
        [WithdrawalsDB.Columns.L1_ADDRESS]: cborBuffer(
          body.l1_address,
          SDK.AddressData,
        ),
        [WithdrawalsDB.Columns.L1_DATUM]: cborBuffer(
          body.l1_datum,
          SDK.CardanoDatum,
        ),
        [WithdrawalsDB.Columns.REFUND_ADDRESS]: cborBuffer(
          withdrawalUTxO.datum.refund_address,
          SDK.AddressData,
        ),
        [WithdrawalsDB.Columns.REFUND_DATUM]: cborBuffer(
          withdrawalUTxO.datum.refund_datum,
          SDK.CardanoDatum,
        ),
        [WithdrawalsDB.Columns.VALIDITY]: null,
        [WithdrawalsDB.Columns.VALIDITY_DETAIL]: {},
        [WithdrawalsDB.Columns.PROJECTED_HEADER_HASH]: null,
        [WithdrawalsDB.Columns.STATUS]: WithdrawalsDB.Status.Awaiting,
      };
    },
    catch: (cause) =>
      new SDK.LucidError({
        message:
          "Failed to project withdrawal UTxO into an offchain withdrawal entry",
        cause,
      }),
  });

const reconcileVisibleWithdrawalUTxOs = (
  config?: Pick<
    SDK.WithdrawalFetchConfig,
    "inclusionTimeLowerBound" | "inclusionTimeUpperBound"
  >,
): Effect.Effect<
  { readonly reconciledCount: number; readonly completedAt: Date },
  SDK.LucidError | DatabaseError,
  MidgardContracts | Lucid | Database
> =>
  Effect.gen(function* () {
    const { api: lucid } = yield* Lucid;
    const withdrawalUTxOs = yield* fetchWithdrawalUTxOs(lucid, config);

    if (withdrawalUTxOs.length <= 0) {
      yield* Effect.logDebug("No withdrawal UTxOs found.");
      return {
        reconciledCount: 0,
        completedAt: new Date(),
      } as const;
    }

    yield* Effect.logInfo(
      `${withdrawalUTxOs.length} withdrawal UTxO(s) found.`,
    );

    const entries = yield* Effect.forEach(
      withdrawalUTxOs,
      withdrawalUTxOToEntry,
    );
    yield* WithdrawalsDB.insertEntries(entries);
    return {
      reconciledCount: entries.length,
      completedAt: new Date(),
    } as const;
  });

export const fetchAndInsertWithdrawalUTxOs: Effect.Effect<
  void,
  SDK.LucidError | DatabaseError,
  MidgardContracts | Lucid | Database | Globals
> = Effect.gen(function* () {
  const globals = yield* Globals;
  yield* Ref.set(globals.HEARTBEAT_WITHDRAWAL_FETCH, Date.now());

  yield* Effect.logDebug("fetching WithdrawalUTxOs...");
  const { reconciledCount } = yield* reconcileVisibleWithdrawalUTxOs();
  if (reconciledCount <= 0) {
    return;
  }
  yield* Effect.logInfo(
    `Reconciled ${reconciledCount} visible withdrawal UTxO(s) into withdrawal_utxos.`,
  );
});

export const fetchAndInsertWithdrawalUTxOsForCommitBarrier = (
  inclusionTimeUpperBound: Date,
): Effect.Effect<
  Date,
  SDK.LucidError | DatabaseError,
  MidgardContracts | Lucid | Database
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo(
      `Running commit-time withdrawal ingestion barrier up to ${inclusionTimeUpperBound.toISOString()}.`,
    );
    const inclusiveUpperBound = BigInt(inclusionTimeUpperBound.getTime() + 1);
    const { reconciledCount, completedAt } =
      yield* reconcileVisibleWithdrawalUTxOs({
        inclusionTimeUpperBound: inclusiveUpperBound,
      });
    yield* Effect.logInfo(
      `Commit-time withdrawal barrier reconciled ${reconciledCount} withdrawal UTxO(s); fetch completed at ${completedAt.toISOString()} and locked the visibility barrier at ${inclusionTimeUpperBound.toISOString()}.`,
    );
    return inclusionTimeUpperBound;
  });

export const fetchAndInsertWithdrawalUTxOsFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<
  void,
  SDK.LucidError | DatabaseError,
  MidgardContracts | Lucid | Database | Globals
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("Fetch and insert WithdrawalUTxOs.");
    const action = fetchAndInsertWithdrawalUTxOs.pipe(
      Effect.withSpan("fetch-and-insert-withdrawal-utxos-fiber"),
      Effect.catchAllCause(Effect.logWarning),
    );
    yield* Effect.repeat(action, schedule);
  });
