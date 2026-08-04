import {
  decodeMidgardTxOutput,
  encodeMidgardAddressText,
} from "@al-ft/midgard-core/codec";
import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import * as SDK from "@al-ft/midgard-sdk";
import { deriveCanonicalDepositTransitionEffectV1 } from "@al-ft/midgard-validation";
import {
  Data as LucidData,
  LucidEvolution,
  type Network,
} from "@lucid-evolution/lucid";
import { Effect, Ref, Schedule } from "effect";

import { DepositsDB, UserEventsUtils } from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import {
  logReconciledVisibleUserEvents,
  persistVisibleUserEventUTxOs,
  repeatVisibleUserEventIngestionFiber,
  runCommitTimeUserEventIngestionBarrier,
  type UserEventFetchBounds,
  type UserEventReconcileResult,
} from "@/fibers/user-event-ingestion.js";
import {
  Database,
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "@/services/index.js";

/**
 * Background ingestion for deposit UTxOs into the authoritative off-chain
 * deposit log.
 *
 * Projection into the mempool ledger is intentionally handled by a separate
 * step so ingestion remains idempotent and projection can enforce its own
 * timing and exactly-once rules.
 */

/**
 * Projects one deposit UTxO into the database row shape used by the off-chain
 * deposits ledger.
 */
const depositUTxOToEntry = (
  depositUTxO: SDK.DepositUTxO,
  network: Network,
  depositPolicyId: string,
): Effect.Effect<DepositsDB.Entry, SDK.LucidError> =>
  Effect.try({
    try: () => {
      const l2Datum = depositUTxO.datum.event.info.l2_datum;
      const effect = deriveCanonicalDepositTransitionEffectV1({
        configuredNetwork: network,
        eventId: depositUTxO.datum.event.id,
        l2NetworkId: depositUTxO.datum.event.info.l2_network_id,
        l2Address: depositUTxO.datum.event.info.l2_address,
        l2DatumCbor:
          l2Datum === null ? null : Buffer.from(LucidData.to(l2Datum), "hex"),
        l1Assets: depositUTxO.utxo.assets,
        depositPolicyId,
        depositAssetNameHex: depositUTxO.assetName,
      });
      const operation = effect.operations[0];
      if (operation === undefined || operation.type !== "insert") {
        throw new Error(
          "canonical deposit projection did not produce an insert",
        );
      }
      const output = Buffer.from(operation.outputCbor);
      const l2Address = encodeMidgardAddressText(
        decodeMidgardTxOutput(output).address,
      );

      return {
        [UserEventsUtils.Columns.ID]: depositUTxO.idCbor,
        [UserEventsUtils.Columns.INFO]: depositUTxO.infoCbor,
        [UserEventsUtils.Columns.INCLUSION_TIME]: depositUTxO.inclusionTime,
        [DepositsDB.Columns.DEPOSIT_L1_TX_HASH]: Buffer.from(
          depositUTxO.utxo.txHash,
          "hex",
        ),
        [DepositsDB.Columns.LEDGER_TX_ID]: computeHash32(depositUTxO.idCbor),
        [DepositsDB.Columns.LEDGER_OUTPUT]: Buffer.from(output),
        [DepositsDB.Columns.LEDGER_ADDRESS]: l2Address,
        [DepositsDB.Columns.PROJECTED_HEADER_HASH]: null,
        [DepositsDB.Columns.STATUS]: DepositsDB.Status.Awaiting,
      };
    },
    catch: (cause) =>
      new SDK.LucidError({
        message: "Failed to project deposit UTxO into an offchain ledger entry",
        cause,
      }),
  });

/**
 * Fetches the currently visible deposit UTxO set.
 *
 * Production correctness matters more than incremental scan efficiency here:
 * repeatedly reconciling the full visible deposit set avoids missing deposits
 * when provider/indexer visibility lags behind the node's previous scan time.
 */
const fetchDepositUTxOs = (
  lucid: LucidEvolution,
  config?: UserEventFetchBounds,
): Effect.Effect<SDK.DepositUTxO[], SDK.LucidError, MidgardContracts> =>
  Effect.gen(function* () {
    const { deposit: depositAuthValidator } = yield* MidgardContracts;
    const fetchConfig: SDK.UserEventFetchConfig = {
      eventAddress: depositAuthValidator.spendingScriptAddress,
      eventPolicyId: depositAuthValidator.policyId,
      ...config,
    };
    return yield* SDK.fetchDepositUTxOsProgram(lucid, fetchConfig);
  });

export const reconcileVisibleDepositUTxOs = (
  config?: UserEventFetchBounds,
): Effect.Effect<
  UserEventReconcileResult,
  SDK.LucidError | DatabaseError,
  MidgardContracts | Lucid | Database | NodeConfig
> =>
  Effect.gen(function* () {
    const { api: lucid } = yield* Lucid;
    const { deposit: depositAuthValidator } = yield* MidgardContracts;
    const nodeConfig = yield* NodeConfig;

    const depositUTxOs: SDK.DepositUTxO[] = yield* fetchDepositUTxOs(
      lucid,
      config,
    );
    return yield* persistVisibleUserEventUTxOs({
      visibleUtxos: depositUTxOs,
      toEntry: (utxo) =>
        depositUTxOToEntry(
          utxo,
          nodeConfig.NETWORK,
          depositAuthValidator.policyId,
        ),
      insertEntries: DepositsDB.insertEntries,
      emptyLogMessage: "🏦 No deposit UTxOs found.",
      foundLogMessage: (count) => `🏦 ${count} deposit UTxOs found.`,
    });
  });

/**
 * Runs one deposit-discovery pass and persists newly visible deposits into the
 * authoritative deposit log.
 */
export const fetchAndInsertDepositUTxOs: Effect.Effect<
  void,
  SDK.LucidError | DatabaseError,
  MidgardContracts | Lucid | Database | Globals | NodeConfig
> = Effect.gen(function* () {
  const globals = yield* Globals;
  yield* Ref.set(globals.HEARTBEAT_DEPOSIT_FETCH, Date.now());

  yield* Effect.logDebug("🏦 fetching DepositUTxOs...");
  const { reconciledCount, completedAt } =
    yield* reconcileVisibleDepositUTxOs();
  yield* Ref.set(globals.LATEST_DEPOSIT_FETCH_TIME, completedAt.getTime());
  yield* logReconciledVisibleUserEvents({
    reconciledCount,
    message: (count) =>
      `🏦 Reconciled ${count} visible deposit UTxO(s) into deposits_utxos.`,
  });
});

export const fetchAndInsertDepositUTxOsForCommitBarrier = (
  inclusionTimeUpperBound: Date,
): Effect.Effect<
  Date,
  SDK.LucidError | DatabaseError,
  MidgardContracts | Lucid | Database | NodeConfig
> =>
  runCommitTimeUserEventIngestionBarrier({
    inclusionTimeUpperBound,
    inclusionTimeUpperBoundOffsetMs: 0,
    startLogMessage: (upperBound) =>
      `🏦 Running commit-time deposit ingestion barrier up to ${upperBound.toISOString()}.`,
    completedLogMessage: ({
      reconciledCount,
      completedAt,
      inclusionTimeUpperBound: upperBound,
    }) =>
      `🏦 Commit-time deposit barrier reconciled ${reconciledCount} deposit UTxO(s); fetch completed at ${completedAt.toISOString()} and locked the visibility barrier at ${upperBound.toISOString()}.`,
    reconcile: reconcileVisibleDepositUTxOs,
  });

/**
 * Fiber wrapper that repeats deposit fetching on the provided schedule.
 */
export const fetchAndInsertDepositUTxOsFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<
  void,
  SDK.LucidError | DatabaseError,
  MidgardContracts | Lucid | Database | Globals | NodeConfig
> =>
  repeatVisibleUserEventIngestionFiber({
    schedule,
    startLogMessage: "🏦 Fetch and insert DepositUTxOs to DepositsDB.",
    spanName: "fetch-and-inser-deposi-utxos-fiber",
    action: fetchAndInsertDepositUTxOs,
  });
