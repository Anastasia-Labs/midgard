import * as SDK from "@al-ft/midgard-sdk";
import {
  Assets,
  Data,
  LucidEvolution,
  TxBuilder,
  TxSignBuilder,
  UTxO,
  toUnit,
} from "@lucid-evolution/lucid";
import {
  MidgardContracts,
  Database,
  Globals,
  Lucid,
  NodeConfig,
} from "@/services/index.js";
import { Effect, Option, Ref, Schedule } from "effect";
import {
  TxConfirmError,
  handleSignSubmit,
  TxSubmitError,
  TxSignError,
} from "@/transactions/utils.js";
import {
  AddressHistoryDB,
  BlocksDB,
  ConfirmedLedgerDB,
  ImmutableDB,
  LatestLedgerDB,
  MempoolDB,
  MempoolLedgerDB,
  ProcessedMempoolDB,
  TxRejectionsDB,
} from "@/database/index.js";
import { deleteLedgerMpt, deleteMempoolMpt } from "@/workers/utils/mpt.js";
import { DatabaseError } from "@/database/utils/common.js";
import { FileSystemError } from "@/utils.js";

const collectAndBurnUTxOsTx = (
  lucid: LucidEvolution,
  authValidator: SDK.AuthenticatedValidator,
  assetUTxOs: {
    utxo: UTxO;
    assetName: string;
  }[],
) =>
  Effect.gen(function* () {
    const tx = lucid.newTx();
    const assetsToBurn: Assets = {};
    assetUTxOs.map(({ utxo, assetName }) => {
      const unit = toUnit(authValidator.policyId, assetName);
      if (assetsToBurn[unit] !== undefined) {
        assetsToBurn[unit] -= 1n;
      } else {
        assetsToBurn[unit] = -1n;
      }
      tx.collectFrom([utxo], Data.void());
    });
    tx.mintAssets(assetsToBurn, Data.void())
      .attach.Script(authValidator.spendingScript)
      .attach.Script(authValidator.mintingScript);
    return tx;
  });

type UTxOsQueue = {
  authValidator: SDK.AuthenticatedValidator;
  assetUTxOs: {
    utxo: UTxO;
    assetName: string;
  }[];
};

const constructBatchTx = (
  lucid: LucidEvolution,
  utxosQueue: UTxOsQueue[],
  batchSize: number,
): Effect.Effect<
  Option.Option<{ batchTx: TxBuilder; restQueue: UTxOsQueue[] }>
> =>
  Effect.gen(function* () {
    if (batchSize === 0) {
      return Option.none();
    }

    const validatorUTxOs = utxosQueue.pop();
    if (validatorUTxOs === undefined) {
      return Option.none();
    }
    if (validatorUTxOs.assetUTxOs.length <= 0) {
      const skippedEmptyAssets = yield* constructBatchTx(
        lucid,
        utxosQueue,
        batchSize,
      );
      return skippedEmptyAssets;
    }

    const partialBatch = validatorUTxOs.assetUTxOs.slice(0, batchSize);

    const leftFromPartialBatch = validatorUTxOs.assetUTxOs.slice(batchSize);
    if (leftFromPartialBatch.length > 0) {
      // Put unconsumed utxos back
      utxosQueue.push({
        authValidator: validatorUTxOs.authValidator,
        assetUTxOs: leftFromPartialBatch,
      });
    }

    const partialBatchTx = yield* collectAndBurnUTxOsTx(
      lucid,
      validatorUTxOs.authValidator,
      partialBatch,
    );
    const optRestBatchTx = yield* constructBatchTx(
      lucid,
      utxosQueue,
      batchSize - partialBatch.length,
    );

    return Option.match(optRestBatchTx, {
      onNone: () =>
        Option.some({ batchTx: partialBatchTx, restQueue: utxosQueue }),
      onSome: ({ batchTx, restQueue }) =>
        Option.some({
          batchTx: partialBatchTx.compose(batchTx),
          restQueue,
        }),
    });
  });

const constructBatchTxs = (
  lucid: LucidEvolution,
  utxosQueue: UTxOsQueue[],
  batchSize: number,
): Effect.Effect<TxBuilder[]> =>
  Effect.gen(function* () {
    let accTransactions: TxBuilder[] = [];
    let currUtxoQueue = utxosQueue;

    while (currUtxoQueue.length > 0) {
      const optBatchTx = yield* constructBatchTx(lucid, utxosQueue, batchSize);
      const batchTx = Option.getOrElse(optBatchTx, () => ({
        batchTx: lucid.newTx(),
        restQueue: [],
      }));

      accTransactions.push(batchTx.batchTx);
      currUtxoQueue = batchTx.restQueue;
    }

    return accTransactions;
  });

const completeResetTxProgram = (
  lucid: LucidEvolution,
  tx: TxBuilder,
): Effect.Effect<void, SDK.LucidError | TxSignError | TxSubmitError> =>
  Effect.gen(function* () {
    const completed: TxSignBuilder = yield* tx.completeProgram().pipe(
      Effect.mapError(
        (e) =>
          new SDK.LucidError({
            message: "Failed to finalize the reset transaction",
            cause: e,
          }),
      ),
    );
    const onSubmitFailure = (err: TxSubmitError) =>
      Effect.gen(function* () {
        yield* Effect.logError(`Submit tx error: ${err}`);
        yield* Effect.fail(
          new TxSubmitError({
            message: "failed to submit a utxos reset tx",
            cause: err,
            txHash: completed.toHash(),
          }),
        );
      });
    const onConfirmFailure = (err: TxConfirmError) =>
      Effect.logError(`Confirm tx error: ${err}`);
    const txHash = yield* handleSignSubmit(lucid, completed).pipe(
      Effect.catchTag("TxSubmitError", onSubmitFailure),
      Effect.catchTag("TxConfirmError", onConfirmFailure),
    );
    return txHash;
  });

export const resetUTxOs: Effect.Effect<
  void,
  SDK.LucidError | TxSubmitError | TxSignError,
  MidgardContracts | Lucid
> = Effect.fail(
  new SDK.LucidError({
    message:
      "Reset endpoint is disabled: deinit path is not implemented for deployed contracts",
    cause: "reset-disabled",
  }),
);

export const resetDatabases: Effect.Effect<
  void,
  DatabaseError | FileSystemError,
  NodeConfig | Database
> = Effect.all(
  [
    MempoolDB.clear,
    MempoolLedgerDB.clear,
    ProcessedMempoolDB.clear,
    BlocksDB.clear,
    ImmutableDB.clear,
    LatestLedgerDB.clear,
    ConfirmedLedgerDB.clear,
    AddressHistoryDB.clear,
    TxRejectionsDB.clear,
    deleteMempoolMpt,
    deleteLedgerMpt,
  ],
  { discard: true },
);

export const program: Effect.Effect<
  void,
  | SDK.LucidError
  | TxSubmitError
  | TxSignError
  | DatabaseError
  | FileSystemError,
  Lucid | NodeConfig | MidgardContracts | Globals | Database
> = Effect.gen(function* () {
  const globals = yield* Globals;
  const resetWorkflow = Effect.gen(function* () {
    yield* Ref.set(globals.RESET_IN_PROGRESS, true);

    yield* Effect.all([
      resetUTxOs.pipe(
        Effect.retry(
          Schedule.intersect(Schedule.fixed("5000 millis"), Schedule.recurs(2)),
        ),
      ),
      resetDatabases,
    ]);

    yield* Effect.logInfo(`🚧 Resetting global variables...`);
    yield* Ref.set(globals.LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH, Date.now());
    yield* Ref.set(globals.BLOCKS_IN_QUEUE, 0);
    yield* Ref.set(globals.AVAILABLE_CONFIRMED_BLOCK, "");
    yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH, "");
    yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS, 0);
    yield* Ref.set(globals.LOCAL_FINALIZATION_PENDING, false);
    yield* Ref.set(globals.HEARTBEAT_BLOCK_COMMITMENT, Date.now());
    yield* Ref.set(globals.HEARTBEAT_BLOCK_CONFIRMATION, Date.now());
    yield* Ref.set(globals.HEARTBEAT_MERGE, Date.now());
    yield* Ref.set(globals.HEARTBEAT_DEPOSIT_FETCH, Date.now());
    yield* Ref.set(globals.HEARTBEAT_TX_QUEUE_PROCESSOR, Date.now());
    yield* Effect.logInfo(`🚧 Done resetting global variables...`);
  });

  yield* resetWorkflow.pipe(
    Effect.ensuring(Ref.set(globals.RESET_IN_PROGRESS, false)),
  );
});
