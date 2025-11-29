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
  AlwaysSucceedsContract,
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
} from "@/database/index.js";
import { deleteLedgerMpt, deleteMempoolMpt } from "@/workers/utils/mpt.js";
import { DatabaseError } from "@/database/utils/common.js";
import { FileSystemError } from "@/utils.js";
import { AuthenticatedValidator } from "@al-ft/midgard-sdk";

const collectAndBurnUTxOsTx = (
  lucid: LucidEvolution,
  authValidator: AuthenticatedValidator,
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
      .attach.Script(authValidator.spendScript)
      .attach.Script(authValidator.mintScript);
    return tx;
  });

type UTxOsQueue = {
  authValidator: AuthenticatedValidator;
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
  AlwaysSucceedsContract | Lucid
> = Effect.gen(function* () {
  const lucid = yield* Lucid;
  const { stateQueueAuthValidator, depositAuthValidator } =
    yield* AlwaysSucceedsContract;

  yield* Effect.logInfo("🚧 Fetching UTxOs...");

  yield* lucid.switchToOperatorsMainWallet;

  const allStateQueueUTxOs = yield* SDK.fetchUnsortedStateQueueUTxOsProgram(
    lucid.api,
    {
      stateQueuePolicyId: stateQueueAuthValidator.policyId,
      stateQueueAddress: stateQueueAuthValidator.spendScriptAddress,
    },
  );

  const allDepositUTxOs = yield* SDK.fetchDepositUTxOsProgram(lucid.api, {
    depositAddress: depositAuthValidator.spendScriptAddress,
    depositPolicyId: depositAuthValidator.policyId,
  });

  if (allStateQueueUTxOs.length <= 0 && allDepositUTxOs.length <= 0) {
    yield* Effect.logInfo(`🚧 No UTxOs were found.`);
  }

  // The bottom UTxOs are handled first
  const utxosQueue: UTxOsQueue[] = [
    {
      authValidator: depositAuthValidator,
      assetUTxOs: allDepositUTxOs,
    },
    {
      authValidator: stateQueueAuthValidator,
      assetUTxOs: allStateQueueUTxOs,
    },
  ];

  const batchSize = 40;
  const batchTransactions = yield* constructBatchTxs(
    lucid.api,
    utxosQueue,
    batchSize,
  );
  yield* Effect.forEach(batchTransactions, (tx, i) =>
    Effect.gen(function* () {
      yield* Effect.logInfo(`🚧 UTxOs Batch ${i}`);
      yield* completeResetTxProgram(lucid.api, tx);
    }).pipe(Effect.tapError((e) => Effect.logError(e))),
  );

  yield* Effect.logInfo(`🚧 Done resetting UTxOs.`);
});

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
  Lucid | NodeConfig | AlwaysSucceedsContract | Globals | Database
> = Effect.gen(function* () {
  const globals = yield* Globals;
  yield* Ref.set(globals.RESET_IN_PROGRESS, true);

  yield* Effect.all([
    resetUTxOs.pipe(Effect.retry(Schedule.fixed("5000 millis"))),
    resetDatabases,
  ]);

  yield* Effect.logInfo(`🚧 Resetting global variables...`);
  yield* Ref.set(globals.LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH, Date.now());
  yield* Ref.set(globals.BLOCKS_IN_QUEUE, 0);
  yield* Ref.set(globals.AVAILABLE_CONFIRMED_BLOCK, "");

  yield* Ref.set(globals.RESET_IN_PROGRESS, false);
  yield* Effect.logInfo(`🚧 Done resetting global variables...`);
});
