import * as SDK from "@al-ft/midgard-sdk";
import { DatabaseError } from "@/database/utils/common.js";
import {
  AlwaysSucceedsContract,
  Database,
  Lucid,
  NodeConfig,
} from "@/services/index.js";
import { TxSignError, TxSubmitError } from "@/transactions/utils.js";
import { CML, Data } from "@lucid-evolution/lucid";
import { Effect, Option, Schedule } from "effect";
import {
  DepositsDB,
  LatestLedgerDB,
  MempoolDB,
  BlocksDB,
  Ledger,
  Tx,
  UserEvents,
  ImmutableDB,
  BlocksTxsDB,
} from "@/database/index.js";
import { batchProgram } from "@/utils.js";

// For database operations.
const BATCH_SIZE = 100;

const submitSignedTxCBOR = (
  l1CborBytes: Buffer,
): Effect.Effect<string, TxSignError | TxSubmitError | SDK.LucidError, Lucid> =>
  Effect.gen(function* () {
    const lucid = yield* Lucid;
    const signedTxHex = SDK.bufferToHex(l1CborBytes);
    const signedTx = yield* lucid.api.fromTx(signedTxHex).completeProgram();
    return yield* signedTx.submitProgram();
  }).pipe(
    Effect.mapError((e) => {
      const commonMsg = "Failed to submit previously built and signed tx";
      if (e._tag === "TxSubmitError") {
        return new TxSubmitError({
          message: commonMsg,
          cause: e,
          txHash: "<unknown>",
        });
      } else if (e._tag === "TxSignerError") {
        return new TxSignError({
          message: `${commonMsg} due to a bad signature`,
          cause: e,
          txHash: "<unknown>",
        });
      } else if (e._tag === "RunTimeError") {
        return new SDK.LucidError({
          message: `${commonMsg} due to an unknown error`,
          cause: e,
        });
      } else {
        return e;
      }
    }),
  );

const applyEventsToLedger = (
  startDate: Date,
  endDate: Date,
): Effect.Effect<
  BlocksDB.PrevLedgerAndEvents & {
    newLedger: readonly Ledger.Entry[];
    mempoolTxHashes: Buffer[];
  },
  SDK.CmlDeserializationError | DatabaseError,
  NodeConfig | Database | AlwaysSucceedsContract
> =>
  Effect.gen(function* () {
    const blockInfo = yield* BlocksDB.retrievePrevLedgerAndEvents(
      startDate,
      endDate,
    );
    const depositUTxOs = yield* Effect.all(
      blockInfo.deposits.map(DepositsDB.entryToLedgerEntry),
    );

    const afterWithdrawals = yield* Effect.reduce(
      blockInfo.withdrawals,
      blockInfo.prevLedger,
      (acc, w, _i) =>
        Effect.filter(acc, (utxoEntry) =>
          Effect.gen(function* () {
            const withdrawalInfo = Data.from(
              SDK.bufferToHex(w[UserEvents.Columns.INFO]),
              SDK.WithdrawalInfo,
            );
            const spentOutRef = withdrawalInfo.body.l2_outref;
            const ledgerEntryOutRef = CML.TransactionInput.from_cbor_bytes(
              utxoEntry[Ledger.Columns.OUTREF],
            );
            return (
              spentOutRef.txHash.hash !==
                ledgerEntryOutRef.transaction_id().to_hex() &&
              spentOutRef.outputIndex !== ledgerEntryOutRef.index()
            );
          }),
        ),
    );
    const afterTxOrders = yield* Effect.reduce(
      blockInfo.txOrders,
      afterWithdrawals,
      (acc, txOrder, _i) =>
        Ledger.applyTx(acc, txOrder[UserEvents.Columns.INFO]),
    );
    const mempoolTxHashes: Buffer[] = [];
    const afterTxRequests = yield* Effect.reduce(
      blockInfo.txRequests,
      afterTxOrders,
      (acc, txRequest, _i) =>
        Effect.sync(() =>
          mempoolTxHashes.push(txRequest[Tx.Columns.TX_ID]),
        ).pipe(
          Effect.andThen((_) => Ledger.applyTx(acc, txRequest[Tx.Columns.TX])),
        ),
    );
    const afterDeposits = [...afterTxRequests, ...depositUTxOs];
    return {
      ...blockInfo,
      newLedger: afterDeposits,
      mempoolTxHashes,
    };
  });

const submitEarliestBlock = Effect.gen(function* () {
  const optUnsubmittedBlock = yield* BlocksDB.retrieveEarliestEntry;
  yield* Option.match(optUnsubmittedBlock, {
    onNone: () => Effect.logInfo("No unsubmitted blocks in queue."),
    onSome: (blockEntry) =>
      Effect.gen(function* () {
        yield* Effect.logInfo("🔗 ✉️  Submitting block commitment...");
        const txHash = yield* submitSignedTxCBOR(
          blockEntry[BlocksDB.Columns.L1_CBOR],
        );
        yield* Effect.logInfo(`🔗 🚀 Block commitment submitted: ${txHash}`);
        const { txRequests, newLedger, mempoolTxHashes } =
          yield* applyEventsToLedger(
            blockEntry[BlocksDB.Columns.EVENT_START_TIME],
            blockEntry[BlocksDB.Columns.EVENT_END_TIME],
          );

        yield* LatestLedgerDB.clear;

        const updateLatestLedgerDB = batchProgram(
          BATCH_SIZE,
          newLedger.length,
          "Insert new ledger in LatestLedgerDB",
          (startIndex, endIndex) =>
            LatestLedgerDB.insertMultiple(
              newLedger.slice(startIndex, endIndex),
            ),
        );

        const transferMempoolTxs = batchProgram(
          BATCH_SIZE,
          txRequests.length,
          "Transfer of MempoolDB entries to ImmutableDB and BlocksTxsDB",
          (startIndex, endIndex) => {
            const txsBatch = txRequests.slice(startIndex, endIndex);
            const txHashesBatch = mempoolTxHashes.slice(startIndex, endIndex);
            return Effect.all(
              [
                MempoolDB.clearTxs(txHashesBatch),
                ImmutableDB.insertTxs(txsBatch),
                BlocksTxsDB.insert(
                  blockEntry[BlocksDB.Columns.HEADER_HASH],
                  txHashesBatch,
                ),
              ],
              { concurrency: "unbounded" },
            );
          },
        );

        yield* Effect.all([
          updateLatestLedgerDB,
          transferMempoolTxs,
          BlocksDB.setStatusOfEntry(blockEntry, BlocksDB.Status.SUBMITTED),
        ]);
      }),
  });
});

export const blockSubmissionFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<
  void,
  never,
  NodeConfig | Database | Lucid | AlwaysSucceedsContract
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🔗 Block submission fiber started.");
    const action = submitEarliestBlock.pipe(
      Effect.withSpan("submit-blocks-fiber"),
      Effect.catchAllCause(Effect.logWarning),
    );
    yield* Effect.repeat(action, schedule);
  });
