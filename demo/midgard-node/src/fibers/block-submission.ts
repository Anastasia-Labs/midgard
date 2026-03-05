import * as SDK from "@al-ft/midgard-sdk";
import { DatabaseError, NotFoundError } from "@/database/utils/common.js";
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
  WithdrawalsDB,
} from "@/database/index.js";
import { batchProgram, breakDownTx } from "@/utils.js";

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

const processEventsForLedgerApplication = (
  startDate: Date,
  endDate: Date,
): Effect.Effect<
  BlocksDB.Events & {
    entriesProducedByL2Transactions: Ledger.Entry[];
    outRefsSpentByL2Transactions: Buffer[];
    mempoolTxHashes: Buffer[];
    depositedLedgerEntries: Ledger.Entry[];
    withdrawnOutRefs: Buffer[];
  },
  | SDK.CmlDeserializationError
  | SDK.DataCoercionError
  | DatabaseError
  | NotFoundError,
  NodeConfig | Database | AlwaysSucceedsContract
> =>
  Effect.gen(function* () {
    const blockEvents = yield* BlocksDB.retrieveEvents(startDate, endDate);

    const withdrawnOutRefs: Buffer[] = [];
    const entriesProducedByL2Transactions: Ledger.Entry[] = [];
    const outRefsSpentByL2Transactions: Buffer[] = [];
    const mempoolTxHashes: Buffer[] = [];
    const depositedLedgerEntries = yield* Effect.all(
      blockEvents.deposits.map(DepositsDB.entryToLedgerEntry),
      { concurrency: "unbounded" },
    );

    // TODO: Allowing DataCoercionError to bubble up from here might be
    //       incorrect. WithdrawalsDB is most likely trust-worthy at this point.
    yield* Effect.forEach(blockEvents.withdrawals, (w) =>
      WithdrawalsDB.entryToOutRef(w).pipe(
        Effect.andThen((outRef) =>
          Effect.sync(() => withdrawnOutRefs.push(outRef)),
        ),
      ),
    );
    yield* Effect.forEach(blockEvents.txOrders, (txOrder) =>
      breakDownTx(txOrder[UserEvents.Columns.INFO]).pipe(
        Effect.andThen(({ spent, produced }) =>
          Effect.sync(() => {
            outRefsSpentByL2Transactions.push(...spent);
            entriesProducedByL2Transactions.push(...produced);
          }),
        ),
      ),
    );
    yield* Effect.forEach(blockEvents.txRequests, (txRequest) =>
      breakDownTx(txRequest[Tx.Columns.TX]).pipe(
        Effect.andThen(({ spent, produced }) =>
          Effect.sync(() => {
            mempoolTxHashes.push(txRequest[Tx.Columns.TX_ID]);
            outRefsSpentByL2Transactions.push(...spent);
            entriesProducedByL2Transactions.push(...produced);
          }),
        ),
      ),
    );
    yield* Effect.forEach(blockEvents.deposits, (deposit) =>
      DepositsDB.entryToLedgerEntry(deposit).pipe(
        Effect.andThen((ledgerEntry) =>
          Effect.sync(() => {
            entriesProducedByL2Transactions.push(ledgerEntry);
          }),
        ),
      ),
    );

    return {
      ...blockEvents,
      entriesProducedByL2Transactions,
      outRefsSpentByL2Transactions,
      mempoolTxHashes,
      depositedLedgerEntries,
      withdrawnOutRefs,
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
        const {
          txRequests,
          entriesProducedByL2Transactions,
          outRefsSpentByL2Transactions,
          mempoolTxHashes,
          depositedLedgerEntries,
          withdrawnOutRefs,
        } = yield* processEventsForLedgerApplication(
          blockEntry[BlocksDB.Columns.EVENT_START_TIME],
          blockEntry[BlocksDB.Columns.EVENT_END_TIME],
        );

        const addToLedgerProgram = batchProgram(
          BATCH_SIZE,
          entriesProducedByL2Transactions.length,
          "Insert new entries to LatestLedgerDB",
          (startIndex, endIndex) =>
            LatestLedgerDB.insertMultiple(
              entriesProducedByL2Transactions.slice(startIndex, endIndex),
            ),
        );
        const removeFromLedgerProgram = batchProgram(
          BATCH_SIZE,
          outRefsSpentByL2Transactions.length,
          "Remove spent outrefs from LatestLedgerDB",
          (startIndex, endIndex) =>
            LatestLedgerDB.clearUTxOs(
              outRefsSpentByL2Transactions.slice(startIndex, endIndex),
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

        // TODO: Update AddressHistoryDB as well.

        yield* Effect.all(
          [
            updateLatestLedgerDB,
            transferMempoolTxs,
            BlocksDB.setStatusOfEntry(blockEntry, BlocksDB.Status.SUBMITTED),
          ],
          { concurrency: "unbounded" },
        );
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
