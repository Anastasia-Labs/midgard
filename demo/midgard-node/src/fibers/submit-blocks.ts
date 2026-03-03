import * as SDK from "@al-ft/midgard-sdk";
import { DatabaseError } from "@/database/utils/common.js";
import {
  AlwaysSucceedsContract,
  Database,
  Globals,
  Lucid,
  NodeConfig,
} from "@/services/index.js";
import {
  handleSignSubmitNoConfirmation,
  TxConfirmError,
  TxSignError,
  TxSubmitError,
} from "@/transactions/utils.js";
import {
  CML,
  Data,
  fromHex,
  LucidEvolution,
  TxHash,
} from "@lucid-evolution/lucid";
import { Effect, Option, Ref, Schedule } from "effect";
import { serializeStateQueueUTxO } from "@/workers/utils/commit-block-header.js";
import {
  DepositsDB,
  LatestLedgerDB,
  MempoolDB,
  TxOrdersDB,
  UnsubmittedBlocksDB,
  WithdrawalsDB,
  LedgerUtils as Ledger,
  TxUtils as Tx,
  UserEventsUtils as UserEvent,
  ImmutableDB,
  BlocksDB,
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

const removeSpentOutRef = (
  ledger: Ledger.Entry[],
  spentOutRefCBOR: Buffer,
): Effect.Effect<Ledger.Entry[], SDK.CmlDeserializationError> =>
  Effect.filter(ledger, (ledgerEntry: Ledger.Entry) =>
    Effect.gen(function* () {
      // TODO: Raising deserialization error might not be needed here.
      const [ledgerEntryOutRef, spentOutRef] = yield* Effect.try({
        try: () => [
          CML.TransactionInput.from_cbor_bytes(
            ledgerEntry[Ledger.Columns.OUTREF],
          ),
          CML.TransactionInput.from_cbor_bytes(spentOutRefCBOR),
        ],
        catch: (e) =>
          new SDK.CmlDeserializationError({
            message:
              "Failed to deserialize an outref into a CML.TransactionInput",
            cause: e,
          }),
      });
      return (
        spentOutRef.transaction_id().to_hex() !==
          ledgerEntryOutRef.transaction_id().to_hex() &&
        spentOutRef.index() !== ledgerEntryOutRef.index()
      );
    }),
  );

const applyTxToLedger = <E, R>(
  ledger: readonly Ledger.Entry[],
  txCbor: Buffer,
) =>
  Effect.gen(function* () {
    const { spent, produced } = yield* breakDownTx(txCbor);
    return yield* Effect.reduce(
      spent,
      [...ledger, ...produced],
      (accLedger, s, _i) => removeSpentOutRef(accLedger, s),
    );
  });

type PrevLedgerAndEvents = {
  prevLedger: readonly Ledger.Entry[];
  withdrawals: readonly UserEvent.Entry[];
  txOrders: readonly UserEvent.Entry[];
  txRequests: readonly Tx.Entry[];
  deposits: readonly UserEvent.Entry[];
};

const getBlocksPrevLedgerAndEvents = (
  startDate: Date,
  endDate: Date,
): Effect.Effect<PrevLedgerAndEvents, DatabaseError, Database> =>
  Effect.gen(function* () {
    const [prevLedger, withdrawals, txOrders, txRequests, deposits] =
      yield* Effect.all(
        [
          LatestLedgerDB.retrieveNoTimeStamps,
          WithdrawalsDB.retrieveTimeBoundEntries(startDate, endDate),
          TxOrdersDB.retrieveTimeBoundEntries(startDate, endDate),
          MempoolDB.retrieveTimeBoundEntries(startDate, endDate),
          DepositsDB.retrieveTimeBoundEntries(startDate, endDate),
        ],
        { concurrency: "unbounded" },
      );
    return {
      prevLedger,
      withdrawals,
      txOrders,
      txRequests,
      deposits,
    };
  });

const depositEntryToLedgerEntry = (
  deposit: UserEvent.Entry,
): Effect.Effect<
  Ledger.Entry,
  SDK.CmlDeserializationError,
  NodeConfig | AlwaysSucceedsContract
> =>
  Effect.gen(function* () {
    const { deposit: depositAuthVal } = yield* AlwaysSucceedsContract;
    const cmlUTxO = yield* DepositsDB.entryToCMLUTxO(
      deposit,
      depositAuthVal.policyId,
    );
    return {
      [Ledger.Columns.ADDRESS]: cmlUTxO.output().address().to_bech32(),
      [Ledger.Columns.OUTPUT]: Buffer.from(cmlUTxO.output().to_cbor_bytes()),
      [Ledger.Columns.OUTREF]: Buffer.from(cmlUTxO.input().to_cbor_bytes()),
      [Ledger.Columns.TX_ID]: Buffer.from(
        cmlUTxO.input().transaction_id().to_raw_bytes(),
      ),
    };
  });

const applyEventsToLedger = (
  startDate: Date,
  endDate: Date,
): Effect.Effect<
  PrevLedgerAndEvents & {
    newLedger: readonly Ledger.Entry[];
    mempoolTxHashes: Buffer[];
  },
  SDK.CmlDeserializationError | DatabaseError,
  NodeConfig | Database | AlwaysSucceedsContract
> =>
  Effect.gen(function* () {
    const blockInfo = yield* getBlocksPrevLedgerAndEvents(startDate, endDate);
    const depositUTxOs = yield* Effect.all(
      blockInfo.deposits.map(depositEntryToLedgerEntry),
    );

    const afterWithdrawals = yield* Effect.reduce(
      blockInfo.withdrawals,
      blockInfo.prevLedger,
      (acc, w, _i) =>
        Effect.filter(acc, (utxoEntry) =>
          Effect.gen(function* () {
            const withdrawalInfo = Data.from(
              SDK.bufferToHex(w[UserEvent.Columns.INFO]),
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
        applyTxToLedger(acc, txOrder[UserEvent.Columns.INFO]),
    );
    const mempoolTxHashes: Buffer[] = [];
    const afterTxRequests = yield* Effect.reduce(
      blockInfo.txRequests,
      afterTxOrders,
      (acc, txRequest, _i) =>
        Effect.sync(() =>
          mempoolTxHashes.push(txRequest[Tx.Columns.TX_ID]),
        ).pipe(
          Effect.andThen((_) => applyTxToLedger(acc, txRequest[Tx.Columns.TX])),
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
  const optUnsubmittedBlock = yield* UnsubmittedBlocksDB.retrieveEarliestEntry;
  yield* Option.match(optUnsubmittedBlock, {
    onNone: () => Effect.logInfo("No unsubmitted blocks in queue."),
    onSome: (blockEntry) =>
      Effect.gen(function* () {
        yield* Effect.logInfo("🔗 ✉️  Submitting block commitment...");
        const txHash = yield* submitSignedTxCBOR(
          blockEntry[UnsubmittedBlocksDB.Columns.L1_CBOR],
        );
        yield* Effect.logInfo(`🔗 🚀 Block commitment submitted: ${txHash}`);
        const { txRequests, newLedger, mempoolTxHashes } =
          yield* applyEventsToLedger(
            blockEntry[UnsubmittedBlocksDB.Columns.EVENT_START_TIME],
            blockEntry[UnsubmittedBlocksDB.Columns.EVENT_END_TIME],
          );

        yield* LatestLedgerDB.clear;

        const updateLatestLedgerDB = batchProgram(
          BATCH_SIZE,
          newLedger.length,
          "Insert new ledger in LatestLedgerDB",
          (startIndex, endIndex) => LatestLedgerDB.insertMultiple(
            newLedger.slice(startIndex, endIndex),
          ),
        );

        const transferMempoolTxs = batchProgram(
          BATCH_SIZE,
          txRequests.length,
          "Transfer of MempoolDB entries to ImmutableDB and BlocksDB",
          (startIndex, endIndex) => {
            const txsBatch = txRequests.slice(startIndex, endIndex);
            const txHashesBatch = mempoolTxHashes.slice(startIndex, endIndex);
            return Effect.all(
              [
                MempoolDB.clearTxs(txHashesBatch),
                ImmutableDB.insertTxs(txsBatch),
                BlocksDB.insert(
                  blockEntry[UnsubmittedBlocksDB.Columns.HEADER_HASH],
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
          UnsubmittedBlocksDB.setStatusOfEntry(
            blockEntry,
            UnsubmittedBlocksDB.Status.SUBMITTED,
          ),
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
