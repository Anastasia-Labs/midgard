import * as SDK from "@al-ft/midgard-sdk";
import { DatabaseError, NotFoundError } from "@/database/utils/common.js";
import { Database, Globals, Lucid } from "@/services/index.js";
import { TxSignError, TxSubmitError } from "@/transactions/utils.js";
import { Effect, Option, Ref, Schedule } from "effect";
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
  AddressHistoryDB,
  PendingBlockFinalizationsDB,
} from "@/database/index.js";
import { batchProgram, breakDownTx, ProcessedTx } from "@/utils.js";

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

/**
 * Going through withdrawal events and resolving their spent outrefs from
 * `LatestLedgerDB`, since it's the ledger that represents the state after
 * the latest submitted block.
 *
 * TODO: Allowing DataCoercionError to bubble up from here might be
 *       incorrect. WithdrawalsDB is most likely trust-worthy at this point.
 */
const processWithdrawalsProgram = (withdrawals: readonly UserEvents.Entry[]) =>
  Effect.gen(function* () {
    const withdrawnOutRefs: Buffer[] = [];
    const withdrawalAddressHistoryEntries: AddressHistoryDB.Entry[] = [];
    yield* Effect.forEach(withdrawals, (w) =>
      WithdrawalsDB.resolveEntry(LatestLedgerDB.tableName, w).pipe(
        Effect.andThen((resolvedWithdrawal) =>
          Effect.sync(() => {
            withdrawnOutRefs.push(
              resolvedWithdrawal.ledgerEntry[Ledger.Columns.OUTREF],
            );
            withdrawalAddressHistoryEntries.push(
              AddressHistoryDB.resolvedWithdrawalToEntry(
                resolvedWithdrawal,
                AddressHistoryDB.Status.SUBMITTED,
              ),
            );
          }),
        ),
      ),
    );
    return {
      withdrawnOutRefs,
      withdrawalAddressHistoryEntries,
    };
  });

const processTxOrdersProgram = (txOrders: readonly UserEvents.Entry[]) =>
  Effect.gen(function* () {
    const processedTxOrders: ProcessedTx[] = [];
    yield* Effect.forEach(txOrders, (txOrder) =>
      breakDownTx(txOrder[UserEvents.Columns.INFO]).pipe(
        Effect.andThen((processedTx) =>
          Effect.sync(() => {
            processedTxOrders.push(processedTx);
          }),
        ),
      ),
    );
    return processedTxOrders;
  });

const processTxRequestsProgram = (txRequests: readonly Tx.Entry[]) =>
  Effect.gen(function* () {
    const mempoolTxHashes: Buffer[] = [];
    const processedTxRequests: ProcessedTx[] = [];
    yield* Effect.forEach(txRequests, (txRequest) =>
      breakDownTx(txRequest[Tx.Columns.TX]).pipe(
        Effect.andThen((processedTx) =>
          Effect.sync(() => {
            mempoolTxHashes.push(txRequest[Tx.Columns.TX_ID]);
            processedTxRequests.push(processedTx);
          }),
        ),
      ),
    );
    return {
      mempoolTxHashes,
      processedTxRequests,
    };
  });

/*
 * Add produced ledger entries and corresponding address history entries for
 * the deposit events.
 */
const processDepositsProgram = (deposits: readonly DepositsDB.Entry[]) =>
  Effect.gen(function* () {
    const depositLedgerEntries: Ledger.Entry[] = [];
    const depositAddressHistoryEntries: AddressHistoryDB.Entry[] = [];
    yield* Effect.forEach(deposits, (deposit) =>
      Effect.gen(function* () {
        const ledgerEntry = yield* DepositsDB.toLedgerEntry(deposit);
        const addressHistoryEntry = yield* AddressHistoryDB.depositEntryToEntry(
          deposit,
          AddressHistoryDB.Status.SUBMITTED,
        );
        depositLedgerEntries.push(ledgerEntry);
        depositAddressHistoryEntries.push(addressHistoryEntry);
      }),
    );
    return {
      depositLedgerEntries,
      depositAddressHistoryEntries,
    };
  });

/**
 * Given an event interval, this function looks up the user events tables plus
 * `MempoolDB` to gather all the events that fall within that window. Uses
 * `LatestLedgerDB` as the state prior to the collected events.
 *
 * Here we are trusting the built and signed block transaction. Meaning, we
 * don't validate the order in which we apply the events to the ledger.
 *
 * Returns a set of values prepared for updating relevant tables.
 */
const processEventsForLedgerApplication = (
  startDate: Date,
  endDate: Date,
): Effect.Effect<
  {
    txRequests: readonly Tx.Entry[];
    allProducedLedgerEntries: Ledger.Entry[];
    allSpentOutRefs: Buffer[];
    mempoolTxHashes: Buffer[];
    allAddressHistoryEntries: AddressHistoryDB.Entry[];
  },
  | SDK.CmlDeserializationError
  | SDK.DataCoercionError
  | DatabaseError
  | NotFoundError,
  Database
> =>
  Effect.gen(function* () {
    const blockEvents = yield* BlocksDB.retrieveEvents(startDate, endDate);

    const { withdrawnOutRefs, withdrawalAddressHistoryEntries } =
      yield* processWithdrawalsProgram(blockEvents.withdrawals);

    const processedTxOrders = yield* processTxOrdersProgram(
      blockEvents.txOrders,
    );

    const { mempoolTxHashes, processedTxRequests } =
      yield* processTxRequestsProgram(blockEvents.txRequests);

    const { depositLedgerEntries, depositAddressHistoryEntries } =
      yield* processDepositsProgram(blockEvents.deposits);

    // Going through all the collected `ProcessedTx` values and getting prepared
    // values for updating the ledger, and also adding/updating entries to
    // `AddressHistoryDB`.
    const {
      addressHistoryEntries: l2TxsAddressHistoryEntries,
      collectiveProduced,
      collectiveSpent,
    } = yield* AddressHistoryDB.aggregateProcessedTxs(
      LatestLedgerDB.tableName,
      [...processedTxOrders, ...processedTxRequests],
      AddressHistoryDB.Status.SUBMITTED,
    );

    const allAddressHistoryEntries: AddressHistoryDB.Entry[] = [
      ...withdrawalAddressHistoryEntries,
      ...l2TxsAddressHistoryEntries,
      ...depositAddressHistoryEntries,
    ];
    const allProducedLedgerEntries: Ledger.Entry[] = [
      ...collectiveProduced,
      ...depositLedgerEntries,
    ];
    const allSpentOutRefs: Buffer[] = [...withdrawnOutRefs, ...collectiveSpent];

    return {
      txRequests: blockEvents.txRequests,
      allProducedLedgerEntries,
      allSpentOutRefs,
      mempoolTxHashes,
      allAddressHistoryEntries,
    };
  });

/**
 * Phase 1 of block submission. Posts the pre-built and signed L1 commit tx to
 * the Cardano node and updates the journal/gates. Mempool, ledger, and address
 * history are NOT mutated here — local finalization is deferred until
 * confirmation in `finalizeConfirmedBlock` so that an expired or otherwise
 * unincluded tx leaves no local divergence to roll back.
 */
const submitEarliestBlock = Effect.gen(function* () {
  const globals = yield* Globals;
  const unconfirmedSubmittedTxHash = yield* Ref.get(
    globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH,
  );
  if (unconfirmedSubmittedTxHash !== "") {
    yield* Effect.logInfo(
      "🔗 Skipping block submission; an earlier submission is awaiting confirmation.",
    );
    return;
  }
  const optUnsubmittedBlock = yield* BlocksDB.retrieveEarliestUnsubmittedEntry;
  yield* Option.match(optUnsubmittedBlock, {
    onNone: () => Effect.logInfo("No unsubmitted blocks in queue."),
    onSome: (blockEntry) =>
      Effect.gen(function* () {
        const headerHash = blockEntry[BlocksDB.Columns.HEADER_HASH];
        const blockEndTime = blockEntry[BlocksDB.Columns.EVENT_END_TIME];
        yield* PendingBlockFinalizationsDB.preparePendingSubmission({
          headerHash,
          blockEndTime,
          depositEventIds: [],
          mempoolTxIds: [],
        });
        yield* Effect.logInfo("🔗 ✉️  Submitting block commitment...");
        const txHash = yield* submitSignedTxCBOR(
          blockEntry[BlocksDB.Columns.L1_CBOR],
        );
        yield* Effect.logInfo(`🔗 🚀 Block commitment submitted: ${txHash}`);
        yield* PendingBlockFinalizationsDB.markSubmitted(
          headerHash,
          Buffer.from(txHash, "hex"),
        );
        yield* Ref.set(
          globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH,
          txHash,
        );
        yield* Ref.set(
          globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS,
          Date.now(),
        );
        yield* Ref.set(globals.AVAILABLE_CONFIRMED_BLOCK, "");
      }),
  });
});

/**
 * Phase 2 of block submission. Called by the confirmation fiber after the
 * pending block is observed in the canonical state queue. Mutates local DBs
 * (mempool clear, ledger update, address history, BlocksTxsDB / ImmutableDB
 * transfer, BlocksDB status) and marks the journal as locally finalized.
 *
 * Idempotent for already-finalized blocks: if the BlocksDB row is missing
 * (e.g., already finalized in a previous run) this is a no-op.
 */
export const finalizeConfirmedBlock = (
  headerHash: Buffer,
): Effect.Effect<
  void,
  | SDK.CmlDeserializationError
  | SDK.DataCoercionError
  | DatabaseError
  | NotFoundError,
  Database
> =>
  Effect.gen(function* () {
    const optBlockEntry = yield* BlocksDB.retrieveByHeaderHash(headerHash);
    yield* Option.match(optBlockEntry, {
      onNone: () =>
        Effect.logInfo(
          `🔗 finalizeConfirmedBlock: BlocksDB row already absent for ${headerHash.toString(
            "hex",
          )} (treating as already finalized).`,
        ),
      onSome: (blockEntry) =>
        Effect.gen(function* () {
          const {
            txRequests,
            allProducedLedgerEntries,
            allSpentOutRefs,
            mempoolTxHashes,
            allAddressHistoryEntries,
          } = yield* processEventsForLedgerApplication(
            blockEntry[BlocksDB.Columns.EVENT_START_TIME],
            blockEntry[BlocksDB.Columns.EVENT_END_TIME],
          );

          const addToLedgerProgram = batchProgram(
            BATCH_SIZE,
            allProducedLedgerEntries.length,
            "Insert new entries to LatestLedgerDB",
            (startIndex, endIndex) =>
              LatestLedgerDB.insertMultiple(
                allProducedLedgerEntries.slice(startIndex, endIndex),
              ),
          );

          const removeFromLedgerProgram = batchProgram(
            BATCH_SIZE,
            allSpentOutRefs.length,
            "Remove spent outrefs from LatestLedgerDB",
            (startIndex, endIndex) =>
              LatestLedgerDB.clearUTxOs(
                allSpentOutRefs.slice(startIndex, endIndex),
              ),
          );

          // Note that this does NOT have unbounded concurrency. We first want
          // to add any new UTxOs before deleting the spent ones, as some
          // transactions could have spent UTxOs produced by other transactions.
          const updateLatestLedgerDBProgram = Effect.all([
            addToLedgerProgram,
            removeFromLedgerProgram,
          ]);

          const transferMempoolTxsProgram = batchProgram(
            BATCH_SIZE,
            txRequests.length,
            "Transfer of MempoolDB entries to ImmutableDB and BlocksTxsDB",
            (startIndex, endIndex) => {
              const txsBatch = txRequests.slice(startIndex, endIndex);
              const txHashesBatch = mempoolTxHashes.slice(
                startIndex,
                endIndex,
              );
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

          const addToAddressHistoryProgram = batchProgram(
            BATCH_SIZE,
            allAddressHistoryEntries.length,
            "Insert AddressHistoryDB entries for all events",
            (startIndex, endIndex) =>
              AddressHistoryDB.upsertEntries(
                allAddressHistoryEntries.slice(startIndex, endIndex),
              ),
          );

          yield* Effect.all(
            [
              updateLatestLedgerDBProgram,
              transferMempoolTxsProgram,
              addToAddressHistoryProgram,
              BlocksDB.setStatusOfEntry(blockEntry, BlocksDB.Status.SUBMITTED),
            ],
            { concurrency: "unbounded" },
          );
          yield* PendingBlockFinalizationsDB.markLocalFinalizationComplete(
            headerHash,
          );
        }),
    });
  });

export const blockSubmissionFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<
  void,
  never,
  Database | Globals | Lucid
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🔗 Block submission fiber started.");
    const action = submitEarliestBlock.pipe(
      Effect.withSpan("submit-blocks-fiber"),
      Effect.catchAllCause(Effect.logWarning),
    );
    yield* Effect.repeat(action, schedule);
  });
