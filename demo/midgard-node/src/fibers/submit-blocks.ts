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
} from "@/database/index.js";
import { breakDownTx } from "@/utils.js";

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

const applyTxToLedger = (ledger: readonly Ledger.Entry[], txCbor: Buffer) =>
  Effect.gen(function* () {
    const { spent, produced } = yield* breakDownTx(txCbor);
    return yield* Effect.reduce(
      spent,
      [...ledger, ...produced],
      (accLedger, s, _i) => removeSpentOutRef(accLedger, s),
    );
  });

const getAllEventsWithinInterval = (
  startDate: Date,
  endDate: Date,
): Effect.Effect<
  {
    prevLedger: readonly Ledger.Entry[];
    withdrawals: readonly UserEvent.Entry[];
    txOrders: readonly UserEvent.Entry[];
    txRequests: readonly Tx.Entry[];
    deposits: readonly UserEvent.Entry[];
  },
  DatabaseError,
  Database
> =>
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
  Ledger.Entry[],
  SDK.CmlDeserializationError | DatabaseError,
  NodeConfig | Database | AlwaysSucceedsContract
> =>
  Effect.gen(function* () {
    const { prevLedger, withdrawals, txOrders, txRequests, deposits } =
      yield* getAllEventsWithinInterval(startDate, endDate);
    const depositUTxOs = yield* Effect.all(
      deposits.map(depositEntryToLedgerEntry),
    );

    const afterWithdrawals = yield* Effect.reduce(
      withdrawals,
      prevLedger,
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
      txOrders,
      afterWithdrawals,
      (acc, txOrder, _i) =>
        applyTxToLedger(acc, txOrder[UserEvent.Columns.INFO]),
    );
    const afterTxRequests = yield* Effect.reduce(
      txRequests,
      afterTxOrders,
      (acc, txRequest, _i) => applyTxToLedger(acc, txRequest[Tx.Columns.TX]),
    );
    const afterDeposits = [...afterTxRequests, ...depositUTxOs];
    return afterDeposits;
  });

const temp = Effect.gen(function* () {
  const optUnsubmittedBlock = yield* UnsubmittedBlocksDB.retrieveEarliestEntry;
  yield* Option.match(optUnsubmittedBlock, {
    onNone: () => Effect.logInfo("No unsubmitted blocks in queue."),
    onSome: (entry) =>
      Effect.gen(function* () {
        const txHash = yield* submitSignedTxCBOR(
          entry[UnsubmittedBlocksDB.Columns.L1_CBOR],
        );
        const newLedger = yield* applyEventsToLedger(
          entry[UnsubmittedBlocksDB.Columns.EVENT_START_TIME],
          entry[UnsubmittedBlocksDB.Columns.EVENT_END_TIME],
        );
      }),
  });
});

const fetchLatestBlock = (
  lucid: LucidEvolution,
): Effect.Effect<
  SDK.StateQueueUTxO,
  SDK.StateQueueError | SDK.LucidError,
  AlwaysSucceedsContract
> =>
  Effect.gen(function* () {
    const { stateQueue: stateQueueAuthValidator } =
      yield* AlwaysSucceedsContract;
    const fetchConfig: SDK.StateQueueFetchConfig = {
      stateQueueAddress: stateQueueAuthValidator.spendingScriptAddress,
      stateQueuePolicyId: stateQueueAuthValidator.policyId,
    };
    return yield* SDK.fetchLatestCommittedBlockProgram(lucid, fetchConfig);
  });

const syncUnsubmittedBlocksWithLatestOnchainBlock = (
  latestBlock: SDK.StateQueueUTxO,
): Effect.Effect<void, SDK.DataCoercionError | DatabaseError, Database> =>
  Effect.gen(function* () {
    const headerHashHex = yield* SDK.headerHashFromStateQueueUTxO(latestBlock);
    const latestHeaderHash = Buffer.from(fromHex(headerHashHex));
    yield* UnsubmittedBlocksDB.deleteUpToAndIncludingBlock(latestHeaderHash);
  });

const confirmSubmittedBlock = (
  lucid: LucidEvolution,
  targetTxHash: TxHash,
): Effect.Effect<
  SDK.StateQueueUTxO,
  | SDK.CborSerializationError
  | SDK.CmlUnexpectedError
  | SDK.LucidError
  | SDK.StateQueueError
  | TxConfirmError,
  AlwaysSucceedsContract
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo(`🔗 Confirming tx: ${targetTxHash}`);

    yield* Effect.retry(
      Effect.tryPromise({
        try: () => lucid.awaitTx(targetTxHash),
        catch: (cause) => cause,
      }),
      Schedule.recurs(4),
    ).pipe(Effect.catchAllCause(Effect.logInfo));

    const latestBlock = yield* fetchLatestBlock(lucid);

    if (latestBlock.utxo.txHash === targetTxHash) {
      return latestBlock;
    } else {
      return yield* new TxConfirmError({
        message:
          "After multiple attempts, the block available on-chain has not updated yet.",
        cause: "Unknown",
        txHash: targetTxHash,
      });
    }
  });

const syncLatestConfirmedBlock = (
  lucid: LucidEvolution,
  globals: Globals,
): Effect.Effect<
  void,
  | SDK.CborSerializationError
  | SDK.CborDeserializationError
  | SDK.CmlUnexpectedError
  | SDK.DataCoercionError
  | SDK.LucidError
  | SDK.StateQueueError
  | DatabaseError,
  AlwaysSucceedsContract | Database | Globals
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo(
      "🔗 No cached confirmed block available. Fetching latest on-chain block...",
    );
    const latestBlock = yield* fetchLatestBlock(lucid);
    yield* syncUnsubmittedBlocksWithLatestOnchainBlock(latestBlock);
    const serializedLatestBlock = yield* serializeStateQueueUTxO(latestBlock);
    yield* Ref.set(globals.AVAILABLE_CONFIRMED_BLOCK, serializedLatestBlock);
    yield* Effect.logInfo("🔗 Updated cached confirmed block from chain tip.");
  });

const handleUnconfirmedSubmission = (
  lucid: LucidEvolution,
  globals: Globals,
  submittedTxHash: TxHash,
): Effect.Effect<
  void,
  | SDK.CborSerializationError
  | SDK.CmlUnexpectedError
  | SDK.DataCoercionError
  | SDK.LucidError
  | SDK.StateQueueError
  | DatabaseError
  | TxConfirmError,
  AlwaysSucceedsContract | Database
> =>
  Effect.gen(function* () {
    const confirmedBlock = yield* confirmSubmittedBlock(lucid, submittedTxHash);
    yield* syncUnsubmittedBlocksWithLatestOnchainBlock(confirmedBlock);
    yield* Ref.set(
      globals.AVAILABLE_CONFIRMED_BLOCK,
      yield* serializeStateQueueUTxO(confirmedBlock),
    );
    yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH, "");
    yield* Effect.logInfo(
      `🔗 ☑️  Confirmed block submission tx: ${submittedTxHash}`,
    );
  });

export const submitBlocks: Effect.Effect<
  void,
  | SDK.CborSerializationError
  | SDK.CborDeserializationError
  | SDK.CmlUnexpectedError
  | SDK.DataCoercionError
  | SDK.LucidError
  | SDK.StateQueueError
  | TxConfirmError
  | TxSignError
  | TxSubmitError
  | DatabaseError,
  AlwaysSucceedsContract | Lucid | Database | Globals
> = Effect.gen(function* () {
  const globals = yield* Globals;
  const lucid = yield* Lucid;

  const resetInProgress = yield* Ref.get(globals.RESET_IN_PROGRESS);
  if (resetInProgress) {
    return;
  }

  const availableConfirmedBlock = yield* Ref.get(
    globals.AVAILABLE_CONFIRMED_BLOCK,
  );
  const unconfirmedSubmittedBlockTxHash = yield* Ref.get(
    globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH,
  );

  if (unconfirmedSubmittedBlockTxHash !== "") {
    yield* handleUnconfirmedSubmission(
      lucid.api,
      globals,
      unconfirmedSubmittedBlockTxHash,
    );
    return;
  }

  if (availableConfirmedBlock === "") {
    yield* syncLatestConfirmedBlock(lucid.api, globals);
    return;
  }

  const unsubmittedBlocks = yield* UnsubmittedBlocksDB.retrieve;
  if (unsubmittedBlocks.length <= 0) {
    return;
  }

  const nextUnsubmittedBlock = unsubmittedBlocks[0];
  const blockHashHex =
    nextUnsubmittedBlock[UnsubmittedBlocksDB.Columns.HEADER_HASH].toString(
      "hex",
    );
  const txCborHex =
    nextUnsubmittedBlock[UnsubmittedBlocksDB.Columns.L1_CBOR].toString("hex");

  yield* Effect.logInfo(`🔗 Submitting block ${blockHashHex}`);
  const submittedTxHash = yield* handleSignSubmitNoConfirmation(
    lucid.api,
    lucid.api.fromTx(txCborHex),
  );
  yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH, submittedTxHash);

  yield* handleUnconfirmedSubmission(lucid.api, globals, submittedTxHash);
});

export const submitBlocksFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<
  void,
  never,
  Globals | Database | Lucid | AlwaysSucceedsContract
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🔗 Block submission fiber started.");
    const action = submitBlocks.pipe(
      Effect.withSpan("submit-blocks-fiber"),
      Effect.catchAllCause(Effect.logWarning),
    );
    yield* Effect.repeat(action, schedule);
  });
