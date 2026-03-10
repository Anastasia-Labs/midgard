import { Effect, Option } from "effect";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Data,
  LucidEvolution,
  UTxO,
  coreToUtxo,
  fromHex,
  utxoToCore,
} from "@lucid-evolution/lucid";
import * as ETH_UTILS from "@ethereumjs/util";
import { keyValueMptRoot, MidgardMpt, MptError } from "./mpt.js";
import {
  DepositsDB,
  ProcessedMempoolDB,
  Tx,
  UserEvents,
  Ledger,
  AddressHistoryDB,
  MempoolLedgerDB,
  WithdrawalsDB,
  BlocksDB,
} from "@/database/index.js";
import {
  AlwaysSucceedsContract,
  Database,
  Lucid,
  NodeConfig,
} from "@/services/index.js";
import {
  DatabaseError,
  deserializeUTxOsFromStorage,
  serializeUTxOsForStorage,
} from "@/database/utils/common.js";
import {
  handleSignSubmitNoConfirmation,
  TxSignError,
  TxSubmitError,
} from "@/transactions/utils.js";
import { breakDownTxMinimally } from "@/utils.js";
import {
  Columns as UserEventsColumns,
  retrieveTimeBoundEntries,
} from "@/database/utils/user-events.js";

export type WorkerInput = {
  data: {
    availableConfirmedBlock: "" | SerializedStateQueueUTxO;
    mempoolTxsCountSoFar: number;
    sizeOfTxsSoFar: number;
  };
};

export type SuccessfulCommitmentOutput = {
  type: "SuccessfulCommitmentOutput";
  submittedTxHash: string;
  txSize: number;
  mempoolTxsCount: number;
  sizeOfBlocksTxs: number;
};

export type SkippedSubmissionOutput = {
  type: "SkippedSubmissionOutput";
  mempoolTxsCount: number;
  sizeOfTxs: number;
};

export type NothingToCommitOutput = {
  type: "NothingToCommitOutput";
};

export type FailureOutput = {
  type: "FailureOutput";
  error: string;
};

export type WorkerOutput =
  | SuccessfulCommitmentOutput
  | SkippedSubmissionOutput
  | NothingToCommitOutput
  | FailureOutput;

// Datatype to use CBOR hex of state queue UTxOs instead of `UTxO` from LE for
// transferability.
export type SerializedStateQueueUTxO = Omit<
  SDK.StateQueueUTxO,
  "utxo" | "datum"
> & { utxo: string; datum: string };

export const serializeStateQueueUTxO = (
  stateQueueUTxO: SDK.StateQueueUTxO,
): Effect.Effect<
  SerializedStateQueueUTxO,
  SDK.CmlUnexpectedError | SDK.CborSerializationError
> =>
  Effect.gen(function* () {
    const core: CML.TransactionUnspentOutput = yield* Effect.try({
      try: () => utxoToCore(stateQueueUTxO.utxo),
      catch: (e) =>
        new SDK.CmlUnexpectedError({
          message: `Failed to serialize state queue UTxO: ${e}`,
          cause: e,
        }),
    });
    const datumCBOR = yield* Effect.try({
      try: () => Data.to(stateQueueUTxO.datum, SDK.StateQueueDatum),
      catch: (e) =>
        new SDK.CborSerializationError({
          message: `Failed to serialize state queue datum: ${e}`,
          cause: e,
        }),
    });
    return {
      ...stateQueueUTxO,
      utxo: core.to_cbor_hex(),
      datum: datumCBOR,
    };
  });

export const deserializeStateQueueUTxO = (
  stateQueueUTxO: SerializedStateQueueUTxO,
): Effect.Effect<
  SDK.StateQueueUTxO,
  SDK.CmlUnexpectedError | SDK.CborDeserializationError
> =>
  Effect.gen(function* () {
    const u: UTxO = yield* Effect.try({
      try: () =>
        coreToUtxo(
          CML.TransactionUnspentOutput.from_cbor_hex(stateQueueUTxO.utxo),
        ),
      catch: (e) =>
        new SDK.CmlUnexpectedError({
          message: `Failed to convert state queue UTxO to CML: ${e}`,
          cause: e,
        }),
    });
    const d = yield* Effect.try({
      try: () => Data.from(stateQueueUTxO.datum, SDK.StateQueueDatum),
      catch: (e) =>
        new SDK.CborDeserializationError({
          message: `Failed to deserialize datum: ${e}`,
          cause: e,
        }),
    });
    return {
      ...stateQueueUTxO,
      utxo: u,
      datum: d,
    };
  });

export const getBlockHeadersEndDate = (
  latestBlocksDatum: SDK.StateQueueDatum,
): Effect.Effect<Date, SDK.DataCoercionError, never> =>
  Effect.gen(function* () {
    let endTimeBigInt: bigint;
    if (latestBlocksDatum.key === "Empty") {
      const { data: confirmedState } =
        yield* SDK.getConfirmedStateFromStateQueueDatum(latestBlocksDatum);
      endTimeBigInt = confirmedState.endTime;
    } else {
      const latestHeader =
        yield* SDK.getHeaderFromStateQueueDatum(latestBlocksDatum);
      endTimeBigInt = latestHeader.endTime;
    }
    return new Date(Number(endTimeBigInt));
  });

/**
 * We are assuming that it's impossible for mempool table to have an older tx
 * than any of the txs in the processed mempool table.
 */
export const establishEndDateFromTxRequests = (
  mempoolTxs: readonly Tx.EntryWithTimeStamp[],
): Effect.Effect<Option.Option<Date>, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (mempoolTxs.length <= 0) {
      yield* Effect.logInfo(
        "🔹 No transactions were found in MempoolDB, checking ProcessedMempoolDB...",
      );
      const processedMempoolTxs = yield* ProcessedMempoolDB.retrieve;
      if (processedMempoolTxs.length <= 0) {
        // No transaction requests are available for inclusion in a block.
        return Option.none();
      } else {
        // No new transactions received, but there are uncommitted transactions
        // in the MPT. So its root must be used to submit a new block, and if
        // successful, `ProcessedMempoolDB` must be cleared.
        return Option.some(processedMempoolTxs[0][Tx.Columns.TIMESTAMPTZ]);
      }
    } else {
      yield* Effect.logInfo(`🔹 ${mempoolTxs.length} retrieved.`);
      return Option.some(mempoolTxs[0][Tx.Columns.TIMESTAMPTZ]);
    }
  });

/**
 * Given the target user event table, this helper finds all the events falling
 * in the given time range and if any was found, returns an `Effect` that finds
 * the MPT root of those events with the retrieved event entries.
 */
export const userEventsProgram = (
  tableName: string,
  startDate: Date,
  endDate: Date,
): Effect.Effect<
  Option.Option<{
    mptRoot: Effect.Effect<string, MptError>;
    retreivedEvents: readonly UserEvents.Entry[];
  }>,
  DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    const events = yield* retrieveTimeBoundEntries(
      tableName,
      startDate,
      endDate,
    );

    if (events.length <= 0) {
      yield* Effect.logInfo(
        `🔹 No events found in ${tableName} table between ${startDate.getTime()} and ${endDate.getTime()}.`,
      );
      return Option.none();
    } else {
      yield* Effect.logInfo(
        `🔹 ${events.length} event(s) found in ${tableName} table between ${startDate.getTime()} and ${endDate.getTime()}.`,
      );
      const eventIDs = events.map((event) => event[UserEventsColumns.ID]);
      const eventInfos = events.map((event) => event[UserEventsColumns.INFO]);
      return Option.some({
        mptRoot: keyValueMptRoot(eventIDs, eventInfos),
        retreivedEvents: events,
      });
    }
  });

const txEntryToBatchDBOps = (
  txHash: Buffer,
  txCbor: Buffer,
): Effect.Effect<
  {
    spent: Buffer[];
    produced: Ledger.MinimalEntry[];
    delOps: ETH_UTILS.BatchDBOp[];
    putOps: ETH_UTILS.BatchDBOp[];
  },
  SDK.CmlUnexpectedError
> =>
  Effect.gen(function* () {
    const { spent, produced } = yield* breakDownTxMinimally(
      txCbor,
      txHash,
    ).pipe(Effect.withSpan("breakDownTxMinimally"));
    const delOps: ETH_UTILS.BatchDBOp[] = spent.map((outRef) => ({
      type: "del",
      key: outRef,
    }));
    const putOps: ETH_UTILS.BatchDBOp[] = produced.map(
      (le: Ledger.MinimalEntry) => ({
        type: "put",
        key: le[Ledger.Columns.OUTREF],
        value: le[Ledger.Columns.OUTPUT],
      }),
    );
    return {
      spent,
      produced,
      delOps,
      putOps,
    };
  });

export const applyWithdrawalsToLedger = (
  ledgerTrie: MidgardMpt,
  withdrawalEntries: readonly UserEvents.Entry[],
): Effect.Effect<
  {
    withdrawnOutRefs: Buffer[];
    withdrawalsRoot: string;
    sizeOfWithdrawals: number;
  },
  SDK.CmlDeserializationError | SDK.DataCoercionError | MptError
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo(
      `🔹 Applying ${withdrawalEntries.length} withdrawal(s) to the ledgerTrie`,
    );

    const withdrawalsTrie: MidgardMpt = yield* MidgardMpt.create("withdrawals");
    const withdrawnOutRefs: Buffer[] = [];
    const ledgerBatchOps: ETH_UTILS.BatchDBOp[] = [];
    const withdrawalsBatchOps: ETH_UTILS.BatchDBOp[] = [];
    let sizeOfWithdrawals = 0;

    yield* Effect.forEach(
      withdrawalEntries,
      (withdrawalEntry: UserEvents.Entry) =>
        Effect.gen(function* () {
          const withdrawalInfo = withdrawalEntry[UserEvents.Columns.INFO];
          const spentOutRef =
            yield* WithdrawalsDB.entryToOutRef(withdrawalEntry);
          ledgerBatchOps.push({
            type: "del",
            key: spentOutRef,
          });
          withdrawalsBatchOps.push({
            type: "put",
            key: withdrawalEntry[UserEvents.Columns.ID],
            value: withdrawalInfo,
          });
          sizeOfWithdrawals += withdrawalInfo.length;
          withdrawnOutRefs.push(spentOutRef);
        }),
    );

    yield* Effect.all(
      [
        ledgerTrie.batch(ledgerBatchOps),
        withdrawalsTrie.batch(withdrawalsBatchOps),
      ],
      { concurrency: "unbounded" },
    );

    const withdrawalsRoot = yield* withdrawalsTrie.getRootHex();

    return {
      withdrawnOutRefs,
      ledgerBatchOps,
      withdrawalsRoot,
      sizeOfWithdrawals,
    };
  });

export const applyTxOrdersToLedger = (
  ledgerTrie: MidgardMpt,
  txOrders: readonly UserEvents.Entry[],
): Effect.Effect<
  {
    txOrdersHashes: Buffer[];
    spentByTxOrders: Buffer[];
    producedByTxOrders: Ledger.MinimalEntry[];
    txsTrie: MidgardMpt;
    sizeOfTxOrders: number;
  },
  SDK.CmlUnexpectedError | MptError | DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo(
      `🔹 Applying ${txOrders.length} tx order(s) to the ledgerTrie`,
    );

    let sizeOfTxOrders = 0;
    const txOrdersHashes: Buffer[] = [];
    const ledgerBatchOps: ETH_UTILS.BatchDBOp[] = [];
    const spentByTxOrders: Buffer[] = [];
    const producedByTxOrders: Ledger.MinimalEntry[] = [];
    const txOrdersLedgerBatchOps: ETH_UTILS.BatchDBOp[] = [];
    const txsTrie: MidgardMpt = yield* MidgardMpt.create("txs");

    yield* Effect.forEach(txOrders, (txOrder: UserEvents.Entry) =>
      Effect.gen(function* () {
        const txHash = txOrder[UserEvents.Columns.ID];
        const txCbor = txOrder[UserEvents.Columns.INFO];
        const { delOps, putOps, spent, produced } = yield* txEntryToBatchDBOps(
          txHash,
          txCbor,
        );
        sizeOfTxOrders += txCbor.length;
        txOrdersHashes.push(txHash);
        ledgerBatchOps.push(...delOps);
        ledgerBatchOps.push(...putOps);
        spentByTxOrders.push(...spent);
        producedByTxOrders.push(...produced);
        txOrdersLedgerBatchOps.push({
          type: "put",
          key: txHash,
          value: txCbor,
        });
      }),
    );

    yield* ledgerTrie.batch(ledgerBatchOps);

    return {
      txOrdersHashes,
      spentByTxOrders,
      producedByTxOrders,
      txsTrie,
      sizeOfTxOrders,
    };
  });

export const applyTxRequestsToLedger = (
  ledgerTrie: MidgardMpt,
  txsTrie: MidgardMpt,
  mempoolTxs: readonly Tx.Entry[],
): Effect.Effect<
  {
    txRequestsHashes: Buffer[];
    txsRoot: string;
    sizeOfTxRequests: number;
  },
  SDK.CmlUnexpectedError | MptError
> =>
  Effect.gen(function* () {
    const mempoolTxHashes: Buffer[] = [];
    const mempoolBatchOps: ETH_UTILS.BatchDBOp[] = [];
    const ledgerBatchOps: ETH_UTILS.BatchDBOp[] = [];
    let sizeOfTxRequests = 0;
    yield* Effect.logInfo(
      "🔹 Going through mempool and processings transactions...",
    );
    yield* Effect.forEach(mempoolTxs, (entry: Tx.Entry) =>
      Effect.gen(function* () {
        const txHash = entry[Tx.Columns.TX_ID];
        const txCbor = entry[Tx.Columns.TX];
        const { delOps, putOps } = yield* txEntryToBatchDBOps(txHash, txCbor);
        mempoolTxHashes.push(txHash);
        sizeOfTxRequests += txCbor.length;
        mempoolBatchOps.push({
          type: "put",
          key: txHash,
          value: txCbor,
        });
        ledgerBatchOps.push(...delOps);
        ledgerBatchOps.push(...putOps);
      }),
    );

    yield* Effect.all(
      [txsTrie.batch(mempoolBatchOps), ledgerTrie.batch(ledgerBatchOps)],
      { concurrency: "unbounded" },
    );

    const txsRoot = yield* txsTrie.getRootHex();

    return {
      txRequestsHashes: mempoolTxHashes,
      txsRoot,
      sizeOfTxRequests,
    };
  });

export const applyDepositsToLedger = (
  ledgerTrie: MidgardMpt,
  deposits: readonly UserEvents.Entry[],
): Effect.Effect<
  {
    depositLedgerEntries: Ledger.Entry[];
    depositsRoot: string;
    sizeOfDeposits: number;
  },
  MptError | SDK.CmlDeserializationError,
  NodeConfig | AlwaysSucceedsContract
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo(
      `🔹 Applying ${deposits.length} deposit(s) to the ledgerTrie`,
    );
    const depositLedgerEntries: Ledger.Entry[] = [];
    const depositsTrie: MidgardMpt = yield* MidgardMpt.create("deposits");
    const depositsBatchOps: ETH_UTILS.BatchDBOp[] = [];
    const ledgerBatchOps: ETH_UTILS.BatchDBOp[] = [];
    let sizeOfDeposits = 0;
    yield* Effect.forEach(deposits, (depositEntry) =>
      Effect.gen(function* () {
        const ledgerEntry = yield* DepositsDB.entryToLedgerEntry(depositEntry);
        depositLedgerEntries.push(ledgerEntry);
        sizeOfDeposits += depositEntry[UserEvents.Columns.INFO].length;
        ledgerBatchOps.push({
          type: "put",
          key: ledgerEntry[Ledger.Columns.OUTREF],
          value: ledgerEntry[Ledger.Columns.OUTPUT],
        });
        depositsBatchOps.push({
          type: "put",
          key: depositEntry[UserEvents.Columns.ID],
          value: depositEntry[UserEvents.Columns.INFO],
        });
      }),
    );

    yield* Effect.all(
      [ledgerTrie.batch(ledgerBatchOps), depositsTrie.batch(depositsBatchOps)],
      { concurrency: "unbounded" },
    );

    const depositsRoot = yield* depositsTrie.getRootHex();

    return {
      depositLedgerEntries,
      depositsRoot,
      sizeOfDeposits,
    };
  });

const prepareLucidForBlockCommitment = (
  entry: BlocksDB.Entry,
): Effect.Effect<
  {
    lucidPreparation: Effect.Effect<LucidEvolution>;
    appendedUTxO: SDK.StateQueueUTxO;
  },
  SDK.CborDeserializationError | SDK.CmlUnexpectedError | SDK.StateQueueError,
  AlwaysSucceedsContract | Lucid
> =>
  Effect.gen(function* () {
    const newWalletUTxOs = yield* deserializeUTxOsFromStorage(
      entry[BlocksDB.Columns.NEW_WALLET_UTXOS],
    );
    const appendedUTxO =
      yield* BlocksDB.getAppendedStateQueueUTxOFromEntry(entry);
    const lucid = yield* Lucid;
    const lucidPreparation = Effect.gen(function* () {
      yield* lucid.switchToOperatorsBlockCommitmentWallet;
      yield* Effect.sync(() => lucid.api.overrideUTxOs(newWalletUTxOs));
      return lucid.api;
    });
    return {
      lucidPreparation,
      appendedUTxO,
    };
  });

export const buildNewBlockEntry = (
  entry: BlocksDB.Entry,
  utxosRoot: string,
  txsRoot: string,
  depositsRoot: string,
  withdrawalsRoot: string,
  endDate: Date,
  stats: {
    depositsCount: number;
    txRequestsCount: number;
    txOrdersCount: number;
    withdrawalsCount: number;
    totalEventsSize: number;
  },
): Effect.Effect<
  BlocksDB.EntryNoMeta,
  | SDK.CmlUnexpectedError
  | SDK.CborDeserializationError
  | SDK.CborSerializationError
  | SDK.DataCoercionError
  | SDK.HashingError
  | SDK.LucidError
  | SDK.StateQueueError
  | TxSignError,
  AlwaysSucceedsContract | Lucid
> =>
  Effect.gen(function* () {
    const { lucidPreparation, appendedUTxO } =
      yield* prepareLucidForBlockCommitment(entry);
    const initLucidAPI = yield* lucidPreparation;
    const { nodeDatum: updatedNodeDatum, header: newHeader } =
      yield* SDK.updateLatestBlocksDatumAndGetTheNewHeaderProgram(
        initLucidAPI,
        appendedUTxO.datum,
        utxosRoot,
        txsRoot,
        depositsRoot,
        withdrawalsRoot,
        BigInt(endDate.getTime()),
      );
    const newHeaderHash = yield* SDK.hashBlockHeader(newHeader);
    yield* Effect.logInfo(`🔹 New header hash is: ${newHeaderHash}`);
    const { stateQueue } = yield* AlwaysSucceedsContract;
    const commitBlockParams: SDK.StateQueueCommitBlockParams = {
      anchorUTxO: appendedUTxO,
      updatedAnchorDatum: updatedNodeDatum,
      newHeader: newHeader,
      stateQueueSpendingScript: stateQueue.spendingScript,
      policyId: stateQueue.policyId,
      stateQueueMintingScript: stateQueue.mintingScript,
    };

    yield* Effect.logInfo("🔹 Building block commitment transaction...");
    const fetchConfig: SDK.StateQueueFetchConfig = {
      stateQueueAddress: stateQueue.spendingScriptAddress,
      stateQueuePolicyId: stateQueue.policyId,
    };
    // Rerunning `lucidPreparation` to ensure Lucid API object is in proper state.
    const lucidAPI = yield* lucidPreparation;
    const txBuilder = yield* SDK.incompleteCommitBlockHeaderTxProgram(
      lucidAPI,
      fetchConfig,
      commitBlockParams,
    );
    const [newWalletUTxOs, producedUTxOs, txSignBuilder] = yield* txBuilder
      .chainProgram()
      .pipe(
        Effect.mapError(
          (e) =>
            new SDK.LucidError({
              message:
                "Failed to complete (chain method) built block commitment transaction",
              cause: e,
            }),
        ),
      );
    const signedTx = yield* txSignBuilder.sign
      .withWallet()
      .completeProgram()
      .pipe(
        Effect.mapError(
          (e) =>
            new TxSignError({
              message: "Failed to sign block commitment transaction",
              cause: e,
              txHash: txSignBuilder.toHash(),
            }),
        ),
      );
    const serializedNewWalletUTxOs =
      yield* serializeUTxOsForStorage(newWalletUTxOs);
    const serializedProducedUTxOs =
      yield* serializeUTxOsForStorage(producedUTxOs);
    const l1CBOR = Buffer.from(signedTx.toTransaction().to_cbor_bytes());
    const newBlockEntry: BlocksDB.EntryNoMeta = {
      [BlocksDB.Columns.HEADER_HASH]: Buffer.from(fromHex(newHeaderHash)),
      [BlocksDB.Columns.EVENT_START_TIME]:
        entry[BlocksDB.Columns.EVENT_END_TIME],
      [BlocksDB.Columns.EVENT_END_TIME]: endDate,
      [BlocksDB.Columns.NEW_WALLET_UTXOS]: serializedNewWalletUTxOs,
      [BlocksDB.Columns.PRODUCED_UTXOS]: serializedProducedUTxOs,
      [BlocksDB.Columns.L1_CBOR]: l1CBOR,
      [BlocksDB.Columns.DEPOSITS_COUNT]: stats.depositsCount,
      [BlocksDB.Columns.TX_REQUESTS_COUNT]: stats.txRequestsCount,
      [BlocksDB.Columns.TX_ORDERS_COUNT]: stats.txOrdersCount,
      [BlocksDB.Columns.WITHDRAWALS_COUNT]: stats.withdrawalsCount,
      [BlocksDB.Columns.TOTAL_EVENTS_SIZE]: stats.totalEventsSize,
      [BlocksDB.Columns.STATUS]: BlocksDB.Status.UNSUBMITTED,
    };
    return newBlockEntry;
  });
