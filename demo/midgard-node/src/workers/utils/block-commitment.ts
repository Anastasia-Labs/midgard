import { Effect, Option } from "effect";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Data,
  UTxO,
  coreToUtxo,
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
} from "@/database/index.js";
import {
  AlwaysSucceedsContract,
  Database,
  Lucid,
  NodeConfig,
} from "@/services/index.js";
import { DatabaseError } from "@/database/utils/common.js";
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

export const buildUnsignedBlockCommitmentTx = (
  stateQueueAuthValidator: SDK.AuthenticatedValidator,
  latestBlock: SDK.StateQueueUTxO,
  utxosRoot: string,
  txsRoot: string,
  depositsRoot: string,
  endDate: Date,
): Effect.Effect<
  {
    newHeaderHash: string;
    signAndSubmitProgram: Effect.Effect<string, TxSubmitError | TxSignError>;
    txSize: number;
  },
  | SDK.DataCoercionError
  | SDK.HashingError
  | SDK.LucidError
  | SDK.StateQueueError,
  Lucid
> =>
  Effect.gen(function* () {
    const lucid = yield* Lucid;
    yield* Effect.logInfo("🔹 Finding updated block datum and new header...");
    yield* lucid.switchToOperatorsMainWallet;
    const { nodeDatum: updatedNodeDatum, header: newHeader } =
      yield* SDK.updateLatestBlocksDatumAndGetTheNewHeaderProgram(
        lucid.api,
        latestBlock.datum,
        utxosRoot,
        txsRoot,
        depositsRoot,
        "00".repeat(32),
        BigInt(endDate.getTime()),
      );

    const newHeaderHash = yield* SDK.hashBlockHeader(newHeader);
    yield* Effect.logInfo(`🔹 New header hash is: ${newHeaderHash}`);

    const commitBlockParams: SDK.StateQueueCommitBlockParams = {
      anchorUTxO: latestBlock,
      updatedAnchorDatum: updatedNodeDatum,
      newHeader: newHeader,
      stateQueueSpendingScript: stateQueueAuthValidator.spendingScript,
      policyId: stateQueueAuthValidator.policyId,
      stateQueueMintingScript: stateQueueAuthValidator.mintingScript,
    };

    const aoUpdateCommitmentTimeParams = {};

    yield* Effect.logInfo("🔹 Building block commitment transaction...");
    const fetchConfig: SDK.StateQueueFetchConfig = {
      stateQueueAddress: stateQueueAuthValidator.spendingScriptAddress,
      stateQueuePolicyId: stateQueueAuthValidator.policyId,
    };
    yield* lucid.switchToOperatorsMainWallet;
    const txBuilder = yield* SDK.unsignedCommitBlockHeaderTxProgram(
      lucid.api,
      fetchConfig,
      commitBlockParams,
      aoUpdateCommitmentTimeParams,
    );

    const txSize = txBuilder.toCBOR().length / 2;
    yield* Effect.logInfo(`🔹 Transaction built successfully. Size: ${txSize}`);

    const signAndSubmitProgram = handleSignSubmitNoConfirmation(
      lucid.api,
      txBuilder,
    ).pipe(Effect.withSpan("handleSignSubmit-commit-block"));

    return {
      newHeaderHash,
      signAndSubmitProgram,
      txSize,
    };
  });
