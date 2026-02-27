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
  TxUtils as TxTable,
  UserEventsUtils,
  LedgerUtils as Ledger,
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
import { findSpentAndProducedUTxOs } from "@/utils.js";
import {
  Columns as UserEventsColumns,
  retrieveTimeBoundEntries,
} from "@/database/utils/user-events.js";

export type WorkerInput = {
  data: {
    availableConfirmedBlock: "" | SerializedStateQueueUTxO;
    mempoolTxsCountSoFar: number;
    sizeOfProcessedTxsSoFar: number;
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
  sizeOfProcessedTxs: number;
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
  mempoolTxs: readonly TxTable.EntryWithTimeStamp[],
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
        return Option.some(processedMempoolTxs[0][TxTable.Columns.TIMESTAMPTZ]);
      }
    } else {
      yield* Effect.logInfo(`🔹 ${mempoolTxs.length} retrieved.`);
      return Option.some(mempoolTxs[0][TxTable.Columns.TIMESTAMPTZ]);
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
    retreivedEvents: readonly UserEventsUtils.Entry[];
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
    const { spent, produced } = yield* findSpentAndProducedUTxOs(
      txCbor,
      txHash,
    ).pipe(Effect.withSpan("findSpentAndProducedUTxOs"));
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

export const applyMempoolToLedger = (
  ledgerTrie: MidgardMpt,
  txsTrie: MidgardMpt,
  mempoolTxs: readonly TxTable.Entry[],
): Effect.Effect<
  {
    mempoolTxHashes: Buffer[];
    sizeOfProcessedTxs: number;
  },
  SDK.CmlUnexpectedError | MptError
> =>
  Effect.gen(function* () {
    if (mempoolTxs.length <= 0) {
      return {
        mempoolTxHashes: [],
        sizeOfProcessedTxs: 0,
      };
    }
    const mempoolTxHashes: Buffer[] = [];
    const mempoolBatchOps: ETH_UTILS.BatchDBOp[] = [];
    const ledgerBatchOps: ETH_UTILS.BatchDBOp[] = [];
    let sizeOfProcessedTxs = 0;
    yield* Effect.logInfo(
      "🔹 Going through mempool and processings transactions...",
    );
    yield* Effect.forEach(mempoolTxs, (entry: TxTable.Entry) =>
      Effect.gen(function* () {
        const txHash = entry[TxTable.Columns.TX_ID];
        const txCbor = entry[TxTable.Columns.TX];
        const { delOps, putOps } = yield* txEntryToBatchDBOps(txHash, txCbor);
        mempoolTxHashes.push(txHash);
        sizeOfProcessedTxs += txCbor.length;
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

    return {
      mempoolTxHashes: mempoolTxHashes,
      sizeOfProcessedTxs: sizeOfProcessedTxs,
    };
  });

export const applyTxOrdersToLedger = (
  ledgerTrie: MidgardMpt,
  txOrderUTxOs: readonly SDK.TxOrderUTxO[],
) =>
  Effect.gen(function* () {
    if (txOrderUTxOs.length <= 0) {
      return {
        txHashes: [],
        spent: [],
        produced: [],
        sizeOfProcessedTxs: 0,
      };
    }
    yield* Effect.logInfo(
      `🔹 Applying ${txOrderUTxOs.length} tx order(s) to the ledgerTrie`,
    );

    const spentTxOrderUTxOs: Buffer[] = [];
    const producedTxOrderUTxOs: Ledger.MinimalEntry[] = [];
    const txOrderTxHashes: Buffer[] = [];
    const ledgerBatchOps: ETH_UTILS.BatchDBOp[] = [];
    let sizeOfProcessedTxs = 0;

    yield* Effect.forEach(txOrderUTxOs, (txOrderUTxO: SDK.TxOrderUTxO) =>
      Effect.gen(function* () {
        const txHash = txOrderUTxO.idCbor;
        const txCbor = txOrderUTxO.infoCbor;
        const { delOps, putOps, spent, produced } = yield* txEntryToBatchDBOps(
          txHash,
          txCbor,
        );
        txOrderTxHashes.push(txHash);
        sizeOfProcessedTxs += txCbor.length;
        ledgerBatchOps.push(...delOps);
        ledgerBatchOps.push(...putOps);
        spentTxOrderUTxOs.push(...spent);
        producedTxOrderUTxOs.push(...produced);
      }),
    );

    yield* ledgerTrie.batch(ledgerBatchOps);

    return {
      txHashes: txOrderTxHashes,
      spent: spentTxOrderUTxOs,
      produced: producedTxOrderUTxOs,
      sizeOfProcessedTxs,
    };
  });

export const applyWithdrawalsToLedger = (
  ledgerTrie: MidgardMpt,
  withdrawalUTxOs: readonly SDK.WithdrawalUTxO[],
) =>
  Effect.gen(function* () {
    if (withdrawalUTxOs.length <= 0) {
      return {
        txHashes: [],
        spent: [],
        produced: [],
        sizeOfProcessedTxs: 0,
      };
    }
    yield* Effect.logInfo(
      `🔹 Applying ${withdrawalUTxOs.length} withdrawal(s) to the ledgerTrie`,
    );

    const spentWithdrawalUTxOs: Buffer[] = [];
    const producedWithdrawalUTxOs: Ledger.MinimalEntry[] = [];
    const withdrawalTxHashes: Buffer[] = [];
    const ledgerBatchOps: ETH_UTILS.BatchDBOp[] = [];
    let sizeOfProcessedTxs = 0;

    yield* Effect.forEach(
      withdrawalUTxOs,
      (withdrawalUTxO: SDK.WithdrawalUTxO) =>
        Effect.gen(function* () {
          const txHash = withdrawalUTxO.idCbor;
          const txCbor = withdrawalUTxO.infoCbor;
          const { delOps, spent } = yield* txEntryToBatchDBOps(txHash, txCbor);
          withdrawalTxHashes.push(txHash);
          sizeOfProcessedTxs += txCbor.length;
          ledgerBatchOps.push(...delOps);
          spentWithdrawalUTxOs.push(...spent);
        }),
    );

    yield* ledgerTrie.batch(ledgerBatchOps);

    return {
      txHashes: withdrawalTxHashes,
      spent: spentWithdrawalUTxOs,
      produced: producedWithdrawalUTxOs,
      sizeOfProcessedTxs,
    };
  });

/**
 * Converts given deposit events (db entries) to Cardano UTxOs and adds them to
 * the given `ledgerTrie`. Returns the converted UTxOs and their inclusion
 * times.
 */
export const applyDepositsToLedger = (
  addOrRemove: "add" | "remove",
  ledgerTrie: MidgardMpt,
  deposits: readonly UserEventsUtils.Entry[],
): Effect.Effect<
  { utxo: CML.TransactionUnspentOutput; inclusionTime: Date }[],
  MptError | SDK.CmlUnexpectedError,
  NodeConfig | AlwaysSucceedsContract
> =>
  Effect.gen(function* () {
    if (deposits.length <= 0) {
      return [];
    }
    yield* Effect.logInfo(
      `🔹 Applying ${deposits.length} deposit(s) to the ledgerTrie`,
    );
    const { deposit: depositAuthValidator } = yield* AlwaysSucceedsContract;
    let insertedUTxOsWithDates: {
      utxo: CML.TransactionUnspentOutput;
      inclusionTime: Date;
    }[] = [];
    const putOpsRaw: (ETH_UTILS.BatchDBOp | void)[] = yield* Effect.forEach(
      deposits,
      (dbDeposit) =>
        Effect.gen(function* () {
          const utxo =
            yield* DepositsDB.depositEventToCmlTransactionUnspentOutput(
              dbDeposit,
              depositAuthValidator.policyId,
            );

          insertedUTxOsWithDates.push({
            utxo,
            inclusionTime: dbDeposit[UserEventsUtils.Columns.INCLUSION_TIME],
          });
          const putOp: ETH_UTILS.BatchDBOp =
            addOrRemove === "add"
              ? {
                  type: "put",
                  key: Buffer.from(utxo.input().to_cbor_bytes()),
                  value: Buffer.from(utxo.output().to_cbor_bytes()),
                }
              : { type: "del", key: Buffer.from(utxo.input().to_cbor_bytes()) };
          return putOp;
        }).pipe(Effect.catchAllCause(Effect.logInfo)),
    );

    const putOps = putOpsRaw.flatMap((f) => (f ? [f] : []));
    yield* ledgerTrie.batch(putOps);

    return insertedUTxOsWithDates;
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
