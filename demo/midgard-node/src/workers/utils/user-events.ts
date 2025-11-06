import { TxOrdersDB, LedgerUtils } from "@/database/index.js";
import { Effect, Option } from "effect";
import * as ETH_UTILS from "@ethereumjs/util";
import { findSpentAndProducedUTxOs } from "@/utils.js";
import { Database } from "@/services/database.js";
import { DatabaseError } from "@/database/utils/common.js";
import * as SDK from "@al-ft/midgard-sdk";
import {
  emptyRootHexProgram,
  keyValueMptRoot,
  MidgardMpt,
  MptError,
} from "./mpt.js";
import * as Tx from "@/database/utils/tx.js";
import * as Ledger from "@/database/utils/ledger.js";
import * as UserEvents from "@/database/utils/user-events.js";

export const processTxOrderEvent = (
  startTime: Date,
  endTime: Date,
  ledgerTrie: MidgardMpt,
): Effect.Effect<
  number,
  DatabaseError | SDK.CmlUnexpectedError | MptError,
  Database
> =>
  Effect.gen(function* () {
    const txOrders = yield* TxOrdersDB.retrieveTimeBoundEntries(
      startTime,
      endTime,
    );
    const utxoBatchDBOps: ETH_UTILS.BatchDBOp[] = [];
    yield* Effect.logInfo(`🔹 Processing ${txOrders.length} new tx orders...`);
    let sizeOfProcessedTxs = 0;
    yield* Effect.forEach(txOrders, (entry: UserEvents.Entry) =>
      Effect.gen(function* () {
        const txHash = entry[UserEvents.Columns.ID];
        const txCbor = entry[UserEvents.Columns.INFO];
        const { spent, produced } = yield* findSpentAndProducedUTxOs(
          txCbor,
          txHash,
        ).pipe(Effect.withSpan("findSpentAndProducedUTxOs"));
        sizeOfProcessedTxs += txCbor.length;
        const delOps: ETH_UTILS.BatchDBOp[] = spent.map((outRef) => ({
          type: "del",
          key: outRef,
        }));
        const putOps: ETH_UTILS.BatchDBOp[] = produced.map(
          (le: LedgerUtils.MinimalEntry) => ({
            type: "put",
            key: le[LedgerUtils.Columns.OUTREF],
            value: le[LedgerUtils.Columns.OUTPUT],
          }),
        );
        yield* Effect.sync(() => utxoBatchDBOps.push(...delOps));
        yield* Effect.sync(() => utxoBatchDBOps.push(...putOps));
      }),
    );
    yield* ledgerTrie.batch(utxoBatchDBOps);
    const utxoRoot = yield* ledgerTrie.getRootHex();
    yield* Effect.logInfo(
      `🔹 ${txOrders.length} new tx orders processed - new ledger root is ${utxoRoot}`,
    );
    yield* Effect.logInfo(`🔹 New UTxO root found: ${utxoRoot}`);
    return sizeOfProcessedTxs;
  });

export const processTxRequestEvent = (
  ledgerTrie: MidgardMpt,
  mempoolTrie: MidgardMpt,
  mempoolTxs: readonly Tx.Entry[],
): Effect.Effect<
  {
    mempoolTxHashes: Buffer[];
    sizeOfTxRequestTxs: number;
  },
  MptError | SDK.CmlUnexpectedError,
  Database
> =>
  Effect.gen(function* () {
    const mempoolTxHashes: Buffer[] = [];
    const mempoolBatchOps: ETH_UTILS.BatchDBOp[] = [];
    const batchDBOps: ETH_UTILS.BatchDBOp[] = [];
    let sizeOfProcessedTxs = 0;
    yield* Effect.logInfo(
      `🔹 Processing ${mempoolTxs.length} new tx requests...`,
    );
    yield* Effect.forEach(mempoolTxs, (entry: Tx.Entry) =>
      Effect.gen(function* () {
        const txHash = entry[Tx.Columns.TX_ID];
        const txCbor = entry[Tx.Columns.TX];
        mempoolTxHashes.push(txHash);
        const { spent, produced } = yield* findSpentAndProducedUTxOs(
          txCbor,
          txHash,
        ).pipe(Effect.withSpan("findSpentAndProducedUTxOs"));
        sizeOfProcessedTxs += txCbor.length;
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
        yield* Effect.sync(() =>
          mempoolBatchOps.push({
            type: "put",
            key: txHash,
            value: txCbor,
          }),
        );
        yield* Effect.sync(() => batchDBOps.push(...delOps));
        yield* Effect.sync(() => batchDBOps.push(...putOps));
      }),
    );

    yield* Effect.all(
      [mempoolTrie.batch(mempoolBatchOps), ledgerTrie.batch(batchDBOps)],
      { concurrency: "unbounded" },
    );

    const txRoot = yield* mempoolTrie.getRootHex();
    const utxoRoot = yield* ledgerTrie.getRootHex();
    yield* Effect.logInfo(`🔹 ${mempoolTxs.length} new tx requests processed`);
    yield* Effect.logInfo(`🔹 New transaction root found: ${txRoot}`);
    yield* Effect.logInfo(`🔹 New UTxO root found: ${utxoRoot}`);

    return {
      mempoolTxHashes,
      sizeOfTxRequestTxs: sizeOfProcessedTxs,
    };
  });

export const processDepositEvent = (
  optDepositsRootProgram: Option.Option<Effect.Effect<string, MptError, never>>,
): Effect.Effect<string, MptError, Database> =>
  Effect.gen(function* () {
    const depositsRoot: string = yield* Option.match(optDepositsRootProgram, {
      onNone: () => emptyRootHexProgram,
      onSome: (p) =>
        Effect.gen(function* ($) {
          const depositsRootFiber = yield* $(Effect.fork(p));
          const depositRoot = yield* $(depositsRootFiber);
          return depositRoot;
        }),
    });
    yield* Effect.logInfo(`🔹 Deposits root is: ${depositsRoot}`);
    return depositsRoot;
  });

/**
 * Given the target user event table, this helper finds all the events falling
 * in the given time range and if any was found, returns an `Effect` that finds
 * the MPT root of those events.
 */
export const userEventsProgram = (
  tableName: string,
  startDate: Date,
  endDate: Date,
): Effect.Effect<
  Option.Option<Effect.Effect<string, MptError>>,
  DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    const events = yield* UserEvents.retrieveTimeBoundEntries(
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
      const eventIDs = events.map((event) => event[UserEvents.Columns.ID]);
      const eventInfos = events.map((event) => event[UserEvents.Columns.INFO]);
      return Option.some(keyValueMptRoot(eventIDs, eventInfos));
    }
  });
