import {
  TxOrdersDB,
  DepositsDB,
  LedgerUtils,
  UserEventsUtils,
} from "@/database/index.js";
import { Effect } from "effect";
import * as ETH_UTILS from "@ethereumjs/util";
import { findSpentAndProducedUTxOs } from "@/utils.js";
import { Database } from "@/services/database.js";
import { DatabaseError } from "@/database/utils/common.js";
import * as SDK from "@al-ft/midgard-sdk";
import { keyValueMptRoot, MidgardMpt, MptError } from "./mpt.js";
import * as Tx from "@/database/utils/tx.js";
import * as Ledger from "@/database/utils/ledger.js";
import * as UserEvents from "@/database/utils/user-events.js";

export const processTxOrderEvent = (
  startTime: Date,
  endTime: Date,
  ledgerTrie: MidgardMpt,
): Effect.Effect<
  number,
  DatabaseError | SDK.Utils.CmlUnexpectedError | MptError,
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
  MptError | SDK.Utils.CmlUnexpectedError,
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
  startTime: Date,
  endTime: Date,
): Effect.Effect<
  { depositRoot: string; sizeOfDepositTxs: number },
  DatabaseError | MptError,
  Database
> =>
  Effect.gen(function* () {
    const deposits = yield* DepositsDB.retrieveTimeBoundEntries(
      startTime,
      endTime,
    );
    yield* Effect.logInfo(`🔹 Processing ${deposits.length} new deposits...`);
    const depositIDs = deposits.map(
      (deposit) => deposit[UserEvents.Columns.ID],
    );
    const depositInfos = deposits.map(
      (deposit) => deposit[UserEvents.Columns.INFO],
    );
    const depositRootFiber = yield* Effect.fork(
      keyValueMptRoot(depositIDs, depositInfos),
    );
    const depositRoot = yield* depositRootFiber;
    const sizeOfDepositTxs = depositInfos
      .map((i) => i.length)
      .reduce((acc, curr) => acc + curr, 0);
    yield* Effect.logInfo(
      `🔹 ${deposits.length} new deposits processed - new deposit root is ${depositRoot}`,
    );
    yield* Effect.logInfo(`🔹 New deposits root found: ${depositRoot}`);
    return {
      depositRoot,
      sizeOfDepositTxs,
    };
  });
