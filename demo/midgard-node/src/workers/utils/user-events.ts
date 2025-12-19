import { TxOrdersDB, LedgerUtils } from "@/database/index.js";
import { Effect, Option } from "effect";
import * as ETH_UTILS from "@ethereumjs/util";
import { breakDownTx, findSpentAndProducedUTxOs } from "@/utils.js";
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
): Effect.Effect<
  {
    sizeOfTxOrderTxs: number;
    txOrderLedgerUTxOUpdate: LedgerUTxOUpdate;
  },
  DatabaseError | SDK.CmlDeserializationError | MptError,
  Database
> =>
  Effect.gen(function* () {
    const txOrders = yield* TxOrdersDB.retrieveTimeBoundEntries(
      startTime,
      endTime,
    );
    const utxoBatchDBOps: ETH_UTILS.BatchDBOp[] = [];
    yield* Effect.logInfo(`🔹 Processing ${txOrders.length} new tx orders...`);
    let sizeOfTxOrderTxs = 0;
    const spentUTxOs: Buffer[] = [];
    const producedUTxOs: LedgerUtils.Entry[] = [];
    yield* Effect.forEach(txOrders, (entry: UserEvents.Entry) =>
      Effect.gen(function* () {
        const txCbor = entry[UserEvents.Columns.INFO];
        const { spent, produced } = yield* breakDownTx(txCbor).pipe(
          Effect.withSpan("findSpentAndProducedUTxOs"),
        );
        sizeOfTxOrderTxs += txCbor.length;
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
        yield* Effect.sync(() => spentUTxOs.push(...spent));
        yield* Effect.sync(() => producedUTxOs.push(...produced));
      }),
    );
    const txOrderLedgerUTxOUpdate = (ledgerTrie: MidgardMpt) =>
      Effect.gen(function* () {
        yield* ledgerTrie.batch(utxoBatchDBOps);
        return { spentUTxOs, producedUTxOs };
      });
    // yield* ledgerTrie.batch(utxoBatchDBOps);
    // const utxoRoot = yield* ledgerTrie.getRootHex();
    // yield* Effect.logInfo(
    //   `🔹 ${txOrders.length} new tx orders processed - new ledger root is ${utxoRoot}`,
    // );
    // yield* Effect.logInfo(`🔹 New UTxO root found: ${utxoRoot}`);
    return {
      spentTxOrderUTxOs: spentUTxOs,
      producedTxOrderUTxOs: producedUTxOs,
      sizeOfTxOrderTxs,
      txOrderLedgerUTxOUpdate,
    };
  });

// No spent and produced utxos in ledger update here, because it is already
// handled by submit endpoint
export const processTxRequestEvent = (
  // ledgerTrie: MidgardMpt,
  mempoolTrie: MidgardMpt,
  mempoolTxs: readonly Tx.Entry[],
): Effect.Effect<
  {
    mempoolTxHashes: Buffer[];
    sizeOfTxRequestTxs: number;
    txRequestLedgerUTxOUpdate: (
      ledgerTrie: MidgardMpt,
    ) => Effect.Effect<void, SDK.CmlDeserializationError | MptError>;
  },
  MptError | SDK.CmlUnexpectedError,
  Database
> =>
  Effect.gen(function* () {
    const mempoolTxHashes: Buffer[] = [];
    const mempoolBatchOps: ETH_UTILS.BatchDBOp[] = [];
    const batchDBOps: ETH_UTILS.BatchDBOp[] = [];
    const spentUTxOs: Buffer[] = [];
    const producedUTxOs: LedgerUtils.MinimalEntry[] = [];
    let sizeOfTxRequestTxs = 0;
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
        sizeOfTxRequestTxs += txCbor.length;
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
        yield* Effect.sync(() => spentUTxOs.push(...spent));
        yield* Effect.sync(() => producedUTxOs.push(...produced));
      }),
    );
    const txRequestLedgerUTxOUpdate = (ledgerTrie: MidgardMpt) =>
      ledgerTrie.batch(batchDBOps);
    yield* mempoolTrie.batch(mempoolBatchOps);
    const txRoot = yield* mempoolTrie.getRootHex();
    yield* Effect.logInfo(`🔹 ${mempoolTxs.length} new tx requests processed`);
    yield* Effect.logInfo(`🔹 New transaction root found: ${txRoot}`);

    return {
      mempoolTxHashes,
      sizeOfTxRequestTxs,
      txRequestLedgerUTxOUpdate,
    };
  });

export const processWithdrawalEvent = (
  optWithdrawalRootProgram: Option.Option<UserEventMptsUpdate>,
): Effect.Effect<
  {
    withdrawalsRoot: string;
    withdrawalLedgerUTxOUpdate: LedgerUTxOUpdate;
    sizeOfWithdrawalsTxs: number;
  },
  MptError,
  Database
> =>
  Effect.gen(function* () {
    const withdrawalsRoot: string = yield* Option.match(
      optWithdrawalRootProgram,
      {
        onNone: () => emptyRootHexProgram,
        onSome: (p) =>
          Effect.gen(function* ($) {
            const withdrawalsRootFiber = yield* $(
              Effect.fork(p.getTxRootProgram),
            );
            const withdrawalRoot = yield* $(withdrawalsRootFiber);
            return withdrawalRoot;
          }),
      },
    );
    const withdrawalLedgerUTxOUpdate = (ledgerTrie: MidgardMpt) =>
      Option.match(optWithdrawalRootProgram, {
        onNone: () => Effect.succeed({ spentUTxOs: [], producedUTxOs: [] }),
        onSome: (p) => p.updateLedgerUTxOsProgram(ledgerTrie),
      });
    const sizeOfWithdrawalsTxs = yield* Option.match(optWithdrawalRootProgram, {
      onNone: () => Effect.succeed(0),
      onSome: (p) => Effect.succeed(p.sizeOfProcessedTxs),
    });
    yield* Effect.logInfo(`🔹 Withdrawal root is: ${withdrawalsRoot}`);
    return {
      withdrawalsRoot,
      withdrawalLedgerUTxOUpdate,
      sizeOfWithdrawalsTxs,
    };
  });

export const processDepositEvent = (
  optDepositsRootProgram: Option.Option<UserEventMptsUpdate>,
): Effect.Effect<
  {
    depositsRoot: string;
    depositLedgerUTxOUpdate: LedgerUTxOUpdate;
    sizeOfDepositTxs: number;
  },
  MptError,
  Database
> =>
  Effect.gen(function* () {
    const depositsRoot: string = yield* Option.match(optDepositsRootProgram, {
      onNone: () => emptyRootHexProgram,
      onSome: (p) =>
        Effect.gen(function* ($) {
          const depositsRootFiber = yield* $(Effect.fork(p.getTxRootProgram));
          const depositRoot = yield* $(depositsRootFiber);
          return depositRoot;
        }),
    });
    const depositLedgerUTxOUpdate = (ledgerTrie: MidgardMpt) =>
      Option.match(optDepositsRootProgram, {
        onNone: () => Effect.succeed({ spentUTxOs: [], producedUTxOs: [] }),
        onSome: (p) => p.updateLedgerUTxOsProgram(ledgerTrie),
      });
    const sizeOfDepositTxs = yield* Option.match(optDepositsRootProgram, {
      onNone: () => Effect.succeed(0),
      onSome: (p) => Effect.succeed(p.sizeOfProcessedTxs),
    });
    yield* Effect.logInfo(`🔹 Deposits root is: ${depositsRoot}`);
    return { depositsRoot, depositLedgerUTxOUpdate, sizeOfDepositTxs };
  });

/**
 * Given the target user event table, this helper finds all the events falling
 * in the given time range and if any was found, returns an two `Effect`s and a
 * size of processed txs.
 * First effect finds the tx MPT root of those events, and returns spent and
 * produced UTxOs.
 * Second effect updates UTxO set of the ledger trie.
 */
export const userEventsProgram = (
  tableName: string,
  startDate: Date,
  endDate: Date,
): Effect.Effect<Option.Option<UserEventMptsUpdate>, DatabaseError, Database> =>
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
      const getTxRootProgram = keyValueMptRoot(eventIDs, eventInfos);
      let sizeOfProcessedTxs = 0;
      const updateLedgerUTxOsProgram = (ledgerTrie: MidgardMpt) =>
        Effect.gen(function* () {
          const utxoBatchDBOps: ETH_UTILS.BatchDBOp[] = [];
          const spentUTxOs: Buffer[] = [];
          const producedUTxOs: LedgerUtils.Entry[] = [];
          yield* Effect.forEach(events, (entry: UserEvents.Entry) =>
            Effect.gen(function* () {
              const txCbor = entry[UserEvents.Columns.INFO];
              const { spent, produced } = yield* breakDownTx(txCbor).pipe(
                Effect.withSpan("findSpentAndProducedUTxOs"),
              );
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
              yield* Effect.sync(() => spentUTxOs.push(...spent));
              yield* Effect.sync(() => producedUTxOs.push(...produced));
            }),
          );
          yield* ledgerTrie.batch(utxoBatchDBOps);
          return { spentUTxOs, producedUTxOs };
        });
      return Option.some({
        getTxRootProgram,
        updateLedgerUTxOsProgram,
        sizeOfProcessedTxs,
      });
    }
  });

export type UserEventMptsUpdate = {
  getTxRootProgram: Effect.Effect<string, MptError>;
  updateLedgerUTxOsProgram: LedgerUTxOUpdate;
  sizeOfProcessedTxs: number;
};

export type LedgerUTxOUpdate = (
  ledgerTrie: MidgardMpt,
) => Effect.Effect<
  { spentUTxOs: Buffer[]; producedUTxOs: LedgerUtils.Entry[] },
  SDK.CmlDeserializationError | MptError
>;
