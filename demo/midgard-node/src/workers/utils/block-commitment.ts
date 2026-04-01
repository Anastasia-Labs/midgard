import { Effect } from "effect";
import * as SDK from "@al-ft/midgard-sdk";
import { LucidEvolution, fromHex } from "@lucid-evolution/lucid";
import * as ETH_UTILS from "@ethereumjs/util";
import { MidgardMpt, MptError } from "./mpt.js";
import {
  DepositsDB,
  Tx,
  UserEvents,
  Ledger,
  WithdrawalsDB,
  BlocksDB,
} from "@/database/index.js";
import {
  DatabaseError,
  deserializeUTxOsFromStorage,
  serializeUTxOsForStorage,
} from "@/database/utils/common.js";
import {
  AlwaysSucceedsContract,
  Database,
  Lucid,
  NodeConfig,
} from "@/services/index.js";
import { TxSignError } from "@/transactions/utils.js";
import { breakDownTx } from "@/utils.js";

export type WorkerInput = {
  data: {};
};

export type SuccessfulCommitmentOutput = {
  type: "SuccessfulCommitmentOutput";
  stats: BlocksDB.Stats;
};

export type FailureOutput = {
  type: "FailureOutput";
  error: string;
};

export type SeededOutput = {
  type: "SeededOutput";
};

export type WorkerOutput =
  | SuccessfulCommitmentOutput
  | FailureOutput
  | SeededOutput;

const txEntryToBatchDBOps = (
  txCbor: Buffer,
): Effect.Effect<
  {
    spent: Buffer[];
    produced: Ledger.Entry[];
    delOps: ETH_UTILS.BatchDBOp[];
    putOps: ETH_UTILS.BatchDBOp[];
  },
  SDK.CmlDeserializationError
> =>
  Effect.gen(function* () {
    const { spent, produced } = yield* breakDownTx(txCbor).pipe(
      Effect.withSpan("breakDownTx"),
    );
    const delOps: ETH_UTILS.BatchDBOp[] = spent.map((outRef: Buffer) => ({
      type: "del",
      key: outRef,
    }));
    const putOps: ETH_UTILS.BatchDBOp[] = produced.map((le: Ledger.Entry) => ({
      type: "put",
      key: le[Ledger.Columns.OUTREF],
      value: le[Ledger.Columns.OUTPUT],
    }));
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
    producedByTxOrders: Ledger.Entry[];
    txsTrie: MidgardMpt;
    sizeOfTxOrders: number;
  },
  SDK.CmlDeserializationError | MptError | DatabaseError,
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
    const producedByTxOrders: Ledger.Entry[] = [];
    const txOrdersLedgerBatchOps: ETH_UTILS.BatchDBOp[] = [];
    const txsTrie: MidgardMpt = yield* MidgardMpt.create("txs");

    yield* Effect.forEach(txOrders, (txOrder: UserEvents.Entry) =>
      Effect.gen(function* () {
        const txHash = txOrder[UserEvents.Columns.ID];
        const txCbor = txOrder[UserEvents.Columns.INFO];
        const { delOps, putOps, spent, produced } =
          yield* txEntryToBatchDBOps(txCbor);
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
  SDK.CmlDeserializationError | MptError
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
        const { delOps, putOps } = yield* txEntryToBatchDBOps(txCbor);
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
  stats: BlocksDB.Stats,
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
      ...stats,
      [BlocksDB.Columns.HEADER_HASH]: Buffer.from(fromHex(newHeaderHash)),
      [BlocksDB.Columns.EVENT_START_TIME]:
        entry[BlocksDB.Columns.EVENT_END_TIME],
      [BlocksDB.Columns.EVENT_END_TIME]: endDate,
      [BlocksDB.Columns.NEW_WALLET_UTXOS]: serializedNewWalletUTxOs,
      [BlocksDB.Columns.PRODUCED_UTXOS]: serializedProducedUTxOs,
      [BlocksDB.Columns.L1_CBOR]: l1CBOR,
      [BlocksDB.Columns.STATUS]: BlocksDB.Status.UNSUBMITTED,
    };
    return newBlockEntry;
  });
