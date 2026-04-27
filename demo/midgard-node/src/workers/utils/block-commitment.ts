import { Effect } from "effect";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Data as LucidData,
  coreToTxOutput,
  fromHex,
  toUnit,
} from "@lucid-evolution/lucid";
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
  serializeUTxOsForStorage,
} from "@/database/utils/common.js";
import {
  MidgardContracts,
  Database,
  Lucid,
} from "@/services/index.js";
import { TxSignError, TxSubmitError } from "@/transactions/utils.js";
import { breakDownTx } from "@/utils.js";
import { fetchRealStateQueueWitnessContext } from "@/workers/utils/scheduler-refresh.js";
import { buildDeterministicCommitTxBuilder } from "@/workers/utils/commit-tx-builder.js";
import { resolveAlignedCommitEndTime } from "@/workers/utils/commit-end-time.js";
import { extractWalletOutputsFromSubmittedTx } from "@/operator-wallet-view.js";

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
      `🔹 Going through mempool and processing (${mempoolTxs.length}) transactions...`,
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
  deposits: readonly DepositsDB.Entry[],
): Effect.Effect<
  {
    depositLedgerEntries: Ledger.Entry[];
    depositsRoot: string;
    sizeOfDeposits: number;
  },
  MptError | DatabaseError
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
        const ledgerEntry = yield* DepositsDB.toLedgerEntry(depositEntry);
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

const ACTIVE_OPERATOR_MATURITY_DURATION_MS = 30n;
const STATE_QUEUE_HEADER_NODE_LOVELACE = 5_000_000n;

const decodeActiveOperatorDatum = (data: unknown): SDK.ActiveOperatorDatum =>
  LucidData.castFrom(data as never, SDK.ActiveOperatorDatum as never) as SDK.ActiveOperatorDatum;

const getLatestEndTimeMs = (
  datum: SDK.StateQueueNodeView,
): Effect.Effect<number, SDK.DataCoercionError> =>
  datum.key === "Empty"
    ? SDK.getConfirmedStateFromStateQueueDatum(datum).pipe(
        Effect.map(({ data }) => Number(data.endTime)),
      )
    : SDK.getHeaderFromStateQueueDatum(datum).pipe(
        Effect.map((h) => Number(h.endTime)),
      );

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
  | TxSignError
  | TxSubmitError,
  MidgardContracts | Lucid
> =>
  Effect.gen(function* () {
    const appendedUTxO = yield* BlocksDB.getAppendedStateQueueUTxOFromEntry(entry);
    const lucidService = yield* Lucid;
    yield* lucidService.switchToOperatorsBlockCommitmentWallet;
    const lucidAPI = lucidService.api;

    const contracts = yield* MidgardContracts;
    const stateQueue = contracts.stateQueue;

    const latestEndTimeMs = yield* getLatestEndTimeMs(appendedUTxO.datum);
    const { resolvedEndTime: alignedEndTime } = resolveAlignedCommitEndTime({
      lucid: lucidAPI,
      latestEndTime: latestEndTimeMs,
      candidateEndTime: endDate.getTime(),
    });

    yield* Effect.logInfo("🔹 Fetching real state_queue witness context...");
    const witnessContext = yield* fetchRealStateQueueWitnessContext(
      lucidAPI,
      contracts,
      alignedEndTime,
      undefined,
      lucidService.referenceScriptsAddress,
    );

    yield* Effect.logInfo("🔹 Finding updated block datum and new header...");
    const { nodeDatum: updatedNodeDatum, header: newHeader } =
      yield* SDK.updateLatestBlocksDatumAndGetTheNewHeaderProgram(
        lucidAPI,
        appendedUTxO.datum,
        utxosRoot,
        txsRoot,
        depositsRoot,
        withdrawalsRoot,
        BigInt(alignedEndTime),
      );
    const newHeaderHash = yield* SDK.hashBlockHeader(newHeader);
    yield* Effect.logInfo(`🔹 New header hash is: ${newHeaderHash}`);

    const headerNodeUnit = toUnit(
      stateQueue.policyId,
      SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + newHeaderHash,
    );
    const commitMintAssets = { [headerNodeUnit]: 1n };
    const headerNodeOutputAssets = {
      lovelace: STATE_QUEUE_HEADER_NODE_LOVELACE,
      ...commitMintAssets,
    };

    const appendedNodeDatum: SDK.StateQueueNodeView = {
      key: updatedNodeDatum.next,
      next: "Empty",
      data: SDK.castHeaderToData(newHeader) as SDK.LinkedListNodeView["data"],
    };
    const appendedNodeDatumCbor = SDK.encodeLinkedListNodeView(appendedNodeDatum);
    const updatedNodeDatumCbor = SDK.encodeLinkedListNodeView(updatedNodeDatum);

    const updatedActiveOperatorDatumCbor = yield* Effect.try({
      try: () => {
        const activeOperatorLinkedListDatum = LucidData.from(
          witnessContext.activeOperatorInput.datum,
          SDK.NodeDatum,
        );
        const activeOperatorNodeView = SDK.linkedListDatumToNodeView(
          activeOperatorLinkedListDatum,
          SDK.ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX + witnessContext.operatorKeyHash,
        );
        const activeOperatorDatum = decodeActiveOperatorDatum(activeOperatorNodeView.data);
        const updatedActiveOperatorNodeDatum: SDK.LinkedListNodeView = {
          ...activeOperatorNodeView,
          data: SDK.castActiveOperatorDatumToData({
            ...activeOperatorDatum,
            bond_unlock_time:
              BigInt(alignedEndTime) - 1n + ACTIVE_OPERATOR_MATURITY_DURATION_MS,
          }) as SDK.LinkedListNodeView["data"],
        };
        return SDK.encodeLinkedListNodeView(updatedActiveOperatorNodeDatum);
      },
      catch: (cause) =>
        new SDK.StateQueueError({
          message: "Failed to update active-operator bond-hold datum for commit tx",
          cause,
        }),
    });

    const makeBaseCommitTx = (stateQueueCommitRedeemer: string) =>
      lucidAPI
        .newTx()
        .validTo(alignedEndTime)
        .collectFrom([appendedUTxO.utxo], stateQueueCommitRedeemer)
        .pay.ToContract(
          stateQueue.spendingScriptAddress,
          { kind: "inline", value: appendedNodeDatumCbor },
          headerNodeOutputAssets,
        )
        .pay.ToContract(
          stateQueue.spendingScriptAddress,
          { kind: "inline", value: updatedNodeDatumCbor },
          appendedUTxO.utxo.assets,
        );

    yield* Effect.logInfo("🔹 Building block commitment transaction...");
    const txSignBuilder = yield* buildDeterministicCommitTxBuilder({
      lucid: lucidAPI,
      contracts,
      latestBlockInput: appendedUTxO.utxo,
      witness: witnessContext,
      headerNodeUnit,
      appendedNodeDatumCbor,
      previousHeaderNodeDatumCbor: updatedNodeDatumCbor,
      updatedActiveOperatorDatumCbor,
      commitMintAssets,
      makeBaseCommitTx,
    });

    const txHash = txSignBuilder.toHash();
    const signedTx = yield* txSignBuilder.sign
      .withWallet()
      .completeProgram()
      .pipe(
        Effect.mapError(
          (e) =>
            new TxSignError({
              message: "Failed to sign block commitment transaction",
              cause: e,
              txHash,
            }),
        ),
      );

    const cmlTx: CML.Transaction = signedTx.toTransaction();
    const walletAddress = yield* Effect.tryPromise({
      try: () => lucidAPI.wallet().address(),
      catch: (e) =>
        new SDK.LucidError({
          message: "Failed to get block commitment wallet address",
          cause: e,
        }),
    });
    const newWalletUTxOs = extractWalletOutputsFromSubmittedTx(
      cmlTx,
      txHash,
      walletAddress,
    );
    // Collect the new state-queue UTxO (the appended header node) as the produced UTxO
    const outputs = cmlTx.body().outputs();
    const producedUTxOs = [];
    for (let i = 0; i < outputs.len(); i++) {
      const out = coreToTxOutput(outputs.get(i));
      if (
        out.address === stateQueue.spendingScriptAddress &&
        (out.assets[headerNodeUnit] ?? 0n) === 1n
      ) {
        producedUTxOs.push({ txHash, outputIndex: i, ...out });
      }
    }

    const serializedNewWalletUTxOs = yield* serializeUTxOsForStorage(newWalletUTxOs);
    const serializedProducedUTxOs = yield* serializeUTxOsForStorage(producedUTxOs);
    const l1CBOR = Buffer.from(cmlTx.to_cbor_bytes());

    const endTime = new Date(alignedEndTime);
    const newBlockEntry: BlocksDB.EntryNoMeta = {
      ...stats,
      [BlocksDB.Columns.HEADER_HASH]: Buffer.from(fromHex(newHeaderHash)),
      [BlocksDB.Columns.EVENT_START_TIME]: entry[BlocksDB.Columns.EVENT_END_TIME],
      [BlocksDB.Columns.EVENT_END_TIME]: endTime,
      [BlocksDB.Columns.NEW_WALLET_UTXOS]: serializedNewWalletUTxOs,
      [BlocksDB.Columns.PRODUCED_UTXOS]: serializedProducedUTxOs,
      [BlocksDB.Columns.L1_CBOR]: l1CBOR,
      [BlocksDB.Columns.STATUS]: BlocksDB.Status.UNSUBMITTED,
    };
    return newBlockEntry;
  });
