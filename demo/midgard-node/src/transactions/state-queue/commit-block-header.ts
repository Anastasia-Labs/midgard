// Build a tx Merkle root with all the mempool txs

import { makeConfig } from "@/config.js";
import { ImmutableDB, LatestLedgerDB, MempoolDB } from "@/database/index.js";
import { makeAlwaysSucceedsServiceFn } from "@/services/always-succeeds.js";
import { findAllSpentAndProducedUTxOs } from "@/utils.js";
import * as SDK from "@al-ft/midgard-sdk";
import { LucidEvolution } from "@lucid-evolution/lucid";
import { Effect, Metric } from "effect";
import pg from "pg";
import { handleSignSubmitWithoutConfirmation } from "../utils.js";

const commitBlockNumTxGauge = Metric.gauge("commit_block_num_tx_count", {
  description:
    "A gauge for tracking the current number of transactions in the commit block",
  bigint: true,
});

const totalTxSizeGauge = Metric.gauge("total_tx_size", {
  description:
    "A gauge for tracking the total size of transactions in the commit block",
});

const commitBlockCounter = Metric.counter("commit_block_count", {
  description: "A counter for tracking the number of committed blocks",
  bigint: true,
  incremental: true,
});

const commitBlockTxCounter = Metric.counter("commit_block_tx_count", {
  description:
    "A counter for tracking the number of transactions in the commit block",
  bigint: true,
  incremental: true,
});

const commitBlockTxSizeGauge = Metric.gauge("commit_block_tx_size", {
  description: "A gauge for tracking the size of the commit block transaction",
});

// Apply mempool txs to LatestLedgerDB, and find the new UTxO set

// Update LatestLedgerDB to store this updated set

// Clear MempoolDB, and inject all the processed txs into ImmutableDB

// Build a Merkle root using this updated UTxO set

// Build and submit the commitment block using these 2 roots

export const buildAndSubmitCommitmentBlock = (
  lucid: LucidEvolution,
  db: pg.Pool,
  fetchConfig: SDK.TxBuilder.StateQueue.FetchConfig,
  endTime: number,
) =>
  Effect.gen(function* () {
    Effect.logInfo("buildAndSubmitCommitmentBlock... :>> ");
    // Fetch transactions from the first block
    const txList = yield* Effect.tryPromise(() => MempoolDB.retrieve(db)).pipe(
      Effect.withSpan("retrieve mempool transaction"),
    );
    const numTx = BigInt(txList.length);
    // console.log("numTx :>> ", numTx);
    if (numTx > 0n) {
      const txs = txList.map(([txHash, txCbor]) => ({ txHash, txCbor }));
      const txRoot = yield* SDK.Utils.mptFromList(
        txs.map((tx) => tx.txCbor),
      ).pipe(Effect.withSpan("build MPT tx root"));
      const txCbors = txList.map(([_txHash, txCbor]) => txCbor);
      const { spent: spentList, produced: producedList } =
        yield* findAllSpentAndProducedUTxOs(txCbors).pipe(
          Effect.withSpan("findAllSpentAndProducedUTxOs"),
        );
      const utxoList = yield* Effect.tryPromise(() =>
        LatestLedgerDB.retrieve(db),
      ).pipe(Effect.withSpan("retrieve latest ledger utxo list"));
      // Remove spent UTxOs from utxoList
      const filteredUTxOList = utxoList.filter(
        (utxo) =>
          !spentList.some(
            (spent) =>
              spent.txHash === utxo.txHash &&
              spent.outputIndex === utxo.outputIndex,
          ),
      );

      // Merge filtered utxoList with producedList
      const newUTxOList = [...filteredUTxOList, ...producedList].map(
        (utxo) => utxo.txHash + utxo.outputIndex,
      );

      const utxoRoot = yield* SDK.Utils.mptFromList(newUTxOList);
      const nodeConfig = yield* makeConfig;
      const { policyId, spendScript, mintScript } =
        yield* makeAlwaysSucceedsServiceFn(nodeConfig);
      // Build commitment block
      const commitBlockParams: SDK.TxBuilder.StateQueue.CommitBlockParams = {
        newUTxOsRoot: utxoRoot.hash.toString("hex"),
        transactionsRoot: txRoot.hash.toString("hex"),
        endTime: BigInt(endTime),
        stateQueueSpendingScript: spendScript,
        policyId,
        stateQueueMintingScript: mintScript,
      };
      const aoUpdateCommitmentTimeParams = {};
      const txBuilder = yield* SDK.Endpoints.commitBlockHeaderProgram(
        lucid,
        fetchConfig,
        commitBlockParams,
        aoUpdateCommitmentTimeParams,
      );
      const txSize = txBuilder.toCBOR().length / 2;
      // console.log("txBuilder.toCBOR() :>> ", txBuilder.toCBOR());
      console.log("txSize :>> ", txSize);
      // Submit the transaction
      yield* handleSignSubmitWithoutConfirmation(lucid, txBuilder).pipe(
        Effect.withSpan("handleSignSubmit-commit-block"),
      );
      const totalTxSize = txCbors.reduce(
        (acc, cbor) => acc + cbor.length / 2,
        0,
      );
      yield* commitBlockTxSizeGauge(Effect.succeed(txSize));
      yield* commitBlockNumTxGauge(Effect.succeed(numTx));
      yield* Metric.increment(commitBlockCounter);
      yield* Metric.incrementBy(commitBlockTxCounter, numTx);
      yield* totalTxSizeGauge(Effect.succeed(totalTxSize));
      // console.log("spentList.length :>> ", spentList.length);
      const bs = 100;
      for (let i = 0; i < spentList.length; i += bs) {
        yield* Effect.tryPromise(() =>
          LatestLedgerDB.clearUTxOs(db, spentList.slice(i, i + bs)),
        ).pipe(Effect.withSpan(`latest-ledger-clearUTxOs-${i}`));
      }
      for (let i = 0; i < producedList.length; i += bs) {
        yield* Effect.tryPromise(() =>
          LatestLedgerDB.insert(db, producedList.slice(i, i + bs)),
        ).pipe(Effect.withSpan(`latest-ledger-insert-${i}`));
      }
      for (let i = 0; i < txs.length; i += bs) {
        yield* Effect.tryPromise(() =>
          ImmutableDB.insertTxs(db, txs.slice(i, i + bs)),
        ).pipe(Effect.withSpan(`immutable-db-insert-${i}`));
      }

      const txHashes = txs.map((tx) => tx.txHash);
      yield* Effect.tryPromise(() => MempoolDB.clearTxs(db, txHashes)).pipe(
        Effect.withSpan("clear mempool"),
      );
      // TODO: For final product, handle tx submission failures properly.
      // yield* Effect.tryPromise({
      //   try: () =>
      //     UtilsDB.modifyMultipleTables(
      //       db,
      //       [LatestLedgerDB.clearUTxOs, spentList],
      //       [LatestLedgerDB.insert, producedList],
      //       [MempoolDB.clear],
      //       [ImmutableDB.insertTxs, txs]
      //     ),
      //   catch: (e) => new Error(`Transaction failed: ${e}`),
      // });
    }
  });
