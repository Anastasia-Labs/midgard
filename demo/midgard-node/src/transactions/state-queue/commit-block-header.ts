// Build a tx Merkle root with all the mempool txs

import { makeConfig } from "@/config.js";
import { LatestLedgerDB, MempoolDB } from "@/database/index.js";
import * as DB from "@/database/transaction.js";
import { makeAlwaysSucceedsServiceFn } from "@/services/always-succeeds.js";
import { findAllSpentAndProducedUTxOs } from "@/utils.js";
import * as SDK from "@al-ft/midgard-sdk";
import { LucidEvolution } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { Database } from "sqlite3";
import { parentPort } from "worker_threads";
import { handleSignSubmit } from "../utils.js";

// Apply mempool txs to LatestLedgerDB, and find the new UTxO set

// Update LatestLedgerDB to store this updated set

// Clear MempoolDB, and inject all the processed txs into ImmutableDB

// Build a Merkle root using this updated UTxO set

// Build and submit the commitment block using these 2 roots

export const buildAndSubmitCommitmentBlock = (
  lucid: LucidEvolution,
  db: Database,
  fetchConfig: SDK.TxBuilder.StateQueue.FetchConfig,
  endTime: number
) =>
  Effect.gen(function* () {
    // Fetch transactions from the first block
    const txList = yield* Effect.tryPromise(() => MempoolDB.retrieve(db));
    const numTx = BigInt(txList.length);
    parentPort?.postMessage({
      type: "mempool-metrics",
      data: {
        numTx,
      },
    });
    if (numTx > 0n) {
      const txs = txList.map(([txHash, txCbor]) => ({ txHash, txCbor }));
      const txRoot = yield* SDK.Utils.mptFromList(txs.map((tx) => tx.txCbor));
      const txCbors = txList.map(([_txHash, txCbor]) => txCbor);
      const { spent: spentList, produced: producedList } =
        yield* findAllSpentAndProducedUTxOs(txCbors);
      const utxoList = yield* Effect.tryPromise(() =>
        LatestLedgerDB.retrieve(db)
      );
      // Remove spent UTxOs from utxoList
      const filteredUTxOList = utxoList.filter(
        (utxo) =>
          !spentList.some(
            (spent) =>
              spent.txHash === utxo.txHash &&
              spent.outputIndex === utxo.outputIndex
          )
      );

      // Merge filtered utxoList with producedList
      const newUTxOList = [...filteredUTxOList, ...producedList].map(
        (utxo) => utxo.txHash + utxo.outputIndex
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
        aoUpdateCommitmentTimeParams
      );
      const txSize = txBuilder.toCBOR().length / 2;
      // console.log("txBuilder.toCBOR() :>> ", txBuilder.toCBOR());
      console.log("txSize :>> ", txSize);
      // Submit the transaction
      yield* handleSignSubmit(lucid, txBuilder);
      const totalTxSize = txCbors.reduce(
        (acc, cbor) => acc + cbor.length / 2,
        0
      );
      parentPort?.postMessage({
        type: "commit-block-metrics",
        data: {
          txSize,
          numTx,
          totalTxSize,
        },
      });
      // TODO: For final product, handle tx submission failures properly.
      yield* Effect.tryPromise({
        try: () => DB.submitBlock(db, spentList, producedList, txs),
        catch: (e) => new Error(`Transaction failed: ${e}`),
      });
    }
  });
