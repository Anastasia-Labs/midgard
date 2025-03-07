/**
 * This script performs the following tasks to merge the first block into the confirmed state:
 *
 * 1. Fetch transactions of the first block by querying ImmutableDB.
 * 2. Apply those transactions to ConfirmedLedgerDB and update the table to store the updated UTxO set.
 * 3. Remove all header hashes from BlocksDB associated with the merged block.
 * 4. Build and submit the merge transaction.
 */

import { LucidEvolution, Script } from "@lucid-evolution/lucid";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Metric } from "effect";
import {
  fetchFirstBlockTxs,
  handleSignSubmit,
  handleSignSubmitWithoutConfirmation,
} from "../utils.js";
import { findAllSpentAndProducedUTxOs } from "@/utils.js";
import { BlocksDB, ConfirmedLedgerDB, UtilsDB } from "@/database/index.js";
import { AlwaysSucceeds } from "@/services/index.js";
import { parentPort } from "worker_threads";
import pg from "pg";

const mergeBlockCounter = Metric.counter("merge_block_count", {
  description: "A counter for tracking merge blocks",
  bigint: true,
  incremental: true,
});

/**
 * Build and submit the merge transaction.
 *
 * @param lucid - The LucidEvolution instance.
 * @param db - The database instance.
 * @param fetchConfig - The configuration for fetching data.
 * @param header_hash - The header hash of the block to be merged.
 * @returns An Effect that resolves when the merge transaction is built and submitted.
 */
export const buildAndSubmitMergeTx = (
  lucid: LucidEvolution,
  db: pg.Pool,
  fetchConfig: SDK.TxBuilder.StateQueue.FetchConfig,
  spendScript: Script,
  mintScript: Script,
) =>
  Effect.gen(function* ($) {
    yield* Effect.logInfo("buildAndSubmitMergeTx... :>> ");
    // Fetch transactions from the first block
    const { txs: firstBlockTxs, headerHash } = yield* $(
      fetchFirstBlockTxs(lucid, fetchConfig, db),
    );
    // Build the transaction
    const txBuilder = yield* SDK.Endpoints.mergeToConfirmedStateProgram(
      lucid,
      fetchConfig,
      {
        stateQueueSpendingScript: spendScript,
        stateQueueMintingScript: mintScript,
      },
    );

    // Submit the transaction
    yield* handleSignSubmit(lucid, txBuilder);
    yield* Metric.increment(mergeBlockCounter);
    yield* Effect.logInfo("Merge transaction submitted, updating the db...");
    if (firstBlockTxs.length === 0) {
      return;
    }
    const { spent: spentOutRefs, produced: producedUTxOs } =
      yield* findAllSpentAndProducedUTxOs(firstBlockTxs);

    const bs = 100;
    yield* Effect.logInfo("Clear confirmed ledger db...");
    for (let i = 0; i < spentOutRefs.length; i += bs) {
      yield* Effect.tryPromise(() =>
        ConfirmedLedgerDB.clearUTxOs(db, spentOutRefs.slice(i, i + bs)),
      );
    }
    yield* Effect.logInfo("Insert produced UTxOs...");
    for (let i = 0; i < producedUTxOs.length; i += bs) {
      yield* Effect.tryPromise(() =>
        ConfirmedLedgerDB.insert(db, producedUTxOs.slice(i, i + bs)),
      );
    }
    yield* Effect.logInfo("Clear block from BlocksDB...");
    yield* Effect.tryPromise(() => BlocksDB.clearBlock(db, headerHash));
    yield* Effect.logInfo("Merge transaction completed.");
    // - Clear all the spent UTxOs from the confirmed ledger
    // - Add all the produced UTxOs from the confirmed ledger
    // - Remove all the tx hashes of the merged block from BlocksDB
    // yield* Effect.tryPromise({
    //   try: () =>
    //     UtilsDB.modifyMultipleTables(
    //       db,
    //       [ConfirmedLedgerDB.clearUTxOs, spentOutRefs],
    //       [ConfirmedLedgerDB.insert, producedUTxOs],
    //       [BlocksDB.clearBlock, headerHash]
    //     ),
    //   catch: (e) => new Error(`Transaction failed: ${e}`),
    // });
  });
