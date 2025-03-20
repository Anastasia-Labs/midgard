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
import { fetchFirstBlockTxs, handleSignSubmit } from "../utils.js";
import { findAllSpentAndProducedUTxOs } from "@/utils.js";
import { BlocksDB, ConfirmedLedgerDB } from "@/database/index.js";
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
 * @param spendScript - State queue's spending script.
 * @param mintScript - State queue's minting script.
 * @returns An Effect that resolves when the merge transaction is built and submitted.
 */
export const buildAndSubmitMergeTx = (
  lucid: LucidEvolution,
  db: pg.Pool,
  fetchConfig: SDK.TxBuilder.StateQueue.FetchConfig,
  spendScript: Script,
  mintScript: Script,
) =>
  Effect.gen(function* () {
    yield* Effect.logInfo(
      "🔸 Fetching confirmed state and the first block in queue from L1...",
    );
    const { confirmed: confirmedUTxO, link: firstBlockUTxO } =
      yield* SDK.Endpoints.fetchConfirmedStateAndItsLinkProgram(
        lucid,
        fetchConfig,
      );
    if (firstBlockUTxO) {
      yield* Effect.logInfo(
        `🔸 First block found: ${firstBlockUTxO.txHash}#${firstBlockUTxO.outputIndex}`,
      );
      // Fetch transactions from the first block
      yield* Effect.logInfo("🔸 Looking up its transactions from BlocksDB...");
      const { txs: firstBlockTxs, headerHash } = yield* fetchFirstBlockTxs(
        firstBlockUTxO,
        db,
      ).pipe(Effect.withSpan("fetchFirstBlockTxs"));
      if (firstBlockTxs.length === 0) {
        yield* Effect.logInfo(
          "🔸 ❌ Failed to find first block's transactions in BlocksDB.",
        );
        return;
      }
      yield* Effect.logInfo("🔸 Building merge transaction...");
      // Build the transaction
      const txBuilder = yield* SDK.Endpoints.mergeToConfirmedStateProgram(
        lucid,
        fetchConfig,
        {
          confirmedUTxO,
          firstBlockUTxO,
          stateQueueSpendingScript: spendScript,
          stateQueueMintingScript: mintScript,
        },
      ).pipe(Effect.withSpan("mergeToConfirmedStateProgram"));

      // Increment the merge block counter before successful L1 as it seems
      // confirmation can timeout.
      yield* Metric.increment(mergeBlockCounter).pipe(
        Effect.withSpan("increment-merge-block-counter"),
      );

      // Submit the transaction
      yield* handleSignSubmit(lucid, txBuilder).pipe(
        Effect.withSpan("handleSignSubmit-merge-tx"),
      );
      yield* Effect.logInfo(
        "🔸 Merge transaction submitted, updating the db...",
      );
      if (firstBlockTxs.length === 0) {
        return;
      }
      const { spent: spentOutRefs, produced: producedUTxOs } =
        yield* findAllSpentAndProducedUTxOs(firstBlockTxs).pipe(
          Effect.withSpan("findAllSpentAndProducedUTxOs"),
        );

      // - Clear all the spent UTxOs from the confirmed ledger
      // - Add all the produced UTxOs from the confirmed ledger
      // - Remove all the tx hashes of the merged block from BlocksDB
      const bs = 100;
      yield* Effect.logInfo("🔸 Clear confirmed ledger db...");
      for (let i = 0; i < spentOutRefs.length; i += bs) {
        yield* Effect.tryPromise(() =>
          ConfirmedLedgerDB.clearUTxOs(db, spentOutRefs.slice(i, i + bs)),
        ).pipe(Effect.withSpan(`confirmed-ledger-clearUTxOs-${i}`));
      }
      yield* Effect.logInfo("🔸 Insert produced UTxOs...");
      for (let i = 0; i < producedUTxOs.length; i += bs) {
        yield* Effect.tryPromise(() =>
          ConfirmedLedgerDB.insert(db, producedUTxOs.slice(i, i + bs)),
        ).pipe(Effect.withSpan(`confirmed-ledger-insert-${i}`));
      }
      yield* Effect.logInfo("🔸 Clear block from BlocksDB...");
      yield* Effect.tryPromise(() => BlocksDB.clearBlock(db, headerHash)).pipe(
        Effect.withSpan("clear-block-from-BlocksDB"),
      );
      yield* Effect.logInfo("🔸 ☑️  Merge transaction completed.");
    } else {
      yield* Effect.logInfo("🔸 No blocks found in queue.");
      return;
    }
  });
