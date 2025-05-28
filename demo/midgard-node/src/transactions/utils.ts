import * as SDK from "@al-ft/midgard-sdk";
import {
  LucidEvolution,
  OutRef,
  TxSignBuilder,
  UTxO,
} from "@lucid-evolution/lucid";
import { Effect, Schedule } from "effect";
import * as BlocksDB from "../database/blocks.js";
import * as ImmutableDB from "../database/immutable.js";
import { Database } from "@/services/database.js";

/**
 * Handle the signing and submission of a transaction.
 *
 * @param lucid - The LucidEvolution instance.
 * @param signBuilder - The transaction sign builder.
 * @returns An Effect that resolves when the transaction is signed, submitted, and confirmed.
 */
export const handleSignSubmit = (
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
): Effect.Effect<string, Error> =>
  Effect.gen(function* () {
    const signed = yield* signBuilder.sign.withWallet().completeProgram();
    yield* Effect.logInfo("✉️  Submitting transaction...");
    const txHash = yield* signed
      .submitProgram()
      .pipe(
        Effect.retry(
          Schedule.compose(Schedule.exponential(5_000), Schedule.recurs(5)),
        ),
      );
    yield* Effect.logInfo(`🚀 Transaction submitted: ${txHash}`);
    yield* Effect.logInfo(`⏳ Confirming Transaction...`);
    yield* Effect.tryPromise(() => lucid.awaitTx(txHash, 10_000));
    yield* Effect.logInfo(`🎉 Transaction confirmed: ${txHash}`);
    yield* Effect.logInfo("⌛ Pausing for 10 seconds...");
    yield* Effect.sleep("10 seconds");
    yield* Effect.logInfo("✅ Pause ended.");
    return txHash;
  });

/**
 * Handle the signing and submission of a transaction without waiting for the transaction to be confirmed.
 *
 * @param signBuilder - The transaction sign builder.
 * @returns An Effect that resolves when the transaction is signed, submitted, and confirmed.
 */
export const handleSignSubmitWithoutConfirmation = (
  signBuilder: TxSignBuilder,
): Effect.Effect<string, Error> =>
  Effect.gen(function* () {
    const signed = yield* signBuilder.sign.withWallet().completeProgram();
    const txHash = yield* signed
      .submitProgram()
      .pipe(
        Effect.retry(
          Schedule.compose(Schedule.exponential(5_000), Schedule.recurs(5)),
        ),
      );
    yield* Effect.logDebug(`🚀 Transaction submitted: ${txHash}`);
    return txHash;
  });

/**
 * Fetch transactions of the first block by querying BlocksDB and ImmutableDB.
 *
 * @param firstBlockUTxO - UTxO of the first block in queue.
 * @param db - The database instance.
 * @returns An Effect that resolves to an array of transactions.
 */
export const fetchFirstBlockTxs = (
  firstBlockUTxO: UTxO,
): Effect.Effect<{ txs: string[]; headerHash: string }, Error, Database> =>
  Effect.gen(function* () {
    const blockHeader = yield* SDK.Utils.getHeaderFromBlockUTxO(firstBlockUTxO);
    const headerHash = yield* SDK.Utils.hashHeader(blockHeader);
    const txHashes = yield* BlocksDB.retrieveTxHashesByBlockHash(headerHash);
    const txs = yield* ImmutableDB.retrieveTxCborsByHashes(txHashes);
    return { txs, headerHash };
  });

export const utxoToOutRef = (utxo: UTxO): OutRef => ({
  txHash: utxo.txHash,
  outputIndex: utxo.outputIndex,
});

export const outRefsAreEqual = (outRef0: OutRef, outRef1: OutRef): boolean => {
  return (
    outRef0.txHash === outRef1.txHash &&
    outRef0.outputIndex === outRef1.outputIndex
  );
};
