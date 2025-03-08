import {
  LucidEvolution,
  OutRef,
  TxSignBuilder,
  UTxO,
} from "@lucid-evolution/lucid";
import { Effect, Schedule } from "effect";
import * as SDK from "@al-ft/midgard-sdk";
import * as BlocksDB from "../database/blocks.js";
import * as ImmutableDB from "../database/immutable.js";
import pg from "pg";
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
    const txHash = yield* signed
      .submitProgram()
      .pipe(
        Effect.retry(
          Schedule.compose(Schedule.exponential(5_000), Schedule.recurs(5)),
        ),
      );
    yield* Effect.logInfo(`🚀 Transaction submitted: ${txHash}`);
    yield* Effect.logInfo(`Confirming Transaction...`);
    yield* Effect.tryPromise(() => lucid.awaitTx(txHash, 10_000));
    yield* Effect.logInfo(`✅ Transaction confirmed: ${txHash}`);
    yield* Effect.logInfo("Pausing for 10 seconds...");
    yield* Effect.sleep("10 seconds");
    return txHash;
  });

/**
 * Handle the signing and submission of a transaction without waiting for the transaction to be confirmed.
 *
 * @param lucid - The LucidEvolution instance.
 * @param signBuilder - The transaction sign builder.
 * @returns An Effect that resolves when the transaction is signed, submitted, and confirmed.
 */
export const handleSignSubmitWithoutConfirmation = (
  lucid: LucidEvolution,
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
 * @param lucid - The LucidEvolution instance.
 * @param fetchConfig - The configuration for fetching data.
 * @param db - The database instance.
 * @returns An Effect that resolves to an array of transactions.
 */
export const fetchFirstBlockTxs = (
  lucid: LucidEvolution,
  fetchConfig: SDK.TxBuilder.StateQueue.FetchConfig,
  db: pg.Pool,
): Effect.Effect<{ txs: string[]; headerHash: string }, Error> =>
  Effect.gen(function* () {
    const { link: firstBlockUTxO } =
      yield* SDK.Endpoints.fetchConfirmedStateAndItsLinkProgram(
        lucid,
        fetchConfig,
      );
    if (!firstBlockUTxO) {
      return { txs: [], headerHash: "" };
    } else {
      const blockHeader =
        yield* SDK.Utils.getHeaderFromBlockUTxO(firstBlockUTxO);
      const headerHash = yield* SDK.Utils.hashHeader(blockHeader);
      const txHashes = yield* Effect.tryPromise({
        try: () => BlocksDB.retrieveTxHashesByBlockHash(db, headerHash),
        catch: (e) => new Error(`${e}`),
      });
      const txs = yield* Effect.tryPromise({
        try: () => ImmutableDB.retrieveTxCborsByHashes(db, txHashes),
        catch: (e) => {
          return new Error(`${e}`);
        },
      });
      return { txs, headerHash };
    }
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
