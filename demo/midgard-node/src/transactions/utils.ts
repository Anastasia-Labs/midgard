import * as SDK from "@al-ft/midgard-sdk";
import {
  LucidEvolution,
  OutRef,
  TxSignBuilder,
  UTxO,
  fromHex,
} from "@lucid-evolution/lucid";
import { Data, Effect, pipe, Schedule } from "effect";
import * as BlocksDB from "../database/blocks.js";
import { Database } from "@/services/database.js";
import { ImmutableDB } from "@/database/index.js";

const RETRY_ATTEMPTS = 1;

const INIT_RETRY_AFTER_MILLIS = 2_000;

const PAUSE_DURATION = "5 seconds";

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
  onSubmitFailure: (error: SubmitError) => Effect.Effect<void, Error>,
  onConfirmFailure: (error: ConfirmError) => Effect.Effect<void, Error>,
): Effect.Effect<string | void, Error> =>
  Effect.gen(function* () {
    const txHash = yield* signSubmitHelper(lucid, signBuilder);
    yield* Effect.logInfo(`⏳ Confirming Transaction...`);
    yield* Effect.tryPromise({
      try: () => lucid.awaitTx(txHash, 10_000),
      catch: (err: any) => new ConfirmError({ err, txHash }),
    });
    yield* Effect.logInfo(`🎉 Transaction confirmed: ${txHash}`);
    yield* Effect.logInfo(`⌛ Pausing for ${PAUSE_DURATION}...`);
    yield* Effect.sleep(PAUSE_DURATION);
    yield* Effect.logInfo("✅ Pause ended.");
    return txHash;
  }).pipe(
    Effect.catchAll((err: HandleSignSubmitError) => {
      switch (err._tag) {
        case "SubmitError":
          return onSubmitFailure(err);
        case "ConfirmError":
          return onConfirmFailure(err);
        case "SignError":
          return pipe(
            Effect.logError(`Signing tx error: ${err.err}`),
            Effect.flatMap(() => Effect.fail(err)),
          );
      }
    }),
  );

/**
 * Handle the signing and submission of a transaction without waiting for the transaction to be confirmed.
 *
 * @param lucid - The LucidEvolution instance. Here it's only used for logging the signer's address.
 * @param signBuilder - The transaction sign builder.
 * @returns An Effect that resolves when the transaction is signed, submitted, and confirmed.
 */
export const handleSignSubmitNoConfirmation = (
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
  onSubmitFailure: (error: SubmitError) => Effect.Effect<void, Error>,
): Effect.Effect<string | void, Error> =>
  Effect.gen(function* () {
    const txHash = yield* signSubmitHelper(lucid, signBuilder);
    return txHash;
  }).pipe(
    Effect.catchAll((err: SignError | SubmitError) =>
      err._tag === "SubmitError"
        ? onSubmitFailure(err)
        : pipe(
            Effect.logError(`Signing tx error: ${err.err}`),
            Effect.flatMap(() => Effect.fail(err)),
          ),
    ),
  );

const signSubmitHelper = (
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
): Effect.Effect<string, SignError | SubmitError> =>
  Effect.gen(function* () {
    const walletAddr = yield* Effect.tryPromise({
      try: () => lucid.wallet().address(),
      catch: (e) => new Error(`${e}`),
    }).pipe(Effect.catchAll((_e) => Effect.succeed("<unknown>")));
    yield* Effect.logInfo(`✍  Signing tx with ${walletAddr}...`);
    const signed = yield* signBuilder.sign
      .withWallet()
      .completeProgram()
      .pipe(Effect.mapError((err) => new SignError({ err })));
    yield* Effect.logInfo(`Signed tx CBOR is:
${signed.toCBOR()}
`);
    yield* Effect.logInfo("✉️  Submitting transaction...");
    const txHash = yield* signed.submitProgram().pipe(
      Effect.retry(
        Schedule.compose(
          Schedule.exponential(INIT_RETRY_AFTER_MILLIS),
          Schedule.recurs(RETRY_ATTEMPTS),
        ),
      ),
      Effect.mapError((err) => new SubmitError({ err })),
    );
    yield* Effect.logInfo(`🚀 Transaction submitted: ${txHash}`);
    return txHash;
  });

export type HandleSignSubmitError = SignError | SubmitError | ConfirmError;

export class SignError extends Data.TaggedError("SignError")<{
  readonly err: Error;
}> {}

export class SubmitError extends Data.TaggedError("SubmitError")<{
  readonly err: Error;
}> {}

export class ConfirmError extends Data.TaggedError("ConfirmError")<{
  readonly err: Error;
  readonly txHash: string;
}> {}

/**
 * Fetch transactions of the first block by querying BlocksDB and ImmutableDB.
 *
 * @param firstBlockUTxO - UTxO of the first block in queue.
 * @returns An Effect that resolves to an array of transactions, and block's
 *          header hash.
 */
export const fetchFirstBlockTxs = (
  firstBlockUTxO: SDK.TxBuilder.StateQueue.StateQueueUTxO,
): Effect.Effect<
  { txs: readonly Buffer[]; headerHash: Buffer },
  Error,
  Database
> =>
  Effect.gen(function* () {
    const blockHeader = yield* SDK.Utils.getHeaderFromStateQueueDatum(
      firstBlockUTxO.datum,
    );
    const headerHash = yield* SDK.Utils.hashHeader(blockHeader).pipe(
      Effect.map((hh) => Buffer.from(fromHex(hh))),
    );
    const txHashes = yield* BlocksDB.retrieveTxHashesByHeaderHash(headerHash);
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
