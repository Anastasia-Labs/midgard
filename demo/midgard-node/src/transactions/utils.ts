import * as SDK from "@al-ft/midgard-sdk";
import {
  LucidEvolution,
  OutRef,
  TxSignBuilder,
  UTxO,
  fromHex,
} from "@lucid-evolution/lucid";
import { Data, Effect, Schedule } from "effect";
import * as BlocksDB from "../database/blocks.js";
import { Database } from "@/services/index.js";
import { ImmutableDB } from "@/database/index.js";
import { DBSelectError } from "@/database/utils/common.js";
import { UnknownException } from "effect/Cause";

const RETRY_ATTEMPTS = 1;

const INIT_RETRY_AFTER_MILLIS = 2_000;

const PAUSE_DURATION = "5 seconds";

/**
 * Handle the signing and submission of a transaction.
 *
 * (TODO: the type signature for `onSubmitFailure` is unfortunate. How can this
 * be averted?)
 *
 * @param lucid - The LucidEvolution instance.
 * @param signBuilder - The transaction sign builder.
 * @param onSubmitFailure - A function that given a `TxSubmitError` returns another effect
 * @param onConfirmFailure - A function that given a `TxConfirmError` returns another effect
 * @returns An Effect that resolves when the transaction is signed, submitted, and confirmed.
 */
export const handleSignSubmit = <E, F>(
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
  onSubmitFailure: (
    error: TxSubmitError | { _tag: "TxSubmitError" },
  ) => Effect.Effect<void, E>,
  onConfirmFailure: (error: TxConfirmError) => Effect.Effect<void, F>,
): Effect.Effect<string | void, E | F | TxSignError> =>
  Effect.gen(function* () {
    const txHash = yield* signSubmitHelper(lucid, signBuilder);
    yield* Effect.logInfo(`⏳ Confirming Transaction...`);
    const result = yield* Effect.tryPromise(() =>
      lucid.awaitTx(txHash, 10_000),
    ).pipe(
      Effect.matchEffect({
        onFailure: (e: UnknownException) => {
          const confirmError = new TxConfirmError({
            message: `Failed to confirm transaction`,
            txHash,
            cause: e,
          });
          return onConfirmFailure(confirmError);
        },
        onSuccess: (_b) =>
          Effect.gen(function* () {
            yield* Effect.logInfo(`🎉 Transaction confirmed: ${txHash}`);
            yield* Effect.logInfo(`⌛ Pausing for ${PAUSE_DURATION}...`);
            yield* Effect.sleep(PAUSE_DURATION);
            yield* Effect.logInfo("✅ Pause ended.");
            return txHash;
          }),
      }),
    );
    return result;
  }).pipe(
    Effect.tapErrorTag("TxSignError", (e) =>
      Effect.logError(`TxSignError: ${e}`),
    ),
    Effect.catchTag("TxSubmitError", (e) => onSubmitFailure(e)),
  );

/**
 * Handle the signing and submission of a transaction without waiting for the
 * transaction to be confirmed.
 *
 * (TODO: similar to above—type signature of `onSubmitFailure`)
 *
 * @param lucid - The LucidEvolution instance. Here it's only used for logging the signer's address.
 * @param signBuilder - The transaction sign builder.
 * @param onSubmitFailure - A function that given a `TxSubmitError`, returns another effect, allowing for taking control of the logic.
 * @returns An Effect that resolves when the transaction is signed, submitted, and confirmed.
 */
export const handleSignSubmitNoConfirmation = <E>(
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
  onSubmitFailure: (
    error: TxSubmitError | { _tag: "TxSubmitError" },
  ) => Effect.Effect<void, E>,
): Effect.Effect<string | void, TxSignError | E> =>
  Effect.gen(function* () {
    const txHash = yield* signSubmitHelper(lucid, signBuilder);
    return txHash;
  }).pipe(
    Effect.tapErrorTag("TxSignError", (e) =>
      Effect.logError(`TxSignError: ${e}`),
    ),
    Effect.catchTag("TxSubmitError", (e) => onSubmitFailure(e)),
  );

const signSubmitHelper = (
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
): Effect.Effect<string, TxSubmitError | TxSignError> =>
  Effect.gen(function* () {
    const walletAddr = yield* Effect.tryPromise(() =>
      lucid.wallet().address(),
    ).pipe(Effect.catchAll((_e) => Effect.succeed("<unknown>")));
    yield* Effect.logInfo(`✍  Signing tx with ${walletAddr}`);
    const signedProgram = signBuilder.sign
      .withWallet()
      .completeProgram()
      .pipe(
        Effect.tapError((e) => Effect.logError(e)),
        Effect.mapError(
          (e) =>
            new TxSignError({
              message: `Failed to sign transaction`,
              cause: e,
            }),
        ),
      );
    const signed = yield* signedProgram;
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
      Effect.tapError((e) => Effect.logError(e)),
      Effect.mapError(
        (e) =>
          new TxSubmitError({
            message: `Failed to submit transaction`,
            cause: e,
          }),
      ),
    );
    yield* Effect.logInfo(`🚀 Transaction submitted: ${txHash}`);
    return txHash;
  });

/**
 * Fetch transactions of the first block by querying BlocksDB and ImmutableDB.
 *
 * TODO: `Error` type is temporary as we want the same error that SDK gives.
 *
 * @param firstBlockUTxO - UTxO of the first block in queue.
 * @returns An Effect that resolves to an array of transactions, and block's
 *          header hash.
 */
export const fetchFirstBlockTxs = (
  firstBlockUTxO: SDK.TxBuilder.StateQueue.StateQueueUTxO,
): Effect.Effect<
  { txs: readonly Buffer[]; headerHash: Buffer },
  SDK.Utils.HashingError | SDK.Utils.DataCoercionError | DBSelectError,
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

export class TxSignError extends Data.TaggedError("TxSignError")<
  SDK.Utils.GenericErrorFields & {
    readonly txHash?: string;
  }
> {}

export class TxSubmitError extends Data.TaggedError("TxSubmitError")<
  SDK.Utils.GenericErrorFields & {
    readonly txHash?: string;
  }
> {}

export class TxConfirmError extends Data.TaggedError("TxConfirmError")<
  SDK.Utils.GenericErrorFields & {
    readonly txHash?: string;
  }
> {}
