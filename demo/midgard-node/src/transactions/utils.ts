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
import { SerializationError } from "@/utils.js";
import { SelectError } from "@/database/utils/common.js";

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
  onSubmitFailure: (
    error: SubmitError,
  ) => Effect.Effect<void, SubmitError>,
  onConfirmFailure: (
    error: ConfirmError,
  ) => Effect.Effect<void, ConfirmError>,
): Effect.Effect<string | void, SubmitError | ConfirmError | SignError> =>
  Effect.gen(function* () {
    const txHash = yield* signSubmitHelper(lucid, signBuilder);
    yield* Effect.logInfo(`⏳ Confirming Transaction...`);
    yield* Effect.tryPromise({
      try: () => lucid.awaitTx(txHash, 10_000),
      catch: (e: unknown) =>
        new ConfirmError({
          message: `Failed to confirm transaction`,
          txHash,
          cause: e,
        }),
    });
    yield* Effect.logInfo(`🎉 Transaction confirmed: ${txHash}`);
    yield* Effect.logInfo(`⌛ Pausing for ${PAUSE_DURATION}...`);
    yield* Effect.sleep(PAUSE_DURATION);
    yield* Effect.logInfo("✅ Pause ended.");
    return txHash;
  }).pipe(
    Effect.catchAll((e: SubmitError | ConfirmError | SignError): Effect.Effect<void, SubmitError | ConfirmError | SignError, never> => {
      switch (e._tag) {
        case "SubmitError":
          return onSubmitFailure(e);
        case "ConfirmError":
          return onConfirmFailure(e);
        case "SignError":
          return pipe(
            Effect.logError(`Signing tx error: ${e.message}`),
            Effect.flatMap(() => Effect.fail(e)),
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
  onSubmitFailure: (
    error: SubmitError,
  ) => Effect.Effect<void, SignError | SubmitError>,
): Effect.Effect<string | void, SignError | SubmitError> =>
  Effect.gen(function* () {
    const txHash = yield* signSubmitHelper(lucid, signBuilder);
    return txHash;
  }).pipe(
    Effect.catchAll((e: SignError | SubmitError) =>
      e._tag === "SubmitError"
        ? onSubmitFailure(e)
        : pipe(
            Effect.logError(`Signing tx error: ${e.message}`),
            Effect.flatMap(() => Effect.fail(e)),
          ),
    ),
  );

const signSubmitHelper = (
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
): Effect.Effect<string, SubmitError | SignError> =>
  Effect.gen(function* () {
    const walletAddr = yield* Effect.tryPromise({
      try: () => lucid.wallet().address(),
      catch: (e) =>
        new SignError({
          message: `Failed to get wallet address:`,
          cause: e,
        }),
    }).pipe(Effect.catchAll((_e) => Effect.succeed("<unknown>")));
    yield* Effect.logInfo(`✍  Signing tx with ${walletAddr}...`);
    const signed = yield* signBuilder.sign
      .withWallet()
      .completeProgram()
      .pipe(
        Effect.mapError(
          (e) =>
            new SignError({
              message: `Failed to sign transaction:`,
              cause: e,
            }),
        ),
      );
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
      Effect.mapError(
        (e) =>
          new SubmitError({
            message: `Failed to submit transaction:`,
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
 * @param firstBlockUTxO - UTxO of the first block in queue.
 * @returns An Effect that resolves to an array of transactions, and block's
 *          header hash.
 */
export const fetchFirstBlockTxs = (
  firstBlockUTxO: SDK.TxBuilder.StateQueue.StateQueueUTxO,
): Effect.Effect<
  { txs: readonly Buffer[]; headerHash: Buffer },
  SelectError | SerializationError,
  Database
> =>
  Effect.gen(function* () {
    const blockHeader = yield* SDK.Utils.getHeaderFromStateQueueDatum(
      firstBlockUTxO.datum,
    ).pipe(
      Effect.mapError(
        (e) =>
          new SerializationError({
            message: `An error occurred on getting header from state queue datum`,
            cause: e,
          }),
      ),
    );
    const headerHash = yield* SDK.Utils.hashHeader(blockHeader).pipe(
      Effect.map((hh) => Buffer.from(fromHex(hh))),
      Effect.mapError(
        (e) =>
          new SerializationError({
            message: `An error occurred on hashing block header ${blockHeader}`,
            cause: e,
          }),
      ),
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

export class SignError extends Data.TaggedError("SignError")<{
  readonly message: string;
  readonly txHash?: string;
  readonly cause?: unknown;
}> {}

export class SubmitError extends Data.TaggedError("SubmitError")<{
  readonly message: string;
  readonly txHash?: string;
  readonly cause?: unknown;
}> {}

export class ConfirmError extends Data.TaggedError("ConfirmError")<{
  readonly message: string;
  readonly txHash?: string;
  readonly cause?: unknown;
}> {}
