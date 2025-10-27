import * as SDK from "@al-ft/midgard-sdk";
import {
  LucidEvolution,
  OutRef,
  TxSignBuilder,
  TxSigned,
  UTxO,
  fromHex,
} from "@lucid-evolution/lucid";
import { Data, Effect, Schedule } from "effect";
import * as BlocksDB from "../database/blocks.js";
import { Database } from "@/services/index.js";
import { ImmutableDB } from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import { UnknownException } from "effect/Cause";

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
): Effect.Effect<string, TxSignError | TxSubmitError | TxConfirmError> =>
  Effect.gen(function* () {
    const txHash = yield* signSubmitHelper(lucid, signBuilder);
    yield* Effect.logInfo(`⏳ Confirming Transaction...`);
    yield* Effect.tryPromise({
      try: () => lucid.awaitTx(txHash, 10_000),
      catch: (e) =>
        new TxConfirmError({
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
    Effect.tapErrorTag("TxSignError", (e) =>
      Effect.logError(`TxSignError: ${e}`),
    ),
  );

/**
 * Handle the signing and submission of a transaction without waiting for the
 * transaction to be confirmed.
 *
 * @param lucid - The LucidEvolution instance. Here it's only used for logging the signer's address.
 * @param signBuilder - The transaction sign builder.
 * @returns An Effect that resolves when the transaction is signed, submitted, and confirmed.
 */
export const handleSignSubmitNoConfirmation = (
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
): Effect.Effect<string, TxSignError | TxSubmitError> =>
  Effect.gen(function* () {
    const txHash = yield* signSubmitHelper(lucid, signBuilder);
    return txHash;
  }).pipe(
    Effect.tapErrorTag("TxSignError", (e) =>
      Effect.logError(`TxSignError: ${e}`),
    ),
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
    const txHash = signBuilder.toHash();
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
              txHash,
            }),
        ),
      );
    const signed = yield* signedProgram;
    yield* Effect.logInfo(`Signed tx CBOR is:
${signed.toCBOR()}
`);
    yield* Effect.logInfo("✉️  Submitting transaction...");
    yield* signed.submitProgram().pipe(
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
            txHash,
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
  SDK.Utils.HashingError | SDK.Utils.DataCoercionError | DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    const blockHeader = yield* SDK.Utils.getHeaderFromStateQueueDatum(
      firstBlockUTxO.datum,
    );
    const headerHash: Buffer = yield* SDK.Utils.hashHeader(blockHeader).pipe(
      Effect.map((hh) => Buffer.from(fromHex(hh))),
    );
    const txHashes = yield* BlocksDB.retrieveTxHashesByHeaderHash(headerHash);
    const txs: readonly Buffer[] = yield* ImmutableDB.retrieveTxCborsByHashes(txHashes);
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
    readonly txHash: string;
  }
> {}

export class TxSubmitError extends Data.TaggedError("TxSubmitError")<
  SDK.Utils.GenericErrorFields & {
    readonly txHash: string;
  }
> {}

export class TxConfirmError extends Data.TaggedError("TxConfirmError")<
  SDK.Utils.GenericErrorFields & {
    readonly txHash: string;
  }
> {}

export class GenesisDepositError extends Data.TaggedError("GenesisDepositError")<
  SDK.Utils.GenericErrorFields
> {}
