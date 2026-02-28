import * as SDK from "@al-ft/midgard-sdk";
import {
  LucidEvolution,
  OutRef,
  TxSignBuilder,
  UTxO,
  fromHex,
} from "@lucid-evolution/lucid";
import { Data, Effect, Schedule } from "effect";
import * as BlocksDB from "@/database/blocks.js";
import { Database } from "@/services/index.js";
import { ImmutableDB } from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";

const RETRY_ATTEMPTS = 1;

const INIT_RETRY_AFTER_MILLIS = 2_000;

const PAUSE_DURATION = "5 seconds";
const TX_CONFIRMATION_TIMEOUT_MS = 90_000;
const TX_CONFIRMATION_RETRIES = 1;
const TX_CONFIRMATION_POLL_INTERVAL_MS = 5_000;

export type BlockTxPayload = {
  readonly txId: Buffer;
  readonly txCbor: Buffer;
};

const formatSubmitError = (error: unknown): string => {
  if (error instanceof Error) {
    return `${error.name}: ${error.message}`;
  }
  if (typeof error === "string") {
    return error;
  }
  try {
    return JSON.stringify(error);
  } catch {
    return String(error);
  }
};

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
    const awaitWithTimeout = Effect.tryPromise({
      try: () =>
        new Promise<boolean>((resolve, reject) => {
          const timeoutId = setTimeout(() => {
            reject(
              new Error(
                `timed out waiting for tx confirmation after ${TX_CONFIRMATION_TIMEOUT_MS}ms`,
              ),
            );
          }, TX_CONFIRMATION_TIMEOUT_MS);

          lucid
            .awaitTx(txHash, TX_CONFIRMATION_POLL_INTERVAL_MS)
            .then((result) => {
              clearTimeout(timeoutId);
              resolve(result);
            })
            .catch((error) => {
              clearTimeout(timeoutId);
              reject(error);
            });
        }),
      catch: (e) =>
        new TxConfirmError({
          message: `Failed to confirm transaction`,
          txHash,
          cause: e,
        }),
    }).pipe(Effect.retry(Schedule.recurs(TX_CONFIRMATION_RETRIES)));

    yield* awaitWithTimeout;
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
    const signedTxCbor = signed.toCBOR();
    yield* Effect.logInfo(
      `✍  Signed tx prepared: txHash=${txHash}, cborBytes=${signedTxCbor.length / 2}`,
    );
    yield* Effect.logInfo("✉️  Submitting transaction...");
    yield* signed.submitProgram().pipe(
      Effect.retry(
        Schedule.compose(
          Schedule.exponential(INIT_RETRY_AFTER_MILLIS),
          Schedule.recurs(RETRY_ATTEMPTS),
        ),
      ),
      Effect.tapError((e) =>
        Effect.logError(
          `Tx submission provider error for ${txHash}: ${formatSubmitError(e)}`,
        ),
      ),
      Effect.mapError(
        (e) =>
          new TxSubmitError({
            message: `Failed to submit transaction: ${formatSubmitError(e)}`,
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
  firstBlockUTxO: SDK.StateQueueUTxO,
): Effect.Effect<
  {
    txs: readonly BlockTxPayload[];
    txHashes: readonly Buffer[];
    headerHash: Buffer;
  },
  SDK.HashingError | SDK.DataCoercionError | DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    const blockHeader = yield* SDK.getHeaderFromStateQueueDatum(
      firstBlockUTxO.datum,
    );
    const headerHash: Buffer = yield* SDK.hashBlockHeader(blockHeader).pipe(
      Effect.map((hh) => Buffer.from(fromHex(hh))),
    );
    const txHashes = yield* BlocksDB.retrieveTxHashesByHeaderHash(headerHash);
    const txEntries = yield* ImmutableDB.retrieveTxEntriesByHashes(txHashes);
    const txById = new Map<string, Buffer>();
    for (const entry of txEntries) {
      txById.set(entry.tx_id.toString("hex"), entry.tx);
    }
    const txs: BlockTxPayload[] = [];
    for (const txHash of txHashes) {
      const txCbor = txById.get(txHash.toString("hex"));
      if (txCbor !== undefined) {
        txs.push({
          txId: txHash,
          txCbor,
        });
      }
    }
    return { txs, txHashes, headerHash };
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
  SDK.GenericErrorFields & {
    readonly txHash: string;
  }
> {}

export class TxSubmitError extends Data.TaggedError("TxSubmitError")<
  SDK.GenericErrorFields & {
    readonly txHash: string;
  }
> {}

export class TxConfirmError extends Data.TaggedError("TxConfirmError")<
  SDK.GenericErrorFields & {
    readonly txHash: string;
  }
> {}

export class GenesisDepositError extends Data.TaggedError(
  "GenesisDepositError",
)<SDK.GenericErrorFields> {}
