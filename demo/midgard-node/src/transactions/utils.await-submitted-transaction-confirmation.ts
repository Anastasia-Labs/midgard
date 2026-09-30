import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { LucidEvolution, TxSignBuilder } from "@lucid-evolution/lucid";
import { Duration, Effect, Schedule } from "effect";

import {
  awaitExactTransactionConfirmation,
  awaitRequiredOutputVisibility,
  NoInlineSubmitDefer,
  type SignSubmitContext,
  TxConfirmError,
  TxSignError,
  TxSubmitError,
} from "./utils.await-required-output-visibility.js";
import {
  TX_CONFIRMATION_POLL_INTERVAL_MS,
  TX_CONFIRMATION_RETRIES,
  TX_CONFIRMATION_TIMEOUT_MS,
  TX_OUTPUT_VISIBILITY_POLL_INTERVAL_MS,
  TX_OUTPUT_VISIBILITY_TIMEOUT_MS,
} from "./utils.parse-structured-outside-validity-interval-details.js";
import {
  isNoInlineSubmitDefer,
  type NoInlineSubmitRecoveryOptions,
  reconcileWalletUtxosFromSignedTx,
  type SignSubmitNoConfirmationResult,
  type SubmitRecoveryInlineOptions,
  type SubmitRecoveryOptions,
} from "./utils.reconcile-wallet-utxos-from-signed-tx.js";
import { submitSignedTxWithRecovery } from "./utils.submit-signed-tx-with-recovery.js";

/**
 * Handle the signing and submission of a transaction.
 *
 * @param lucid - The LucidEvolution instance.
 * @param signBuilder - The transaction sign builder.
 * @returns An Effect that resolves when the transaction is signed, submitted, and confirmed.
 */
export function handleSignSubmit(
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
  options: NoInlineSubmitRecoveryOptions,
): Effect.Effect<
  string,
  TxSignError | TxSubmitError | TxConfirmError | NoInlineSubmitDefer
>;

export function handleSignSubmit(
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
  options?: SubmitRecoveryOptions,
): Effect.Effect<string, TxSignError | TxSubmitError | TxConfirmError>;

export function handleSignSubmit(
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
  options: SubmitRecoveryOptions = {},
): Effect.Effect<
  string,
  TxSignError | TxSubmitError | TxConfirmError | NoInlineSubmitDefer
> {
  return Effect.gen(function* () {
    const submission = yield* signSubmitTransaction(
      lucid,
      signBuilder,
      options,
    );
    return yield* awaitSubmittedTransactionConfirmation(
      lucid,
      submission,
      options,
    );
  }).pipe(
    Effect.tapErrorTag("TxSignError", (e) =>
      Effect.logError(`TxSignError: ${e.message}`),
    ),
  );
}

export const awaitSubmittedTransactionConfirmation = (
  lucid: LucidEvolution,
  submission: SignSubmitContext,
  options: Pick<
    SubmitRecoveryInlineOptions,
    | "confirmationTimeoutMs"
    | "confirmationRetries"
    | "confirmationPollIntervalMs"
    | "confirmationDeadlineMs"
    | "requiredOutputIndexes"
    | "label"
  > = {},
): Effect.Effect<string, TxConfirmError> =>
  Effect.gen(function* () {
    const txHash = submission.txHash;
    const confirmationTimeoutMs =
      options.confirmationTimeoutMs ?? TX_CONFIRMATION_TIMEOUT_MS;
    const confirmationRetries =
      options.confirmationRetries ?? TX_CONFIRMATION_RETRIES;
    const confirmationPollIntervalMs =
      options.confirmationPollIntervalMs ?? TX_CONFIRMATION_POLL_INTERVAL_MS;
    if (
      !Number.isSafeInteger(confirmationTimeoutMs) ||
      confirmationTimeoutMs <= 0 ||
      !Number.isSafeInteger(confirmationRetries) ||
      confirmationRetries < 0 ||
      !Number.isSafeInteger(confirmationPollIntervalMs) ||
      confirmationPollIntervalMs <= 0 ||
      (options.confirmationDeadlineMs !== undefined &&
        !Number.isSafeInteger(options.confirmationDeadlineMs))
    ) {
      return yield* Effect.fail(
        new TxConfirmError({
          message: "Invalid transaction confirmation options",
          txHash,
          cause: `timeout_ms=${confirmationTimeoutMs.toString()},retries=${confirmationRetries.toString()},poll_interval_ms=${confirmationPollIntervalMs.toString()},deadline_ms=${String(options.confirmationDeadlineMs)}`,
        }),
      );
    }
    yield* Effect.logInfo(`⏳ Confirming Transaction...`);
    const awaitWithTimeout = Effect.tryPromise({
      try: () =>
        awaitExactTransactionConfirmation(lucid, txHash, {
          timeout: confirmationTimeoutMs,
          checkInterval: confirmationPollIntervalMs,
        }),
      catch: (e) =>
        new TxConfirmError({
          message: `Failed to confirm transaction`,
          txHash,
          cause: e,
        }),
    }).pipe(
      Effect.retry(
        Schedule.intersect(
          Schedule.fixed(Duration.millis(confirmationPollIntervalMs)),
          Schedule.recurs(confirmationRetries),
        ),
      ),
    );

    const confirmationDeadlineMs = options.confirmationDeadlineMs;
    yield* awaitWithTimeout.pipe(
      Effect.zipRight(
        awaitRequiredOutputVisibility(
          lucid,
          submission,
          options.requiredOutputIndexes ?? [],
          Math.min(confirmationTimeoutMs, TX_OUTPUT_VISIBILITY_TIMEOUT_MS),
          Math.min(
            confirmationPollIntervalMs,
            TX_OUTPUT_VISIBILITY_POLL_INTERVAL_MS,
          ),
        ),
      ),
      (wait) =>
        confirmationDeadlineMs === undefined
          ? wait
          : wait.pipe(
              Effect.timeoutFail({
                duration: Duration.millis(
                  Math.max(0, confirmationDeadlineMs - Date.now()),
                ),
                onTimeout: () =>
                  new TxConfirmError({
                    message: "Transaction confirmation deadline passed",
                    txHash,
                    cause: `deadline_ms=${confirmationDeadlineMs.toString()}`,
                  }),
              }),
            ),
    );
    yield* reconcileWalletUtxosFromSignedTx(lucid, submission);
    yield* Effect.logInfo(`🎉 Transaction confirmed: ${txHash}`);
    return txHash;
  });

/**
 * Handle the signing and submission of a transaction without waiting for the
 * transaction to be confirmed.
 *
 * @param lucid - The LucidEvolution instance. Here it's only used for logging the signer's address.
 * @param signBuilder - The transaction sign builder.
 * @returns An Effect that resolves when the transaction is signed, submitted, and confirmed.
 */
export function handleSignSubmitNoConfirmation(
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
  options: NoInlineSubmitRecoveryOptions,
): Effect.Effect<SignSubmitNoConfirmationResult, TxSignError | TxSubmitError>;

export function handleSignSubmitNoConfirmation(
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
  options?: SubmitRecoveryOptions,
): Effect.Effect<string, TxSignError | TxSubmitError>;

export function handleSignSubmitNoConfirmation(
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
  options: SubmitRecoveryOptions = {},
): Effect.Effect<
  string | SignSubmitNoConfirmationResult,
  TxSignError | TxSubmitError | NoInlineSubmitDefer
> {
  const returnNoInlineDefer =
    options.inlineWaitPolicy === "defer_positive_wait";
  return Effect.gen(function* () {
    const submissionResult = yield* Effect.either(
      signSubmitTransactionWithDefer(lucid, signBuilder, options),
    );
    if (submissionResult._tag === "Left") {
      const error = submissionResult.left;
      if (isNoInlineSubmitDefer(error) && returnNoInlineDefer) {
        return {
          status: "deferred",
          defer: error,
        } satisfies SignSubmitNoConfirmationResult;
      }
      return yield* Effect.fail(error);
    }
    const submission = submissionResult.right;
    if (returnNoInlineDefer) {
      return {
        status: "submitted",
        txHash: submission.txHash,
      } satisfies SignSubmitNoConfirmationResult;
    }
    return submission.txHash;
  }).pipe(
    Effect.tapErrorTag("TxSignError", (e) =>
      Effect.logError(`TxSignError: ${e.message}`),
    ),
  );
}

/**
 * Shared implementation used by the confirmation and no-confirmation sign/
 * submit entrypoints.
 */
export function signSubmitTransaction(
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
  options: SubmitRecoveryOptions & {
    readonly inlineWaitPolicy: "defer_positive_wait";
  },
): Effect.Effect<
  SignSubmitContext,
  TxSubmitError | TxSignError | NoInlineSubmitDefer
>;

export function signSubmitTransaction(
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
  options?: SubmitRecoveryOptions,
): Effect.Effect<SignSubmitContext, TxSubmitError | TxSignError>;

export function signSubmitTransaction(
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
  options: SubmitRecoveryOptions = {},
): Effect.Effect<
  SignSubmitContext,
  TxSubmitError | TxSignError | NoInlineSubmitDefer
> {
  return signSubmitTransactionWithDefer(lucid, signBuilder, options);
}

const signSubmitTransactionWithDefer = (
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
  options: SubmitRecoveryOptions = {},
): Effect.Effect<
  SignSubmitContext,
  TxSubmitError | TxSignError | NoInlineSubmitDefer
> =>
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
    yield* submitSignedTxWithRecovery(lucid, signed, txHash, options).pipe(
      Effect.tapError((e) =>
        isNoInlineSubmitDefer(e)
          ? Effect.void
          : Effect.logError(
              `Tx submission provider error for ${txHash}: ${formatUnknownError(
                e,
                {
                  includeCause: true,
                },
              )}`,
            ),
      ),
      Effect.mapError((e) =>
        isNoInlineSubmitDefer(e)
          ? e
          : new TxSubmitError({
              message: `Failed to submit transaction: ${formatUnknownError(e, {
                includeCause: true,
              })}`,
              cause: e,
              txHash,
            }),
      ),
    );
    yield* Effect.logInfo(`🚀 Transaction submitted: ${txHash}`);
    return {
      txHash,
      signedTxCbor,
      walletAddress: walletAddr,
    };
  });
