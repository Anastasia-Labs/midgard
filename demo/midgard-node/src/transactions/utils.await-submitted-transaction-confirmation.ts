import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import {
  isEmulatorProvider,
  LucidEvolution,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect, Metric } from "effect";

import type {
  IntentJournal,
  SubmissionIntent,
} from "../services/intent-journal.js";
import {
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
  type IntendedStateProbe,
  isNoInlineSubmitDefer,
  type NoInlineSubmitRecoveryOptions,
  type SignSubmitLostRace,
  type SignSubmitNoConfirmationResult,
  type SubmitRecoveryInlineOptions,
  submitRecoveryNow,
  type SubmitRecoveryOptions,
  submitRecoverySleep,
} from "./utils.submit-recovery-options.js";
import { submitSignedTxWithRecovery } from "./utils.submit-signed-tx-with-recovery.js";
import {
  readTxExpiry,
  signedTxExpiryFacts,
  txExpiredCounter,
  TxExpiredError,
} from "./utils.tx-expiry.js";
import { signOverWalletView } from "./utils.wallet-view.js";

/**
 * Handle the signing and submission of a transaction.
 *
 * @param lucid - The LucidEvolution instance.
 * @param signBuilder - The transaction sign builder.
 * @returns An Effect that resolves when the transaction is signed, submitted, and confirmed.
 * With `intendedStateOnChain`, a lost race resolves instead, unconfirmed.
 */
export function handleSignSubmit(
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
  intent: SubmissionIntent,
  options: NoInlineSubmitRecoveryOptions & IntendedStateProbe,
): Effect.Effect<
  string | SignSubmitLostRace,
  | TxSignError
  | TxSubmitError
  | TxConfirmError
  | TxExpiredError
  | NoInlineSubmitDefer,
  IntentJournal
>;

export function handleSignSubmit(
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
  intent: SubmissionIntent,
  options: NoInlineSubmitRecoveryOptions,
): Effect.Effect<
  string,
  | TxSignError
  | TxSubmitError
  | TxConfirmError
  | TxExpiredError
  | NoInlineSubmitDefer,
  IntentJournal
>;

export function handleSignSubmit(
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
  intent: SubmissionIntent,
  options?: SubmitRecoveryOptions,
): Effect.Effect<
  string,
  TxSignError | TxSubmitError | TxConfirmError | TxExpiredError,
  IntentJournal
>;

export function handleSignSubmit(
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
  intent: SubmissionIntent,
  options: SubmitRecoveryOptions = {},
): Effect.Effect<
  string | SignSubmitLostRace,
  | TxSignError
  | TxSubmitError
  | TxConfirmError
  | TxExpiredError
  | NoInlineSubmitDefer,
  IntentJournal
> {
  return Effect.gen(function* () {
    const submission = yield* signSubmitTransaction(
      lucid,
      signBuilder,
      intent,
      options,
    );
    if (submission.lostRace === true) {
      return {
        status: "lost_race",
        txHash: submission.txHash,
      } satisfies SignSubmitLostRace;
    }
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

/**
 * Waits for the exact submitted transaction to confirm, then for its
 * required outputs to read back. Each poll reads the transaction's status;
 * a transaction with an upper validity bound is also read for expiry
 * (`readTxExpiry`), and fails with {@link TxExpiredError} once the tip
 * reached its bound with every input unspent, so the caller can rebuild
 * (`rebuildOnTxExpiry`). Before that the wait goes on. Past the budget
 * (`confirmationTimeoutMs` for each of the `confirmationRetries + 1`
 * attempts) an unsettled transaction fails with {@link TxConfirmError}. On
 * the Lucid emulator each poll first produces the block a pending
 * transaction needs, and the wait runs on the emulator's clock.
 */
export const awaitSubmittedTransactionConfirmation = (
  lucid: LucidEvolution,
  submission: SignSubmitContext,
  options: Pick<
    SubmitRecoveryInlineOptions,
    | "confirmationTimeoutMs"
    | "confirmationRetries"
    | "confirmationPollIntervalMs"
    | "requiredOutputIndexes"
    | "label"
  > = {},
): Effect.Effect<string, TxConfirmError | TxExpiredError> =>
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
      confirmationPollIntervalMs <= 0
    ) {
      return yield* Effect.fail(
        new TxConfirmError({
          message: "Invalid transaction confirmation options",
          txHash,
          cause: `timeout_ms=${confirmationTimeoutMs.toString()},retries=${confirmationRetries.toString()},poll_interval_ms=${confirmationPollIntervalMs.toString()}`,
        }),
      );
    }
    yield* Effect.logInfo(`⏳ Confirming Transaction...`);
    yield* awaitConfirmedOrExpired(
      lucid,
      submission,
      confirmationTimeoutMs * (confirmationRetries + 1),
      confirmationPollIntervalMs,
    );
    yield* awaitRequiredOutputVisibility(
      lucid,
      submission,
      options.requiredOutputIndexes ?? [],
      Math.min(confirmationTimeoutMs, TX_OUTPUT_VISIBILITY_TIMEOUT_MS),
      Math.min(
        confirmationPollIntervalMs,
        TX_OUTPUT_VISIBILITY_POLL_INTERVAL_MS,
      ),
    );
    yield* Effect.logInfo(`🎉 Transaction confirmed: ${txHash}`);
    return txHash;
  });

const awaitConfirmedOrExpired = (
  lucid: LucidEvolution,
  submission: SignSubmitContext,
  budgetMs: number,
  pollIntervalMs: number,
): Effect.Effect<void, TxConfirmError | TxExpiredError> =>
  Effect.gen(function* () {
    const { txHash } = submission;
    const now = submitRecoveryNow(lucid);
    const sleep = submitRecoverySleep(lucid);
    const emulator = isEmulatorProvider(lucid.config().provider);
    const expiry = signedTxExpiryFacts(submission.signedTxCbor);
    const deadline = now() + budgetMs;
    let lastReadError: unknown;
    for (;;) {
      if (emulator)
        yield* Effect.tryPromise(() =>
          lucid.awaitTx(txHash, pollIntervalMs),
        ).pipe(Effect.ignore);
      const status = yield* Effect.either(
        Effect.tryPromise(() => lucid.transactionStatus(txHash)),
      );
      if (status._tag === "Left") lastReadError = status.left.cause;
      else if (status.right.status === "confirmed") return;
      else if (status.right.status === "failed")
        return yield* Effect.fail(
          new TxConfirmError({
            message: "Failed to confirm transaction",
            txHash,
            cause: new Error(`Transaction ${txHash} failed.`, {
              cause: status.right.reason,
            }),
          }),
        );
      if (expiry !== undefined) {
        const read = yield* readTxExpiry(lucid, expiry);
        if (read.kind === "expired") {
          yield* Metric.increment(txExpiredCounter);
          return yield* Effect.fail(
            new TxExpiredError({
              message: `Transaction expired: the L1 tip (slot ${read.tipSlot.toString()}) reached its upper validity bound (slot ${expiry.invalidHereafterSlot.toString()}) with every input unspent`,
              txHash,
              invalidHereafterSlot: expiry.invalidHereafterSlot,
              tipSlot: read.tipSlot,
              cause: undefined,
            }),
          );
        }
      }
      const remainingMs = deadline - now();
      if (remainingMs <= 0)
        return yield* Effect.fail(
          new TxConfirmError({
            message: "Failed to confirm transaction",
            txHash,
            cause: new Error(`Timed out waiting for transaction ${txHash}.`, {
              cause: lastReadError,
            }),
          }),
        );
      yield* sleep(Math.min(pollIntervalMs, remainingMs));
    }
  });

/**
 * Handle the signing and submission of a transaction without waiting for the
 * transaction to be confirmed.
 *
 * @param lucid - The LucidEvolution instance. Here it's only used for logging the signer's address.
 * @param signBuilder - The transaction sign builder.
 * @returns An Effect that resolves when the transaction is signed, submitted, and confirmed.
 * With `intendedStateOnChain`, a lost race resolves too.
 */
export function handleSignSubmitNoConfirmation(
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
  intent: SubmissionIntent,
  options: NoInlineSubmitRecoveryOptions & IntendedStateProbe,
): Effect.Effect<
  SignSubmitNoConfirmationResult | SignSubmitLostRace,
  TxSignError | TxSubmitError,
  IntentJournal
>;

export function handleSignSubmitNoConfirmation(
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
  intent: SubmissionIntent,
  options: NoInlineSubmitRecoveryOptions,
): Effect.Effect<
  SignSubmitNoConfirmationResult,
  TxSignError | TxSubmitError,
  IntentJournal
>;

export function handleSignSubmitNoConfirmation(
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
  intent: SubmissionIntent,
  options?: SubmitRecoveryOptions,
): Effect.Effect<string, TxSignError | TxSubmitError, IntentJournal>;

export function handleSignSubmitNoConfirmation(
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
  intent: SubmissionIntent,
  options: SubmitRecoveryOptions = {},
): Effect.Effect<
  string | SignSubmitNoConfirmationResult | SignSubmitLostRace,
  TxSignError | TxSubmitError | NoInlineSubmitDefer,
  IntentJournal
> {
  const returnNoInlineDefer =
    options.inlineWaitPolicy === "defer_positive_wait";
  return Effect.gen(function* () {
    const submissionResult = yield* Effect.either(
      signSubmitTransactionWithDefer(lucid, signBuilder, intent, options),
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
    if (submission.lostRace === true) {
      return {
        status: "lost_race",
        txHash: submission.txHash,
      } satisfies SignSubmitLostRace;
    }
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
  intent: SubmissionIntent,
  options: SubmitRecoveryOptions & {
    readonly inlineWaitPolicy: "defer_positive_wait";
  },
): Effect.Effect<
  SignSubmitContext,
  TxSubmitError | TxSignError | NoInlineSubmitDefer,
  IntentJournal
>;

export function signSubmitTransaction(
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
  intent: SubmissionIntent,
  options?: SubmitRecoveryOptions,
): Effect.Effect<SignSubmitContext, TxSubmitError | TxSignError, IntentJournal>;

export function signSubmitTransaction(
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
  intent: SubmissionIntent,
  options: SubmitRecoveryOptions = {},
): Effect.Effect<
  SignSubmitContext,
  TxSubmitError | TxSignError | NoInlineSubmitDefer,
  IntentJournal
> {
  return signSubmitTransactionWithDefer(lucid, signBuilder, intent, options);
}

const signSubmitTransactionWithDefer = (
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
  intent: SubmissionIntent,
  options: SubmitRecoveryOptions = {},
): Effect.Effect<
  SignSubmitContext,
  TxSubmitError | TxSignError | NoInlineSubmitDefer,
  IntentJournal
> =>
  Effect.gen(function* () {
    const walletAddr = yield* Effect.tryPromise(() =>
      lucid.wallet().address(),
    ).pipe(Effect.catchAll((_e) => Effect.succeed("<unknown>")));
    yield* Effect.logInfo(`✍  Signing tx with ${walletAddr}`);
    const txHash = signBuilder.toHash();
    const signedProgram = (yield* signOverWalletView(lucid, signBuilder))
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
    const outcome = yield* submitSignedTxWithRecovery(
      lucid,
      signed,
      txHash,
      intent,
      options,
    ).pipe(
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
    if (outcome === "lost_race") {
      return { txHash, signedTxCbor, lostRace: true } as const;
    }
    yield* Effect.logInfo(`🚀 Transaction submitted: ${txHash}`);
    return { txHash, signedTxCbor };
  });
