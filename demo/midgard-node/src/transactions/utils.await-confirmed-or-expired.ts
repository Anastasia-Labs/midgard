/**
 * The confirmation half of the submitted-transaction wait: polls the exact
 * transaction's status until it confirms, and settles a bounded
 * transaction's expiry on the way (`watchTxExpiry`).
 */
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { isEmulatorProvider, LucidEvolution } from "@lucid-evolution/lucid";
import { Effect, Metric } from "effect";

import {
  type SignSubmitContext,
  TxConfirmError,
} from "./utils.await-required-output-visibility.js";
import {
  submitRecoveryNow,
  submitRecoverySleep,
} from "./utils.submit-recovery-options.js";
import {
  signedTxExpiryFacts,
  txExpiredCounter,
  TxExpiredError,
  watchTxExpiry,
  withReadDeadline,
} from "./utils.tx-expiry.js";

/**
 * Polls `submission` until it confirms. A bounded transaction is polled until
 * its bound is settled; an unbounded one until `budgetMs` runs out. Each read
 * is bounded by `readDeadlineMs` (by the remaining budget too, unbounded).
 */
export const awaitConfirmedOrExpired = (
  lucid: LucidEvolution,
  submission: SignSubmitContext,
  options: Readonly<{
    budgetMs: number;
    readDeadlineMs: number;
    pollIntervalMs: number;
  }>,
): Effect.Effect<void, TxConfirmError | TxExpiredError> =>
  Effect.gen(function* () {
    const { txHash } = submission;
    const { budgetMs, readDeadlineMs, pollIntervalMs } = options;
    const now = submitRecoveryNow(lucid);
    const sleep = submitRecoverySleep(lucid);
    const emulator = isEmulatorProvider(lucid.config().provider);
    const expiry = signedTxExpiryFacts(submission.signedTxCbor);
    const watch =
      expiry === undefined
        ? undefined
        : watchTxExpiry(
            lucid,
            expiry,
            DEPLOYMENT_MANIFEST_L1_FINALITY,
            readDeadlineMs,
          );
    const deadline = (yield* now) + budgetMs;
    let budgetPassedLogged = false;
    let lastReadError: unknown;
    const readStatus = (deadlineMs: number) =>
      Effect.either(
        withReadDeadline(
          Effect.tryPromise({
            try: () => lucid.transactionStatus(txHash),
            catch: (cause) => cause,
          }),
          deadlineMs,
          `the status read of transaction ${txHash}`,
        ),
      );
    for (;;) {
      if (emulator)
        yield* Effect.tryPromise(() =>
          lucid.awaitTx(txHash, pollIntervalMs),
        ).pipe(Effect.ignore);
      const remainingMs = deadline - (yield* now);
      const status = yield* readStatus(
        watch === undefined
          ? Math.max(1, Math.min(readDeadlineMs, remainingMs))
          : readDeadlineMs,
      );
      if (status._tag === "Left") lastReadError = status.left;
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
      if (watch !== undefined && expiry !== undefined) {
        const read = yield* watch;
        if (read.kind === "expired") {
          yield* Metric.increment(txExpiredCounter);
          return yield* Effect.fail(
            new TxExpiredError({
              message: `Transaction expired: its upper validity bound (slot ${expiry.invalidHereafterSlot.toString()}) lies the confirmation depth under the L1 tip (slot ${read.tipSlot.toString()}) with every input unspent`,
              txHash,
              invalidHereafterSlot: expiry.invalidHereafterSlot,
              tipSlot: read.tipSlot,
              cause: undefined,
            }),
          );
        }
        if (read.kind === "spent") {
          // Settled past the bound with an input spent: it landed (read
          // its status once more) or another transaction spent the input.
          const settled = yield* readStatus(readDeadlineMs);
          if (settled._tag === "Right") {
            if (settled.right.status === "confirmed") return;
            return yield* Effect.fail(
              new TxConfirmError({
                message: "Failed to confirm transaction",
                txHash,
                cause: new Error(
                  `Transaction ${txHash} did not land: its upper validity bound (slot ${expiry.invalidHereafterSlot.toString()}) lies the confirmation depth under the L1 tip (slot ${read.tipSlot.toString()}) and another transaction spent one of its inputs.`,
                ),
              }),
            );
          }
          lastReadError = settled.left;
        }
        if (remainingMs <= 0 && !budgetPassedLogged) {
          budgetPassedLogged = true;
          yield* Effect.logWarning(
            `Transaction ${txHash} is unconfirmed past its confirmation budget; it has an upper validity bound (slot ${expiry.invalidHereafterSlot.toString()}), so the wait goes on until that bound is settled`,
          );
        }
        yield* sleep(pollIntervalMs);
        continue;
      }
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
