/**
 * The confirmation half of the submitted-transaction wait: polls the exact
 * transaction's status until it confirms, and settles a bounded
 * transaction's expiry on the way (`watchTxExpiry`). A read that misses its
 * deadline is aborted, and each kind of read is held to a bounded number in
 * flight (`boundedReads`).
 */
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
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
  boundedReads,
  signedTxExpiryFacts,
  txExpiredCounter,
  TxExpiredError,
  type TxExpiryRead,
  watchTxExpiry,
} from "./utils.tx-expiry.js";

/**
 * Polls `submission` until it confirms. A bounded transaction is polled until
 * its bound is settled; an unbounded one until `budgetMs` runs out. Each read
 * is bounded by `readDeadlineMs` (by the remaining budget too, unbounded).
 *
 * A bounded transaction's wait logs when its reads stop deciding (a read
 * failed or missed its deadline) and when they decide again: one line per
 * change of which reads are stuck, so a wait that goes on is seen.
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
    const statusReads = boundedReads(
      `the status read of transaction ${txHash}`,
    );
    /** The reads the last undecided line named stuck; empty while all decide. */
    let stuckLogged = "";
    const logUndecided = (
      statusFailure: unknown,
      read: TxExpiryRead,
      boundSlot: number,
    ) => {
      const stuck = [
        ...(statusFailure === undefined ? [] : ["status"]),
        ...(read.kind === "undecided" ? [read.read] : []),
      ].join(",");
      if (stuck === stuckLogged) return Effect.void;
      stuckLogged = stuck;
      if (stuck === "")
        return Effect.logInfo(
          `Transaction ${txHash}: its confirmation reads decide again`,
        );
      const reasons = [
        ...(statusFailure === undefined
          ? []
          : [`the status read failed: ${formatUnknownError(statusFailure)}`]),
        ...(read.kind === "undecided" ? [read.reason] : []),
      ];
      return Effect.logWarning(
        `Transaction ${txHash} is undecided: ${reasons.join("; ")}. The wait goes on until its upper validity bound (slot ${boundSlot.toString()}) is settled.`,
      );
    };
    const readStatus = (deadlineMs: number) =>
      Effect.either(
        statusReads(
          (signal) => lucid.transactionStatus(txHash, { signal }),
          deadlineMs,
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
      let statusFailure = status._tag === "Left" ? status.left : undefined;
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
          statusFailure = settled.left;
        }
        yield* logUndecided(statusFailure, read, expiry.invalidHereafterSlot);
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
