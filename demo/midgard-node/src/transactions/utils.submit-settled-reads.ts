/**
 * What landed after a submit that did not plainly succeed, read from L1 for
 * `submitSignedTxWithRecovery`: the exact transaction after a refusal for
 * spent inputs or an unknown outcome, and the state the transaction was sent
 * for when another transaction may have reached it first.
 */
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { LucidEvolution } from "@lucid-evolution/lucid";
import { Effect, Schedule } from "effect";

import { awaitExactTransactionConfirmation } from "./utils.await-required-output-visibility.js";
import {
  SUBMIT_RECOVERY_AWAIT_TIMEOUT_MS,
  SUBMIT_RECOVERY_POLL_INTERVAL_MS,
} from "./utils.parse-structured-outside-validity-interval-details.js";
import { type SubmitOutcome } from "./utils.submit-recovery-options.js";

/**
 * After a refusal for spent inputs: the bounded wait for the exact
 * transaction to confirm. With the caller's `intendedStateOnChain`, the
 * intended state turning visible ends the wait early (`lost_race`): the
 * exact transaction cannot land once another one spent its inputs.
 */
export const awaitExactTxOrIntendedState = (
  lucid: LucidEvolution,
  txHash: string,
  intendedStateOnChain: (() => Effect.Effect<boolean>) | undefined,
): Effect.Effect<"confirmed" | "unconfirmed" | "lost_race"> => {
  const confirmation = (abortable: boolean) =>
    Effect.tryPromise((signal) =>
      awaitExactTransactionConfirmation(lucid, txHash, {
        timeout: SUBMIT_RECOVERY_AWAIT_TIMEOUT_MS,
        checkInterval: SUBMIT_RECOVERY_POLL_INTERVAL_MS,
        // Only the race below ends the wait early, so only it aborts it.
        ...(abortable ? { signal } : {}),
      }),
    ).pipe(
      Effect.match({
        onFailure: () => "unconfirmed" as const,
        onSuccess: () => "confirmed" as const,
      }),
    );
  return intendedStateOnChain === undefined
    ? confirmation(false)
    : Effect.raceFirst(
        confirmation(true),
        intendedStateOnChain().pipe(
          Effect.repeat({
            until: (reached) => reached,
            schedule: Schedule.spaced(SUBMIT_RECOVERY_POLL_INTERVAL_MS),
          }),
          Effect.as("lost_race" as const),
        ),
      );
};

/**
 * A refusal for spent inputs whose intended state is on chain: "sent" when
 * the exact transaction is the one that landed (a resend of a landed
 * transaction), otherwise a lost race, logged at info. Either way the state
 * the transaction was for is reached, so nothing here is an error.
 */
export const lostRace = (
  lucid: Pick<LucidEvolution, "transactionStatus">,
  txHash: string,
  submitError: string,
  label: string | undefined,
): Effect.Effect<SubmitOutcome> =>
  Effect.gen(function* () {
    const read = yield* Effect.either(
      Effect.tryPromise(() => lucid.transactionStatus(txHash)),
    );
    if (read._tag === "Right" && read.right.status === "confirmed") {
      yield* Effect.logInfo(
        `Tx ${txHash} confirmed after submit race; treating submission as successful.`,
      );
      return "sent";
    }
    yield* Effect.logInfo(
      `Tx ${txHash}${label === undefined ? "" : ` (${label})`} lost an L1 race: its inputs were already spent by another transaction that reached the state it was sent for; carrying on without it: ${submitError}`,
    );
    return "lost_race";
  });

/**
 * Whether `txHash`, sent once with an unknown outcome, landed: its status on
 * the provider (the follower's view of the exact id). A transaction that
 * landed phase-2 invalid fails here: its collateral is gone and resending
 * cannot change that. An unreadable status resends the same bytes, which
 * cannot land twice.
 */
export const landedAfterOutcomeUnknown = (
  lucid: Pick<LucidEvolution, "transactionStatus">,
  txHash: string,
  outcomeUnknown: unknown,
): Effect.Effect<boolean, Error> =>
  Effect.gen(function* () {
    const read = yield* Effect.either(
      Effect.tryPromise(() => lucid.transactionStatus(txHash)),
    );
    if (read._tag === "Left") {
      yield* Effect.logWarning(
        `Tx ${txHash} status unreadable after a submit with an unknown outcome; sending the same bytes again: ${formatUnknownError(read.left, { includeCause: true })}`,
      );
      return false;
    }
    if (read.right.status === "failed") {
      return yield* Effect.fail(
        new Error(
          `Tx ${txHash} landed phase-2 invalid after a submit with an unknown outcome`,
          { cause: outcomeUnknown },
        ),
      );
    }
    if (read.right.status !== "confirmed") return false;
    yield* Effect.logInfo(
      `Tx ${txHash} landed after a submit with an unknown outcome; not sending it again.`,
    );
    return true;
  });
