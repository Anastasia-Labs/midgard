import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { SqlClient } from "@effect/sql";
import { Duration, Effect } from "effect";

import { Database } from "../services/database.js";
import {
  Columns,
  Status,
  tableName,
} from "./stateQueueMutationLeases.columns.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

export const release = (
  token: string,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`UPDATE ${sql(tableName)}
      SET ${sql(Columns.STATUS)} = ${Status.Released},
          ${sql(Columns.RELEASED_AT)} = NOW()
      WHERE ${sql(Columns.TOKEN)} = ${token}
        AND ${sql(Columns.STATUS)} = ${Status.Active}`;
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to release state-queue mutation lease",
    ),
  );

export const markFailed = (
  token: string,
  error: string,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`UPDATE ${sql(tableName)}
      SET ${sql(Columns.STATUS)} = ${Status.Failed},
          ${sql(Columns.RELEASED_AT)} = NOW(),
          ${sql(Columns.LAST_ERROR)} = ${error.slice(0, 4000)}
      WHERE ${sql(Columns.TOKEN)} = ${token}
        AND ${sql(Columns.STATUS)} = ${Status.Active}`;
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to mark state-queue mutation lease failed",
    ),
  );

/** How a lease's holder records the end of its hold. */
export type LeaseSettlement =
  | { readonly kind: "released" }
  | { readonly kind: "failed"; readonly error: string };

export const LEASE_SETTLE_ATTEMPTS = 5;
export const LEASE_SETTLE_BASE_DELAY_MS = 200;
const MAX_UNSETTLED_OWN_LEASES = 64;

/** Leases this process holds whose release or failure record did not reach
 * the database. Each write is a compare-and-set on the lease's own token, so
 * a retry can only end that lease, never a later holder's. */
const unsettledOwnLeases = new Map<string, LeaseSettlement>();

export const unsettledOwnLeaseTokens = (): readonly string[] => [
  ...unsettledOwnLeases.keys(),
];

const settleOnce = (token: string, settlement: LeaseSettlement) =>
  settlement.kind === "released"
    ? release(token)
    : markFailed(token, settlement.error);

const rememberUnsettled = (token: string, settlement: LeaseSettlement) => {
  unsettledOwnLeases.delete(token);
  // A dropped token's row still ends at its expiry.
  while (unsettledOwnLeases.size >= MAX_UNSETTLED_OWN_LEASES) {
    const oldest = unsettledOwnLeases.keys().next().value;
    if (oldest === undefined) break;
    unsettledOwnLeases.delete(oldest);
  }
  unsettledOwnLeases.set(token, settlement);
};

/**
 * Records the end of a hold, retrying a failed write with doubling backoff
 * (LEASE_SETTLE_ATTEMPTS attempts, about 3 s in all). A write that still
 * fails leaves the token for the next acquisition in this process to retry,
 * so a transient database fault never holds the lease until its expiry.
 * Runs in the holder's uninterruptible release finalizer: against a server
 * that will not connect, each attempt waits out the 10 s pool connect
 * timeout, so ending or interrupting a holder can take about 53 s at worst.
 * A connection that hangs mid-statement has no statement timeout here, as
 * the single release before these retries had none.
 */
export const settleLeaseDurably = (
  token: string,
  holder: string,
  settlement: LeaseSettlement,
): Effect.Effect<void, never, Database> =>
  Effect.gen(function* () {
    for (let attempt = 1; ; attempt++) {
      const result = yield* Effect.either(settleOnce(token, settlement));
      if (result._tag === "Right") {
        unsettledOwnLeases.delete(token);
        if (attempt > 1)
          yield* Effect.logInfo(
            `State-queue mutation lease ${settlement.kind} on attempt ${attempt.toString()}; holder=${holder},token=${token}`,
          );
        return;
      }
      const error = formatUnknownError(result.left);
      if (attempt >= LEASE_SETTLE_ATTEMPTS) {
        rememberUnsettled(token, settlement);
        yield* Effect.logWarning(
          `State-queue mutation lease could not be ${settlement.kind} after ${attempt.toString()} attempts; the next acquisition retries it; holder=${holder},token=${token},error=${error}`,
        );
        return;
      }
      const delayMs = LEASE_SETTLE_BASE_DELAY_MS * 2 ** (attempt - 1);
      yield* Effect.logWarning(
        `State-queue mutation lease ${settlement.kind} write failed (attempt ${attempt.toString()}/${LEASE_SETTLE_ATTEMPTS.toString()}); retrying in ${delayMs.toString()}ms; holder=${holder},token=${token},error=${error}`,
      );
      yield* Effect.sleep(Duration.millis(delayMs));
    }
  });

/** One attempt at every lease this process failed to settle. */
export const settleUnsettledOwnLeases: Effect.Effect<void, never, Database> =
  Effect.gen(function* () {
    for (const [token, settlement] of [...unsettledOwnLeases]) {
      const result = yield* Effect.either(settleOnce(token, settlement));
      if (result._tag === "Right") {
        unsettledOwnLeases.delete(token);
        yield* Effect.logInfo(
          `Settled a state-queue mutation lease left unsettled, at a later acquisition; token=${token}`,
        );
      } else {
        yield* Effect.logWarning(
          `State-queue mutation lease still unsettled; token=${token},error=${formatUnknownError(result.left)}`,
        );
      }
    }
  });
