import { SqlError } from "@effect/sql";
import { Effect } from "effect";

import { DatabaseError } from "./utils/common.js";

/** The one refusal a prepare gets while another pending journal is active. */
export const ACTIVE_PENDING_JOURNAL_REFUSAL =
  "Refusing to prepare a new pending block while another active pending-finalization record exists";

/** Unique partial index admitting at most one active pending journal. */
export const SINGLE_ACTIVE_INDEX =
  "uniq_pending_block_finalizations_single_active";

const UNIQUE_VIOLATION = "23505";

/** True when a SQL failure is the single-active index rejecting an insert. */
export const isSingleActiveIndexViolation = (error: unknown): boolean => {
  let current = error;
  while (typeof current === "object" && current !== null) {
    const { code, constraint_name } = current as {
      readonly code?: unknown;
      readonly constraint_name?: unknown;
    };
    if (code === UNIQUE_VIOLATION && constraint_name === SINGLE_ACTIVE_INDEX)
      return true;
    current = (current as { readonly cause?: unknown }).cause;
  }
  return false;
};

/**
 * Concurrent prepares can both pass the active-journal pre-check (neither
 * sees the other's uncommitted row); the index then rejects the loser's
 * insert. The loser gets the same typed refusal as the pre-check.
 */
export const refuseOnSingleActiveIndexLoss =
  (tableName: string, requestedHeaderHash: Buffer) =>
  <A, E, R>(
    insert: Effect.Effect<A, E | SqlError.SqlError, R>,
  ): Effect.Effect<A, E | SqlError.SqlError | DatabaseError, R> =>
    Effect.catchIf(
      insert,
      (error): error is SqlError.SqlError =>
        error instanceof SqlError.SqlError &&
        isSingleActiveIndexViolation(error.cause),
      () =>
        Effect.fail(
          new DatabaseError({
            table: tableName,
            message: ACTIVE_PENDING_JOURNAL_REFUSAL,
            cause: `index=${SINGLE_ACTIVE_INDEX},requested_header_hash=${requestedHeaderHash.toString("hex")}`,
          }),
        ),
    );
