import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { tableName } from "./pendingBlockFinalizations.columns.js";
import {
  Columns as LeaseColumns,
  SCOPE as LEASE_SCOPE,
  Status as LeaseStatus,
  tableName as leaseTableName,
} from "./stateQueueMutationLeases.columns.js";
import { DatabaseError } from "./utils/common.js";

export const MUTATION_LEASE_NOT_HELD =
  "Refusing a journal write whose state-queue mutation lease is no longer held";

/**
 * Holds `token`'s lease for the rest of the caller's transaction: the lease
 * row must be active and unexpired, and it stays locked `FOR SHARE` until the
 * transaction ends. A journal's preparation and its signed intent are written
 * under this lock, so no later lease holder can see the window between them
 * open: taking the scope over needs the old row marked released or failed
 * (one active lease per scope, `uniq_state_queue_mutation_leases_active_scope`),
 * and that update waits behind this lock until the write commits. An unsigned
 * active journal a later holder finds was therefore never signed and never
 * will be (`abandonUnsignedActiveJournal`).
 *
 * `NOW()` is the transaction's start time, so a lease that expired after the
 * transaction began passes the check. That cannot let a write through after
 * a takeover: the expiry is the same update of the row, and it waits for this
 * lock. Renewal waits behind it too, for the length of the write.
 */
export const holdMutationLease = (token: string, headerHash: Buffer) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const held = yield* sql`SELECT 1 FROM ${sql(leaseTableName)}
      WHERE ${sql(LeaseColumns.TOKEN)} = ${token}
        AND ${sql(LeaseColumns.SCOPE)} = ${LEASE_SCOPE}
        AND ${sql(LeaseColumns.STATUS)} = ${LeaseStatus.Active}
        AND ${sql(LeaseColumns.EXPIRES_AT)} >= NOW()
      FOR SHARE`;
    if (held.length !== 1)
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message: MUTATION_LEASE_NOT_HELD,
          cause: `header_hash=${headerHash.toString("hex")},state_queue_lease_token=${token}`,
        }),
      );
  });
