/**
 * The state-queue mutation lease a journal fixture is written under. A
 * journal is prepared and signed only while the lease it names is the
 * scope's active, unexpired lease (`holdMutationLease`), as the commit
 * worker holds it; a fixture that writes journals directly takes the lease
 * for the write here.
 */
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { PendingBlockFinalizationsDB } from "../../src/database/index.js";
import {
  Columns,
  SCOPE,
  Status,
  tableName,
} from "../../src/database/stateQueueMutationLeases.columns.js";

/**
 * Runs `effect` with `token` as the scope's active lease. A lease row the
 * helper had to create is deleted afterwards, so the fixture leaves the lease
 * table as it found it; an existing row for `token` is used as it stands
 * (a released or expired one refuses the write, as in production).
 */
export const withTestMutationLease =
  (token: string) =>
  <A, E, R>(effect: Effect.Effect<A, E, R>) =>
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      return yield* Effect.acquireUseRelease(
        sql<{ token: string }>`INSERT INTO ${sql(tableName)} (
            ${sql(Columns.TOKEN)}, ${sql(Columns.SCOPE)}, ${sql(Columns.HOLDER)},
            ${sql(Columns.STATUS)}, ${sql(Columns.EXPIRES_AT)}
          ) VALUES (
            ${token}, ${SCOPE}, 'journal-fixture', ${Status.Active},
            NOW() + INTERVAL '1 hour'
          ) ON CONFLICT (${sql(Columns.TOKEN)}) DO NOTHING
          RETURNING ${sql(Columns.TOKEN)}`.pipe(
          Effect.map((rows) => rows.length === 1),
        ),
        () => effect,
        (inserted) =>
          inserted
            ? sql`DELETE FROM ${sql(tableName)}
                WHERE ${sql(Columns.TOKEN)} = ${token}`.pipe(Effect.orDie)
            : Effect.void,
      );
    });

/** `preparePendingSubmission(input, options)` under the lease `input` names. */
export const prepareUnderTestLease = (
  input: PendingBlockFinalizationsDB.PrepareInput,
  options?: Parameters<
    typeof PendingBlockFinalizationsDB.preparePendingSubmission
  >[1],
) =>
  PendingBlockFinalizationsDB.preparePendingSubmission(input, options).pipe(
    withTestMutationLease(input.metadata.stateQueueLeaseToken),
  );
