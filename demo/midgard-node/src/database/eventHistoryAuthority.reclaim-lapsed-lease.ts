import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";

import type { Database } from "../services/database.js";
import {
  type Row,
  tableName,
  type Token,
  tokenFromRow,
} from "./eventHistoryAuthority.js";
import { type DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

/** Re-take this owner's own lease after it lapsed with nobody else claiming
 * it. One compare-and-set under the row lock: deployment, owner and
 * generation still name `token`, the row is not suspended, and its lease has
 * ended by the database clock. The claim advances the generation into
 * Recovering, exactly as a restart's acquire would, so every holder of the
 * lapsed generation stays fenced. None means nothing changed: the row moved
 * on (another owner, a newer generation, a suspension) or its lease is live. */
export const reclaimLapsedLease = (
  token: Token,
  leaseDurationMs: number,
  reason: string,
): Effect.Effect<Option.Option<Token>, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Row>`UPDATE event_history_authority
      SET generation = generation + 1, state = 'recovering', reason = ${reason},
          lease_until = clock_timestamp() + (${leaseDurationMs} * interval '1 millisecond'),
          updated_at = clock_timestamp()
      WHERE singleton = true AND state <> 'suspended'
        AND deployment_identity = ${Buffer.from(token.deploymentIdentity, "hex")}
        AND owner_token = ${token.ownerToken}::uuid AND generation = ${token.generation}::bigint
        AND lease_until <= clock_timestamp()
      RETURNING *`;
    return Option.map(Option.fromNullable(rows[0]), tokenFromRow);
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to reclaim history authority"),
  );
