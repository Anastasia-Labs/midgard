import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { Database } from "../services/database.js";
import {
  admissionOwnerTableName,
  entryTableName,
  membershipTableName,
  retainedStateOwnerTableName,
  STORE_ADVISORY_LOCK_KEY,
  STORE_ADVISORY_LOCK_NAMESPACE,
} from "./cekProgramMaterial.canonical-entries.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

/** Called after owner release, including the authenticated DA retention prune. */
export const collectUnownedMaterial: Effect.Effect<
  void,
  DatabaseError,
  Database
> = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  yield* sql.withTransaction(
    Effect.gen(function* () {
      yield* sql`SELECT pg_advisory_xact_lock(${STORE_ADVISORY_LOCK_NAMESPACE}, ${STORE_ADVISORY_LOCK_KEY})`;
      yield* sql`DELETE FROM ${sql(membershipTableName)} AS membership
        WHERE membership.durable_pin = false
          AND NOT EXISTS (SELECT 1 FROM ${sql(admissionOwnerTableName)} owner
            WHERE owner.program_envelope_hash = membership.program_envelope_hash
              AND owner.material_root = membership.material_root)
          AND NOT EXISTS (SELECT 1 FROM ${sql(retainedStateOwnerTableName)} retained
            WHERE retained.program_envelope_hash = membership.program_envelope_hash
              AND retained.material_root = membership.material_root)`;
      yield* sql`DELETE FROM ${sql(entryTableName)} AS material
        WHERE NOT EXISTS (SELECT 1 FROM ${sql(membershipTableName)} membership
          WHERE membership.material_root = material.material_root)`;
    }),
  );
}).pipe(
  sqlErrorToDatabaseError(
    entryTableName,
    "Failed to collect unowned CEK material",
  ),
);
