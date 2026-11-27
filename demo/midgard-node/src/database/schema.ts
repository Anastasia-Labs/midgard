/**
 * The node database schema: `schema.sql`, created whole on an empty
 * database, then the L1 follower's tables (`follower-schema.ts`). The
 * database records the checksum of the text that created it (`node_schema`).
 * A database created by a different text, or holding tables but no record,
 * is refused: it is recreated, never altered.
 */
import { SqlClient } from "@effect/sql";
import { Data, Effect } from "effect";

import type { Database } from "../services/database.js";
import { sha256Hex } from "../sha256.js";
import { installFollowerSchema } from "./follower-schema.js";
import schemaSql from "./schema.sql";

export const SCHEMA_CHECKSUM = sha256Hex(schemaSql);

export class NodeSchemaError extends Data.TaggedError("NodeSchemaError")<{
  readonly message: string;
}> {}

const RECREATE =
  "recreate it (drop it and create it empty); the node creates the schema when it starts";

/**
 * Creates the schema on an empty database, or checks that the database was
 * created by this text, in one transaction under an advisory lock, so two
 * processes never create it concurrently.
 */
export const ensure: Effect.Effect<void, unknown, Database> = Effect.gen(
  function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* sql`SELECT pg_advisory_xact_lock(hashtext('midgard-node:schema'))`;
        const [state] = yield* sql<{
          readonly recorded: boolean;
          readonly occupied: boolean;
        }>`SELECT
            to_regclass('public.node_schema') IS NOT NULL AS recorded,
            EXISTS (SELECT 1 FROM pg_tables WHERE schemaname = 'public') AS occupied`;
        if (state?.recorded === true) {
          const [row] = yield* sql<{
            readonly checksum: string;
          }>`SELECT checksum FROM node_schema`;
          if (row?.checksum !== SCHEMA_CHECKSUM)
            return yield* new NodeSchemaError({
              message: `This database was created by a different node schema; ${RECREATE}`,
            });
        } else if (state?.occupied === true) {
          return yield* new NodeSchemaError({
            message: `This database holds tables but no node schema record; ${RECREATE}`,
          });
        } else {
          yield* sql.unsafe(schemaSql);
          yield* sql`CREATE TABLE node_schema (checksum text NOT NULL)`;
          yield* sql`INSERT INTO node_schema (checksum) VALUES (${SCHEMA_CHECKSUM})`;
        }
        // The node's SQL joins the follower's admission identity, so its
        // tables are part of every node database.
        yield* installFollowerSchema;
      }),
    );
  },
);
