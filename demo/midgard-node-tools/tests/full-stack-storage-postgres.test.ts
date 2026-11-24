import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import * as NodeSchema from "midgard-node/database/schema";
import {
  provideDatabaseLayers,
  resetApplicationTables,
} from "midgard-node/tests/utils";
import { describe, expect, it } from "vitest";

import {
  ATTACHMENT_QUERY,
  createIdentityQuery,
  FRESH_STORAGE_QUERY,
  IDENTITY_ROW_QUERY,
  IDENTITY_TABLE_QUERY,
} from "../src/full-stack/storage.js";

const dbEnabled = process.env.MIDGARD_SKIP_DB_TESTS !== "1";
const marker = {
  runId: "de615069-9923-4006-9f83-8945969206d3",
  hubOraclePolicyId: "a".repeat(56),
};

/** Runs `body` on a never-deployed database with its schema, as the stack finds it. */
const onNewDatabase = (
  body: (sql: SqlClient.SqlClient) => Effect.Effect<void, unknown>,
) =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* NodeSchema.ensure;
        const clean = Effect.gen(function* () {
          yield* sql.unsafe(
            "DROP TABLE IF EXISTS full_stack_controller_identity",
          );
          yield* resetApplicationTables;
          // The reset keeps the follower's writer row; a never-deployed
          // database holds it at its seed.
          yield* sql.unsafe(
            "UPDATE l1_follower_writer SET writer_epoch = 0, next_generation = 0",
          );
        });
        yield* clean;
        yield* body(sql).pipe(Effect.ensuring(Effect.orDie(clean)));
      }),
    ) as Effect.Effect<void, unknown, never>,
  );
/** A multi-statement query answers one row list per statement; psql -At prints the last. */
const lastRow = (rows: readonly unknown[]): unknown => {
  const row = rows.at(-1);
  return Array.isArray(row) ? lastRow(row) : row;
};
/** The JSON document the query's last row carries, as psql -At prints it. */
const json = (sql: SqlClient.SqlClient, query: string) =>
  sql.unsafe(query).pipe(
    Effect.map((rows) => {
      const row = lastRow(rows) as Record<string, unknown> | undefined;
      return row === undefined ? null : Object.values(row)[0];
    }),
  );
const failure = (sql: SqlClient.SqlClient, query: string) =>
  sql.unsafe(query).pipe(
    Effect.flip,
    Effect.map((error) => {
      const cause = (error as { cause?: { message?: string } }).cause;
      return cause?.message ?? String(error);
    }),
  );

describe.skipIf(!dbEnabled)("stack storage queries against Postgres", () => {
  it("treats a never-deployed database as fresh and refuses any deployment row", () =>
    onNewDatabase((sql) =>
      Effect.gen(function* () {
        yield* sql.unsafe(FRESH_STORAGE_QUERY);
        yield* sql.unsafe(
          `INSERT INTO node_deployment (hub_oracle_policy_id) VALUES ('${marker.hubOraclePolicyId}')`,
        );
        expect(yield* failure(sql, FRESH_STORAGE_QUERY)).toContain(
          "Fresh deployment requires empty local storage: node_deployment",
        );
        expect(yield* json(sql, ATTACHMENT_QUERY)).toEqual({
          initTxHashes: [],
        });
        yield* sql.unsafe(
          `INSERT INTO l1_protocol_init (one_shot, tx_hash, slot) VALUES (decode('${"01".repeat(34)}', 'hex'), decode('${"ab".repeat(32)}', 'hex'), 7)`,
        );
        expect(yield* json(sql, ATTACHMENT_QUERY)).toEqual({
          initTxHashes: ["ab".repeat(32)],
        });
      }),
    ));
  it("accepts the calibration seed only with its seeded values", () =>
    onNewDatabase((sql) =>
      Effect.gen(function* () {
        yield* sql.unsafe(
          "UPDATE commit_build_calibration SET sample_count = sample_count + 1",
        );
        expect(yield* failure(sql, FRESH_STORAGE_QUERY)).toContain(
          "Fresh deployment requires empty local storage: commit_build_calibration",
        );
      }),
    ));
  it("accepts the L1 follower's writer seed only before a follower has run", () =>
    onNewDatabase((sql) =>
      Effect.gen(function* () {
        yield* sql.unsafe(
          "UPDATE l1_follower_writer SET writer_epoch = writer_epoch + 1",
        );
        expect(yield* failure(sql, FRESH_STORAGE_QUERY)).toContain(
          "Fresh deployment requires empty local storage: l1_follower_writer",
        );
      }),
    ));
  it("records one controller identity and reports a store without one", () =>
    onNewDatabase((sql) =>
      Effect.gen(function* () {
        expect(yield* json(sql, IDENTITY_TABLE_QUERY)).toEqual({
          exists: false,
        });
        expect(yield* failure(sql, IDENTITY_ROW_QUERY)).toContain(
          'relation "full_stack_controller_identity" does not exist',
        );
        const row = {
          singleton: true,
          run_id: marker.runId,
          hub_oracle_policy_id: marker.hubOraclePolicyId,
        };
        expect(yield* json(sql, createIdentityQuery(marker))).toEqual(row);
        expect(
          yield* json(
            sql,
            createIdentityQuery({ ...marker, runId: "another-run" }),
          ),
        ).toEqual(row);
        expect(yield* json(sql, IDENTITY_TABLE_QUERY)).toEqual({
          exists: true,
        });
        expect(yield* failure(sql, FRESH_STORAGE_QUERY)).toContain(
          "full_stack_controller_identity",
        );
      }),
    ));
});
