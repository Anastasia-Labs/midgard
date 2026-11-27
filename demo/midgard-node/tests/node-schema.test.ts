import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { NodeSchema } from "../src/database/index.js";
import { BatchSql } from "../src/services/database.js";
import { provideDatabaseLayers } from "./utils.js";

/** Empties the shard and creates the schema on it. */
const recreate = (sql: SqlClient.SqlClient) =>
  Effect.gen(function* () {
    yield* sql.unsafe("DROP SCHEMA public CASCADE; CREATE SCHEMA public");
    yield* NodeSchema.ensure.pipe(
      Effect.provideService(SqlClient.SqlClient, sql),
    );
  });

const onShard = <A>(
  body: (sql: SqlClient.SqlClient) => Effect.Effect<A, unknown, never>,
) =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        const sql = yield* BatchSql;
        return yield* body(sql);
      }),
    ) as Effect.Effect<A, unknown, never>,
  );

describe("node schema", () => {
  it("refuses a database created by a different schema, naming the reset", () =>
    onShard((sql) =>
      Effect.gen(function* () {
        yield* sql`UPDATE node_schema SET checksum = 'other'`;
        const refusal = yield* NodeSchema.ensure.pipe(
          Effect.provideService(SqlClient.SqlClient, sql),
          Effect.flip,
        );
        expect(refusal).toBeInstanceOf(NodeSchema.NodeSchemaError);
        expect((refusal as NodeSchema.NodeSchemaError).message).toBe(
          "This database was created by a different node schema; recreate it (drop it and create it empty); the node creates the schema when it starts",
        );
      }).pipe(Effect.ensuring(Effect.orDie(recreate(sql)))),
    ));

  /** Only the rebuildable delta cache may lose its WAL. */
  const UNLOGGED_TABLES = ["mempool_tx_deltas"] as const;

  /**
   * Each replay/deployment discriminator the node relies on when reloading a
   * journal is enforced by the database, not only by the writer. The fragments
   * are matched against Postgres's own rendering of the constraint, so the
   * schema text's formatting cannot move them.
   */
  const REQUIRED_CHECKS: readonly {
    readonly table: string;
    readonly constraint: string;
    readonly fragments: readonly string[];
  }[] = [
    {
      table: "pending_block_finalizations",
      constraint: "pending_block_finalizations_format_version_check",
      fragments: ["format_version = 1"],
    },
    {
      table: "pending_block_finalizations",
      constraint: "pending_block_finalizations_replay_kind_check",
      fragments: ["ledger_delta_v1", "ledger_delta_native_mpf_v1"],
    },
    {
      table: "pending_block_finalizations",
      constraint: "pending_block_finalizations_mpf_replay_all_or_none_check",
      fragments: [
        "mpf_owner_schema = 1",
        "octet_length(mpf_owner_binary_sha256) = 32",
        "octet_length(mpf_replay_event_log) >= 92",
        "octet_length(mpf_replay_event_roots) = (mpf_replay_event_count * 32)",
        "encode(mpf_replay_base_root, 'hex'::text) = base_utxos_root",
        "encode(mpf_replay_candidate_root, 'hex'::text) = expected_utxos_root",
      ],
    },
    {
      table: "pending_block_finalizations",
      constraint: "pending_block_finalizations_hub_oracle_policy_id_check",
      fragments: ["'^[0-9a-f]{56}$'"],
    },
  ];

  it("creates the schema on an empty database, with its replay, deployment and durability contracts", () =>
    onShard((sql) =>
      Effect.gen(function* () {
        yield* recreate(sql);
        const [record] = yield* sql<{
          readonly checksum: string;
        }>`SELECT checksum FROM node_schema`;
        expect(record?.checksum).toBe(NodeSchema.SCHEMA_CHECKSUM);
        const [seed] = yield* sql<{
          readonly count: string;
        }>`SELECT count(*)::text AS count FROM commit_build_calibration`;
        expect(seed?.count).toBe("1");

        const unlogged = yield* sql<{
          readonly relname: string;
        }>`SELECT relname
             FROM pg_class
             WHERE relnamespace = 'public'::regnamespace
               AND relkind = 'r'
               AND relpersistence = 'u'
             ORDER BY relname`;
        expect(unlogged.map((row) => row.relname)).toEqual([
          ...UNLOGGED_TABLES,
        ]);

        const constraints = yield* sql<{
          readonly relname: string;
          readonly conname: string;
          readonly definition: string;
        }>`SELECT c.relname,
                  con.conname,
                  pg_get_constraintdef(con.oid) AS definition
             FROM pg_constraint AS con
             JOIN pg_class AS c ON c.oid = con.conrelid
             WHERE c.relnamespace = 'public'::regnamespace
               AND con.contype = 'c'`;
        const definitionByKey = new Map(
          constraints.map((row) => [
            `${row.relname}.${row.conname}`,
            row.definition,
          ]),
        );
        const unsatisfied = REQUIRED_CHECKS.flatMap((required) => {
          const definition = definitionByKey.get(
            `${required.table}.${required.constraint}`,
          );
          return definition === undefined
            ? [`${required.table}.${required.constraint}`]
            : required.fragments
                .filter((fragment) => !definition.includes(fragment))
                .map(
                  (fragment) =>
                    `${required.table}.${required.constraint}: ${fragment}`,
                );
        });
        expect(unsatisfied).toEqual([]);
      }),
    ));
});
