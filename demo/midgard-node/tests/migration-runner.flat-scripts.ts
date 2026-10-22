import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { installFollowerSchema } from "../src/database/follower-schema.js";
import {
  MIGRATION_MANIFEST_HASH,
  MIGRATIONS,
} from "../src/database/migrations/index.js";
import * as MigrationRunner from "../src/database/migrations/runner.js";
import { BatchSql } from "../src/services/database.js";
import { testDatabaseName } from "./test-env.js";
import { provideDatabaseLayers } from "./utils.js";

export const withPredecessor = <A, E, R>(test: Effect.Effect<A, E, R>) =>
  provideDatabaseLayers(
    Effect.gen(function* () {
      const sql = yield* BatchSql;
      const [database] = yield* sql<{
        name: string;
      }>`SELECT current_database() AS name`;
      expect(database!.name).toBe(testDatabaseName());
      expect(database!.name).toMatch(/^midgard_(?:contrib|test)_/u);
      yield* sql.unsafe("DROP SCHEMA public CASCADE; CREATE SCHEMA public");
      yield* MigrationRunner.getStatus.pipe(
        Effect.provideService(SqlClient.SqlClient, sql),
      );
      for (const migration of MIGRATIONS.filter(
        (entry) => entry.version <= 28,
      )) {
        for (const statement of MigrationRunner.splitSqlStatements(
          migration.sql,
        ))
          yield* sql.unsafe(statement);
        yield* sql`INSERT INTO schema_migrations
          (version, name, checksum_sha256, manifest_hash_sha256, app_version, execution_ms, applied_by)
          VALUES (${migration.version}, ${migration.name}, ${migration.checksumSha256},
            ${MIGRATION_MANIFEST_HASH}, 'flat-predecessor', 1, 'flat-test')`;
      }
      return yield* test.pipe(Effect.provideService(SqlClient.SqlClient, sql));
    }),
  );

const migration = MigrationRunner.migrate({
  appVersion: "flat-test",
  actor: "flat-upgrade",
});
const txId = Buffer.alloc(32, 0x21);
const txCbor = Buffer.from("8101", "hex");
const fullHash = Buffer.alloc(32, 0x22);

export const retiredStorageShape = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  return yield* sql`SELECT table_name, column_name FROM information_schema.columns
    WHERE table_schema = 'public' AND (
      table_name IN ('cek_program_material_entries', 'cek_program_material_memberships',
        'cek_program_material_admission_owners', 'cek_program_material_retained_state_owners')
      OR (table_name IN ('tx_admission_payloads', 'pending_block_finalization_txs', 'forced_transaction_utxos')
        AND column_name IN ('cek_program_material_sidecar_cbor', 'cek_program_material_sidecar_sha256')))
    ORDER BY table_name, column_name`;
});

describe("stored Flat migration", () => {
  it.each([
    "node_deployment",
    "tx_admission_payloads",
    "tx_admissions",
    "tx_rejections",
    "da_payloads",
  ] as const)(
    "refuses retained %s state without altering data or the schema ledger",
    async (table) => {
      await Effect.runPromise(
        withPredecessor(
          Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            if (table === "node_deployment") {
              yield* sql`INSERT INTO node_deployment (deployment_id) VALUES (${"31".repeat(32)})`;
            } else if (table === "tx_admissions") {
              yield* sql`INSERT INTO tx_admissions (tx_id, status, terminal_at, submit_source)
                VALUES (${txId}, 'accepted', now(), 'native')`;
            } else if (table === "tx_rejections") {
              yield* sql`INSERT INTO tx_rejections (tx_id, reject_code)
                VALUES (${txId}, 'ScriptProgramEncoding')`;
            } else if (table === "tx_admission_payloads") {
              yield* sql`INSERT INTO tx_admissions (tx_id, status, submit_source)
            VALUES (${txId}, 'queued', 'native')`;
              yield* sql`INSERT INTO tx_admission_payloads
            (tx_id, tx_canonical_cbor, tx_full_hash_v1, cek_program_material_sidecar_cbor, cek_program_material_sidecar_sha256)
            VALUES (${txId}, ${txCbor}, ${fullHash}, ${Buffer.from("80", "hex")}, ${Buffer.alloc(32, 0x23)})`;
            } else {
              const root = "00".repeat(32);
              yield* sql`INSERT INTO da_payloads
            (header_hash, version, payload_cbor, payload_sha256, utxos_root,
             forced_transactions_root, transactions_root, deposits_root, withdrawals_root,
             transition_trace_root, event_to_step_root, withdrawal_count, forced_transaction_count,
             l2_transaction_count, deposit_count, total_event_count, transition_step_count,
             consensus_profile_id, block_start_time, block_end_time, block_time)
            VALUES (${Buffer.alloc(28, 0x32)}, 1, ${Buffer.from("80", "hex")}, ${Buffer.alloc(32, 0x33)},
              ${root}, ${root}, ${root}, ${root}, ${root}, ${root}, ${root},
              0, 0, 0, 0, 0, 0, 'midgard-consensus-v1', '2026-01-01', '2026-01-01', '2026-01-01')`;
            }
            const rowsBefore = yield* sql`SELECT * FROM ${sql(table)}`;
            const ledgerBefore =
              yield* sql`SELECT * FROM schema_migrations ORDER BY version`;
            const storageBefore = yield* retiredStorageShape;
            expect(storageBefore.length).toBeGreaterThan(6);
            const failed = yield* Effect.either(migration);
            expect(failed._tag).toBe("Left");
            if (failed._tag === "Left")
              expect(failed.left).toMatchObject({
                code: "schema_migration_requires_fresh_database",
                message: `Migration 29_drop_cek_program_material refused this database: Flat-script migration needs a fresh node database: ${table} retains state. Flat scripts start from a fresh genesis. Point the node at a new, empty database and run db:migrate there.`,
              });
            expect(yield* sql`SELECT * FROM ${sql(table)}`).toEqual(rowsBefore);
            expect(
              yield* sql`SELECT * FROM schema_migrations ORDER BY version`,
            ).toEqual(ledgerBefore);
            expect(yield* retiredStorageShape).toEqual(storageBefore);
            expect(
              yield* sql`SELECT event_type FROM schema_migration_events
          WHERE version = 29 AND actor = 'flat-upgrade' AND event_type = 'failed'`,
            ).toEqual([{ event_type: "failed" }]);
          }),
        ),
      );
    },
  );

  it.each([false, true])(
    "migrates an unbound empty database with installed follower schema=%s and keeps transaction identity constraints",
    async (followerInstalled) => {
      await Effect.runPromise(
        withPredecessor(
          Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            if (followerInstalled) yield* installFollowerSchema;
            const status = yield* migration;
            expect(status.compatible).toBe(true);
            expect(yield* retiredStorageShape).toEqual([]);
            yield* sql`INSERT INTO tx_admissions (tx_id, status, submit_source)
        VALUES (${txId}, 'queued', 'native')`;
            yield* sql`INSERT INTO tx_admission_payloads (tx_id, tx_canonical_cbor, tx_full_hash_v1)
        VALUES (${txId}, ${txCbor}, ${fullHash})`;
            expect(
              yield* sql`SELECT * FROM tx_admission_payloads WHERE tx_id = ${txId}`,
            ).toEqual([
              {
                tx_id: txId,
                tx_canonical_cbor: txCbor,
                tx_full_hash_v1: fullHash,
              },
            ]);
            const [constraint] = yield* sql<{
              definition: string;
            }>`SELECT pg_get_constraintdef(oid) AS definition
        FROM pg_constraint WHERE conrelid = 'public.forced_transaction_utxos'::regclass
          AND conname = 'forced_transaction_utxos_exact_transaction'`;
            for (const fragment of [
              "midgard-consensus-v1",
              "octet_length(native_tx_cbor) <= 295041",
              "octet_length(transaction_commitment) = 32",
            ])
              expect(constraint?.definition).toContain(fragment);
            const malformedIdentity =
              yield* Effect.either(sql`UPDATE tx_admission_payloads
        SET tx_full_hash_v1 = ${Buffer.alloc(31)} WHERE tx_id = ${txId}`);
            expect(malformedIdentity._tag).toBe("Left");
            expect(
              yield* sql`SELECT tx_full_hash_v1 FROM tx_admission_payloads WHERE tx_id = ${txId}`,
            ).toEqual([{ tx_full_hash_v1: fullHash }]);
          }),
        ),
      );
    },
  );
});
