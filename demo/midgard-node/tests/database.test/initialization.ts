import { SqlClient } from "@effect/sql";
import { it } from "@effect/vitest";
import { Effect } from "effect";
import { describe, expect } from "vitest";

import { CekProgramMaterialDB } from "../../src/database/index.js";
import { AdmissionSql, BatchSql } from "../../src/services/database.js";
import { isolatedDb } from "./fixtures.js";

export const registerInitializationTests = () => {
  describe("Database: initialization and basic operations", () => {
    it.effect("initialize and flush", (_) =>
      isolatedDb(
        Effect.gen(function* () {
          // Smoke select to ensure connection works
          const sql = yield* SqlClient.SqlClient;
          const now = yield* sql<Date>`SELECT NOW()`;
          expect(now.length).toBeGreaterThan(0);
        }),
      ),
    );

    it.effect(
      "requires every active mempool row to carry canonical transaction bytes",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            const columns = yield* sql<{
              readonly is_nullable: "YES" | "NO";
            }>`SELECT is_nullable
            FROM information_schema.columns
            WHERE table_schema = 'public'
              AND table_name = 'mempool'
              AND column_name = 'tx'`;
            expect(columns).toHaveLength(1);
            expect(columns[0]?.is_nullable).toBe("NO");
          }),
        ),
    );

    it.effect(
      "creates CEK durable pins and admission ownership with composite membership integrity",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            const durablePin = yield* sql<{
              readonly is_nullable: "YES" | "NO";
              readonly column_default: string | null;
            }>`SELECT is_nullable, column_default
            FROM information_schema.columns
            WHERE table_schema = 'public'
              AND table_name =
                ${CekProgramMaterialDB.membershipTableName}
              AND column_name = 'durable_pin'`;
            expect(durablePin).toEqual([
              {
                is_nullable: "NO",
                column_default: "true",
              },
            ]);
            const ownerForeignKeys = yield* sql<{
              readonly definition: string;
            }>`SELECT pg_get_constraintdef(oid) AS definition
            FROM pg_constraint
            WHERE conrelid =
                ${CekProgramMaterialDB.admissionOwnerTableName}::regclass
              AND contype = 'f'`;
            expect(ownerForeignKeys).toHaveLength(1);
            expect(ownerForeignKeys[0]?.definition).toContain(
              "FOREIGN KEY (program_envelope_hash, material_root)",
            );
            expect(ownerForeignKeys[0]?.definition).toContain(
              "REFERENCES cek_program_material_memberships(program_envelope_hash, material_root) ON DELETE CASCADE",
            );
          }),
        ),
    );

    it.effect("drops the superseded admission-payload hash lookup index", () =>
      isolatedDb(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          const indexes = yield* sql<{ readonly index_name: string }>`
            SELECT indexname AS index_name
            FROM pg_indexes
            WHERE schemaname = 'public'
              AND indexname = 'idx_tx_admission_payloads_tx_id_hash'`;
          expect(indexes).toEqual([]);
        }),
      ),
    );

    it.effect(
      "uses a dedicated active-lease index for owner and tx-id point lookups",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            const indexes = yield* sql<{
              readonly index_name: string;
              readonly index_definition: string;
            }>`SELECT
              indexname AS index_name,
              indexdef AS index_definition
            FROM pg_indexes
            WHERE schemaname = 'public'
              AND tablename = 'tx_admissions'
              AND indexname IN (
                'idx_tx_admissions_active_lease',
                'idx_tx_admissions_lease'
              )
            ORDER BY indexname`;
            expect(indexes).toHaveLength(2);
            expect(indexes[0]?.index_name).toBe(
              "idx_tx_admissions_active_lease",
            );
            expect(indexes[0]?.index_definition).toContain(
              "(lease_owner, tx_id)",
            );
            expect(indexes[0]?.index_definition).toContain(
              "WHERE (status = 'validating'::tx_admission_status)",
            );
            expect(indexes[1]?.index_name).toBe("idx_tx_admissions_lease");
            expect(indexes[1]?.index_definition).toContain(
              "(lease_expires_at)",
            );
          }),
        ),
    );

    it.effect(
      "keeps only the rebuildable transaction-delta cache unlogged",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            const persistence = yield* sql<{
              readonly relation_name: string;
              readonly persistence: "p" | "u";
            }>`SELECT relname AS relation_name, relpersistence AS persistence
          FROM pg_class
          WHERE relname IN (
            'mempool_tx_deltas',
            'tx_admissions',
            'tx_admission_payloads'
          )
          ORDER BY relname`;
            expect(persistence).toEqual([
              { relation_name: "mempool_tx_deltas", persistence: "u" },
              { relation_name: "tx_admission_payloads", persistence: "p" },
              { relation_name: "tx_admissions", persistence: "p" },
            ]);
          }),
        ),
    );

    it.effect(
      "keeps admission and batch traffic on distinct labeled pools",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const batchSql = yield* BatchSql;
            const admissionSql = yield* AdmissionSql;
            const [batch, admission] = yield* Effect.all(
              [
                batchSql<{
                  readonly application_name: string;
                  readonly backend_pid: number;
                }>`SELECT current_setting('application_name') AS application_name, pg_backend_pid() AS backend_pid`,
                admissionSql<{
                  readonly application_name: string;
                  readonly backend_pid: number;
                }>`SELECT current_setting('application_name') AS application_name, pg_backend_pid() AS backend_pid`,
              ],
              { concurrency: "unbounded" },
            );

            expect(batch[0]?.application_name).toBe("midgard-node-batch");
            expect(admission[0]?.application_name).toBe(
              "midgard-node-admission",
            );
            expect(batch[0]?.backend_pid).not.toBe(admission[0]?.backend_pid);
          }),
        ),
    );
  });
};
