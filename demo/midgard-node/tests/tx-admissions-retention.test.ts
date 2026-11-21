import { createHash } from "node:crypto";

import { SqlClient } from "@effect/sql";
import { Effect, Ref } from "effect";
import { describe, expect, it } from "vitest";

import {
  MigrationRunner,
  TxAdmissionsDB,
  TxRejectionsDB,
} from "../src/database/index.js";
import { sweepTerminalAdmissions } from "../src/fibers/admission-retention.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

const RETENTION_MS = 60_000;
// One terminal mark stamps its whole batch with one time, to the microsecond.
const OLD_MARK = "2026-01-01 00:00:00.123456+00";
const LATER_OLD_MARK = "2026-01-01 00:00:01.654321+00";

const txIdOf = (label: string): Buffer =>
  createHash("sha256").update(label).digest();

const admit = (label: string) =>
  TxAdmissionsDB.tryInsert({
    txId: txIdOf(label),
    txCanonicalCbor: Buffer.from(`retention-payload:${label}`),
    submitSource: "native",
  });

const markTerminal = (
  label: string,
  status: "accepted" | "rejected",
  terminalAt: string | "now",
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const at =
      terminalAt === "now" ? sql`NOW()` : sql`${terminalAt}::timestamptz`;
    yield* sql`UPDATE tx_admissions
      SET status = ${status}::tx_admission_status,
        terminal_at = ${at},
        reject_code = ${status === "rejected" ? "E_TEST" : null}
      WHERE tx_id = ${txIdOf(label)}`;
  });

const remainingAdmissions = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{ tx_id: Buffer }>`SELECT tx_id FROM tx_admissions`;
  return new Set(rows.map((row) => row.tx_id.toString("hex")));
});

const freshDatabase = (name: string) =>
  Effect.gen(function* () {
    yield* MigrationRunner.migrate({ appVersion: name, actor: name });
    yield* resetApplicationTables;
  });

describe("terminal admission retention", () => {
  it("deletes old terminal rows nothing needs and keeps every other row", async () => {
    await Effect.runPromise(
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* freshDatabase("tx-admissions-retention-test");
          const sql = yield* SqlClient.SqlClient;
          const labels = [
            "accepted-old",
            "rejected-old",
            "accepted-pending",
            "accepted-processed",
            "rejected-with-evidence",
            "accepted-recent",
            "queued",
            "validating",
          ];
          for (const label of labels) {
            expect(yield* admit(label)).not.toBeNull();
          }
          yield* markTerminal("accepted-old", "accepted", OLD_MARK);
          yield* markTerminal("rejected-old", "rejected", OLD_MARK);
          yield* markTerminal("accepted-pending", "accepted", OLD_MARK);
          yield* markTerminal("accepted-processed", "accepted", OLD_MARK);
          yield* markTerminal("rejected-with-evidence", "rejected", OLD_MARK);
          yield* markTerminal("accepted-recent", "accepted", "now");
          yield* sql`UPDATE tx_admissions
          SET status = 'validating', lease_owner = 'retention-test',
            lease_expires_at = NOW() - INTERVAL '1 day'
          WHERE tx_id = ${txIdOf("validating")}`;
          yield* sql`UPDATE tx_admissions
          SET first_seen_at = ${OLD_MARK}::timestamptz,
            last_seen_at = ${OLD_MARK}::timestamptz,
            updated_at = ${OLD_MARK}::timestamptz
          WHERE tx_id IN (${txIdOf("queued")}, ${txIdOf("validating")})`;
          yield* sql`INSERT INTO mempool (tx_id, tx)
          VALUES (${txIdOf("accepted-pending")}, ${Buffer.from("tx")})`;
          yield* sql`INSERT INTO processed_mempool (tx_id, tx)
          VALUES (${txIdOf("accepted-processed")}, ${Buffer.from("tx")})`;
          yield* TxRejectionsDB.insert({
            [TxRejectionsDB.Columns.TX_ID]: txIdOf("rejected-with-evidence"),
            [TxRejectionsDB.Columns.REJECT_CODE]: "E_TEST",
            [TxRejectionsDB.Columns.REJECT_DETAIL]: null,
          });

          const cursor =
            yield* Ref.make<TxAdmissionsDB.TerminalAdmissionCursor | null>(
              null,
            );
          const sweep = yield* sweepTerminalAdmissions({
            retentionMs: RETENTION_MS,
            cursor,
          });

          expect(sweep.deleted).toBe(2);
          expect(yield* remainingAdmissions).toStrictEqual(
            new Set(
              [
                "accepted-pending",
                "accepted-processed",
                "rejected-with-evidence",
                "accepted-recent",
                "queued",
                "validating",
              ].map((label) => txIdOf(label).toString("hex")),
            ),
          );
          // The deleted rows' payloads went with them.
          const payloads = yield* sql<{ tx_id: Buffer }>`
          SELECT tx_id FROM tx_admission_payloads
          WHERE tx_id IN (${txIdOf("accepted-old")}, ${txIdOf("rejected-old")})`;
          expect(payloads).toEqual([]);
        }),
      ),
    );
  });

  it("walks past rows that must stay to reach deletable rows behind them", async () => {
    await Effect.runPromise(
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* freshDatabase("tx-admissions-retention-walk-test");
          const sql = yield* SqlClient.SqlClient;
          // Five pending rows from one old terminal mark sit at the head of
          // the walk; three deletable rows come after them.
          const held = Array.from({ length: 5 }, (_, i) => `held-${i}`);
          const deletable = Array.from({ length: 3 }, (_, i) => `old-${i}`);
          for (const label of held) {
            yield* admit(label);
            yield* markTerminal(label, "accepted", OLD_MARK);
            yield* sql`INSERT INTO mempool (tx_id, tx)
            VALUES (${txIdOf(label)}, ${Buffer.from("tx")})`;
          }
          for (const label of deletable) {
            yield* admit(label);
            yield* markTerminal(label, "accepted", LATER_OLD_MARK);
          }

          const cursor =
            yield* Ref.make<TxAdmissionsDB.TerminalAdmissionCursor | null>(
              null,
            );
          // Each sweep examines two rows; four sweeps cover all eight.
          let deleted = 0;
          for (let sweep = 0; sweep < 4; sweep += 1) {
            deleted += (yield* sweepTerminalAdmissions({
              retentionMs: RETENTION_MS,
              cursor,
              batchRows: 2,
              maxBatches: 1,
            })).deleted;
          }

          expect(deleted).toBe(3);
          expect(yield* remainingAdmissions).toStrictEqual(
            new Set(held.map((label) => txIdOf(label).toString("hex"))),
          );
        }),
      ),
    );
  });
});
