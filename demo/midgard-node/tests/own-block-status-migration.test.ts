/**
 * Migration 0037 on a populated node database at version 36, with and
 * without the follower's tables (installed after the node migrations): each
 * run rebuilds the schema through 36 in a transaction it rolls back, so the
 * shard keeps its schema.
 *
 * - It carries over a locally applied journal a processed landed row
 *   applied, or a rollback removed, or the state queue merged; an abandoned
 *   signed journal with no intent row; an active signed journal the intent
 *   journal holds with its exact bytes; and an unfinished local
 *   finalization of a submitted journal. It deletes an abandoned journal
 *   never signed, and drops the status, its columns and the job table.
 * - It refuses, leaving every row as it was, with
 *   `MIGRATION_0037_SIGNED_JOURNAL_WITHOUT_INTENT` a submitted hash with no
 *   intended one, an active signed journal the intent journal holds with
 *   other bytes, and one with no follower tables to hold it; with
 *   `MIGRATION_0037_UNRECOVERABLE_LOCAL_FINALIZATION` an unfinished job of a
 *   journal never submitted; with `MIGRATION_0037_APPLIED_JOURNAL_UNDERIVABLE`
 *   a locally applied journal nothing applied or folded.
 */
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { installFollowerSchema } from "../src/database/follower-schema.js";
import { MIGRATIONS } from "../src/database/migrations/index.js";
import { splitSqlStatements } from "../src/database/migrations/runner.js";
import { ownBlockStatusSql } from "../src/database/pendingBlockFinalizations.derived-status.js";
import { insertRow } from "../src/landed-blocks/store.js";
import {
  simCommitTxHash,
  simJournalRow,
} from "./helpers/landed-blocks-sim.own.js";
import { insertCommitIntent } from "./helpers/own-block-facts.js";
import { insertQueueTerminal } from "./helpers/queue-terminal-rows.js";
import { landedRow } from "./landed-blocks-rebase.fixture.js";
import { provideDatabaseLayers } from "./utils.js";

const ZERO_ROOT = "00".repeat(32);
const ANCHOR = { hash: Buffer.alloc(32, 0xa1), height: 1, slot: 10 };
const header = (n: number) => n.toString(16).padStart(2, "0").repeat(28);
const bytes = (headerHash: string) => Buffer.from(headerHash, "hex");
const signedBytes = (headerHash: string) =>
  Buffer.concat([Buffer.from("own-block-migration:"), bytes(headerHash)]);

type Journal = Readonly<{
  status:
    | "pending_submission"
    | "submitted_local_finalization_pending"
    | "submitted_unconfirmed"
    | "observed_waiting_stability"
    | "locally_applied"
    | "abandoned";
  /** Signed under `simCommitTxHash`, with these bytes (by default its own). */
  signed?: boolean;
  /** The submitted hash: the signed one, or one with no intended hash. */
  submitted?: "signed" | "unsigned";
}>;

/** Journal `headerHash` as the release before this one stored it. */
const insertJournal = (headerHash: string, journal: Journal, at: number) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const signed = journal.signed ?? false;
    const txHash = simCommitTxHash(bytes(headerHash));
    const row = simJournalRow(
      {
        headerHash,
        baseHeaderHash: header(0xf0),
        baseUtxosRoot: ZERO_ROOT,
        expectedUtxosRoot: ZERO_ROOT,
        spent: [],
        produced: [],
        txIds: [],
        at: new Date(Date.parse("2026-10-01T00:00:00.000Z") + at * 60_000),
        signed,
      },
      ANCHOR,
    );
    yield* sql`INSERT INTO pending_block_finalizations ${sql.insert({
      ...row,
      status: journal.status,
      signed_tx_cbor: signed ? signedBytes(headerHash) : null,
      submitted_tx_hash:
        journal.submitted === "signed"
          ? txHash
          : journal.submitted === "unsigned"
            ? Buffer.alloc(32, at)
            : null,
    } as never)}`;
  });

/** The commit's intent row, with its journal's bytes or `other` ones. */
const insertIntent = (headerHash: string, other = false) =>
  insertCommitIntent(
    bytes(headerHash),
    simCommitTxHash(bytes(headerHash)),
    other ? Buffer.from("other-bytes") : signedBytes(headerHash),
  );

const insertJob = (headerHash: string, status: "running" | "failed") =>
  Effect.flatMap(
    SqlClient.SqlClient,
    (sql) => sql`INSERT INTO local_mutation_jobs (job_id, kind, status)
      VALUES (${`local_block_finalization:${headerHash}`},
        'local_block_finalization', ${status})`,
  );

/** A landed row the working ledger took in; a removed row always is. */
const land = (headerHash: string, state: "processed" | "removed") =>
  insertRow(landedRow({ headerHash, kind: "own", state, applied: true }));

/** The named refusal `error` carries, in its message or a cause's. */
const refusal = (error: unknown): string | undefined => {
  for (
    let cause: unknown = error;
    cause instanceof Object;
    cause = (cause as { cause?: unknown }).cause
  ) {
    const code = String((cause as { message?: unknown }).message).match(
      /MIGRATION_0037_[A-Z_]+/u,
    )?.[0];
    if (code !== undefined) return code;
  }
  return undefined;
};

/** The rows a refusal must leave as they were. */
const journals = Effect.flatMap(
  SqlClient.SqlClient,
  (sql) => sql`SELECT * FROM pending_block_finalizations ORDER BY header_hash`,
);

const migration0037 = MIGRATIONS.find((entry) => entry.version === 37)!;

/** The work rolled back, carrying what ran under it. */
class RolledBack<A> {
  constructor(readonly value: A) {}
}

/**
 * Runs `populate` on the schema through version 36 (with the follower's
 * tables if `follower`), then migration 0037 in a savepoint: its refusal,
 * or the journals' derived statuses and what is left of the old shape.
 */
const migrate = <E>(
  follower: boolean,
  populate: Effect.Effect<void, E, SqlClient.SqlClient>,
) =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        return yield* sql
          .withTransaction(
            Effect.gen(function* () {
              yield* sql.unsafe("DROP SCHEMA public CASCADE");
              yield* sql.unsafe("CREATE SCHEMA public");
              for (const migration of MIGRATIONS.filter(
                (entry) => entry.version <= 36,
              ))
                for (const statement of splitSqlStatements(migration.sql))
                  yield* sql.unsafe(statement);
              if (follower) yield* installFollowerSchema;
              yield* populate;
              const before = yield* journals;
              const ran = yield* Effect.either(
                sql.withTransaction(
                  Effect.forEach(
                    splitSqlStatements(migration0037.sql),
                    (statement) => sql.unsafe(statement),
                  ),
                ),
              );
              if (ran._tag === "Left") {
                expect(yield* journals).toEqual(before);
                return yield* Effect.fail(
                  new RolledBack({ refused: refusal(ran.left) }),
                );
              }
              // The follower's tables, as a node installs them after the
              // node migrations.
              if (!follower) yield* installFollowerSchema;
              const statuses = yield* sql<{
                header_hash: Buffer;
                status: string;
              }>`SELECT p.header_hash, ${ownBlockStatusSql(sql, "p")} AS status
                FROM pending_block_finalizations p ORDER BY p.header_hash`;
              const [shape] = yield* sql<{
                columns: number;
                jobs: boolean;
              }>`SELECT
                  (SELECT COUNT(*)::int FROM information_schema.columns
                    WHERE table_schema = 'public'
                      AND table_name = 'pending_block_finalizations'
                      AND column_name IN ('status', 'submitted_tx_hash',
                        'signed_tx_cbor', 'observed_confirmed_at_ms',
                        'correction_transition_digest')) AS columns,
                  to_regclass('local_mutation_jobs') IS NOT NULL AS jobs`;
              return yield* Effect.fail(
                new RolledBack({
                  statuses: Object.fromEntries(
                    statuses.map((row) => [
                      row.header_hash.toString("hex"),
                      row.status,
                    ]),
                  ),
                  shape,
                }),
              );
            }),
          )
          .pipe(
            Effect.catchIf(
              (error): error is RolledBack<Shown> =>
                error instanceof RolledBack,
              (rolledBack) => Effect.succeed(rolledBack.value),
            ),
          );
      }),
    ),
  );

type Shown = Readonly<{
  refused?: string;
  statuses?: Readonly<Record<string, string>>;
  shape?: Readonly<{ columns: number; jobs: boolean }>;
}>;

const APPLIED = header(0x01);
const REMOVED = header(0x02);
const MERGED = header(0x03);
const DEAD_SIGNED = header(0x04);
const NEVER_SIGNED = header(0x05);
const SUBMITTED = header(0x06);
const FINALIZING = header(0x07);

describe("migration 0037 on a populated node database", () => {
  it.each([false, true])(
    "carries over the journals it can derive, with the follower's tables %s",
    async (follower) => {
      const shown = await migrate(
        follower,
        Effect.gen(function* () {
          yield* insertJournal(APPLIED, { status: "locally_applied" }, 1);
          yield* land(APPLIED, "processed");
          yield* insertJournal(REMOVED, { status: "locally_applied" }, 2);
          yield* land(REMOVED, "removed");
          yield* insertJournal(
            DEAD_SIGNED,
            { status: "abandoned", signed: true },
            4,
          );
          yield* insertJournal(NEVER_SIGNED, { status: "abandoned" }, 5);
          yield* insertJournal(
            FINALIZING,
            { status: "submitted_local_finalization_pending" },
            7,
          );
          yield* insertJob(FINALIZING, "running");
          if (!follower) return;
          yield* insertJournal(MERGED, { status: "locally_applied" }, 3);
          yield* insertQueueTerminal({
            headerHash: bytes(MERGED),
            outcome: "merged",
            height: 5,
          });
        }),
      );
      expect(shown).toEqual({
        statuses: {
          [APPLIED]: "applied",
          [REMOVED]: "unlanded",
          [DEAD_SIGNED]: "unlanded",
          [FINALIZING]: "unlanded",
          ...(follower ? { [MERGED]: "folded" } : {}),
        },
        shape: { columns: 0, jobs: false },
      });
    },
  );

  // One active journal at a time before 0037: the carry-over above holds
  // FINALIZING, so this one stands alone.
  it("carries over an active signed journal the intent journal holds with its exact bytes", async () => {
    expect(
      await migrate(
        true,
        Effect.gen(function* () {
          yield* insertJournal(
            SUBMITTED,
            {
              status: "submitted_unconfirmed",
              signed: true,
              submitted: "signed",
            },
            6,
          );
          yield* insertIntent(SUBMITTED);
        }),
      ),
    ).toEqual({
      statuses: { [SUBMITTED]: "unlanded" },
      shape: { columns: 0, jobs: false },
    });
  });

  it.each([false, true])(
    "refuses a submitted hash with no intended one, with the follower's tables %s",
    async (follower) => {
      expect(
        await migrate(
          follower,
          insertJournal(
            SUBMITTED,
            { status: "submitted_unconfirmed", submitted: "unsigned" },
            6,
          ),
        ),
      ).toEqual({ refused: "MIGRATION_0037_SIGNED_JOURNAL_WITHOUT_INTENT" });
    },
  );

  it("refuses an active signed journal the intent journal holds with other bytes", async () => {
    expect(
      await migrate(
        true,
        Effect.gen(function* () {
          yield* insertJournal(
            SUBMITTED,
            { status: "observed_waiting_stability", signed: true },
            6,
          );
          yield* insertIntent(SUBMITTED, true);
        }),
      ),
    ).toEqual({ refused: "MIGRATION_0037_SIGNED_JOURNAL_WITHOUT_INTENT" });
  });

  it("refuses an active signed journal with no follower tables to hold it", async () => {
    expect(
      await migrate(
        false,
        insertJournal(
          SUBMITTED,
          { status: "pending_submission", signed: true },
          6,
        ),
      ),
    ).toEqual({ refused: "MIGRATION_0037_SIGNED_JOURNAL_WITHOUT_INTENT" });
  });

  it.each([false, true])(
    "refuses an unfinished local finalization of a journal never submitted, with the follower's tables %s",
    async (follower) => {
      expect(
        await migrate(
          follower,
          Effect.gen(function* () {
            yield* insertJournal(
              FINALIZING,
              { status: "pending_submission" },
              7,
            );
            yield* insertJob(FINALIZING, "failed");
          }),
        ),
      ).toEqual({ refused: "MIGRATION_0037_UNRECOVERABLE_LOCAL_FINALIZATION" });
    },
  );

  it.each([false, true])(
    "refuses a locally applied journal nothing applied or folded, with the follower's tables %s",
    async (follower) => {
      expect(
        await migrate(
          follower,
          insertJournal(APPLIED, { status: "locally_applied" }, 1),
        ),
      ).toEqual({ refused: "MIGRATION_0037_APPLIED_JOURNAL_UNDERIVABLE" });
    },
  );
});
