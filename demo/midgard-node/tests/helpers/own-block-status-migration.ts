/**
 * The fixtures of `own-block-status-migration.test.ts`: journals as the
 * release before migration 0037 stored them, and runs of the migration on
 * a populated schema at version 36, rolled back or committed.
 */
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { expect } from "vitest";

import { installFollowerSchema } from "../../src/database/follower-schema.js";
import {
  MIGRATION_MANIFEST_HASH,
  MIGRATIONS,
} from "../../src/database/migrations/index.js";
import * as MigrationRunner from "../../src/database/migrations/runner.js";
import { splitSqlStatements } from "../../src/database/migrations/runner.js";
import { ownBlockStatusSql } from "../../src/database/pendingBlockFinalizations.status-sql.js";
import { Frontier, insertRow } from "../../src/landed-blocks/store.js";
import { BatchSql } from "../../src/services/database.js";
import { landedRow } from "../landed-blocks-rebase.fixture.js";
import { testDatabaseName } from "../test-env.js";
import { provideDatabaseLayers } from "../utils.js";
import { simCommitTxHash, simJournalRow } from "./landed-blocks-sim.own.js";
import { insertCommitIntent } from "./own-block-facts.js";

const ZERO_ROOT = "00".repeat(32);
const ANCHOR = { hash: Buffer.alloc(32, 0xa1), height: 1, slot: 10 };
export const header = (n: number) => n.toString(16).padStart(2, "0").repeat(28);
const BASE = header(0xf0);
export const bytes = (headerHash: string) => Buffer.from(headerHash, "hex");
const signedBytes = (headerHash: string) =>
  Buffer.concat([Buffer.from("own-block-migration:"), bytes(headerHash)]);

export type Journal = Readonly<{
  status:
    | "pending_submission"
    | "submitted_local_finalization_pending"
    | "submitted_unconfirmed"
    | "observed_waiting_stability"
    | "locally_applied"
    | "abandoned";
  /** Signed under `simCommitTxHash`, with these bytes (by default its own). */
  signed?: boolean;
  /** Prepared under `simCommitTxHash` (a signed journal always is). */
  prepared?: boolean;
  /** The submitted hash: the signed one, or one with no intended hash. */
  submitted?: "signed" | "unsigned";
}>;

/** Journal `headerHash` as the release before this one stored it. */
export const insertJournal = (
  headerHash: string,
  journal: Journal,
  at: number,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const signed = journal.signed ?? false;
    const txHash = simCommitTxHash(bytes(headerHash));
    const row = simJournalRow(
      {
        headerHash,
        baseHeaderHash: BASE,
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
      prepared_tx_hash: signed || journal.prepared === true ? txHash : null,
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
export const insertIntent = (headerHash: string, other = false) =>
  insertCommitIntent(
    bytes(headerHash),
    simCommitTxHash(bytes(headerHash)),
    other ? Buffer.from("other-bytes") : signedBytes(headerHash),
  );

export const insertJob = (headerHash: string, status: "running" | "failed") =>
  Effect.flatMap(
    SqlClient.SqlClient,
    (sql) => sql`INSERT INTO local_mutation_jobs (job_id, kind, status)
      VALUES (${`local_block_finalization:${headerHash}`},
        'local_block_finalization', ${status})`,
  );

/**
 * Every journal's commit anchor on the follower's chain and its base the
 * landed frontier, so only its commit can make it dead.
 */
export const anchorAndBaseOnChain = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  yield* sql`INSERT INTO l1_blocks (slot, hash, height, parent_hash, qualifying_tx_count)
    VALUES (${ANCHOR.slot}, ${ANCHOR.hash}, ${ANCHOR.height}, NULL, 0)`;
  yield* Frontier.upsert({ headerHash: BASE, utxosRoot: ZERO_ROOT });
});

/** A landed row the working ledger took in; a removed row always is. */
export const land = (headerHash: string, state: "processed" | "removed") =>
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
 * or the journals' derived statuses and intended hashes and what is left of
 * the old shape.
 */
export const migrate = <E>(
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
                intended: string | null;
              }>`SELECT p.header_hash, ${ownBlockStatusSql(sql, "p")} AS status,
                  encode(p.intended_tx_hash, 'hex') AS intended
                FROM pending_block_finalizations p ORDER BY p.header_hash`;
              const [shape] = yield* sql<{
                columns: number;
                jobs: boolean;
              }>`SELECT
                  (SELECT COUNT(*)::int FROM information_schema.columns
                    WHERE table_schema = 'public'
                      AND table_name = 'pending_block_finalizations'
                      AND column_name IN ('status', 'submitted_tx_hash',
                        'signed_tx_cbor', 'correction_transition_digest'))
                    AS columns,
                  to_regclass('local_mutation_jobs') IS NOT NULL AS jobs`;
              return yield* Effect.fail(
                new RolledBack({
                  statuses: Object.fromEntries(
                    statuses.map((row) => [
                      row.header_hash.toString("hex"),
                      row.status,
                    ]),
                  ),
                  intended: Object.fromEntries(
                    statuses.map((row) => [
                      row.header_hash.toString("hex"),
                      row.intended,
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
  intended?: Readonly<Record<string, string | null>>;
  shape?: Readonly<{ columns: number; jobs: boolean }>;
}>;

/**
 * Runs `populate` on a database a node with a follower left at version 36,
 * then the migration runner and `after`, all committed (a prune's follower
 * write gate must own its transaction), as the runner's own tests do.
 */
export const migrateCommitted = <E, A>(
  populate: Effect.Effect<void, E, SqlClient.SqlClient>,
  after: Effect.Effect<A, unknown, SqlClient.SqlClient>,
) =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        const sql = yield* BatchSql;
        const [database] = yield* sql<{
          name: string;
        }>`SELECT current_database() AS name`;
        expect(database!.name).toBe(testDatabaseName());
        const withSql = Effect.provideService(SqlClient.SqlClient, sql);
        yield* sql.unsafe("DROP SCHEMA public CASCADE; CREATE SCHEMA public");
        yield* withSql(MigrationRunner.getStatus);
        for (const migration of MIGRATIONS.filter(
          (entry) => entry.version <= 36,
        )) {
          for (const statement of splitSqlStatements(migration.sql))
            yield* sql.unsafe(statement);
          yield* sql`INSERT INTO schema_migrations
            (version, name, checksum_sha256, manifest_hash_sha256, app_version, execution_ms, applied_by)
            VALUES (${migration.version}, ${migration.name}, ${migration.checksumSha256},
              ${MIGRATION_MANIFEST_HASH}, 'own-block-predecessor', 1, 'own-block-test')`;
        }
        yield* withSql(installFollowerSchema);
        yield* withSql(populate);
        yield* withSql(
          MigrationRunner.migrate({
            appVersion: "own-block-test",
            actor: "own-block-upgrade",
          }),
        );
        return yield* after;
      }),
    ),
  );
