import { createHash } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { type Address, Data as LucidData } from "@lucid-evolution/lucid";
import { Effect, Layer } from "effect";
import { expect } from "vitest";

import { MIGRATIONS } from "../src/database/migrations/index.js";
import * as TxAdmissionsDB from "../src/database/txAdmissions.js";
import * as LedgerUtils from "../src/database/utils/ledger.js";
import {
  ADMISSION_WRITE_BATCH_MAX_ROWS,
  ADMISSION_WRITE_BATCH_TARGET_ROWS,
  ADMISSION_WRITE_QUEUE_CAPACITY,
  ADMISSION_WRITE_SHARD_COUNT,
  AdmissionWriter,
  AdmissionWriterLive,
  makeAdmissionWriterWithOptions,
} from "../src/services/admission-writer.js";
import { NodeConfig } from "../src/services/config.js";
import { AdmissionSql, Database } from "../src/services/database.js";
import { FollowerWriteFixture } from "../src/services/follower-write-gate.js";
import { WriteBehindLive } from "../src/services/write-behind.js";
import { applyMidgardNodeTestEnv, testDatabaseName } from "./test-env.js";

// Importing this module is what pins a test file to its worker's database
// shard; see tests/test-env.ts for why the shard is assigned rather than read
// from an ambient POSTGRES_DB.
applyMidgardNodeTestEnv();

const AdmissionWriterTestLive = Layer.scoped(
  AdmissionWriter,
  Effect.gen(function* () {
    const admissionSql = yield* AdmissionSql;
    return yield* makeAdmissionWriterWithOptions(
      (requests) =>
        TxAdmissionsDB.admitReservedBatch(requests).pipe(
          Effect.provideService(SqlClient.SqlClient, admissionSql),
        ),
      {
        shardCount: ADMISSION_WRITE_SHARD_COUNT,
        batchMaxRows: ADMISSION_WRITE_BATCH_MAX_ROWS,
        batchTargetRows: ADMISSION_WRITE_BATCH_TARGET_ROWS,
        batchDeadlineMs: 0,
        queueCapacity: ADMISSION_WRITE_QUEUE_CAPACITY,
      },
    );
  }),
);

const admissionWriterLayer =
  process.env.PHASE1_ADMISSION_OPERATOR === "1"
    ? AdmissionWriterLive
    : AdmissionWriterTestLive;

export const provideDatabaseLayers = <A, E, R>(eff: Effect.Effect<A, E, R>) =>
  eff.pipe(
    Effect.provideService(FollowerWriteFixture, true),
    Effect.provide(WriteBehindLive),
    Effect.provide(admissionWriterLayer),
    Effect.provide(Database.layer),
    Effect.provide(NodeConfig.layer),
  );

/**
 * A migration's INSERT statements are the rows a migrated database must always
 * contain (e.g. the `commit_build_calibration` singleton). A reset replays only
 * these, wherever they sit in the file, without re-running the migration's DDL.
 * A seed row must therefore be a plain `INSERT INTO ...;` statement: rows
 * seeded by `COPY` or inside a `DO` block would not be restored. Dollar-quoted
 * bodies are skipped, so an INSERT inside a trigger or function body (which
 * runs when the function does, not at migration time) is never replayed. A
 * top-level `INSERT ... SELECT` is a one-time carry-over of an existing
 * database's rows, not a seed row: against the emptied tables it adds nothing,
 * and the table it reads may since have been dropped by a later migration, so
 * it is never replayed.
 */
const MIGRATION_INSERT_STATEMENT = /^\s*INSERT\s+INTO\b[^;]*;/gim;
const DOLLAR_QUOTED_BODY = /\$([A-Za-z_][A-Za-z0-9_]*)?\$[\s\S]*?\$\1\$/g;
const CARRY_OVER = /\bSELECT\b/iu;

const migrationSeedRowsSql: readonly string[] = MIGRATIONS.flatMap(
  (migration) =>
    (
      migration.sql
        .replace(DOLLAR_QUOTED_BODY, "")
        .match(MIGRATION_INSERT_STATEMENT) ?? []
    ).filter((statement) => !CARRY_OVER.test(statement)),
);

/**
 * The tables a reset keeps, each with why. Every other table in the test
 * database's schemas is emptied, so a table a new migration (node or
 * follower) adds is reset without editing this list.
 */
export const RESET_KEPT_TABLES: ReadonlyMap<string, string> = new Map([
  [
    "schema_migrations",
    "the node's migration ledger: the runner checks it against MIGRATIONS on every migrate, and the schema it records is still in place",
  ],
  [
    "schema_migration_events",
    "the node's migration audit trail, written beside the ledger",
  ],
  [
    "l1_follower_migrations",
    "the follower store's migration ledger, the follower's schema_migrations",
  ],
  [
    "l1_follower_tables",
    "the follower's catalog of the tables its migrations declared; the follower's own reset reads it",
  ],
  [
    "l1_follower_writer",
    "the follower's writer-fence singleton (epoch, next generation), written by its migration; a store without the row refuses to open, and generations only ever rise",
  ],
]);

/**
 * Returns the test database to its freshly migrated contents: every table in
 * its schemas except `RESET_KEPT_TABLES` is emptied in one TRUNCATE
 * (identities restarted), and the migrations' seed rows (the
 * `commit_build_calibration` singleton) are restored, in one transaction.
 * The tables are read from the catalog, not listed, so no table can be left
 * holding another file's chain: the fork pool runs every file of a worker
 * on that worker's database.
 */
export const resetApplicationTables = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const database = yield* sql<{
    name: string;
  }>`SELECT current_database() AS name`;
  if (database[0]?.name !== testDatabaseName()) {
    return yield* Effect.dieMessage(
      "Refusing application reset outside this invocation's disposable test shard",
    );
  }
  yield* sql.withTransaction(
    Effect.gen(function* () {
      const tables = yield* sql<{
        schema: string;
        name: string;
      }>`SELECT schemaname AS schema, tablename AS name FROM pg_tables
        WHERE schemaname <> 'information_schema' AND left(schemaname, 3) <> 'pg_'
        ORDER BY schemaname, tablename`;
      const emptied = tables.filter(({ name }) => !RESET_KEPT_TABLES.has(name));
      if (emptied.length > 0)
        yield* sql.unsafe(
          `TRUNCATE TABLE ${emptied
            .map(({ schema, name }) => `"${schema}"."${name}"`)
            .join(", ")} RESTART IDENTITY CASCADE`,
        );
      for (const seedRowsSql of migrationSeedRowsSql) {
        yield* sql.unsafe(seedRowsSql);
      }
    }),
  );
});

export const deterministicFixtureBytes = (
  label: string,
  length: number,
): Buffer => {
  const chunks: Buffer[] = [];
  let bytesGenerated = 0;
  for (let counter = 0; bytesGenerated < length; counter += 1) {
    const chunk = createHash("sha256")
      .update("midgard-node-test-fixture")
      .update("\0")
      .update(label)
      .update("\0")
      .update(counter.toString())
      .digest();
    chunks.push(chunk);
    bytesGenerated += chunk.length;
  }
  return Buffer.concat(chunks).subarray(0, length);
};

export const deterministicFixtureTxHash = (label: string): Buffer =>
  deterministicFixtureBytes(`tx-hash:${label}`, 32);

export const deterministicFixtureOutputReference = (
  label: string,
  outputIndex: number | bigint = 0n,
): SDK.OutputReference => ({
  transactionId: deterministicFixtureTxHash(
    `output-reference:${label}`,
  ).toString("hex"),
  outputIndex: BigInt(outputIndex),
});

export const deterministicFixtureOutputReferenceId = (
  label: string,
  outputIndex: number | bigint = 0n,
): Buffer =>
  Buffer.from(
    LucidData.to(
      deterministicFixtureOutputReference(label, outputIndex),
      SDK.OutputReference,
    ),
    "hex",
  );

type LedgerLikeEntry = {
  readonly [LedgerUtils.Columns.TX_ID]: Buffer;
  readonly [LedgerUtils.Columns.OUTREF]: Buffer;
  readonly [LedgerUtils.Columns.OUTPUT]: Buffer;
  readonly [LedgerUtils.Columns.ADDRESS]: Address;
};

const withoutLedgerTimestamp = (
  entry: LedgerLikeEntry,
): LedgerUtils.EntryNoTimeStamp => ({
  [LedgerUtils.Columns.TX_ID]: entry[LedgerUtils.Columns.TX_ID],
  [LedgerUtils.Columns.OUTREF]: entry[LedgerUtils.Columns.OUTREF],
  [LedgerUtils.Columns.OUTPUT]: entry[LedgerUtils.Columns.OUTPUT],
  [LedgerUtils.Columns.ADDRESS]: entry[LedgerUtils.Columns.ADDRESS],
});

const sortLedgerEntries = (
  entries: readonly LedgerUtils.EntryNoTimeStamp[],
): LedgerUtils.EntryNoTimeStamp[] =>
  [...entries].sort((left, right) =>
    Buffer.compare(
      left[LedgerUtils.Columns.OUTREF],
      right[LedgerUtils.Columns.OUTREF],
    ),
  );

export const expectLedgerUtxos = (
  actual: readonly LedgerLikeEntry[],
  expected: readonly LedgerLikeEntry[],
): void => {
  expect(
    sortLedgerEntries(actual.map((entry) => withoutLedgerTimestamp(entry))),
  ).toStrictEqual(
    sortLedgerEntries(expected.map((entry) => withoutLedgerTimestamp(entry))),
  );
};
