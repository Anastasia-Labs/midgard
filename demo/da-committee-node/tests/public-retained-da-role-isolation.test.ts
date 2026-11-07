import { randomBytes } from "node:crypto";

import {
  FOLLOWER_BOOKKEEPING_DDL,
  followerMigrations,
} from "@al-ft/midgard-l1-follower";
import { declaredTables } from "@al-ft/midgard-l1-follower/lint";
import { Client } from "pg";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import { committeeMigrations } from "../src/l1/follower/queue-table.js";
import { PostgresCommitteeStore } from "../src/store/postgres.js";
import {
  PostgresPublicRetainedDaStore,
  PUBLIC_RETAINED_DA_TABLES,
} from "../src/store/public-retained-da.js";
import {
  type PostgresTestDatabase,
  postgresTestDatabases,
} from "./helpers/postgres-database.js";

const databases = postgresTestDatabases("committee_public_reader");

/**
 * The committee's store and the L1 follower's tables share one database
 * (§18.1 Q1). The public retained-DA reader serves two of its tables to
 * anyone; its role must reach nothing else, the follower's tables least of
 * all.
 */
describe("public retained-DA reader role isolation", () => {
  const suffix = randomBytes(6).toString("hex");
  const readerRole = `${(process.env.MIDGARD_TEST_DATABASE_PREFIX ?? "committee_public_reader").toLowerCase().replace(/[^a-z0-9_]/gu, "_")}_reader_${suffix}`;
  const readerPassword = `pw_${suffix}`;
  let database: PostgresTestDatabase;
  let admin: Client;
  let readerUrl: string;

  beforeAll(async () => {
    database = await databases.create();
    // The committee node creates its tables on open.
    const store = await PostgresCommitteeStore.open(database.url);
    await store.close();
    admin = new Client({ connectionString: database.url });
    await admin.connect();
    // The follower's order: its bookkeeping tables, then the migrations
    // (a later follower migration alters a bookkeeping table).
    await admin.query(FOLLOWER_BOOKKEEPING_DDL);
    for (const set of [
      followerMigrations("postgres"),
      committeeMigrations("postgres"),
    ])
      for (const migration of set.migrations) await admin.query(migration.sql);
    await admin.query(
      `CREATE ROLE ${readerRole} LOGIN PASSWORD '${readerPassword}'`,
    );
    await admin.query(
      `GRANT CONNECT ON DATABASE ${database.name} TO ${readerRole}`,
    );
    await admin.query(`GRANT USAGE ON SCHEMA public TO ${readerRole}`);
    await admin.query(
      `GRANT SELECT ON ${PUBLIC_RETAINED_DA_TABLES.join(", ")} TO ${readerRole}`,
    );
    const url = new URL(database.url);
    url.username = readerRole;
    url.password = readerPassword;
    readerUrl = url.toString();
  }, 60_000);

  afterAll(async () => {
    await admin?.end();
    await databases.dropAll();
    const cluster = new Client({
      connectionString: database.url.replace(/\/[^/]+$/u, "/postgres"),
    });
    await cluster.connect();
    try {
      await cluster.query(`DROP ROLE IF EXISTS ${readerRole}`);
    } finally {
      await cluster.end();
    }
  }, 60_000);

  const openReader = () =>
    PostgresPublicRetainedDaStore.open({
      databaseUrl: readerUrl,
      expectedRole: readerRole,
    });

  it("opens on its two tables and cannot SELECT any follower or private committee table", async () => {
    const reader = await openReader();
    await reader.close();
    const tables = (
      await admin.query<{ readonly table_name: string }>(
        "SELECT table_name FROM information_schema.tables WHERE table_schema = 'public' ORDER BY table_name",
      )
    ).rows.map((row) => row.table_name);
    const followerTables = declaredTables([
      followerMigrations("postgres"),
      committeeMigrations("postgres"),
    ]).map((table) => table.table);
    expect(followerTables.length).toBeGreaterThan(0);
    expect(tables).toEqual(expect.arrayContaining(followerTables));
    expect(tables).toEqual(expect.arrayContaining(["committee_da_signatures"]));
    const client = new Client({ connectionString: readerUrl });
    await client.connect();
    try {
      for (const table of PUBLIC_RETAINED_DA_TABLES)
        await expect(
          client.query(`SELECT 1 FROM ${table} LIMIT 0`),
        ).resolves.toBeDefined();
      const reachable: string[] = [];
      for (const table of tables) {
        if ((PUBLIC_RETAINED_DA_TABLES as readonly string[]).includes(table))
          continue;
        const refused = await client
          .query(`SELECT 1 FROM ${table} LIMIT 0`)
          .then(
            () => undefined,
            (error: { readonly code?: string }) => error.code,
          );
        if (refused !== "42501") reachable.push(table);
      }
      expect(reachable).toEqual([]);
    } finally {
      await client.end();
    }
  }, 60_000);

  it("refuses to start once its role can read a follower table, and starts again when the grant is revoked", async () => {
    const [followerTable] = declaredTables([followerMigrations("postgres")]);
    expect(followerTable).toBeDefined();
    await admin.query(
      `GRANT SELECT ON ${followerTable!.table} TO ${readerRole}`,
    );
    try {
      await expect(openReader()).rejects.toThrow(
        /no privilege on any other table/u,
      );
    } finally {
      await admin.query(
        `REVOKE SELECT ON ${followerTable!.table} FROM ${readerRole}`,
      );
    }
    const reader = await openReader();
    await reader.close();
  }, 60_000);
});
