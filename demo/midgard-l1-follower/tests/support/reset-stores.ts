import { join } from "node:path";

import {
  type DialectName,
  type FactStore,
  type FactStoreOptions,
  type MigrationSet,
  openPostgresBackend,
  openPostgresFactStore,
  openSqliteBackend,
  openSqliteFactStore,
  type SqlBackend,
} from "../../src/index.js";
import {
  FIXTURE_DERIVATION,
  FIXTURE_TABLES,
  fixtureMigrations,
} from "./fixture.js";
import type { testDatabases } from "./postgres.js";
import { options } from "./small-chain.js";

/**
 * A role's own tables beside the follower's: a class B table (signed
 * material) and a class C table (content); reset keeps both. The fixture D-t tables come with the fixture derivation.
 */
const roleSql = (dialect: DialectName): string => {
  const bytes = dialect === "postgres" ? "bytea" : "BLOB";
  return `
-- class: B; retention: forever (test)
CREATE TABLE role_signed (
  id integer PRIMARY KEY,
  body ${bytes} NOT NULL
);

-- class: C; retention: while referenced (test)
CREATE TABLE role_content (
  content_hash ${bytes} PRIMARY KEY,
  bytes ${bytes} NOT NULL
);
`;
};

export const roleMigrations = (dialect: DialectName): MigrationSet => ({
  namespace: "role",
  migrations: [{ id: "0001_role_tables", sql: roleSql(dialect) }],
});

export const roleOptions = (dialect: DialectName): FactStoreOptions => ({
  ...options(2),
  temporalTables: FIXTURE_TABLES,
  migrations: [fixtureMigrations(dialect), roleMigrations(dialect)],
  derivations: [FIXTURE_DERIVATION],
});

/** One database location, and how to open stores, backends and the CLI on it. */
export type Location = Readonly<{
  store: () => FactStore;
  backend: () => SqlBackend;
  cliArgs: readonly string[];
  /** Postgres only: the database URL (for killing the lease session). */
  url?: string;
}>;

export type ResetAdapter = Readonly<{
  name: DialectName;
  create: () => Promise<Location>;
}>;

export const resetAdapters = (
  databases: ReturnType<typeof testDatabases>,
  scratch: string,
): readonly ResetAdapter[] => [
  {
    name: "sqlite",
    create: async () => {
      const path = join(scratch, `${String(Math.random()).slice(2)}.db`);
      return {
        store: () => openSqliteFactStore({ ...roleOptions("sqlite"), path }),
        backend: () => openSqliteBackend(path),
        cliArgs: ["--sqlite", path],
      };
    },
  },
  {
    name: "postgres",
    create: async () => {
      const { url } = await databases.create();
      return {
        store: () =>
          openPostgresFactStore({
            ...roleOptions("postgres"),
            connection: { connectionString: url },
          }),
        backend: () => openPostgresBackend({ connectionString: url }),
        cliArgs: ["--postgres", url],
        url,
      };
    },
  },
];
