import { createRequire } from "node:module";
import { resolve } from "node:path";

import { worktreeIdentity } from "../lib/worktree-identity.mjs";
import { workspacePackages } from "./files.mjs";

// Disposable test databases and schemas on the shared test Postgres
// (127.0.0.1:5433) are named under a prefix. Every name contrib creates or
// drops carries the checkout's path hash (scripts/lib/worktree-identity.mjs),
// so one checkout can drop exactly its own and never another's: a bare
// family such as `midgard_test` would also match every worktree's
// `midgard_test_<hash>_w1`.
const FAMILIES = ["midgard_test", "midgard_tools_test", "midgard_contrib"];
const SCOPED =
  /^midgard_(?:test|tools_test|contrib)_[0-9a-f]{8}(?:_[a-z0-9]+)*$/u;

/** A fresh prefix for one contrib invocation, scoped to this checkout. */
export const invocationDatabasePrefix = (root, random) =>
  `midgard_contrib_${worktreeIdentity(root).hash}_${random}`;

/** Every prefix a linked worktree's suites and contrib runs use. */
export const checkoutDatabasePrefixes = (root) => {
  const { hash } = worktreeIdentity(root);
  return FAMILIES.map((family) => `${family}_${hash}`);
};

const owned = (prefixes) => (name) =>
  prefixes.some((prefix) => name === prefix || name.startsWith(`${prefix}_`));

// The workspace already depends on `pg`; borrow it from a package that
// declares it rather than giving the tooling its own copy.
const loadPg = (root) => {
  const owner = workspacePackages(root).find(
    (pkg) => pkg.dependencies?.pg || pkg.devDependencies?.pg,
  );
  if (!owner) throw new Error("no workspace package declares pg");
  return createRequire(resolve(root, owner.directory, "package.json"))("pg");
};

const adminClient = (root, env) => {
  if (
    (env.POSTGRES_HOST &&
      !["127.0.0.1", "localhost"].includes(env.POSTGRES_HOST)) ||
    (env.POSTGRES_PORT && env.POSTGRES_PORT !== "5433")
  )
    throw new Error(
      "test databases are dropped only on the local test Postgres (127.0.0.1:5433); unset POSTGRES_HOST/POSTGRES_PORT",
    );
  const pg = loadPg(root);
  return new (pg.default ?? pg).Client({
    host: "127.0.0.1",
    port: 5433,
    user: env.POSTGRES_USER ?? "postgres",
    password: env.POSTGRES_PASSWORD ?? "postgres",
    database: "postgres",
    connectionTimeoutMillis: 5_000,
  });
};

/**
 * Drop every database, and every schema in the `postgres` database (where the
 * watcher suites put theirs), named by one of `prefixes`. Returns what was
 * dropped, or `{ status: "unavailable" }` when no server is listening: then
 * nothing can have been created on it either.
 */
export const dropTestDatabases = async (
  root,
  prefixes,
  { env = process.env, client = adminClient(root, env) } = {},
) => {
  const unscoped = prefixes.filter((prefix) => !SCOPED.test(prefix));
  if (unscoped.length)
    throw new Error(
      `refusing to drop under a prefix without a checkout hash: ${unscoped.join(", ")}`,
    );
  try {
    await client.connect();
  } catch (error) {
    if (error.code === "ECONNREFUSED")
      return { status: "unavailable", detail: error.message };
    throw error;
  }
  try {
    const mine = owned(prefixes);
    const names = async (sql) =>
      (await client.query(sql)).rows.map((row) => row.name).filter(mine);
    const databases = await names("SELECT datname AS name FROM pg_database");
    const schemas = await names("SELECT nspname AS name FROM pg_namespace");
    for (const name of [...databases, ...schemas])
      if (!/^[a-z0-9_]+$/u.test(name))
        throw new Error(`unexpected test database name ${name}`);
    for (const name of databases)
      await client.query(`DROP DATABASE IF EXISTS "${name}" WITH (FORCE)`);
    for (const name of schemas)
      await client.query(`DROP SCHEMA IF EXISTS "${name}" CASCADE`);
    return { status: "dropped", databases, schemas };
  } finally {
    await client.end();
  }
};
