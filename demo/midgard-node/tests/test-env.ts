/**
 * Test environment defaults and the per-worker Postgres shard.
 *
 * This module has no vitest and no `@/` imports on purpose: `vitest.config.ts`
 * and `tests/global-setup.ts` both need the shard vocabulary before any test
 * worker exists, and `tests/utils.ts` needs the env defaults inside one.
 */

/**
 * How many test files may run at once. Overridable with
 * `MIDGARD_NODE_TEST_FORKS`; anything that is not a positive integer falls back
 * to the default.
 */
const DEFAULT_MAX_FORKS = 4;

export const parsePositiveInteger = (
  raw: string | undefined,
): number | undefined => {
  if (raw === undefined) {
    return undefined;
  }
  const parsed = Number(raw.trim());
  if (!Number.isInteger(parsed) || parsed < 1) {
    return undefined;
  }
  return parsed;
};

export const testMaxForks = (): number =>
  parsePositiveInteger(process.env.MIDGARD_NODE_TEST_FORKS) ??
  DEFAULT_MAX_FORKS;

/**
 * The suite never shares one database between concurrently running files.
 *
 * Every database-touching file gets the database of the worker it happens to
 * land on, and a fork pool worker only ever runs one file at a time, so two
 * files can never clear each other's tables mid-assertion. `VITEST_POOL_ID` is
 * the worker slot (1-based); `_w0` is the fallback for anything run outside a
 * pooled worker.
 */
export const TEST_DATABASE_PREFIX = "midgard_test";

export const testDatabaseNameForShard = (shard: string | number): string =>
  `${TEST_DATABASE_PREFIX}_w${shard}`;

export const testDatabaseName = (): string =>
  testDatabaseNameForShard(process.env.VITEST_POOL_ID ?? "0");

/** Every shard a run with the current fork bound can reach. */
export const testDatabaseNames = (): readonly string[] => {
  const names = ["0"];
  for (let shard = 1; shard <= testMaxForks(); shard += 1) {
    names.push(String(shard));
  }
  return names.map(testDatabaseNameForShard);
};

const TEST_ENV_DEFAULTS: Record<string, string> = {
  L1_PROVIDER: "Kupmios",
  L1_OGMIOS_KEY: "http://127.0.0.1:1337",
  L1_KUPO_KEY: "http://127.0.0.1:1442",
  L1_OPERATOR_SEED_PHRASE:
    "panther fly crawl express smile lend company blue slogan dawn wall tip angle tomorrow battle myth category vanish misery ocean include salon wood rail",
  L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX:
    "second salad helmet humble left noise inform person swamp surround twice animal fitness sing laundry saddle stove guess cabin rural kidney reject oil fee",
  L1_REFERENCE_SCRIPT_SEED_PHRASE:
    "cactus chalk grit reopen true slight whale sand law sibling silver fringe cement twist process bracket history leopard churn federal coral three hockey fossil",
  L1_REFERENCE_SCRIPT_ADDRESS:
    "addr_test1qpdjresrrk294hy9ndtqly955ldlhy688507shkfxpwtgf39vzk9uwp87k96zkd5yal83h9x0qheeu0lrqp9lldvsqjs5s4ggd",
  NETWORK: "Preprod",
  POSTGRES_HOST: "127.0.0.1",
  POSTGRES_PORT: "5433",
  POSTGRES_USER: "postgres",
  POSTGRES_PASSWORD: "postgres",
  TESTNET_GENESIS_WALLET_SEED_PHRASE_A:
    "panther fly crawl express smile lend company blue slogan dawn wall tip angle tomorrow battle myth category vanish misery ocean include salon wood rail",
  TESTNET_GENESIS_WALLET_SEED_PHRASE_B:
    "second salad helmet humble left noise inform person swamp surround twice animal fitness sing laundry saddle stove guess cabin rural kidney reject oil fee",
  TESTNET_GENESIS_WALLET_SEED_PHRASE_C:
    "cactus chalk grit reopen true slight whale sand law sibling silver fringe cement twist process bracket history leopard churn federal coral three hockey fossil",
};

/**
 * Apply the suite's env defaults and pin this process to its database shard.
 *
 * The shard name is assigned, not defaulted: an ambient `POSTGRES_DB` names the
 * node's own database (CI sets `POSTGRES_DB=midgard` job-wide for the
 * `db:migrate` and acceptance steps that surround the suite), and honouring it
 * here would put every worker of a parallel run on one database. The refusal
 * below still fires on that ambient value, so an operator who deliberately
 * points the suite at the protected `midgard` database gets the same loud
 * error it has always produced rather than a silent redirect.
 */
export const applyMidgardNodeTestEnv = (): void => {
  const explicitPostgresDb =
    process.env.POSTGRES_DB !== undefined && process.env.POSTGRES_DB !== "";

  for (const [key, value] of Object.entries(TEST_ENV_DEFAULTS)) {
    if (process.env[key] === undefined || process.env[key] === "") {
      process.env[key] = value;
    }
  }

  if (
    process.env.POSTGRES_DB === "midgard" &&
    process.env.MIDGARD_ALLOW_TEST_DATABASE_MIDGARD !== "1" &&
    process.env.CI !== "true"
  ) {
    throw new Error(
      explicitPostgresDb
        ? "Refusing to run Midgard tests against POSTGRES_DB=midgard. Use an isolated test database, or set MIDGARD_ALLOW_TEST_DATABASE_MIDGARD=1 only for an intentionally disposable local environment."
        : "Refusing to default Midgard tests to POSTGRES_DB=midgard. Use an isolated test database.",
    );
  }

  process.env.POSTGRES_DB = testDatabaseName();
};
