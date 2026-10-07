/**
 * Follower stores carrying the node event projection, on SQLite (memory)
 * or a fresh Postgres database of the workspace test cluster, and a driver
 * that feeds them a simulated chain.
 */
import { randomBytes } from "node:crypto";

import {
  applyChainSyncEvent,
  type DialectName,
  type FactStore,
  openPostgresFactStore,
  openSqliteFactStore,
  type TrackedSet,
} from "@al-ft/midgard-l1-follower";
import {
  type FollowerProjection,
  projectionStoreOptions,
} from "@al-ft/midgard-l1-follower/shadow";
import {
  SIM_ORIGIN,
  SimChain,
  type SimTx,
  simUniverse,
} from "@al-ft/midgard-l1-follower/testing";
import { SqlClient } from "@effect/sql";
import { PgClient } from "@effect/sql-pg";
import { Effect, Redacted } from "effect";

import { TEST_DATABASE_PREFIX } from "../test-env.js";

const admin = {
  host: process.env.POSTGRES_HOST ?? "127.0.0.1",
  port: Number(process.env.POSTGRES_PORT ?? "5433"),
  user: process.env.POSTGRES_USER ?? "postgres",
  password: process.env.POSTGRES_PASSWORD ?? "postgres",
};

const adminSql = (statement: string): Promise<void> =>
  Effect.runPromise(
    Effect.provide(
      Effect.flatMap(SqlClient.SqlClient, (sql) => sql.unsafe(statement)),
      PgClient.layer({
        host: admin.host,
        port: admin.port,
        username: admin.user,
        password: Redacted.make(admin.password),
        database: "postgres",
        maxConnections: 1,
      }),
    ),
  ).then(() => undefined);

/** Fresh databases named under the worktree's test prefix; fails closed when the cluster is down. */
export const testDatabases = () => {
  const created: string[] = [];
  const prefix = TEST_DATABASE_PREFIX.toLowerCase().replace(
    /[^a-z0-9_]/gu,
    "_",
  );
  return {
    create: async (): Promise<string> => {
      const name = `${prefix}_ev_${randomBytes(5).toString("hex")}`;
      await adminSql(`CREATE DATABASE ${name}`);
      created.push(name);
      return `postgresql://${admin.user}:${admin.password}@${admin.host}:${String(admin.port)}/${name}`;
    },
    dropAll: async (): Promise<void> => {
      for (const name of created.splice(0))
        await adminSql(`DROP DATABASE IF EXISTS ${name} WITH (FORCE)`);
    },
  };
};

const EMPTY: TrackedSet = {
  addresses: new Set(),
  paymentCredentials: new Set(),
  policies: new Set(),
};

export type StoreOpener = (
  projections: readonly FollowerProjection[],
  k: number,
) => Promise<FactStore>;

/** Opens (and starts) a store of `dialect` with `projections` plugged in. */
export const storeOpener = (
  dialect: DialectName,
  databases: ReturnType<typeof testDatabases>,
): StoreOpener => {
  return async (projections, k) => {
    const options = projectionStoreOptions(
      projections,
      { securityParameter: k, trackedSet: EMPTY },
      dialect,
    );
    const store =
      dialect === "sqlite"
        ? openSqliteFactStore({ ...options, path: ":memory:" })
        : openPostgresFactStore({
            ...options,
            connection: { connectionString: await databases.create() },
          });
    const started = await store.start();
    if (started.kind !== "ready")
      throw new Error(`store start: ${JSON.stringify(started)}`);
    return store;
  };
};

/** A simulated chain from SIM_ORIGIN that feeds every event to `store`. */
export class ChainDriver {
  readonly chain: SimChain;

  constructor(
    readonly store: FactStore,
    tracked: TrackedSet,
  ) {
    this.chain = new SimChain(simUniverse(), SIM_ORIGIN, tracked);
  }

  async init(): Promise<void> {
    const init = await this.store.initialize(SIM_ORIGIN);
    if (init.kind !== "initialized")
      throw new Error(`initialize: ${init.kind}`);
  }

  /** Appends a block of `txs`; returns its tx hashes. */
  async forward(txs: readonly SimTx[]): Promise<readonly Buffer[]> {
    const { event, encoded } = this.chain.forward(txs);
    const result = await applyChainSyncEvent(this.store, event);
    if (result.result.kind !== "applied")
      throw new Error(
        `apply: ${JSON.stringify(result.result, (_, v: unknown) => (typeof v === "bigint" ? v.toString() : v))}`,
      );
    return encoded.txHashes;
  }

  async backward(depth: number): Promise<void> {
    const result = await applyChainSyncEvent(
      this.store,
      this.chain.backward(depth),
    );
    if (result.result.kind !== "rewound")
      throw new Error(`rewind: ${result.result.kind}`);
  }

  get tip() {
    return this.chain.tip;
  }
}
