import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import pg from "pg";
import { afterAll, describe, expect, it } from "vitest";

import {
  type DialectName,
  type FactStore,
  type FactStoreOptions,
  openPostgresFactStore,
  openSqliteFactStore,
  type PinResult,
  type RetainedPin,
} from "../src/index.js";
import { testDatabases } from "./support/postgres.js";
import { chain, options, ORIGIN, TX1, TX2 } from "./support/small-chain.js";

/**
 * `pinRetained` against pruning. With k = 1 and the small chain followed to
 * b3, a prune step deletes TX1 (its one tracked output was spent at b2, the
 * retained boundary) and keeps TX2 (its output is spent above the boundary).
 * A role pins a tx through a `retentionPins` table, as the watcher does.
 */
const K = 1;
const PIN_TABLE = "test_tx_pins";

const pinOptions = (dialect: DialectName): FactStoreOptions => ({
  ...options(K),
  migrations: [
    {
      namespace: "pin_test",
      migrations: [
        {
          id: "0001_test_tx_pins",
          sql: `-- class: B; retention: owner (the pin test)
CREATE TABLE ${PIN_TABLE} (tx_hash ${dialect === "postgres" ? "bytea" : "BLOB"} PRIMARY KEY);`,
        },
      ],
    },
  ],
  retentionPins: { txs: [{ table: PIN_TABLE, column: "tx_hash" }] },
});

const txPin = (hash: Buffer): RetainedPin => ({
  retained: async (tx) =>
    (await tx.query("SELECT 1 AS one FROM l1_txs WHERE tx_hash = ?", [hash]))
      .length > 0,
  insert: async (tx) => {
    await tx.query(
      `INSERT INTO ${PIN_TABLE} (tx_hash) VALUES (?) ON CONFLICT DO NOTHING`,
      [hash],
    );
  },
});

const pinRows = async (store: FactStore, hash: Buffer): Promise<number> =>
  (
    await store.transaction("read", (tx) =>
      tx.query(`SELECT tx_hash FROM ${PIN_TABLE} WHERE tx_hash = ?`, [hash]),
    )
  ).length;

const followed = async (store: FactStore): Promise<void> => {
  expect(await store.start()).toMatchObject({ kind: "ready" });
  expect(await store.initialize(ORIGIN)).toMatchObject({
    kind: "initialized",
  });
  for (const block of chain())
    expect(await store.applyBlock(block)).toMatchObject({ kind: "applied" });
};

const databases = testDatabases();
const scratch = mkdtempSync(join(tmpdir(), "l1-follower-pin-retained-"));

afterAll(async () => {
  await databases.dropAll();
  rmSync(scratch, { recursive: true, force: true });
});

const adapters: readonly Readonly<{
  name: DialectName;
  open: () => Promise<FactStore>;
}>[] = [
  {
    name: "sqlite",
    open: async () =>
      openSqliteFactStore({
        ...pinOptions("sqlite"),
        path: join(scratch, `${String(Math.random()).slice(2)}.db`),
      }),
  },
  {
    name: "postgres",
    open: async () =>
      openPostgresFactStore({
        ...pinOptions("postgres"),
        connection: { connectionString: (await databases.create()).url },
      }),
  },
];

describe.each(adapters)("pinRetained ($name)", (adapter) => {
  it("pins a retained tx, and the pin keeps it through a prune that would delete it", async () => {
    const store = await adapter.open();
    try {
      await followed(store);
      expect(await store.pinRetained(txPin(TX1))).toEqual({ kind: "pinned" });
      expect(await pinRows(store, TX1)).toBe(1);
      expect(await store.prune()).toMatchObject({ done: true });
      expect(await store.txByHash(TX1)).not.toBeNull();
    } finally {
      await store.close();
    }
  });

  it("returns already_pruned and writes nothing once a prune removed the tx (negative control: unpinned TX1 is pruned)", async () => {
    const store = await adapter.open();
    try {
      await followed(store);
      expect(await store.prune()).toMatchObject({ done: true });
      expect(await store.txByHash(TX1)).toBeNull();
      expect(await store.txByHash(TX2)).not.toBeNull();
      expect(await store.pinRetained(txPin(TX1))).toEqual({
        kind: "already_pruned",
      });
      expect(await pinRows(store, TX1)).toBe(0);
      expect(await store.pinRetained(txPin(TX2))).toEqual({ kind: "pinned" });
    } finally {
      await store.close();
    }
  });
});

/**
 * Concurrent pin and prune on Postgres, interleaved deterministically: a
 * third session holds a row lock that parks one transaction mid-flight, the
 * other is started and seen waiting in `pg_stat_activity`, then the third
 * session lets go.
 */
describe("pinRetained racing a prune step (postgres)", () => {
  const openRace = async () => {
    const { url, name } = await databases.create();
    const store = openPostgresFactStore({
      ...pinOptions("postgres"),
      connection: { connectionString: url },
    });
    await followed(store);
    const holder = new pg.Client({ connectionString: url });
    await holder.connect();
    const observer = new pg.Client({ connectionString: url });
    await observer.connect();
    /**
     * Resolves once `count` sessions wait on a lock; fails when `settled`
     * (the transaction expected to wait) finishes without waiting.
     */
    const waiting = async (
      count: number,
      settled: Promise<unknown>,
    ): Promise<void> => {
      let done = false;
      void settled.finally(() => {
        done = true;
      });
      for (let i = 0; i < 500 && !done; i += 1) {
        const rows = await observer.query<{ n: string }>(
          "SELECT count(*) AS n FROM pg_stat_activity WHERE datname = $1 AND wait_event_type = 'Lock'",
          [name],
        );
        if (Number(rows.rows[0]?.n ?? 0) >= count) return;
        await new Promise((resolve) => setTimeout(resolve, 20));
      }
      throw new Error(
        done
          ? "the transaction finished without waiting on a lock"
          : `fewer than ${String(count)} lock waiters`,
      );
    };
    const close = async (): Promise<void> => {
      await holder.end();
      await observer.end();
      await store.close();
    };
    return { store, holder, waiting, close };
  };

  it("a pin that starts while a prune step is deleting the tx waits for it and returns already_pruned", async () => {
    const { store, holder, waiting, close } = await openRace();
    try {
      // Parks the prune step at its l1_txs delete, after it took the cursor lock.
      await holder.query("BEGIN");
      await holder.query("SELECT 1 FROM l1_txs WHERE tx_hash = $1 FOR UPDATE", [
        TX1,
      ]);
      const pruned = store.prune();
      await waiting(1, pruned);
      const pinned: Promise<PinResult> = store.pinRetained(txPin(TX1));
      await waiting(2, pinned);
      await holder.query("COMMIT");
      expect(await pruned).toMatchObject({ done: true });
      expect(await pinned).toEqual({ kind: "already_pruned" });
      expect(await pinRows(store, TX1)).toBe(0);
      expect(await store.txByHash(TX1)).toBeNull();
    } finally {
      await holder.query("ROLLBACK").catch(() => undefined);
      await close();
    }
  });

  it("a prune step that starts while a pin holds the cursor lock waits for it and keeps the tx", async () => {
    const { store, holder, waiting, close } = await openRace();
    try {
      // Parks both on the cursor row: the pin first, then the prune step.
      await holder.query("BEGIN");
      await holder.query("SELECT 1 FROM l1_follower_cursor FOR UPDATE");
      const pinned: Promise<PinResult> = store.pinRetained(txPin(TX1));
      await waiting(1, pinned);
      const pruned = store.prune();
      await waiting(2, pruned);
      await holder.query("COMMIT");
      expect(await pinned).toEqual({ kind: "pinned" });
      expect(await pruned).toMatchObject({ done: true });
      expect(await pinRows(store, TX1)).toBe(1);
      expect(await store.txByHash(TX1)).not.toBeNull();
    } finally {
      await holder.query("ROLLBACK").catch(() => undefined);
      await close();
    }
  });
});
