import { mkdtempSync, rmSync, unlinkSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { setTimeout as sleep } from "node:timers/promises";

import pg from "pg";
import { afterAll, describe, expect, it } from "vitest";

import {
  type BlockSummary,
  type FactStore,
  openSqliteFactStore,
  type StartResult,
  writerLeasePath,
} from "../src/index.js";
import { testDatabases } from "./support/postgres.js";
import { resetAdapters, roleOptions } from "./support/reset-stores.js";
import { chain, ORIGIN } from "./support/small-chain.js";

const databases = testDatabases();
const scratch = mkdtempSync(join(tmpdir(), "l1-follower-lease-"));

afterAll(async () => {
  await databases.dropAll();
  rmSync(scratch, { recursive: true, force: true });
});

const [b1, b2] = chain() as [BlockSummary, BlockSummary, BlockSummary];

/** The caller's loop: retry `start()` with backoff while it is `store_locked`. */
const startWithin = async (
  store: FactStore,
  attempts: number,
): Promise<StartResult> => {
  let result = await store.start();
  for (let i = 1; i < attempts && result.kind === "store_locked"; i += 1) {
    await sleep(50);
    result = await store.start();
  }
  return result;
};

describe.each(resetAdapters(databases, scratch))(
  "writer lease ($name)",
  (adapter) => {
    it("a second start against a held lease is store_locked; the waiter takes over once the holder closes", async () => {
      const location = await adapter.create();
      const holder = location.store();
      const waiter = location.store();
      let holderOpen = true;
      try {
        expect(await holder.start()).toMatchObject({ kind: "ready" });
        expect(await holder.initialize(ORIGIN)).toMatchObject({
          kind: "initialized",
        });
        expect(await waiter.start()).toEqual({
          kind: "store_locked",
          detail: expect.stringMatching(/writer lease/u) as unknown,
        });
        // The waiter writes nothing while it waits.
        expect(await waiter.applyBlock(b1)).toMatchObject({ kind: "error" });
        expect(await holder.applyBlock(b1)).toMatchObject({ kind: "applied" });
        holderOpen = false;
        await holder.close();
        expect(await startWithin(waiter, 1)).toMatchObject({
          kind: "ready",
          cursor: { height: 51 },
        });
        expect(await waiter.applyBlock(b2)).toMatchObject({ kind: "applied" });
      } finally {
        await waiter.close();
        if (holderOpen) await holder.close();
      }
    });

    it("a holder fenced by a newer epoch gets store_locked on its next write, then starts again", async () => {
      const location = await adapter.create();
      const store = location.store();
      const other = location.backend();
      try {
        expect(await store.start()).toMatchObject({ kind: "ready" });
        expect(await store.initialize(ORIGIN)).toMatchObject({
          kind: "initialized",
        });
        // What a newer lease holder's start does.
        await other.transaction("write", (tx) =>
          tx.query(
            "UPDATE l1_follower_writer SET writer_epoch = writer_epoch + 1",
          ),
        );
        expect(await store.applyBlock(b1)).toMatchObject({
          kind: "store_locked",
          detail: expect.stringMatching(
            /lost the store's writer lease/u,
          ) as unknown,
        });
        expect((await store.cursor())?.height).toBe(ORIGIN.height);
        expect(await store.prune()).toMatchObject({ kind: "store_locked" });
        expect(await store.start()).toMatchObject({ kind: "ready" });
        expect(await store.applyBlock(b1)).toMatchObject({ kind: "applied" });
      } finally {
        await other.close();
        await store.close();
      }
    });
  },
);

describe("writer lease (sqlite sidecar)", () => {
  it("is lost once its sidecar file is gone: the holder's next write is store_locked, and it starts again on a new sidecar", async () => {
    const path = join(scratch, `${String(Math.random()).slice(2)}.db`);
    const store = openSqliteFactStore({ ...roleOptions("sqlite"), path });
    try {
      expect(await store.start()).toMatchObject({ kind: "ready" });
      expect(await store.initialize(ORIGIN)).toMatchObject({
        kind: "initialized",
      });
      unlinkSync(writerLeasePath(path));
      // Another process could now lock a new sidecar at the same path.
      expect(await store.applyBlock(b1)).toMatchObject({
        kind: "store_locked",
        detail: expect.stringMatching(
          /lost the store's writer lease/u,
        ) as unknown,
      });
      expect((await store.cursor())?.height).toBe(ORIGIN.height);
      expect(await store.start()).toMatchObject({ kind: "ready" });
      expect(await store.applyBlock(b1)).toMatchObject({ kind: "applied" });
    } finally {
      await store.close();
    }
  });
});

describe("writer lease (postgres session)", () => {
  it("the waiter takes over within one reconnect once the holder's lease session is killed; the old holder cannot start or write", async () => {
    const [, postgres] = resetAdapters(databases, scratch);
    const location = await postgres!.create();
    const holder = location.store();
    const waiter = location.store();
    const admin = new pg.Client({ connectionString: location.url });
    await admin.connect();
    try {
      expect(await holder.start()).toMatchObject({ kind: "ready" });
      expect(await holder.initialize(ORIGIN)).toMatchObject({
        kind: "initialized",
      });
      expect(await waiter.start()).toMatchObject({ kind: "store_locked" });
      const killed = await admin.query<{ pid: number }>(
        `SELECT pid FROM pg_locks
          WHERE locktype = 'advisory' AND granted AND pid <> pg_backend_pid()
            AND database = (SELECT oid FROM pg_database WHERE datname = current_database())`,
      );
      expect(killed.rows).toHaveLength(1);
      await admin.query("SELECT pg_terminate_backend($1)", [
        killed.rows[0]?.pid,
      ]);
      expect(await startWithin(waiter, 40)).toMatchObject({
        kind: "ready",
        cursor: { height: ORIGIN.height },
      });
      expect(await holder.applyBlock(b1)).toMatchObject({
        kind: "store_locked",
      });
      expect(await holder.start()).toMatchObject({ kind: "store_locked" });
      expect(await waiter.applyBlock(b1)).toMatchObject({ kind: "applied" });
    } finally {
      await admin.end();
      await waiter.close();
      await holder.close();
    }
  });
});
