import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import pg from "pg";
import { afterAll, describe, expect, it } from "vitest";

import { runFollowerCli } from "../src/cli/run.js";
import {
  type BlockSummary,
  type FactStore,
  listenForGenerations,
  resetToOrigin,
} from "../src/index.js";
import { asNumber } from "../src/sql/backend.js";
import { testDatabases } from "./support/postgres.js";
import { type Location, resetAdapters } from "./support/reset-stores.js";
import { chain, fill, ORIGIN } from "./support/small-chain.js";

const databases = testDatabases();
const scratch = mkdtempSync(join(tmpdir(), "l1-follower-reset-"));

afterAll(async () => {
  await databases.dropAll();
  rmSync(scratch, { recursive: true, force: true });
});

/** Another origin: what the operator configured after the reset. */
const OTHER_ORIGIN = { point: { slot: 90, hash: fill(0x90) }, height: 45 };

const count = async (store: FactStore, table: string): Promise<number> =>
  store.transaction("read", async (tx) =>
    asNumber((await tx.query(`SELECT count(*) AS n FROM ${table}`))[0]?.n),
  );

/**
 * A store that followed origin + three blocks (with seeds, the fixture
 * derivation's D-t rows and an event key), plus one class B and one class C
 * role row; closed again, as an operator stops it before a reset.
 */
const followed = async (location: Location): Promise<void> => {
  const store = location.store();
  try {
    expect(await store.start()).toMatchObject({ kind: "ready" });
    expect(await store.initialize(ORIGIN)).toMatchObject({
      kind: "initialized",
    });
    for (const block of chain())
      expect(await store.applyBlock(block)).toMatchObject({ kind: "applied" });
    await store.transaction("write", async (tx) => {
      await tx.query("INSERT INTO role_signed (id, body) VALUES (?, ?)", [
        1,
        fill(0xb0, 8),
      ]);
      await tx.query(
        "INSERT INTO role_content (content_hash, bytes) VALUES (?, ?)",
        [fill(0xcc), fill(0xcd, 8)],
      );
    });
    expect(await count(store, "fixture_block_marks")).toBe(3);
    expect(await count(store, "l1_outputs")).toBeGreaterThan(0);
  } finally {
    await store.close();
  }
};

const reset = async (location: Location) => {
  const backend = location.backend();
  try {
    return await resetToOrigin(backend);
  } finally {
    await backend.close();
  }
};

describe.each(resetAdapters(databases, scratch))(
  "reset --to-origin ($name)",
  (adapter) => {
    it("after a reset through the CLI, a start with a different origin initializes cleanly", async () => {
      const location = await adapter.create();
      await followed(location);
      const before = location.store();
      try {
        await before.start();
        expect(await before.initialize(OTHER_ORIGIN)).toMatchObject({
          kind: "origin_mismatch",
        });
      } finally {
        await before.close();
      }
      const out: string[] = [];
      const err: string[] = [];
      expect(
        await runFollowerCli(
          ["reset", "--to-origin", ...location.cliArgs],
          {},
          {
            stdout: (text) => out.push(text),
            stderr: (text) => err.push(text),
          },
        ),
      ).toBe(0);
      expect(err).toEqual([]);
      expect(JSON.parse(out.join(""))).toMatchObject({
        reset: "to-origin",
        nextGeneration: 1,
        tables: expect.arrayContaining([
          "fixture_block_marks",
          "l1_blocks",
          "l1_follower_cursor",
          "l1_outputs",
          "l1_scripts",
          "role_content",
        ]) as unknown,
      });
      const after = location.store();
      try {
        expect(await after.start()).toMatchObject({
          kind: "ready",
          cursor: null,
          liveOutRefs: 0,
        });
        expect(await after.initialize(OTHER_ORIGIN)).toMatchObject({
          kind: "initialized",
          cursor: { origin: { slot: OTHER_ORIGIN.point.slot } },
        });
        for (const table of [
          "l1_txs",
          "l1_outputs",
          "l1_event_keys",
          "l1_rollbacks",
          "fixture_block_marks",
          "fixture_spend_log",
          "fixture_address_live_count",
          "role_content",
        ])
          expect([table, await count(after, table)]).toEqual([table, 0]);
        expect((await after.checkInvariants()).ok).toBe(true);
      } finally {
        await after.close();
      }
    });

    it("keeps a class B row, and a second reset is a no-op", async () => {
      const location = await adapter.create();
      await followed(location);
      const first = await reset(location);
      expect(first).toMatchObject({ kind: "reset", nextGeneration: 1 });
      if (first.kind !== "reset") throw new Error("unreachable");
      expect(first.tables).not.toContain("role_signed");
      expect(await reset(location)).toEqual(first);
      const store = location.store();
      try {
        await store.start();
        const kept = await store.transaction("read", (tx) =>
          tx.query("SELECT id, body FROM role_signed"),
        );
        expect(
          kept.map((row) => [
            asNumber(row.id),
            Buffer.from(row.body as Uint8Array).toString("hex"),
          ]),
        ).toEqual([[1, fill(0xb0, 8).toString("hex")]]);
        expect(await count(store, "role_content")).toBe(0);
        expect(await count(store, "l1_follower_migrations")).toBeGreaterThan(0);
      } finally {
        await store.close();
      }
    });

    it("refuses against a running follower, and changes nothing", async () => {
      const location = await adapter.create();
      await followed(location);
      const running = location.store();
      try {
        expect(await running.start()).toMatchObject({ kind: "ready" });
        expect(await reset(location)).toEqual({
          kind: "store_locked",
          detail: expect.stringMatching(/stop it before resetting/u) as unknown,
        });
        const err: string[] = [];
        expect(
          await runFollowerCli(
            ["reset", "--to-origin", ...location.cliArgs],
            {},
            { stdout: () => undefined, stderr: (text) => err.push(text) },
          ),
        ).toBe(4);
        expect(err.join("")).toMatch(/^reset refused: a running follower/u);
        expect(await running.cursor()).toMatchObject({ height: 53 });
        expect(await count(running, "fixture_block_marks")).toBe(3);
        expect(await count(running, "role_content")).toBe(1);
      } finally {
        await running.close();
      }
    });

    it("refuses, and deletes nothing, when a table outside the catalog references a reset table", async () => {
      const location = await adapter.create();
      await followed(location);
      const store = location.store();
      try {
        await store.start();
        await store.transaction("write", async (tx) => {
          // A role table no follower migration declares, so never in the catalog.
          await tx.exec(
            "CREATE TABLE outside_note (slot bigint REFERENCES l1_blocks(slot) ON DELETE CASCADE)",
          );
          await tx.query("INSERT INTO outside_note (slot) VALUES (?)", [
            ORIGIN.point.slot,
          ]);
        });
      } finally {
        await store.close();
      }
      // SQLite names the table; Postgres refuses the TRUNCATE itself.
      await expect(reset(location)).rejects.toThrow(
        /outside_note|cannot truncate a table referenced in a foreign key/u,
      );
      const after = location.store();
      try {
        expect(await after.start()).toMatchObject({
          kind: "ready",
          cursor: { height: 53 },
        });
        expect(await count(after, "outside_note")).toBe(1);
      } finally {
        await after.close();
      }
    });

    it("keeps generations monotonic: a view from before the reset is invalid after reset and initialize", async () => {
      const location = await adapter.create();
      await followed(location);
      const old = location.store();
      let view;
      try {
        await old.start();
        view = await old.currentView();
        expect(view).toMatchObject({ generation: 0, height: 53 });
      } finally {
        await old.close();
      }
      expect(await reset(location)).toMatchObject({ nextGeneration: 1 });
      const store = location.store();
      try {
        await store.start();
        expect(await store.initialize(ORIGIN)).toMatchObject({
          kind: "initialized",
          cursor: { generation: 1 },
        });
        expect(await store.viewValid(view!)).toBe(false);
        // A rewind after the reset goes on from there.
        const [b1, b2] = chain() as [BlockSummary, BlockSummary];
        await store.applyBlock(b1);
        await store.applyBlock(b2);
        expect(await store.rewind(b1.point)).toMatchObject({
          kind: "rewound",
          generation: 2,
        });
      } finally {
        await store.close();
      }
      // A second reset starts above the rewound generation.
      expect(await reset(location)).toMatchObject({ nextGeneration: 3 });
    });
  },
);

describe("reset --to-origin (postgres notification)", () => {
  it("notifies l1_generation with the next generation", async () => {
    const [, postgres] = resetAdapters(databases, scratch);
    const location = await postgres!.create();
    await followed(location);
    const pool = new pg.Pool({ connectionString: location.url });
    const seen: number[] = [];
    const stop = await listenForGenerations(pool, (generation) =>
      seen.push(generation),
    );
    try {
      expect(await reset(location)).toMatchObject({ nextGeneration: 1 });
      for (let i = 0; i < 50 && seen.length === 0; i += 1)
        await new Promise((resolve) => setTimeout(resolve, 20));
      expect(seen).toEqual([1]);
    } finally {
      await stop();
      await pool.end();
    }
  });
});
