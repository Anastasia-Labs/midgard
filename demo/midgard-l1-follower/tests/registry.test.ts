import { describe, expect, it } from "vitest";

import {
  type BlockSummary,
  createTemporalRegistry,
  openSqliteFactStore,
  RegistryError,
  ROLLBACK_LOG_ROWS,
} from "../src/index.js";
import { chain, options, ORIGIN } from "./support/small-chain.js";

describe("rollback log retention (SQLite)", () => {
  it("keeps the last 1,000 l1_rollbacks rows", async () => {
    const store = openSqliteFactStore({ ...options(2), path: ":memory:" });
    try {
      await store.start();
      await store.initialize(ORIGIN);
      const hash = Buffer.alloc(32);
      for (let i = 0; i < ROLLBACK_LOG_ROWS + 5; i += 1) {
        hash.writeUInt32BE(i + 1, 0);
        const block: BlockSummary = {
          point: { slot: 101, hash: Buffer.from(hash) },
          height: 51,
          parentHash: ORIGIN.point.hash,
          txs: [],
        };
        expect(await store.applyBlock(block)).toMatchObject({
          kind: "applied",
        });
        expect(await store.rewind(ORIGIN.point)).toMatchObject({
          kind: "rewound",
          generation: i + 1,
        });
      }
      expect(await store.prune(2)).toMatchObject({ done: false });
      expect(await store.prune(10)).toMatchObject({ done: true });
      const rows = await store.transaction("read", (sql) =>
        sql.query(
          "SELECT min(generation) AS lo, count(*) AS n FROM l1_rollbacks",
        ),
      );
      expect([Number(rows[0]?.lo), Number(rows[0]?.n)]).toEqual([
        6,
        ROLLBACK_LOG_ROWS,
      ]);
    } finally {
      await store.close();
    }
  });
});

describe("temporal registry", () => {
  it("orders parents first and generates children-first rewind SQL", () => {
    const registry = createTemporalRegistry([
      {
        name: "child",
        shape: "append_only",
        slotColumn: "slot",
        parents: ["parent", "l1_blocks"],
        retention: { kind: "created_k_deep" },
      },
      {
        name: "parent",
        shape: "versioned",
        startColumn: "from_slot",
        endColumn: "to_slot",
        retention: { kind: "closed_k_deep" },
      },
    ]);
    expect(registry.tables.map((table) => table.name)).toEqual([
      "parent",
      "child",
    ]);
    expect(registry.rewindStatements(42)).toEqual([
      { sql: "DELETE FROM child WHERE slot > ?", params: [42] },
      { sql: "DELETE FROM parent WHERE from_slot > ?", params: [42] },
      {
        sql: "UPDATE parent SET to_slot = NULL WHERE to_slot > ?",
        params: [42],
      },
    ]);
  });

  it.each([
    [
      "the l1_ prefix",
      [
        {
          name: "l1_mine",
          shape: "append_only",
          slotColumn: "slot",
          retention: { kind: "created_k_deep" },
        },
      ],
    ],
    [
      "a duplicate",
      [
        {
          name: "t",
          shape: "append_only",
          slotColumn: "slot",
          retention: { kind: "created_k_deep" },
        },
        {
          name: "t",
          shape: "append_only",
          slotColumn: "slot",
          retention: { kind: "created_k_deep" },
        },
      ],
    ],
    [
      "a non-identifier",
      [
        {
          name: "t; DROP",
          shape: "append_only",
          slotColumn: "slot",
          retention: { kind: "created_k_deep" },
        },
      ],
    ],
    [
      "an unknown parent",
      [
        {
          name: "t",
          shape: "append_only",
          slotColumn: "slot",
          parents: ["nope"],
          retention: { kind: "created_k_deep" },
        },
      ],
    ],
    [
      "a cycle",
      [
        {
          name: "a",
          shape: "append_only",
          slotColumn: "slot",
          parents: ["b"],
          retention: { kind: "created_k_deep" },
        },
        {
          name: "b",
          shape: "append_only",
          slotColumn: "slot",
          parents: ["a"],
          retention: { kind: "created_k_deep" },
        },
      ],
    ],
    [
      "an unregistered pinning table",
      [
        {
          name: "t",
          shape: "append_only",
          slotColumn: "slot",
          retention: { kind: "created_k_deep" },
          pinnedBy: [{ column: "id", table: "nope", tableColumn: "id" }],
        },
      ],
    ],
    [
      "a pin through a pinned table",
      [
        {
          name: "a",
          shape: "append_only",
          slotColumn: "slot",
          retention: { kind: "created_k_deep" },
          pinnedBy: [{ column: "id", table: "b", tableColumn: "id" }],
        },
        {
          name: "b",
          shape: "append_only",
          slotColumn: "slot",
          retention: { kind: "created_k_deep" },
          pinnedBy: [{ column: "id", table: "c", tableColumn: "id" }],
        },
        {
          name: "c",
          shape: "append_only",
          slotColumn: "slot",
          retention: { kind: "created_k_deep" },
        },
      ],
    ],
    [
      "a pin column that is not an identifier",
      [
        {
          name: "a",
          shape: "append_only",
          slotColumn: "slot",
          retention: { kind: "created_k_deep" },
          pinnedBy: [{ column: "id = 1 OR 1", table: "c", tableColumn: "id" }],
        },
        {
          name: "c",
          shape: "append_only",
          slotColumn: "slot",
          retention: { kind: "created_k_deep" },
        },
      ],
    ],
    [
      "a retention that does not fit the shape",
      [
        {
          name: "t",
          shape: "append_only",
          slotColumn: "slot",
          retention: { kind: "closed_k_deep" },
        },
      ],
    ],
  ] as const)("refuses %s", (_, specs) => {
    expect(() => createTemporalRegistry(specs)).toThrow(RegistryError);
  });
});

describe("temporal row pins (SQLite)", () => {
  const ids = async (
    store: ReturnType<typeof openSqliteFactStore>,
    table: string,
  ): Promise<number[]> =>
    (
      await store.transaction("read", (sql) =>
        sql.query(`SELECT id FROM ${table} ORDER BY id`),
      )
    ).map((row) => Number(row.id));

  const pruneAll = async (store: ReturnType<typeof openSqliteFactStore>) => {
    for (let n = 0; n < 10; n += 1) {
      const pruned = await store.prune(100);
      if ("kind" in pruned) throw new Error(pruned.kind);
      if (pruned.done) return;
    }
  };

  it("keeps a row past its rule while a pinning row lives, then prunes it", async () => {
    const store = openSqliteFactStore({
      ...options(2),
      path: ":memory:",
      temporalTables: [
        {
          name: "keeper",
          shape: "versioned",
          startColumn: "from_slot",
          endColumn: "to_slot",
          retention: { kind: "closed_k_deep" },
        },
        {
          name: "log",
          shape: "append_only",
          slotColumn: "slot",
          retention: { kind: "created_k_deep" },
          pinnedBy: [{ column: "id", table: "keeper", tableColumn: "id" }],
        },
      ],
      migrations: [
        {
          namespace: "pins",
          migrations: [
            {
              id: "0001",
              sql: `
-- class: D-t; retention: closed rows once to_slot is k deep
CREATE TABLE keeper (id integer NOT NULL, from_slot INTEGER NOT NULL, to_slot INTEGER);
-- class: D-t; retention: rows once slot is k deep, unless a keeper row names them
CREATE TABLE log (id integer NOT NULL, slot INTEGER NOT NULL);
`,
            },
          ],
        },
      ],
    });
    try {
      await store.start();
      await store.initialize(ORIGIN);
      for (const block of chain())
        expect(await store.applyBlock(block)).toMatchObject({
          kind: "applied",
        });
      // Height 53, k 2: the final boundary is b1 (slot 101).
      await store.transaction("write", async (sql) => {
        await sql.query("INSERT INTO log (id, slot) VALUES (1, 101), (2, 101)");
        await sql.query(
          "INSERT INTO keeper (id, from_slot, to_slot) VALUES (1, 101, NULL)",
        );
      });
      await pruneAll(store);
      expect(await ids(store, "log")).toEqual([1]);
      await store.transaction("write", (sql) =>
        sql.query("UPDATE keeper SET to_slot = 101 WHERE id = 1"),
      );
      await pruneAll(store);
      await pruneAll(store);
      expect(await ids(store, "keeper")).toEqual([]);
      expect(await ids(store, "log")).toEqual([]);
    } finally {
      await store.close();
    }
  });
});
