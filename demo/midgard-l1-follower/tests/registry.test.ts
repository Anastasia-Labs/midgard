import { describe, expect, it } from "vitest";

import {
  type BlockSummary,
  createTemporalRegistry,
  openSqliteFactStore,
  RegistryError,
  ROLLBACK_LOG_ROWS,
} from "../src/index.js";
import { options, ORIGIN } from "./support/small-chain.js";

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
