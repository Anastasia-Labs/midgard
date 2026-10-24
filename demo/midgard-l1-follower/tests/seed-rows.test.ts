import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import pg from "pg";
import { afterAll, describe, expect, it } from "vitest";

import { type BlockSummary, openPostgresFactStore } from "../src/index.js";
import { testDatabases } from "./support/postgres.js";
import {
  chain,
  fill,
  options,
  ORIGIN,
  output,
  point,
  started,
  storeAdapters,
  TRACKED,
  TX1,
  TX2,
  UNTRACKED,
} from "./support/small-chain.js";

const databases = testDatabases();
const scratch = mkdtempSync(join(tmpdir(), "l1-follower-seed-rows-"));

afterAll(async () => {
  await databases.dropAll();
  rmSync(scratch, { recursive: true, force: true });
});

/** Seed rows in the fact store (§5.3 step 4, INV6). */
describe.each(storeAdapters(databases, scratch))(
  "seed rows ($name)",
  (adapter) => {
    it("stores seed outputs once, skipping only outrefs the store holds a row for", async () => {
      const { store } = await started(adapter, 2, 1);
      try {
        const at = point(chain()[0]);
        const seed = {
          outRef: { txHash: fill(0x5e), index: 2 },
          output: output(TRACKED, 7n, true),
        };
        // TX1 is stored; its output #1 (untracked address) got no row, so the
        // seed takes it. TX1#0 has a row and is skipped.
        const ofStoredTx = {
          outRef: { txHash: TX1, index: 1 },
          output: output(UNTRACKED, 1n),
        };
        const first = await store.insertSeedOutputs(at, [
          seed,
          seed,
          ofStoredTx,
          { outRef: { txHash: TX1, index: 0 }, output: output(TRACKED, 1n) },
        ]);
        expect(first).toMatchObject({
          kind: "seeded",
          cursor: { point: at },
          inserted: [{ index: 2 }, { txHash: TX1, index: 1 }],
          skipped: [{ index: 2 }, { txHash: TX1, index: 0 }],
        });
        expect(await store.insertSeedOutputs(at, [seed])).toMatchObject({
          inserted: [],
          skipped: [{ index: 2 }],
        });
        expect(await store.output(seed.outRef)).toMatchObject({
          created: null,
          seedSlot: at.slot,
          spent: null,
        });
        expect(await store.output(ofStoredTx.outRef)).toMatchObject({
          created: null,
          seedSlot: at.slot,
        });
        expect(store.isTrackedLive(seed.outRef)).toBe(true);
        expect(
          await store.liveUtxos(
            { by: "address", address: TRACKED },
            ORIGIN.point,
          ),
        ).toMatchObject({ kind: "ok", utxos: [{ seedSlot: at.slot }] });
        expect((await store.checkInvariants()).ok).toBe(true);
      } finally {
        await store.close();
      }
    });

    it("rewinds the seed rows seeded above the target and keeps the rest", async () => {
      const { store } = await started(adapter, 4, 1);
      const [b1, b2, b3] = chain();
      const seedAt = async (byte: number, at: BlockSummary | undefined) => {
        const seed = {
          outRef: { txHash: fill(byte), index: 0 },
          output: output(TRACKED, 1n),
        };
        expect(await store.insertSeedOutputs(point(at), [seed])).toMatchObject({
          kind: "seeded",
          inserted: [seed.outRef],
        });
        return seed.outRef;
      };
      try {
        const atB1 = await seedAt(0x51, b1);
        expect(await store.applyBlock(b2 as BlockSummary)).toMatchObject({
          kind: "applied",
        });
        const atB2 = await seedAt(0x52, b2);
        expect(await store.applyBlock(b3 as BlockSummary)).toMatchObject({
          kind: "applied",
        });
        const atB3 = await seedAt(0x53, b3);
        const rewound = await store.rewind(point(b2));
        expect(rewound).toMatchObject({ kind: "rewound" });
        expect(
          rewound.kind === "rewound" ? rewound.deleted : [],
        ).toContainEqual(atB3);
        expect(await store.output(atB3)).toBeNull();
        expect(store.isTrackedLive(atB3)).toBe(false);
        // Seeded at the target and below: still canonical, untouched.
        expect(await store.output(atB2)).toMatchObject({ seedSlot: 103 });
        expect(await store.output(atB1)).toMatchObject({ seedSlot: 101 });
        expect(store.isTrackedLive(atB2)).toBe(true);
        expect((await store.checkInvariants()).ok).toBe(true);
      } finally {
        await store.close();
      }
    });

    it("fails INV6 on a seed row above the cursor or one its stored creator contradicts", async () => {
      const { store } = await started(adapter, 2, 2);
      const insertSeedRow = async (
        txHash: Buffer,
        index: number,
        seedSlot: number,
      ) =>
        await store.transaction("write", (sql) =>
          sql.query(
            "INSERT INTO l1_outputs (tx_hash, output_index, address, lovelace, assets, seed_slot) VALUES (?, ?, ?, ?, ?, ?)",
            [txHash, index, UNTRACKED, "1", store.dialect.json("{}"), seedSlot],
          ),
        );
      try {
        expect((await store.checkInvariants()).ok).toBe(true);
        // TX2 is stored (one output): a seed row of TX2#0 would duplicate its
        // row (the key refuses it); one of TX2#5 names an output TX2 lacks.
        await insertSeedRow(TX2, 5, 103);
        expect(await store.checkInvariants()).toMatchObject({
          ok: false,
          violations: [
            {
              invariant: "INV6",
              check: "seed row its stored creator contradicts",
            },
          ],
        });
        await store.transaction("write", (sql) =>
          sql.query(
            "DELETE FROM l1_outputs WHERE tx_hash = ? AND output_index = 5",
            [TX2],
          ),
        );
        // A seed of TX1#1 at the origin (slot 100) claims an output that the
        // stored TX1 created later, at slot 101.
        await insertSeedRow(TX1, 1, 100);
        expect(await store.checkInvariants()).toMatchObject({
          ok: false,
          violations: [
            {
              invariant: "INV6",
              check: "seed row its stored creator contradicts",
            },
          ],
        });
        await store.transaction("write", (sql) =>
          sql.query(
            "DELETE FROM l1_outputs WHERE tx_hash = ? AND output_index = 1",
            [TX1],
          ),
        );
        // A seed row above the cursor (slot 103) survived a rewind.
        await insertSeedRow(fill(0x5d), 0, 105);
        expect(await store.checkInvariants()).toMatchObject({
          ok: false,
          violations: [{ invariant: "INV6", check: "seed row above cursor" }],
        });
      } finally {
        await store.close();
      }
    });

    it("refuses seed rows read at a point that is no longer the cursor", async () => {
      const { store } = await started(adapter, 2, 1);
      try {
        const late = {
          outRef: { txHash: fill(0x5f), index: 0 },
          output: output(TRACKED, 3n),
        };
        expect(
          await store.insertSeedOutputs(ORIGIN.point, [late]),
        ).toMatchObject({
          kind: "cursor_moved",
          cursor: { point: point(chain()[0]) },
        });
        expect(await store.output(late.outRef)).toBeNull();
        expect(store.isTrackedLive(late.outRef)).toBe(false);
      } finally {
        await store.close();
      }
    });
  },
);

/**
 * A pool whose next COMMIT runs and then fails, as when the connection is
 * lost while COMMIT's answer is on the way; and, while `failReads` is set,
 * whose read transactions fail to begin.
 */
const ambiguousPool = (url: string) => {
  const pool = new pg.Pool({ connectionString: url, max: 4 });
  pool.on("error", () => undefined);
  const faults = { commit: false, failReads: false };
  const patched = new WeakSet<pg.PoolClient>();
  const connect = pool.connect.bind(pool) as () => Promise<pg.PoolClient>;
  pool.connect = (async () => {
    const client = await connect();
    if (!patched.has(client)) {
      patched.add(client);
      const query = client.query.bind(client) as (
        text: unknown,
        values?: unknown,
      ) => Promise<unknown>;
      client.query = (async (text: unknown, values?: unknown) => {
        if (faults.failReads && String(text).includes("READ ONLY"))
          throw new Error("connection refused while reading back");
        const result = await query(text, values);
        if (faults.commit && text === "COMMIT") {
          faults.commit = false;
          throw new Error("connection lost while COMMIT's answer was sent");
        }
        return result;
      }) as typeof client.query;
    }
    return client;
  }) as typeof pool.connect;
  return { pool, faults };
};

describe("seed rows on Postgres: a commit whose outcome is unknown", () => {
  const seed = {
    outRef: { txHash: fill(0x5d), index: 1 },
    output: output(TRACKED, 9n),
  };
  const opened = async () => {
    const { url } = await databases.create();
    const { pool, faults } = ambiguousPool(url);
    const store = openPostgresFactStore({
      ...options(2),
      connection: { pool },
    });
    expect(await store.start()).toMatchObject({ kind: "ready" });
    expect(await store.initialize(ORIGIN)).toMatchObject({
      kind: "initialized",
    });
    expect(await store.applyBlock(chain()[0]!)).toMatchObject({
      kind: "applied",
    });
    return { store, pool, faults };
  };

  it("reads the committed rows back into the live set", async () => {
    const { store, pool, faults } = await opened();
    try {
      faults.commit = true;
      expect(
        await store.insertSeedOutputs(point(chain()[0]), [seed]),
      ).toMatchObject({ kind: "error" });
      // Committed, and tracked live, so a spend of it qualifies.
      expect(await store.output(seed.outRef)).not.toBeNull();
      expect(store.isTrackedLive(seed.outRef)).toBe(true);
      // The retry skips the row and the set stays right.
      expect(
        await store.insertSeedOutputs(point(chain()[0]), [seed]),
      ).toMatchObject({ kind: "seeded", inserted: [], skipped: [seed.outRef] });
      expect(store.isTrackedLive(seed.outRef)).toBe(true);
    } finally {
      await store.close();
      await pool.end();
    }
  });

  it("refuses writes until a start reloads the set when the rows cannot be read back", async () => {
    const { store, pool, faults } = await opened();
    try {
      faults.commit = true;
      faults.failReads = true;
      expect(
        await store.insertSeedOutputs(point(chain()[0]), [seed]),
      ).toMatchObject({ kind: "error" });
      faults.failReads = false;
      expect(store.isTrackedLive(seed.outRef)).toBe(false);
      expect(await store.applyBlock(chain()[1]!)).toMatchObject({
        kind: "store_locked",
        detail: expect.stringMatching(/commit outcome is unknown/u) as unknown,
      });
      expect(await store.start()).toMatchObject({ kind: "ready" });
      expect(store.isTrackedLive(seed.outRef)).toBe(true);
      expect(await store.applyBlock(chain()[1]!)).toMatchObject({
        kind: "applied",
      });
    } finally {
      await store.close();
      await pool.end();
    }
  });
});
