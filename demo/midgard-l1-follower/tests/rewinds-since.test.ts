/**
 * `rewindsSince(after)`, the pull side of the generation notification
 * (`store/rewinds-since.ts`), over the rollback log: the lowest target over
 * `(after, generation]`, and the origin when a row of that range is missing
 * or `after` is above the store's generation; a reader that never handled a
 * generation (`after` null) reads the range from 0 under the same rule.
 */
import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterAll, describe, expect, it } from "vitest";

import {
  decodeBlock,
  type DialectName,
  type FactStore,
  openPostgresFactStore,
  openSqliteFactStore,
  type Point,
} from "../src/index.js";
import {
  SIM_ORIGIN,
  SimChain,
  simStoreOptions,
  simUniverse,
} from "../src/testing/index.js";
import { testDatabases } from "./support/postgres.js";

const databases = testDatabases();
const scratch = mkdtempSync(join(tmpdir(), "l1-follower-rewinds-since-"));

afterAll(async () => {
  await databases.dropAll();
  rmSync(scratch, { recursive: true, force: true });
});

const K = 10;

const open = async (dialect: DialectName): Promise<FactStore> => {
  const options = simStoreOptions([], K, dialect);
  return dialect === "sqlite"
    ? openSqliteFactStore({
        ...options,
        path: join(scratch, `${String(Math.random()).slice(2)}.db`),
      })
    : openPostgresFactStore({
        ...options,
        connection: { connectionString: (await databases.create()).url },
      });
};

/**
 * Six blocks from the origin, then two rewinds whose targets descend: the
 * first (generation 1) to the fourth block, the second (generation 2) to the
 * second. Returns the blocks' points.
 */
const twoRewinds = async (store: FactStore): Promise<Point[]> => {
  expect((await store.start()).kind).toBe("ready");
  expect((await store.initialize(SIM_ORIGIN)).kind).toBe("initialized");
  const chain = new SimChain(simUniverse(), SIM_ORIGIN);
  const points: Point[] = [];
  for (let i = 0; i < 6; i += 1) {
    expect(
      (await store.applyBlock(decodeBlock(chain.forward([]).encoded.raw))).kind,
    ).toBe("applied");
    points.push((await store.cursor())!.point);
  }
  expect((await store.rewind(points[3]!)).kind).toBe("rewound");
  expect((await store.rewind(points[1]!)).kind).toBe("rewound");
  expect((await store.cursor())?.generation).toBe(2);
  return points;
};

const dropRow = (store: FactStore, generation: number) =>
  store.transaction("write", (tx) =>
    tx.query("DELETE FROM l1_rollbacks WHERE generation = ?", [generation]),
  );

describe.each(["sqlite", "postgres"] as const)(
  "rewindsSince (%s)",
  (dialect) => {
    it("is null before the store has a cursor", async () => {
      const store = await open(dialect);
      try {
        expect((await store.start()).kind).toBe("ready");
        expect(await store.rewindsSince(null)).toBeNull();
      } finally {
        await store.close();
      }
    });

    it("answers no target at the store's generation, and for a reader that never handled one on a store that never rewound", async () => {
      const store = await open(dialect);
      try {
        expect((await store.start()).kind).toBe("ready");
        expect((await store.initialize(SIM_ORIGIN)).kind).toBe("initialized");
        expect(await store.rewindsSince(null)).toEqual({
          generation: 0,
          target: null,
        });
        expect(await store.rewindsSince(0)).toEqual({
          generation: 0,
          target: null,
        });
      } finally {
        await store.close();
      }
    });

    it("returns the lowest target over the range, not the highest", async () => {
      const store = await open(dialect);
      try {
        const points = await twoRewinds(store);
        // Generation 1 went back to the fourth block, 2 to the second.
        for (const after of [null, 0])
          expect(await store.rewindsSince(after)).toEqual({
            generation: 2,
            target: points[1],
          });
        expect(await store.rewindsSince(1)).toEqual({
          generation: 2,
          target: points[1],
        });
        expect(await store.rewindsSince(2)).toEqual({
          generation: 2,
          target: null,
        });
      } finally {
        await store.close();
      }
    });

    it("returns the lowest target over the range when a later rewind went back less far", async () => {
      const store = await open(dialect);
      try {
        expect((await store.start()).kind).toBe("ready");
        expect((await store.initialize(SIM_ORIGIN)).kind).toBe("initialized");
        const chain = new SimChain(simUniverse(), SIM_ORIGIN);
        const forward = async (blocks: number) => {
          for (let i = 0; i < blocks; i += 1)
            expect(
              (
                await store.applyBlock(
                  decodeBlock(chain.forward([]).encoded.raw),
                )
              ).kind,
            ).toBe("applied");
        };
        await forward(6);
        chain.backward(4);
        const low = chain.tip.point;
        expect((await store.rewind(low)).kind).toBe("rewound");
        await forward(4);
        chain.backward(1);
        const high = chain.tip.point;
        expect((await store.rewind(high)).kind).toBe("rewound");
        expect(high.slot).toBeGreaterThan(low.slot);
        expect(await store.rewindsSince(0)).toEqual({
          generation: 2,
          target: low,
        });
        expect(await store.rewindsSince(1)).toEqual({
          generation: 2,
          target: high,
        });
      } finally {
        await store.close();
      }
    });

    it("returns the origin when a row of the range is missing", async () => {
      const store = await open(dialect);
      try {
        await twoRewinds(store);
        // The lower target's row is gone: what is left is the higher one,
        // which is no answer for a range that misses a rewind.
        await dropRow(store, 2);
        expect(await store.rewindsSince(0)).toEqual({
          generation: 2,
          target: SIM_ORIGIN.point,
        });
        expect(await store.rewindsSince(1)).toEqual({
          generation: 2,
          target: SIM_ORIGIN.point,
        });
      } finally {
        await store.close();
      }
    });

    it("returns the origin to a reader that never handled a generation when the log holds fewer rows than the store's generation", async () => {
      const store = await open(dialect);
      try {
        const points = await twoRewinds(store);
        await dropRow(store, 1);
        // The row left targets the second block; one row is missing.
        expect(await store.rewindsSince(1)).toEqual({
          generation: 2,
          target: points[1],
        });
        expect(await store.rewindsSince(null)).toEqual({
          generation: 2,
          target: SIM_ORIGIN.point,
        });
      } finally {
        await store.close();
      }
    });

    it("returns the origin when the reader's generation is above the store's", async () => {
      const store = await open(dialect);
      try {
        await twoRewinds(store);
        expect(await store.rewindsSince(3)).toEqual({
          generation: 2,
          target: SIM_ORIGIN.point,
        });
      } finally {
        await store.close();
      }
    });
  },
);
