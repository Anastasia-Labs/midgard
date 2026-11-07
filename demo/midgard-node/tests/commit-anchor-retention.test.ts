/**
 * The commit anchor against the follower's real prune (plan §8.1, §11): a
 * Postgres follower store opened with the node's retained depth
 * (`followerRetainedDepth`) keeps the anchor block of a commit planned at
 * view P while the cursor runs up to k blocks past P, at d = k, where the
 * store's default depth (k) prunes it one block past P.
 */
import {
  type FactStore,
  openPostgresFactStore,
  projectionStoreOptions,
  type TrackedSet,
  type View,
} from "@al-ft/midgard-l1-follower";
import { SqlClient } from "@effect/sql";
import { PgClient } from "@effect/sql-pg";
import { Effect, Redacted } from "effect";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import {
  commitAnchorHeight,
  followerRetainedDepth,
  readCommitAnchorBlock,
} from "../src/database/commit-anchor.js";
import { ChainDriver, testDatabases } from "./helpers/l1-events-store.js";

const K = 4;
const D = K;
/** Blocks the chain grows above the origin before the commit's planning view. */
const PLANNING_BLOCKS = 3 * K;

const EMPTY: TrackedSet = {
  addresses: new Set(),
  paymentCredentials: new Set(),
  policies: new Set(),
};

const databases = testDatabases();
const opened: FactStore[] = [];
afterEach(async () => {
  await Promise.all(opened.splice(0).map((store) => store.close()));
});
afterAll(async () => {
  await databases.dropAll();
});

/** A started store with the node's retained depth, on a fresh database. */
const openNodeStore = async () => {
  const url = await databases.create();
  const store = openPostgresFactStore({
    ...projectionStoreOptions(
      [],
      {
        securityParameter: K,
        retainedDepth: followerRetainedDepth(K, D),
        trackedSet: EMPTY,
      },
      "postgres",
    ),
    connection: { connectionString: url },
  });
  opened.push(store);
  const started = await store.start();
  if (started.kind !== "ready") throw new Error(`store start: ${started.kind}`);
  const driver = new ChainDriver(store, EMPTY);
  await driver.init();
  return { store, driver, url };
};

const pruneAll = async (store: FactStore) => {
  for (let step = 0; step < 100; step += 1) {
    const pruned = await store.prune();
    if (!("done" in pruned)) throw new Error(`prune: ${pruned.kind}`);
    if (pruned.done) return;
  }
  throw new Error("prune did not finish");
};

const readAnchor = (url: string, view: View) =>
  Effect.runPromise(
    Effect.provide(
      readCommitAnchorBlock({ view, depth: D }),
      PgClient.layer({ url: Redacted.make(url), maxConnections: 1 }),
    ),
  );

const blockCount = (url: string) =>
  Effect.runPromise(
    Effect.provide(
      Effect.flatMap(
        SqlClient.SqlClient,
        (sql) => sql<{ n: string }>`SELECT count(*)::text AS n FROM l1_blocks`,
      ),
      PgClient.layer({ url: Redacted.make(url), maxConnections: 1 }),
    ),
  ).then((rows) => Number(rows[0]?.n));

describe("commit anchor inside the node follower's retained window (d = k)", () => {
  it.each([1, K])(
    "reads the anchor of a view the cursor passed by %i block(s) after a full prune",
    async (lag) => {
      const { store, driver, url } = await openNodeStore();
      for (let i = 0; i < PLANNING_BLOCKS; i += 1) await driver.forward([]);
      const view: View = (await store.currentView())!;
      for (let i = 0; i < lag; i += 1) await driver.forward([]);
      expect((await store.cursor())!.height - view.height).toBe(lag);
      const before = await blockCount(url);
      await pruneAll(store);
      // The prune ran: blocks below the window are gone.
      expect(await blockCount(url)).toBeLessThan(before);
      const anchorHeight = commitAnchorHeight(view.height, D);
      expect(anchorHeight).toBe(view.height - D);
      const read = await readAnchor(url, view);
      expect(read.kind === "anchored" ? read.anchor.height : read.reason).toBe(
        anchorHeight,
      );
    },
  );

  it("loses the anchor once the cursor is more than k blocks past the view", async () => {
    const { store, driver, url } = await openNodeStore();
    for (let i = 0; i < PLANNING_BLOCKS; i += 1) await driver.forward([]);
    const view: View = (await store.currentView())!;
    for (let i = 0; i < K + 1; i += 1) await driver.forward([]);
    await pruneAll(store);
    const read = await readAnchor(url, view);
    expect(read).toMatchObject({
      kind: "unavailable",
      reason: "outside_history",
    });
  });
});
