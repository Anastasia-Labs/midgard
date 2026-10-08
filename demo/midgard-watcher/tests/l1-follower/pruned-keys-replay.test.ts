/**
 * The pruned-key record hooks are `record` prune hooks: they run in every
 * prune step, in the transaction of the D-t deletes they record, while the
 * tracked-set record's `replaying` mark is set as well as when it is not.
 * Two cases, in both dialects:
 *
 * - with the mark set, a prune step past k records the followed unit and
 *   the removed header whose closed history rows it deletes, and the
 *   unpinned read of the unit minted again takes `beyond_retention`;
 * - the upgrade order: a store migrated without 0009 (no record tables, no
 *   record hooks) and without a tracked-set record prunes past k; the next
 *   start migrates 0009 (empty record tables) and resets the store as
 *   `unrecorded`, which sets the mark; the replay from the origin prunes
 *   past k with the mark set throughout, and the record then holds every
 *   key a store that never reset records over the same chain.
 */
import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  decodeBlock,
  type FactStore,
  type FactStoreOptions,
  openSqliteFactStore,
} from "@al-ft/midgard-l1-follower";
import {
  SIM_ORIGIN,
  SimChain,
  simStoreOptions,
  type SimTx,
  simUniverse,
} from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import {
  watcherProjection,
  watcherUnitHistoryPolicies,
} from "../../src/l1-follower/projection.js";
import { createFollowerRawReads } from "../../src/l1-follower/raw-reads.js";
import { rawPointOf } from "../../src/l1-follower/raw-reads.types.js";
import {
  WATCHER_PRUNED_HEADERS_TABLE,
  WATCHER_PRUNED_UNITS_TABLE,
  WATCHER_QUEUE_UNIT_HISTORY_TABLE,
  WATCHER_UNIT_HISTORY_TABLE,
} from "../../src/l1-follower/tables.js";
import { K, reasonOf } from "../support/l1-follower-raw-reads-fixture.js";
import { removeTailTx } from "../support/l1-follower-state-queue-removal.js";
import {
  commitTx,
  initTx,
  queueState,
  scriptAddress,
} from "../support/l1-follower-state-queue-traffic.js";
import { postgresSchemas } from "../support/postgres-schemas.js";
import {
  closeRemovedHeaders,
  FOLLOWED_UNIT,
  FOLLOWING,
  removedHeader,
} from "../support/proof-retention-removed-header.js";

afterEach(closeRemovedHeaders);
const schemas = postgresSchemas();
const scratch = mkdtempSync(join(tmpdir(), "watcher-pruned-keys-replay-"));
afterAll(async () => {
  await schemas.dropAll();
  rmSync(scratch, { recursive: true, force: true });
});

const FOLLOWED = FOLLOWED_UNIT.slice(0, 56);
const one = (quantity: bigint) =>
  new Map([[FOLLOWED, new Map([["aa", quantity]])]]);

const setReplayMark = (store: FactStore, replaying: boolean) =>
  store.transaction("write", (tx) =>
    tx.query(
      "UPDATE l1_follower_tracked_set SET replaying = ? WHERE id = 1 RETURNING id",
      [replaying ? 1 : 0],
    ),
  );

const keysIn = async (
  store: FactStore,
  table: string,
  column: string,
): Promise<string[]> =>
  (
    await store.transaction("read", (tx) =>
      tx.query(`SELECT ${column} AS k FROM ${table}`),
    )
  )
    .map((row) => Buffer.from(row.k as Uint8Array).toString("hex"))
    .sort();

const rowsOf = async (
  store: FactStore,
  table: string,
  column: string,
  key: string,
): Promise<number> =>
  Number(
    (
      await store.transaction("read", (tx) =>
        tx.query(`SELECT COUNT(*) AS n FROM ${table} WHERE ${column} = ?`, [
          Buffer.from(key, "hex"),
        ]),
      )
    )[0]!.n,
  );

const records = async (store: FactStore) => ({
  units: await keysIn(store, WATCHER_PRUNED_UNITS_TABLE, "unit"),
  headers: await keysIn(store, WATCHER_PRUNED_HEADERS_TABLE, "header_hash"),
});

const pruneAll = async (store: FactStore): Promise<void> => {
  for (;;) {
    const pruned = await store.prune(1_000);
    if ("kind" in pruned) throw new Error(`prune: ${pruned.kind}`);
    if (pruned.done) return;
  }
};

/** The watcher migrations up to (not including) the pruned-key record. */
const PRE_RECORD_MIGRATIONS = new Set([
  "0009_watcher_pruned_keys",
  "0010_watcher_follower_generation",
]);
const beforeRecord = (options: FactStoreOptions): FactStoreOptions => ({
  ...options,
  migrations: (options.migrations ?? []).map((set) => ({
    ...set,
    migrations: set.migrations.filter(
      ({ id }) => !PRE_RECORD_MIGRATIONS.has(id),
    ),
  })),
  pruneHooks: [],
});

/**
 * The protocol init, a header commit, a followed unit minted and burned, the
 * header removed, then K + 4 empty blocks: the unit's and the header's
 * history rows all close and go k deep. Returns each block's raw bytes and
 * the header.
 */
const closedHistories = () => {
  const chain = new SimChain(simUniverse(), SIM_ORIGIN);
  const blocks: Buffer[] = [];
  const forward = (txs: readonly SimTx[]) => {
    const { encoded } = chain.forward(txs);
    blocks.push(encoded.raw);
    return encoded.txHashes;
  };
  forward([initTx(FOLLOWING)]);
  const commit = commitTx(queueState(chain, FOLLOWING)!, FOLLOWING);
  const header = [
    ...(commit.mint?.get(FOLLOWING.stateQueueMint)?.keys() ?? []),
  ][0]!.slice(SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX.length);
  forward([commit]);
  const [minted] = forward([
    {
      inputs: [chain.outsideInput()],
      outputs: [
        {
          address: scriptAddress(FOLLOWED),
          lovelace: 2_000_000n,
          assets: one(1n),
        },
      ],
      mint: one(1n),
      nonce: chain.nonce(),
    },
  ]);
  forward([
    {
      inputs: [{ txHash: minted!, index: 0 }],
      outputs: [
        { address: simUniverse().untrackedAddress, lovelace: 2_000_000n },
      ],
      mint: one(-1n),
      nonce: chain.nonce(),
    },
  ]);
  forward([removeTailTx(queueState(chain, FOLLOWING)!, FOLLOWING)]);
  for (let i = 0; i < K + 4; i += 1) forward([]);
  const remint = () =>
    chain.forward([
      {
        inputs: [chain.outsideInput()],
        outputs: [
          {
            address: scriptAddress(FOLLOWED),
            lovelace: 2_000_000n,
            assets: one(1n),
          },
        ],
        mint: one(1n),
        nonce: chain.nonce(),
      },
    ]).encoded.raw;
  return { blocks, header, remint };
};

/** Applies each block and prunes everything prunable after it, as the loop does. */
const follow = async (store: FactStore, blocks: readonly Buffer[]) => {
  for (const raw of blocks) {
    expect((await store.applyBlock(decodeBlock(raw))).kind).toBe("applied");
    await pruneAll(store);
  }
};

const unpinnedUnitRead = async (store: FactStore) => {
  const cursor = (await store.cursor())!;
  return createFollowerRawReads(store, {
    stateQueuePolicyId: FOLLOWING.stateQueueMint,
    unitHistoryPolicies: watcherUnitHistoryPolicies(FOLLOWING),
  }).unitHistoryAtPoint(
    FOLLOWED_UNIT,
    rawPointOf({
      slot: cursor.point.slot,
      hash: cursor.point.hash,
      height: cursor.height,
    }),
  );
};

describe.each(["sqlite", "postgres"] as const)(
  "pruned-key record hooks while a store reset replays (%s)",
  (dialect) => {
    const opener = async (): Promise<
      (options: FactStoreOptions) => FactStore
    > => {
      if (dialect === "postgres") return (await schemas.open()).store;
      const path = join(scratch, `${String(Math.random()).slice(2)}.db`);
      return (options) => openSqliteFactStore({ ...options, path });
    };

    it("with the replay mark set, a prune step past k records the keys whose closed history rows it deletes, and the unpinned read of the unit minted again is beyond_retention", async () => {
      const r = await removedHeader({
        pin: false,
        resolveAtIngest: true,
        followUnit: true,
        ...(dialect === "postgres" ? { open: await schemas.open() } : {}),
      });
      expect(r.h.store.dialect.name).toBe(dialect);
      expect(await setReplayMark(r.h.store, true)).toHaveLength(1);
      expect(await r.h.store.trackedSetRecord()).toMatchObject({
        replaying: true,
      });
      expect(
        await r.count(WATCHER_UNIT_HISTORY_TABLE, "unit", FOLLOWED_UNIT),
      ).toBe(2);
      expect(
        await r.count(
          WATCHER_QUEUE_UNIT_HISTORY_TABLE,
          "header_hash",
          r.header,
        ),
      ).toBeGreaterThan(0);
      await r.passK();
      // The D-t deletes ran, and so did the record hooks beside them.
      expect(
        await r.count(WATCHER_UNIT_HISTORY_TABLE, "unit", FOLLOWED_UNIT),
      ).toBe(0);
      expect(
        await r.count(
          WATCHER_QUEUE_UNIT_HISTORY_TABLE,
          "header_hash",
          r.header,
        ),
      ).toBe(0);
      expect(
        await r.count(WATCHER_PRUNED_UNITS_TABLE, "unit", FOLLOWED_UNIT),
      ).toBe(1);
      expect(
        await r.count(WATCHER_PRUNED_HEADERS_TABLE, "header_hash", r.header),
      ).toBe(1);
      await r.remint();
      expect(
        reasonOf(
          await r.h.reads().unitHistoryAtPoint(FOLLOWED_UNIT, r.h.tipPoint()),
        ),
      ).toBe("beyond_retention");
      // The mark held throughout.
      expect(await r.h.store.trackedSetRecord()).toMatchObject({
        replaying: true,
      });
    });

    it("upgrade order: 0009 starts empty, the unrecorded reset sets the mark, and the replay past k records every key a store that never reset records", async () => {
      const { blocks, header, remint } = closedHistories();
      const open = await opener();
      const options = simStoreOptions(
        [watcherProjection(FOLLOWING)],
        K,
        dialect,
      );

      // What a store that never reset records over the same chain.
      const reference = (await opener())(options);
      let expected: Awaited<ReturnType<typeof records>>;
      try {
        expect((await reference.start()).kind).toBe("ready");
        expect((await reference.initialize(SIM_ORIGIN)).kind).toBe(
          "initialized",
        );
        await follow(reference, blocks);
        expected = await records(reference);
      } finally {
        await reference.close();
      }
      // Not vacuous: both histories were pruned and recorded.
      expect(expected.units).toContain(FOLLOWED_UNIT);
      expect(expected.headers).toContain(header);

      // Before the upgrade: no record tables, no record hooks, no
      // tracked-set record; the prune past k deletes the closed rows.
      const before = open(beforeRecord(options));
      try {
        expect((await before.start()).kind).toBe("ready");
        expect((await before.initialize(SIM_ORIGIN)).kind).toBe("initialized");
        await follow(before, blocks);
        expect(
          await rowsOf(
            before,
            WATCHER_UNIT_HISTORY_TABLE,
            "unit",
            FOLLOWED_UNIT,
          ),
        ).toBe(0);
        await before.transaction("write", (tx) =>
          tx.query("DELETE FROM l1_follower_tracked_set"),
        );
      } finally {
        await before.close();
      }

      // The first upgraded start: 0009 and 0010 migrate, the record tables
      // are empty, and the unrecorded store resets with the mark set.
      const store = open(options);
      try {
        const started = await store.start();
        expect(started).toMatchObject({
          kind: "ready",
          cursor: null,
          trackedSet: { kind: "reset", cause: "unrecorded" },
          replaying: true,
        });
        expect("migrated" in started ? started.migrated : []).toEqual(
          expect.arrayContaining([
            expect.stringContaining("0009_watcher_pruned_keys"),
          ]),
        );
        expect(await records(store)).toEqual({ units: [], headers: [] });
        expect((await store.initialize(SIM_ORIGIN)).kind).toBe("initialized");
        await follow(store, blocks);
        // The replay ran with the mark set throughout.
        expect(await store.trackedSetRecord()).toMatchObject({
          replaying: true,
        });
        expect(await records(store)).toEqual(expected);
        expect((await store.applyBlock(decodeBlock(remint()))).kind).toBe(
          "applied",
        );
        expect(reasonOf(await unpinnedUnitRead(store))).toBe(
          "beyond_retention",
        );
      } finally {
        await store.close();
      }
    });
  },
);
