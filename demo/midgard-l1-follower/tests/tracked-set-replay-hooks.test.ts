/**
 * A store reset replays from the origin with the `replaying` flag set
 * (`tracked-set-record.ts`): a tracked-set reset at start and a manual
 * `reset --to-origin` alike. While the flag is set, prune runs no projection
 * prune hooks (the follower's own class A retention goes on), the flag clears
 * only once the cursor is back at the height it held before the reset, and
 * the reset leaves its own `l1_rollbacks` row for readers that missed the
 * notification.
 */
import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterAll, describe, expect, it } from "vitest";

import {
  decodeBlock,
  deriveIntentStatusesIn,
  type DialectName,
  type FactStore,
  type FactStoreOptions,
  type FollowerProjection,
  intentJournalProjection,
  openPostgresBackend,
  openPostgresFactStore,
  openSqliteBackend,
  openSqliteFactStore,
  type OutRef,
  projectionStoreOptions,
  resetToOrigin,
  type SqlBackend,
  type TrackedSet,
  trackedSetItems,
} from "../src/index.js";
import { asNumber } from "../src/sql/backend.js";
import {
  encodeSimTx,
  SIM_ORIGIN,
  SimChain,
  type SimTx,
  simTxHash,
  simUniverse,
} from "../src/testing/index.js";
import { testDatabases } from "./support/postgres.js";
import { recordAtCurrentView } from "./support/record-at-view.js";

const databases = testDatabases();
const scratch = mkdtempSync(join(tmpdir(), "l1-follower-replay-hooks-"));

afterAll(async () => {
  await databases.dropAll();
  rmSync(scratch, { recursive: true, force: true });
});

const K = 3;
const u = simUniverse();

type Location = Readonly<{
  store: (trackedSet?: TrackedSet) => FactStore;
  backend: () => SqlBackend;
}>;

const adapters: readonly Readonly<{
  name: DialectName;
  create: () => Promise<Location>;
}>[] = [
  {
    name: "sqlite",
    create: async () => {
      const path = join(scratch, `${String(Math.random()).slice(2)}.db`);
      return {
        store: (trackedSet = u.tracked) =>
          openSqliteFactStore({
            ...projectionStoreOptions(
              [intentJournalProjection],
              { securityParameter: K, trackedSet },
              "sqlite",
            ),
            path,
          }),
        backend: () => openSqliteBackend(path),
      };
    },
  },
  {
    name: "postgres",
    create: async () => {
      const { url } = await databases.create();
      return {
        store: (trackedSet = u.tracked) =>
          openPostgresFactStore({
            ...projectionStoreOptions(
              [intentJournalProjection],
              { securityParameter: K, trackedSet },
              "postgres",
            ),
            connection: { connectionString: url },
          }),
        backend: () => openPostgresBackend({ connectionString: url }),
      };
    },
  },
];

const short = (hash: Buffer): string => hash.toString("hex").slice(0, 8);

const journal = async (store: FactStore): Promise<string[]> =>
  (
    await store.transaction("read", (tx) =>
      deriveIntentStatusesIn(tx, store.dialect),
    )
  ).states.map((state) => `${short(state.intent.txHash)}:${state.status.kind}`);

const journaled = async (store: FactStore): Promise<string[]> =>
  (
    await store.transaction("read", (tx) =>
      deriveIntentStatusesIn(tx, store.dialect),
    )
  ).states.map((state) => short(state.intent.txHash));

const rollbackRows = (store: FactStore) =>
  store.transaction("read", async (tx) =>
    (
      await tx.query(
        "SELECT generation, from_slot, to_slot, to_hash, depth_blocks FROM l1_rollbacks ORDER BY generation",
      )
    ).map((row) => ({
      generation: asNumber(row.generation),
      fromSlot: asNumber(row.from_slot),
      toSlot: asNumber(row.to_slot),
      toHash: Buffer.from(row.to_hash as Uint8Array).toString("hex"),
      depth: asNumber(row.depth_blocks),
    })),
  );

/**
 * A deep origin (more than k empty blocks), then an own-intent chain X -> Y:
 * X lands and is pruned k deep; Y, spending X's output, is journaled and
 * live. Returns the raw blocks, the expected journal and the cursor height;
 * the store is closed.
 */
const followed = async (location: Location) => {
  const chain = new SimChain(u, SIM_ORIGIN);
  const blocks: Buffer[] = [];
  const store = location.store();
  try {
    expect((await store.start()).kind).toBe("ready");
    expect((await store.initialize(SIM_ORIGIN)).kind).toBe("initialized");
    const forward = async (txs: readonly SimTx[] = []) => {
      const { encoded } = chain.forward(txs);
      blocks.push(encoded.raw);
      expect((await store.applyBlock(decodeBlock(encoded.raw))).kind).toBe(
        "applied",
      );
    };
    const record = async (tx: SimTx) =>
      expect(
        (
          await store.transaction("write", (sql) =>
            recordAtCurrentView(sql, store.dialect, {
              family: "commit",
              workflowKey: `commit:${simTxHash(tx).toString("hex")}`,
              txCbor: encodeSimTx(tx),
              isOwnOutput: (output) => output.address.equals(u.trackedAddress),
            }),
          )
        ).kind,
      ).toBe("recorded");
    for (let i = 0; i < K + 3; i += 1) await forward();
    const funding: SimTx = {
      inputs: [chain.outsideInput()],
      outputs: [{ address: u.trackedAddress, lovelace: 10_000_000n }],
      nonce: chain.nonce(),
    };
    await forward([funding]);
    const x: SimTx = {
      inputs: [{ txHash: simTxHash(funding), index: 0 }],
      outputs: [{ address: u.trackedAddress, lovelace: 9_000_000n }],
      nonce: chain.nonce(),
    };
    await record(x);
    await forward([x]);
    const xOut: OutRef = { txHash: simTxHash(x), index: 0 };
    const y: SimTx = {
      inputs: [xOut],
      outputs: [{ address: u.trackedAddress, lovelace: 8_000_000n }],
      nonce: chain.nonce(),
    };
    await record(y);
    for (let i = 0; i < K + 2; i += 1) await forward();
    expect(await store.prune()).toMatchObject({ done: true });
    const expected = [`${short(simTxHash(y))}:live`];
    // X landed and its entry was pruned k deep; Y is live.
    expect(await journal(store)).toEqual(expected);
    const cursor = await store.cursor();
    if (cursor === null) throw new Error("unreachable");
    return { blocks, expected, cursor };
  } finally {
    await store.close();
  }
};

const dropRecord = async (location: Location) => {
  const store = location.store();
  try {
    expect((await store.start()).kind).toBe("ready");
    await store.transaction("write", (tx) =>
      tx.query("DELETE FROM l1_follower_tracked_set"),
    );
  } finally {
    await store.close();
  }
};

const manualReset = async (location: Location) => {
  const backend = location.backend();
  try {
    expect(await resetToOrigin(backend)).toMatchObject({
      kind: "reset",
      nextGeneration: 1,
    });
  } finally {
    await backend.close();
  }
};

describe.each(adapters)("replay after a store reset ($name)", (adapter) => {
  it.each([
    ["a tracked-set reset at start", dropRecord],
    ["a manual reset --to-origin", manualReset],
    [
      "a manual reset --to-origin of a store with no tracked-set record",
      async (location: Location) => {
        await dropRecord(location);
        await manualReset(location);
      },
    ],
  ] as const)(
    "%s marks the replay, runs no retention prune hook until the cursor is back at its previous height, and keeps a journaled own intent whose input lands mid-replay",
    async (_, reset) => {
      const location = await adapter.create();
      const { blocks, expected, cursor: before } = await followed(location);
      const intents = expected.map((entry) => entry.split(":")[0]);
      await reset(location);
      const store = location.store();
      try {
        expect(await store.start()).toMatchObject({
          kind: "ready",
          cursor: null,
          replaying: true,
        });
        expect(await store.trackedSetRecord()).toMatchObject({
          replaying: true,
          replayHeight: before.height,
        });
        // The reset is in the rollback log: from the old cursor to the origin.
        expect(await rollbackRows(store)).toEqual([
          {
            generation: 1,
            fromSlot: before.point.slot,
            toSlot: SIM_ORIGIN.point.slot,
            toHash: SIM_ORIGIN.point.hash.toString("hex"),
            depth: before.height - SIM_ORIGIN.height,
          },
        ]);
        expect((await store.initialize(SIM_ORIGIN)).kind).toBe("initialized");
        // The record holds the configured set, and the mark stays.
        expect(await store.trackedSetRecord()).toEqual({
          trackedSet: trackedSetItems(u.tracked),
          replaying: true,
          replayHeight: before.height,
        });
        expect(await store.rewindsSince(0)).toEqual({
          generation: 1,
          target: SIM_ORIGIN.point,
        });
        expect(await store.rewindsSince(1)).toEqual({
          generation: 1,
          target: null,
        });
        for (const [index, raw] of blocks.entries()) {
          expect((await store.applyBlock(decodeBlock(raw))).kind).toBe(
            "applied",
          );
          const pruned = await store.prune();
          expect(pruned).toMatchObject({ done: true });
          // The class A retention goes on while replaying.
          expect(
            "kind" in pruned ? null : pruned.prunedThroughSlot,
          ).toBeGreaterThanOrEqual(SIM_ORIGIN.point.slot);
          // Every journaled intent is kept; its derived status may read the
          // replay's incomplete facts until the replay passes them.
          expect(await journaled(store)).toEqual(intents);
          if (index === blocks.length - 2)
            // At a node tip below the previous height the replay goes on.
            expect(await store.endTrackedSetReplay()).toBe(
              "below_replay_height",
            );
        }
        expect((await store.cursor())?.height).toBe(before.height);
        expect(await journal(store)).toEqual(expected);
        expect(await store.trackedSetRecord()).toMatchObject({
          replaying: true,
        });
        expect(await store.endTrackedSetReplay()).toBe("ended");
        expect(await store.endTrackedSetReplay()).toBe("not_replaying");
        expect(await store.trackedSetRecord()).toMatchObject({
          replaying: false,
          replayHeight: null,
        });
        // With the flag cleared the hooks run again over complete facts.
        expect(await store.prune()).toMatchObject({ done: true });
        expect(await journal(store)).toEqual(expected);
      } finally {
        await store.close();
      }
    },
  );
});

describe.each(adapters)("a second reset during a replay ($name)", (adapter) => {
  it("keeps the higher replay height: the one the first reset recorded, above the replay's cursor", async () => {
    const location = await adapter.create();
    const { blocks, cursor: before } = await followed(location);
    await manualReset(location);
    const replay = location.store();
    try {
      expect((await replay.start()).kind).toBe("ready");
      expect((await replay.initialize(SIM_ORIGIN)).kind).toBe("initialized");
      for (const raw of blocks.slice(0, 3))
        expect((await replay.applyBlock(decodeBlock(raw))).kind).toBe(
          "applied",
        );
      expect((await replay.cursor())?.height).toBeLessThan(before.height);
    } finally {
      await replay.close();
    }
    const backend = location.backend();
    try {
      expect(await resetToOrigin(backend)).toMatchObject({ kind: "reset" });
    } finally {
      await backend.close();
    }
    const store = location.store();
    try {
      expect(await store.start()).toMatchObject({ replaying: true });
      expect(await store.trackedSetRecord()).toMatchObject({
        replaying: true,
        replayHeight: before.height,
      });
    } finally {
      await store.close();
    }
  });
});

describe.each(adapters)("the configured tracked set ($name)", (adapter) => {
  it("an item configured in uppercase hex qualifies like its lowercase form, and the record holds it lowercase", async () => {
    const upper = (values: ReadonlySet<string>) =>
      new Set([...values].map((value) => value.toUpperCase()));
    const shouting: TrackedSet = {
      addresses: upper(u.tracked.addresses),
      paymentCredentials: upper(u.tracked.paymentCredentials),
      policies: upper(u.tracked.policies),
    };
    // Not vacuous: the items carry hex letters.
    expect([...shouting.addresses][0]).not.toBe([...u.tracked.addresses][0]);
    const location = await adapter.create();
    const chain = new SimChain(u, SIM_ORIGIN);
    const tx: SimTx = {
      inputs: [chain.outsideInput()],
      outputs: [
        { address: u.trackedAddress, lovelace: 3_000_000n },
        { address: u.credentialAddress, lovelace: 4_000_000n },
        { address: u.untrackedAddress, lovelace: 5_000_000n },
      ],
      nonce: chain.nonce(),
    };
    const { encoded } = chain.forward([tx]);
    const store = location.store(shouting);
    try {
      expect((await store.start()).kind).toBe("ready");
      expect((await store.initialize(SIM_ORIGIN)).kind).toBe("initialized");
      expect((await store.applyBlock(decodeBlock(encoded.raw))).kind).toBe(
        "applied",
      );
      const live = (index: number) =>
        store.isTrackedLive({ txHash: simTxHash(tx), index });
      expect([live(0), live(1), live(2)]).toEqual([true, true, false]);
      expect(await store.trackedSetRecord()).toMatchObject({
        trackedSet: trackedSetItems(u.tracked),
      });
    } finally {
      await store.close();
    }
    const lower = location.store();
    try {
      expect(await lower.start()).toMatchObject({
        trackedSet: { kind: "equal" },
      });
    } finally {
      await lower.close();
    }
  });
});

describe.each(adapters)("prune hooks by kind ($name)", (adapter) => {
  it("runs a record hook in every prune step and a retention hook only while the store is not replaying", async () => {
    const calls: string[] = [];
    const spies: FollowerProjection = {
      name: "spies",
      pruneHooks: [
        {
          kind: "retention",
          table: "spy_retention",
          apply: ({ boundarySlot }) => {
            calls.push(`retention@${boundarySlot.toString()}`);
            return Promise.resolve(0);
          },
        },
        {
          kind: "record",
          table: "spy_record",
          apply: ({ boundarySlot }) => {
            calls.push(`record@${boundarySlot.toString()}`);
            return Promise.resolve();
          },
        },
      ],
    };
    const options = projectionStoreOptions(
      [intentJournalProjection, spies],
      { securityParameter: K, trackedSet: u.tracked },
      adapter.name,
    );
    const store =
      adapter.name === "sqlite"
        ? openSqliteFactStore({
            ...options,
            path: join(scratch, `${String(Math.random()).slice(2)}.db`),
          })
        : openPostgresFactStore({
            ...(options as FactStoreOptions),
            connection: { connectionString: (await databases.create()).url },
          });
    try {
      expect((await store.start()).kind).toBe("ready");
      expect((await store.initialize(SIM_ORIGIN)).kind).toBe("initialized");
      const chain = new SimChain(u, SIM_ORIGIN);
      for (let i = 0; i < K + 2; i += 1)
        expect(
          (await store.applyBlock(decodeBlock(chain.forward([]).encoded.raw)))
            .kind,
        ).toBe("applied");
      const mark = (replaying: boolean) =>
        store.transaction("write", (tx) =>
          tx.query("UPDATE l1_follower_tracked_set SET replaying = ?", [
            replaying ? 1 : 0,
          ]),
        );
      const boundary = async () => {
        const pruned = await store.prune();
        if ("kind" in pruned) throw new Error(`prune: ${pruned.kind}`);
        return pruned.prunedThroughSlot.toString();
      };

      await mark(true);
      const replayed = await boundary();
      expect(calls.splice(0)).toEqual([`record@${replayed}`]);

      await mark(false);
      const settled = await boundary();
      expect(calls.splice(0)).toEqual([
        `retention@${settled}`,
        `record@${settled}`,
      ]);
    } finally {
      await store.close();
    }
  });
});
