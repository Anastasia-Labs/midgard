import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterAll, describe, expect, it } from "vitest";

import {
  createWalletSeeder,
  type DialectName,
  type FactStore,
  type FactStoreOptions,
  type GenerationListener,
  openPostgresFactStore,
  openSqliteFactStore,
  type TrackedSet,
  trackedSetItems,
} from "../src/index.js";
import { asNumber } from "../src/sql/backend.js";
import {
  diffDumps,
  dumpStore,
  encodeUtxoAnswer,
} from "../src/testing/index.js";
import { testDatabases } from "./support/postgres.js";
import { roleOptions } from "./support/reset-stores.js";
import {
  chain,
  fill,
  ORIGIN,
  point,
  POLICY,
  UNTRACKED,
} from "./support/small-chain.js";

const databases = testDatabases();
const scratch = mkdtempSync(join(tmpdir(), "l1-follower-tracked-set-"));

afterAll(async () => {
  await databases.dropAll();
  rmSync(scratch, { recursive: true, force: true });
});

/** The protocol set `roleOptions` configures: TRACKED and POLICY. */
const BASE: TrackedSet = roleOptions("sqlite").trackedSet;
/** An own wallet (enterprise key address); never part of the record. */
const WALLET = Buffer.concat([Buffer.of(0x60), fill(0x44, 28)]);

const plus = (
  set: TrackedSet,
  add: Partial<Record<keyof TrackedSet, string>>,
): TrackedSet => ({
  addresses: new Set([
    ...set.addresses,
    ...(add.addresses === undefined ? [] : [add.addresses]),
  ]),
  paymentCredentials: new Set([
    ...set.paymentCredentials,
    ...(add.paymentCredentials === undefined ? [] : [add.paymentCredentials]),
  ]),
  policies: new Set([
    ...set.policies,
    ...(add.policies === undefined ? [] : [add.policies]),
  ]),
});

type Opener = (
  config?: Readonly<{ trackedSet?: TrackedSet; wallets?: Buffer[] }>,
) => FactStore;

/** One database, opened as often as a test likes under any tracked set. */
const adapters: readonly Readonly<{
  name: DialectName;
  database: () => Promise<Opener>;
}>[] = [
  {
    name: "sqlite",
    database: async () => {
      const path = join(scratch, `${String(Math.random()).slice(2)}.db`);
      return (config = {}) =>
        openSqliteFactStore({ ...configured("sqlite", config), path });
    },
  },
  {
    name: "postgres",
    database: async () => {
      const { url } = await databases.create();
      return (config = {}) =>
        openPostgresFactStore({
          ...configured("postgres", config),
          connection: { connectionString: url },
        });
    },
  },
];

const configured = (
  dialect: DialectName,
  config: Readonly<{ trackedSet?: TrackedSet; wallets?: Buffer[] }>,
): FactStoreOptions => ({
  ...roleOptions(dialect),
  ...(config.trackedSet === undefined ? {} : { trackedSet: config.trackedSet }),
  ...(config.wallets === undefined ? {} : { wallets: config.wallets }),
});

const B3 = point(chain()[2]);

/** Initializes `store` at ORIGIN and applies the small chain. */
const follow = async (store: FactStore): Promise<void> => {
  expect(await store.initialize(ORIGIN)).toMatchObject({
    kind: "initialized",
  });
  for (const block of chain())
    expect(await store.applyBlock(block)).toMatchObject({ kind: "applied" });
};

/** A store that followed the chain, with one class B and one class C role row; closed. */
const followed = async (open: Opener, trackedSet = BASE): Promise<void> => {
  const store = open({ trackedSet });
  try {
    expect(await store.start()).toMatchObject({
      kind: "ready",
      trackedSet: { kind: "unchecked" },
    });
    await follow(store);
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
  } finally {
    await store.close();
  }
};

const rows = async (store: FactStore, sql: string): Promise<string[]> =>
  store.transaction("read", async (tx) =>
    (await tx.query(sql)).map((row) =>
      Object.values(row)
        .map((value) =>
          value instanceof Uint8Array
            ? Buffer.from(value).toString("hex")
            : String(value),
        )
        .join("|"),
    ),
  );

const roleRows = async (store: FactStore) => ({
  signed: await rows(store, "SELECT id, body FROM role_signed ORDER BY id"),
  content: await rows(
    store,
    "SELECT content_hash, bytes FROM role_content ORDER BY content_hash",
  ),
});

const liveOutRefs = (store: FactStore) =>
  rows(
    store,
    "SELECT tx_hash, output_index FROM l1_outputs WHERE spent_slot IS NULL ORDER BY tx_hash, output_index",
  );

const generation = async (store: FactStore): Promise<number | undefined> =>
  (await store.cursor())?.generation;

const writerGeneration = (store: FactStore): Promise<number> =>
  store.transaction("read", async (tx) =>
    asNumber(
      (await tx.query("SELECT next_generation FROM l1_follower_writer"))[0]
        ?.next_generation,
    ),
  );

/** The store's dump after following the chain from scratch under `trackedSet`. */
const freshDump = async (open: Opener, trackedSet: TrackedSet) => {
  const store = open({ trackedSet });
  try {
    expect((await store.start()).kind).toBe("ready");
    await follow(store);
    return await dumpStore(store);
  } finally {
    await store.close();
  }
};

const hex = (bytes: Buffer): string => bytes.toString("hex");

describe.each(adapters)("the tracked-set record ($name)", (adapter) => {
  it("records the protocol set at initialize, never the wallets", async () => {
    const open = await adapter.database();
    const store = open({ wallets: [WALLET] });
    try {
      expect(await store.start()).toMatchObject({
        kind: "ready",
        cursor: null,
        trackedSet: { kind: "unchecked" },
        replaying: false,
      });
      expect(await store.trackedSetRecord()).toBeNull();
      await follow(store);
      expect(await store.trackedSetRecord()).toEqual({
        trackedSet: trackedSetItems(BASE),
        replaying: false,
      });
      // The wallet is tracked all the same.
      expect(store.trackedSet().addresses.has(hex(WALLET))).toBe(true);
    } finally {
      await store.close();
    }
  });

  it("an equal set proceeds; a removal rewrites the record and keeps every row", async () => {
    const open = await adapter.database();
    await followed(open);
    const reduced: TrackedSet = { ...BASE, policies: new Set() };
    const equal = open();
    let dump;
    try {
      expect(await equal.start()).toMatchObject({
        kind: "ready",
        cursor: { point: B3 },
        trackedSet: { kind: "equal" },
        replaying: false,
      });
      dump = await dumpStore(equal);
    } finally {
      await equal.close();
    }
    const removed = open({ trackedSet: reduced });
    try {
      expect(await removed.start()).toMatchObject({
        kind: "ready",
        cursor: { point: B3, generation: 0 },
        trackedSet: {
          kind: "removed",
          removed: {
            addresses: [],
            paymentCredentials: [],
            policies: [hex(POLICY)],
          },
        },
        replaying: false,
      });
      expect(diffDumps(await dumpStore(removed), dump)).toBeNull();
      expect(await removed.trackedSetRecord()).toEqual({
        trackedSet: trackedSetItems(reduced),
        replaying: false,
      });
    } finally {
      await removed.close();
    }
    const again = open({ trackedSet: reduced });
    try {
      expect(await again.start()).toMatchObject({
        trackedSet: { kind: "equal" },
      });
    } finally {
      await again.close();
    }
  });

  it.each([
    ["an address", { addresses: hex(UNTRACKED) }],
    ["a payment credential", { paymentCredentials: hex(fill(0x22, 28)) }],
    ["a policy", { policies: hex(fill(0x55, 28)) }],
  ] as [string, Partial<Record<keyof TrackedSet, string>>][])(
    "adding %s resets to the origin, keeps B and C, tells the listeners and replays to a fresh store's rows",
    async (_, add) => {
      const open = await adapter.database();
      await followed(open);
      const before = open();
      let previousLive: string[];
      let previousRole;
      try {
        await before.start();
        previousLive = await liveOutRefs(before);
        previousRole = await roleRows(before);
        expect(previousLive.length).toBeGreaterThan(0);
      } finally {
        await before.close();
      }
      const grown = plus(BASE, add);
      const store = open({ trackedSet: grown });
      const heard: Parameters<GenerationListener>[0][] = [];
      store.onGeneration((event) => heard.push(event));
      try {
        const started = await store.start();
        expect(started).toMatchObject({
          kind: "ready",
          cursor: null,
          liveOutRefs: 0,
          replaying: true,
          trackedSet: {
            kind: "reset",
            cause: "added",
            added: {
              addresses: add.addresses === undefined ? [] : [add.addresses],
              paymentCredentials:
                add.paymentCredentials === undefined
                  ? []
                  : [add.paymentCredentials],
              policies: add.policies === undefined ? [] : [add.policies],
            },
            tables: expect.arrayContaining([
              "fixture_block_marks",
              "l1_blocks",
              "l1_outputs",
              "l1_follower_cursor",
            ]) as unknown,
          },
        });
        expect(
          started.kind === "ready" && started.trackedSet.kind === "reset"
            ? started.trackedSet.tables
            : [],
        ).not.toContain("role_signed");
        // To every listener the reset is a rewind from b3 to the origin
        // that deleted every live outref.
        expect(heard).toHaveLength(1);
        const event = heard[0]!;
        expect(event.generation).toBe(1);
        expect(event.rewound).toMatchObject({
          kind: "rewound",
          generation: 1,
          from: B3,
          to: ORIGIN.point,
          depth: 3,
          unspent: [],
        });
        expect(
          event.rewound.deleted
            .map((o) => `${hex(o.txHash)}|${o.index}`)
            .sort(),
        ).toEqual(previousLive);
        expect(await roleRows(store)).toEqual(previousRole);
        expect(await store.trackedSetRecord()).toEqual({
          trackedSet: trackedSetItems(grown),
          replaying: true,
        });
        // The replay from the origin rebuilds what a fresh store holds.
        await follow(store);
        expect(await generation(store)).toBe(1);
        expect(
          diffDumps(
            await dumpStore(store),
            await freshDump(await adapter.database(), grown),
          ),
        ).toBeNull();
        expect(await store.endTrackedSetReplay()).toBe(true);
        expect(await store.endTrackedSetReplay()).toBe(false);
        expect(await store.trackedSetRecord()).toMatchObject({
          replaying: false,
        });
      } finally {
        await store.close();
      }
      const next = open({ trackedSet: grown });
      try {
        expect(await next.start()).toMatchObject({
          trackedSet: { kind: "equal" },
          replaying: false,
          cursor: { point: B3, generation: 1 },
        });
      } finally {
        await next.close();
      }
    },
  );

  it("a store with a cursor and no record resets once, then starts equal", async () => {
    const open = await adapter.database();
    await followed(open);
    const store = open();
    try {
      await store.start();
      await store.transaction("write", (tx) =>
        tx.query("DELETE FROM l1_follower_tracked_set"),
      );
      expect(await store.start()).toMatchObject({
        kind: "ready",
        cursor: null,
        replaying: true,
        trackedSet: {
          kind: "reset",
          cause: "unrecorded",
          added: trackedSetItems(BASE),
        },
      });
      expect(await writerGeneration(store)).toBe(1);
      await follow(store);
      expect(await store.start()).toMatchObject({
        cursor: { point: B3, generation: 1 },
        trackedSet: { kind: "equal" },
        replaying: true,
      });
    } finally {
      await store.close();
    }
  });

  it("own wallets never reset the store", async () => {
    const open = await adapter.database();
    await followed(open);
    const store = open({ wallets: [WALLET, UNTRACKED] });
    try {
      expect(await store.start()).toMatchObject({
        cursor: { point: B3, generation: 0 },
        trackedSet: { kind: "equal" },
        replaying: false,
      });
      expect(store.trackedSet().addresses.has(hex(UNTRACKED))).toBe(true);
      expect(await store.trackedSetRecord()).toMatchObject({
        trackedSet: trackedSetItems(BASE),
      });
    } finally {
      await store.close();
    }
  });

  it("the wallet seeder owes its wallets again after a reset and re-seeds them after the replay", async () => {
    const open = await adapter.database();
    const store = open({ wallets: [WALLET] });
    const seed = {
      outRef: { txHash: fill(0xe1), index: 0 },
      output: { address: WALLET, lovelace: 7_000_000n },
    };
    const queries: string[] = [];
    const seeder = createWalletSeeder({
      store,
      wallets: [WALLET],
      ledger: {
        withLedgerState: async (_at, use) =>
          await use({
            query: async () => {
              queries.push("utxo_by_address");
              return encodeUtxoAnswer([seed]);
            },
          }),
      },
    });
    const seedRows = () =>
      rows(
        store,
        "SELECT tx_hash, seed_slot FROM l1_outputs WHERE created_slot IS NULL",
      );
    try {
      expect((await store.start()).kind).toBe("ready");
      await follow(store);
      expect(await seeder.step()).toEqual({ kind: "ready" });
      expect(await seedRows()).toEqual([`${hex(fill(0xe1))}|${B3.slot}`]);
      // A store from before the record: the next start resets it.
      await store.transaction("write", (tx) =>
        tx.query("DELETE FROM l1_follower_tracked_set"),
      );
      expect(await store.start()).toMatchObject({
        trackedSet: { kind: "reset", cause: "unrecorded" },
      });
      expect(await seedRows()).toEqual([]);
      expect(seeder.owed()).toEqual([WALLET]);
      await follow(store);
      expect(await seeder.step()).toEqual({ kind: "ready" });
      expect(queries).toHaveLength(2);
      expect(await seedRows()).toEqual([`${hex(fill(0xe1))}|${B3.slot}`]);
      expect(store.isTrackedLive(seed.outRef)).toBe(true);
      expect((await store.checkInvariants()).ok).toBe(true);
    } finally {
      seeder.close();
      await store.close();
    }
  });
});
