/**
 * The intent journal's prune floor across a store reset's replay
 * (`INTENT_PRUNE_FLOOR`, §8.2 retention over §5's reset and §11's role
 * floors), on a SQLite and a Postgres store. While the tracked-set record
 * says `replaying`, the prune step skips the journal's retention hook, and
 * the floor holds the class A boundary at the boundary in the hook's mark
 * (`l1_intent_prune_mark`), or, with no mark, where the replay began. The
 * first prune step after the replay mark clears sets no floor.
 *
 * The scenario: the hook last ran at the funding block's boundary; then a
 * foreign tx spends one funded output (its own outputs are untracked), an
 * own intent spends the other, and a later foreign tx spends that intent's
 * only output. Every one of those spends and their `l1_txs` rows sits above
 * the mark's boundary and at or below the prune boundary at the replay's
 * end.
 */
import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import type { ChainSyncEvent } from "@al-ft/l1-node-transport";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import {
  applyChainSyncEvent,
  deriveIntentStatusesIn,
  type DialectName,
  type FactStore,
  intentJournalProjection,
  openPostgresFactStore,
  openSqliteFactStore,
  projectionStoreOptions,
  type TrackedSet,
} from "../src/index.js";
import { readPruneMarkIn } from "../src/intents/windows.js";
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
const scratch = mkdtempSync(join(tmpdir(), "l1-follower-intent-floor-"));
const opened: FactStore[] = [];
afterEach(async () => {
  await Promise.all(opened.splice(0).map((store) => store.close()));
});
afterAll(async () => {
  await databases.dropAll();
  rmSync(scratch, { recursive: true, force: true });
});

const K = 3;
const u = simUniverse();
/** The universe's set plus an address: reopening under it resets the store. */
const GROWN: TrackedSet = {
  ...u.tracked,
  addresses: new Set([...u.tracked.addresses, `61${"cd".repeat(28)}`]),
};

const opener = async (dialect: DialectName) => {
  const where =
    dialect === "sqlite"
      ? join(scratch, `${String(Math.random()).slice(2)}.db`)
      : (await databases.create()).url;
  return async (trackedSet: TrackedSet): Promise<FactStore> => {
    const options = projectionStoreOptions(
      [intentJournalProjection],
      { securityParameter: K, trackedSet },
      dialect,
    );
    const store =
      dialect === "sqlite"
        ? openSqliteFactStore({ ...options, path: where })
        : openPostgresFactStore({
            ...options,
            connection: { connectionString: where },
          });
    opened.push(store);
    expect((await store.start()).kind).toBe("ready");
    return store;
  };
};

const close = async (store: FactStore) => {
  opened.splice(opened.indexOf(store), 1);
  await store.close();
};

const apply = async (store: FactStore, event: ChainSyncEvent) => {
  const result = await applyChainSyncEvent(store, event);
  expect(result.result.kind).toBe("applied");
};

const prune = async (store: FactStore) => {
  const result = await store.prune();
  if (!("done" in result)) throw new Error(JSON.stringify(result));
  return result;
};

const count = async (
  store: FactStore,
  sql: string,
  params: readonly Buffer[] = [],
) =>
  Number(
    (await store.transaction("read", (tx) => tx.query(sql, [...params])))[0]!.n,
  );

const statuses = async (store: FactStore) =>
  new Map(
    (
      await store.transaction("read", (tx) =>
        deriveIntentStatusesIn(tx, store.dialect),
      )
    ).states.map((state) => [
      state.intent.txHash.toString("hex"),
      state.status.kind,
    ]),
  );

/**
 * Follows the funding block k deep and prunes once (the hook writes its
 * mark at the funding block's slot), journals the two own intents, then
 * resets the store and replays every block with a prune step after each.
 * Returns the store with the replay mark still set.
 */
const replayed = async (dialect: DialectName) => {
  const reopen = await opener(dialect);
  const chain = new SimChain(u, SIM_ORIGIN);
  const events: ChainSyncEvent[] = [];
  const forward = (txs: readonly SimTx[] = []) => {
    const { event } = chain.forward(txs);
    events.push(event);
    return event;
  };
  let store = await reopen(u.tracked);
  expect((await store.initialize(SIM_ORIGIN)).kind).toBe("initialized");
  const funding: SimTx = {
    inputs: [chain.outsideInput()],
    outputs: [
      { address: u.trackedAddress, lovelace: 10_000_000n },
      { address: u.trackedAddress, lovelace: 10_000_000n },
    ],
    nonce: chain.nonce(),
  };
  await apply(store, forward([funding]));
  for (let i = 0; i < K; i += 1) await apply(store, forward());
  const first = await prune(store);
  const mark = await store.transaction("read", readPruneMarkIn);
  expect(mark?.boundarySlot).toBe(first.prunedThroughSlot);
  const fundingHash = simTxHash(funding);
  const conflicted: SimTx = {
    inputs: [{ txHash: fundingHash, index: 0 }],
    outputs: [{ address: u.trackedAddress, lovelace: 9_000_000n }],
    nonce: chain.nonce(),
  };
  const landed: SimTx = {
    inputs: [{ txHash: fundingHash, index: 1 }],
    outputs: [{ address: u.trackedAddress, lovelace: 9_000_000n }],
    nonce: chain.nonce(),
  };
  for (const intent of [conflicted, landed])
    expect(
      (
        await store.transaction("write", (tx) =>
          recordAtCurrentView(tx, store.dialect, {
            family: "commit",
            workflowKey: `commit:${simTxHash(intent).toString("hex")}`,
            txCbor: encodeSimTx(intent),
            isOwnOutput: (output) => output.address.equals(u.trackedAddress),
          }),
        )
      ).kind,
    ).toBe("recorded");
  await close(store);

  // The chain goes on: a foreign spender of the first funded output whose
  // outputs are untracked, the landed intent, then a foreign spender of the
  // landed intent's only output; k + 2 blocks follow.
  const spender: SimTx = {
    inputs: [{ txHash: fundingHash, index: 0 }],
    outputs: [{ address: u.untrackedAddress, lovelace: 9_500_000n }],
    nonce: chain.nonce(),
  };
  const later: SimTx = {
    inputs: [{ txHash: simTxHash(landed), index: 0 }],
    outputs: [{ address: u.untrackedAddress, lovelace: 8_500_000n }],
    nonce: chain.nonce(),
  };
  forward([spender]);
  forward([landed]);
  forward([later]);
  for (let i = 0; i < K + 2; i += 1) forward();

  // A grown tracked set resets the store; the replay prunes every step.
  store = await reopen(GROWN);
  expect((await store.trackedSetRecord())?.replaying).toBe(true);
  expect((await store.initialize(SIM_ORIGIN)).kind).toBe("initialized");
  const steps = [];
  for (const event of events) {
    await apply(store, event);
    steps.push(await prune(store));
  }
  return {
    store,
    mark: mark!,
    steps,
    hashes: {
      funding: fundingHash,
      conflicted: simTxHash(conflicted).toString("hex"),
      landed: simTxHash(landed).toString("hex"),
      txs: [spender, landed, later].map(simTxHash),
    },
  };
};

const SPENT = (hash: Buffer) =>
  [
    "SELECT count(*) AS n FROM l1_outputs WHERE tx_hash = ? AND spent_slot IS NOT NULL",
    [hash],
  ] as const;
const TXS = (hashes: readonly Buffer[]) =>
  [
    `SELECT count(*) AS n FROM l1_txs WHERE tx_hash IN (${hashes.map(() => "?").join(", ")})`,
    [...hashes],
  ] as const;

describe.each(["sqlite", "postgres"] as const)(
  "the intent journal's prune floor across a reset's replay on %s",
  (dialect) => {
    it("holds class A at the mark's boundary while replaying, so a conflicted intent whose spender keeps no retained output and a landed intent whose outputs were all spent later both reach a terminal status and are pruned once the replay ends", async () => {
      const { store, mark, steps, hashes } = await replayed(dialect);
      // The floor held: the replay's last steps stopped at the mark's
      // boundary, below the block k under the cursor.
      const last = steps.at(-1)!;
      expect(last.prunedThroughSlot).toBe(mark.boundarySlot);
      expect(last.floorLags.map(({ floor }) => floor)).toEqual([
        "intent_journal_replay",
      ]);
      expect(last.floorLags[0]!.lagSlots).toBeGreaterThan(0);
      for (const step of steps)
        expect(step.prunedThroughSlot).toBeLessThanOrEqual(mark.boundarySlot);
      // Every spend above the mark's boundary and its tx rows are kept.
      expect(await count(store, ...SPENT(hashes.funding))).toBe(2);
      expect(await count(store, ...TXS(hashes.txs))).toBe(3);

      expect(await store.endTrackedSetReplay()).toBe("ended");
      expect(await statuses(store)).toEqual(
        new Map([
          [hashes.conflicted, "conflicted"],
          [hashes.landed, "landed"],
        ]),
      );
      const pruned = await prune(store);
      expect(pruned.deleted.l1_intents).toBe(2);
      expect(await count(store, "SELECT count(*) AS n FROM l1_intents")).toBe(
        0,
      );
    });

    it("lifts the floor in the first prune step after the replay mark clears, and class A then prunes past the mark's boundary", async () => {
      const { store, mark, hashes } = await replayed(dialect);
      // Still replaying: the floor holds the boundary however often prune runs.
      const held = await prune(store);
      expect(held.prunedThroughSlot).toBe(mark.boundarySlot);
      expect(held.floorLags.map(({ floor }) => floor)).toEqual([
        "intent_journal_replay",
      ]);
      expect(held.floorLags[0]!.lagSlots).toBeGreaterThan(0);

      expect(await store.endTrackedSetReplay()).toBe("ended");
      const lifted = await prune(store);
      expect(lifted.floorLags).toEqual([]);
      expect(lifted.prunedThroughSlot).toBeGreaterThan(mark.boundarySlot);
      expect(lifted.prunedThroughSlot).toBe(
        (await store.transaction("read", readPruneMarkIn))?.boundarySlot,
      );
      expect(await count(store, ...SPENT(hashes.funding))).toBe(0);
      expect(await count(store, ...TXS(hashes.txs))).toBe(0);
    });

    it("with no mark, holds the boundary where the replay began until the replay mark clears", async () => {
      const reopen = await opener(dialect);
      const chain = new SimChain(u, SIM_ORIGIN);
      const events: ChainSyncEvent[] = [];
      let store = await reopen(u.tracked);
      expect((await store.initialize(SIM_ORIGIN)).kind).toBe("initialized");
      // No prune before the reset: the hook never ran, so there is no mark.
      const funding: SimTx = {
        inputs: [chain.outsideInput()],
        outputs: [{ address: u.trackedAddress, lovelace: 10_000_000n }],
        nonce: chain.nonce(),
      };
      const spend: SimTx = {
        inputs: [{ txHash: simTxHash(funding), index: 0 }],
        outputs: [{ address: u.untrackedAddress, lovelace: 9_000_000n }],
        nonce: chain.nonce(),
      };
      for (const txs of [[funding], [spend]])
        events.push(chain.forward(txs).event);
      for (let i = 0; i < K + 2; i += 1) events.push(chain.forward([]).event);
      for (const event of events) await apply(store, event);
      await close(store);

      store = await reopen(GROWN);
      expect((await store.initialize(SIM_ORIGIN)).kind).toBe("initialized");
      expect(await store.transaction("read", readPruneMarkIn)).toBeNull();
      for (const event of events) {
        await apply(store, event);
        expect((await prune(store)).prunedThroughSlot).toBe(
          SIM_ORIGIN.point.slot,
        );
      }
      expect(await count(store, ...SPENT(simTxHash(funding)))).toBe(1);
      expect(await store.endTrackedSetReplay()).toBe("ended");
      const lifted = await prune(store);
      expect(lifted.floorLags).toEqual([]);
      expect(lifted.prunedThroughSlot).toBeGreaterThan(SIM_ORIGIN.point.slot);
      expect(await count(store, ...SPENT(simTxHash(funding)))).toBe(0);
    });
  },
);
