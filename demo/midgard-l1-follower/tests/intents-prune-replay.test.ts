/**
 * The intent prune hook across a store reset's replay (§8.2 retention over
 * §5's reset), on a SQLite and a Postgres store. While a reset replays, a
 * prune step skips the retention hook but still deletes spent outputs at or
 * below its boundary; the journal's floor (`INTENT_PRUNE_FLOOR`) holds that
 * boundary at the one in the hook's mark (`l1_intent_prune_mark`). The mark
 * also holds the generation the hook last ran under: the reset's generation
 * is one the rollback log does not explain, so the hook's first run after
 * the replay derives every retained intent once, then reads candidates
 * again.
 *
 * The skip is the prune step's own, while the tracked-set record says
 * `replaying`; the test's hook only records the SQL each run reads.
 */
import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import type { ChainSyncEvent } from "@al-ft/l1-node-transport";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import {
  applyChainSyncEvent,
  type DialectName,
  type FactStore,
  INTENT_PRUNE_HOOK,
  intentJournalProjection,
  openPostgresFactStore,
  openSqliteFactStore,
  projectionStoreOptions,
  type PruneHook,
  recordIntentIn,
  type SqlTx,
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

const databases = testDatabases();
const scratch = mkdtempSync(join(tmpdir(), "l1-follower-intent-prune-"));
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
  addresses: new Set([...u.tracked.addresses, `61${"ab".repeat(28)}`]),
};
/** The hook's full derivation reads every journaled intent with this statement. */
const FULL_READ = "SELECT tx_hash FROM l1_intents";

/**
 * The intent hook, recording each run's SQL. The prune step itself skips it
 * while the store replays a reset, as it skips every retention hook.
 */
const recordingHook = (runs: string[][]): PruneHook => {
  const inner = INTENT_PRUNE_HOOK;
  if (inner.kind !== "retention")
    throw new Error("the intent prune hook is a retention hook");
  return {
    kind: "retention",
    table: inner.table,
    apply: async (context) => {
      const run: string[] = [];
      runs.push(run);
      const tx: SqlTx = {
        query: (sql, params) => {
          run.push(sql);
          return context.tx.query(sql, params);
        },
        exec: (sql) => context.tx.exec(sql),
      };
      return inner.apply({ ...context, tx });
    },
  };
};

const opener = async (dialect: DialectName, runs: string[][]) => {
  const where =
    dialect === "sqlite"
      ? join(scratch, `${String(Math.random()).slice(2)}.db`)
      : (await databases.create()).url;
  return async (trackedSet: TrackedSet): Promise<FactStore> => {
    const options = projectionStoreOptions(
      [{ ...intentJournalProjection, pruneHooks: [recordingHook(runs)] }],
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
    const started = await store.start();
    expect(started.kind).toBe("ready");
    return store;
  };
};

const apply = async (store: FactStore, event: ChainSyncEvent) => {
  const result = await applyChainSyncEvent(store, event);
  expect(["applied", "rewound"]).toContain(result.result.kind);
};

const prune = async (store: FactStore) => {
  const result = await store.prune();
  if (!("done" in result)) throw new Error(JSON.stringify(result));
  return result;
};

const count = async (store: FactStore, sql: string, params: Buffer[] = []) =>
  Number(
    (await store.transaction("read", (tx) => tx.query(sql, params)))[0]!.n,
  );

const close = async (store: FactStore) => {
  opened.splice(opened.indexOf(store), 1);
  await store.close();
};

describe.each(["sqlite", "postgres"] as const)(
  "the intent prune hook across a reset's replay on %s",
  (dialect) => {
    it("derives every retained intent once after a replay that pruned spent outputs without it, then reads candidates again", async () => {
      const runs: string[][] = [];
      const reopen = await opener(dialect, runs);
      const chain = new SimChain(u, SIM_ORIGIN);
      const events: ChainSyncEvent[] = [];
      const forward = (txs: readonly SimTx[] = []) => {
        const { event } = chain.forward(txs);
        events.push(event);
        return event;
      };

      // Before the reset: the hook's first run (no mark) derives everything.
      let store = await reopen(u.tracked);
      expect((await store.initialize(SIM_ORIGIN)).kind).toBe("initialized");
      const funding: SimTx = {
        inputs: [chain.outsideInput()],
        outputs: [{ address: u.trackedAddress, lovelace: 10_000_000n }],
        nonce: chain.nonce(),
      };
      await apply(store, forward([funding]));
      for (let i = 0; i < K; i += 1) await apply(store, forward());
      await prune(store);
      expect(runs).toHaveLength(1);
      expect(runs[0]).toContain(FULL_READ);
      const before = await store.transaction("read", readPruneMarkIn);
      expect(before).not.toBeNull();
      const intent: SimTx = {
        inputs: [{ txHash: simTxHash(funding), index: 0 }],
        outputs: [{ address: u.trackedAddress, lovelace: 9_000_000n }],
        nonce: chain.nonce(),
      };
      const recorded = await store.transaction("write", (tx) =>
        recordIntentIn(tx, store.dialect, {
          family: "commit",
          workflowKey: "commit:replay",
          txCbor: encodeSimTx(intent),
          isOwnOutput: (output) => output.address.equals(u.trackedAddress),
        }),
      );
      expect(recorded.kind).toBe("recorded");
      await close(store);

      // The intent lands, k + 2 blocks follow; a grown tracked set resets
      // the store, and the replay prunes every step with the hook skipped.
      forward([intent]);
      for (let i = 0; i < K + 2; i += 1) forward();
      store = await reopen(GROWN);
      expect((await store.trackedSetRecord())?.replaying).toBe(true);
      expect((await store.initialize(SIM_ORIGIN)).kind).toBe("initialized");
      for (const event of events) {
        await apply(store, event);
        await prune(store);
      }
      expect(runs).toHaveLength(1);
      // The intent spent the funding output above the mark's boundary: the
      // journal's floor keeps it through the replay.
      const funded = [simTxHash(funding)];
      expect(
        await count(
          store,
          "SELECT count(*) AS n FROM l1_outputs WHERE tx_hash = ?",
          funded,
        ),
      ).toBe(1);
      expect(await count(store, "SELECT count(*) AS n FROM l1_intents")).toBe(
        1,
      );
      expect(await store.endTrackedSetReplay()).toBe("ended");

      // A restart between the replay and the hook's next run loses nothing.
      await close(store);
      store = await reopen(GROWN);
      const pruned = await prune(store);
      expect(runs).toHaveLength(2);
      expect(runs[1]).toContain(FULL_READ);
      expect(pruned.deleted.l1_intents).toBe(1);
      expect(await count(store, "SELECT count(*) AS n FROM l1_intents")).toBe(
        0,
      );
      const cursor = (await store.cursor())!;
      expect(await store.transaction("read", readPruneMarkIn)).toEqual({
        generation: cursor.generation,
        boundarySlot: pruned.prunedThroughSlot,
      });
      expect(cursor.generation).toBeGreaterThan(before!.generation);

      // Then candidates again: on the next block, and across a rewind the
      // rollback log explains.
      await apply(store, forward());
      await prune(store);
      await apply(store, chain.backward(1));
      await apply(store, forward());
      await prune(store);
      expect(runs).toHaveLength(4);
      for (const run of runs.slice(2)) expect(run).not.toContain(FULL_READ);
    });
  },
);
