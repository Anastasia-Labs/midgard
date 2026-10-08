/**
 * The node's operator set (NC14) and this operator's membership (D-N7) over
 * a follower store, on SQLite and Postgres, through the production hook.
 *
 * - After its first load the set reads only the rows a block changed: a
 *   block that touches nothing it holds, or that grows the retired list,
 *   reads no row, and the rows every run's statements return stay the same
 *   however long the retired list grows.
 * - A retirement or a slash reads `removed` and raises `operator_removed`
 *   with the process up; a rollback deeper than cd that undoes the removal
 *   reads `active` again and clears it.
 * - A restart (a fresh mirror, hook and globals over the same facts and
 *   activity record) reads the same state back.
 * - The activity record keeps a slashed operator removed once the facts
 *   have pruned its spent active node; without the record it is unknown.
 * - The retired insertion anchor is the live predecessor, read by asset name.
 */
import {
  currentViewIn,
  type FactStore,
  type SqlTx,
} from "@al-ft/midgard-l1-follower";
import { type SimTx, simUniverse } from "@al-ft/midgard-l1-follower/testing";
import { Effect, Ref } from "effect";
import { afterAll, afterEach, beforeAll, describe, expect, it } from "vitest";

import {
  createOperatorSetMirror,
  memoryActivityRecord,
  OPERATOR_REMOVED,
  type OperatorActivityRecord,
  type OperatorSet,
  operatorSetHook,
  operatorSetProjection,
  type OperatorSetRun,
  operatorSetTrackedSet,
  publishOperatorMembership,
  retiredInsertionAnchorIn,
} from "../src/l1-operator-set/index.js";
import { Globals } from "../src/services/globals.js";
import { HaltSource } from "../src/services/liveness-halt.js";
import {
  ChainDriver,
  storeOpener,
  testDatabases,
} from "./helpers/l1-events-store.js";
import {
  loadOperatorSetChainFixture,
  operatorKey,
  OperatorSetChain,
  type OperatorSetChainFixture,
  txOf,
  type TxParts,
} from "./helpers/operator-set-chain.js";

const K = 6;
const DEPTH = { confirmationDepth: 2, securityParameter: K };
const OWN = operatorKey(0x50);
const FOREIGN = operatorKey(0x20);

const databases = testDatabases();
const opened: FactStore[] = [];
let fixture: OperatorSetChainFixture;

beforeAll(async () => {
  fixture = await loadOperatorSetChainFixture();
}, 120_000);
afterEach(async () => {
  await Promise.all(opened.splice(0).map((store) => store.close()));
});
afterAll(async () => {
  await databases.dropAll();
});

/** A record that never keeps anything (an activation it never saw final). */
const lostRecord: OperatorActivityRecord = {
  read: () => Promise.resolve(null),
  write: () => Promise.resolve(),
};

/**
 * One node process's operator set over `store`: a fresh mirror, hook and
 * globals. `step` runs the hook once and returns the run, the hold and the
 * `/readyz` reason it left, with the rows every statement returned.
 */
const nodeProcess = async (
  store: FactStore,
  activity: OperatorActivityRecord,
) => {
  const globals = await Effect.runPromise(
    Effect.provide(Globals, Globals.Default),
  );
  let rows = 0;
  const counted: Pick<FactStore, "dialect" | "transaction"> = {
    dialect: store.dialect,
    transaction: (mode, work) =>
      store.transaction(mode, (tx) =>
        work({
          query: async (text, params) => {
            const result = await tx.query(text, params);
            rows += result.length;
            return result;
          },
          exec: (text) => tx.exec(text),
        } satisfies SqlTx),
      ),
  };
  const runs: OperatorSetRun[] = [];
  const hook = operatorSetHook({
    store: counted,
    mirror: createOperatorSetMirror({ config: fixture.config, ownKey: OWN }),
    depth: DEPTH,
    activity,
    publish: async (run) => {
      runs.push(run);
      await Effect.runPromise(
        publishOperatorMembership(run.membership).pipe(
          Effect.provideService(Globals, globals),
        ),
      );
    },
  });
  const step = async () => {
    const view = await store.transaction("read", (tx) =>
      currentViewIn(tx, store.dialect),
    );
    if (view === null) throw new Error("no follower view");
    rows = 0;
    const hold = await hook({ kind: "unchanged", view });
    const run = runs[runs.length - 1]!;
    const reason = (
      await Effect.runPromise(Ref.get(globals.LIVENESS_REASONS))
    ).get(HaltSource.operatorMembership);
    return { run, hold, reason, rows };
  };
  return { step };
};

/** What the set holds, comparable across reads. */
const summary = (set: OperatorSet) => ({
  registered: set.registered.map((node) => node.assetName),
  active: set.active.map((node) => [
    node.assetName,
    node.active?.inactivity_strikes ?? null,
  ]),
  ownRetired: set.ownRetired?.node.assetName ?? null,
  scheduler: `${set.scheduler?.utxo.txHash}#${set.scheduler?.utxo.outputIndex}`,
  hubOracle: `${set.hubOracle?.utxo.txHash}#${set.hubOracle?.utxo.outputIndex}`,
  ownActivity: [...set.ownActivity].sort((a, b) =>
    a.outRef < b.outRef ? -1 : 1,
  ),
  unhealthy: set.unhealthy,
});

describe.each(["sqlite", "postgres"] as const)(
  "the node operator set over follower facts (%s)",
  (dialect) => {
    const open = storeOpener(dialect, databases);

    const chainOf = async () => {
      const store = await open([operatorSetProjection(fixture.config)], K);
      opened.push(store);
      const driver = new ChainDriver(
        store,
        operatorSetTrackedSet(fixture.config),
      );
      await driver.init();
      const lists = new OperatorSetChain(fixture);
      const live = () => driver.chain.live();
      const land = (...parts: TxParts[]) =>
        driver.forward([txOf(parts, driver.chain.nonce())]);
      const unrelated = (): SimTx => ({
        inputs: [driver.chain.outsideInput()],
        outputs: [
          { address: simUniverse().untrackedAddress, lovelace: 2_000_000n },
        ],
        nonce: driver.chain.nonce(),
      });
      const idle = async (blocks: number) => {
        for (let i = 0; i < blocks; i += 1) await driver.forward([unrelated()]);
      };
      await land(lists.genesis(driver.chain.outsideInput()));
      return { store, driver, lists, live, land, idle };
    };

    it("reads only the rows that changed since its last read", async () => {
      const { store, lists, live, land, idle } = await chainOf();
      await land(lists.insert(live(), "active", FOREIGN));
      await land(lists.insert(live(), "active", OWN));
      await land(lists.shift(live(), OWN, 1_000n));
      const node = await nodeProcess(store, memoryActivityRecord());

      const first = await node.step();
      expect(first.run.loaded).toBe(true);
      expect(first.run.set.unhealthy).toBeNull();
      expect(first.run.membership.state).toBe("active");
      expect(first.run.set.active.map((n) => n.assetName)).toEqual([
        fixture.config.active.rootAssetName,
        lists.assetName("active", FOREIGN),
        lists.assetName("active", OWN),
      ]);

      // Blocks that touch nothing the set holds: no row. (Past k the
      // activation is recorded, so later runs read the record's point.)
      await idle(K + 1);
      expect((await node.step()).run).toMatchObject({ rowsRead: 0 });
      await idle(1);
      const quiet = await node.step();
      expect(quiet.run).toMatchObject({ loaded: false, rowsRead: 0 });

      // The retired list grows by foreign retirements: still no row, and
      // every statement of the run returns what it did for the quiet block.
      for (const byte of [0x10, 0x30, 0x60, 0x70, 0x90, 0xa0, 0xb0, 0xc0]) {
        await land(lists.insert(live(), "retired", operatorKey(byte)));
        const grown = await node.step();
        expect(grown.run).toMatchObject({ loaded: false, rowsRead: 0 });
        expect(grown.rows).toBe(quiet.rows);
      }

      // The scheduler hands the shift on: its spent and its new output.
      await land(lists.shift(live(), FOREIGN, 2_000n));
      const shifted = await node.step();
      expect(shifted.run).toMatchObject({ loaded: false, rowsRead: 2 });
      expect(shifted.run.set.scheduler?.datum).toEqual({
        ActiveOperator: { operator: FOREIGN, start_time: 2_000n },
      });

      // A strike on the foreign node: its spent and its new output.
      await land(lists.strike(live(), FOREIGN));
      const struck = await node.step();
      expect(struck.run).toMatchObject({ loaded: false, rowsRead: 2 });

      // The changed rows applied equal a fresh load of the same facts.
      const fresh = await (
        await nodeProcess(store, memoryActivityRecord())
      ).step();
      expect(fresh.run.loaded).toBe(true);
      expect(summary(struck.run.set)).toEqual(summary(fresh.run.set));
    });

    it("reads a retirement as removed with the process up, and active again once a rollback deeper than cd undoes it", async () => {
      const { store, driver, lists, live, land, idle } = await chainOf();
      const record = memoryActivityRecord();
      const node = await nodeProcess(store, record);

      await land(lists.insert(live(), "registered", OWN));
      expect((await node.step()).run.membership.state).toBe(
        "awaiting_activation",
      );
      await land(
        lists.remove(live(), "registered", OWN),
        lists.insert(live(), "active", OWN),
      );
      await idle(1);
      const active = await node.step();
      expect(active.run.membership.state).toBe("active");
      expect(active.reason).toBeUndefined();

      // The retirement: out of the active list, into the retired one.
      await land(
        lists.remove(live(), "active", OWN),
        lists.insert(live(), "retired", OWN),
      );
      const removed = await node.step();
      expect(removed.run.membership.state).toBe("removed");
      expect(removed.run.membership.detail).toContain("retired list");
      expect(removed.reason).toBe(OPERATOR_REMOVED);
      // Removal holds duties through the reason, never the driver.
      expect(removed.hold).toBeUndefined();

      // Past cd: still a view of the facts, nothing sticky.
      await idle(2);
      const safe = await node.step();
      expect(safe.run.membership.state).toBe("removed");
      expect(safe.run.membership.detail).toContain("safe");

      // A restart over the same facts and record reads the same state.
      const restarted = await (await nodeProcess(store, record)).step();
      expect(restarted.run.membership.state).toBe("removed");
      expect(restarted.reason).toBe(OPERATOR_REMOVED);
      expect(restarted.hold).toBeUndefined();

      // A rollback of depth 3 (> cd) undoes the retirement.
      await driver.backward(3);
      const undone = await node.step();
      expect(undone.run.loaded).toBe(true);
      expect(undone.run.membership.state).toBe("active");
      expect(undone.reason).toBeUndefined();
      const restartedAfter = await (await nodeProcess(store, record)).step();
      expect(restartedAfter.run.membership.state).toBe("active");
      expect(restartedAfter.reason).toBeUndefined();
    });

    it("reads a slash as removed, and only the activity record keeps it removed once the facts prune the spent node", async () => {
      const { store, lists, live, land, idle } = await chainOf();
      await land(lists.insert(live(), "active", OWN));
      const record = memoryActivityRecord();
      const node = await nodeProcess(store, record);
      // An incremental set whose record never kept the activation.
      const unrecorded = await nodeProcess(store, lostRecord);
      expect((await node.step()).run.membership.state).toBe("active");
      expect((await unrecorded.step()).run.membership.state).toBe("active");
      await idle(K + 1);
      expect((await node.step()).run.membership.state).toBe("active");
      expect(await record.read()).not.toBeNull();

      // The slash: out of the active list, its token burned, no retired node.
      await land(lists.remove(live(), "active", OWN));
      const slashed = await node.step();
      expect(slashed.run.membership.state).toBe("removed");
      expect(slashed.run.membership.detail).toContain("in no operator list");
      expect(slashed.reason).toBe(OPERATOR_REMOVED);
      expect((await unrecorded.step()).run.membership.state).toBe("removed");

      // The spend falls out of retention. The unrecorded set reads up to
      // the tip first, so its next read takes the changes, not a load.
      await idle(K + 2);
      const beforePrune = await unrecorded.step();
      expect(beforePrune.run.loaded).toBe(false);
      expect(beforePrune.run.set.ownActivity.length).toBeGreaterThan(0);
      expect(beforePrune.run.membership.state).toBe("removed");
      for (;;) {
        const pruned = await store.prune();
        if ("kind" in pruned) throw new Error(`prune: ${pruned.kind}`);
        if (pruned.done) break;
      }
      const kept = await node.step();
      expect(kept.run.set.ownActivity).toEqual([]);
      expect(kept.run.membership.state).toBe("removed");
      expect(kept.reason).toBe(OPERATOR_REMOVED);
      const restarted = await (await nodeProcess(store, record)).step();
      expect(restarted.run.membership.state).toBe("removed");

      // Without the record nothing shows it was active: incremental or
      // loaded, the set reads what a fresh replay of the facts reads.
      const lost = await unrecorded.step();
      expect(lost.run.loaded).toBe(false);
      expect(lost.run.set.ownActivity).toEqual([]);
      expect(lost.run.membership.state).toBe("unknown");
      expect(lost.reason).toBeUndefined();
      const fresh = await (
        await nodeProcess(store, memoryActivityRecord())
      ).step();
      expect(fresh.run.membership.state).toBe("unknown");
    });

    it("reads the retired insertion anchor as the live predecessor by asset name", async () => {
      const { store, lists, live, land } = await chainOf();
      const anchorOf = async (key: string) => {
        let rows = 0;
        const anchor = await store.transaction("read", (tx) =>
          retiredInsertionAnchorIn(
            {
              query: async (text, params) => {
                const result = await tx.query(text, params);
                rows += result.length;
                return result;
              },
              exec: (text) => tx.exec(text),
            },
            store.dialect,
            fixture.config.retired,
            key,
          ),
        );
        const datumKey = anchor?.datum.key;
        return {
          key:
            datumKey === undefined
              ? undefined
              : datumKey === "Empty"
                ? null
                : datumKey.Key.key,
          rows,
        };
      };

      // Only the root: the anchor is the root.
      expect(await anchorOf(OWN)).toEqual({ key: null, rows: 1 });
      for (const byte of [0x20, 0x40, 0x60, 0x70, 0x80, 0x90, 0xa0, 0xb0])
        await land(lists.insert(live(), "retired", operatorKey(byte)));
      // The greatest lower key, one row, however long the list.
      expect(await anchorOf(OWN)).toEqual({ key: operatorKey(0x40), rows: 1 });
      expect(await anchorOf(operatorKey(0x10))).toEqual({
        key: null,
        rows: 1,
      });
      expect(await anchorOf(operatorKey(0xf0))).toEqual({
        key: operatorKey(0xb0),
        rows: 1,
      });
      // A bond recovery takes 0x40 out: the next lower live node.
      await land(lists.remove(live(), "retired", operatorKey(0x40)));
      expect(await anchorOf(OWN)).toEqual({ key: operatorKey(0x20), rows: 1 });
    });
  },
);
