/**
 * The node's follower-change driver (`createFollowerDriver`, N1, plan §7.3)
 * over a simulated chain on a SQLite and a Postgres follower store: which
 * change each run applies, the ticket hooks' order, and the holds `/readyz`
 * names. The Postgres sink it drives is covered by
 * `follower-events-ingestion.test.ts`.
 */
import type { FactStore, OutRef } from "@al-ft/midgard-l1-follower";
import {
  eventProjection,
  eventTrackedSet,
} from "@al-ft/midgard-l1-follower/events";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import {
  createFollowerDriver,
  DRIVER_HOOK_ORDER,
  type DriverHold,
  type DriverHooks,
  EVENT_IDENTITY_CONFLICT,
  EVENT_UNDECODABLE,
  type EventRefusal,
  EVENTS_HOOK_FAILED,
  EVENTS_INGESTION_FAILED,
  EVENTS_ORPHAN_RECOVERY,
  type FollowerChange,
  type IngestionPlan,
  type SinkResult,
} from "../src/l1-events/driver.js";
import {
  admissionTx,
  eventOrder,
  EVENTS_CONFIG,
} from "./helpers/l1-events-chain.js";
import {
  ChainDriver,
  storeOpener,
  testDatabases,
} from "./helpers/l1-events-store.js";

const databases = testDatabases();
const opened: FactStore[] = [];
afterEach(async () => {
  await Promise.all(opened.splice(0).map((store) => store.close()));
});
afterAll(async () => {
  await databases.dropAll();
});

let nonces = 0;
const nonceRef = (): OutRef => {
  nonces += 1;
  const txHash = Buffer.alloc(32, 0xd7);
  txHash.writeUInt32BE(nonces, 28);
  return { txHash, index: 0 };
};

const APPLIED: SinkResult = {
  kind: "applied",
  inserted: 0,
  orphans: 0,
  refused: [],
};

/**
 * A sink that records each change and plan it is handed and answers from
 * `answers` (then `applied`); an Error answer is thrown.
 */
const recordingSink = (answers: (SinkResult | Error)[] = []) => {
  const calls: { change: FollowerChange; keys: string[] }[] = [];
  return {
    calls,
    sink: {
      apply: (change: FollowerChange, plan: IngestionPlan) => {
        calls.push({ change, keys: plan.events.map((event) => event.key) });
        const answer = answers.shift() ?? APPLIED;
        return answer instanceof Error
          ? Promise.reject(answer)
          : Promise.resolve(answer);
      },
    },
  };
};

/** Every ticket hook, each logging `name:change` and answering `answer`. */
const loggingHooks = (
  log: string[],
  answer: (
    name: (typeof DRIVER_HOOK_ORDER)[number],
  ) => DriverHold | Error | undefined = () => undefined,
): DriverHooks =>
  Object.fromEntries(
    DRIVER_HOOK_ORDER.map((name) => [
      name,
      (change: FollowerChange) => {
        log.push(`${name}:${change.kind}`);
        const result = answer(name);
        return result instanceof Error
          ? Promise.reject(result)
          : Promise.resolve(result);
      },
    ]),
  );

const hookLog = (kind: FollowerChange["kind"]) =>
  DRIVER_HOOK_ORDER.map((name) => `${name}:${kind}`);

describe.each(["sqlite", "postgres"] as const)(
  "the follower-change driver over a %s follower store",
  (dialect) => {
    const open = storeOpener(dialect, databases);
    const followed = async () => {
      const store = await open([eventProjection(EVENTS_CONFIG)], 4);
      opened.push(store);
      const chain = new ChainDriver(store, eventTrackedSet(EVENTS_CONFIG));
      await chain.init();
      return { store, chain };
    };

    it("applies each advance and rewind once with the projection there, then every ticket hook in order", async () => {
      const { store, chain } = await followed();
      const deposit = eventOrder("deposit", nonceRef());
      const withdrawal = eventOrder("withdrawal", nonceRef());
      await chain.forward([admissionTx(deposit, 1)]);
      const { calls, sink } = recordingSink();
      const log: string[] = [];
      const driver = createFollowerDriver({
        store,
        config: EVENTS_CONFIG,
        sink,
        hooks: loggingHooks(log),
      });

      const initial = await driver.run();
      expect(initial).toMatchObject({ kind: "ran", holds: [] });
      expect(calls).toEqual([
        {
          change: { kind: "initial", view: driver.applied() },
          keys: [deposit.key],
        },
      ]);
      expect(log).toEqual(hookLog("initial"));
      const first = driver.applied()!;

      // Nothing moved and nothing is held: no sink call, no hook.
      expect(await driver.run()).toMatchObject({
        kind: "ran",
        change: { kind: "unchanged" },
        holds: [],
      });
      expect(calls).toHaveLength(1);
      expect(log).toHaveLength(DRIVER_HOOK_ORDER.length);

      await chain.forward([admissionTx(withdrawal, 2)]);
      await driver.run();
      const second = driver.applied()!;
      expect(second.generation).toBe(first.generation);
      expect(second.point.slot).toBeGreaterThan(first.point.slot);
      expect(calls[1]).toEqual({
        change: { kind: "advance", before: first, view: second },
        keys: [deposit.key, withdrawal.key],
      });

      // The follower rewinds the withdrawal's block: a generation change.
      await chain.backward(1);
      await driver.run();
      const rewound = driver.applied()!;
      expect(rewound.generation).not.toBe(second.generation);
      expect(calls[2]).toEqual({
        change: { kind: "rewind", before: second, view: rewound },
        keys: [deposit.key],
      });
      expect(log).toEqual([
        ...hookLog("initial"),
        ...hookLog("advance"),
        ...hookLog("rewind"),
      ]);
    });

    it("keeps the last applied view while the sink does not apply, names every hold, and replays the change", async () => {
      const { store, chain } = await followed();
      await chain.forward([admissionTx(eventOrder("deposit", nonceRef()), 1)]);
      const orphanHold: DriverHold = {
        reason: EVENTS_ORPHAN_RECOVERY,
        detail: "1 orphaned admission",
      };
      const { calls, sink } = recordingSink([
        APPLIED,
        { kind: "held", hold: orphanHold },
        new Error("pool closed"),
        APPLIED,
      ]);
      const log: string[] = [];
      let failing = true;
      const intentHold: DriverHold = {
        reason: "intent_pending",
        detail: "signed intent awaits landing",
      };
      const driver = createFollowerDriver({
        store,
        config: EVENTS_CONFIG,
        sink,
        hooks: loggingHooks(log, (name) =>
          !failing
            ? undefined
            : name === "correctionRecompute"
              ? new Error("recompute broke")
              : name === "intentStatus"
                ? intentHold
                : undefined,
        ),
      });
      failing = false;
      await driver.run();
      const first = driver.applied()!;
      failing = true;

      await chain.forward([admissionTx(eventOrder("deposit", nonceRef()), 2)]);
      const held = await driver.run();
      // The sink's hold first, then the hooks' holds in hook order.
      const expectedHolds = [
        orphanHold,
        {
          reason: EVENTS_HOOK_FAILED,
          detail: "correctionRecompute: recompute broke",
        },
        intentHold,
      ];
      expect(held).toMatchObject({ kind: "ran", holds: expectedHolds });
      expect(driver.holds()).toEqual(expectedHolds);
      expect(driver.applied()).toBe(first);

      // A failing sink is a named hold, never a throw; the view still waits.
      const failed = await driver.run();
      expect(failed).toMatchObject({
        kind: "ran",
        change: { kind: "advance", before: first },
        result: {
          kind: "held",
          hold: { reason: EVENTS_INGESTION_FAILED, detail: "pool closed" },
        },
      });
      expect(driver.applied()).toBe(first);

      // Once the sink applies and the hooks clear, the same change lands.
      failing = false;
      const cleared = await driver.run();
      expect(cleared).toMatchObject({ kind: "ran", holds: [] });
      expect(driver.holds()).toEqual([]);
      const second = driver.applied()!;
      expect(second.point.slot).toBeGreaterThan(first.point.slot);
      expect(calls.slice(1).map(({ change }) => change)).toEqual(
        Array.from({ length: 3 }, () => ({
          kind: "advance",
          before: first,
          view: second,
        })),
      );
    });

    it("applies a change past the events the sink refused, holds on a conflict until it clears and logs each refusal once", async () => {
      const { store, chain } = await followed();
      const bad = eventOrder("deposit", nonceRef());
      const conflicted = eventOrder("withdrawal", nonceRef());
      await chain.forward([admissionTx(bad, 1), admissionTx(conflicted, 2)]);
      const undecodable: EventRefusal = {
        kind: "deposit",
        key: bad.key,
        idCbor: "d8",
        reason: EVENT_UNDECODABLE,
        detail: "unsupported committed deposit L2 network id",
      };
      const conflict: EventRefusal = {
        kind: "withdrawal",
        key: conflicted.key,
        idCbor: "d9",
        reason: EVENT_IDENTITY_CONFLICT,
        detail: "a local row of its public id holds another live admission",
      };
      const refusing: SinkResult = {
        ...APPLIED,
        inserted: 1,
        refused: [undecodable, conflict],
      };
      const { sink } = recordingSink([
        refusing,
        { kind: "held", hold: { reason: "x", detail: "y" } },
        { ...APPLIED, refused: [undecodable] },
      ]);
      const lines: string[] = [];
      const driver = createFollowerDriver({
        store,
        config: EVENTS_CONFIG,
        sink,
        log: (line) => lines.push(line),
      });
      const conflictHold: DriverHold = {
        reason: EVENT_IDENTITY_CONFLICT,
        detail: `withdrawal ${conflicted.key} (id d9): a local row of its public id holds another live admission`,
      };

      // The view applies; the conflict holds, the undecodable event only refuses.
      await driver.run();
      const first = driver.applied();
      expect(first).not.toBeNull();
      expect(driver.holds()).toEqual([conflictHold]);
      expect(driver.refused()).toEqual([undecodable]);

      // A run that does not apply keeps the last applied view's refusals.
      await chain.forward([]);
      await driver.run();
      expect(driver.applied()).toBe(first);
      expect(driver.holds()).toEqual([
        { reason: "x", detail: "y" },
        conflictHold,
      ]);
      expect(driver.refused()).toEqual([undecodable]);

      // The conflict clears; the undecodable event stays refused, logged once.
      await driver.run();
      expect(driver.applied()).not.toBe(first);
      expect(driver.holds()).toEqual([]);
      expect(driver.refused()).toEqual([undecodable]);
      expect(lines.filter((line) => line.startsWith("event refused"))).toEqual([
        `event refused (${EVENT_UNDECODABLE}): deposit ${bad.key} (id d8): unsupported committed deposit L2 network id`,
        `event refused (${EVENT_IDENTITY_CONFLICT}): ${conflictHold.detail}`,
      ]);
    });

    it("names an unreadable follower store and applies nothing", async () => {
      const failing = {
        currentView: () => Promise.reject(new Error("store closed")),
      } as unknown as FactStore;
      const { calls, sink } = recordingSink();
      const log: string[] = [];
      const driver = createFollowerDriver({
        store: failing,
        config: EVENTS_CONFIG,
        sink,
        hooks: loggingHooks(log),
      });
      expect(await driver.run()).toEqual({ kind: "no_view" });
      expect(driver.holds()).toEqual([
        { reason: EVENTS_INGESTION_FAILED, detail: "store closed" },
      ]);
      expect(driver.applied()).toBeNull();
      expect(calls).toEqual([]);
      expect(log).toEqual([]);
    });
  },
);
