import type { ChainSyncEvent } from "@al-ft/l1-node-transport";
import { describe, expect, it } from "vitest";

import {
  applyChainSyncEvent,
  type FactStore,
  FOLLOWER_APPLY_STUCK,
  FOLLOWER_CATCHING_UP,
  FOLLOWER_WAITING,
  type FollowStatus,
  openSqliteFactStore,
  stepSettled,
} from "../src/index.js";
import {
  buildForkSteps,
  diffPruned,
  dumpRetained,
  dumpStore,
  forkCorpus,
  SIM_ORIGIN,
  simStoreOptions,
} from "../src/testing/index.js";
import { appliedAll, follow, script, UNSPENT } from "./support/follow-loop.js";
import { FIXTURE_PROJECTION, SIM_K } from "./support/fork-sim.js";

const corpus = forkCorpus(SIM_K);
const eventsOf = (name: string): readonly ChainSyncEvent[] =>
  buildForkSteps(corpus.find((entry) => entry.name === name)!.scenario, [
    FIXTURE_PROJECTION,
  ]).steps.map((step) => step.event);

/** A short scenario, and a long one (a 2k lead) whose spends fall past k. */
const short = eventsOf(corpus[0]!.name);
const long = eventsOf(`reland pruned around a depth-${SIM_K} rollback`);
const lastTip = (events: readonly ChainSyncEvent[]) =>
  events[events.length - 1]!.tip;
/** The events as a lagging follower sees them: every tip is the final one. */
const behind = (events: readonly ChainSyncEvent[]): ChainSyncEvent[] =>
  events.map((event) => ({ ...event, tip: lastTip(events) }));

const openStore = (): FactStore =>
  openSqliteFactStore({
    ...simStoreOptions([FIXTURE_PROJECTION], SIM_K, "sqlite"),
    path: ":memory:",
  });

const reasons = (status: FollowStatus): string[] =>
  status.readiness.map((r) => r.reason);

/** Applies `s`'s events once, so a later run resumes from the store. */
const caughtUpStore = async (events: readonly ChainSyncEvent[]) => {
  const store = openStore();
  const s = script(events);
  await follow({ store, script: s, until: appliedAll(s) });
  return store;
};

/** A store whose `applyBlock` fails at the block in `slot` while `failing()` says so. */
const failingAt = (
  store: FactStore,
  slot: number,
  failure: () => Error | null,
): FactStore => ({
  ...store,
  applyBlock: async (block) => {
    if (block.point.slot === slot) {
      const error = failure();
      if (error !== null) return { kind: "error", error };
    }
    return store.applyBlock(block);
  },
});

const slotOf = (event: ChainSyncEvent): number =>
  event.point.kind === "point" ? Number(event.point.slot) : -1;

const coded = (message: string, code: string): Error =>
  Object.assign(new Error(message), { code });

describe("followChain: interventions by name", () => {
  it("reports R3 (origin_after_protocol_init) at the tip and keeps following", async () => {
    const store = openStore();
    try {
      const s = script(short);
      const { final } = await follow({
        store,
        script: s,
        until: appliedAll(s),
      });
      expect(final.state).toBe("stopped");
      expect(final.interventions.map((i) => i.reason)).toEqual([
        "origin_after_protocol_init",
      ]);
      expect(reasons(final)).toEqual(["origin_after_protocol_init"]);
      expect(final.events).toBe(short.length);
    } finally {
      await store.close();
    }
  });

  it("reports R1 (rollback_beyond_k) and stops", async () => {
    const store = openStore();
    try {
      const rollToGenesis = {
        ...short[1]!,
        kind: "roll_backward",
        seq: short[short.length - 1]!.seq + 1n,
        point: { kind: "origin" },
      } as unknown as ChainSyncEvent;
      const s = script([...short.slice(0, 3), rollToGenesis]);
      const { final } = await follow({
        store,
        script: s,
        until: (status) => status.state === "intervention",
      });
      expect(final.state).toBe("intervention");
      expect(reasons(final)).toContain("rollback_beyond_k");
      expect(final.waiting).toBeNull();
    } finally {
      await store.close();
    }
  });

  it("reports R4 (origin_not_on_chain) on a fresh store and R2 (intersection_outside_history) on resume", async () => {
    const fresh = openStore();
    try {
      const { final } = await follow({
        store: fresh,
        script: script(short),
        origin: {
          origin: { slot: SIM_ORIGIN.point.slot, hash: Buffer.alloc(32, 0x0b) },
          hubOracleOneShot: UNSPENT,
        },
        until: (status) => status.state === "intervention",
      });
      expect(final.interventions.map((i) => i.reason)).toEqual([
        "origin_not_on_chain",
      ]);
      expect(reasons(final)).toContain("origin_not_on_chain");
    } finally {
      await fresh.close();
    }
    const store = await caughtUpStore(short);
    try {
      const { final } = await follow({
        store,
        script: script(short, {
          acked: short.length,
          intersectNotFound: { open: 0, resuming: true },
        }),
        until: (status) => status.state === "intervention",
      });
      expect(final.interventions.map((i) => i.reason)).toEqual([
        "intersection_outside_history",
      ]);
      expect(reasons(final)).toContain("intersection_outside_history");
    } finally {
      await store.close();
    }
  });

  it("reports origin_mismatch without opening a stream", async () => {
    const store = await caughtUpStore(short);
    try {
      const s = script(short, { acked: short.length });
      const { final } = await follow({
        store,
        script: s,
        origin: {
          origin: {
            slot: SIM_ORIGIN.point.slot + 1,
            hash: Buffer.alloc(32, 1),
          },
          hubOracleOneShot: UNSPENT,
        },
        until: (status) => status.state === "intervention",
      });
      expect(final.interventions.map((i) => i.reason)).toEqual([
        "origin_mismatch",
      ]);
      expect(reasons(final)).toContain("origin_mismatch");
      expect(s.opens).toBe(0);
    } finally {
      await store.close();
    }
  });

  it("reports R5 (store_integrity) when the store fails its invariants", async () => {
    const store = await caughtUpStore(short);
    try {
      const cursor = (await store.cursor())!;
      await store.transaction("write", (tx) =>
        tx.query(
          "INSERT INTO l1_blocks (slot, hash, height, parent_hash, qualifying_tx_count) VALUES (?, ?, ?, NULL, 0)",
          [
            cursor.point.slot + 1_000,
            Buffer.alloc(32, 0x5e),
            cursor.height + 500,
          ],
        ),
      );
      const s = script(short, { acked: short.length });
      const { final } = await follow({
        store,
        script: s,
        until: (status) => status.state === "intervention",
      });
      expect(final.interventions.map((i) => i.reason)).toEqual([
        "store_integrity",
      ]);
      expect(reasons(final)).toContain("store_integrity");
      expect(s.opens).toBe(0);
    } finally {
      await store.close();
    }
  });
});

describe("followChain: transient failures and stuck applies", () => {
  const target = slotOf(short[4]!);

  const runFailing = async (
    times: number,
    error: () => Error,
    stuckAfter = 3,
  ) => {
    const store = openStore();
    let left = times;
    let attempts = 0;
    const s = script(short);
    try {
      const run = await follow({
        store: failingAt(store, target, () => {
          attempts += 1;
          if (left <= 0) return null;
          left -= 1;
          return error();
        }),
        script: s,
        stuckAfter,
        until: appliedAll(s),
      });
      return { ...run, attempts };
    } finally {
      await store.close();
    }
  };

  it("retries an unknown failure fewer than N times in a row, and stops under l1_follower_apply_stuck at exactly N", async () => {
    const below = await runFailing(2, () => new Error("boom"));
    expect(below.statuses.some((s) => s.stuck !== null)).toBe(false);
    expect(
      below.statuses.filter((s) => s.waiting?.cause === "apply").length,
    ).toBeGreaterThan(0);
    expect(below.final.events).toBe(short.length);

    const at = await runFailing(10, () => new Error("boom"));
    // The third failure stops the loop: no fourth attempt.
    expect(at.attempts).toBe(3);
    expect(at.final.state).toBe("intervention");
    expect(at.final.stuck?.failures).toBe(3);
    expect(reasons(at.final)).toContain(FOLLOWER_APPLY_STUCK);
    expect(reasons(at.final)).not.toContain(FOLLOWER_WAITING);
    expect(at.final.events).toBe(4);
  });

  it("never escalates a transient failure, however often it repeats", async () => {
    for (const error of [
      () =>
        coded("terminating connection due to administrator command", "57P01"),
      () => coded("read ECONNRESET", "ECONNRESET"),
      () => new Error("Connection terminated unexpectedly"),
    ]) {
      const run = await runFailing(12, error);
      expect(run.statuses.some((s) => s.stuck !== null)).toBe(false);
      const waiting = run.statuses.filter((s) => s.state === "waiting");
      expect(waiting.length).toBeGreaterThanOrEqual(12);
      expect(waiting.every((s) => s.waiting?.cause === "apply")).toBe(true);
      expect(reasons(waiting[0]!)).toContain(FOLLOWER_WAITING);
      expect(run.final.events).toBe(short.length);
    }
  });

  it("stops on a deterministic failure at once, with no retry", async () => {
    const run = await runFailing(10, () =>
      coded("duplicate key value violates unique constraint", "23505"),
    );
    expect(run.attempts).toBe(1);
    expect(run.final.state).toBe("intervention");
    expect(run.final.stuck?.failures).toBe(1);
    expect(reasons(run.final)).toContain(FOLLOWER_APPLY_STUCK);
  });

  it("stops on an undecodable block at once", async () => {
    const store = openStore();
    try {
      const broken = {
        ...short[4]!,
        block: new Uint8Array([0xff, 0x00]),
      } as ChainSyncEvent;
      const s = script([...short.slice(0, 4), broken]);
      const { statuses, final } = await follow({
        store,
        script: s,
        // The loop returns by itself once it stops.
        until: () => false,
      });
      const first = statuses.find((status) => status.stuck !== null)!;
      expect(first.stuck!.failures).toBe(1);
      expect(reasons(first)).toContain(FOLLOWER_APPLY_STUCK);
      expect(first.state).toBe("intervention");
      expect(final.stuck?.failures).toBe(1);
      expect(s.opens).toBe(1);
    } finally {
      await store.close();
    }
  });

  it("waits out a stream drop and a held writer lease without escalating", async () => {
    const store = openStore();
    let locked = 7;
    const proxy: FactStore = {
      ...store,
      start: async () =>
        locked-- > 0
          ? {
              kind: "store_locked",
              detail: "another process holds this store's writer lease",
            }
          : store.start(),
    };
    try {
      const s = script(short, { failAt: 5 });
      const { statuses, final } = await follow({
        store: proxy,
        script: s,
        stuckAfter: 1,
        until: appliedAll(s),
      });
      const causes = new Set(statuses.map((status) => status.waiting?.cause));
      expect(causes).toContain("store_locked");
      expect(causes).toContain("stream");
      const lockWait = statuses.find(
        (status) => status.waiting?.cause === "store_locked",
      )!;
      expect(reasons(lockWait)).toContain(FOLLOWER_WAITING);
      expect(statuses.some((status) => status.stuck !== null)).toBe(false);
      expect(final.events).toBe(short.length);
      expect(s.opens).toBe(2);
    } finally {
      await store.close();
    }
  });

  it("survives a status listener that throws", async () => {
    const store = openStore();
    try {
      const s = script(short);
      const { final, log } = await follow({
        store,
        script: s,
        until: appliedAll(s),
        onStatus: () => {
          throw new Error("disk full");
        },
      });
      expect(final.events).toBe(short.length);
      expect(log.some((line) => line.includes("disk full"))).toBe(true);
    } finally {
      await store.close();
    }
  });
});

describe("followChain: catching up and pruning", () => {
  /** Applies `events` straight to a fresh store, never pruning. */
  const control = async (events: readonly ChainSyncEvent[]) => {
    const store = openStore();
    expect((await store.start()).kind).toBe("ready");
    expect((await store.initialize(SIM_ORIGIN)).kind).toBe("initialized");
    for (const event of events)
      expect(stepSettled(await applyChainSyncEvent(store, event))).toBe(true);
    return store;
  };

  it("reports l1_follower_catching_up until the tip, then not", async () => {
    const store = openStore();
    try {
      const s = script(behind(short));
      const { statuses } = await follow({
        store,
        script: s,
        until: appliedAll(s),
      });
      expect(reasons(statuses[0]!)).toContain(FOLLOWER_CATCHING_UP);
      const applied = statuses.filter((status) => status.events > 0);
      expect(
        applied
          .filter((status) => status.events < short.length)
          .every((status) => reasons(status).includes(FOLLOWER_CATCHING_UP)),
      ).toBe(true);
      const last = applied[applied.length - 1]!;
      expect(last.atTip).toBe(true);
      expect(reasons(last)).not.toContain(FOLLOWER_CATCHING_UP);
    } finally {
      await store.close();
    }
  });

  it("prunes at the tip: rows past k go, every row within k stays", async () => {
    const reference = await control(long);
    const store = openStore();
    try {
      const s = script(long);
      const { final } = await follow({
        store,
        script: s,
        until: appliedAll(s),
      });
      expect(final.prune.steps).toBeGreaterThan(0);
      expect(final.prune.lastError).toBeNull();
      const cursor = (await store.cursor())!;
      const pruned = cursor.prunedThroughSlot;
      expect(pruned).toBeGreaterThan(SIM_ORIGIN.point.slot);
      const spentPast = `SELECT count(*) AS n FROM l1_outputs WHERE spent_slot <= ${pruned}`;
      const count = (f: FactStore) =>
        f.transaction("read", async (tx) =>
          Number((await tx.query(spentPast))[0]!.n),
        );
      // Not vacuous: the unpruned store holds rows past k; the pruned none.
      expect(await count(reference)).toBeGreaterThan(0);
      expect(await count(store)).toBe(0);
      // Every row retention keeps (everything within k included) is there,
      // and nothing else changed.
      expect(
        diffPruned(
          await dumpStore(store),
          await dumpStore(reference),
          await dumpRetained(reference, {}, pruned),
        ),
      ).toBeNull();
      // The boundary is k blocks below the cursor: nothing at or above it.
      const boundary = await store.blockAtHeight(cursor.height - SIM_K);
      expect(boundary?.slot).toBe(pruned);
    } finally {
      await store.close();
      await reference.close();
    }
  });

  it("prunes every N applied events while catching up", async () => {
    const store = openStore();
    try {
      const s = script(behind(long));
      const { statuses } = await follow({
        store,
        script: s,
        prune: { everyEvents: 10 },
        until: appliedAll(s),
      });
      const catchingUp = statuses.filter((status) => !status.atTip);
      const steps = Math.max(...catchingUp.map((status) => status.prune.steps));
      expect(steps).toBeGreaterThanOrEqual(Math.floor((long.length - 1) / 10));
      expect(
        catchingUp.some(
          (status) =>
            status.prune.prunedThroughSlot !== null &&
            status.prune.prunedThroughSlot > SIM_ORIGIN.point.slot,
        ),
      ).toBe(true);
    } finally {
      await store.close();
    }
  });
});
