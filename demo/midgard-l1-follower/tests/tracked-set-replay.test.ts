import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import type { ChainSyncEvent } from "@al-ft/l1-node-transport";
import { afterAll, describe, expect, it } from "vitest";

import {
  type DialectName,
  type FactStore,
  FOLLOWER_TRACKED_SET_CHANGED,
  type FollowStatus,
  openPostgresFactStore,
  openSqliteFactStore,
  type TrackedSet,
} from "../src/index.js";
import {
  buildForkSteps,
  diffDumps,
  dumpStore,
  forkCorpus,
  simStoreOptions,
  simUniverse,
} from "../src/testing/index.js";
import { appliedAll, follow, script } from "./support/follow-loop.js";
import { FIXTURE_PROJECTION, SIM_K } from "./support/fork-sim.js";
import { testDatabases } from "./support/postgres.js";

const databases = testDatabases();
const scratch = mkdtempSync(join(tmpdir(), "l1-follower-tracked-replay-"));

afterAll(async () => {
  await databases.dropAll();
  rmSync(scratch, { recursive: true, force: true });
});

/** A scenario with rollbacks, so the replay rewinds on the way to the tip. */
const events: readonly ChainSyncEvent[] = buildForkSteps(
  forkCorpus(SIM_K).find((entry) => entry.name === "every shape in sequence")!
    .scenario,
  [FIXTURE_PROJECTION],
).steps.map((step) => step.event);
/** The events as a lagging follower sees them: every tip is the final one. */
const behind = (all: readonly ChainSyncEvent[]): ChainSyncEvent[] =>
  all.map((event) => ({ ...event, tip: all[all.length - 1]!.tip }));

const FULL: TrackedSet = simStoreOptions(
  [FIXTURE_PROJECTION],
  SIM_K,
  "sqlite",
).trackedSet;
/** The set without the simulator's tracked policy. */
const REDUCED: TrackedSet = {
  ...FULL,
  policies: new Set(
    [...FULL.policies].filter((p) => p !== simUniverse().trackedPolicy),
  ),
};

type Opener = (trackedSet: TrackedSet) => FactStore;

const adapters: readonly Readonly<{
  name: DialectName;
  database: () => Promise<Opener>;
}>[] = [
  {
    name: "sqlite",
    database: async () => {
      const path = join(scratch, `${String(Math.random()).slice(2)}.db`);
      return (trackedSet) =>
        openSqliteFactStore({
          ...simStoreOptions([FIXTURE_PROJECTION], SIM_K, "sqlite"),
          trackedSet,
          path,
        });
    },
  },
  {
    name: "postgres",
    database: async () => {
      const { url } = await databases.create();
      return (trackedSet) =>
        openPostgresFactStore({
          ...simStoreOptions([FIXTURE_PROJECTION], SIM_K, "postgres"),
          trackedSet,
          connection: { connectionString: url },
        });
    },
  },
];

const reasons = (status: FollowStatus): string[] =>
  status.readiness.map((r) => r.reason);

/** Follows `all` from an empty store under `trackedSet`; the dump. */
const followedDump = async (
  open: Opener,
  trackedSet: TrackedSet,
  all: readonly ChainSyncEvent[],
) => {
  const store = open(trackedSet);
  try {
    const s = script(all);
    await follow({ store, script: s, until: appliedAll(s) });
    return await dumpStore(store);
  } finally {
    await store.close();
  }
};

describe.each(adapters)(
  "followChain after a tracked-set addition ($name)",
  (adapter) => {
    it("resets, replays from the origin unready with tracked_set_changed until the first tip, and ends as a fresh store", async () => {
      const open = await adapter.database();
      const reduced = await followedDump(open, REDUCED, events);
      const store = open(FULL);
      try {
        const s = script(behind(events));
        const { statuses, log } = await follow({
          store,
          script: s,
          until: appliedAll(s),
        });
        // The loop never stopped on its way: only the test's abort ends it.
        const reached = statuses.findIndex(appliedAll(s));
        expect(reached).toBeGreaterThan(0);
        expect(
          statuses
            .slice(0, reached + 1)
            .every(
              (status) =>
                status.state !== "stopped" && status.state !== "intervention",
            ),
        ).toBe(true);
        expect(
          log.some((line) => line.includes("the tracked set gained")),
        ).toBe(true);
        const replaying = statuses.filter(
          (status) => status.events > 0 && !status.atTip,
        );
        expect(replaying.length).toBeGreaterThan(0);
        expect(
          replaying.every((status) =>
            reasons(status).includes(FOLLOWER_TRACKED_SET_CHANGED),
          ),
        ).toBe(true);
        const last = statuses[statuses.length - 1]!;
        expect(last.atTip).toBe(true);
        expect(last.replaying).toBe(false);
        expect(reasons(last)).not.toContain(FOLLOWER_TRACKED_SET_CHANGED);
        expect(await store.trackedSetRecord()).toMatchObject({
          replaying: false,
        });
        const after = await dumpStore(store);
        const control = await followedDump(
          await adapter.database(),
          FULL,
          behind(events),
        );
        // Not vacuous: the policy changes what the store holds.
        expect(diffDumps(reduced, control)).not.toBeNull();
        expect(diffDumps(after, control)).toBeNull();
      } finally {
        await store.close();
      }
      // The next start finds the record equal and is not replaying.
      const next = open(FULL);
      try {
        expect(await next.start()).toMatchObject({
          trackedSet: { kind: "equal" },
          replaying: false,
        });
      } finally {
        await next.close();
      }
    });

    it("a restart mid-replay keeps tracked_set_changed until the tip", async () => {
      const open = await adapter.database();
      await followedDump(open, REDUCED, events);
      const half = Math.floor(events.length / 2);
      // One script across both runs: the second resumes where the node
      // acknowledged the first.
      const s = script(behind(events), { limit: half });
      const first = open(FULL);
      try {
        await follow({
          store: first,
          script: s,
          until: (status) => status.events >= half,
        });
      } finally {
        await first.close();
      }
      const second = open(FULL);
      try {
        const started = await second.start();
        expect(started).toMatchObject({
          trackedSet: { kind: "equal" },
          replaying: true,
        });
        s.limit = undefined;
        const { statuses } = await follow({
          store: second,
          script: s,
          until: (status) => status.atTip,
        });
        expect(
          reasons(statuses.find((status) => status.events > 0)!),
        ).toContain(FOLLOWER_TRACKED_SET_CHANGED);
        expect(statuses[statuses.length - 1]!.replaying).toBe(false);
        expect(await second.trackedSetRecord()).toMatchObject({
          replaying: false,
        });
      } finally {
        await second.close();
      }
    });
  },
);
