import type { ChainSyncEvent } from "@al-ft/l1-node-transport";
import { describe, expect, it } from "vitest";

import {
  type FactStore,
  FOLLOWER_NODE_BEHIND,
  type FollowStatus,
  openSqliteFactStore,
} from "../src/index.js";
import {
  buildForkSteps,
  forkCorpus,
  simStoreOptions,
} from "../src/testing/index.js";
import { appliedAll, follow, script } from "./support/follow-loop.js";
import { FIXTURE_PROJECTION, SIM_K } from "./support/fork-sim.js";

/** A short fork scenario, as a lagging follower sees it: every tip is the final one. */
const corpusEntry = forkCorpus(SIM_K)[0]!;
const short = buildForkSteps(corpusEntry.scenario, [
  FIXTURE_PROJECTION,
]).steps.map((step) => step.event);
const lastTip = (events: readonly ChainSyncEvent[]) =>
  events[events.length - 1]!.tip;
const behind = (events: readonly ChainSyncEvent[]): ChainSyncEvent[] =>
  events.map((event) => ({ ...event, tip: lastTip(events) }));

const openStore = (): FactStore =>
  openSqliteFactStore({
    ...simStoreOptions([FIXTURE_PROJECTION], SIM_K, "sqlite"),
    path: ":memory:",
  });

describe("followChain: a node behind wall-clock time", () => {
  /** Slot time in ms: slot N starts at N seconds. */
  const slotTime = (slot: number) => slot * 1_000;
  const tip = lastTip(short).point;
  const tipSlot = tip.kind === "point" ? Number(tip.slot) : 0;
  const tipMs = slotTime(tipSlot);
  const behindReason = (status: FollowStatus) =>
    status.readiness.find((r) => r.reason === FOLLOWER_NODE_BEHIND);

  it("reports l1_node_behind past the bound, rechecks with no events, and clears on catch-up", async () => {
    const store = openStore();
    try {
      const s = script(behind(short));
      let now = tipMs + 301_000;
      let caughtUp = false;
      const { final, statuses } = await follow({
        store,
        script: s,
        nodeBehind: { slotTime, now: () => now, checkEveryMs: 5 },
        until: (status) => {
          if (!caughtUp && appliedAll(s)(status) && behindReason(status)) {
            // Every event is applied: only the timer can clear it now.
            caughtUp = true;
            now = tipMs + 299_000;
            return false;
          }
          return caughtUp && behindReason(status) === undefined;
        },
      });
      const lastBehind = statuses.filter(behindReason).at(-1)!;
      expect(lastBehind.nodeBehind).toEqual({
        tipSlot,
        lagMs: 301_000,
        boundMs: 300_000,
      });
      expect(behindReason(lastBehind)?.detail).toBe(
        `node tip slot ${tipSlot} is 301 s behind wall-clock time (bound 300 s)`,
      );
      // The loop kept following while behind: no exit, no wait, no stop
      // until the test aborted it.
      expect(
        statuses
          .filter((status) => status.state !== "stopped")
          .every((status) => ["starting", "following"].includes(status.state)),
      ).toBe(true);
      const cleared = statuses.at(-1)!;
      expect(cleared.events).toBe(short.length);
      expect(cleared.nodeBehind).toBeNull();
      expect(final.state).toBe("stopped");
    } finally {
      await store.close();
    }
  });

  it("stays ready within the bound and without the option", async () => {
    for (const nodeBehind of [
      { slotTime, now: () => tipMs + 300_000 },
      undefined,
    ]) {
      const store = openStore();
      try {
        const s = script(behind(short));
        const { statuses } = await follow({
          store,
          script: s,
          ...(nodeBehind === undefined ? {} : { nodeBehind }),
          until: appliedAll(s),
        });
        expect(statuses.some(behindReason)).toBe(false);
        expect(statuses.every((status) => status.nodeBehind === null)).toBe(
          true,
        );
      } finally {
        await store.close();
      }
    }
  });
});
