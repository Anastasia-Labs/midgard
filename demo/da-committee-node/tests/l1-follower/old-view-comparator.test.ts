// Tests the old-code comparator; deleted with it at the C1 cutover.
import { openSqliteFactStore } from "@al-ft/midgard-l1-follower";
import type { ShadowComparator } from "@al-ft/midgard-l1-follower/shadow";
import {
  forkCorpus,
  type ForkRunOptions,
  runForkScenario,
} from "@al-ft/midgard-l1-follower/testing";
import { describe, expect, it } from "vitest";

import {
  committeeComparator,
  factFedSnapshot,
} from "../../src/l1/follower-shadow/comparator.js";
import {
  committeeForkCorpus,
  committeeSimProjection,
  SIM_DEPTHS,
  SIM_K,
  SIM_QUEUE,
  SIM_SLOT_TIME,
  zeroStats,
} from "./queue-sim.js";

const openSqlite: ForkRunOptions["open"] = (optionsFor) =>
  Promise.resolve(
    openSqliteFactStore({ ...optionsFor("sqlite"), path: ":memory:" }),
  );

/** The comparator, counting the blocks at which both sides had a view. */
const countedComparator = (): Readonly<{
  comparator: ShadowComparator;
  compared: () => number;
}> => {
  const inner = committeeComparator({
    name: "queue-logic",
    parameters: SIM_DEPTHS,
    slotTime: SIM_SLOT_TIME,
    identity: {
      deploymentFingerprint: "sim",
      deploymentIdentityDigest: "00".repeat(32),
      stateQueuePolicyId: SIM_QUEUE.stateQueuePolicyId,
      daAttestationPolicyId: "72".repeat(28),
      finalityDepth: SIM_DEPTHS.confirmationDepth,
    },
    snapshot: (context) => factFedSnapshot(context.store, context, SIM_QUEUE),
  });
  let both = 0;
  let projectedValue = false;
  return {
    comparator: {
      ...inner,
      projected: async (context) => {
        const result = await inner.projected(context);
        projectedValue = result.kind === "value";
        return result;
      },
      current: async (context) => {
        const result = await inner.current(context);
        if (projectedValue && result.kind === "value") both += 1;
        return result;
      },
    },
    compared: () => both,
  };
};

const corpus = [...forkCorpus(SIM_K), ...committeeForkCorpus()];

describe("committee comparator against the current scanner (fork simulator)", () => {
  it.each(corpus.map((entry) => [entry.name, entry.scenario] as const))(
    "agrees on honest traffic: %s",
    async (_, scenario) => {
      const { comparator, compared } = countedComparator();
      const outcome = await runForkScenario(scenario, {
        open: openSqlite,
        k: SIM_K,
        projections: [
          committeeSimProjection(zeroStats(), { expectHealthy: true }),
        ],
        comparators: [comparator],
      });
      expect(outcome).toMatchObject({ ok: true });
      // Both sides were read at every event, so a green run compared them.
      expect(compared()).toBe(outcome.stats.events);
    },
  );

  // The current code walks from the root and drops what the walk misses, so
  // an orphan node leaves it a healthy queue the projection refuses (P1).
  // The comparator must surface that difference, not paper over it.
  it("reports the orphan node the current code drops", async () => {
    const reasons: string[] = [];
    for (const { scenario } of corpus.slice(0, 6)) {
      const outcome = await runForkScenario(scenario, {
        open: openSqlite,
        k: SIM_K,
        projections: [
          committeeSimProjection(zeroStats(), { orphanChance: 0.2 }),
        ],
        comparators: [countedComparator().comparator],
      });
      if (!outcome.ok) reasons.push(outcome.reason);
    }
    expect(reasons.length).toBeGreaterThan(0);
    for (const reason of reasons)
      expect(reason).toMatch(/^committee\/queue-logic differs: .*healthy/);
  });
});
