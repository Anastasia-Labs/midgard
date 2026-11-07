import { openSqliteFactStore } from "@al-ft/midgard-l1-follower";
import {
  forkCorpus,
  type ForkRunOptions,
  forkScenarioArbitrary,
  runForkScenario,
} from "@al-ft/midgard-l1-follower/testing";
import fc from "fast-check";
import { describe, expect, it } from "vitest";

import {
  availabilityReadsSimProjection,
  type AvailabilityReadStats,
  zeroAvailabilityReadStats,
} from "./availability-reads-sim.js";
import {
  committeeForkCorpus,
  committeeSimProjection,
  SIM_K,
  zeroStats,
} from "./queue-sim.js";

const RUNS = Number(process.env.L1_FORK_SIM_RUNS ?? "60");

const openSqlite: ForkRunOptions["open"] = (optionsFor) =>
  Promise.resolve(
    openSqliteFactStore({ ...optionsFor("sqlite"), path: ":memory:" }),
  );

const corpus = [...forkCorpus(SIM_K), ...committeeForkCorpus()];

describe("committee projections in the fork simulator (SQLite)", () => {
  const totals = zeroStats();
  const readTotals = zeroAvailabilityReadStats();

  it.each(corpus.map((entry) => [entry.name, entry.scenario] as const))(
    "corpus: %s",
    { timeout: 60_000 },
    async (_, scenario) => {
      const stats = zeroStats();
      const reads = zeroAvailabilityReadStats();
      const outcome = await runForkScenario(scenario, {
        open: openSqlite,
        k: SIM_K,
        projections: [
          committeeSimProjection(stats, { expectHealthy: true }),
          availabilityReadsSimProjection(reads),
        ],
      });
      expect(
        outcome.ok
          ? "ok"
          : `step ${outcome.step.toString()}: ${outcome.reason}`,
      ).toBe("ok");
      expect(stats.steps).toBe(outcome.stats.events);
      expect(reads.steps).toBe(outcome.stats.events);
      for (const key of Object.keys(totals) as (keyof typeof totals)[])
        totals[key] += stats[key];
      for (const key of Object.keys(
        readTotals,
      ) as (keyof AvailabilityReadStats)[])
        readTotals[key] += reads[key];
    },
  );

  // The corpus must reach every case the checks guard, or a green run
  // proves nothing about them.
  it("the corpus signs siblings, loses signed headers, finalizes, re-lands commits, prunes and proves headers unable to land at the exact boundary", () => {
    expect(totals.signed).toBeGreaterThan(0);
    expect(totals.relanded).toBeGreaterThan(0);
    expect(totals.boundary).toBeGreaterThan(0);
    expect(totals.prunedViews).toBeGreaterThan(0);
    expect(totals.beyondRetention).toBeGreaterThan(0);
    expect(totals.siblingPairs).toBeGreaterThan(0);
    expect(totals.disappeared).toBeGreaterThan(0);
    expect(totals.cannotLand).toBeGreaterThan(0);
    expect(totals.final).toBeGreaterThan(0);
    expect(totals.tailRemovedHealthy).toBeGreaterThan(0);
    expect(totals.unhealthy).toBe(0);
  });

  // The availability, promise and retirement reads over the store equal
  // those over a fresh replay after every event of the same corpus: it must
  // reach a read of each kind, abandoned branches and a pruned store.
  it("the corpus reads canonical and abandoned points, submissions and spends, invalidates views, and reads a pruned store", () => {
    expect(readTotals.canonical).toBeGreaterThan(0);
    expect(readTotals.offChain).toBeGreaterThan(0);
    expect(readTotals.submitted).toBeGreaterThan(0);
    expect(readTotals.spends).toBeGreaterThan(0);
    expect(readTotals.invalidatedViews).toBeGreaterThan(0);
    expect(readTotals.prunedReads).toBeGreaterThan(0);
  });

  it(`holds for ${RUNS.toString()} random scenarios (fast-check)`, async () => {
    await fc.assert(
      fc.asyncProperty(forkScenarioArbitrary(SIM_K), async (scenario) => {
        const outcome = await runForkScenario(scenario, {
          open: openSqlite,
          k: SIM_K,
          projections: [
            committeeSimProjection(zeroStats(), { expectHealthy: true }),
            availabilityReadsSimProjection(zeroAvailabilityReadStats()),
          ],
        });
        if (!outcome.ok)
          throw new Error(`step ${outcome.step.toString()}: ${outcome.reason}`);
      }),
      { numRuns: RUNS, seed: 0xc1_5eed },
    );
    // Sixty whole scenarios, each replayed fresh after every event: about
    // 5 s locally and slower on a shared CI runner.
  }, 120_000);

  it("an orphan node with a valid datum makes the queue unhealthy (P1)", async () => {
    let unhealthy = 0;
    for (const { scenario } of corpus.slice(0, 6)) {
      const stats = zeroStats();
      const outcome = await runForkScenario(scenario, {
        open: openSqlite,
        k: SIM_K,
        projections: [committeeSimProjection(stats, { orphanChance: 0.2 })],
      });
      expect(
        outcome.ok
          ? "ok"
          : `step ${outcome.step.toString()}: ${outcome.reason}`,
      ).toBe("ok");
      unhealthy += stats.unhealthy;
    }
    expect(unhealthy).toBeGreaterThan(0);
  });
});
