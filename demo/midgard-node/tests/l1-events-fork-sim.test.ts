/**
 * The node event projection under the fork simulator (plan §15 F8, N1):
 * after every chain-sync event its tables equal a fresh replay's (the
 * runner's check), INV1-INV6 hold, and its events, retired-key refusals and
 * spendable deposits equal an independent model of the canonical chain.
 * Each suite also proves its corpus exercised every §5.5 case it claims.
 */
import {
  type FactStore,
  openPostgresFactStore,
  openSqliteFactStore,
} from "@al-ft/midgard-l1-follower";
import {
  forkCorpus,
  type ForkRunOptions,
  type ForkScenario,
  forkScenarioArbitrary,
  runForkScenario,
} from "@al-ft/midgard-l1-follower/testing";
import fc from "fast-check";
import { afterAll, describe, expect, it } from "vitest";

import {
  eventSimProjection,
  type EventSimStats,
  zeroEventSimStats,
} from "./helpers/l1-events-sim.js";
import {
  DROP_ALL_TIMEOUT_MS,
  testDatabases,
} from "./helpers/l1-events-store.js";

const SIM_K = 6;
const RUNS = Number(process.env.L1_EVENTS_FORK_SIM_RUNS ?? "15");
const databases = testDatabases();

afterAll(async () => {
  await databases.dropAll();
}, DROP_ALL_TIMEOUT_MS);

const openSqlite: ForkRunOptions["open"] = (optionsFor) =>
  Promise.resolve(
    openSqliteFactStore({ ...optionsFor("sqlite"), path: ":memory:" }),
  );

const openPostgres: ForkRunOptions["open"] = async (
  optionsFor,
): Promise<FactStore> =>
  openPostgresFactStore({
    ...optionsFor("postgres"),
    connection: { connectionString: await databases.create() },
  });

const run = async (
  scenario: ForkScenario,
  open: ForkRunOptions["open"],
  stats: EventSimStats,
): Promise<void> => {
  const outcome = await runForkScenario(scenario, {
    open,
    k: SIM_K,
    projections: [eventSimProjection(stats)],
  });
  if (!outcome.ok) throw new Error(`step ${outcome.step}: ${outcome.reason}`);
};

const expectEveryCase = (stats: EventSimStats): void => {
  console.info("node event projection fork-sim", JSON.stringify(stats));
  expect(stats.checks).toBeGreaterThan(0);
  for (const field of [
    "admissions",
    "externals",
    "continuations",
    "retirements",
    "readmissions",
    "retiredKeyRefusals",
    "sameKeyOtherKind",
    "notYetDue",
    "prunedRetirements",
    "refusedAfterPrune",
    "admittedAfterPrune",
  ] as const)
    expect({ field, count: stats[field] > 0 }).toEqual({ field, count: true });
};

/** Long enough that ids orphaned by a rollback come back and retired keys are resubmitted. */
const LONG: ForkScenario = {
  seed: 0x0e1,
  episodes: Array.from({ length: 8 }, (_, index) => ({
    shape: (
      ["reland", "never_reland", "changed_valid_to", "new_fork_only"] as const
    )[index % 4]!,
    depth: 1 + (index % SIM_K),
    extra: 1 + (index % 2),
    landAt: index,
    variant: index,
    lead: 2,
  })),
};

describe.each([
  ["sqlite", openSqlite],
  ["postgres", openPostgres],
] as const)(
  "node event projection under the fork simulator (%s)",
  (_, open) => {
    it("holds over the follower's fork corpus and a long multi-episode run", async () => {
      const stats = zeroEventSimStats();
      for (const { scenario } of forkCorpus(SIM_K))
        await run(scenario, open, stats);
      await run(LONG, open, stats);
      expectEveryCase(stats);
    });

    it(`holds for ${RUNS} random scenarios (fast-check)`, async () => {
      const stats = zeroEventSimStats();
      await fc.assert(
        fc.asyncProperty(forkScenarioArbitrary(SIM_K), (scenario) =>
          run(scenario, open, stats),
        ),
        { numRuns: RUNS, seed: 0x0e1_0001 },
      );
      expect(stats.checks).toBeGreaterThan(0);
    });
  },
);
