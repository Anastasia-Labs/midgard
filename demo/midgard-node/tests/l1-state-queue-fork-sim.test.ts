/**
 * The node's landed state queue (plan §5.5 P1, §15 N2) under the fork
 * simulator, on SQLite and Postgres with prune on: after every chain-sync
 * event P1 at the tip equals an independent model replayed from the
 * canonical blocks (its health, reason, walk and policy-output count), and
 * the runner checks the facts P1 reads against a fresh replay. Each suite
 * proves its corpus exercised every case it claims: an orphan with a valid
 * datum is unhealthy (`orphan_node`), a rollback that removes the tail
 * leaves a healthy queue at the earlier tail, a third party's output at the
 * queue address is ignored while a valid policy node is walked, and checks
 * ran over pruned stores.
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
  DROP_ALL_TIMEOUT_MS,
  testDatabases,
} from "./helpers/l1-events-store.js";
import {
  stateQueueSimProjection,
  type StateQueueSimStats,
  zeroStateQueueSimStats,
} from "./helpers/state-queue-sim.js";

const SIM_K = 6;
const RUNS = Number(process.env.L1_STATE_QUEUE_FORK_SIM_RUNS ?? "15");
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
  stats: StateQueueSimStats,
): Promise<number> => {
  const outcome = await runForkScenario(scenario, {
    open,
    k: SIM_K,
    projections: [stateQueueSimProjection(stats)],
  });
  if (!outcome.ok) throw new Error(`step ${outcome.step}: ${outcome.reason}`);
  return outcome.stats.prunes;
};

const expectEveryCase = (stats: StateQueueSimStats, prunes: number): void => {
  console.info("node state-queue fork-sim", JSON.stringify(stats));
  expect(prunes).toBeGreaterThan(0);
  for (const field of [
    "checks",
    "prunedChecks",
    "appends",
    "attests",
    "merges",
    "tailRemovals",
    "orphans",
    "thirdPartyPayments",
    "orphanUnhealthy",
    "healedByRollback",
    "tailRemovedHealthy",
    "thirdPartyIgnored",
    "policyNodesIncluded",
  ] as const)
    expect({ field, count: stats[field] > 0 }).toEqual({ field, count: true });
};

/** Long enough for orphans to land and be rolled back, and for tails to come and go. */
const LONG: ForkScenario = {
  seed: 0x0b2,
  episodes: Array.from({ length: 8 }, (_, index) => ({
    shape: (
      ["reland", "never_reland", "changed_valid_to", "new_fork_only"] as const
    )[index % 4]!,
    depth: 1 + (index % SIM_K),
    extra: 1 + (index % 2),
    landAt: index,
    variant: index,
    lead: 2,
    prune: index % 2 === 1,
  })),
};

describe.each([
  ["sqlite", openSqlite],
  ["postgres", openPostgres],
] as const)(
  "node landed state queue under the fork simulator (%s)",
  (_, open) => {
    it("equals the canonical chain's queue over the follower's fork corpus (prune on) and a long run", async () => {
      const stats = zeroStateQueueSimStats();
      let prunes = 0;
      for (const { scenario } of forkCorpus(SIM_K))
        prunes += await run(scenario, open, stats);
      prunes += await run(LONG, open, stats);
      expectEveryCase(stats, prunes);
    });

    it(`equals the canonical chain's queue for ${RUNS} random scenarios (fast-check, prune on)`, async () => {
      const stats = zeroStateQueueSimStats();
      await fc.assert(
        fc.asyncProperty(forkScenarioArbitrary(SIM_K), async (scenario) => {
          await run(scenario, open, stats);
        }),
        { numRuns: RUNS, seed: 0x0b2_0001 },
      );
      expect(stats.checks).toBeGreaterThan(0);
    });
  },
);
