/**
 * The forced-order projection under the fork simulator (plan §15 F8, §12,
 * N10): after every chain-sync event its table equals a fresh replay's (the
 * runner's check), and its rows equal an independent model of the canonical
 * chain: same-block carriage resolves, earlier carriage is pending, tampered
 * carriage is malformed, failed order txs open nothing, spends close rows,
 * rollbacks delete them and their keys, and pruning drops spent ones k deep
 * (their keys stay while canonical). Each suite also proves its corpus
 * exercised every case it claims.
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
  forcedOrderSimProjection,
  type ForcedOrderSimStats,
  zeroForcedOrderSimStats,
} from "./helpers/forced-orders-sim.js";
import { testDatabases } from "./helpers/l1-events-store.js";

const SIM_K = 6;
const RUNS = Number(process.env.FORCED_ORDERS_FORK_SIM_RUNS ?? "10");
const databases = testDatabases();

afterAll(async () => {
  await databases.dropAll();
});

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
  stats: ForcedOrderSimStats,
): Promise<void> => {
  const outcome = await runForkScenario(scenario, {
    open,
    k: SIM_K,
    projections: [forcedOrderSimProjection(stats)],
  });
  if (!outcome.ok) throw new Error(`step ${outcome.step}: ${outcome.reason}`);
};

const expectEveryCase = (stats: ForcedOrderSimStats): void => {
  console.info("forced-order projection fork-sim", JSON.stringify(stats));
  expect(stats.checks).toBeGreaterThan(0);
  for (const field of [
    "resolved",
    "pending",
    "malformed",
    "failedOrders",
    "spentOrders",
    "orphanedOrders",
    "prunedOrders",
  ] as const)
    expect({ field, count: stats[field] > 0 }).toEqual({ field, count: true });
};

/** Long enough that orders are spent, orphaned and pruned in one run. */
const LONG: ForkScenario = {
  seed: 0x0f0,
  episodes: Array.from({ length: 8 }, (_, index) => ({
    shape: (
      ["reland", "never_reland", "changed_valid_to", "new_fork_only"] as const
    )[index % 4]!,
    depth: 1 + (index % SIM_K),
    extra: 1 + (index % 2),
    landAt: index,
    variant: index,
    lead: 2,
    ...(index % 3 === 2 ? { prune: true } : {}),
  })),
};

describe.each([
  ["sqlite", openSqlite],
  ["postgres", openPostgres],
] as const)(
  "forced-order projection under the fork simulator (%s)",
  (_, open) => {
    it("holds over the follower's fork corpus and a long multi-episode run", async () => {
      const stats = zeroForcedOrderSimStats();
      for (const { scenario } of forkCorpus(SIM_K))
        await run(scenario, open, stats);
      await run(LONG, open, stats);
      expectEveryCase(stats);
    });

    it(`holds for ${RUNS} random scenarios (fast-check)`, async () => {
      const stats = zeroForcedOrderSimStats();
      await fc.assert(
        fc.asyncProperty(forkScenarioArbitrary(SIM_K), (scenario) =>
          run(scenario, open, stats),
        ),
        { numRuns: RUNS, seed: 0x0f0_0001 },
      );
      expect(stats.checks).toBeGreaterThan(0);
    });
  },
);
