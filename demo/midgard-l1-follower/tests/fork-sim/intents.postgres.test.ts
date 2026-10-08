import fc from "fast-check";
import { afterAll, describe, expect, it } from "vitest";

import { openPostgresFactStore } from "../../src/index.js";
import {
  forkCorpus,
  type ForkRunOptions,
  forkScenarioArbitrary,
  runForkScenario,
} from "../../src/testing/index.js";
import { SIM_K } from "../support/fork-sim.js";
import {
  type IntentSimStats,
  intentSimulation,
} from "../support/intent-sim.js";
import { testDatabases } from "../support/postgres.js";

const RUNS = Number(process.env.L1_FORK_SIM_INTENT_POSTGRES_RUNS ?? "10");
const databases = testDatabases();

afterAll(async () => {
  await databases.dropAll();
});

const openPostgres: ForkRunOptions["open"] = async (optionsFor) => {
  const database = await databases.create();
  return openPostgresFactStore({
    ...optionsFor("postgres"),
    connection: { connectionString: database.url },
  });
};

/** The SQLite suite's §15 I1 checks over the Postgres store (the node's). */
describe("intent journal in the fork simulator (Postgres)", () => {
  it("holds over the corpus and random scenarios", async () => {
    const all: IntentSimStats[] = [];
    for (const { name, scenario } of forkCorpus(SIM_K)) {
      const sim = intentSimulation();
      const outcome = await runForkScenario(scenario, {
        open: openPostgres,
        k: SIM_K,
        projections: [sim.projection],
      });
      if (!outcome.ok)
        throw new Error(`${name}, step ${outcome.step}: ${outcome.reason}`);
      all.push(sim.stats);
    }
    await fc.assert(
      fc.asyncProperty(forkScenarioArbitrary(SIM_K), async (scenario) => {
        const sim = intentSimulation();
        const outcome = await runForkScenario(scenario, {
          open: openPostgres,
          k: SIM_K,
          projections: [sim.projection],
        });
        if (!outcome.ok)
          throw new Error(`step ${outcome.step}: ${outcome.reason}`);
        all.push(sim.stats);
      }),
      { numRuns: RUNS, seed: 0x01_1002 },
    );
    expect(all.reduce((n, s) => n + s.resubmitted, 0)).toBeGreaterThan(0);
    expect(all.reduce((n, s) => n + s.pruned, 0)).toBeGreaterThan(0);
    expect(all.reduce((n, s) => n + s.unlandedToLive, 0)).toBeGreaterThan(0);
  }, 900_000);
});
