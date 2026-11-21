import fc from "fast-check";
import { afterAll, describe, expect, it } from "vitest";

import { openPostgresFactStore } from "../../src/index.js";
import {
  forkCorpus,
  type ForkRunOptions,
  forkScenarioArbitrary,
  runForkScenario,
} from "../../src/testing/index.js";
import { FIXTURE_PROJECTION, SIM_K } from "../support/fork-sim.js";
import { testDatabases } from "../support/postgres.js";

const RUNS = Number(process.env.L1_FORK_SIM_POSTGRES_RUNS ?? "20");
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

describe("fork simulator over the sequential writer (Postgres)", () => {
  it.each(
    forkCorpus(SIM_K).map((entry) => [entry.name, entry.scenario] as const),
  )("corpus: %s", async (_, scenario) => {
    const outcome = await runForkScenario(scenario, {
      open: openPostgres,
      k: SIM_K,
      projections: [FIXTURE_PROJECTION],
    });
    expect(outcome).toMatchObject({ ok: true });
    expect(outcome.stats.checkpoints).toBe(2 * scenario.episodes.length);
  });

  it(`holds for ${RUNS} random scenarios (fast-check)`, async () => {
    await fc.assert(
      fc.asyncProperty(forkScenarioArbitrary(SIM_K), async (scenario) => {
        const outcome = await runForkScenario(scenario, {
          open: openPostgres,
          k: SIM_K,
          projections: [FIXTURE_PROJECTION],
        });
        if (!outcome.ok)
          throw new Error(`step ${outcome.step}: ${outcome.reason}`);
      }),
      { numRuns: RUNS, seed: 0x0f8_0002 },
    );
  });
});
