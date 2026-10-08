import { openPostgresFactStore } from "@al-ft/midgard-l1-follower";
import {
  forkCorpus,
  type ForkRunOptions,
  runForkScenario,
} from "@al-ft/midgard-l1-follower/testing";
import { afterAll, describe, expect, it } from "vitest";

import { postgresTestDatabases } from "../helpers/postgres-database.js";
import {
  committeeForkCorpus,
  committeeSimProjection,
  SIM_K,
  zeroStats,
} from "./queue-sim.js";

const databases = postgresTestDatabases("midgard_test_c1_fork_sim");

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

// The committee's follower tables run on Postgres in production; the
// corpus runs here too so the Postgres dialect of the migrations, the
// derivation and the reads is held to the same oracle.
describe("committee projections in the fork simulator (Postgres)", () => {
  it.each(
    [...forkCorpus(SIM_K), ...committeeForkCorpus()].map(
      (entry) => [entry.name, entry.scenario] as const,
    ),
  )("corpus: %s", async (_, scenario) => {
    const stats = zeroStats();
    const outcome = await runForkScenario(scenario, {
      open: openPostgres,
      k: SIM_K,
      projections: [committeeSimProjection(stats, { expectHealthy: true })],
    });
    expect(
      outcome.ok ? "ok" : `step ${outcome.step.toString()}: ${outcome.reason}`,
    ).toBe("ok");
    expect(stats.steps).toBe(outcome.stats.events);
  });
});
