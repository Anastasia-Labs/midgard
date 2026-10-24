import { afterAll, describe, expect, it } from "vitest";

import { openPostgresFactStore } from "../src/index.js";
import { matching } from "./support/matchers.js";
import { testDatabases } from "./support/postgres.js";
import { runProperty } from "./support/property.js";

const OPS = Number(process.env.L1_FOLLOWER_PROPERTY_OPS ?? "10000");
const databases = testDatabases();

afterAll(async () => {
  await databases.dropAll();
});

describe("fact-store property test (Postgres adapter)", () => {
  it(`equals a fresh replay after each of ${OPS} random apply/rollback steps`, async () => {
    const database = await databases.create();
    const outcome = await runProperty({
      open: async (optionsFor) =>
        openPostgresFactStore({
          ...optionsFor("postgres"),
          connection: { connectionString: database.url },
        }),
      seed: 0x5eed_0002,
      ops: OPS,
      k: 8,
    });
    console.info("postgres property", JSON.stringify(outcome));
    expect(outcome).toMatchObject({ ok: true });
  });

  it("catches a rewind that cuts the temporal tables one slot too low", async () => {
    const database = await databases.create();
    const outcome = await runProperty({
      open: async (optionsFor) =>
        openPostgresFactStore({
          ...optionsFor("postgres"),
          connection: { connectionString: database.url },
        }),
      seed: 0x5eed_0003,
      ops: 2_000,
      k: 8,
      fault: "temporal_cut_below_target",
    });
    console.info("postgres broken rewind", JSON.stringify(outcome));
    expect(outcome).toMatchObject({
      ok: false,
      reason: matching(/differs from a fresh replay: fixture_/u),
    });
  });
});

describe("fact-store property test with interleaved pruning (Postgres adapter)", () => {
  it("matches a fresh replay inside the retained window and keeps INV1-INV6", async () => {
    const database = await databases.create();
    const outcome = await runProperty({
      open: async (optionsFor) =>
        openPostgresFactStore({
          ...optionsFor("postgres"),
          connection: { connectionString: database.url },
        }),
      seed: 0x5eed_0006,
      ops: 3_000,
      k: 8,
      pruneEvery: 3,
      pruneBudget: 7,
      rollbackProbability: 0.15,
    });
    console.info("postgres prune property", JSON.stringify(outcome));
    expect(outcome).toMatchObject({ ok: true });
  });
});
