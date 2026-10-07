import fc from "fast-check";
import { describe, expect, it } from "vitest";

import { rewindFaults } from "../../src/store/fact-store.js";
import type { RewindFault } from "../../src/store/rewind.js";
import {
  forkCorpus,
  forkScenarioArbitrary,
  runForkScenario,
} from "../../src/testing/index.js";
import { FIXTURE_PROJECTION, openSqlite, SIM_K } from "../support/fork-sim.js";

const RUNS = Number(process.env.L1_FORK_SIM_RUNS ?? "200");
const corpus = forkCorpus(SIM_K);

describe("fork simulator over the sequential writer (SQLite)", () => {
  it.each(corpus.map((entry) => [entry.name, entry.scenario] as const))(
    "corpus: %s",
    async (_, scenario) => {
      const outcome = await runForkScenario(scenario, {
        open: openSqlite,
        k: SIM_K,
        projections: [FIXTURE_PROJECTION],
      });
      expect(outcome).toMatchObject({ ok: true });
      expect(outcome.stats.rollbacks).toBe(scenario.episodes.length);
      expect(outcome.stats.checkpoints).toBe(2 * scenario.episodes.length);
    },
  );

  it(`holds for ${RUNS} random scenarios (fast-check)`, async () => {
    let rollbacks = 0;
    await fc.assert(
      fc.asyncProperty(forkScenarioArbitrary(SIM_K), async (scenario) => {
        const outcome = await runForkScenario(scenario, {
          open: openSqlite,
          k: SIM_K,
          projections: [FIXTURE_PROJECTION],
        });
        if (!outcome.ok)
          throw new Error(`step ${outcome.step}: ${outcome.reason}`);
        rollbacks += outcome.stats.rollbacks;
      }),
      { numRuns: RUNS, seed: 0x0f8_5eed },
    );
    expect(rollbacks).toBeGreaterThanOrEqual(RUNS);
  });

  // Red checks: each seeded rewind fault must fail the corpus somewhere,
  // so a green run means the simulator can see that class of bug.
  it.each<RewindFault>([
    "skip_temporal_truncation",
    "temporal_cut_below_target",
    "skip_unspend_cache_patch",
  ])("fails when rewind has the %s fault", async (fault) => {
    const failures: string[] = [];
    for (const { scenario } of corpus) {
      const outcome = await runForkScenario(scenario, {
        open: openSqlite,
        k: SIM_K,
        projections: [FIXTURE_PROJECTION],
        prepare: (store) => rewindFaults.set(store, fault),
      });
      if (!outcome.ok) failures.push(outcome.reason);
    }
    expect(failures.length).toBeGreaterThan(0);
    console.info(
      `${fault}: ${failures.length}/${corpus.length} fail; first: ${failures[0]}`,
    );
  });

  it("fails when a plugged projection's own check fails", async () => {
    const outcome = await runForkScenario(corpus[0]!.scenario, {
      open: openSqlite,
      k: SIM_K,
      projections: [
        {
          ...FIXTURE_PROJECTION,
          check: async ({ step }) =>
            Promise.resolve(
              step.event.kind === "roll_backward" ? "saw a rollback" : null,
            ),
        },
      ],
    });
    expect(outcome).toMatchObject({
      ok: false,
      reason: "fixture: saw a rollback",
    });
  });

  it("fails when a comparator disagrees", async () => {
    let calls = 0;
    const outcome = await runForkScenario(corpus[0]!.scenario, {
      open: openSqlite,
      k: SIM_K,
      comparators: [
        {
          role: "committee",
          name: "drifting",
          projected: async ({ at }) =>
            Promise.resolve({ kind: "value", value: { height: at.height } }),
          current: async ({ at }) => {
            calls += 1;
            return Promise.resolve({
              kind: "value",
              value: { height: calls > 3 ? at.height + 1 : at.height },
            });
          },
        },
      ],
    });
    expect(outcome.ok).toBe(false);
    expect(outcome).toMatchObject({ step: 3 });
    expect(!outcome.ok && outcome.reason).toContain(
      "committee/drifting differs",
    );
  });
});
