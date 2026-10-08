import fc from "fast-check";
import { describe, expect, it } from "vitest";

import {
  forkCorpus,
  forkScenarioArbitrary,
  runForkScenario,
} from "../../src/testing/index.js";
import { openSqlite, SIM_K } from "../support/fork-sim.js";
import {
  type IntentSimStats,
  intentSimulation,
} from "../support/intent-sim.js";

const RUNS = Number(process.env.L1_FORK_SIM_INTENT_RUNS ?? "40");

const total = (all: readonly IntentSimStats[]) => ({
  recorded: all.reduce((n, s) => n + s.recorded, 0),
  resubmitted: all.reduce((n, s) => n + s.resubmitted, 0),
  abandoned: all.reduce((n, s) => n + s.abandoned, 0),
  unlandedToLive: all.reduce((n, s) => n + s.unlandedToLive, 0),
  pruned: all.reduce((n, s) => n + s.pruned, 0),
  superseded: all.reduce((n, s) => n + s.superseded, 0),
  staleAtWrite: all.reduce((n, s) => n + s.staleAtWrite, 0),
  recordedAcrossRewind: all.reduce((n, s) => n + s.recordedAcrossRewind, 0),
  submitHeld: all.reduce((n, s) => n + s.submitHeld, 0),
  sentAcrossRewind: all.reduce((n, s) => n + s.sentAcrossRewind, 0),
  seen: (kind: string) => all.reduce((n, s) => n + (s.seen[kind] ?? 0), 0),
});

/**
 * §15 I1 on the F8 simulator (SQLite): after every event the derived intent
 * statuses equal those over a fresh replay, with prune on; a rollback
 * writes nothing to the journal (a landed intent it un-lands is live
 * again); S6 sends only a live intent's journaled bytes, never a dead one.
 * §15 I5: a rewind between plan and record makes the record
 * `stale_at_write` exactly when the planned view is gone, and a rewind
 * between record and submit holds the send exactly then; a view whose
 * point survived an unrelated rewind is accepted at both.
 */
describe("intent journal in the fork simulator (SQLite)", () => {
  it("holds over the corpus and random scenarios, reaching every status", async () => {
    const all: IntentSimStats[] = [];
    for (const { name, scenario } of forkCorpus(SIM_K)) {
      const sim = intentSimulation();
      const outcome = await runForkScenario(scenario, {
        open: openSqlite,
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
          open: openSqlite,
          k: SIM_K,
          projections: [sim.projection],
        });
        if (!outcome.ok)
          throw new Error(`step ${outcome.step}: ${outcome.reason}`);
        all.push(sim.stats);
      }),
      { numRuns: RUNS, seed: 0x01_15eed },
    );
    const t = total(all);
    console.info(
      JSON.stringify({
        ...t,
        seen: Object.fromEntries(
          [
            "landed",
            "failed_landed",
            "conflicted",
            "expired",
            "dependency_dead",
            "abandoned",
            "live",
          ].map((kind) => [kind, t.seen(kind)]),
        ),
      }),
    );
    expect(t.recorded).toBeGreaterThan(0);
    expect(t.resubmitted).toBeGreaterThan(0);
    expect(t.abandoned).toBeGreaterThan(0);
    expect(t.unlandedToLive).toBeGreaterThan(0);
    expect(t.pruned).toBeGreaterThan(0);
    expect(t.superseded).toBeGreaterThan(0);
    expect(t.staleAtWrite).toBeGreaterThan(0);
    expect(t.recordedAcrossRewind).toBeGreaterThan(0);
    expect(t.submitHeld).toBeGreaterThan(0);
    expect(t.sentAcrossRewind).toBeGreaterThan(0);
    for (const kind of [
      "landed",
      "failed_landed",
      "conflicted",
      "expired",
      "dependency_dead",
      "abandoned",
      "live",
    ])
      expect(t.seen(kind), kind).toBeGreaterThan(0);
  }, 600_000);
});
