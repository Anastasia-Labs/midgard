/**
 * The watcher's whole follower store under the fork simulator: the
 * state-queue projection and the deposit and withdrawal event projection,
 * composed as the runtime composes them (`watcherFollowerProjections`), with
 * both traffics on one chain. After every chain-sync event every table of
 * both projections equals a fresh replay's (the runner's check), and each
 * projection agrees with its independent model of the canonical chain.
 */
import {
  type FollowerProjection,
  openSqliteFactStore,
} from "@al-ft/midgard-l1-follower";
import {
  forkCorpus,
  type ForkRunOptions,
  type ForkScenario,
  runForkScenario,
  type SimOutput,
} from "@al-ft/midgard-l1-follower/testing";
import { EVENTS_CONFIG } from "midgard-node/tests/helpers/l1-events-chain";
import {
  eventSimProjection,
  type EventSimStats,
  zeroEventSimStats,
} from "midgard-node/tests/helpers/l1-events-sim";
import { describe, expect, it } from "vitest";

import { watcherFollowerProjections } from "../../src/l1-follower/follower-runtime.js";
import {
  SIM_DA_ATTESTATION_ADDRESS,
  SIM_WATCHER_DEPLOYMENT,
} from "../support/l1-follower-state-queue-traffic.js";
import { stateQueueTraffic } from "../support/l1-follower-state-queue-traffic.scenario.js";
import {
  type Coverage,
  coverage,
  watcherCheck,
} from "../support/l1-follower-watcher-check.js";

const K = 6;
const D = SIM_WATCHER_DEPLOYMENT;

const protocolAddresses = [
  D.stateQueueSpend,
  D.correctionLockSpend,
  D.hubOracleMint,
]
  .map((hash) => Buffer.concat([Buffer.of(0x70), Buffer.from(hash, "hex")]))
  .concat([SIM_DA_ATTESTATION_ADDRESS]);

const isProtocolOutput = (output: SimOutput): boolean =>
  protocolAddresses.some((address) => address.equals(output.address)) &&
  output.assets !== undefined;

/**
 * The runtime's projections, each given its simulator traffic and model
 * check. The event projection is the runtime's own; only the simulator
 * hooks come from the node's event simulation.
 */
const watcherStore = (
  seen: Coverage,
  stats: EventSimStats,
): FollowerProjection[] => {
  const composed = watcherFollowerProjections(D, EVENTS_CONFIG);
  expect(composed).toHaveLength(2);
  const [queue, events] = composed as [FollowerProjection, FollowerProjection];
  const sim = eventSimProjection(stats);
  return [
    {
      ...queue,
      traffic: stateQueueTraffic({
        commitChance: 0.6,
        attestChance: 0.4,
        mergeChance: 0.15,
      }),
      protects: isProtocolOutput,
      check: watcherCheck(seen),
    },
    {
      ...events,
      traffic: sim.traffic,
      protects: sim.protects,
      check: sim.check,
    },
  ];
};

const openSqlite: ForkRunOptions["open"] = (optionsFor) =>
  Promise.resolve(
    openSqliteFactStore({ ...optionsFor("sqlite"), path: ":memory:" }),
  );

const run = async (
  scenario: ForkScenario,
  projections: FollowerProjection[],
): Promise<string> => {
  const outcome = await runForkScenario(scenario, {
    open: openSqlite,
    k: K,
    projections,
  });
  return outcome.ok ? "ok" : `step ${outcome.step}: ${outcome.reason}`;
};

/** Pruned depth-k forks: events retire, are pruned, and keys come back. */
const LONG: ForkScenario = {
  seed: 0x0e3,
  episodes: Array.from({ length: 8 }, (_, n) => ({
    shape: (
      ["reland", "never_reland", "changed_valid_to", "new_fork_only"] as const
    )[n % 4]!,
    depth: n % 2 === 0 ? K : 1 + (n % K),
    extra: 1 + (n % 2),
    landAt: n % K,
    variant: n,
    lead: K,
    prune: true,
  })),
};

describe("the watcher's follower store on the fork simulator: state queue and user events", () => {
  it("both projections equal a fresh replay and their models after every event", async () => {
    const seen = coverage();
    const stats = zeroEventSimStats();
    for (const { name, scenario } of forkCorpus(K))
      expect({
        name,
        outcome: await run(scenario, watcherStore(seen, stats)),
      }).toEqual({ name, outcome: "ok" });
    expect(await run(LONG, watcherStore(seen, stats))).toBe("ok");
    // Both traffics ran on the same chains and both checks saw their cases.
    expect(seen.queuedHeaders).toBeGreaterThan(20);
    expect(stats.checks).toBeGreaterThan(0);
    for (const field of [
      "admissions",
      "retirements",
      "readmissions",
      "retiredKeyRefusals",
      "prunedRetirements",
    ] as const)
      expect({ field, seen: stats[field] > 0 }).toEqual({ field, seen: true });
  });

  it("refuses an event projection that drifts from a fresh replay", async () => {
    const seen = coverage();
    const stats = zeroEventSimStats();
    const [queue, events] = watcherStore(seen, stats) as [
      FollowerProjection,
      FollowerProjection,
    ];
    let calls = 0;
    const drifting: FollowerProjection = {
      ...events,
      derivations: events.derivations?.map((hook) => ({
        ...hook,
        apply: async (context) => {
          await hook.apply(context);
          calls += 1;
          if (calls === 30)
            for (const table of events.temporalTables ?? [])
              await context.tx.query(`DELETE FROM ${table.name}`, []);
        },
      })),
    };
    const sequence = forkCorpus(K).find(
      ({ name }) => name === "every shape in sequence",
    )!.scenario;
    expect(await run(sequence, [queue, drifting])).toMatch(
      /differs from a fresh replay|events|refusals|due|spendable/u,
    );
  });
});
