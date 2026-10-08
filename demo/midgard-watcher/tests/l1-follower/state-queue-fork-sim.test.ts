import {
  type FollowerProjection,
  openSqliteFactStore,
} from "@al-ft/midgard-l1-follower";
import {
  buildForkSteps,
  forkCorpus,
  type ForkRunOptions,
  type ForkScenario,
  runForkScenario,
  type SimOutput,
} from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  WATCHER_DA_ATTESTATIONS_TABLE,
  WATCHER_QUEUE_CHECKPOINTS_TABLE,
  WATCHER_QUEUE_OUTPUTS_TABLE,
  WATCHER_QUEUE_UNIT_HISTORY_TABLE,
  watcherProjection,
  type WatcherProjectionDeployment,
} from "../../src/l1-follower/projection.js";
import { createChainFollower } from "../support/l1-follower-chain-oracle.js";
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

/** The rollback bound the watcher's fork cases run with. */
const K = 6;
const D = SIM_WATCHER_DEPLOYMENT;

const protocolAddresses = [
  D.stateQueueSpend,
  D.correctionLockSpend,
  D.hubOracleMint,
]
  .map((hash) => Buffer.concat([Buffer.of(0x70), Buffer.from(hash, "hex")]))
  .concat([SIM_DA_ATTESTATION_ADDRESS]);

/** The protocol's own outputs: no third party spends them. */
const isProtocolOutput = (output: SimOutput): boolean =>
  protocolAddresses.some((address) => address.equals(output.address)) &&
  output.assets !== undefined;

const TRAFFIC = {
  commitChance: 0.6,
  attestChance: 0.4,
  mergeChance: 0.15,
} as const;

const projection = (
  seen: Coverage,
  options: Readonly<{
    deployment?: WatcherProjectionDeployment;
    traffic?: Parameters<typeof stateQueueTraffic>[0];
    edit?: (base: FollowerProjection) => FollowerProjection;
  }> = {},
): FollowerProjection => {
  const base: FollowerProjection = {
    ...watcherProjection(options.deployment ?? D),
    traffic: stateQueueTraffic(options.traffic ?? TRAFFIC),
    protects: isProtocolOutput,
    check: watcherCheck(seen),
  };
  return options.edit?.(base) ?? base;
};

const openSqlite: ForkRunOptions["open"] = (optionsFor) =>
  Promise.resolve(
    openSqliteFactStore({ ...optionsFor("sqlite"), path: ":memory:" }),
  );

const run = (scenario: ForkScenario, projections: FollowerProjection[]) =>
  runForkScenario(scenario, { open: openSqlite, k: K, projections });

const failure = (outcome: Awaited<ReturnType<typeof run>>): string =>
  outcome.ok ? "ok" : `step ${outcome.step}: ${outcome.reason}`;

/** A long run that prunes after every depth-k fork: headers stay queued for many k. */
const LONG_PRUNED: ForkScenario = {
  seed: 4242,
  episodes: Array.from({ length: 10 }, (_, n) => ({
    shape: (["reland", "never_reland", "new_fork_only"] as const)[n % 3]!,
    depth: n % 2 === 0 ? K : 2,
    extra: 1,
    landAt: n % K,
    variant: n % 2,
    lead: K,
    prune: true,
  })),
};

/** What the traffic put on the canonical chains of the corpus. */
const trafficCounts = (scenarios: readonly ForkScenario[]) => {
  const counts = { inits: 0, appends: 0, merges: 0, attests: 0, applies: 0 };
  const seen = coverage();
  for (const scenario of scenarios)
    for (const { event } of buildForkSteps(scenario, [projection(seen)])
      .steps) {
      if (event.kind !== "roll_forward") continue;
      const follower = createChainFollower();
      for (const tx of follower.follow({ event }).at(-1)?.txs ?? []) {
        for (const [name, quantity] of tx.mint.get(D.stateQueueMint) ?? [])
          if (name === SDK.STATE_QUEUE_ROOT_ASSET_NAME) counts.inits += 1;
          else if (quantity > 0n) counts.appends += 1;
          else counts.merges += 1;
        for (const [, quantity] of tx.mint.get(D.daAttestationMint) ?? [])
          if (quantity > 0n) counts.attests += 1;
          else counts.applies += 1;
      }
    }
  return counts;
};

const corpus = forkCorpus(K);

describe("watcher projection on the fork simulator: fresh replay + independent checks", () => {
  it("the traffic queues, attests, applies and merges across the corpus", () => {
    const counts = trafficCounts([
      ...corpus.map(({ scenario }) => scenario),
      LONG_PRUNED,
    ]);
    expect(counts.inits).toBeGreaterThan(corpus.length / 2);
    expect(counts.appends).toBeGreaterThan(corpus.length);
    expect(counts.merges).toBeGreaterThan(5);
    expect(counts.attests).toBeGreaterThan(5);
    expect(counts.applies).toBeGreaterThan(5);
  });

  for (const { name, scenario } of corpus)
    it(`equals a fresh replay and the model after every event: ${name}`, async () => {
      const outcome = await run(scenario, [projection(coverage())]);
      expect(failure(outcome)).toBe("ok");
    });

  it("holds the ruling-2 pins through many pruned depth-k forks", async () => {
    const seen = coverage();
    const outcome = await run(LONG_PRUNED, [projection(seen)]);
    expect(failure(outcome)).toBe("ok");
    expect(outcome.stats.prunes).toBeGreaterThan(5);
    expect(outcome.stats.prunedRows).toBeGreaterThan(0);
    expect(outcome.stats.rollbacks).toBeGreaterThan(5);
    // Reads a live decision needs, of txs pruning would otherwise reach.
    expect(seen.pinnedBelowWindow).toBeGreaterThan(20);
    expect(seen.attestationReads).toBeGreaterThan(5);
    // Merged headers are released once their removal is k deep.
    expect(seen.mergedHeaderReads).toBeGreaterThan(0);
    expect(seen.mergedHeadersPruned).toBeGreaterThan(0);
  });

  it("the checks see queues, locks, checkpoints and matched transitions", async () => {
    const seen = coverage();
    for (const { scenario } of corpus.slice(-4))
      expect(failure(await run(scenario, [projection(seen)]))).toBe("ok");
    expect(seen.queuedHeaders).toBeGreaterThan(50);
    expect(seen.locks).toBeGreaterThan(50);
    expect(seen.checkpointRows).toBeGreaterThan(20);
    expect(seen.transitionsMatched).toBeGreaterThan(10);
  });

  describe("the gate catches a wrong projection", () => {
    const sequence = corpus.find(
      ({ name }) => name === "every shape in sequence",
    )?.scenario as ForkScenario;

    const withHook = (
      apply: (
        hook: NonNullable<FollowerProjection["derivations"]>[number],
      ) => NonNullable<FollowerProjection["derivations"]>[number],
    ) =>
      projection(coverage(), {
        edit: (base) => ({
          ...base,
          derivations: base.derivations?.map((hook) =>
            hook.name === "watcher_state_queue" ? apply(hook) : hook,
          ),
        }),
      });

    it("one that never closes a spent output (the model's queue)", async () => {
      const outcome = await run(sequence, [
        withHook((hook) => ({
          ...hook,
          apply: async (context) => {
            await hook.apply(context);
            await context.tx.query(
              `UPDATE ${WATCHER_QUEUE_OUTPUTS_TABLE} SET to_slot = NULL`,
              [],
            );
          },
        })),
      ]);
      expect(failure(outcome)).toMatch(/view unhealthy|queue/u);
    });

    it("one that reads the lock at another credential (the model's lock)", async () => {
      const outcome = await run(sequence, [
        projection(coverage(), {
          deployment: { ...D, correctionLockSpend: "6f".repeat(28) },
          traffic: { ...TRAFFIC, deployment: D },
        }),
      ]);
      expect(failure(outcome)).toContain("lock");
    });

    it("one whose derivation differs on replay (the fresh-replay diff)", async () => {
      let calls = 0;
      const outcome = await run(sequence, [
        withHook((hook) => ({
          ...hook,
          apply: async (context) => {
            await hook.apply(context);
            calls += 1;
            if (calls === 40)
              await context.tx.query(
                `DELETE FROM ${WATCHER_QUEUE_UNIT_HISTORY_TABLE} WHERE to_slot IS NULL`,
                [],
              );
          },
        })),
      ]);
      expect(failure(outcome)).toMatch(
        /differs from a fresh replay|unit history/u,
      );
    });

    it("one without the unit-history tx pin", async () => {
      const outcome = await run(LONG_PRUNED, [
        projection(coverage(), {
          edit: (base) => ({
            ...base,
            retentionPins: {
              txs: base.retentionPins?.txs?.filter(
                (pin) => pin.table !== WATCHER_QUEUE_UNIT_HISTORY_TABLE,
              ),
            },
          }),
        }),
      ]);
      expect(failure(outcome)).toMatch(/history tx .*: beyond_retention/u);
    });

    it("one without the DAAT tx pin", async () => {
      const outcome = await run(LONG_PRUNED, [
        projection(coverage(), {
          edit: (base) => ({
            ...base,
            retentionPins: {
              txs: base.retentionPins?.txs?.filter(
                (pin) => pin.table !== WATCHER_DA_ATTESTATIONS_TABLE,
              ),
            },
          }),
        }),
      ]);
      expect(failure(outcome)).toMatch(/DAAT tx/u);
    });

    it("one without the checkpoint row pin", async () => {
      const outcome = await run(LONG_PRUNED, [
        projection(coverage(), {
          edit: (base) => ({
            ...base,
            temporalTables: base.temporalTables?.map((table) =>
              table.name === WATCHER_QUEUE_CHECKPOINTS_TABLE
                ? { ...table, pinnedBy: [] }
                : table,
            ),
          }),
        }),
      ]);
      expect(failure(outcome)).toMatch(/checkpoint of .*: beyond_retention/u);
    });
  });
});
