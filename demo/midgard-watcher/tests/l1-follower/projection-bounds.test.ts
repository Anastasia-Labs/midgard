/**
 * Two bounds of the watcher projection on the fork simulator (ticket W1):
 * a DA attestation for a header that was never queued opens no row (so
 * nothing pins its tx past k), and a protocol-init root away from the
 * state-queue address keeps the view unhealthy with a named reason for as
 * long as the init is on the canonical chain.
 */
import {
  type FactStore,
  type FollowerProjection,
  openSqliteFactStore,
} from "@al-ft/midgard-l1-follower";
import {
  forkCorpus,
  type ForkRunOptions,
  type ForkScenario,
  type ForkStep,
  runForkScenario,
  type SimOutput,
} from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  WATCHER_DA_ATTESTATIONS_TABLE,
  watcherProjection,
} from "../../src/l1-follower/projection.js";
import { readWatcherQueueView } from "../../src/l1-follower/view.js";
import {
  createChainFollower,
  modelOf,
} from "../support/l1-follower-chain-oracle.js";
import {
  SIM_DA_ATTESTATION_ADDRESS,
  SIM_WATCHER_DEPLOYMENT,
} from "../support/l1-follower-state-queue-traffic.js";
import { stateQueueTraffic } from "../support/l1-follower-state-queue-traffic.scenario.js";
import {
  coverage,
  watcherCheck,
} from "../support/l1-follower-watcher-check.js";

const K = 6;
const D = SIM_WATCHER_DEPLOYMENT;
const NODE_UNIT = `${D.stateQueueMint}${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}`;
const DAAT_UNIT = `${D.daAttestationMint}${SDK.DA_ATTESTATION_ASSET_NAME_PREFIX}`;
const QUEUE_ADDRESS = Buffer.concat([
  Buffer.of(0x70),
  Buffer.from(D.stateQueueSpend, "hex"),
]);
/** A root output at another script address. */
const ELSEWHERE = Buffer.concat([Buffer.of(0x70), Buffer.alloc(28, 0x7e)]);

const protocolAddresses = [D.correctionLockSpend, D.hubOracleMint]
  .map((hash) => Buffer.concat([Buffer.of(0x70), Buffer.from(hash, "hex")]))
  .concat([QUEUE_ADDRESS, SIM_DA_ATTESTATION_ADDRESS, ELSEWHERE]);

/** The protocol's own outputs: no third party spends them. */
const protects = (output: SimOutput): boolean =>
  protocolAddresses.some((address) => address.equals(output.address)) &&
  output.assets !== undefined;

const openSqlite: ForkRunOptions["open"] = (optionsFor) =>
  Promise.resolve(
    openSqliteFactStore({ ...optionsFor("sqlite"), path: ":memory:" }),
  );

const failure = (outcome: Awaited<ReturnType<typeof runForkScenario>>) =>
  outcome.ok ? "ok" : `step ${outcome.step}: ${outcome.reason}`;

type Check = NonNullable<FollowerProjection["check"]>;
type CheckArgs = Readonly<{ store: FactStore; step: ForkStep }>;

const LONG_PRUNED: ForkScenario = {
  seed: 9191,
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

describe("a DA attestation for a header that was never queued", () => {
  const strays = { minted: 0, prunedBelowWindow: 0 };

  /** Every DA row names a header the canonical chain queued; stray DAAT txs are pruned past k. */
  const strayCheck = (): Check => {
    const follower = createChainFollower();
    return async ({ store, step }: CheckArgs) => {
      const model = modelOf(follower.follow(step));
      const cursor = await store.cursor();
      if (cursor === null) return null;
      const rows = await store.transaction("read", (tx) =>
        tx.query(
          `SELECT header_hash, tx_hash FROM ${WATCHER_DA_ATTESTATIONS_TABLE}`,
          [],
        ),
      );
      for (const row of rows) {
        const header = Buffer.from(row.header_hash as Uint8Array).toString(
          "hex",
        );
        if (!model.unitHistory.has(`${NODE_UNIT}${header}`))
          return `DA row for never-queued header ${header}`;
      }
      for (const [unit, entries] of model.unitHistory) {
        if (!unit.startsWith(DAAT_UNIT)) continue;
        const header = unit.slice(DAAT_UNIT.length);
        if (model.unitHistory.has(`${NODE_UNIT}${header}`)) continue;
        for (const { tx, block } of entries) {
          strays.minted += 1;
          if (block.point.slot > cursor.prunedThroughSlot) continue;
          if ((await store.txByHash(tx.hash)) !== null)
            return `stray DAAT tx ${tx.hash.toString("hex")} is still stored past the pruned window`;
          strays.prunedBelowWindow += 1;
        }
      }
      return null;
    };
  };

  const projection = (
    edit?: (base: FollowerProjection) => FollowerProjection,
  ) => {
    const seen = coverage();
    const own = watcherCheck(seen);
    const stray = strayCheck();
    const base: FollowerProjection = {
      ...watcherProjection(D),
      traffic: stateQueueTraffic({
        commitChance: 0.6,
        attestChance: 0.4,
        mergeChance: 0.15,
        strayAttestChance: 0.3,
      }),
      protects,
      check: async (args) => (await own(args)) ?? (await stray(args)),
    };
    return edit?.(base) ?? base;
  };

  it("opens no row and its tx is pruned once k deep", async () => {
    const outcome = await runForkScenario(LONG_PRUNED, {
      open: openSqlite,
      k: K,
      projections: [projection()],
    });
    expect(failure(outcome)).toBe("ok");
    expect(strays.minted).toBeGreaterThan(20);
    expect(strays.prunedBelowWindow).toBeGreaterThan(5);
  });

  it("the check catches a projection that records it", async () => {
    const outcome = await runForkScenario(LONG_PRUNED, {
      open: openSqlite,
      k: K,
      projections: [
        projection((base) => ({
          ...base,
          derivations: base.derivations?.map((hook) =>
            hook.name !== "watcher_state_queue"
              ? hook
              : {
                  ...hook,
                  apply: async (context) => {
                    await hook.apply(context);
                    for (const { tx } of context.qualified)
                      for (const output of tx.outputs)
                        for (const name of output.assets
                          .get(D.daAttestationMint)
                          ?.keys() ?? [])
                          await context.tx.query(
                            `INSERT INTO ${WATCHER_DA_ATTESTATIONS_TABLE} (header_hash, tx_hash, block_height, from_slot, to_slot) VALUES (?, ?, ?, ?, NULL) ON CONFLICT DO NOTHING`,
                            [
                              Buffer.from(
                                name.slice(
                                  SDK.DA_ATTESTATION_ASSET_NAME_PREFIX.length,
                                ),
                                "hex",
                              ),
                              tx.hash,
                              context.block.height,
                              context.block.point.slot,
                            ],
                          );
                  },
                },
          ),
        })),
      ],
    });
    expect(failure(outcome)).toMatch(/never-queued header/u);
  });
});

describe("a protocol-init root away from the state-queue address", () => {
  const sequence = forkCorpus(K).find(
    ({ name }) => name === "every shape in sequence",
  )?.scenario as ForkScenario;

  /** The view names the misplaced root exactly while a misplaced init is canonical. */
  const misplacedCheck = (seen: {
    unhealthy: number;
    healthy: number;
    cleared: number;
  }): Check => {
    const follower = createChainFollower();
    let previous = false;
    return async ({ store, step }: CheckArgs) => {
      const blocks = follower.follow(step);
      const cursor = await store.cursor();
      if (cursor === null) return null;
      const misplaced = blocks.some((block) =>
        block.txs.some((tx) =>
          tx.outputs.some(
            (output) =>
              output.assets
                .get(D.stateQueueMint)
                ?.has(SDK.STATE_QUEUE_ROOT_ASSET_NAME) === true &&
              !output.address.equals(QUEUE_ADDRESS),
          ),
        ),
      );
      const view = await store.transaction("read", (tx) =>
        readWatcherQueueView(tx, cursor.point.slot),
      );
      const named =
        !view.healthy &&
        view.reason === "protocol_init_root_not_at_state_queue";
      if (misplaced !== named)
        return `misplaced root on chain ${String(misplaced)}, view ${view.healthy ? "healthy" : view.reason}`;
      if (named) seen.unhealthy += 1;
      else seen.healthy += 1;
      if (previous && !named) seen.cleared += 1;
      previous = named;
      return null;
    };
  };

  /** A rollback deep enough to remove the first blocks, where the init lands. */
  const earlyRollback = (seed: number): ForkScenario => ({
    seed,
    episodes: [0, 1].map((n) => ({
      shape: "new_fork_only" as const,
      depth: K,
      extra: 1,
      landAt: 0,
      variant: n,
      lead: 0,
    })),
  });

  it("keeps the view unhealthy with the named reason while the init is canonical", async () => {
    const seen = { unhealthy: 0, healthy: 0, cleared: 0 };
    for (const scenario of [
      sequence,
      ...[1, 2, 3, 4, 5, 6].map(earlyRollback),
    ]) {
      const outcome = await runForkScenario(scenario, {
        open: openSqlite,
        k: K,
        projections: [
          {
            ...watcherProjection(D),
            traffic: stateQueueTraffic({ rootAddress: ELSEWHERE }),
            protects,
            check: misplacedCheck(seen),
          },
        ],
      });
      expect(failure(outcome)).toBe("ok");
    }
    expect(seen.unhealthy).toBeGreaterThan(10);
    // A rollback that removes the misplaced init clears the reason.
    expect(seen.cleared).toBeGreaterThan(0);
  });

  it("a root at the state-queue address never trips it", async () => {
    const seen = { unhealthy: 0, healthy: 0, cleared: 0 };
    const outcome = await runForkScenario(sequence, {
      open: openSqlite,
      k: K,
      projections: [
        {
          ...watcherProjection(D),
          traffic: stateQueueTraffic({}),
          protects,
          check: misplacedCheck(seen),
        },
      ],
    });
    expect(failure(outcome)).toBe("ok");
    expect(seen.unhealthy).toBe(0);
    expect(seen.healthy).toBeGreaterThan(10);
  });
});
