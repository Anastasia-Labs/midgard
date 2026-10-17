/**
 * Departed headers on the fork simulator (ticket W1): the predecessor of
 * every queued header and the newest merged header stay resolvable at their
 * newest attested node after the queue rows are pruned, and the release
 * proofs of an older observation's headers name exactly the merges the
 * canonical chain made between that observation and the release-final
 * block, across rollbacks and pruning.
 */
import {
  type FactStore,
  type FollowerProjection,
  openSqliteFactStore,
  type OutputSummary,
} from "@al-ft/midgard-l1-follower";
import {
  type ForkRunOptions,
  type ForkScenario,
  type ForkStep,
  runForkScenario,
  type SimOutput,
} from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  type WatcherAuthenticatedStateQueueObservation,
  WatcherRetainedHeaderAttestationPendingError,
} from "../../src/indexers/authenticated-state-queue-observation.parse-persisted-header.js";
import { createWatcherQueueHeaderSource } from "../../src/l1-follower/observation.js";
import {
  WATCHER_DEPARTED_HEADERS_TABLE,
  watcherProjection,
} from "../../src/l1-follower/projection.js";
import {
  type ChainModel,
  createChainFollower,
  label,
  modelOf,
  modelQueue,
  type OracleEntry,
} from "../support/l1-follower-chain-oracle.js";
import {
  SIM_DA_ATTESTATION_ADDRESS,
  SIM_WATCHER_DEPLOYMENT,
} from "../support/l1-follower-state-queue-traffic.js";
import { stateQueueTraffic } from "../support/l1-follower-state-queue-traffic.scenario.js";

const K = 6;
/** The release depth the reads use (below k, as in production). */
const R = 3;
const D = SIM_WATCHER_DEPLOYMENT;
const NODE_UNIT = `${D.stateQueueMint}${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}`;
const QUEUE_ADDRESS = Buffer.concat([
  Buffer.of(0x70),
  Buffer.from(D.stateQueueSpend, "hex"),
]);

const protocolAddresses = [D.correctionLockSpend, D.hubOracleMint]
  .map((hash) => Buffer.concat([Buffer.of(0x70), Buffer.from(hash, "hex")]))
  .concat([QUEUE_ADDRESS, SIM_DA_ATTESTATION_ADDRESS]);

const protects = (output: SimOutput): boolean =>
  protocolAddresses.some((address) => address.equals(output.address)) &&
  output.assets !== undefined;

const openSqlite: ForkRunOptions["open"] = (optionsFor) =>
  Promise.resolve(
    openSqliteFactStore({ ...optionsFor("sqlite"), path: ":memory:" }),
  );

const failure = (outcome: Awaited<ReturnType<typeof runForkScenario>>) =>
  outcome.ok ? "ok" : `step ${outcome.step}: ${outcome.reason}`;

const SCENARIO: ForkScenario = {
  seed: 4242,
  episodes: Array.from({ length: 12 }, (_, n) => ({
    shape: (["reland", "never_reland", "new_fork_only"] as const)[n % 3]!,
    depth: n % 2 === 0 ? K : 2,
    extra: 1,
    landAt: n % K,
    variant: n % 2,
    lead: K + 2,
    prune: true,
  })),
};

type Seen = {
  predecessors: number;
  predecessorsPending: number;
  /** Predecessors resolved after their own queue rows left the queue. */
  departedPredecessors: number;
  newestMerges: number;
  mergedProofs: number;
  observationsChecked: number;
};

const nodeOf = (output: OutputSummary, header: string): SDK.StateQueueNode =>
  Data.castFrom(
    SDK.linkedListDatumToNodeView(
      Data.from(output.datum!.toString("hex"), SDK.LinkedListDatum),
      `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${header}`,
    ).data,
    SDK.StateQueueNode,
  ) as SDK.StateQueueNode;

/** The header's newest attested node in the model: outRef, tx and block hash. */
const newestAttested = (
  model: ChainModel,
  header: string,
): Readonly<{ outRef: string; txHash: string; blockHash: string }> | null => {
  let found: Readonly<{
    outRef: string;
    txHash: string;
    blockHash: string;
  }> | null = null;
  for (const { tx, block } of model.unitHistory.get(`${NODE_UNIT}${header}`) ??
    []) {
    tx.outputs.forEach((output, index) => {
      if (
        output.assets
          .get(D.stateQueueMint)
          ?.has(`${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${header}`) !==
          true ||
        nodeOf(output, header).da_attestation === SDK.NO_DA_ATTESTATION
      )
        return;
      found = {
        outRef: label(tx.hash, index),
        txHash: tx.hash.toString("hex"),
        blockHash: block.point.hash.toString("hex"),
      };
    });
  }
  return found;
};

/** The tx that burned the header's node in the model, or null. */
const burnOf = (model: ChainModel, header: string): OracleEntry | null =>
  (model.unitHistory.get(`${NODE_UNIT}${header}`) ?? []).find(
    ({ tx }) =>
      (tx.mint
        .get(D.stateQueueMint)
        ?.get(`${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${header}`) ?? 0n) < 0n,
  ) ?? null;

const departedCheck = (seen: Seen) => {
  const follower = createChainFollower();
  return async ({
    store,
    step,
  }: Readonly<{ store: FactStore; step: ForkStep }>): Promise<
    string | null
  > => {
    const blocks = follower.follow(step);
    const cursor = await store.cursor();
    if (cursor === null) return null;
    const model = modelOf(blocks);
    const source = createWatcherQueueHeaderSource(store, { releaseDepth: R });
    const expectRetained = async (
      header: string,
      what: string,
    ): Promise<string | null> => {
      const expected = newestAttested(model, header);
      try {
        const resolved = await source.resolveRetainedHeader({
          headerHash: header,
        });
        if (expected === null)
          return `${what} ${header} resolved with no attested node in the model`;
        if (
          resolved.queueOutRef !== expected.outRef ||
          resolved.observedTransactionHash !== expected.txHash ||
          resolved.observedBlockHash !== expected.blockHash
        )
          return `${what} ${header} resolved at ${resolved.queueOutRef}, model ${expected.outRef}`;
        return null;
      } catch (error) {
        if (error instanceof WatcherRetainedHeaderAttestationPendingError) {
          if (expected !== null)
            return `${what} ${header} is pending; the model attested it at ${expected.outRef}`;
          seen.predecessorsPending += 1;
          return null;
        }
        return `${what} ${header}: ${error instanceof Error ? error.message : String(error)}`;
      }
    };

    // Every queued header's predecessor (when it is a header of this chain).
    const { queue } = modelQueue(model, D);
    for (const { headerHash, outRef } of queue) {
      if (headerHash === null) continue;
      const created = model.created.get(outRef)!.output;
      const previous = nodeOf(created, headerHash).header.prevHeaderHash;
      if (!model.unitHistory.has(`${NODE_UNIT}${previous}`)) continue;
      const failed = await expectRetained(previous, "predecessor");
      if (failed !== null) return failed;
      seen.predecessors += 1;
      if (burnOf(model, previous) !== null) seen.departedPredecessors += 1;
    }

    // The newest merge: the confirmed state's header.
    const merged = [...model.unitHistory.keys()]
      .filter((unit) => unit.startsWith(NODE_UNIT))
      .map((unit) => unit.slice(NODE_UNIT.length))
      .map((header) => ({ header, burn: burnOf(model, header) }))
      .filter(
        (entry): entry is { header: string; burn: OracleEntry } =>
          entry.burn !== null,
      )
      .sort(
        (left, right) =>
          left.burn.block.height - right.burn.block.height ||
          left.burn.tx.index - right.burn.tx.index,
      );
    const newest = merged.at(-1);
    if (newest !== undefined) {
      const failed = await expectRetained(newest.header, "newest merge");
      if (failed !== null) return failed;
      seen.newestMerges += 1;
    }

    // Release proofs of an observation R + 2 blocks deep.
    const observed = blocks.at(-(R + 2));
    if (
      observed === undefined ||
      observed.point.slot <= cursor.prunedThroughSlot ||
      !blocks.at(-1)!.point.hash.equals(cursor.point.hash)
    )
      return null;
    const then = modelQueue(
      modelOf(blocks.slice(0, blocks.indexOf(observed) + 1)),
      D,
    ).queue.flatMap(({ headerHash }) =>
      headerHash === null ? [] : [headerHash],
    );
    const observation = {
      nativePoint: { slot: observed.point.slot.toString() },
      finalizedHeaders: then.map((headerHash) => ({ headerHash })),
    } as unknown as WatcherAuthenticatedStateQueueObservation;
    const proofs = await source.resolveMergedHeaders({ observation });
    const wanted = new Map<string, string>();
    for (const header of then) {
      const burn = burnOf(model, header);
      if (
        burn !== null &&
        burn.block.point.slot > observed.point.slot &&
        cursor.height - burn.block.height + 1 >= R
      )
        wanted.set(header, burn.tx.hash.toString("hex"));
    }
    const got = new Map(
      [...proofs].map(([header, proof]) => [
        header,
        "mergeTransactionHash" in proof
          ? proof.mergeTransactionHash
          : "removal",
      ]),
    );
    if (JSON.stringify([...got].sort()) !== JSON.stringify([...wanted].sort()))
      return `release proofs ${JSON.stringify([...got])} != model ${JSON.stringify([...wanted])}`;
    seen.mergedProofs += got.size;
    seen.observationsChecked += 1;
    return null;
  };
};

const projection = (
  seen: Seen,
  edit?: (base: FollowerProjection) => FollowerProjection,
): FollowerProjection => {
  const base: FollowerProjection = {
    ...watcherProjection(D),
    traffic: stateQueueTraffic({
      commitChance: 0.7,
      attestChance: 0.6,
      mergeChance: 0.35,
      attestNodes: true,
    }),
    protects,
    check: departedCheck(seen),
  };
  return edit?.(base) ?? base;
};

const fresh = (): Seen => ({
  predecessors: 0,
  predecessorsPending: 0,
  departedPredecessors: 0,
  newestMerges: 0,
  mergedProofs: 0,
  observationsChecked: 0,
});

describe("departed state-queue headers", () => {
  it("stay resolvable and prove their release across rollbacks and pruning", async () => {
    const seen = fresh();
    const outcome = await runForkScenario(SCENARIO, {
      open: openSqlite,
      k: K,
      projections: [projection(seen)],
    });
    expect(failure(outcome)).toBe("ok");
    expect(seen.predecessors).toBeGreaterThan(50);
    expect(seen.predecessorsPending).toBeGreaterThan(5);
    expect(seen.departedPredecessors).toBeGreaterThan(5);
    expect(seen.newestMerges).toBeGreaterThan(20);
    expect(seen.mergedProofs).toBeGreaterThan(5);
    expect(seen.observationsChecked).toBeGreaterThan(20);
  }, 120_000);

  it("the check catches a projection that keeps no departures", async () => {
    const outcome = await runForkScenario(SCENARIO, {
      open: openSqlite,
      k: K,
      projections: [
        projection(fresh(), (base) => ({
          ...base,
          derivations: base.derivations?.map((hook) =>
            hook.name !== "watcher_state_queue"
              ? hook
              : {
                  ...hook,
                  apply: async (context) => {
                    await hook.apply(context);
                    await context.tx.query(
                      `DELETE FROM ${WATCHER_DEPARTED_HEADERS_TABLE}`,
                      [],
                    );
                  },
                },
          ),
        })),
      ],
    });
    expect(failure(outcome)).toMatch(
      /newest merge|predecessor|release proofs/u,
    );
  }, 120_000);
});
